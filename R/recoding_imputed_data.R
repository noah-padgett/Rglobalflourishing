#' Reformat Imputed Data
#'
#' Reformated the imputed (nested) data for use in coordinated regression analyses.
#'
#' @param df.imp a nested data.frame with results from run_impute_data(.).
#' @param list.default (required) a names list with DEMOGRAPHICS.CHILDHOOD.PRED.VEC, OUTCOME.VE,
#'    FOCAL_PREDICTOR, USE_DEFAULT, FORCE_BINARY, VALUES_DEFINING_UPPER_CATEGORY, VALUES_DEFINING_LOWER_CATEGORY, FORCE_CONTINUOUS
#' @param list.composites (optional) a named list with elements LIST.OUTCOME.COMPOSITES, LIST.COMPOSITE.COMBINE.METHOD and COMPOSITE.VEC.
#' @param list.manual (optional) a list like list.default (to-do)
#' @param wave (default is wave 2) but can be coerced to use wave 1 data (use wave = 1 to appropriately utilize recoding of wave 1 data)
#' @param method.income (optional) method for how income, based on country specific labels, is recoded. Options include 'quintiles.top.fixed', 'quintiles.top.random', 'numeric'. Defaults to 'quintiles.top.random'.
#' @param ... other arguments passed to mice
#' @returns a long data.frame for use in analyses
#' @examples {
#'   # none
#' }
#' @export
#' @description
#'
#' By default, the recoding creates a set of new variables for each demographic and childhood predictor variable with the prefix "COV_".
#'
#' method.income has several options.
#' * 'quintiles.num.fixed', 'quintiles.num.random' use survey::svyquantile() to identify the country specific categories that separate responses into quintiles. This is only approximate however because the categories are coarse and the top 20% isn't guarenteed to be exact. The used of the 'fixed' version uses these coarse not exactly 20% quantiles. The 'random' version will randomly sample exactly 20% from a top down approach to obtain quintiles.
#' * 'quintiles.top.fixed', ''quintiles.top.random' use the
#' * 'numeric
#'
#' More to come...
#'
recode_imputed_data <- function(
    df.imp = NULL,
    list.default = NULL,
    list.composites = NULL,
    list.manual = NULL,
    wave = 2,
    method.income = "quintiles.top.random",
    wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU",
    ...) {
  if (is.null(list.default)) {
    stop("'list.default' was not supplied. Check input for recode_imputed data.")
  }
  if (!(is.null(list.manual))) {
    # TODO
    # add functionality to over-write default provided in list.default(.)
  }
  drop_created_vars <- c("AGE_GRP_Y1", "AGE_GRP_Y2", "RACE", "RACE_PLURALITY_Y1", "INCOME_QUINTILE_Y1", "INCOME_QUINTILE_Y2")
  ## ============================================================================================ ##
  ## ====== Initial Setup ======================================================================= ##
  # extract imputed data and coerce into "long format"

  #df.imp.long <- df.imp %>%
  #  mutate(imp.complete = map(imp.res, ~ complete(., action = "long"))) %>%
  #  select(COUNTRY, imp.complete) %>%
  #  unnest(imp.complete)

  df.imp.long <- complete(df.imp, action = "long") %>%
    select(!any_of(drop_created_vars)) %>%
    mutate(
      across(where(is.factor), \(x){
      	if(str_detect(cur_column(), "COUNTRY", negate=TRUE)){
        	x <- sub("\\..*", "", x)
        	x = case_when(x == "(Missing)" ~ NA, .default = x) |>
          		as.numeric()
        }
        if(str_detect(cur_column(), "BORN_COUNTRY")){
        	x <- sub("\\..*", "", x)
        	x = case_when(x == "(Missing)" ~ NA, .default = x) |>
          		as.numeric()
        }
        x
      })
    )
  #options(warn=2)
  #for(i in 1:ncol(df.imp.long)){
  #x = df.imp.long[[i]]
  #if(is.factor(x) & str_detect(colnames(df.imp.long)[i],"COUNTRY", negate=TRUE)){
  # x <- sub("\\..*", "", x)
  # x = case_when(x == "(Missing)" ~ NA, .default = x) |>
  #   as.numeric()
  #}
  #}
  ## ============================================================================================ ##
  ## ====== WAVE SPECIFIC CODE ================================================================== ##
  ## TO-DO: develop a generalized approach...
  ## Wave 1
  if(wave == 1 | wave == "W1" | wave == "w1" | wave == "Y1"){
    df.imp.long <- df.imp.long %>%
      dummy_cols(
        select_columns = c(
          "SVCS_MOTHER",
          "SVCS_FATHER",
          "MOTHER_RELATN",
          "MOTHER_LOVED",
          "FATHER_RELATN",
          "FATHER_LOVED",
          "MARITAL_STATUS"
        ),
        ignore_na = T
      ) %>%
      mutate(
        COV_AGE_GRP = recode_labels(AGE, "AGE_GRP"),
        COV_AGE_GRP = recode_to_type(COV_AGE_GRP, "AGE_GRP"),
        across(any_of(c(list.default[["DEMOGRAPHICS.CHILDHOOD.PRED.VEC"]])), \(x){
          x <- recode_labels(x, cur_column(), include.value = FALSE)
          x <- case_when(x == "(Missing)" ~ NA, .default = x) # needed for Abused and other 100% missing variable sin some countries
          #x <- str_sub(x, 4, -1) # removed the numbers
          #x <- str_replace_all(x, "\\n", "")
          #x <- str_trim(x)
          x <- recode_to_type(x, cur_column())
          x
        }, .names = "COV_{.col}"),
        COV_MOTHER_RELATN = case_when(MOTHER_RELATN_1 == 1 | MOTHER_RELATN_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
        COV_MOTHER_RELATN = factor(COV_MOTHER_RELATN),
        COV_FATHER_RELATN = case_when(FATHER_RELATN_1 == 1 | FATHER_RELATN_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
        COV_FATHER_RELATN = factor(COV_FATHER_RELATN),
        COV_MOTHER_NA = case_when(SVCS_MOTHER_97 == 1 | MOTHER_RELATN_97 == 1 | MOTHER_LOVED_97 == 1 ~ 1, .default = 0),
        COV_FATHER_NA = case_when(SVCS_FATHER_97 == 1 | FATHER_RELATN_97 == 1 | FATHER_LOVED_97 == 1 ~ 1, .default = 0),

        # enforce reference group
        COV_AGE_GRP = relevel(COV_AGE_GRP, ref = levels(COV_AGE_GRP)[str_detect(levels(COV_AGE_GRP),"18-24")]),
        COV_GENDER = relevel(COV_GENDER, ref = "Male"),
        COV_EDUCATION_3 = relevel(COV_EDUCATION_3, ref = "9-15"),
        COV_EMPLOYMENT = relevel(COV_EMPLOYMENT, ref = "Employed for an employer"),
        COV_MARITAL_STATUS = relevel(COV_MARITAL_STATUS, ref = "Single/Never been married"),
        COV_ATTEND_SVCS = relevel(COV_ATTEND_SVCS, ref = "Never"),
        COV_BORN_COUNTRY = relevel(COV_BORN_COUNTRY, ref = "Born in this country"),
        COV_PARENTS_12YRS = relevel(COV_PARENTS_12YRS, ref = "Yes, married"),
        COV_MOTHER_RELATN = relevel(COV_MOTHER_RELATN, ref = "Very/somewhat bad"),
        COV_FATHER_RELATN = relevel(COV_FATHER_RELATN, ref = "Very/somewhat bad"),
        COV_SVCS_12YRS = relevel(COV_SVCS_12YRS, ref = "Never"),
        COV_OUTSIDER = relevel(COV_OUTSIDER, ref = "No"),
        COV_ABUSED = ifelse(COUNTRY == "Israel", COV_ABUSED, relevel(COV_ABUSED, ref = "No")),
        COV_HEALTH_GROWUP = relevel(COV_HEALTH_GROWUP, ref = "Good"),
        COV_INCOME_12YRS = relevel(COV_INCOME_12YRS, ref = "Got by"),

        # Other basic setup to make things easier later...
        RACE = recode_labels(SELFID1, "SELFID1", include.value = FALSE),
        MARITAL_STATUS_EVER_MARRIED = case_when(MARITAL_STATUS %in% c(2:5) ~ 1, .default = 0),
        MARITAL_STATUS_DIVORCED = case_when(MARITAL_STATUS == 4 ~ 1, .default = 0),
        CIGARETTES_BINARY = case_when(CIGARETTES > 0 ~ 1, .default = 0),
        COUNTRY2 = recode_labels(COUNTRY2, "COUNTRY", include.value = FALSE)
      )
  }
  ## Wave 2
  if(is.null(wave) | wave == 2 | wave == "W2" | wave == "w2" | wave == "Y2"){
    df.imp.long <- df.imp.long %>%
      dummy_cols(
        select_columns = c(
          "SVCS_MOTHER_Y1",
          "SVCS_FATHER_Y1",
          "MOTHER_RELATN_Y1",
          "MOTHER_LOVED_Y1",
          "FATHER_RELATN_Y1",
          "FATHER_LOVED_Y1",
          "MARITAL_STATUS_Y1",
          "MARITAL_STATUS_Y2"
        ),
        ignore_na = T
      ) %>%
      mutate(
        COV_AGE_GRP_Y1 = recode_labels(AGE_Y1, "AGE_GRP_Y1"),
        COV_AGE_GRP_Y1 = recode_to_type(COV_AGE_GRP_Y1, "AGE_GRP_Y1"),
        across(any_of(c("URBAN_RURAL_Y1", list.default[["DEMOGRAPHICS.CHILDHOOD.PRED.VEC"]])), \(x){
          x <- recode_labels(x, cur_column(), include.value = FALSE)
          x <- case_when(x == "(Missing)" ~ NA, .default = x) # needed for Abused and other 100% missing variable sin some countries
          #x <- str_sub(x, 4, -1) # removed the numbers
          #x <- str_replace_all(x, "\\n", "")
          #x <- str_trim(x)
          x <- recode_to_type(x, cur_column())
          x
        }, .names = "COV_{.col}"),
        COV_MOTHER_RELATN_Y1 = case_when(MOTHER_RELATN_Y1_1 == 1 | MOTHER_RELATN_Y1_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
        COV_MOTHER_RELATN_Y1 = factor(COV_MOTHER_RELATN_Y1),
        COV_FATHER_RELATN_Y1 = case_when(FATHER_RELATN_Y1_1 == 1 | FATHER_RELATN_Y1_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
        COV_FATHER_RELATN_Y1 = factor(COV_FATHER_RELATN_Y1),
        COV_MOTHER_NA = case_when(SVCS_MOTHER_Y1_97 == 1 | MOTHER_RELATN_Y1_97 == 1 | MOTHER_LOVED_Y1_97 == 1 ~ 1, .default = 0),
        COV_FATHER_NA = case_when(SVCS_FATHER_Y1_97 == 1 | FATHER_RELATN_Y1_97 == 1 | FATHER_LOVED_Y1_97 == 1 ~ 1, .default = 0),

        # enforce reference group
        COV_AGE_GRP_Y1 = relevel(COV_AGE_GRP_Y1, ref = levels(COV_AGE_GRP_Y1)[str_detect(levels(COV_AGE_GRP_Y1),"18-24")]),
        COV_GENDER = relevel(COV_GENDER, ref = "Male"),
        COV_EDUCATION_3_Y1 = relevel(COV_EDUCATION_3_Y1, ref = "9-15"),
        COV_EMPLOYMENT_Y1 = relevel(COV_EMPLOYMENT_Y1, ref = "Employed for an employer"),
        COV_MARITAL_STATUS_Y1 = relevel(COV_MARITAL_STATUS_Y1, ref = "Single/Never been married"),
        COV_ATTEND_SVCS_Y1 = relevel(COV_ATTEND_SVCS_Y1, ref = "Never"),
        COV_BORN_COUNTRY_Y1 = relevel(COV_BORN_COUNTRY_Y1, ref = "Born in this country"),
        COV_PARENTS_12YRS_Y1 = relevel(COV_PARENTS_12YRS_Y1, ref = "Yes, married"),
        COV_MOTHER_RELATN_Y1 = relevel(COV_MOTHER_RELATN_Y1, ref = "Very/somewhat bad"),
        COV_FATHER_RELATN_Y1 = relevel(COV_FATHER_RELATN_Y1, ref = "Very/somewhat bad"),
        COV_SVCS_12YRS_Y1 = relevel(COV_SVCS_12YRS_Y1, ref = "Never"),
        COV_OUTSIDER_Y1 = relevel(COV_OUTSIDER_Y1, ref = "No"),
        COV_ABUSED_Y1 = ifelse(COUNTRY == "Israel", COV_ABUSED_Y1, relevel(COV_ABUSED_Y1, ref = "No")),
        COV_HEALTH_GROWUP_Y1 = relevel(COV_HEALTH_GROWUP_Y1, ref = "Good"),
        COV_INCOME_12YRS_Y1 = relevel(COV_INCOME_12YRS_Y1, ref = "Got by"),

        # Other basic setup to make things easier later...
        MODE_RECRUIT = recode_labels( MODE_RECRUIT, "MODE_RECRUIT", include.value = FALSE),
        MODE_RECRUIT = recode_to_type(MODE_RECRUIT, "MODE_RECRUIT"),
        RACE = COV_SELFID1,
        MARITAL_STATUS_EVER_MARRIED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(2:5) ~ 1, .default = 0),
        MARITAL_STATUS_EVER_MARRIED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(2:5) ~ 1, .default = 0),
        MARITAL_STATUS_DIVORCED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(4) ~ 1, .default = 0),
        MARITAL_STATUS_DIVORCED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(4) ~ 1, .default = 0),
        CIGARETTES_BINARY_Y1 = case_when(CIGARETTES_Y1 > 0 ~ 1, .default = 0),
        CIGARETTES_BINARY_Y2 = case_when(CIGARETTES_Y2 > 0 ~ 1, .default = 0),
        COUNTRY2 = COUNTRY #recode_labels(COUNTRY2, "COUNTRY_Y1", include.value = FALSE)
      )
  }
  ## ============================================================================================ ##
  ## ====== CREATE COMPOSITES =================================================================== ##
  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(list.composites)) {
    if(wave == 1 | wave == "W1"){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES0"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD0"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC0"]]
    }
    if(is.null(wave) | wave == 2 | wave == "W2"){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]
    }
    for (i in 1:length(LIST.OUTCOME.COMPOSITES)) {
      # create a temporary variable then rename
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "mean") {
        df.imp.long <- df.imp.long %>%
          mutate(
            temp.var.i = rowMeans(
              across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column())
                x
              })
            )
          )
      }
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "sum") {
        df.imp.long <- df.imp.long %>%
          mutate(
            temp.var.i = rowSums(
              across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column(), TRUE)
                x
              })
            )
          )
      }
      colnames(df.imp.long)[length(colnames(df.imp.long))] <- COMPOSITE.VEC[i]
    }
  }
  ## ============================================================================================ ##
  ## ====== RACE PLURALITY INDICATOR ============================================================ ##
  {
  df.imp.long <- df.imp.long %>%
    mutate(
      COV_RACE_PLURALITY = case_when(
        COUNTRY2 == "Argentina" & str_detect(RACE, "White") ~ 0,
        COUNTRY2 == "Argentina" & str_detect(RACE, "White", negate = TRUE) ~ 1,
        COUNTRY2 == "Australia" & str_detect(RACE, "Australian") ~ 0,
        COUNTRY2 == "Australia" & str_detect(RACE, "Australian", negate = TRUE) ~ 1,
        COUNTRY2 == "Brazil" & str_detect(RACE, "Branca") ~ 0,
        COUNTRY2 == "Brazil" & str_detect(RACE, "Branca", negate = TRUE) ~ 1,
        COUNTRY2 == "Egypt" & str_detect(RACE, "Arab") ~ 0,
        COUNTRY2 == "Egypt" & str_detect(RACE, "Arab", negate = TRUE) ~ 1,
        COUNTRY2 == "Germany" ~ NA,
        COUNTRY2 == "Hong Kong" & str_detect(RACE, "Chinese (Cantonese)") ~ 0,
        COUNTRY2 == "Hong Kong" & str_detect(RACE, "Chinese (Cantonese)", negate = TRUE) ~ 1,
        COUNTRY2 == "India" & str_detect(RACE, "Other backward caste") ~ 0,
        COUNTRY2 == "India" & str_detect(RACE, "Other backward caste", negate = TRUE) ~ 1,
        COUNTRY2 == "Indonesia" & str_detect(RACE, "Jawa") ~ 0,
        COUNTRY2 == "Indonesia" & str_detect(RACE, "Jawa", negate = TRUE) ~ 1,
        COUNTRY2 == "Israel" & str_detect(RACE, "Jewish") ~ 0,
        COUNTRY2 == "Israel" & str_detect(RACE, "Jewish", negate = TRUE) ~ 1,
        COUNTRY2 == "Japan" ~ NA,
        COUNTRY2 == "Kenya" & str_detect(RACE, "Kikuyu") ~ 0,
        COUNTRY2 == "Kenya" & str_detect(RACE, "Kikuyu", negate = TRUE) ~ 1,
        COUNTRY2 == "Mexico" & str_detect(RACE, "Mestizo") ~ 0,
        COUNTRY2 == "Mexico" & str_detect(RACE, "Mestizo", negate = TRUE) ~ 1,
        COUNTRY2 == "Nigeria" & str_detect(RACE, "Hausa") ~ 0,
        COUNTRY2 == "Nigeria" & str_detect(RACE, "Hausa", negate = TRUE) ~ 1,
        COUNTRY2 == "Philippines" & str_detect(RACE, "Tagalog") ~ 0,
        COUNTRY2 == "Philippines" & str_detect(RACE, "Tagalog", negate = TRUE) ~ 1,
        COUNTRY2 == "Poland" & str_detect(RACE, "Polish") ~ 0,
        COUNTRY2 == "Poland" & str_detect(RACE, "Polish", negate = TRUE) ~ 1,
        COUNTRY2 == "South Africa" & str_detect(RACE, "Black") ~ 0,
        COUNTRY2 == "South Africa" & str_detect(RACE, "Black", negate = TRUE) ~ 1,
        COUNTRY2 == "Spain" ~ NA,
        COUNTRY2 == "Sweden" ~ NA,
        COUNTRY2 == "Tanzania" & str_detect(RACE, "African") ~ 0,
        COUNTRY2 == "Tanzania" & str_detect(RACE, "African", negate = TRUE) ~ 1,
        COUNTRY2 == "Turkiye" & str_detect(RACE, "Turkish") ~ 0,
        COUNTRY2 == "Turkiye" & str_detect(RACE, "Turkish", negate = TRUE) ~ 1,
        COUNTRY2 == "United Kingdom" & str_detect(RACE, "White") ~ 0,
        COUNTRY2 == "United Kingdom" & str_detect(RACE, "White", negate = TRUE) ~ 1,
        COUNTRY2 == "United States" & str_detect(RACE, "White") ~ 0,
        COUNTRY2 == "United States" & str_detect(RACE, "White", negate = TRUE) ~ 1,
        .default = 0
      )
    )
  }
  ## ============================================================================================ ##
  ## ====== RECODING RELIGION DATA ============================================================== ##
  {
    # Next, recode religion.
    # 	This must be done BY country.
    # function to recode religion
    # recodes religion variable to either use "No religion" as reference group if that grp contains at least 3% of the total sample.
    # 	else, recodes such that the most prominant religion is the reference grp.
    # 	combines all religious grps with less than 3% of the smaple into a single combined grp.
    recode_REL <- function(data, var, wgt) {
      # re-level REL2 based on most prominent religion as reference
      rel.prominence <- data %>%
        group_by((!!as.name(var)), .drop = FALSE) %>%
        summarise(N = sum(.data[[wgt]])) %>%
        ungroup() %>%
        mutate(N = N / sum(N))
      rel.prominence.tab <- rel.prominence[, 2, drop = T]
      names(rel.prominence.tab) <- rel.prominence[, 1, drop = T]
      rel.lvl <- levels(data[[var]])
      rel.ag.prom <- rel.prominence.tab[rel.lvl == "No religion/Atheist/Agnostic"]
        if (length(rel.ag.prom) == 0) {
            rel.ag.prom <- 0 }
      if (rel.ag.prom > 0.03) {
        rel.mp <- "No religion/Atheist/Agnostic"
      } else {
        rel.mp <- rel.lvl[which.max(rel.prominence.tab)]
      }
      data[[var]] <- fct_lump_prop(data[[var]], 0.03, other_level = "Combined")
      data[[var]] <- fct_infreq(data[[var]])
      data[[var]] <- relevel(data[[var]], ref = rel.mp)
      data
    }

    df.imp.long <- df.imp.long %>%
      group_by(COUNTRY, .imp) %>%
      nest() %>%
      mutate(
        data = map(data, \(x){
          recode_REL(x, paste0("COV_REL1", ifelse(wave == 2, "_Y1", "")), wgt = wgt)
        }),
        data = map(data, \(x){
          recode_REL(x, paste0("COV_REL2", ifelse(wave == 2, "_Y1", "")), wgt = wgt)
        })
      ) %>%
      unnest(c(data))
  }
  ## ============================================================================================ ##
  ## ====== RECODING INCOME DATA ================================================================ ##
  {
    if(wave == 1 | wave == "W1"){
      income.quintiles <- df.imp.long  %>%
        select(
          dplyr::all_of(c(".imp", "ID", "COUNTRY", "INCOME", wgt, strata, psu))
        ) %>%
        mutate(
          across(INCOME,\(x){
            case_when(
              x < 0 ~ NA,
              x > 9900 ~ NA,
              x == 9900 ~ 0,
              .default=x
            )
          })
        ) %>%
        group_by(.imp, COUNTRY) %>%
        nest() %>%
        mutate(
          svy.data = map(data, \(x){
            svydesign(data=x, ids=~(!!as.name(psu)), strata=~(!!as.name(strata)), weights=~(!!as.name(wgt)))
          }),
          quintiles = map(svy.data, \(x){
            svyquantile(~INCOME, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          })
        ) %>%
        select(.imp, COUNTRY, quintiles_w1)

      df.imp.long <- df.imp.long %>%
        group_by(.imp, COUNTRY2) %>%
        nest() %>%
        mutate(
          data = map(data, \(x){
            cur.country = x$COUNTRY[1]
            cur.imp = x$.imp[1]

            tmp.quintiles <- income.quintiles %>%
              filter(COUNTRY == cur.country, .imp == cur.imp)

            tmp.quintiles.w1 <- tmp.quintiles$quintiles[[1]][[1]]

            x <- x %>%
              mutate(
                INCOME_QUINTILE = case_when(
                  INCOME >= tmp.quintiles.w1["0.8",1] ~ 5,
                  INCOME >= tmp.quintiles.w1["0.6",1] & INCOME < tmp.quintiles.w1["0.8",1] ~ 4,
                  INCOME >= tmp.quintiles.w1["0.4",1] & INCOME < tmp.quintiles.w1["0.6",1] ~ 3,
                  INCOME >= tmp.quintiles.w1["0.2",1] & INCOME < tmp.quintiles.w1["0.4",1] ~ 2,
                  INCOME < tmp.quintiles.w1["0.2",1] ~ 1
                )
              )
            if(method.income=="quintiles.top.fixed"){
              x <- x %>%
                mutate(
                  across(contains("INCOME_QUINTILE"), \(x){
                    y <- case_when(
                      x == 5 ~ 1,
                      x %in% 1:4 ~ 0
                    )
                    y
                  })
                )
            }
            if(method.income == "quintiles.top.random"){
              set.seed(314150)
              x <- x %>%
                mutate(
                  across(contains("INCOME_QUINTILE"), \(x){
                    y <- case_when(
                      x == 5 ~ 1,
                      x %in% 1:4 ~ 0
                    )
                    y.mean = mean(y)
                    if(y.mean > 0.20){
                      n0 <- length(y)
                      nq <- round(y.mean*n0,0) - round(0.2*n0,0) # number of cases to randomly fix to 0
                      y[y==1][sample(1:(length(y[y==1])), nq, replace = FALSE)] <- 0
                    }
                    if(y.mean < 0.20){
                      n0 <- length(y)
                      nq <- round(0.2*n0,0) - round(y.mean*n0,0) # number of cases to randomly fix to 1 from those who are in the fourth quintile
                      x[x==4][sample(1:(length(x[x==4])), nq, replace = FALSE)] <- 5
                      y <- case_when(
                        x == 5 ~ 1,
                        x %in% 1:4 ~ 0
                      )
                    }
                    y
                  })
                )
            }
            x
          }),
          data = map(data, \(x){
            x %>%
              mutate(
                INCOME = recode_labels(INCOME, "INCOME"),
                INCOME_QUINTILE = recode_labels(INCOME_QUINTILE, "INCOME_QUINTILE"),
                INCOME_QUINTILE = factor(INCOME_QUINTILE),
                INCOME_QUINTILE = recode_to_numeric(INCOME_QUINTILE, "INCOME_QUINTILE")
              )
          })
        ) %>%
        unnest(c(data)) %>%
        ungroup()
    }
    if(is.null(wave) | wave == 2 | wave == "W2"){

      income.quintiles <-  df.imp.long  %>%
        select(
          dplyr::all_of(c(".imp", "ID", "COUNTRY", "INCOME_Y1", "INCOME_Y2", wgt, strata, psu))
        ) %>%
        mutate(
          across(contains("INCOME"),\(x){
            case_when(
              x < 0 ~ NA,
              x > 9900 ~ NA,
              x == 9900 ~ 0,
              .default=x
            )
          })
        ) %>%
        group_by(.imp, COUNTRY) %>%
        nest() %>%
        mutate(
          svy.data = map(data, \(x){
            svydesign(data=x, ids=~(!!as.name(psu)), strata=~(!!as.name(strata)), weights=~(!!as.name(wgt)))
          }),
          quintiles_w1 = map(svy.data, \(x){
            svyquantile(~INCOME_Y1, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          }),
          quintiles_w2 = map(svy.data, \(x){
            svyquantile(~INCOME_Y2, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          }),
        ) %>%
        select(.imp, COUNTRY, quintiles_w1, quintiles_w2)

       df.imp.long <-  df.imp.long %>%
        mutate(.imp2 = .imp) %>%
        group_by(.imp2, COUNTRY2) %>%
        nest() %>%
        mutate(
          data = map(data, \(x){
            cur.country = x$COUNTRY[1]
            cur.imp = x$.imp[1]

            tmp.quintiles <- income.quintiles %>%
              filter(COUNTRY == cur.country, .imp == cur.imp)

            tmp.quintiles.w1 <- tmp.quintiles$quintiles_w1[[1]][[1]]
            tmp.quintiles.w2 <- tmp.quintiles$quintiles_w2[[1]][[1]]

            x <- x %>%
              mutate(
                INCOME_QUINTILE_Y1 = case_when(
                  INCOME_Y1 >= tmp.quintiles.w1["0.8",1] ~ 5,
                  INCOME_Y1 >= tmp.quintiles.w1["0.6",1] & INCOME_Y1 < tmp.quintiles.w1["0.8",1] ~ 4,
                  INCOME_Y1 >= tmp.quintiles.w1["0.4",1] & INCOME_Y1 < tmp.quintiles.w1["0.6",1] ~ 3,
                  INCOME_Y1 >= tmp.quintiles.w1["0.2",1] & INCOME_Y1 < tmp.quintiles.w1["0.4",1] ~ 2,
                  INCOME_Y1 < tmp.quintiles.w1["0.2",1] ~ 1
                ),
                INCOME_QUINTILE_Y2 = case_when(
                  INCOME_Y2 >= tmp.quintiles.w2["0.8",1] ~ 5,
                  INCOME_Y2 >= tmp.quintiles.w2["0.6",1] & INCOME_Y2 < tmp.quintiles.w2["0.8",1] ~ 4,
                  INCOME_Y2 >= tmp.quintiles.w2["0.4",1] & INCOME_Y2 < tmp.quintiles.w2["0.6",1] ~ 3,
                  INCOME_Y2 >= tmp.quintiles.w2["0.2",1] & INCOME_Y2 < tmp.quintiles.w2["0.4",1] ~ 2,
                  INCOME_Y2 < tmp.quintiles.w2["0.2",1] ~ 1
                )
              )
            if(method.income=="quintiles.top.fixed"){
              x <- x %>%
                mutate(
                  across(contains("INCOME_QUINTILE"), \(x){
                    y <- case_when(
                      x == 5 ~ 1,
                      x %in% 1:4 ~ 0
                    )
                    y
                  })
                )
            }
            if(method.income == "quintiles.top.random"){
              set.seed(314150)
              x <- x %>%
                mutate(
                  across(contains("INCOME_QUINTILE"), \(x){
                    y <- case_when(
                      x == 5 ~ 1,
                      x %in% 1:4 ~ 0
                    )
                    y.mean = mean(y, na.rm=TRUE)
                    if(y.mean > 0.20){
                      n0 <- length(y)
                      nq <- round(y.mean*n0,0) - round(0.2*n0,0) # number of cases to randomly fix to 0
                      y[y==1][sample(1:(length(y[y==1])), nq, replace = FALSE)] <- 0
                    }
                    if(y.mean < 0.20){
                      n0 <- length(y)
                      nq <- round(0.2*n0,0) - round(y.mean*n0,0) # number of cases to randomly fix to 1 from those who are in the fourth quintile
                      x[x==4][sample(1:(length(x[x==4])), nq, replace = FALSE)] <- 5
                      y <- case_when(
                        x == 5 ~ 1,
                        x %in% 1:4 ~ 0
                      )
                    }
                    y
                  })
                )
            }
            x
          }),
          data = map(data, \(x){
            x %>%
              mutate(
                INCOME_Y1 = recode_labels(INCOME_Y1, "INCOME_Y1"),
                INCOME_Y2 = recode_labels(INCOME_Y2, "INCOME_Y2"),
                INCOME_QUINTILE_Y1 = recode_labels(INCOME_QUINTILE_Y1, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y2 = recode_labels(INCOME_QUINTILE_Y2, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y1 = factor(INCOME_QUINTILE_Y1),
                INCOME_QUINTILE_Y2 = factor(INCOME_QUINTILE_Y2),
                INCOME_QUINTILE_Y1 = recode_to_numeric(INCOME_QUINTILE_Y1, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y2 = recode_to_numeric(INCOME_QUINTILE_Y2, "INCOME_QUINTILE")
              )
          })
        ) %>%
        unnest(c(data)) %>%
        ungroup()
    }
  }
  ## ============================================================================================ ##
  ## ====== CATEGORICAL LEVELS CHECK ============================================================ ##
  # Next, make sure all categorical items are recoded as binary
  # 	Must do this last as a few composites are formed from the
  # 	categorical Likert-type items
  tmp.var <- list.default[["VARIABLES.VEC"]][1]
  for (tmp.var in list.default[["VARIABLES.VEC"]]) {
    # print(tmp.var)
    x <- df.imp.long[, tmp.var, drop = TRUE]
    if ((tmp.var %in% list.default[["FOCAL_PREDICTOR"]]) & !(all(list.default[["USE_DEFAULT"]]))) {
      # if the focal variable is continous, essential ignore and moe to next
      if (get_outcome_scale(tmp.var) == "cont") {
        if (list.default[["FORCE_BINARY"]][tmp.var]) {
          x <- case_when(
            x %in% c(list.default[["VALUES_DEFINING_UPPER_CATEGORY"]][[tmp.var]]) ~ 1,
            x %in% c(list.default[["VALUES_DEFINING_LOWER_CATEGORY"]][[tmp.var]]) ~ 0
          )
        } else {
          x <- recode_to_type(x, tmp.var)
          x <- reorder_levels(x, tmp.var)
          x <- recode_to_numeric(x, tmp.var)
        }
      } else if (get_outcome_scale(tmp.var) %in% c("bin", "likert")) {
      	  if (list.default[["FORCE_CONTINUOUS"]][tmp.var]) {
      	  	x <- recode_to_type(x, tmp.var)
      	  	x <- reorder_levels(x, tmp.var)
      	  	if (get_outcome_scale(tmp.var) %in% c("bin", "likert")) {
      	    	x <- as.numeric(x)
      	  	} else {
      	    	x <- recode_to_numeric(x, tmp.var)
      	  	}
      	  } else {
      	    # if focal variable is binary/categorical, then collasp as user-specified
      	    x <- case_when(
      	      x %in% c(list.default[["VALUES_DEFINING_UPPER_CATEGORY"]][[tmp.var]]) ~ 1,
      	      x %in% c(list.default[["VALUES_DEFINING_LOWER_CATEGORY"]][[tmp.var]]) ~ 0
      	    )
      	  }
      }
    } else {
      if (str_detect(tmp.var, "COMPOSITE")) {
        is.sum <- ifelse(
          str_detect(tmp.var, "COMPOSITE_DEPRESSION") |
            str_detect(tmp.var, "COMPOSITE_ANXIETY") |
            str_detect(tmp.var, "COMPOSITE_DEP_ANX_COMBO"),
          TRUE, FALSE
        )
        x <- recode_to_numeric(x, tmp.var, is.sum)
      } else if (get_outcome_scale(tmp.var) == "cont") {
        x <- recode_to_type(x, tmp.var)
        x <- reorder_levels(x, tmp.var)
        x <- recode_to_numeric(x, tmp.var)
      } else {
        x <- recode_to_numeric(x, tmp.var)
      }
    }
    if (!is.null(x)) {
      df.imp.long[, tmp.var] <- x
    }
  }
  ## ============================================================================================ ##
  df.imp.long
}


#' @export
recode_imp_by_country <- function(data.dir, .nimp = 20, ...){

  # get list of files in data.dir
  imp.files <- list.files(data.dir)
  imp.files <- imp.files[str_detect(imp.files, "imputed_data_obj")]
  imp.files <- imp.files[str_detect(imp.files, paste0("_nimp_",.nimp))]

  # run recode_imputed_data by country
  out.cap <- map(imp.files, \(x){
    load(here::here(data.dir,x), ex <- new.env())
    # ls.str(ex)
    tmp.imp <- recode_imputed_data(ex$fit.imp,...)
    cur.country <- as.character(tmp.imp$COUNTRY[1])
    c.file.name <- paste0("recoded_imputed_data_obj_",cur.country,".rds")
    readr::write_rds(tmp.imp, file = here::here(data.dir, c.file.name), compress = "gz")
  })

}
