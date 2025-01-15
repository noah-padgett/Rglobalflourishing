#' Reformat Imputed Data
#'
#' Reformated the imputed (nested) data for use in coordinated regression analyses.
#'
#' @param df.imp a nested data.frame with results from run_impute_data(.).
#' @param list.default (required) a names list with DEMOGRAPHICS.CHILDHOOD.PRED.VEC, OUTCOME.VE,
#'    FOCAL_PREDICTOR, USE_DEFAULT, FORCE_BINARY, VALUES_DEFINING_UPPER_CATEGORY, VALUES_DEFINING_LOWER_CATEGORY, FORCE_CONTINUOUS
#' @param list.composites (optional) a named list with elements LIST.OUTCOME.COMPOSITES, LIST.COMPOSITE.COMBINE.METHOD and COMPOSITE.VEC.
#' @param list.manual (optional) a list like list.default (to-do)
#' @param ... other arguments passed to mice
#' @returns a long data.frame for use in analyses
#' @examples {
#'   # none
#' }
#' @export
#' @description
#' TO-DO
#'
recode_imputed_data <- function(
    df.imp = NULL,
    list.default = NULL,
    list.composites = NULL,
    list.manual = NULL,
    ...) {
  if (is.null(list.default)) {
    stop("'list.default' was not supplied. Check input for recode_imputed data.")
  }
  if (!(is.null(list.manual))) {
    # TODO
    # add functionality to over-write default provided in list.default(.)
  }

  # extract imputed data and coerce into "long format"
  df.imp.long <- df.imp %>%
    mutate(imp.complete = map(imp.res, ~ complete(., action = "long"))) %>%
    select(COUNTRY, imp.complete) %>%
    unnest(imp.complete) %>%
    mutate(
      across(where(is.factor) | where(is.character), \(x){
        x <- sub("\\..*", "", x)
        case_when(x == "(Missing)" ~ NA, .default = x) |>
          as.numeric()
      })
    ) %>%
    # dplyr::filter(.imp <= Nimp.keep) %>%
    dummy_cols(
      select_columns = c(
        "SVCS_MOTHER_W1",
        "SVCS_FATHER_W1",
        "MOTHER_RELATN_W1",
        "MOTHER_LOVED_W1",
        "FATHER_RELATN_W1",
        "FATHER_LOVED_W1",
        "MARITAL_STATUS_W1",
        "MARITAL_STATUS_W2"
      ),
      ignore_na = T
    ) %>%
    mutate(
      COV_AGE_GRP_W1 = recode_labels(AGE_W1, "AGE_GRP_W1"),
      COV_AGE_GRP_W1 = recode_to_type(COV_AGE_GRP_W1, "AGE_GRP_W1"),
      across(any_of(c(list.default[["DEMOGRAPHICS.CHILDHOOD.PRED.VEC"]])), \(x){
        x <- recode_labels(x, cur_column())
        x <- case_when(x == "(Missing)" ~ NA, .default = x) # needed for Abused and other 100% missing variable sin some countries
        x <- str_sub(x, 4, -1) # removed the numbers
        x <- str_replace_all(x, "\\n", "")
        x <- str_trim(x)
        x <- recode_to_type(x, cur_column())
        x
      }, .names = "COV_{.col}"),
      COV_MOTHER_RELATN_W1 = case_when(MOTHER_RELATN_W1_1 == 1 | MOTHER_RELATN_W1_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
      COV_MOTHER_RELATN_W1 = factor(COV_MOTHER_RELATN_W1),
      COV_FATHER_RELATN_W1 = case_when(FATHER_RELATN_W1_1 == 1 | FATHER_RELATN_W1_2 == 1 ~ "Very/somewhat good", .default = "Very/somewhat bad"),
      COV_FATHER_RELATN_W1 = factor(COV_FATHER_RELATN_W1),
      COV_MOTHER_NA = case_when(SVCS_MOTHER_W1_97 == 1 | MOTHER_RELATN_W1_97 == 1 | MOTHER_LOVED_W1_97 == 1 ~ 1, .default = 0),
      COV_FATHER_NA = case_when(SVCS_FATHER_W1_97 == 1 | FATHER_RELATN_W1_97 == 1 | FATHER_LOVED_W1_97 == 1 ~ 1, .default = 0),

      # enforce reference group
      COV_AGE_GRP_W1 = relevel(COV_AGE_GRP_W1, ref = "18-24"),
      COV_GENDER_W1 = relevel(COV_GENDER_W1, ref = "Male"),
      COV_EDUCATION_3_W1 = relevel(COV_EDUCATION_3_W1, ref = "9-15"),
      COV_EMPLOYMENT_W1 = relevel(COV_EMPLOYMENT_W1, ref = "Employed for an employer"),
      COV_MARITAL_STATUS_W1 = relevel(COV_MARITAL_STATUS_W1, ref = "Single/Never been married"),
      COV_ATTEND_SVCS_W1 = relevel(COV_ATTEND_SVCS_W1, ref = "Never"),
      COV_BORN_COUNTRY_W1 = relevel(COV_BORN_COUNTRY_W1, ref = "Born in this country"),
      COV_PARENTS_12YRS_W1 = relevel(COV_PARENTS_12YRS_W1, ref = "Yes, married"),
      COV_MOTHER_RELATN_W1 = relevel(COV_MOTHER_RELATN_W1, ref = "Very/somewhat bad"),
      COV_FATHER_RELATN_W1 = relevel(COV_FATHER_RELATN_W1, ref = "Very/somewhat bad"),
      COV_SVCS_12YRS_W1 = relevel(COV_SVCS_12YRS_W1, ref = "Never"),
      COV_OUTSIDER_W1 = relevel(COV_OUTSIDER_W1, ref = "No"),
      COV_ABUSED_W1 = relevel(COV_ABUSED_W1, ref = "No"),
      COV_HEALTH_GROWUP_W1 = relevel(COV_HEALTH_GROWUP_W1, ref = "Good"),
      COV_INCOME_12YRS_W1 = relevel(COV_INCOME_12YRS_W1, ref = "Got by"),

      # Other basic setup to make things easier later...
      RACE = recode_labels(SELFID1_W1, "SELFID1_W1"),
      MARITAL_STATUS_EVER_MARRIED_W1 = case_when(MARITAL_STATUS_W1 %in% c(2:5) ~ 1, .default = 0),
      MARITAL_STATUS_EVER_MARRIED_W2 = case_when(MARITAL_STATUS_W2 %in% c(2:5) ~ 1, .default = 0),
      COUNTRY2 = recode_labels(COUNTRY2, "COUNTRY_W1"),
      COUNTRY2 = str_sub(COUNTRY2, 4, -1),
      COUNTRY2 = str_replace_all(COUNTRY2, "\\n", ""),
      COUNTRY2 = str_trim(COUNTRY2)
    )

  # colnames(df.imp.long)[-c(1:3)] <- sub("\\..*", "", colnames(df.imp.long)[-c(1:3)])
  # df.imp.long <- df.imp.long %>%

  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(LIST.OUTCOME.COMPOSITES)) {
    LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
    LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
    COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]

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

  # Recode RACE
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
        COUNTRY2 == "United States" & str_detect(RACE, "White", negate = TRUE) ~ 1
      )
    )

  # Next, make sure all categorical items are recoded as binary
  # 	Must do this last as a few composites are formed from the
  # 	categorical Likert-type items

  tmp.outcome <- list.default[["OUTCOME.VEC"]][1]
  for (tmp.outcome in list.default[["OUTCOME.VEC"]]) {
    # print(tmp.outcome)
    x <- df.imp.long[, tmp.outcome, drop = TRUE]
    if ((tmp.outcome == list.default[["FOCAL_PREDICTOR"]]) & !(list.default[["USE_DEFAULT"]])) {
      # if the focal variable is continous, essential ignore and moe to next
      if (get_outcome_scale(tmp.outcome) == "cont") {
        if (list.default[["FORCE_BINARY"]]) {
          x <- case_when(
            x %in% c(list.default[["VALUES_DEFINING_UPPER_CATEGORY"]]) ~ 1,
            x %in% c(list.default[["VALUES_DEFINING_LOWER_CATEGORY"]]) ~ 0
          )
        } else {
          x <- recode_to_type(x, tmp.outcome)
          x <- reorder_levels(x, tmp.outcome)
          x <- recode_to_numeric(x, tmp.outcome)
        }
      } else if (list.default[["FORCE_CONTINUOUS"]]) {
        x <- recode_to_type(x, tmp.outcome)
        x <- reorder_levels(x, tmp.outcome)
        if (get_outcome_scale(tmp.outcome) %in% c("bin", "likert")) {
          x <- as.numeric(x)
        } else {
          x <- recode_to_numeric(x, tmp.outcome)
        }
      } else {
        # if focal variable is binary/categorical, then collasp as user-specified
        x <- case_when(
          x %in% c(list.default[["VALUES_DEFINING_UPPER_CATEGORY"]]) ~ 1,
          x %in% c(list.default[["VALUES_DEFINING_LOWER_CATEGORY"]]) ~ 0
        )
      }
    } else {
      if (str_detect(tmp.outcome, "COMPOSITE")) {
        is.sum <- ifelse(
          str_detect(tmp.outcome, "COMPOSITE_DEPRESSION") |
            str_detect(tmp.outcome, "COMPOSITE_ANXIETY") |
            str_detect(tmp.outcome, "COMPOSITE_DEP_ANX_COMBO"),
          TRUE, FALSE
        )
        x <- recode_to_numeric(x, tmp.outcome, is.sum)
      } else if (get_outcome_scale(tmp.outcome) == "cont") {
        x <- recode_to_type(x, tmp.outcome)
        x <- reorder_levels(x, tmp.outcome)
        x <- recode_to_numeric(x, tmp.outcome)
      } else {
        x <- recode_to_numeric(x, tmp.outcome)
      }
    }
    if (!is.null(x)) {
      df.imp.long[, tmp.outcome] <- x
    }
  }


  # Next, recode religion.
  # 	This must be done BY country.
  # function to recode religion
  # recodes religion variable to either use "No religion" as reference group if that grp contains at least 3% of the total sample.
  # 	else, recodes such that the most prominant religion is the reference grp.
  # 	combines all religious grps with less than 3% of the smaple into a single combined grp.
  recode_REL <- function(data, var) {
    # re-level REL2 based on most prominent religion as reference
    rel.prominence <- data %>%
      group_by((!!as.name(var)), .drop = FALSE) %>%
      summarise(N = sum(WGT)) %>%
      ungroup() %>%
      mutate(N = N / sum(N))
    rel.prominence.tab <- rel.prominence[, 2, drop = T]
    names(rel.prominence.tab) <- rel.prominence[, 1, drop = T]
    rel.lvl <- levels(data[[var]])
    rel.ag.prom <- rel.prominence.tab[rel.lvl == "No religion/Atheist/Agnostic"]
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
        recode_REL(x, "COV_REL1_W1")
      }),
      data = map(data, \(x){
        recode_REL(x, "COV_REL2_W1")
      })
    ) %>%
    unnest(c(data)) %>%
    mutate(
      PSU = PSU_W1,
      STRATA = STRATA_W1,
      WGT = WGT
    )

  df.imp.long
}
