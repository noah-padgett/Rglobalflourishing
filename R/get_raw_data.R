#' Estimate and Save Attrition Weights
#'
#' Estimate (save fitted model) and append attrition weights to data.frame
#'
#' @param file name of file to get data from
#' @param list.composites a lit of lists to construct composite variables
#' @param wave (default is wave 2) but can be coerced to use wave 1 data (use wave = 1 to appropriately utilize recoding of wave 1 data)
#' @param method.income method for how income, based on country specific labels, is recoded. Options include 'quintiles.num.fixed', 'quintiles.num.random', 'quintiles.top.fixed', 'quintiles.top.random', 'numeric'.
#' @param data.is.wide (FALSE) change to TRUE only if the data are in long format
#' @param reverse.code.cont (FALSE) reverse code numeric variables (e.g., lonely) for computing summary statistics. DO NOT apply this recoding before conducting imputation. THIS IS ONLY FOR SUMMARY STATISTICS.
#' @param to.numeric (optional) FALSE
#' @param ... other arguments
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_get_labelled_raw_data <- function(file, list.composites = NULL, wave = 2, method.income="quintiles.top.random", wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU", data.is.long = FALSE, reverse.code.cont = FALSE, to.numeric = FALSE,...) {
  # IF SPSS file format
  if (stringr::str_detect(stringr::str_to_lower(file), ".sav")) {
    df.original <- haven::read_spss(file)
    df.original <- haven::zap_labels(df.original)
  }
  if (stringr::str_detect(stringr::str_to_lower(file), ".dta")) {
    df.original <- haven::read_dta(file)
    df.original <- haven::zap_labels(df.original)
  }

  if (stringr::str_detect(stringr::str_to_lower(file), ".csv")) {
    df.original <- readr::read_csv(file)
    df.original <- df.original
  }

  ## columns to drop
  cols_to_drop <- c(
    "CNTRY_REL_BUD", "CNTRY_REL_CHI", "CNTRY_REL_CHR",
    "CNTRY_REL_HIN", "CNTRY_REL_ISL", "CNTRY_REL_JUD", "CNTRY_REL_SHI",
    paste0("REL",3:9),
    paste0("TEACHINGS_",1:15)
  )
  ##

  if(wave == 1 | wave == "Y1"| wave == "W1"){
    df.original <- df.original %>%
      dplyr::mutate(
        COUNTRY = recode_labels(COUNTRY, "COUNTRY"),
        COUNTRY = factor(COUNTRY),
        COUNTRY2 = COUNTRY
      )
    df.original <- df.original %>%
      select(!(any_of(cols_to_drop)))
  }
  ## ============================================================================================ ##
  ## ====== Restructure to "wide" data ========================================================== ##
  if(is.null(wave) | wave == 2 | wave == "Y2"| wave == "W2"){
    if(data.is.long){
      df.original <- gfs_data_to_wide(df.original,...)
    }

    df.original <- df.original %>%
      dplyr::mutate(
        COUNTRY = recode_labels(COUNTRY, "COUNTRY"),
        COUNTRY = factor(COUNTRY),
        COUNTRY2 = COUNTRY
      )

    ## DROP UNUSED COLUMNS
    cols_to_drop <- c(paste0(cols_to_drop, "_Y1"), paste0(cols_to_drop, "_Y2"))
    df.original <- df.original %>%
      select(!(any_of(cols_to_drop)))

    ## ========================================================================================== ##
    ## ====== MISSINGNESS INDICATOR ============================================================= ##
    # if the respondent gives a response to at least half the items in wave 2,
    # that respondent counts as "observed"
    cnames <- colnames(df.original)[stringr::str_detect(colnames(df.original), "_Y2")]
    cnames <- get_wave_flag(cnames, "Y2")
    ncol_w2 <- length(cnames)
    df.original <- df.original %>%
      dplyr::mutate(
        CASE_OBSERVED_Y2 = rowSums(across(any_of(cnames), \(x){
          !(is.na(x) | x %in% c(-98, 98, 99) )
        })),
        CASE_OBSERVED_Y2 = CASE_OBSERVED_Y2/ncol_w2,
        CASE_OBSERVED_Y2 = dplyr::case_when(CASE_OBSERVED_Y2 > 0.50 ~ 1, .default = 0)
      )
  }
  ## ============================================================================================ ##
  ## ====== CREATE COMPOSITES =================================================================== ##
  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(list.composites)) {
    if(wave == 1 | wave == "W1"| wave == "W2"){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES0"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD0"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC0"]]
    }
    if(is.null(wave) | wave == 2 | wave == "W2"| wave == "Y2"){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]
    }

    for (i in 1:length(LIST.OUTCOME.COMPOSITES)) {
      # create a temporary variable then rename
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "mean") {
        df.original <- df.original %>%
          dplyr::mutate(
            temp.var.i = rowMeans(
              dplyr::across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column())
                x
              }),
              na.rm = FALSE
            )
          )
      }
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "sum") {
        df.original <- df.original %>%
          dplyr::mutate(
            temp.var.i = rowSums(
              dplyr::across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column(), TRUE)
                x
              }),
              na.rm = FALSE
            )
          )
      }

      df.original <- df.original %>%
          dplyr::mutate(
            temp.var.i = recode_to_numeric(temp.var.i, COMPOSITE.VEC[i])
          )

      colnames(df.original)[length(colnames(df.original))] <- COMPOSITE.VEC[i]

    }
  }
  ## ============================================================================================ ##
  ## ====== VARIABLE RECODING =================================================================== ##
  {
    df.original <- df.original %>%
      mutate(
        dplyr::across(
          !(any_of(c("ID", "COUNTRY", "CASE_OBSERVED_Y2",
                     paste0("INCOME",c("","_Y1","_Y2")),
                     paste0("INCOME_QUINTILE",c("","_Y1","_Y2")),
                     paste0("SELFID1",c("")),
                     paste0("SELFID2",c("")),
                     paste0("DOI_RECRUIT",c("","_Y1","_Y2")),
                     paste0("DOI_ANNUAL",c("","_Y1"))
                     ))
          ), \(x){
            x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
            x <- recode_labels(x, cur_column())
            x <- recode_to_type(x, cur_column())

            x
          }, .names = "{.col}")
      )
  }
  ## ============================================================================================ ##
  ## ====== RECODE DEMOGRAPHIC VARIABLES ======================================================== ##
  {
    if(wave == 1 | wave == "W1"| wave == "Y1"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP = recode_labels(AGE, "AGE_GRP"),
          AGE_GRP = recode_to_type(AGE_GRP, "AGE_GRP"),
          RACE = recode_labels(SELFID1, "SELFID1"),
          RACE_PLURALITY = recode_race_to_plurality(RACE, COUNTRY2)
        )
    }
    if(is.null(wave) | wave == 2 | wave == "W2"| wave == "Y2"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP_Y1 = recode_labels(AGE_Y1, "AGE_GRP_Y1",...),
          AGE_GRP_Y1 = recode_to_type(AGE_GRP_Y1, "AGE_GRP_Y1"),
          AGE_GRP_Y2 = recode_labels(AGE_Y2, "AGE_GRP_Y2",...),
          AGE_GRP_Y2 = recode_to_type(AGE_GRP_Y2, "AGE_GRP_Y2"),
          RACE1 = recode_labels(SELFID1, "SELFID1",...),
          RACE2 = recode_labels(SELFID2, "SELFID2",...),
          RACE_PLURALITY1 = recode_race_to_plurality(RACE1, COUNTRY2),
          RACE_PLURALITY2 = recode_race_to_plurality(RACE2, COUNTRY2)
        )
    }
  }
  ## ============================================================================================ ##
  ## ====== RECODING INCOME DATA =============================================================== ##
  {
    if(wave == 1 | wave == "W1"| wave == "Y1"){
      income.quintiles <- df.original  %>%
        select(
          dplyr::all_of(c("ID", "COUNTRY", "INCOME", wgt, strata, psu))
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
        group_by(COUNTRY) %>%
        nest() %>%
        mutate(
          svy.data = map(data, \(x){
            svydesign(data=x, ids=~(!!as.name(psu)), strata=~(!!as.name(strata)), weights=~(!!as.name(wgt)))
          }),
          quintiles_w1 = map(svy.data, \(x){
            svyquantile(~INCOME, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          })
        ) %>%
        select(COUNTRY, quintiles_w1)

      df.original <- df.original %>%
        group_by(COUNTRY2) %>%
        nest() %>%
        mutate(
          data = map(data, \(x){
            cur.country = x$COUNTRY[1]

            tmp.quintiles <- income.quintiles %>%
              filter(COUNTRY == cur.country)

            tmp.quintiles.w1 <- tmp.quintiles$quintiles_w1[[1]][[1]]

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
                  INCOME_QUINTILE = case_when(
                    INCOME_QUINTILE == 5 ~ 1,
                    INCOME_QUINTILE %in% 1:4 ~ 0
                  )
                )
            }
            x
          }),
          data = map(data, \(x){
            x %>%
              mutate(
                INCOME = recode_labels(INCOME, "INCOME", ...)
              )
          })
        ) %>%
        unnest(c(data)) %>%
        ungroup()
    }
    if(is.null(wave) | wave == 2 | wave == "W2"| wave == "Y2"){

    income.quintiles <- df.original  %>%
      select(
        dplyr::all_of(c("ID", "COUNTRY", "INCOME_Y1", "INCOME_Y2", wgt, strata, psu))
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
      group_by(!!sym("COUNTRY")) %>%
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
      select(COUNTRY, quintiles_w1, quintiles_w2)

    df.original <- df.original %>%
      group_by(COUNTRY2) %>%
      nest() %>%
      mutate(
        data = map(data, \(x){
          cur.country = x$COUNTRY[1]

          tmp.quintiles <- income.quintiles %>%
            filter(COUNTRY == cur.country)

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
                INCOME_QUINTILE_Y1 = case_when(
                  INCOME_QUINTILE_Y1 == 5 ~ 1,
                  INCOME_QUINTILE_Y1 %in% 1:4 ~ 0
                ),
                INCOME_QUINTILE_Y2 = case_when(
                  INCOME_QUINTILE_Y2 == 5 ~ 1,
                  INCOME_QUINTILE_Y2 %in% 1:4 ~ 0
                )
              )
          }
          if(method.income == "quintiles.top.random"){
            set.seed(314150)
            x <- x %>%
              mutate(
                across(contains("INCOME_QUINTILE"), \(x){
                  xm <- which(is.na(x))
                  xnm <- which(!is.na(x))
                  x0 <- na.omit(x)
                  y <- case_when(
                    x == 5 ~ 1,
                    x %in% 1:4 ~ 0
                  )
                  ym <- which(is.na(y))
                  ynm <- which(!is.na(y))
                  y0 <- na.omit(y)
                  y.mean = mean(y0, na.rm=TRUE)
                  if(y.mean > 0.20){
                    n0 <- length(y0)
                    nq <- round(y.mean*n0,0) - round(0.2*n0,0) # number of cases to randomly fix to 0
                    y0[y0==1][sample(1:(length(y0[y0==1])), nq, replace = FALSE)] <- 0
                  }
                  if(y.mean < 0.20){
                    n0 <- length(y0)
                    nq <- round(0.2*n0,0) - round(y.mean*n0,0) # number of cases to randomly fix to 1 from those who are in the fourth quintile
                    x0[x0==4][sample(1:(length(x0[x0==4])), nq, replace = FALSE)] <- 5
                    y0 <- case_when(
                      x0 == 5 ~ 1,
                      x0 %in% 1:4 ~ 0
                    )
                  }
                  y[ynm] <- y0
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
              INCOME_QUINTILE_Y2 = factor(INCOME_QUINTILE_Y2)
            )
        })
      ) %>%
      unnest(c(data)) %>%
      ungroup()
    }
  }
  ## ============================================================================================ ##
  if(reverse.code.cont){
    df.original <- df.original %>%
      mutate(
        across(contains("LONELY"), \(x){
          x <- recode_to_type(x, "LONELY")
          x <- reorder_levels(x, "LONELY")
          x <- recode_to_numeric(x, "LONELY")
          x
        })
      )
  }
  if(to.numeric){

    # all.vars <- colnames(df.original)
    # for(v in all.vars){
    #   #cat(v, ":\t", class(df.original[[v]]),"\n")
    #   if(class(df.original[[v]]) == "factor" & str_detect(v, "AGE_GRP", negate=TRUE)){
    #     cat(v, "\n")
    #     x <- df.original[,v, drop=TRUE]
    #     if(str_detect(v, "COUNTRY", negate=TRUE)){
    #       x <- sub("\\..*", "", x)
    #       x = case_when(x == "(Missing)" ~ NA, .default = x) |>
    #         as.numeric()
    #     }
    #     if(str_detect(v, "BORN_COUNTRY")){
    #       x <- sub("\\..*", "", x)
    #       x = case_when(x == "(Missing)" ~ NA, .default = x) |>
    #         as.numeric()
    #     }
    #   }
    # }

    df.original <- df.original %>%
      mutate(
        across(where(is.factor), \(x){
          if(str_detect(cur_column(), "AGE_GRP", negate=TRUE)){
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
          }
          x
        }),
        CIGARETTES_BINARY_Y1 = case_when(
          CIGARETTES_Y1 == 0 ~ 0,
          CIGARETTES_Y1 %in% 1:97 ~ 1
        ),
        CIGARETTES_BINARY_Y2 = case_when(
          CIGARETTES_Y2 == 0 ~ 0,
          CIGARETTES_Y2 %in% 1:97 ~ 1
        ),
        MARITAL_STATUS_EVER_MARRIED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(2:5) ~ 1, MARITAL_STATUS_Y1 %in% c(1,6) ~ 0),
        MARITAL_STATUS_EVER_MARRIED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(2:5) ~ 1, MARITAL_STATUS_Y2 %in% c(1,6) ~ 0),
        MARITAL_STATUS_DIVORCED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(4) ~ 1, MARITAL_STATUS_Y1 %in% c(1:3,5:6) ~ 0),
        MARITAL_STATUS_DIVORCED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(4) ~ 1, MARITAL_STATUS_Y2 %in% c(1:3,5:6) ~ 0)
      )

    all.vars <- unique(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2"))
    for (tmp.var in  all.vars){
      # print(tmp.var)
      x <- df.original[, tmp.var, drop = TRUE]

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
      if (!is.null(x)) {
        df.original[, tmp.var] <- x
      }
    }
  }
  ## ============================================================================================ ##
  df.original
}
