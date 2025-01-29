#' Estimate and Save Attrition Weights
#'
#' Estimate (save fitted model) and append attrition weights to data.frame
#'
#' @param file name of file to get data from
#' @param list.composites a lit of lists to construct composite variables
#' @param wave (default is wave 2) but can be coerced to use wave 1 data (use wave = 1 to appropriately utilize recoding of wave 1 data)
#' @param method.income method for how income, based on country specific labels, is recoded. Options include 'quintiles.num.fixed', 'quintiles.num.random', 'quintiles.top.fixed', ''quintiles.top.random', 'numeric'.
#' @param .test (for internal use only)
#' @param ... other arguments
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_get_labelled_raw_data <- function(file, list.composites = NULL, wave = 2, method.income="quintiles.top.fixed", .test=FALSE, ...) {
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

  if(wave == 1 | wave == "W1"){
    df.original <- df.original %>%
      dplyr::mutate(
        COUNTRY = recode_labels(COUNTRY, "COUNTRY"),
        COUNTRY = factor(COUNTRY),
        COUNTRY2 = COUNTRY
      )
  }
  ## ============================================================================================ ##
  ## ====== Restructure to "wide" data ========================================================== ##
  if(is.null(wave) | wave == 2 | wave == "W2"){
    df.original <- gfs_data_to_wide(df.original, test=.test)

    df.original <- df.original %>%
      dplyr::mutate(
        COUNTRY = recode_labels(COUNTRY_W1, "COUNTRY_W1"),
        COUNTRY = factor(COUNTRY),
        COUNTRY2 = COUNTRY
      )

    ## ============================================================================================ ##
    ## ====== MISSINGNESS INDICATOR =============================================================== ##
    ncol_w2 <- length(colnames(df.original)[stringr::str_detect(colnames(df.original), "_W2")])
    df.original <- df.original %>%
      dplyr::mutate(
        CASE_MISSING_W2 = rowSums(across(contains("_W2"), \(x){
          is.na(x)
        })),
        CASE_MISSING_W2 = CASE_MISSING_W2/ncol_w2,
        CASE_MISSING_W2 = dplyr::case_when(CASE_MISSING_W2 > 0.75 ~ 1, .default = 0)
      )
  }
  ## ============================================================================================ ##
  ## ====== CREATE COMPOSITES =================================================================== ##
  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(list.composites)) {
    LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
    LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
    COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]

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
      colnames(df.original)[length(colnames(df.original))] <- COMPOSITE.VEC[i]
    }
  }
  ## ============================================================================================ ##
  ## ====== VARIABLE RECODING =================================================================== ##
  {
    df.original0 <- df.original %>%
      mutate(
        dplyr::across(
          c(contains("BORN_COUNTRY"),
            !(contains("COMPOSITE_") |
                contains("SELFID") |
                contains("COUNTRY") |
                contains("INCOME_QUINTILE") |
                any_of(c("INCOME", "INCOME_W1, INCOME_W2")) |
                CASE_MISSING_W2
              )
          ), \(x){
            x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
            x <- recode_labels(x, cur_column(), ...)
            x <- recode_to_type(x, cur_column())
            x
          }, .names = "{.col}")
      )
  }
  ## ============================================================================================ ##
  ## ====== RECODE DEMOGRAPHIC VARIABLES ======================================================== ##
  {
    if(wave == 1 | wave == "W1"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP = recode_labels(AGE, "AGE_GRP", ...),
          AGE_GRP = recode_to_type(AGE_GRP, "AGE_GRP"),
          RACE = recode_labels(SELFID1, "SELFID1", ...),
          RACE_PLURALITY_ = recode_race_to_plurality(RACE, COUNTRY2)
        )
    }
    if(is.null(wave) | wave == 2 | wave == "W2"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP_W1 = recode_labels(AGE_W1, "AGE_GRP_W1", ...),
          AGE_GRP_W1 = recode_to_type(AGE_GRP_W1, "AGE_GRP_W1"),
          AGE_GRP_W2 = recode_labels(AGE_W2, "AGE_GRP_W2", ...),
          AGE_GRP_W2 = recode_to_type(AGE_GRP_W2, "AGE_GRP_W2"),
          RACE_W1 = recode_labels(SELFID1_W1, "SELFID1_W1", ...),
          RACE_W2 = recode_labels(SELFID1_W2, "SELFID1_W2", ...),
          RACE_PLURALITY_W1 = recode_race_to_plurality(RACE_W1, COUNTRY2),
          RACE_PLURALITY_W2 = recode_race_to_plurality(RACE_W2, COUNTRY2)
        )
    }
  }
  ## ============================================================================================ ##
  ## ====== RECODING INCOME DATA =============================================================== ##
  {
    if(wave == 1 | wave == "W1"){

      income.quintiles <- df.original  %>%
        select(COUNTRY, INCOME, ANNUAL_WEIGHT1, STRATA, PSU) %>%
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
            svydesign(data=x, ids=~PSU, strata=~STRATA, weight=~ANNUAL_WEIGHT1)
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
                  INCOME >= tmp.quintiles.w1["0.6",1] & INCOME_W1 < tmp.quintiles.w1["0.8",1] ~ 4,
                  INCOME >= tmp.quintiles.w1["0.4",1] & INCOME_W1 < tmp.quintiles.w1["0.6",1] ~ 3,
                  INCOME >= tmp.quintiles.w1["0.2",1] & INCOME_W1 < tmp.quintiles.w1["0.4",1] ~ 2,
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
    if(is.null(wave) | wave == 2 | wave == "W2"){

    income.quintiles <- df.original  %>%
      select(COUNTRY, INCOME_W1, INCOME_W2, ANNUAL_WEIGHT1_W1, STRATA_W1, PSU_W1) %>%
      mutate(
        across(INCOME_W1:INCOME_W2,\(x){
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
          svydesign(data=x, ids=~PSU_W1, strata=~STRATA_W1, weight=~ANNUAL_WEIGHT1_W1)
        }),
        quintiles_w1 = map(svy.data, \(x){
          svyquantile(~INCOME_W1, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
        }),
        quintiles_w2 = map(svy.data, \(x){
          svyquantile(~INCOME_W2, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
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
              INCOME_QUINTILE_W1 = case_when(
                INCOME_W1 >= tmp.quintiles.w1["0.8",1] ~ 5,
                INCOME_W1 >= tmp.quintiles.w1["0.6",1] & INCOME_W1 < tmp.quintiles.w1["0.8",1] ~ 4,
                INCOME_W1 >= tmp.quintiles.w1["0.4",1] & INCOME_W1 < tmp.quintiles.w1["0.6",1] ~ 3,
                INCOME_W1 >= tmp.quintiles.w1["0.2",1] & INCOME_W1 < tmp.quintiles.w1["0.4",1] ~ 2,
                INCOME_W1 < tmp.quintiles.w1["0.2",1] ~ 1
              ),
              INCOME_QUINTILE_W2 = case_when(
                INCOME_W2 >= tmp.quintiles.w2["0.8",1] ~ 5,
                INCOME_W2 >= tmp.quintiles.w2["0.6",1] & INCOME_W2 < tmp.quintiles.w2["0.8",1] ~ 4,
                INCOME_W2 >= tmp.quintiles.w2["0.4",1] & INCOME_W2 < tmp.quintiles.w2["0.6",1] ~ 3,
                INCOME_W2 >= tmp.quintiles.w2["0.2",1] & INCOME_W2 < tmp.quintiles.w2["0.4",1] ~ 2,
                INCOME_W2 < tmp.quintiles.w2["0.2",1] ~ 1
              )
            )
          if(method.income=="quintiles.top.fixed"){
            x <- x %>%
              mutate(
                INCOME_QUINTILE_W1 = case_when(
                  INCOME_QUINTILE_W1 == 5 ~ 1,
                  INCOME_QUINTILE_W1 %in% 1:4 ~ 0
                ),
                INCOME_QUINTILE_W2 = case_when(
                  INCOME_QUINTILE_W2 == 5 ~ 1,
                  INCOME_QUINTILE_W2 %in% 1:4 ~ 0
                )
              )
          }
          x
        }),
        data = map(data, \(x){
          x %>%
            mutate(
              INCOME_W1 = recode_labels(INCOME_W1, "INCOME_W1", ...),
              INCOME_W2 = recode_labels(INCOME_W2, "INCOME_W2", ...)
            )
        })
      ) %>%
      unnest(c(data)) %>%
      ungroup()
    }
  }
  ## ============================================================================================ ##
  df.original
}
