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
gfs_get_labelled_raw_data <- function(file, list.composites = NULL, wave = 2, method.income="quintiles", .test=FALSE, wgt = "ANNUAL_WEIGHT1_W1", strata = "STRATA_W1", psu = "PSU_W1", ...) {
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
        COUNTRY = recode_labels(COUNTRY, "COUNTRY"),
        COUNTRY = factor(COUNTRY),
        COUNTRY2 = COUNTRY
      )

    ## ========================================================================================== ##
    ## ====== MISSINGNESS INDICATOR ============================================================= ##
    # if the respondent gives a response to at least half the items in wave 2,
    # that respondent counts as "observed"
    cnames <- colnames(df.original)[stringr::str_detect(colnames(df.original), "_W2")]
    cnames <- get_wave_flag(cnames, "W2")
    ncol_w2 <- length(cnames)
    df.original <- df.original %>%
      dplyr::mutate(
        CASE_OBSERVED_W2 = rowSums(across(any_of(cnames), \(x){
          !is.na(x)
        })),
        CASE_OBSERVED_W2 = CASE_OBSERVED_W2/ncol_w2,
        CASE_OBSERVED_W2 = dplyr::case_when(CASE_OBSERVED_W2 > 0.50 ~ 1, .default = 0)
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
    df.original <- df.original %>%
      mutate(
        dplyr::across(
          !(any_of(c("ID", "COUNTRY", "CASE_OBSERVED_W2",
                     paste0("INCOME",c("","_W1","_W2")),
                     paste0("INCOME_QUINTILE",c("","_W1","_W2")),
                     paste0("SELFID1",c("","_W1","_W2")),
                     paste0("SELFID2",c("","_W1","_W2"))
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
    if(wave == 1 | wave == "W1"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP = recode_labels(AGE, "AGE_GRP"),
          AGE_GRP = recode_to_type(AGE_GRP, "AGE_GRP"),
          RACE = recode_labels(SELFID1, "SELFID1"),
          RACE_PLURALITY = recode_race_to_plurality(RACE, COUNTRY2)
        )
    }
    if(is.null(wave) | wave == 2 | wave == "W2"){
      df.original <- df.original %>%
        mutate(
          AGE_GRP_W1 = recode_labels(AGE_W1, "AGE_GRP_W1"),
          AGE_GRP_W1 = recode_to_type(AGE_GRP_W1, "AGE_GRP_W1"),
          AGE_GRP_W2 = recode_labels(AGE_W2, "AGE_GRP_W2"),
          AGE_GRP_W2 = recode_to_type(AGE_GRP_W2, "AGE_GRP_W2"),
          RACE1_W1 = recode_labels(SELFID1_W1, "SELFID1_W1"),
          RACE2_W1 = recode_labels(SELFID2_W1, "SELFID2_W1"),
          RACE_PLURALITY1_W1 = recode_race_to_plurality(RACE1_W1, COUNTRY2),
          RACE_PLURALITY2_W1 = recode_race_to_plurality(RACE2_W1, COUNTRY2)
        )
    }
  }
  ## ============================================================================================ ##
  ## ====== RECODING INCOME DATA =============================================================== ##
  {
    if(wave == 1 | wave == "W1"){
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
    if(is.null(wave) | wave == 2 | wave == "W2"){

    income.quintiles <- df.original  %>%
      select(
        dplyr::all_of(c("ID", "COUNTRY", "INCOME_W1", "INCOME_W2", wgt, strata, psu))
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
              INCOME_W1 = recode_labels(INCOME_W1, "INCOME_W1"),
              INCOME_W2 = recode_labels(INCOME_W2, "INCOME_W2"),
              INCOME_QUINTILE_W1 = recode_labels(INCOME_QUINTILE_W1, "INCOME_QUINTILE"),
              INCOME_QUINTILE_W2 = recode_labels(INCOME_QUINTILE_W2, "INCOME_QUINTILE"),
              INCOME_QUINTILE_W1 = factor(INCOME_QUINTILE_W1),
              INCOME_QUINTILE_W2 = factor(INCOME_QUINTILE_W2)              
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
