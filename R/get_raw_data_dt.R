#' Estimate and Save Attrition Weights
#'
#' Estimate (save fitted model) and append attrition weights to data.frame
#'
#' @param file name of file to get data from
#' @param list.composites a lit of lists to construct composite variables
#' @param wave (default is wave 2) but can be coerced to use wave 1 data (use wave = 1 to appropriately utilize recoding of wave 1 data)
#' @param method.income method for how income, based on country specific labels, is recoded. Options include 'quintiles.num.fixed', 'quintiles.num.random', 'quintiles.top.fixed', 'quintiles.top.random', 'numeric'.
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
test_dt <- function(file, list.composites = NULL, wave = 2, method.income="quintiles.num.fixed", wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU", reverse.code.cont = FALSE, to.numeric = FALSE,...) {
  #file =  here::here(data.dir, dataset.name); list.composites = get_variable_codes('LIST.COMPOSITES'); wave = 3; add.whitespace = FALSE; reverse.code.cont = FALSE; wave = 3; method.income="quintiles.num.fixed"; wgt = "ANNUAL_WEIGHT_R3"; strata = "STRATA"; psu = "PSU"; to.numeric=FALSE; reverse.code.cont = FALSE

  # read using haven/readr as you already do
  df.original <- {
    if (stringr::str_detect(stringr::str_to_lower(file), ".sav")) {
      haven::read_spss(file) |> haven::zap_labels()
    } else if (stringr::str_detect(stringr::str_to_lower(file), ".dta")) {
      haven::read_dta(file) |> haven::zap_labels()
    } else if (stringr::str_detect(stringr::str_to_lower(file), ".csv")) {
      readr::read_csv(file)
    } else {
      stop("Unsupported file extension")
    }
  }

  # wrap in lazy_dt once; use this everywhere below
  df_dt <- lazy_dt(df.original, immutable = FALSE)

  ##

  ## columns to drop
  cols_to_drop <- c(
    "CNTRY_REL_BUD", "CNTRY_REL_CHI", "CNTRY_REL_CHR",
    "CNTRY_REL_HIN", "CNTRY_REL_ISL", "CNTRY_REL_JUD", "CNTRY_REL_SHI",
    paste0("REL",3:9),
    paste0("TEACHINGS_",1:15)
  )
  ##
    df_dt <- df_dt %>%
      mutate(
        COUNTRY = recode_labels(COUNTRY, "COUNTRY"),
        COUNTRY = factor(COUNTRY)
      )

    ## DROP UNUSED COLUMNS
    cols_to_drop <- c(cols_to_drop,
                      paste0(cols_to_drop, "_Y1"), paste0(cols_to_drop, "_Y2"),
                      paste0(cols_to_drop, "_Y3"), paste0(cols_to_drop, "_Y4"),
                      paste0(cols_to_drop, "_Y5"))
    df_dt <- df_dt %>%
      select(!(any_of(cols_to_drop)))

    ## ====== MISSINGNESS INDICATORS ============================================================ ##

    df_dt <- df_dt %>%
      mutate(
        across(contains("FULL_PARTIAL_"), \(x){
          case_when(!is.na(x) ~ 1, .default = 0)
        },.names = "CASE_OBSERVED_{.col}")
      )

    tmp <- colnames(df_dt)[str_detect(colnames(ddf_dt), "CASE_OBSERVED_FULL_PARTIAL")]
    tmp <- str_remove(tmp, "FULL_PARTIAL_")
    colnames(df_dt)[str_detect(colnames(df_dt), "CASE_OBSERVED_FULL_PARTIAL")] <- tmp

    df_dt <- df_dt %>%
      mutate(
        CASE_OBSERVED_ALL = rowSums(across(contains("CASE_OBSERVED_"))),
        CASE_OBSERVED_ALL = case_when(
          CASE_OBSERVED_ALL == max(CASE_OBSERVED_ALL) ~ 1,
          .default = 0
        )
      )
  ## ============================================================================================ ##
  ## ====== CREATE COMPOSITES =================================================================== ##
  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(list.composites)) {
    if(wave == 1 | wave == "W1"){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES0"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD0"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC0"]]
    }
    if( (wave %in% c(2:3) | wave %in% c("W2", "W3")) ){
      LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
      LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
      COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]
    }

    n <- names(LIST.OUTCOME.COMPOSITES)[1]
    for (n in names(LIST.OUTCOME.COMPOSITES)) {

      tvars <- LIST.OUTCOME.COMPOSITES[[n]]
      tcheck = unlist(lapply(tvars, function(x) x %in% colnames(df_dt)))
      if(all(tcheck)){
        # create a temporary variable then rename
        if (LIST.COMPOSITE.COMBINE.METHOD[[n]] == "mean") {
          df_dt <- df_dt %>%
            mutate(
              temp.var.i = rowMeans(
                across(all_of(LIST.OUTCOME.COMPOSITES[[n]]), \(x){
                  x <- case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                  x <- recode_to_type(x, cur_column())
                  x <- reorder_levels(x, cur_column())
                  x <- recode_to_numeric(x, cur_column())
                  x
                }),
                na.rm = FALSE
              )
            )
        }
        if (LIST.COMPOSITE.COMBINE.METHOD[[n]] == "sum") {
          df_dt <- df_dt %>%
            mutate(
              temp.var.i = rowSums(
                across(all_of(LIST.OUTCOME.COMPOSITES[[n]]), \(x){
                  x <- case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                  x <- recode_to_type(x, cur_column())
                  x <- reorder_levels(x, cur_column())
                  x <- recode_to_numeric(x, cur_column(), TRUE)
                  x
                }),
                na.rm = FALSE
              )
            )
        }

        df_dt <- df_dt %>%
            mutate(
              temp.var.i = recode_to_numeric(temp.var.i, COMPOSITE.VEC[n])
            )

        colnames(df_dt)[length(colnames(df_dt))] <- COMPOSITE.VEC[n]
      }

    }
  }
  ## ============================================================================================ ##
  ## ====== VARIABLE RECODING =================================================================== ##
  {
    df_dt <- df_dt %>%
      mutate(
        across(
          !(any_of(c("ID", "COUNTRY", "CASE_OBSERVED_Y2", "CASE_OBSERVED_Y3",
                     paste0("INCOME",c("","_Y1","_Y2","_Y3")),
                     paste0("INCOME_QUINTILE",c("","_Y1","_Y2","_Y3")),
                     paste0("SELFID1",c("")),
                     paste0("SELFID2",c("")),
                     paste0("DOI_RECRUIT",c("","_Y1","_Y2","_Y3")),
                     paste0("DOI_ANNUAL",c("","_Y1","_Y2","_Y3"))
                     ))
          ), \(x){
            x <- case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
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
      df_dt <- df_dt %>%
        mutate(
          AGE_GRP = recode_labels(AGE, "AGE_GRP"),
          AGE_GRP = recode_to_type(AGE_GRP, "AGE_GRP"),
          RACE = recode_labels(SELFID1, "SELFID1",...),
          RACE2 = recode_labels(SELFID2, "SELFID2",...),
          RACE_PLURALITY = recode_race_to_plurality(RACE, COUNTRY),
          RACE_PLURALITY2 = recode_race_to_plurality(RACE2, COUNTRY)
        )
    }
    if(wave == 2 | wave == "W2"| wave == "Y2"){
      df_dt <- df_dt %>%
        mutate(
          AGE_GRP_Y1 = recode_labels(AGE_Y1, "AGE_GRP_Y1",...),
          AGE_GRP_Y1 = recode_to_type(AGE_GRP_Y1, "AGE_GRP_Y1"),
          AGE_GRP_Y2 = recode_labels(AGE_Y2, "AGE_GRP_Y2",...),
          AGE_GRP_Y2 = recode_to_type(AGE_GRP_Y2, "AGE_GRP_Y2"),
          RACE = recode_labels(SELFID1, "SELFID1",...),
          RACE2 = recode_labels(SELFID2, "SELFID2",...),
          RACE_PLURALITY = recode_race_to_plurality(RACE, COUNTRY),
          RACE_PLURALITY2 = recode_race_to_plurality(RACE2, COUNTRY)
        )
    }
    if(wave == 3 | wave == "W3"| wave == "Y3"){
      df_dt <- df_dt %>%
        mutate(
          AGE_GRP_Y1 = recode_labels(AGE_Y1, "AGE_GRP_Y1",...),
          AGE_GRP_Y1 = recode_to_type(AGE_GRP_Y1, "AGE_GRP_Y1"),
          AGE_GRP_Y2 = recode_labels(AGE_Y2, "AGE_GRP_Y2",...),
          AGE_GRP_Y2 = recode_to_type(AGE_GRP_Y2, "AGE_GRP_Y2"),
          AGE_GRP_Y3 = recode_labels(AGE_Y3, "AGE_GRP_Y3",...),
          AGE_GRP_Y3 = recode_to_type(AGE_GRP_Y3, "AGE_GRP_Y3"),
          RACE = recode_labels(SELFID1, "SELFID",...),
          RACE2 = recode_labels(SELFID2, "SELFID2",...),
          RACE_PLURALITY = recode_race_to_plurality(RACE, COUNTRY),
          RACE_PLURALITY2 = recode_race_to_plurality(RACE2, COUNTRY)
        )
    }
  }
  ## ============================================================================================ ##
  ## ====== RECODING INCOME DATA =============================================================== ##
  {
    if(wave == 1 | wave == "W1"| wave == "Y1"){

        # step 1: clean income in a dtplyr chain
        df_dt <- df_dt %>%
          mutate(
            INCOME = case_when(
              INCOME < 0    ~ NA_real_,
              INCOME > 9900 ~ NA_real_,
              INCOME == 9900 ~ 0,
              TRUE ~ INCOME
            )
          )

        # materialize just the small subset we need for survey::svy*
        inc_sub <- df_dt %>%
          select(all_of(c("ID", "COUNTRY", "INCOME", wgt, strata, psu))) %>%
          as_tibble()

        # step 2: compute quintiles per country with survey
        income_quintiles <- inc_sub %>%
          group_by(COUNTRY) %>%
          group_map(~{
            x <- .x %>% filter(!is.na(.data[[wgt]]))
            dsgn <- survey::svydesign(
              ids    = as.formula(paste0("~", psu)),
              strata = as.formula(paste0("~", strata)),
              weights= as.formula(paste0("~", wgt)),
              data   = x
            )
            q <- survey::svyquantile(~INCOME, design = dsgn,
                                     quantiles = c(0.20, 0.40, 0.60, 0.80),
                                     na.rm = TRUE)
            tibble(
              COUNTRY = unique(.x$COUNTRY),
              q20 = as.numeric(q[["INCOME"]]["0.2"]),
              q40 = as.numeric(q[["INCOME"]]["0.4"]),
              q60 = as.numeric(q[["INCOME"]]["0.6"]),
              q80 = as.numeric(q[["INCOME"]]["0.8"])
            )
          }) %>%
          bind_rows()

        # turn cutpoints into a lazy_dt keyed by COUNTRY
        inc_q_dt <- lazy_dt(income_quintiles, immutable = TRUE)

        df_dt <- df_dt %>%
          left_join(inc_q_dt, by = "COUNTRY") %>%
          mutate(
            INCOME_QUINTILE = case_when(
              !is.na(INCOME) & INCOME >= q80 ~ 5L,
              !is.na(INCOME) & INCOME >= q60 & INCOME < q80 ~ 4L,
              !is.na(INCOME) & INCOME >= q40 & INCOME < q60 ~ 3L,
              !is.na(INCOME) & INCOME >= q20 & INCOME < q40 ~ 2L,
              !is.na(INCOME) & INCOME <  q20 ~ 1L,
              TRUE ~ NA_integer_
            )
          )

        if (method.income == "quintiles.top.fixed") {
          df_dt <- df_dt %>%
            mutate(
              INCOME_QUINTILE = case_when(
                INCOME_QUINTILE == 5L ~ 1L,
                INCOME_QUINTILE %in% 1L:4L ~ 0L,
                TRUE ~ NA_integer_
              )
            )
        }

        # we can drop the helper q20–q80 columns if not wanted downstream
        df_dt <- df_dt %>% select(-q20, -q40, -q60, -q80)

        # recode INCOME labels as in your original
        df_dt <- df_dt %>%
          mutate(
            INCOME = recode_labels(INCOME, "INCOME", ...)
          )
      }
    if(wave == 2 | wave == "W2"| wave == "Y2"){

    income.quintiles <- df.original  %>%
      select(
        all_of(c("ID", "COUNTRY", "INCOME_Y1", "INCOME_Y2", wgt, strata, psu))
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
          x <- x |> filter(!is.na(!!sym(wgt)))
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
      group_by(COUNTRY) %>%
      nest() %>%
      mutate(
        data = pmap(list(data,COUNTRY), \(x, grp){
          #cur.country = x$COUNTRY[1]

          tmp.quintiles <- income.quintiles %>%
            filter(COUNTRY == grp)

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
    if(wave == 3 | wave == "W3"| wave == "Y3"){

      income.quintiles <- df.original  %>%
        select(
          all_of(c("ID", "COUNTRY", "INCOME_Y1", "INCOME_Y2", "INCOME_Y3", wgt, strata, psu))
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
            x <- x |> filter(!is.na(!!sym(wgt)))
            svydesign(data=x, ids=~(!!as.name(psu)), strata=~(!!as.name(strata)), weights=~(!!as.name(wgt)))
          }),
          quintiles_w1 = map(svy.data, \(x){
            svyquantile(~INCOME_Y1, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          }),
          quintiles_w2 = map(svy.data, \(x){
            svyquantile(~INCOME_Y2, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          }),
          quintiles_w3 = map(svy.data, \(x){
            svyquantile(~INCOME_Y3, design=x, quantiles=c(0.20,0.40,0.60,0.80), na.rm = TRUE)
          })
        ) %>%
        select(COUNTRY, quintiles_w1, quintiles_w2, quintiles_w3)

      df.original <- df.original %>%
        group_by(COUNTRY) %>%
        nest() %>%
        mutate(
          data = pmap(list(data, COUNTRY), \(x, grp){

            tmp.quintiles <- income.quintiles %>%
              filter(COUNTRY == grp)

            tmp.quintiles.w1 <- tmp.quintiles$quintiles_w1[[1]][[1]]
            tmp.quintiles.w2 <- tmp.quintiles$quintiles_w2[[1]][[1]]
            tmp.quintiles.w3 <- tmp.quintiles$quintiles_w3[[1]][[1]]

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
                ),
                INCOME_QUINTILE_Y3 = case_when(
                  INCOME_Y3 >= tmp.quintiles.w3["0.8",1] ~ 5,
                  INCOME_Y3 >= tmp.quintiles.w3["0.6",1] & INCOME_Y3 < tmp.quintiles.w3["0.8",1] ~ 4,
                  INCOME_Y3 >= tmp.quintiles.w3["0.4",1] & INCOME_Y3 < tmp.quintiles.w3["0.6",1] ~ 3,
                  INCOME_Y3 >= tmp.quintiles.w3["0.2",1] & INCOME_Y3 < tmp.quintiles.w3["0.4",1] ~ 2,
                  INCOME_Y3 < tmp.quintiles.w3["0.2",1] ~ 1
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
                  ),
                  INCOME_QUINTILE_Y3 = case_when(
                    INCOME_QUINTILE_Y3 == 5 ~ 1,
                    INCOME_QUINTILE_Y3 %in% 1:4 ~ 0
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
                INCOME_Y3 = recode_labels(INCOME_Y3, "INCOME_Y3"),
                INCOME_QUINTILE_Y1 = recode_labels(INCOME_QUINTILE_Y1, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y2 = recode_labels(INCOME_QUINTILE_Y2, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y3 = recode_labels(INCOME_QUINTILE_Y3, "INCOME_QUINTILE"),
                INCOME_QUINTILE_Y1 = factor(INCOME_QUINTILE_Y1),
                INCOME_QUINTILE_Y2 = factor(INCOME_QUINTILE_Y2),
                INCOME_QUINTILE_Y3 = factor(INCOME_QUINTILE_Y3)
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
        CIGARETTES_BINARY_Y3 = case_when(
          CIGARETTES_Y3 == 0 ~ 0,
          CIGARETTES_Y3 %in% 1:97 ~ 1
        ),
        DRINKS_BINARY_Y1 = case_when(
          DRINKS_Y1 == 0 ~ 0,
          DRINKS_Y1 %in% 1:97 ~ 1
        ),
        DRINKS_BINARY_Y2 = case_when(
          DRINKS_Y2 == 0 ~ 0,
          DRINKS_Y2 %in% 1:97 ~ 1
        ),
        DRINKS_BINARY_Y3 = case_when(
          DRINKS_Y3 == 0 ~ 0,
          DRINKS_Y3 %in% 1:97 ~ 1
        ),
        MARITAL_STATUS_EVER_MARRIED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(2:5) ~ 1, MARITAL_STATUS_Y1 %in% c(1,6) ~ 0),
        MARITAL_STATUS_EVER_MARRIED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(2:5) ~ 1, MARITAL_STATUS_Y2 %in% c(1,6) ~ 0),
        MARITAL_STATUS_EVER_MARRIED_Y3 = case_when(MARITAL_STATUS_Y3 %in% c(2:5) ~ 1, MARITAL_STATUS_Y3 %in% c(1,6) ~ 0),
        MARITAL_STATUS_DIVORCED_Y1 = case_when(MARITAL_STATUS_Y1 %in% c(4) ~ 1, MARITAL_STATUS_Y1 %in% c(1:3,5:6) ~ 0),
        MARITAL_STATUS_DIVORCED_Y2 = case_when(MARITAL_STATUS_Y2 %in% c(4) ~ 1, MARITAL_STATUS_Y2 %in% c(1:3,5:6) ~ 0),
        MARITAL_STATUS_DIVORCED_Y3 = case_when(MARITAL_STATUS_Y3 %in% c(4) ~ 1, MARITAL_STATUS_Y3 %in% c(1:3,5:6) ~ 0)
      )

    all.vars <- unique(c(get_variable_codes("VARS.Y1"),
                         get_variable_codes("VARS.Y2"),
                         get_variable_codes("VARS.Y3")))
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
  #df.original
  df_out <- as_tibble(df_dt)
  df_out
}

