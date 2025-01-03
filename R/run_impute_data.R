#' Impute Missing GFS Data
#'
#' A wrapper function to conducting imputation by country.
#'
#' @param data data.frame with raw data with missing coded as "(missing)"
#' @param data.dir directory where to save the imputed data
#' @param Nimp number of imputed datasets to generated
#' @param Miter number of iteration per imputed dataset to use during imputatiuon (uses mice default)
#' @param ... other arguments passed to mice
#' @returns a nested (by country) data.frame of mice objects with completed imputed datasets ready
#' for extraction and recoding.
#' @examples {
#'   # TO-DO
#' }
#' @description
#' Additional details forthcoming....
#'
#' @export
run_impute_data <- function(data, data.dir, Nimp = 20, Miter = 5, ...) {
  df.imp <- data %>%
    # need to remove all "composite" variables prior to imputation, these will be recreated post-imputation
    dplyr::select(!contains("COMPOSITE_")) %>%
    dplyr::mutate(
      COUNTRY2 = COUNTRY
    ) %>%
    dplyr::group_by(COUNTRY) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, \(tmp.dat){
        tmp.dat %>%
          dplyr::mutate(
            dplyr::across(everything(), \(x){
              # check if system var
              if (!(cur_column() %in% c("ATTR_WGT", "WGT", "COUNTRY2"))) {
                x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                x <- dplyr::case_when(x %in% "(Missing)" ~ NA, .default = x)
                x <- recode_to_type(x, cur_column())
              }
              x
            }, .names = "{.col}"),
            dplyr::across(where(is.factor), droplevels)
          )
      }),
      passive.imp = purrr::map(data, \(x) mice::mice(x, maxit = 0)),
      imp.res = purrr::map(passive.imp, \(x) {
        tmp.dat <- x$data
        tmp.pred <- x$predictorMatrix
        tmp.meth <- x$method
        var.ignore <- c(
          "ID",
          "COUNTRY_W1", "WAVE_W1", "MODE_RECRUIT_W1", "MODE_ANNUAL_W1", "RECRUIT_TYPE_W1", "DOI_RECRUIT_W1", "DOI_ANNUAL_W1",
          "STRATA_W1", "PSU_W1", "FULL_PARTIAL_W1",
          "COUNTRY_W2", "WAVE_W2", "MODE_RECRUIT_W2", "MODE_ANNUAL_W2", "RECRUIT_TYPE_W2", "DOI_RECRUIT_W2", "DOI_ANNUAL_W2",
          "STRATA_W2", "PSU_W2", "FULL_PARTIAL_W2",
          "ANNUAL_WEIGHT1_W1", "ANNUAL_WEIGHT1_W2", "ATTR_WGT", "WGT", "COUNTRY2"
        )
        tmp.meth[!(names(tmp.meth) %in% var.ignore)] <- "pmm"
        tmp.meth[names(tmp.meth) %in% var.ignore] <- ""
        # missingness predictors (**TODO**)
        pred.vars <- c("WGT", "ATTR_WGT", "ANNUAL_WEIGHT1_W1") # , "MODE_RECRUIT_W1",  "MODE_ANNUAL_W1",
        # "AGE_W1", "GENDER_W1", "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1" , "BORN_COUNTRY_W1", "URBAN_RURAL_W1")
        keep.var <- keep_variable(pred.vars, tmp.dat)
        tmp.pred <- quickpred2(tmp.dat, mincor = 0.0, maxcor = 0.0, include = pred.vars[keep.var])

        # tmp.pred[,] <- 0
        # tmp.pred[pred.vars,] <- tmp.pred[,pred.vars] <- 1
        # diag(tmp.pred) <- 0

        fit.imp <- NULL
        try({
          fit.imp <- mice::mice(
            tmp.dat,
            m = Nimp,
            maxit = Miter,
            method = tmp.meth,
            predictorMatrix = tmp.pred,
            visitSequence = "monotone",
            ridge = 0.01
          ) # specifiying the "ridge" argument was necessary to avoid singularity in the predictor matrix
        })
        # the following is used just in case the above fails
        if (is.null(fit.imp)) {
          try({
            fit.imp <- mice::mice(
              tmp.dat,
              m = Nimp,
              maxit = Miter,
              method = "cart",
              visitSequence = "monotone",
              predictorMatrix = tmp.pred
            )
          })
        }

        fit.imp
      })
    )


  save(df.imp, file = paste0(data.dir, "gfs_imputed_data_test.RData"))
  df.imp
}
