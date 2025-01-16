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
#' The imputation automatically excludes variables that are 100% in some countries (e.g., APPROVE_GOVT in Egypt) and variables that are conditional on previous questions (e.g, REL3-9 and TEACHINGS1-15).
#'
#' The imputation method for all variables is predictive mean matching (pmm).
#'
#' @export
run_impute_data <- function(data, data.dir, Nimp = 20, Miter = 5, ...) {
  df.imp <- data %>%
    # need to remove all "composite"
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
            dplyr::across(!c(ATTR_WGT, WGT, COUNTRY2, AGE_GRP_W1, RACE, RACE_PLURALITY_W1, CASE_MISSING_W2), \(x){
                x <- dplyr::case_when(x %in% get_missing_codes(dplyr::cur_column()) ~ NA, .default = x)
                x <- dplyr::case_when(x %in% "(Missing)" ~ NA, .default = x)
                x <- recode_to_type(x, dplyr::cur_column())
              x
            }, .names = "{.col}"),
            dplyr::across(where(is.factor), droplevels)
          )
      }),
      imp.res = purrr::map(data, \(x) {
        print(paste0("Country: ", as.character(x$COUNTRY2[1])))
        # check for variables will 100% missing
        comp.miss <- x %>%
          dplyr::summarise(
            N_total = n(),
            dplyr::across(dplyr::everything(), \(x){
              sum(is.na(x))/N_total
            })
          )
        comp.miss <- colnames(comp.miss)[comp.miss == 1.00]
        # passive imputation to get method vector
        pass.imp <- mice::mice(x, maxit = 0)
        tmp.dat <- pass.imp$data
        tmp.pred <- pass.imp$predictorMatrix
        tmp.meth <- pass.imp$method
        var.ignore0 <- c("COUNTRY", "WAVE", "MODE_RECRUIT", "MODE_ANNUAL", "RECRUIT_TYPE", "DOI_RECRUIT", "DOI_ANNUAL","STRATA", "PSU", "FULL_PARTIAL", "ANNUAL_WEIGHT1", "AGE_GRP", "CNTRY_REL_BUD", "CNTRY_REL_CHI", "CNTRY_REL_CHR", "CNTRY_REL_HIN", "CNTRY_REL_ISL", "CNTRY_REL_JUD","CNTRY_REL_SHI", paste0("TEACHINGS_",1:15),paste0("REL",3:9), paste0("REGION"))
        var.ignore <- c(
          "ID", "ATTR_WGT", "WGT", "COUNTRY2", "CASE_MISSING_W2", "RACE",
          paste0(var.ignore0, "_W1"), paste0(var.ignore0, "_W2")
        )

        var.class <- x %>%
          summarise(
            across(everything(), \(x) class(x))
          )
        var.n.unique <- x %>%
          summarise(
            across(everything(), \(x) length(na.omit(unique(x))))
          )
        tmp.meth[!(names(tmp.meth) %in% var.ignore)] <- "pmm"
        tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                   (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique == 2]) &
                   (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "logreg"
        tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                   (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 2]) &
                   (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "polyreg"
        tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                   (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 7]) &
                   (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "cart"
        #tmp.meth[!(names(tmp.meth) %in% var.ignore) & (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "cart"
        tmp.meth[(names(tmp.meth) %in% var.ignore | names(tmp.meth) %in% comp.miss) ] <- ""
        # Minimal set of missingness predictors (**TODO**)
        pred.vars <- c(
          "WGT", "ATTR_WGT", "ANNUAL_WEIGHT1_W1", "MODE_RECRUIT_W1",  "MODE_ANNUAL_W1",
          "AGE_W1", "GENDER_W1", "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1",
          "RACE_PLURALITY_W1" , "BORN_COUNTRY_W1", "URBAN_RURAL_W1"
        )
        keep.var <- keep_variable(pred.vars, tmp.dat)
        tmp.pred <- quickpred2(
          tmp.dat,
          mincor = 0.225, maxcor = 0.99, minpuc = 0.33,
          include = pred.vars[keep.var],
          exclude = c("APPROVE_GOVT", "SAY_IN_GOVT")
        )
        # difficult variables: INCOME, EDUCATION (not edu_3), REGION, SELFID

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
            visitSequence = "monotone"
          )
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


  save(df.imp, file = here::here(data.dir, "gfs_imputed_data_test.RData"))
  df.imp
}
