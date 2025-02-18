#' Impute Missing GFS Data
#'
#' A wrapper function to conducting imputation by country.
#'
#' @param data data.frame with raw data with missing coded as "(missing)"
#' @param data.dir directory where to save the imputed data
#' @param Nimp number of imputed datasets to generated (default of 20)
#' @param Miter number of iteration per imputed dataset to use during imputatiuon (uses mice default of 5)
#' @param pred.vars (optional) character vector of variable names to to as potential predictors in missing data model (see details for more information)
#' @param file.name (optional) character string of the file name, must end in ".RData"
#' @param use.log.poly (FALSE) whether to use logistic/mutlinomial logistic regression for factor variables.
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
#' The imputation method for most variables is predictive mean matching (pmm). You can change this by using the use.log.poly option to use logistic & multinomial logistic regression for all factor (categorical) variables. Some variables use CART (classification and regression tree) method to help avoid singularities and errors in during the imputation stage. Such as factor/nominal variables with a lot of categories.
#'
#' For more information about pmm see https://stefvanbuuren.name/fimd/sec-pmm.html
#'
#' @export
run_impute_data <- function(data,
                            data.dir,
                            Nimp = 20,
                            Miter = 5,
                            pred.vars = NULL,
                            file.name = NULL,
                            use.log.poly = FALSE,
                            ...) {


  df.imp <- data %>%
    # need to remove all "composite"
    dplyr::select(!contains("COMPOSITE_")) %>%
    dplyr::mutate(COUNTRY2 = COUNTRY) %>%
    dplyr::group_by(COUNTRY) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, \(tmp.dat) {
      tmp.dat %>%
        dplyr::mutate(dplyr::across(
          !any_of(c(
            "ATTR_WGT",
            "COMPOSITE_WGT",
            "COUNTRY2",
            "AGE_GRP",
            "AGE_GRP_W1",
            "RACE",
            "RACE_PLURALITY",
            "RACE_PLURALITY_W1",
            "CASE_MISSING_W2"
          )),
          \(x) {
            x <- dplyr::case_when(x %in% get_missing_codes(dplyr::cur_column()) ~ NA,
                                  .default = x)
            x <- dplyr::case_when(x %in% "(Missing)" ~ NA, .default = x)
            x <- recode_to_type(x, dplyr::cur_column())
            x
          },
          .names = "{.col}"
        ),
        dplyr::across(where(is.factor), droplevels))
    }),
    imp.res = purrr::map(data, \(x) {
      print(paste0("Country: ", as.character(x$COUNTRY2[1])))
      # check for variables will 100% missing
      comp.miss <- x %>%
        dplyr::summarise(N_total = n(), dplyr::across(dplyr::everything(), \(x) {
          sum(is.na(x)) / N_total
        }))
      comp.miss <- colnames(comp.miss)[comp.miss == 1.00]
      # passive imputation to get method vector
      pass.imp <- mice::mice(x, maxit = 0)
      tmp.dat <- pass.imp$data
      tmp.pred <- pass.imp$predictorMatrix
      tmp.meth <- pass.imp$method
      var.ignore0 <- c(
        "COUNTRY",
        "WAVE",
        "MODE_RECRUIT",
        "MODE_ANNUAL",
        "RECRUIT_TYPE",
        "DOI_RECRUIT",
        "DOI_ANNUAL",
        "STRATA",
        "PSU",
        "FULL_PARTIAL",
        #"ANNUAL_WEIGHT1",
        #"ANNUAL_WEIGHT_C2",
        #"ANNUAL_WEIGHT_L2",
        #"ANNUAL_WEIGHT_R2",
        #"RETENTION_WEIGHT_C",
        #"RETENTION_WEIGHT_L",

        "AGE_GRP",
        "CNTRY_REL_BUD",
        "CNTRY_REL_CHI",
        "CNTRY_REL_CHR",
        "CNTRY_REL_HIN",
        "CNTRY_REL_ISL",
        "CNTRY_REL_JUD",
        "CNTRY_REL_SHI",
        paste0("TEACHINGS_", 1:15),
        paste0("REL", 3:9),
        paste0("REGION", 2:3)
      )
      var.ignore <- c(
        "ID",
        "ATTR_WGT",
        "COMPOSITE_WGT",
        "COUNTRY2",
        "CASE_OBSERVED_W2",
        "RACE",
        var.ignore0,
        paste0(var.ignore0, "_W1"),
        paste0(var.ignore0, "_W2")
      )

      var.class <- x %>%
        summarise(across(everything(), \(x) class(x)))
      var.n.unique <- x %>%
        summarise(across(everything(), \(x) length(na.omit(unique( x )))))
      tmp.meth[!(names(tmp.meth) %in% var.ignore)] <- "pmm"
      ## If you want to change to usine logreg/polyreg (much slower) change use.log.poly = TRUE
      if(!use.log.poly){
      	tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                 (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique == 2]) &
                 (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "logreg"
      	tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                 (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 2]) &
                 (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "polyreg"
      }
      #
      tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                 (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 7]) &
                 (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "cart"
      # tmp.meth[names(tmp.meth) %in% c("NUM_CHILDREN", "NUM_CHILDREN_W1", "NUM_CHILDREN_W2")] <- "cart"
      tmp.meth[(names(tmp.meth) %in% var.ignore | names(tmp.meth) %in% comp.miss)] <- ""
      # Minimal set of missingness predictors (**TODO**)
      if(is.null(pred.vars)){
      	vars0 <- c(
      	"ANNUAL_WEIGHT1",
        "MODE_RECRUIT",
        "MODE_ANNUAL",
        "AGE",
        "GENDER",
        "EDUCATION_3",
        "EMPLOYMENT",
        "MARITAL_STATUS",
        "RACE_PLURALITY" ,
        "BORN_COUNTRY",
        "URBAN_RURAL",
        "INCOME_QUINTILE"
        )

        pred.vars <- c(
          "COMPOSITE_WGT",
          "ATTR_WGT",
          vars0,
          paste0(vars0,"_W1")
        )
      }
      keep.var <- keep_variable(pred.vars, tmp.dat)

      exclude.var <- c(
      	var.ignore[var.ignore %in% colnames(tmp.dat)],
      	colnames(tmp.dat)[!(colnames(tmp.dat) %in% pred.vars[keep.var])]
      ) |> unique()

      tmp.pred <- quickpred2(
        tmp.dat,
        include = pred.vars[keep.var],
        exclude = exclude.var
      )
      # difficult variables: INCOME, EDUCATION (not edu_3), REGION, SELFID

      # tmp.pred[,] <- 0
      # tmp.pred[pred.vars,] <- tmp.pred[,pred.vars] <- 1
      # diag(tmp.pred) <- 0

      fit.imp <- NULL
      try({
      	# use futuremice if number of imputation is greater than 5 (speed boost)
      	if(Nimp > 5){
        fit.imp <- mice::futuremice(
          tmp.dat,
          m = Nimp,
          maxit = Miter,
          method = tmp.meth,
          predictorMatrix = tmp.pred,
          donors = 10,
          threshold = 1.0, # see https://github.com/amices/mice/issues/314 for threshold information
          n.core = round(future::availableCores()/2,0), # use half of available cores
          seed = 31415
        )
        } else {
        	fit.imp <- mice::mice(
          tmp.dat,
          m = Nimp,
          maxit = Miter,
          method = tmp.meth,
          predictorMatrix = tmp.pred,
          donors = 10,
          threshold = 1.0,
          seed = 31415
        )
        }
      })
      # the following is used just in case the above fails
      if (is.null(fit.imp)) {
        try({
          fit.imp <- mice::mice(
            tmp.dat,
            m = Nimp,
            maxit = Miter,
            method = "cart",
            predictorMatrix = tmp.pred,
          	seed = 31415
          )
        })
      }

      fit.imp
    }))

  if(is.null(file.name)){
  	file.name = "gfs_imputed_data_test.RData"
  }
  save(df.imp,
       file = here::here(data.dir, file.name))
  df.imp
}
