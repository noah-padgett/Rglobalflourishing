#' Impute Missing GFS Data
#'
#' A wrapper function to conducting imputation by country.
#'
#' @param data data.frame with raw data with missing coded as "(missing)", must be a flat data.frame with no nesting
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
#' The imputation automatically excludes variables that are 100% in some countries (e.g., APPROVE_GOVT in Egypt, ABUSED in Israel) and variables that are conditional on previous questions (e.g, REL3-9 and TEACHINGS1-15).
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

  data <- data %>%
    # need to remove all "composite"
    dplyr::select(!contains("COMPOSITE_")) %>%
    dplyr::mutate(COUNTRY2 = COUNTRY) %>%
    dplyr::group_by(COUNTRY) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, \(tmp.dat) {
        tmp.dat %>%
          dplyr::mutate(dplyr::across(
            !any_of(c(
              "COUNTRY2",
              "AGE_GRP",
              "AGE_GRP_Y1",
              "RACE",
              "RACE_PLURALITY",
              "RACE_PLURALITY_Y1",
              "CASE_MISSING_Y2"
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
      })
    )

  # country, character string
  # x, df (from above) to be filtered based on country
  run_imputation <- function(country, df.tmp){
    # filter, unnest and extract data
    df.tmp <- df.tmp %>%
      filter(COUNTRY == country) %>%
      ungroup() %>%
      unnest(c(data))

    cur.country <- as.character(df.tmp$COUNTRY2[1])
    print(paste0("Country: ", cur.country ))
    # check for variables will 100% missing
    comp.miss <- df.tmp %>%
      dplyr::summarise(N_total = n(), dplyr::across(dplyr::everything(), \(x) {
        sum(is.na(x)) / N_total
      }))
    comp.miss <- colnames(comp.miss)[comp.miss == 1.00]
    ## =================================================================== ##
    ## =================================================================== ##
    # passive imputation to get method vector
    pass.imp <- mice::mice(df.tmp, maxit = 0)
    tmp.dat <- pass.imp$data
    tmp.pred <- pass.imp$predictorMatrix
    tmp.meth <- pass.imp$method
    ## =================================================================== ##
    ## =================================================================== ##
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
      "ANNUAL_WEIGHT1",
      "ANNUAL_WEIGHT_C2",
      "ANNUAL_WEIGHT_L2",
      "ANNUAL_WEIGHT_R2",
      "RETENTION_WEIGHT_C",
      "RETENTION_WEIGHT_L",

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
      "COUNTRY2",
      "CASE_OBSERVED_Y2",
      "RACE",
      var.ignore0,
      paste0(var.ignore0, "_Y1"),
      paste0(var.ignore0, "_Y2")
    )
    ## =================================================================== ##
    var.class <- df.tmp %>%
      summarise(across(everything(), \(x) class(x)))
    var.n.unique <- df.tmp %>%
      summarise(across(everything(), \(x) length(na.omit(unique( x )))))
    tmp.meth[!(names(tmp.meth) %in% var.ignore)] <- "pmm"
    ## If you want to change to usine logreg/polyreg (much slower) change use.log.poly = TRUE
    if(use.log.poly){
      tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                 (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique == 2]) &
                 (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "logreg"
      tmp.meth[!(names(tmp.meth) %in% var.ignore) &
                 (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 2]) &
                 (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "polyreg"
    }
    # cart seems to never fail to throw errors --  use as default for may categoried factors
    tmp.meth[!(names(tmp.meth) %in% var.ignore) &
               (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 7]) &
               (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "cart"
    tmp.meth[names(tmp.meth) %in%
               c("NUM_CHILDREN", "NUM_CHILDREN_Y1", "NUM_CHILDREN_Y2", paste0("INCOME",c("","_Y1","_Y2")))
    ] <- "cart"
    tmp.meth[(names(tmp.meth) %in% var.ignore | names(tmp.meth) %in% comp.miss)] <- ""
    ## =================================================================== ##
    # Set up base predictor matrix
    # Minimal set of missingness predictors (**TO-DO**)
    if(is.null(pred.vars)){
      vars0 <- c(
        "ANNUAL_WEIGHT1",
        "MODE_RECRUIT",
        "MODE_ANNUAL",
        "STRATA",
        get_variable_codes("DEMOGRAPHIC.VARS"),
        get_variable_codes("RETROSPECTIVE.VARS"),
        "URBAN_RURAL"
      )
      pred.vars <- c(
        vars0,
        paste0(vars0,"_Y1")
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
    # predictor matrix:
    # rows: designate variable being imputed
    # cols: designate what variable(s) are used to predict row
    ## =================================================================== ##
    # NEXT, set up lag predictors
    tmp.vec <- c(get_variable_codes("OUTCOME.VEC"), "INCOME")
    tmp.vec[tmp.vec == "CIGARETTES_BINARY"] <- "CIGARETTES"
    tmp.vec <- tmp.vec[str_detect(tmp.vec, "COMPOSITE_", negate=TRUE)]
    i <- 1
    for(i in 1:length(tmp.vec)){
      t1 <- paste0(tmp.vec[i], "_Y1")
      t2 <- paste0(tmp.vec[i], "_Y2")
      if(t1 %in% colnames(tmp.pred) & t2 %in% colnames(tmp.pred)){
      	tmp.pred[t2,t1] <- 1
      	#print(tmp.pred[c(t1,t2),c(t1,t2)])
      }
      #tmp.pred[t1,][tmp.pred[t1,] == 1]
      #tmp.pred[t2,][tmp.pred[t2,] == 1]
    }
    ## =================================================================== ##
    fit.imp <- NULL
    try({
      # use futuremice if number of imputation is greater than 5 (should give a speed boost)
      if(Nimp > 5){
        fit.imp <- mice::futuremice(
          tmp.dat,
          m = Nimp,
          maxit = Miter,
          method = tmp.meth,
          predictorMatrix = tmp.pred,
          donors = 10,
          threshold = 2.0, # see https://github.com/amices/mice/issues/314 for threshold information
          n.core = round(future::availableCores()/2,0), # use half of available cores
          parallelseed = 31415
        )
      } else {
        fit.imp <- mice::mice(
          tmp.dat,
          m = Nimp,
          maxit = Miter,
          method = tmp.meth,
          predictorMatrix = tmp.pred,
          donors = 10,
          threshold = 2.0,
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
    ## save country-specific imputation result
    c.file.name <- paste0("imputed_data_obj_",cur.country,"_nimp_",Nimp,".RData")
    save(fit.imp, file = here::here(data.dir, c.file.name))
  }

  # data, a flat data.frame with no nesting
  # Save:
  # 1. separate files by country
  country_vec <- sort(as.character(unique(data$COUNTRY)))

  map(.x = country_vec,
      .f = ~run_imputation(country = .x, df.tmp = data))


}
