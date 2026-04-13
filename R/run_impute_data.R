#' Impute Missing GFS Data
#'
#' A wrapper function to conducting imputation by country.
#'
#' @param data data.frame with raw data with missing coded as "(missing)", must be a flat data.frame with no nesting
#' @param data.dir directory where to save the imputed data
#' @param Nimp number of imputed datasets to generated (default of 20)
#' @param Miter number of iteration per imputed dataset to use during imputatiuon (uses mice default of 5)
#' @param visitSequence (see mice, default is monotone)
#' @param pred.vars (optional) character vector of variable names to to as potential predictors in missing data model (see details for more information)
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
                            Miter = 2,
                            save.method = "combined",
                            visitSequence = "monotone",
                            pred.vars = NULL,
                            use.log.poly = FALSE,
                            includes.midyr = FALSE,
                            use.parallel = FALSE,
                            ...) {

  # check if data.dir exists
  if(!dir.exists(here(data.dir))){
    dir.create(here(data.dir))
  } else if(!dir.exists(here(data.dir, "imp"))){
    dir.create(here(data.dir, "imp"))
  }

  data <- data %>%
    # need to remove all "composite"
    dplyr::select(!contains("COMPOSITE_")) %>%
    dplyr::select(!contains("INCOME_QUINTILE")) %>%
    dplyr::group_by(COUNTRY) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, \(tmp.dat) {
        tmp.dat %>%
          dplyr::mutate(dplyr::across(
            !any_of(c(
              "AGE_GRP",
              "AGE_GRP_Y1",
              "RACE",
              "RACE_PLURALITY",
              "RACE_PLURALITY_Y1"
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

  # data, a flat data.frame with no nesting
  # Save:
  # 1. separate files by country
  country_vec <- sort(as.character(unique(data$COUNTRY)))

  #country_vec <- sort(as.character(unique(data$COUNTRY)))[-c(1:4)]
  # parallelization is over imputations within country
  if(save.method == "combined"){

    # x <- country_vec[1]
    walk(country_vec,\(x){
      run_imputation(country = x, df.tmp = data,
                     save.method = "combined",
                     Nimp = Nimp,
                     Miter = Miter,
                     visitSequence = visitSequence,
                     pred.vars = pred.vars,
                     use.log.poly = use.log.poly,
                     data.dir = data.dir,
                     use.parallel = use.parallel
                     )
    })
  }
  # parallelization is over imputations & country
  if(save.method == "separate"){
    imp.list <- expand.grid(country_vec, 1:Nimp)
    vec.iter <- 1:nrow(imp.list)
    vec.c <- imp.list$Var1
    vec.i <- imp.list$Var2

    #plan("multisession", workers = num_cores)
    #with_progress({
     # p <- progressor( along = c(vec.iter, nrow(imp.list)+1 )  )
      # furrr::future_pwalk(list(vec.iter, vec.c, vec.i), \(x,y,i){
      #
      #   load_packages()
      #
      #   Rglobalflourishing:::run_imputation(
      #     country = y, df.tmp = data, Nimp = i,
      #                  save.method = "separate",
      #                  Miter = Miter,
      #                  visitSequence = visitSequence,
      #                  pred.vars = pred.vars,
      #                  use.log.poly = use.log.poly,
      #     data.dir = data.dir)
      #
      #   p(sprintf("x= %s", x))
      # },.options = furrr_options(seed = TRUE))
      # })
      #future::resetWorkers(plan())
    with_progress({
      p <- progressor( along = c(vec.iter)  )
      pwalk(list(vec.iter, vec.c, vec.i), \(x,y,i){

        Rglobalflourishing:::run_imputation(
          country = y, df.tmp = data, Nimp = i,
          save.method = "separate",
          Miter = Miter,
          visitSequence = visitSequence,
          pred.vars = pred.vars,
          use.log.poly = use.log.poly,
          data.dir = data.dir)

        p(sprintf("x= %s", x))
        gc()
      })
    })


  }


}


# country, character string
# x, df (from above) to be filtered based on country
#' @keywords internal
run_imputation <- function(country, df.tmp,
                           save.method = "combined", # OR separate (separate files for each country & imp)
                           Nimp = Nimp,
                           Miter = Miter,
                           visitSequence = visitSequence,
                           pred.vars = pred.vars,
                           use.log.poly = use.log.poly,
                           data.dir = "data",
                           use.parallel = FALSE){
  # country = country_vec[1]
  # df.tmp = data
  # filter, unnest and extract data
  df.tmp <- df.tmp %>%
    filter(COUNTRY == country) %>%
    ungroup() %>%
    unnest(c(data)) |>
    select(-c(AGE_GRP_Y1:RACE_PLURALITY2))

  cur.country <- as.character(df.tmp$COUNTRY[1])
  print(paste0("Country: ", cur.country ))
  # check for variables will 100% missing
  comp.miss <- df.tmp %>%
    dplyr::summarise(N_total = n(), dplyr::across(dplyr::everything(), \(x) {
      sum(is.na(x)) / N_total
    }))
  comp.miss <- colnames(comp.miss)[comp.miss == 1.00]

  # get proportion attrit
  prop.attrit <- mean(df.tmp$CASE_OBSERVED_ALL)
  ## =================================================================== ##
  ## =================================================================== ##
  ## PRE-IMPUTE Variables:
  ## 1. AGE -- has a known structure and relatively easy
  ##   to obtain an approximate age that is reasonable + avoids i
  ##   imputing age as lower than previously observed.
  ##   Method.
  ##   1. get average date of interviews at each wave
  ##   2. compute time between last observed response
  ##   3. calculate age from difference since last observed
  ## 2. RECON_STATUS - gallup provided these statuses with lots' of missing...
  ## 3. REGION1_Y* - region1 is carried forward if missing.

  df.tmp <- df.tmp |>
    mutate(
      across(contains("RECON_STATUS"), \(x){
        case_when(
          x == 1 ~ 2, #"AFTER",
          x == 0 ~ 1, #"BEFORE",
          is.na(x) ~ 0
        )
      }),
      across(contains("RECON_STATUS"), ~ as.factor(.) )
    )

  # use simple mean imputation for wave 1 age
  if(any(str_detect(colnames(df.tmp), "_Y1"))){
    df.tmp <- df.tmp |>
      mutate(
        AGE_Y1 = case_when(
          is.na(AGE_Y1) ~ round(mean(AGE_Y1, na.rm=TRUE)),
          .default = AGE_Y1
        )
      )
  }

  if(any(str_detect(colnames(df.tmp), "_Y2"))){
    doi.w2.avg = mean(df.tmp$DOI_ANNUAL_Y2, na.rm=TRUE)
    df.tmp <- df.tmp |>
      mutate(
        AGE_Y2 = case_when(
          is.na(AGE_Y2) ~ round(AGE_Y1 + as.numeric(doi.w2.avg - DOI_ANNUAL_Y1)/365),
          .default = AGE_Y2
        ),
        REGION1_Y2 = case_when(
          is.na(REGION1_Y2) & !is.na(REGION1_Y1) ~ REGION1_Y1,
          .default = REGION1_Y2
        )
      )
  }
  if(any(str_detect(colnames(df.tmp), "_Y3"))){
    doi.w3.avg = mean(df.tmp$DOI_ANNUAL_Y3, na.rm=TRUE)
    df.tmp <- df.tmp |>
      mutate(
        AGE_Y3 = case_when(
          is.na(AGE_Y3) & !is.na(DOI_ANNUAL_Y2) ~ round(AGE_Y2 + as.numeric(doi.w3.avg - DOI_ANNUAL_Y2)/365),
          is.na(AGE_Y3) & is.na(DOI_ANNUAL_Y2) ~ round(AGE_Y1 + as.numeric(doi.w3.avg - DOI_ANNUAL_Y1)/365),
          .default = AGE_Y3
        ),
        REGION1_Y3 = case_when(
          is.na(REGION1_Y3) & !is.na(REGION1_Y2) ~ REGION1_Y2,
          is.na(REGION1_Y3) & !is.na(REGION1_Y1) ~ REGION1_Y1,
          .default = REGION1_Y3
        )
      )
  }

  ## =================================================================== ##
  ## =================================================================== ##
  # passive imputation to get method vector
  pass.imp <- mice::mice(df.tmp, maxit = 0,visitSequence = "monotone")
  tmp.dat <- pass.imp$data
  tmp.pred <- pass.imp$predictorMatrix
  tmp.meth <- pass.imp$method
  ## =================================================================== ##
  ## =================================================================== ##
  var.ignore0 <- c(
    "ID",
    "COUNTRY",
    "WAVE",
    'WAVE_MY',
    'MIDYEAR_TYPE_MY',
    "RECRUIT_TYPE",
    "DOI_RECRUIT",
    "DOI_ANNUAL",
    'DOI_MY',
    "STRATA",
    "PSU",
    "FULL_PARTIAL",
    "FULL_PARTIAL_MY",
    "ANNUAL_WEIGHT1",
    "ANNUAL_WEIGHT_C1",
    "ANNUAL_WEIGHT_C2",
    "ANNUAL_WEIGHT_L2",
    "ANNUAL_WEIGHT_R2",
    "ANNUAL_WEIGHT_C3",
    "ANNUAL_WEIGHT_L3",
    "ANNUAL_WEIGHT_R3",
    "RETENTION_WEIGHT_C",
    "RETENTION_WEIGHT_L",
    "RETENTION_WEIGHT_L_1M",
    "RETENTION_WEIGHT_L_1M2",

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
    paste0("REGION", 2:3),
    "POLITICAL_ID",
    "REGION2",
    "REGION3"
  )
  var.ignore <- c(
    "ID",
    "MODE_RECRUIT", "MODE_ANNUAL",
    "COUNTRY2",
    "CASE_OBSERVED_Y1",
    "CASE_OBSERVED_Y2",
    "CASE_OBSERVED_Y3",
    "CASE_OBSERVED_ALL",
    "RACE",
    var.ignore0,
    paste0(var.ignore0, "_Y1"),
    paste0(var.ignore0, "_Y2"),
    paste0(var.ignore0, "_Y3"),
    paste0(var.ignore0, "_MY")
  )
  ## =================================================================== ##
  var.class <- df.tmp %>%
    summarise(across(everything(), \(x) class(x)))
  var.n.unique <- df.tmp %>%
    summarise(across(everything(), \(x) length(na.omit(unique( x )))))
  tmp.meth0 <- tmp.meth
  tmp.meth[!(names(tmp.meth) %in% var.ignore )] <- "pmm"
  ## If you want to change to usine logreg/polyreg (much slower) change use.log.poly = TRUE
  if(use.log.poly){
    tmp.meth[!(names(tmp.meth) %in% var.ignore) &
               (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique == 2]) &
               (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "logreg"
    tmp.meth[!(names(tmp.meth) %in% var.ignore) &
               (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 2]) &
               (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "polyreg"
  }
  # cart seems to never fail to throw errors --  use as default for many categoried factors
  tmp.meth[!(names(tmp.meth) %in% var.ignore) &
             (names(tmp.meth) %in% colnames(var.n.unique)[var.n.unique > 7]) &
             (names(tmp.meth) %in% colnames(var.class)[var.class == "factor"])] <- "cart"
  tmp.meth[names(tmp.meth) %in%
             c(paste0("NUM_CHILDREN", c("","_Y1","_Y2","_Y3")),
               paste0("NUM_HOUSEHOLD", c("","_Y1","_Y2","_Y3")),
               paste0("INCOME",c("","_Y1","_Y2","_Y3")))
  ] <- "cart"
  tmp.meth["GENDER"] <- "sample" # randomly assign missing gender
  tmp.meth[(names(tmp.meth) %in% var.ignore | names(tmp.meth) %in% comp.miss)] <- ""
  #tmp.meth[tmp.meth0 == ""] <- ""
  ## For cases where there is only one value observed in the whole sample, fix all missing to that value.
  if(length(colnames(var.n.unique)[var.n.unique == 1 ]) > 0){
    tmp.meth[colnames(var.n.unique)[var.n.unique == 1 ]] <- ""
    ## update all unique zero-variance variable to the observed value -- fixed imputation
    df.tmp <- df.tmp |>
      mutate(
        across(all_of(colnames(var.n.unique)[var.n.unique == 1 ]), \(x){
          uval = unique(x)[1]
          case_when(
            is.na(x) ~ uval,
            .default = x
          )
        })
      )
  }

  ## =================================================================== ##
  # Set up base predictor matrix
  if(is.null(pred.vars)){
    pred.vars <- colnames(tmp.dat)
  }
  keep.var <- keep_variable(pred.vars, tmp.dat)
  exclude.var <- c(
    paste0("AGE", c("_Y2", "_Y3")), ## only include baseline age to avoid collinearity
    var.ignore[var.ignore %in% colnames(tmp.dat)],
    colnames(tmp.dat)[!(colnames(tmp.dat) %in% pred.vars[keep.var])]
  ) |> unique()
  tmp.pred <- quickpred2(
    data = tmp.dat,
    include = c("ANNUAL_WEIGHT_C1", pred.vars[keep.var]),
    exclude = exclude.var[exclude.var != "ANNUAL_WEIGHT_C1"],
    # so that min cor is at least 0.01 + a function of the proportion attritted --> lower for less informed countries
    # mice default is 0.10
    # NP: modified to force countries/groups with MORE attrition to use more variables in the
    #     predictor matrix to help align the Full MI and CCA results.
    maxcor = 0.99, mincor = 0.025 + (0.10 * prop.attrit),
    check.cor.bounds.after.include = TRUE
  )
  # predictor matrix:
  # rows: designate variable being imputed
  # cols: designate what variable(s) are used to predict row
  ## =================================================================== ##
  # To help (slightly) speed, set the following conditions
  # variables that do not need to be imputed:
  do.not.impute <- c(
    'COUNTRY', 'ID', 'WAVE_Y1', 'WAVE_Y2', 'WAVE_Y3', 'MODE_RECRUIT', 'MODE_ANNUAL', 'RECRUIT_TYPE', 'DOI_RECRUIT_Y1', 'DOI_ANNUAL_Y1', 'DOI_ANNUAL_Y2', 'DOI_ANNUAL_Y3',
    paste0("POLITICAL_ID",c("_Y1", "_Y2", "_Y3")),
    paste0("REGION2",c("_Y1", "_Y2", "_Y3")),
    paste0("REGION3",c("_Y1", "_Y2", "_Y3")),
    'RECON_STATUS_Y1', 'RECON_STATUS_Y2', 'RECON_STATUS_Y3', 'STRATA', 'PSU', 'ANNUAL_WEIGHT_C1', 'ANNUAL_WEIGHT_C2', 'ANNUAL_WEIGHT_L2', 'ANNUAL_WEIGHT_R2', 'ANNUAL_WEIGHT_C3', 'ANNUAL_WEIGHT_L3', 'ANNUAL_WEIGHT_R3', 'FULL_PARTIAL_Y1', 'FULL_PARTIAL_Y2', 'FULL_PARTIAL_Y3', 'CASE_OBSERVED_Y1', 'CASE_OBSERVED_Y2', 'CASE_OBSERVED_Y3', 'CASE_OBSERVED_ALL'
  )
  # just some double-checking that these variables are imputed / no predictors
  tmp.pred[do.not.impute,] <- 0
  tmp.meth[do.not.impute] <- ""

  # Longitudinal design incorporation

  # 1. Wave 3 & Wave 2 cannot predict wave 1
  tr <- c(colnames(tmp.pred)[str_detect(colnames(tmp.pred), "_Y1")], "GENDER", "SELFID2", "SELFID1")
  tc <- colnames(tmp.pred)[str_detect(colnames(tmp.pred), "_Y2") | str_detect(colnames(tmp.pred), "_Y3")]
  tmp.pred[tr,tc] <- 0 # all t1 (rows) cannot be predicted by t2 or t3 (cols)

  ## print out file
  #tmp.pred1 <- tmp.pred
  #tmp.pred1 <- tmp.pred1[pass.imp$visitSequence, pass.imp$visitSequence]
  #write_xlsx(tmp.pred1, file="test/ignore/data/imp/pred_matrix_Nigeria2.xlsx", col_names=TRUE, row_names=TRUE)

  # NEXT, set up lag predictors
  # tmp.vec <- c(get_variable_codes("OUTCOME.VEC"), "INCOME")
  # tmp.vec[tmp.vec == "CIGARETTES_BINARY"] <- "CIGARETTES"
  # tmp.vec <- tmp.vec[str_detect(tmp.vec, "COMPOSITE_", negate=TRUE)]
  #
  # ## FOR WAVE 1 & WAVE 2 VARIABLES
  # i <- 1
  # t1 <- c()
  # t2 <- c()
  # for(i in 1:length(tmp.vec)){
  #   t10 <- paste0(tmp.vec[i], "_Y1")
  #   t20 <- paste0(tmp.vec[i], "_Y2")
  #   if(t10 %in% colnames(tmp.pred) & t20 %in% colnames(tmp.pred)){
  #     t1 <- c(t1,t10)
  #     t2 <- c(t2,t20)
  #   	#print(tmp.pred[c(t1,t2),c(t1,t2)])
  #   }
  #   #tmp.pred[t1,][tmp.pred[t1,] == 1]
  #   #tmp.pred[t2,][tmp.pred[t2,] == 1]
  # }
  # tmp.pred[t2,t1] <- 1
  #
  # ## 3 WAVES VARIABLES
  # if(any(str_detect(colnames(tmp.pred), "Y3"))){
  #   # For W1 -> W3
  #   i <- 1
  #     t1 <- c()
  #     t3 <- c()
  #     for(i in 1:length(tmp.vec)){
  #       t10 <- paste0(tmp.vec[i], "_Y1")
  #       t30 <- paste0(tmp.vec[i], "_Y3")
  #       if(t10 %in% colnames(tmp.pred) & t30 %in% colnames(tmp.pred)){
  #         t1 <- c(t1,t10)
  #         t3 <- c(t3,t30)
  #       }
  #     }
  #     tmp.pred[t3,t1] <- 1
  #
  #     # For W2 -> W3
  #     i <- 1
  #     t2 <- c()
  #     t3 <- c()
  #     for(i in 1:length(tmp.vec)){
  #       t20 <- paste0(tmp.vec[i], "_Y2")
  #       t30 <- paste0(tmp.vec[i], "_Y3")
  #       if(t20 %in% colnames(tmp.pred) & t30 %in% colnames(tmp.pred)){
  #         t2 <- c(t2,t20)
  #         t3 <- c(t3,t30)
  #       }
  #     }
  #     tmp.pred[t3,t2] <- 1
  #
  #     # For W3 -> W2 : accounts for people for have missing at wave 2 but not wave 3.
  #     # just "flip" the above
  #     tmp.pred[t2,t3] <- 1
  # }
  # ## =================================================================== ##
  # ## mid-year items
  # if(includes.midyr){
  #   tmy <- c('ACHIEVING_MY', 'BEAUTY_MY', 'DILIGENT_MY', 'ENGAGE_ARTS_MY', 'FOOD_INSECURE_MY', 'GOOD_PERSON_MY', 'GOOD_RELATION_MY', 'HAPPY_IMPORT_MY', 'HEALTHY_MY', 'MEANINGFUL_MY', 'MIND_FOCUSED_MY', 'MONEY_MY', 'NATURE_MY', 'REL_LIFE_MY', 'TIME_MEDIA_MY')
  #   tmp.pred[tmy,t1] <- 1
  # }
  ## =================================================================== ##
  fit.imp <- NULL
  try({
    # use futuremice if number of imputation is greater than 5 (should give a speed boost)
    if(Nimp > 5 & save.method == "combined" & use.parallel){
      fit.imp <- mice::futuremice(
        tmp.dat,
        m = Nimp,
        maxit = Miter,
        visitSequence = visitSequence,
        method = tmp.meth,
        predictorMatrix = tmp.pred,
        donors = 3,
        threshold = 1.0, # see https://github.com/amices/mice/issues/314 for threshold information
        ridge = 0.1,
        n.core = future::availableCores(),
        parallelseed = 31415
      )
      future::resetWorkers(plan())
    } else if(save.method == "separate"){
      fit.imp <- mice::mice(
        tmp.dat,
        m = 1,
        maxit = Miter,
        visitSequence = visitSequence,
        method = tmp.meth,
        predictorMatrix = tmp.pred,
        donors = 3,
        threshold = 1.0,
        ridge = 0.1,
        seed = Nimp # allows for varying seed
      )
    } else {
      fit.imp <- mice::mice(
        tmp.dat,
        m = Nimp,
        maxit = Miter,
        visitSequence = visitSequence,
        method = tmp.meth,
        predictorMatrix = tmp.pred,
        donors = 3,
        threshold = 1.0, #0.99,
        ridge = 0.1,
        seed = 31415
      )
    }
  })
  ## save country-by-imputation-specific files
  c.file.name <- paste0("imputed_data_obj_",cur.country,"_",save.method,"_imp_",Nimp,".RData")
  save(fit.imp, file = here::here(data.dir, "imp", c.file.name))
  # memory saver
  remove(tmp.dat, fit.imp)
  gc()
}
