load("country_data_prepped_for_imp.RData")

data = df.tur;
data.dir;
Nimp = 20;
Miter = 5;
visitSequence = "monotone";
pred.vars = NULL;
use.log.poly = FALSE

## function modified from mice to construct the basic prediction matrix
quickpred2 <- function(data, mincor = 0.1, minpuc = 0, include = "", exclude = "", method = "pearson", maxcor = 0.99) {
  # ` functions copied from mice package to ensure that the above quickpred works... (https://github.com/amices/mice/blob/master/R/check.R)
  check.dataform <- function(data) {
    if (!(is.matrix(data) || is.data.frame(data))) {
      stop("Data should be a matrix or data frame", call. = FALSE)
    }
    if (ncol(data) < 2) {
      stop("Data should contain at least two columns", call. = FALSE)
    }
    data <- as.data.frame(data)
    mat <- sapply(data, is.matrix)
    df <- sapply(data, is.data.frame)
    if (any(mat)) {
      stop(
        "Cannot handle columns with class matrix: ",
        colnames(data)[mat]
      )
    }
    if (any(df)) {
      stop(
        "Cannot handle columns with class data.frame: ",
        colnames(data)[df]
      )
    }

    dup <- duplicated(colnames(data))
    if (any(dup)) {
      stop(
        "Duplicate names found: ",
        paste(colnames(data)[dup], collapse = ", ")
      )
    }
    data
  }
  data <- check.dataform(data)
  nvar <- ncol(data)
  predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(
    names(data),
    names(data)
  ))
  x <- data.matrix(data)
  r <- !is.na(x)
  suppressWarnings(v <- abs(cor(x,
                                use = "pairwise.complete.obs",
                                method = method
  )))
  v[is.na(v)] <- 0
  suppressWarnings(u <- abs(cor(
    y = x, x = r, use = "pairwise.complete.obs",
    method = method
  )))
  u[is.na(u)] <- 0
  maxc <- pmax(v, u)
  predictorMatrix[maxc > mincor] <- 1
  predictorMatrix[maxc > maxcor] <- 0
  p <- mice::md.pairs(data)
  puc <- p$mr / (p$mr + p$mm)
  predictorMatrix[puc < minpuc] <- 0
  yz <- pmatch(include, names(data))
  predictorMatrix[, yz] <- 1
  yz <- pmatch(exclude, names(data))
  predictorMatrix[, yz] <- 0
  diag(predictorMatrix) <- 0
  predictorMatrix[colSums(!r) == 0, ] <- 0
  predictorMatrix
}

# country, character string
# x, df (from above) to be filtered based on country

# data, a flat data.frame with no nesting
# Save:
# 1. separate files by country
country_vec <- sort(as.character(unique(data$COUNTRY)))


walk(country_vec, \(x){
  # country = country_vec[1]
  # df.tmp = datax
  # filter, unnest and extract data
  df.tmp <- data %>%
    filter(COUNTRY == x) %>%
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

  if(any(str_detect(colnames(df.tmp), "_Y2"))){
    doi.w2.avg = mean(df.tmp$DOI_ANNUAL_Y2, na.rm=TRUE)
    df.tmp <- df.tmp |>
      mutate(
        AGE_Y2 = case_when(
          is.na(AGE_Y2) ~ round(AGE_Y1 + as.numeric(doi.w2.avg - DOI_ANNUAL_Y1)/365),
          .default = AGE_Y2
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
        )
      )
  }

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
  tmp.meth[(names(tmp.meth) %in% var.ignore | names(tmp.meth) %in% comp.miss)] <- ""
  tmp.meth[tmp.meth0 == ""] <- ""
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
    tmp.dat,
    maxcor = 0.98, mincor = 0.10,
    include = pred.vars[keep.var],
    exclude = exclude.var
  )
  # predictor matrix:
  # rows: designate variable being imputed
  # cols: designate what variable(s) are used to predict row
  ## =================================================================== ##
  # To help (slightly) speed, set the following conditions
  # 1. Wave 3 cannot predict wave 1
  t1 <- colnames(tmp.pred)[str_detect(colnames(tmp.pred), "_Y1")]
  t3 <- colnames(tmp.pred)[str_detect(colnames(tmp.pred), "_Y3")]
  tmp.pred[t1,t3] <- 0 # all t1 (rows) cannot be predicted by t3 (cols)

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
    if(Nimp > 5){
      fit.imp <- mice::futuremice(
        tmp.dat,
        m = Nimp,
        maxit = Miter,
        visitSequence = visitSequence,
        method = tmp.meth,
        predictorMatrix = tmp.pred,
        donors = 10,
        threshold = 2.0, # see https://github.com/amices/mice/issues/314 for threshold information
        ridge = 0.01,
        n.core = future::availableCores(),
        parallelseed = 31415
      )
      future::resetWorkers(plan())
    } else {
      fit.imp <- mice::mice(
        tmp.dat,
        m = Nimp,
        maxit = Miter,
        visitSequence = visitSequence,
        method = tmp.meth,
        predictorMatrix = tmp.pred,
        donors = 10,
        threshold = 2.0,
        ridge = 0.01,
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
  ## save country-by-imputation-specific files
  c.file.name <- paste0("imputed_data_obj_",cur.country,"_nimp_",Nimp,".RData")
  save(fit.imp, file = here::here(data.dir, c.file.name))

})


