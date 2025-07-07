#' Estimate Attrition Model
#'
#' Estimate (save fitted model)
#'
#' @param data a pre-cleaned and mostly ready for analysis data.frame that will be used to construct
#'   attrition weights.
#' @param attr.pred (character vector) default to NULL, a vector of variables to be used as predictors of staying in the study at wave 2
#' @param stabilized (logical) default to TRUE, where the estimated attrition weight is the divisor and the numerator is the baseline rate of attrition (if FALSE, then the numerator is fixed to 1)
#' @param robust (logical) default to FALSE, of whether to use a "robust" variant of probit regression
#'         (uses the robustbase package instead of the survey package to get weights).
#' @param wgt.trim.quantile (numeric) default to 0.99, quantile of the distribution of weights to trim to
#' @param wgt (character) name of variable containing weights to use as "base-weights"
#' @param strata (character) name of variable containing strata ids
#' @param psu (character) name of variable containing primary sampling unit ids
#' @param ... other arguments passed to svyglm or glmrob functions
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' The default set of predictors: "ANNUAL_WEIGHT_R2", "MODE_ANNUAL_W1", "AGE_W1", "GENDER_W1",
#' "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1".
#'
fit_attrition_model <- function(data, obs.id.var = NULL, attr.pred = NULL, robust = FALSE,  wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU", ...) {
  if (is.null(obs.id.var)) {
  	obs.id.var = "CASE_OBSERVED_Y2"
  }
  if (is.null(attr.pred)) {
    attr.pred <- c(
    "ANNUAL_WEIGHT_R2", "MODE_RECRUIT",
    'COMPOSITE_HAPPI_LIFE_SAT_Y1', 'COMPOSITE_HEALTH_Y1', 'COMPOSITE_MEANING_PURPOSE_Y1',
    'COMPOSITE_CHARACTER_Y1', 'COMPOSITE_SUBJECTIVE_SOC_CONN_Y1', 'COMPOSITE_FINL_MAT_WORRY_Y1',
    'COMPOSITE_EXTRAVERSION_Y1', 'COMPOSITE_OPENNESS_Y1', 'COMPOSITE_AGREEABLENESS_Y1',
    'COMPOSITE_CONSCIENTIOUSNESS_Y1', 'COMPOSITE_NEUROTICISM_Y1',
    'COMPOSITE_DEPRESSION_Y1', 'COMPOSITE_ANXIETY_Y1', 'LONELY_Y1', 'DAYS_EXERCISE_Y1',
    'COV_AGE_GRP_Y1', 'COV_GENDER', 'COV_MARITAL_STATUS_Y1', 'COV_EMPLOYMENT_Y1',
    'COV_ATTEND_SVCS_Y1', 'COV_EDUCATION_3_Y1', 'COV_BORN_COUNTRY_Y1', "COV_RACE_PLURALITY",
    "COV_URBAN_RURAL_Y1", 'COV_INCOME_Y1'
    )
  }

  tmp.data <- data %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), \(x){
        x <- factor(x)
        x <- forcats::fct_drop(x)
        x <- forcats::fct_infreq(x)
        x
      })
      ,dplyr::across(any_of(attr.pred[-1]) & where(is.numeric), \(x){
      	#as.numeric(scale(x))
        svy_scale(x, wgt = .data[[wgt]], psu = .data[[psu]], strata = .data[[strata]])
      })
    )
  svy.data <- tmp.data %>%
    as_survey_design(
      ids = {{psu}},
      strata = {{strata}},
      weights = {{wgt}},
      calibrate.formula = ~1
    )

  cur.country <- data$COUNTRY2[1]
  cur.country <- stringr::str_remove_all(str_replace(cur.country, "\n", "_"), "_")
  cur.imp <- data$.imp2[1]

  keep.var <- keep_variable(attr.pred, svy.data[["variables"]], reason = "any")

  mod.form <- stats::reformulate(attr.pred[keep.var], response = obs.id.var)

  if (robust) {
    svy.data <- tmp.data
    fit.attr <- robustbase::glmrob(mod.form, data = svy.data,
                                   family = stats::quasibinomial(link = "logit"),
                                   weights = tmp.data[wgt])
  } else {
    fit.attr <- survey::svyglm(mod.form, design = svy.data,
                               family = stats::quasibinomial(link = "logit"),
                               #family = stats::quasipoisson(link = "log"),
                               control = list(maxit = 1000))
  }
  fit.attr
}


#' Estimate and Save Attrition Weights
#'
#' Estimate attrition weights to data.frame
#'
#' @param fit (model object) that was fit to estimate attrition weights
#' @param stabilized (logical) default to TRUE, where the estimated attrition weight is the divisor and the numerator is the baseline rate of attrition (if FALSE, then the numerator is fixed to 1)
#' @param wgt.trim.quantile (numeric) default to 0.99, quantile of the distribution of weights to trim to
#' @param wgt (character) name of variable containing weights to use as "base-weights"
#' @param strata (character) name of variable containing strata ids
#' @param psu (character) name of variable containing primary sampling unit ids
#' @param ... other arguments passed to svyglm or glmrob functions
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' The default set of predictors: "ANNUAL_WEIGHT_R2", "MODE_ANNUAL_W1", "AGE_W1", "GENDER_W1",
#' "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1".
#'
create_attr_wgts <- function(fit, obs.id.var = NULL, stabilized = TRUE, wgt.trim.quantile = 0.99,  wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU", composite.wgt.name = NULL, attr.wgt.name = NULL, ...) {

  if (is.null(obs.id.var)) {
    obs.id.var = "CASE_OBSERVED_Y2"
  }
  if (is.null(composite.wgt.name)){
    composite.wgt.name = as.name("SAMP.ATTR.WGT")
  }
  if (is.null(attr.wgt.name)){
    attr.wgt.name = as.name("ATTR.WGT")
  }
  # extract data
  data <- fit$data
  svy.data <- fit$survey.design

  wgts <- stats::predict(fit, newdata=svy.data, type = "response")
  wgts <- as.numeric(wgts)
  wgts <- case_when(
    wgts < 0.001 ~ 0.001,
    wgts > 0.999 ~ 0.999,
    .default = wgts
  )
  if (length(wgts) != nrow(data)) {
    wgts0 <- rep(NA, nrow(data))
    wgts0[which((paste0(1:nrow(data)) %in% names(wgts)))] <- wgts
    wgts0[is.na(wgts0)] <- stats::median(wgts0, na.rm = TRUE)
    wgts <- wgts0
  }

  attr.wgts <- numeric(length(wgts))
  if (stabilized) {
    baseline.wgt.1 <- mean(data[[obs.id.var]])
    baseline.wgt <- case_when(
      data[[obs.id.var]] == 1 ~ baseline.wgt.1,
      data[[obs.id.var]] == 0 ~ 1 - baseline.wgt.1
    )
    wgts <- case_when(
      data[[obs.id.var]] == 1 ~ wgts,
      data[[obs.id.var]] == 0 ~ 1 - wgts
    )
    attr.wgts <- baseline.wgt / wgts
  } else {
    attr.wgts <- 1 / wgts
  }

  # Weight trimming: avoid highly influential cases
  wgt.max <- stats::quantile(attr.wgts, wgt.trim.quantile)
  attr.wgts[ attr.wgts > wgt.max ] <- wgt.max

  # add weights to "clean" dataset
  wgt0 <- as.name(wgt)
  data <- data  %>%
    mutate(
      "{{attr.wgt.name}}" := as.numeric(attr.wgts),
      "{{composite.wgt.name}}" := {{attr.wgt.name}} * {{wgt0}},
      "{{composite.wgt.name}}" := n() * ( {{composite.wgt.name}} / sum( {{composite.wgt.name}} ))
    ) %>%
    select(ID, PSU, STRATA, {{wgt0}}, {{attr.wgt.name}}, {{composite.wgt.name}})

  data
}

#' Wrapper Function Attrition Weights
#'
#' Wrapper function to use the data.set specific attrition weights function.
#'
#' @param data.dir character defining where the recoded imputed data files are stored
#' @param ... additional arguments passed to ....
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @description
#' TO-DO
#'
#' @export
run_attrition_model_by_country <- function(data.dir, wgt = "ANNUAL_WEIGHT_R2", attr.wgt.name = NULL, pooled.wgt = NULL, composite.wgt.name = NULL,...) {
  if(is.null(pooled.wgt)){
    pooled.wgt = as.name("AVG.SAMP.ATTR.WGT")
  }
  if (is.null(composite.wgt.name)){
    composite.wgt.name = as.name("SAMP.ATTR.WGT")
  }

  # get list of files in data.dir
  df.files <- list.files(data.dir)
  df.files <- df.files[str_detect(df.files, "recoded_imputed_data_obj")]
  ## get country names
  country.vec <- str_remove(df.files, "recoded_imputed_data_obj_") |>
    stringr::word(1, sep = "\\_imp") |>
    unique() |>
    sort()

  if (!dir.exists(here("results-attr"))) dir.create(here::here("results-attr"))

  x <- country.vec[1]
  walk(country.vec, \(x){
    tmp.files <- df.files[str_detect(df.files,x)]
    y <- tmp.files[1]
    df.attr <- map(tmp.files, \(y){

      df.tmp <- readr::read_rds(here::here(data.dir, y))
      df.tmp %>%
        dplyr::group_by(COUNTRY, .imp) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          fit.attr = purrr::map(data, \(tmp.dat){
            fit_attrition_model(tmp.dat,...)
          }),
          data = map(fit.attr, \(x){
            create_attr_wgts(x,...)
          })
        )
    	}) |>
    	bind_rows()

    df.wgts <- df.attr %>%
      select(COUNTRY, .imp, data) %>%
      tidyr::unnest(c(data)) %>%
      group_by(COUNTRY, ID) %>%
      mutate(
        "{{pooled.wgt}}" := mean({{composite.wgt.name}}, na.rm=TRUE)
      ) %>%
      ungroup() %>%
      select(ID, COUNTRY, .imp, PSU, STRATA, contains("WGT"))

    myfile = paste0(x, " fitted attrition model.RData")
    save(df.attr, df.wgts , file = here::here("results-attr", myfile))
  })
}



#' Add Attrition Weights to Raw Data
#'
#' Adds estimated attrition weights to raw data to help summarize data.
#'
#' @param data a pre-cleaned and mostly ready for analysis data.frame that will be used to construct
#'   attrition weights.
#' @param attr.wgt.file (default NULL), a file name to get the stored attrition weights
#' @param attr.wgt.name (default NULL) name of attrition weight in saved data
#' @param composite.wgt.name (default NULL) name of composite weight in saved data
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @description
#' TO-DO
#'
#' @export
append_attrition_weights_to_df <- function(data, country = NULL, composite.wgt.name = NULL, attr.wgt.name = NULL, wgt = NULL, strata = NULL, psu = NULL){
  if (is.null(composite.wgt.name)) {
    composite.wgt.name = as.name("AVG.SAMP.ATTR.WGT")
  }
  if (is.null(attr.wgt.name)) {
    attr.wgt.name = as.name("ATTR.WGT")
  }
  if (is.null(wgt)) {
    wgt = as.name("ANNUAL_WEIGHT_R2")
  }
  if (is.null(strata)) {
    strata = as.name("STRATA")
  }
  if (is.null(psu)) {
    psu = as.name("PSU")
  }

  # get country specific files
  attr.wgt.file <- list.files("results-attr")
  names(attr.wgt.file) <- str_split_i(attr.wgt.file, " fitted", 1)
  # subset to only this specific country
  #attr.wgt.file <- attr.wgt.file[str_detect(attr.wgt.file, country)]
  x <- attr.wgt.file[2]
  saved_attr_wgts <- map(attr.wgt.file, \(x){
    load(here::here("results-attr",x), ex <- new.env())
    # ls.str(ex)
    ex$df.wgts %>%
      filter(.imp == 1) %>%
      select(ID, COUNTRY, {{psu}}, {{strata}}, {{attr.wgt.name}}, {{composite.wgt.name}})
  }) |> bind_rows()
  data <- left_join(data, saved_attr_wgts)

  data
}


#' Add Attrition Weights to Imputed Data
#'
#' Adds estimated attrition weights to imputed data for analysis.
#'
#' @param data.dir a character defining where recoded imputed data is
#' @param attr.dir a character defining where the attrition weight results are
#' @returns null but resave recoded imputed data into data.dir
#' @examples {
#'   # TO-DO
#' }
#' @description
#' TO-DO
#'
#' @export
append_attr_wgts_to_imp_data <- function(data.dir, attr.dir){
	# get list of files in data.dir
	df.files <- list.files(data.dir)
	df.files <- df.files[str_detect(df.files, "recoded_imputed_data_obj")]
	attr.dir <- here(attr.dir)
	plan("multisession", workers = availableCores(constraints = "connections"))
	with_progress({
	  p <- progressor(along = df.files)
	  furrr::future_walk(df.files, \(x){
	  load_packages()
		df.tmp <- readr::read_rds(here::here(data.dir, x))
		cur.country <- str_remove(x, "recoded_imputed_data_obj_") |>
			stringr::word(1, sep = "\\_imp")
		attr.file <- paste0(cur.country, " fitted attrition model.RData")
		load(here::here(attr.dir, attr.file), env.attr <- new.env())
		df.tmp <- left_join(df.tmp, env.attr$df.wgts)
		readr::write_rds(df.tmp, file = here::here(data.dir, x), compress = "gz")
		rm(df.tmp)
		gc()
		p(sprintf("x= %s", x))
	  },.options = furrr_options(seed = TRUE))
	})
	future::resetWorkers(plan())
}
