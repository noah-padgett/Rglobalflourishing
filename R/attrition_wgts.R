#' Estimate and Save Attrition Weights
#'
#' Estimate (save fitted model) and append attrition weights to data.frame
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
append_attrition_wgts <- function(data, obs.id.var = NULL, attr.pred = NULL, stabilized = TRUE, robust = FALSE, wgt.trim.quantile = 0.99,  wgt = "ANNUAL_WEIGHT1", strata = "STRATA", psu = "PSU", composite.wgt.name = NULL, attr.wgt.name = NULL, replace = FALSE, ...) {
  if (is.null(obs.id.var)) {
  	obs.id.var = "CASE_OBSERVED_W2"
  }
  if (is.null(attr.pred)) {
    attr.pred <- c(
      "ANNUAL_WEIGHT1", "MODE_RECRUIT_Y1",
      "AGE_GRP_Y1", "GENDER_Y1", "EDUCATION_3_Y1", "INCOME_Y1",
      "EMPLOYMENT_Y1", "MARITAL_STATUS_Y1", "RACE_PLURALITY_Y1",
      "URBAN_RURAL_Y1"
    )
  }
  if (is.null(composite.wgt.name)){
  	composite.wgt.name = as.name("SAMP.ATTR.WGT")
  }
  if (is.null(attr.wgt.name)){
  	attr.wgt.name = as.name("ATTR.WGT")
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
  # survey::svydesign(
  # 	data = tmp.data,
  #   ids= tmp.data[[psu]],
  #   strata= tmp.data[[strata]],
  #   weights= tmp.data[[wgt]],
  #   calibrate.formula = ~1
  # )
  cur.country <- data$COUNTRY2[1]

  cur.country <- stringr::str_remove_all(str_replace(cur.country, "\n", "_"), "_")

  keep.var <- keep_variable(attr.pred, svy.data[["variables"]], reason = "any")

  mod.form <- stats::reformulate(attr.pred[keep.var], response = obs.id.var)


  if (robust) {
    svy.data <- tmp.data
    fit.attr <- robustbase::glmrob(mod.form, data = svy.data,
                                   family = stats::binomial(link = "probit"),
                                   weights = tmp.data[wgt])
  } else {
    fit.attr <- survey::svyglm(mod.form, design = svy.data,
                               family = stats::quasipoisson(link = "log"),
                               control = list(maxit = 1000))
  }

  # summary(fit.attr)

  wgts <- stats::predict(fit.attr, newdata=svy.data, type = "response")
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
  	)

  # save fitted regression model for use later
  # check if "results-attr" folder exists
  if (!dir.exists("results-attr")) dir.create(here::here(getwd(), "results-attr"))

  myfile = here::here("results-attr", paste0(cur.country, " fitted attrition model.RData"))
  if (replace || !file.exists(myfile)){
  	# save only results from 1 imputed dataset
  	save(data, fit.attr, file = myfile)
  }

  data
}

#' Wrapper Function Attrition Weights
#'
#' Wrapper function to use the data.set specific attrition weights function.
#'
#' @param data a pre-cleaned and mostly ready for analysis data.frame that will be used to construct
#'   attrition weights.
#' @param wgt a character identifying the baseline weight used as the baseline for adjustments.
#' @param ... additional arguments passed to append_attrition_wgts(.)
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @description
#' TO-DO
#'
#' @export
run_attrition_model <- function(data, ...) {
  df.attr <- data %>%
    dplyr::group_by(COUNTRY, .imp) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, \(tmp.dat){
        append_attrition_wgts(tmp.dat, ...)
      })
    ) %>%
    tidyr::unnest(c(data))
  df.attr
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
append_attrition_weights_to_df <- function(data, attr.wgt.file = NULL, composite.wgt.name = NULL, attr.wgt.name = NULL, wgt = NULL, strata = NULL, psu = NULL){
  if (is.null(composite.wgt.name)) {
    composite.wgt.name = as.name("SAMP.ATTR.WGT")
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
  if (is.null(attr.wgt.file)) {
    attr.wgt.file <- list.files("results-attr")
    names(attr.wgt.file) <- str_split_i(attr.wgt.file, " fitted", 1)
  }

  saved_attr_wgts <- map(attr.wgt.file, \(x){
    load(here::here("results-attr",x), ex <- new.env())
    # ls.str(ex)
    ex$data %>%
      select(COUNTRY2, ID, {{psu}}, {{strata}}, {{wgt}}, {{attr.wgt.name}}, {{composite.wgt.name}})
  }) |> bind_rows(.id = "COUNTRY")
  data <- left_join(data, saved_attr_wgts)

  data
}
