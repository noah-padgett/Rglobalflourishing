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
#' @param ... other arguments passed to svyglm or glmrob functions
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' The default set of predictors: "ANNUAL_WEIGHT1_W1", "MODE_ANNUAL_W1", "AGE_W1", "GENDER_W1",
#' "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1".
#'
append_attrition_wgts <- function(data, attr.pred = NULL, stabilized = TRUE, robust = FALSE, wgt.trim.quantile = 0.99, ...) {
  if (is.null(attr.pred)) {
    attr.pred <- c(
      "ANNUAL_WEIGHT1_W1", "MODE_ANNUAL_W1",
      "AGE_GRP_W1", "GENDER_W1", "EDUCATION_3_W1", "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1"
    )
  }

  tmp.data <- data %>%
    dplyr::mutate(
      dplyr::across(!(where(is.numeric)), \(x){
        x <- factor(x)
        x <- forcats::fct_infreq(x)
        x
      })
    )
  svy.data <- survey::svydesign(data = tmp.data, ids = ~PSU_W1, strata = ~STRATA_W1, weights = ~ANNUAL_WEIGHT1_W1)
  cur.country <- data$COUNTRY2[1]

  cur.country <- stringr::str_remove_all(str_replace(cur.country, "\n", "_"), "_")

  keep.var <- keep_variable(attr.pred, svy.data[["variables"]])

  mod.form <- stats::reformulate(attr.pred[keep.var], response = "CASE_MISSING_W2")


  if (robust) {
    svy.data <- tmp.data
    fit.attr <- robustbase::glmrob(mod.form, data = svy.data, family = stats::binomial(link = "probit"), weights = tmp.data$ANNUAL_WEIGHT1_W1)
  } else {
    fit.attr <- survey::svyglm(mod.form, design = svy.data, family = stats::quasibinomial(link = "probit"), control = list(maxit = 1000))
  }

  # summary(fit.attr)

  wgts <- stats::predict(fit.attr, type = "response")
  if (length(wgts) != nrow(data)) {
    wgts0 <- rep(NA, nrow(data))
    wgts0[which((paste0(1:nrow(data)) %in% names(wgts)))] <- wgts
    wgts0[is.na(wgts0)] <- stats::median(wgts0, na.rm = TRUE)
    wgts <- wgts0
  }

  attr.wgts <- 1
  if (stabilized) {
    baseline.wgt <- mean(data$CASE_MISSING_W2)
    attr.wgts <- baseline.wgt / wgts
  } else {
    attr.wgts <- 1 / wgts
  }

  # Weight trimming: avoid highly influential cases
  wgt.max <- stats::quantile(attr.wgts, wgt.trim.quantile)
  attr.wgts[ attr.wgts > wgt.max ] <- wgt.max

  # add weights to dataset
  data$ATTR_WGT <- as.numeric(attr.wgts)

  # save fitted regression model for use later
  # check if "results-attr" folder exists
  if (!dir.exists("results-attr")) dir.create(here::here(getwd(), "results-attr"))

  save(fit.attr, file = here::here("results-attr", paste0(cur.country, " fitted attrition model.RData")))

  data
}

#' Wrapper Function Attrition Weights
#'
#' Wrapper function to use the data.set specific attrition weights function.
#'
#' @param data a pre-cleaned and mostly ready for analysis data.frame that will be used to construct
#'   attrition weights.
#' @param ... additional arguments passed to append_attrition_wgts(.)
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
run_attrition_model <- function(data, ...) {
  df.attr <- data %>%
    dplyr::mutate(
      COUNTRY2 = COUNTRY
    ) %>%
    dplyr::group_by(COUNTRY) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, \(tmp.dat){
        tmp.dat %>%
          dplyr::mutate(
            dplyr::across(where(is.factor), droplevels)
          )
      }),
      data = purrr::map(data, \(tmp.dat){
        append_attrition_wgts(tmp.dat, ...)
      }),
      data = purrr::map(data, \(tmp.dat){
        tmp.dat %>%
          dplyr::mutate(
            WGT = ATTR_WGT * ANNUAL_WEIGHT1_W1,
            # WGT must sum to sample size
            WGT = n() * (WGT / sum(WGT))
          )
      })
    ) %>%
    tidyr::unnest(c(data))
  df.attr
}

