#' GFS Wrapper for Regression
#' Runs relatively simple regression analysis
#'
#' @param formula a model (linear) as used in glm
#' @param svy.design a data.frame or survey.design object
#' @param family the usual family argument as used in glms see below for more information
#' @param robust.huberM a logical defining whether to use the robsurvey pacakge to estimate the linear regression model instead of the usual svyglm(.) function (default: FALSE)
#' @param robust.tune (default 1) tuning parameter for robsurvey package
#' @param ... additional arguments as needed
#' @return a list of containing resulting fitted object (fit), residuals, a tidied version, and
#' a vector containing the included predictor variables.
#' @description
#' #' When robust.huberM is TRUE, for (approximately) continuous outcomes, you have the option of
#' alternatively using a "robust m-estimator" with Huber style robustness weights in addition to
#' the complex sampling design adjustments. It's unknown whether this makes a meaningful difference,
#' but preliminary testing suggests small differences in point estimates but sometimes dramatic
#' changes to standard errors for reasons that are unclear to me. Could be due to a strange
#' interaction of robustness weights, attrition weights, and post-stratified sampling weights.
#'
#' @examples {
#'   # TO-DO
#' }
#' @export
gfs_svyglm <- function(formula, svy.design, family = gaussian(), robust.huberM = FALSE, robust.tune = 1, ...) {
  is.survey <- ifelse(any(class(svy.design) %in% c(
    "survey.design2", "survey.design"
  )), TRUE, FALSE)
  if (is.survey) {
    tmp.data <- svy.design[["variables"]]
  } else {
    stop("svy.design must be a survey.design or survey.design2 object from the survey package. Please check data object.")
  }
  # double check predictors for variance
  y <- paste0(formula)[2]
  x <- rownames(attr(terms(formula), "factors"))[-1]
  keep.pred <- keep_variable(x, tmp.data)
  tmp.model <- reformulate(
    response = y,
    termlabels = x[keep.pred]
  )

  # fit 1: no weights
  fit.dof <- stats::glm(tmp.model, data = tmp.data, ...)
  vcom <- fit.dof$df.residual

  # fit 2: with complex survey adjustments
  tmp.fit <- survey::svyglm(tmp.model, design = svy.design, ...)
  tmp.fit.tidy <- tidy(tmp.fit) # contains, estimates & standard errors
  tmp.fit.tidy <- tmp.fit.tidy %>%
    mutate(
      f.statistic = (estimate**2) / (std.error**2),
      df.num = 1,
      df.dem = vcom,
      p.value = 1 - pf(f.statistic, df.num, df.dem),
      # see: Lumley, T. & Scott, A. Fitting Regression Models to Survey Data. Statistical Science 32, 265–278 (2017). p. 269 left column, middle paragraph
      p.value = case_when(
        p.value == 0 ~ 1e-16,
        .default = p.value
      )
    )
  if (robust.huberM) {
    tmp.fit <- robsurvey::svyreg_huberM(
      tmp.model,
      design = svy.design,
      k = 1,
      maxit = 10000
    )
    tmp.fit.tidy <- data.frame(
      term = names(tmp.fit$estimate),
      estimate = tmp.fit$estimate,
      std.error = sqrt(diag(vcov(tmp.fit, mode = "compound")))
    )
    tmp.fit.tidy <- tmp.fit.tidy %>%
      mutate(
        f.statistic = (estimate**2) / (std.error**2),
        df.num = 1,
        df.dem = vcom,
        p.value = 1 - pf(f.statistic, df.num, df.dem),
        p.value = case_when(
          p.value == 0 ~ 1e-16,
          .default = p.value
        )
      )
  }
  # export (1) the fitted object
  # and (2) the "tidied" version
  tmp.fit.tidy$vcom <- vcom
  out <- list(
    fit = tmp.fit,
    residuals = residuals(tmp.fit),
    fit.tidy = tmp.fit.tidy,
    retained.predictors = x[keep.pred],
    design = svy.design
  )
  out
}
