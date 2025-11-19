#' GFS Wrapper for Regression
#' Runs relatively simple regression analysis
#'
#' @param formula a model (linear) as used in glm
#' @param svy.design a data.frame or survey.design object
#' @param robust.huberM a logical defining whether to use the robsurvey pacakge to estimate the linear regression model instead of the usual svyglm(.) function (default: FALSE)
#' @param robust.tune (default 1) tuning parameter for robsurvey package
#' @param model.type (default "reg")
#' @param var.check (defaults TRUE) whether to double check that each predictor in formula has variance
#' @param ... additional arguments as needed--an important one is family passed to glm
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
#' Setting 'var.check = FALSE' will through an error if one of the supplied predictors has zero variance. The exception is for setting the intercept '1' or '0' which can be useful to get group means.
#' @examples {
#'   # TO-DO
#' }
#' @export
gfs_svyglm <- function(formula, svy.design, robust.huberM = FALSE, robust.tune = 1, model.type = "reg", var.check = TRUE, ...) {

  is.survey <- ifelse(any(class(svy.design) %in% c(
    "survey.design2", "survey.design"
  )), TRUE, FALSE)
  if (is.survey) {
    tmp.data <- svy.design[["variables"]]
  } else {
    stop("svy.design must be a survey.design or survey.design2 object from the survey package. Please check data object.")
  }
  # double check predictors for variance
  tmp.model = formula
  #print(tmp.model)

    y <- paste0(formula)[2]
    x <- rownames(attr(terms(formula), "factors"))[-1]
    keep.pred <- keep_variable(x, tmp.data)
    retained.predictors <- x[keep.pred]
  if(var.check){
    tmp.model <- reformulate(
      response = y,
      termlabels = retained.predictors
    )
  }

  ## initial output object incase try fails for this replications
  out <- list(
    fit = NULL,
    residuals = NULL,
    fit.tidy = NULL,
    retained.predictors = retained.predictors,
    design = NULL,
    fit.lasso = ifelse(model.type == "lasso", tmp.fit.lasso, NA),
    converge = FALSE
  )
  try({
    # fit 1: no weights
  fit.dof <- stats::glm(tmp.model, data = tmp.data, ...)
  #print(fit.dof)
  vcom <- fit.dof$df.residual

  ## check design matrix
  if(var.check){
    dm <- model.matrix(fit.dof)
    dm.cov.det <- det(cov(dm[-1,-1]))
    if(dm.cov.det <= 0){
      warning("WARNING. Analysis may fail. Design matrix is non-positive definite.")
      print("Check country=", unique(tmp.data[["COUNTRY2"]]), "; imp=",unique(tmp.data[[".imp"]])," outcome=", your.outcome, "; focal exposure=",your.pred)
    }
  }

  # fit 2: with complex survey adjustments
  tmp.fit <- survey::svyglm(tmp.model, design = svy.design, ...)
  #print(tmp.fit)
  tmp.fit.tidy <- tidy(tmp.fit) # contains, estimates & standard errors

  if (robust.huberM) {
    tmp.fit <- robsurvey::svyreg_huberM(
      tmp.model,
      design = svy.design,
      k = robust.tune,
      maxit = 10000
    )
    tmp.fit.tidy <- data.frame(
      term = names(tmp.fit$estimate),
      estimate = tmp.fit$estimate,
      std.error = sqrt(diag(vcov(tmp.fit, mode = "compound")))
    )
  }
  if (model.type == "lasso"){
    if(class(family) == "family"){
      family = family$family
    }
    ## Step 1. fit weighted lasso model to get reduced set of predictors
    ## exclude focal predictor so that it doesn't get excluded from final model
    tmp.dat00 <- tmp.data[,c(x[keep.pred][-1], y, names(svy.design$cluster), names(svy.design$strata),names(svy.design$allprob))]
    tmp.fit.lasso <- svyVarSel::wlasso(
      data = as.data.frame(tmp.dat00),
      cluster = names(svy.design$cluster),
      strata = names(svy.design$strata),
      weights = names(svy.design$allprob),
      col.y = y, col.x = x[keep.pred][-1], ...
    )
    retained.predictors = rownames(tmp.fit.lasso$model$min$beta)[tmp.fit.lasso$model$min$beta[,1] != 0]
    retained.predictors = unique(c(x[keep.pred][1], x[keep.pred][ x[keep.pred] %in% retained.predictors]))
    ## fit with the selected predictors
    tmp.model <- reformulate(
      response = y,
      termlabels = retained.predictors
    )
    ## Step 2. Fit actual model to get estimates + standard errors
    # fit 1: no weights
    fit.dof <- stats::glm(tmp.model, data = tmp.data, ...)
    vcom <- fit.dof$df.residual
    # fit 2: with complex survey adjustments
    tmp.fit <- survey::svyglm(tmp.model, design = svy.design, ...)
    tmp.fit.tidy <- tidy(tmp.fit) # contains, estimates & standard errors
  }
  ## compute f-stat and p-value
  tmp.fit.tidy <- tmp.fit.tidy %>%
    mutate(
      f.statistic = (estimate**2) / (std.error**2),
      df.num = 1,
      df.dem = vcom,
      p.value = 1 - pf(f.statistic, df.num, df.dem),
      # see: Lumley, T. & Scott, A. Fitting Regression Models to Survey Data. Statistical Science 32, 265–278 (2017). p. 269 left column, middle paragraph
      p.value = case_when(
        p.value == 0 ~ 2.2e-16,
        .default = p.value
      )
    )
  # export (1) the fitted object
  # and (2) the "tidied" version
  tmp.fit.tidy$vcom <- vcom
  out <- list(
    fit = tmp.fit,
    residuals = residuals(tmp.fit),
    fit.tidy = tmp.fit.tidy,
    retained.predictors = retained.predictors,
    design = svy.design,
    fit.lasso = ifelse(model.type == "lasso", tmp.fit.lasso, NA),
    converge = TRUE
  )
})
  out
}
