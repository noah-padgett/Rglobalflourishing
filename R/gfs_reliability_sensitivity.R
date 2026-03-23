#' Reliability Sensitivity Analysis
#'
#' Calculated reliability corrected estimates of regression coefficients/
#'
#' @param theta baseline parameter value
#' @param se standard error of the estimate
#' @param crit critical value of t-distribution (e.g., qt(1-ci.alpha/2, df)=qt(0.975,98))
#' @param lambda possibly a vector of average reliability coefficients for predictor/outcome combination
#' @return a data.frame
#' @description
#' The reliability sensitivity analysis is based on Spearman attenuate correction for measures of association (Spearman, 1904). The correction aims to provide a "corrected" estimate of association after accounting for the attentuation that occurs due to uncorrelated measurement error.
#' The correction is
#' $$b_{corrected} = \frac{b}{\lambda}$$
#
#'
#' @examples
#' reliability_corrected_estimates(0.10, 0.02, 1.96, c(0.40, 0.55, 0.70))
#' @export
reliability_corrected_estimates <- function(theta, se, crit, lambda) {
  K = length(lambda)
  tmp.df = data.frame(
    reliability = lambda,
    theta = rep(theta, K),
    se = rep(se, K)
  )
  tmp.df |>
    mutate(
      corrected.theta = theta / reliability,
      corrected.se = se / reliability,
      corrected.theta.lb = corrected.theta - crit * se,
      corrected.theta.ub = corrected.theta + crit * se
    )
}
