#' Compute E-values for Estimates and Confidence Intervales
#'
#' Conduct simple sensitivity analyses for OLS or RR estimates. This is a wrapper for
#' the evalues package.
#'
#' @param est effect estimate
#' @param se standard error of estimate
#' @param sd standard deviation of outcome
#' @param ci.low lower confidence limit
#' @param ci.up upper confidence limit
#' @param type how to compute evalue (linear or RR)
#' @param what define which values to extract (default is both using `c("EE", "ECI")`)
#' @param rare an option used in the evalues package
#' @return a vector containing
#' * EE, E-value for estimate
#' * ECI, E-value for confidence interval
#' @examples
#'
#' load_packages()
#' gfs_compute_evalue(est = 0.20, se=0.11, sd=1, type='linear')
#' gfs_compute_evalue(est = 0.20, ci.low=-0.02, ci.up=0.42, type='RR')
#'
#' @export
#' @description
#' TO-DO
gfs_compute_evalue <- function(est, se = NULL, sd = NULL, ci.low = NULL, ci.up = NULL,
                               type = "inear", rare = 0, what = c("EE", "ECI")) {
  suppressMessages({
    fit <- NULL
    if (type == "linear") {
      fit <- evalues.OLS(est, se = se, sd = sd)
    } else if (type == "RR") {
      fit <- evalues.RR(exp(est), lo = exp(ci.low), hi = exp(ci.up), rare = rare)
    }
  })
  out <- c(fit[2], ifelse(is.na(fit[4]), fit[6], fit[4]))
  names(out) <- c("EE", "ECI")
  out[what]
}
