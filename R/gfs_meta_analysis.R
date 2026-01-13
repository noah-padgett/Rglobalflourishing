#' GFS Wrapper for Meta-Analysis
#'
#' Conducted meta-analysis (random effects or population weighted) for a set of effect of interest.
#'
#' @param meta.input a nested data.frame where `data` is a column
#' @param yi a name of the variable containing the effect sizes to be meta-analyzed (e.g., as.name("Est"))
#' @param sei a name of the variable containing the standard error of the effect size (e.g., as.name("SE"))
#' @param estimator (default = "PM") other options include those available in the metafor package
#' @param interval.method (default = "empirical) how the proportion of effect size metric is computed
#' @param better.name (optional) string of the name to use in the forest plot
#' @param p.subtitle (optional) subtitle of forest plot
#' @param ci.alpha (0.05) type-1 error rate (alpha) for confidence intervals
#' @param ... additional arguments
#' @return a list of meta-analysis results from the metafor package
#' @examples
#' # TO-DO
#' @export
gfs_meta_analysis <- function(meta.input, yi = as.name("Est"), sei = as.name("SE"),
                              estimator = "PM", interval.method = "empirical",

                              better.name = NULL, p.subtitle = NULL, ci.alpha = 0.05, ...) {
  # meta.input = df.tmp %>%
  # 	group_by(OUTCOME) %>%
  # nest()
  # estimator = "PM"
  # interval.method = "normal"

  # a data.format check
  cnames <- colnames(meta.input)
  if (!("data" %in% cnames) | !is.list(meta.input$data)) {
    stop("meta.input format incorrect. Please ensure that there is a column called 'data' that is a list of tibbles.")
  }
  # TO-DO: add cluster robust SE to meta-analytic point estimates: https://doi.org/10.1007/s11121-021-01246-3
  # Clustering: default should be continent/world region
  # make flexible to allow for any groupings of countries.
    # x = meta.input$data[[1]]
	#	fit <- rma(
    #      yi = Est,
    #      sei = SE,
    #      data = x,
    #      method = estimator
    #    )
	#	library(clubSandwich)
	#	clubSandwich::coef_test(fit, vcov = "CR2", cluster=c(rep(1,11), rep(2,11)))

  meta.res <- meta.input %>%
    mutate(
      FOCAL_PREDICTOR = map(data, \(x){
        x$FOCAL_PREDICTOR[1]
      }),
      data = map(data, \(x){
        x %>%
          mutate(
            yi = {{yi}},
            sei = {{sei}},
            ci.lb.i = yi - sei*qt(ci.alpha/2, df, lower.tail = FALSE),
            ci.ub.i = yi + sei*qt(ci.alpha/2, df, lower.tail = FALSE)
          )
      }),
      ## ====== random effects meta results ===================================================== ##
      meta.rma = map(data, \(x){
        rma(
          yi = yi,
          sei = sei,
          data = x,
          method = estimator
        )
      }),
      meta.rma.tidy = map(meta.rma, \(x) tidy(x, conf.int = TRUE)),
      theta.rma = map_dbl(meta.rma.tidy, "estimate"),
      theta.rma.se = map_dbl(meta.rma.tidy, "std.error"),
      theta.lb = map_dbl(meta.rma, \(x){
        tmp <- tidy(x, conf.int = TRUE)
        tmp[1, "estimate", drop = TRUE] - qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
      }),
      theta.ub = map_dbl(meta.rma, \(x){
        tmp <- tidy(x, conf.int = TRUE)
        tmp[1, "estimate", drop = TRUE] + qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
      }),
      theta.rma.ci = map_chr(meta.rma, \(x) get_meta_ci(x, "rma", ci.alpha, ...)),
      tau2 = map_dbl(meta.rma, "tau2"),
      tau = sqrt(tau2),
      I2 = map_dbl(meta.rma, "I2"),
      Q.stat = map_dbl(meta.rma, "QE"),
      Q.pval = map_dbl(meta.rma, "QEp"),
      Qprof.ci = map_chr(meta.rma, \(x){
        get_meta_ci(x, type = "Q", ...)
      }),
      calibrated.yi = map(meta.rma, \(x){
        compute_calibrated(x)
      }),
      prob.leqneq0.1 = map_dbl(calibrated.yi, \(x){
          proportion_meaningful(x, q = -0.10, above = FALSE, interval.method, theta.rma, tau)
        }),
      prob.geq0.1 = map_dbl(calibrated.yi, \(x){
          proportion_meaningful(x, q = 0.10, above = TRUE, interval.method, theta.rma, tau)
        }),
      prob.lg.c = paste0("[", .round(prob.leqneq0.1), " / ",.round(prob.geq0.1) ,"]"),
      theta.rma.EE = gfs_compute_evalue(est=theta.rma, se=theta.rma.se, sd=1, what="EE"),
      theta.rma.ECI = gfs_compute_evalue(est=theta.rma, se=theta.rma.se, sd=1, what="ECI"),
      theta.pred.int = gfs_prediction_interval(x=calibrated.yi, theta = theta.rma, tau = tau , ...),
      ## ====== RESULTS ON RISK-RATIO SCALE ===================================================== ##
      # compute exponentiated (used only when scale is binary/Likert)
      rr.calibrated.yi = map(calibrated.yi, \(x) exp(x)),
      rr.theta = exp(theta.rma),
      rr.theta.ci = map_chr(meta.rma, \(x) get_meta_ci(x, "rma.rr", ci.alpha, .exp=TRUE, ...)),
      rr.tau = sqrt((exp(tau2) - 1) * exp(2 * theta.rma + tau2)),
      rr.prob.0.90 = map_dbl(calibrated.yi.exp, \(x){
          proportion_meaningful(x, q = 0.90, above = FALSE, interval.method, rr.theta, rr.tau)
        }),
      rr.prob.1.10 = map_dbl(calibrated.yi.exp, \(x){
          proportion_meaningful(x, q = 1.10, above = TRUE, interval.method, rr.theta, rr.tau)
      }),
      rr.prob.c = paste0("[", .round(prob.rr0.90), " / ",.round(prob.rr1.10) ,"]"),
      rr.theta.EE = gfs_compute_evalue(est=theta.rma, ci.low=theta.lb, ci.up = theta.ub, what="EE", type="RR"),
      rr.theta.ECI = gfs_compute_evalue(est=theta.rma, ci.low=theta.lb, ci.up = theta.ub, what="ECI", type="RR"),
      rr.theta.pred.int = gfs_prediction_interval(x=rr.calibrated.yi, theta = theta.rma, tau = tau , .exp=TRUE, ...),
      ## ====== population weighted meta results ================================================ ##
      pop.wgt.meta = map(data, \(x){
        x <- add_pop_wgts(x)
        rma(
          yi = yi,
          sei = sei,
          weights = wi,
          data = x,
          method = "FE"
        )
      }),
      pop.wgt.theta = map_dbl(pop.wgt.meta, "beta"),
      pop.wgt.thetaci = map_chr(pop.wgt.meta, \(x) get_meta_ci(x, type = "FE", ci.alpha, ...)),
      pop.wgt.meta.tidy = map(pop.wgt.meta., \(x) tidy(x, conf.int = TRUE)),
      ## ====== pooled-pvalue =================================================================== ##
      global.pvalue = map_dbl(data, \(x){
        cnames <- colnames(x)
        out <- NULL
        if ("pvalue" %in% cnames) {
          out <- compute_global_pvalue(x$pvalue)
        }
        out
      }),
      ## ====== Forest plots ==================================================================== ##
      forest.plot = map(meta.rma, \(x){
        gfs_meta_forest_plot(x, better.name = better.name, p.subtitle = p.subtitle, ...)
      })
    )
  meta.res
}
