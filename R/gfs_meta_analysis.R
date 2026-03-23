#' GFS Wrapper for Meta-Analysis
#'
#' Conducted meta-analysis (random effects or population weighted) for a set of effect of interest.
#'
#' @param meta.input a nested data.frame where `data` is a column
#' @param yi a name of the variable containing the effect sizes to be meta-analyzed (e.g., as.name("Est"))
#' @param sei a name of the variable containing the standard error of the effect size (e.g., as.name("SE"))
#' @param grpi the name of the variable used to identify each "study" or group, (defaults to 'as.name("Country")')
#' @param estimator (default = "PM") other options include those available in the metafor package
#' @param interval.method (default = "empirical) how the proportion of effect size metric is computed
#' @param ci.alpha (0.05) type-1 error rate (alpha) for confidence intervals
#' @param ... additional arguments
#' @return a list of meta-analysis results from the metafor package
#' @examples
#' # TO-DO
#' @export
gfs_meta_analysis <- function(meta.input, yi = as.name("Est"), sei = as.name("SE"),
                              grpi = Country, dfi = df,
                              estimator = "PM", interval.method = "empirical",
                              reliability.vec = c(0.25, 0.40, 0.55, 0.70, 0.85),
                              var.name = NULL, ci.alpha = 0.05, better.name = NULL, p.subtitle = NULL) {
  # meta.input = df.tmp %>%
  # 	group_by(OUTCOME) %>%
  # nest()
  # yi = as.name('std.est'); sei = as.name('std.se'); grpi = as.name("Country"); dfi = as.name("df"); estimator = "PM"; interval.method = "normal"; better.name = NULL; p.subtitle = NULL; ci.alpha = 0.05; reliability.vec = c(0.25, 0.40, 0.55, 0.70, 0.85)
  # include.influence = TRUE, better.name = NULL, p.title=NULL, p.subtitle = NULL,
  # @param better.name (optional) string of the name to use in the forest plot
  # @param var.name (optional) string of the name of the variable being focal predictor
  # @param p.subtitle (optional) subtitle of forest plot

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
        if(is.null(var.name)){
          return(x$FOCAL_PREDICTOR[1])
        }
        var.name
      }),
      data = map(data, \(x){
        if(!is.null(dfi)){
          x <- x |> mutate(df = {{dfi}})
        } else {
          x$df <- 9999
        }
        x %>%
          mutate(
            yi = {{yi}},
            sei = {{sei}},
            group = {{grpi}},
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
      theta.lb = map2_dbl(theta.rma, theta.rma.se, \(mu, se) mu - qnorm(1-ci.alpha/2)*se ),
      theta.ub = map2_dbl(theta.rma, theta.rma.se, \(mu, se) mu + qnorm(1-ci.alpha/2)*se ),
      theta.rma.ci = map_chr(meta.rma, \(x) get_meta_ci(x, "rma", ci.alpha)),
      tau2 = map_dbl(meta.rma, "tau2"),
      tau = sqrt(tau2),
      I2 = map_dbl(meta.rma, "I2"),
      Q.stat = map_dbl(meta.rma, "QE"),
      Q.pval = map_dbl(meta.rma, "QEp"),
      Qprof.ci = map_chr(meta.rma, \(x) get_meta_ci(x, type = "Q") ),
      calibrated.yi = map(meta.rma, \(x) compute_calibrated(x) ),
      prob.leqneq0.1 = map_dbl(calibrated.yi, \(x){
          proportion_meaningful(x, q = -0.10, above = FALSE, interval.method, theta.rma, tau)
        }),
      prob.geq0.1 = map_dbl(calibrated.yi, \(x){
          proportion_meaningful(x, q = 0.10, above = TRUE, interval.method, theta.rma, tau)
        }),
      prob.lg.c = paste0("[", .round(prob.leqneq0.1), " / ",.round(prob.geq0.1) ,"]"),
      theta.rma.EE = gfs_compute_evalue(est=theta.rma, se=theta.rma.se, sd=1, what="EE"),
      theta.rma.ECI = gfs_compute_evalue(est=theta.rma, se=theta.rma.se, sd=1, what="ECI"),
      theta.pred.int = gfs_prediction_interval(x=calibrated.yi, theta = theta.rma, tau = tau ),
      # theta.metafor.blup = map(meta.rma, \(x){
      #   y = metafor::blup(x)
      #   rownames(y) <- x$data$group
      #   y
      # }),
      ## ====== RESULTS ON RISK-RATIO SCALE ===================================================== ##
      # compute exponentiated coefficients (used only when scale is binary/Likert)
      rr.calibrated.yi = map(calibrated.yi, \(x) exp(x)),
      rr.theta = exp(theta.rma),
      rr.theta.ci = map_chr(meta.rma, \(x) get_meta_ci(x, "rma.rr", ci.alpha, .exp=TRUE)),
      rr.tau = sqrt((exp(tau2) - 1) * exp(2 * theta.rma + tau2)),
      rr.prob.0.90 = map_dbl(rr.calibrated.yi, \(x){
          proportion_meaningful(x, q = 0.90, above = FALSE, interval.method, rr.theta, rr.tau)
        }),
      rr.prob.1.10 = map_dbl(rr.calibrated.yi, \(x){
          proportion_meaningful(x, q = 1.10, above = TRUE, interval.method, rr.theta, rr.tau)
      }),
      rr.prob.c = paste0("[", .round(rr.prob.0.90), " / ",.round(rr.prob.1.10) ,"]"),
      rr.theta.EE = gfs_compute_evalue(est=theta.rma, ci.low=theta.lb, ci.up = theta.ub, what="EE", type="RR"),
      rr.theta.ECI = gfs_compute_evalue(est=theta.rma, ci.low=theta.lb, ci.up = theta.ub, what="ECI", type="RR"),
      rr.theta.pred.int = gfs_prediction_interval(x=rr.calibrated.yi, theta = theta.rma, tau = tau , .exp=TRUE),
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
      pop.wgt.thetaci = map_chr(pop.wgt.meta, \(x) get_meta_ci(x, type = "FE", ci.alpha)),
      pop.wgt.meta.tidy = map(pop.wgt.meta, \(x) tidy(x, conf.int = TRUE)),
      ## ====== sensitivity analyses ============================================================ ##
      sens.rma.loo.influence = map(meta.rma, \(x) meta_loo_inf(x)),
      sens.reliability = map(meta.rma.tidy, \(x){
        reliability_corrected_estimates(
          theta=as.numeric(x['estimate']),
          se=as.numeric(x['std.error']),
          crit = qnorm(1-ci.alpha/2),
          lambda = reliability.vec
        )
      }),
      ## ====== pooled-pvalue =================================================================== ##
      global.pvalue = map_dbl(data, \(x){
        cnames <- colnames(x)
        out <- NULL
        if ("pvalue" %in% cnames) {
          out <- compute_global_pvalue(x$pvalue)
        }
        out
      }) #,
      ## ====== forest plots ==================================================================== ##
      ## DO NOT RUN HERE TOO MEMORY INTENSIVE
      #forest.plot = map(meta.rma, \(x){
      #  gfs_meta_forest_plot(x, better.name = better.name, p.title=p.title, p.subtitle = p.subtitle, include.influence = include.influence)
      #})
    )
  # drop the unneeded duplicate column
  if("FOCAL_PREDICTOR" %in% colnames(meta.res)){
    meta.res <- meta.res |> select(-FOCAL_PREDICTOR)
  }

  meta.res
}
