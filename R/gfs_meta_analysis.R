#' GFS Wrapper for Meta-Analysis
#'
#' Conducted meta-analysis (random effects or population weighted) for a set of effect of interest.
#'
#' @param meta.input a nested data.frame where `data` is a column
#' @param ... additional arguments
#' @return a list of meta-analysis results from the metafor package
#' @examples
#' # TO-DO
#' @export
gfs_meta_analysis <- function(meta.input, estimator = "PM", interval.method = "normal", better.name = NULL,
                              p.subtitle = NULL, ...) {
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
  meta.res <- meta.input %>%
    mutate(
      FOCAL_PREDICTOR = map(data, \(x){
        x$FOCAL_PREDICTOR[1]
      }),
      meta.pop.wgt = map(data, \(x){
        x <- add_pop_wgts(x)
        rma(
          yi = Est,
          sei = SE,
          weights = wi,
          data = x,
          method = "FE"
        )
      }),
      theta.pop.wgt = map_dbl(meta.pop.wgt, "beta"),
      meta.pop.wgt.tidy = map(meta.pop.wgt, \(x) tidy(x, conf.int = TRUE)),
      theta.pop.wgt.ci = map_chr(meta.pop.wgt, \(x){
        get_meta_ci(x, type = "FE")
      }),
      meta.fit = map(data, \(x){
        rma(
          yi = Est,
          sei = SE,
          data = x,
          method = estimator
        )
      }),
      meta.rma.tidy = map(meta.fit, \(x) tidy(x, conf.int = TRUE)),
      theta = map_dbl(meta.fit, "beta"),
      tau2 = map_dbl(meta.fit, "tau2"),
      tau = sqrt(tau2),
      I2 = map_dbl(meta.fit, "I2"),
      Q.stat = map_dbl(meta.fit, "QE"),
      Q.pval = map_dbl(meta.fit, "QEp"),
      Qprof.ci = map_chr(meta.fit, \(x){
        get_meta_ci(x, type = "Q")
      }),
      calibrated.yi = map(meta.fit, \(x){
        compute_calibrated(x)
      }),
      prob.leqneq0.1 = map_dbl(
        calibrated.yi, \(x){
          proportion_meaningful(x, q = -0.10, above = FALSE, interval.method, theta, tau)
        }
      ),
      prob.geq0.1 = map_dbl(
        calibrated.yi, \(x){
          proportion_meaningful(x, q = 0.10, above = TRUE, interval.method, theta, tau)
        }
      ),
      # compute exponentiated (used only when scale is binary/Likert)
      calibrated.yi.exp = map(calibrated.yi, \(x) exp(x)),
      rr.theta = exp(theta),
      rr.tau = sqrt((exp(tau2) - 1) * exp(2 * theta + tau2)),
      prob.rr0.90 = map_dbl(
        calibrated.yi.exp, \(x){
          proportion_meaningful(x, q = 0.90, above = FALSE, interval.method, rr.theta, rr.tau)
        }
      ),
      prob.rr1.10 = map_dbl(
        calibrated.yi.exp, \(x){
          proportion_meaningful(x, q = 1.10, above = TRUE, interval.method, rr.theta, rr.tau)
        }
      ),
      global.pvalue = map_dbl(data, \(x){
        cnames <- colnames(x)
        out <- NULL
        if ("pvalue" %in% cnames) {
          out <- compute_global_pvalue(x$pvalue)
        }
        out
      }),
      forest.plot = map(meta.fit, \(x){
        gfs_meta_forest_plot(x, better.name = better.name, p.subtitle = p.subtitle)
      })
    )
  meta.res
}
