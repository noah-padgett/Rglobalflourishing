# Create the full regression output
#' Pool Results across Imputations
#'
#' Pool results across imputed datasets using Rubin's Rules.
#'
#' @param data a long.data.frame with at least a .imp column and a fit.tidy
#' @return a one-row data.frame with columns
#' * nimp
#' * estimate.pooled
#' * se.pooled
#' * df.approx
#' * t.statistic
#' * f.statistic
#' * ci.low
#' * ci.up
#' * p.value
#' * miss.info
#' see below for additional information about these statistics
#' @description
#' TO-DO
#' @export
gfs_pool_estimates <- function(data) {
  # TO-DO add in checks of columns
  data %>%
    mutate(Var = std.error^2) %>%
    summarise(
      nimp = n(),
      estimate.pooled = mean(estimate),
      Vw = sum(Var) / nimp,
      Vb = sum((estimate - mean(estimate))^2 / (nimp - 1)),
      vcom = mean(vcom)
    ) %>%
    mutate(
      Vb = ifelse(Vb <= 0, 0.001, Vb),
      Vt = Vw + (1 + 1 / nimp) * Vb,
      se.pooled = sqrt(Vt),
      lambda = (Vb + (Vb / nimp)) / Vt,
      vm = (nimp - 1) / lambda^2,
      vm.obs = ((vcom) + 1) / ((vcom) + 3) * (vcom) * (1 - lambda),
      vm.adj = (vm * vm.obs) / (vm + vm.obs),
      df.approx = ifelse(Vb > 0 & vm.adj > 1, vm.adj, vm.obs),
      t.statistic = estimate.pooled / se.pooled,
      f.statistic = (estimate.pooled**2) / (se.pooled**2),
      ci.low = case_when(
        df.approx > 1 ~ estimate.pooled - stats::qt(0.975, df.approx) * se.pooled,
        .default = NA
      ),
      ci.up = case_when(
        df.approx > 1 ~ estimate.pooled + stats::qt(0.975, df.approx) * se.pooled,
        .default = NA
      ),
      p.value = case_when(
        df.approx > 1 ~ 1 - stats::pf(f.statistic, 1, df.approx),
        .default = NA
      ),
      p.value = case_when(
        p.value <= 1e-16 ~ 1e-16,
        .default = p.value
      ),
      miss.info = (lambda+2/(vm+3))/(lambda+1)
      # miss info reported from mitools package (https://github.com/cran/mitools/blob/master/R/MI.R; line 143)
    ) %>%
    select(!c(Vw, Vb, Vt, lambda, vm, vm.obs, vm.adj, vcom))
}
