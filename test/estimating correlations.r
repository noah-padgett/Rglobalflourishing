# Script to estimate meta-analyzed country and country specific correlations of two variables

  source("load_packages.R")
  library(msm)
  source("outcome_variables.R")
  source("demo_childhood_variables.R")

  Nimp.keep = 20
  load("gfs_imputed_data_20imp.RData")

  source("recoding_imputed_data.R")

svy.data.imp <- df.imp.long %>%
  group_by(COUNTRY, .imp) %>%
  nest() %>%
  mutate(
    svy.data = map(
      data,
      ~ svydesign(
        data = .,
        id =  ~ PSU,
        strata =  ~ STRATA,
        weights = ~ WGT
      )
    )
  )
.round <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}
get_meta_ci <- function(fit, type = "Q", .digits=2) {
  if (type == "Q") {
    tmp <- confint(fit, type = "PL")
    ci <- paste0(
      "(",
      .round(tmp[["random"]][2, 2], .digits), ",",
      .round(tmp[["random"]][2, 3], .digits), ")"
    )
  }
  if ( type == "rma") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci = paste0(
      "(",
      .round(tmp[1, "conf.low", drop = TRUE], .digits), ",",
      .round(tmp[1, "conf.high", drop = TRUE], .digits), ")"
    )
  }
  if ( type == "rma.rr") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci = paste0(
      "(",
      .round(exp(tmp[1, "conf.low", drop = TRUE]), .digits), ",",
      .round(exp(tmp[1, "conf.high", drop = TRUE]), .digits), ")"
    )
  }
  if (type == "FE") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci <- paste0(
      "(",
      .round(tmp[1, "conf.low", drop = TRUE], .digits), ",",
      .round(tmp[1, "conf.high", drop = TRUE], .digits), ")"
    )
  }
  ci
}

# method = "identity" uses the "raw" correlation and deltamethod standard error of the correlation
gfs_cor <- function(data, x0, y0, wgt = "WGT", method = "cor"){
 data %>%
  mutate(
  	n = map_dbl(svy.data, \(x) sum(x[['variables']][[wgt]])),
  	fit.var = map(svy.data, \(x) svyvar(reformulate(c(x0,y0)), design = x)),
  	cor.est = map_dbl(fit.var, \(x) cov2cor(as.matrix(x))[1,2] ),
  	cor.se = map_dbl(fit.var, \(x){
  		xcov <- attr(x, which="var")
		  xcov <- xcov[c(1,2,4), c(1,2,4)]
		  msm::deltamethod(~ x2/(sqrt(x1)*sqrt(x3)), c(x[1,1],x[1,2],x[2,2]), xcov)
  	})
  ) %>%
  select(COUNTRY, .imp, n, cor.est, cor.se) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  summarise(
  	N = mean(n),
    nimp = n(),
    estimate.pooled = mean(cor.est),
    Vw = sum(cor.se**2) / nimp,
    Vb = sum((cor.est - mean(cor.est))^2 / (nimp - 1))
  ) %>%
    mutate(
      vcom = N - 3,
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
      ci = paste0("(", round(ci.low,3), ",", round(ci.up,3),")"),
      p.value = case_when(
        df.approx > 1 ~ 1 - stats::pf(f.statistic, 1, df.approx),
        .default = NA
      ),
      p.value = case_when(
        p.value <= 1e-16 ~ 1e-16,
        .default = p.value
      ),
      miss.info = (lambda+2/(vm+3))/(lambda+1)
   ) %>%
   mutate(
   ESTIMATED_COR = paste0(x0, "--", y0),
   .before = 1) %>%
   group_by(ESTIMATED_COR) %>% nest() %>%
  	mutate(
  		meta.rma = map(data, \(x){
  		  ## do you assume that the sample size is good representation of uncertainty (method = "cor") or do you assume the estimated standard error from teh deltamethod and survey package is better (method = "identity")
  			if(method == "cor"){
  				x = escalc(measure = "COR", ri = x[['estimate.pooled']], ni = x[['N']])
  			}
  			if(method == "identity"){
  				x = x %>% mutate(
  					yi = estimate.pooled,
  					vi = se.pooled**2
  				)
  			}
  			x %>%
  			rma(
          	yi = yi,
          	vi = vi,
          	data = .,
          	method = "PM"
   	    	)
  		}),
  		meta.rma.tidy = map(meta.rma, \(x) tidy(x, conf.int = TRUE)),
      	cor.rma = map_dbl(meta.rma.tidy, "estimate"),
      	cor.rma.se = map_dbl(meta.rma.tidy, "std.error"),
      	cor.rma.ci = map_chr(meta.rma, \(x) get_meta_ci(x, "rma")),
      	tau2 = map_dbl(meta.rma, "tau2"),
      	tau = sqrt(tau2),
      	I2 = map_dbl(meta.rma, "I2"),
      	Q.stat = map_dbl(meta.rma, "QE"),
      	Q.pval = map_dbl(meta.rma, "QEp"),
      	Qprof.ci = map_chr(meta.rma, \(x){
       	 get_meta_ci(x, type = "Q")
      	})
  	)
 }



estimated.cor <- gfs_cor(svy.data.imp, "HAPPY", "LIFE_SAT", method = "identity")

# estimated correlation & 95% CI
estimated.cor$cor.rma
estimated.cor$cor.rma.ci

# country-specific correlations
estimated.cor$data[[1]] %>% select(COUNTRY, estimate.pooled, se.pooled, ci)

# alternatively
estimated.cor <- gfs_cor(svy.data.imp, "HAPPY", "LIFE_SAT", method = "identity")

# estimated correlation & 95% CI
estimated.cor$cor.rma
estimated.cor$cor.rma.ci

# country-specific correlations
estimated.cor$data[[1]] %>% select(COUNTRY, estimate.pooled, se.pooled, ci)
