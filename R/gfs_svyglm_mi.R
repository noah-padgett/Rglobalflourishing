#' GFS Wrapper for generalized linear models with multiple imputation
#'
#' Run GLMs through the survey package with the folder structure for memory ease and pooling across imputed datasets. Cannot incorporate principle components into the analysis; if that is desired using the gfs_run_regression_single_outcome(.) function.
#'
#' @param fx (NULL) a formula argument that can be used instead of your.pred/your.outcome to specify the set of predictors and outcome
#' @param data.dir a character string defining where data are located
#' @param force.linear a logical of whether to force a linear model (default: FALSE)
#' @param force.binary a logical of whether to force a Poisson model (default: FALSE)
#' @param robust.huberM a  logical of whether to use a robust variant of the linear regression model (default: FALSE), see below for additional details.
#' @param robust.tune a numeric value defining the tuning parameter for the robust.huberM option.
#' @param res.dir a character string defining directory to save results to.
#' @param direct.subset a quoted subset statement to directly subset the sample
#' @param domain.subset a quoted subset statement to run domain estimation
#' @param country.subset a string vector of countries included in analysis
#' @param appnd.txt.to.filename (optional) character string to help differentiate saved results file (default "")
#' @param save.all (FALSE) logical of whether to save the fitted regression models from each imputed dataset (this is a double-checking feature and should generally be set to FALSE unless you plan to probe the individual regression models--see the methods paper for Wave 2 for example of it's used)
#' @param ... other arguments passed to svyglm or glmrob functions
#' @returns a data.frame that contains the meta-analysis input results
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' When force.linear is TRUE, forces the estimation of a linear model even for binary/Likert outcomes.
#' I think this is sometimes called as a "linear probability model."
#'
#' When robust.huberM is TRUE, for (approximately) continuous outcomes, you have the option of
#' alternatively using a "robust m-estimator" with Huber style robustness weights in addition to
#' the complex sampling design adjustments. It's unknown whether this makes a meaningful difference,
#' but preliminary testing suggests small differences in point estimates but sometimes dramatic
#' changes to standard errors for reasons that are unclear to me. Could be due to a strange
#' interaction of robustness weights, attrition weights, and post-stratified sampling weights.
#'
#'
gfs_svyglm_mi <- function(
    fx = NULL,
    data.dir = NULL,
    wgt = ANNUAL_WEIGHT_R2,
    psu = PSU,
    strata = STRATA,
    force.linear = FALSE,
    force.binary = FALSE,
    robust.huberM = FALSE,
    robust.tune = 1,
    res.dir = "results",
    direct.subset = NULL,
    domain.subset = NULL,
    country.subset = NULL,
    family = NULL,
    appnd.txt.to.filename = "",
    save.all = FALSE,
    ...) {

  #fx = COMPOSITE_FLOURISHING_SECURE_Y2 ~ 0 + COV_GENDER;  data.dir = "test/ignore/data";  wgt = as.name("ANNUAL_WEIGHT_R2"); psu = as.name("PSU"); strata = as.name("STRATA");  force.linear = FALSE;  force.binary = FALSE;  robust.huberM = FALSE;  robust.tune = 1;  res.dir = "test/ignore/results-primary";  direct.subset = NULL;  domain.subset = NULL;  family = NULL;  appnd.txt.to.filename = ""; save.all = FALSE; country.subset = c("China", "Hong Kong", "India", "Japan", "United States");  domain.subset = quote(REL1_Y1 == 4)

  suppressMessages({
    suppressWarnings({
      if(is.null(fx)){
        warning("no model formula supplied.")

        reg.centered = TRUE
      } else {
        fx.char <- as.character(fx)
        your.outcome <- fx.char[2]
        covariates <- c(str_split(fx.char[3],pattern = " \\+ ", simplify = TRUE))
        covariates <- unique(covariates)
        reg.centered = ifelse(any(c(str_split(fx.char[3],pattern = " \\+ ", simplify = TRUE)) == "0"), FALSE, TRUE)

      }
      res.dir <- here(res.dir)
      if (!dir.exists(res.dir)) {
        dir.create(res.dir)
      }
      # construct "type" indicator
      outcome.type <- case_when(
        get_outcome_scale(your.outcome) %in% c("cont", "Continuous") ~ "linear",
        get_outcome_scale(your.outcome) %in% c("bin", "likert") ~ "RR",
        .default = "linear"
      )
      outcome.type <- case_when(
        force.binary ~ "RR",
        force.linear ~ "linear",
        .default = outcome.type
      )

      df.files <- list.files(data.dir)
      df.files <- df.files[str_detect(df.files, "recoded_imputed_data_obj")]
      country.vec <- str_remove(df.files, "recoded_imputed_data_obj_") |>
        stringr::word(1, sep = "\\_imp") |>
        unique() |>
        sort()
      if(!is.null(country.subset)){
          country.vec <- country.subset
      }

      ##
      ##x <- country.vec[1]
      .run_internal_func <- function(x){
        cur.country <- x
        ###
        # country-specific files
        country.files <- df.files[str_detect(df.files, x)]
        ###
        .check_if_valid_comb <- function(){
          out <- TRUE
          # combinations of outcomes/predictors known to lead to issues such as 100% within country,
          # zero change from wave 1, or zero variation in outcome within country
          if (str_detect(your.outcome,"APPROVE_GOVT")) {
            if(cur.country %in% c("China","Egypt") ){
              out <- FALSE & out
            }
          }
          if (str_detect(your.outcome,"ABUSED")) {

            if(cur.country %in% c("Israel") ){
              out <- FALSE & out
            }

          }
          if(str_detect(your.outcome,"BELIEVE_GOD")) {
            if(cur.country %in% c("Egypt") ){
              out <- FALSE & out
            }
          }
          if (str_detect(your.outcome,"BELONGING") ) {
            if(cur.country %in% c("China") ){
              out <- FALSE & out
            }
          }
          if (str_detect(your.outcome, "EDUCATION_3")){
            if(cur.country %in% c("China")){
              out <- TRUE & out
            }
          }
          if (str_detect(your.outcome,"SAY_IN_GOVT")) {
            if(cur.country %in% c("China") ){
              out <- FALSE & out
            }
          }
          if (str_detect(your.outcome,"COVID_DEATH")) {
            if(cur.country %in% c("China") ){
              out <- FALSE & out
            }
          }
          out
        }
        .get_data <- function(file){
          #print(file)
          data <- readr::read_rds(here::here(data.dir, file))
          ## code for direct subset (NOT SUBPOPULATUON)
          if(!is.null(direct.subset)){
            data <- subset(data, eval(direct.subset))
            data <- data %>%
              mutate(
                "{{wgt}}" := n() * {{wgt}} / sum( {{wgt}} , na.rm = TRUE)
              )
          }

          # renaming .imp outside of dplyr
          data$imp_num <- data$.imp

          # convert to nested survey object
          svy.data.imp <- data %>%
            mutate(
              COUNTRY = COUNTRY2
            ) %>%
            group_by(COUNTRY, imp_num) %>%
            nest() %>%
            mutate(
              data = map(data, \(tmp.dat){
                tmp.dat %>%
                  mutate(across(where(is.factor), \(x) droplevels(x)))
              }),
              svy.data = map(data, \(x) {
                x %>%
                  as_survey_design(
                    ids = {{psu}},
                    strata = {{strata}},
                    weights = {{wgt}},
                    calibrate.formula = ~1
                  )
              })
            )
          if(!is.null(domain.subset)){
            svy.data.imp <- svy.data.imp %>%
              mutate(
                data = map(data, \(x){
                  subset(x, eval(domain.subset))
                }),
                svy.data = map(svy.data, \(x){
                  subset(x, eval(domain.subset))
                })
              )
          }
          svy.data.imp
        }

        if(.check_if_valid_comb()){
          #x <- country.files[1]

          fitted.reg.models <- map(country.files,\(x){
            svy.data.imp <- .get_data(x)
            # ============================================================================================== #
            # RUN REGRESSION ANALYSIS
            # svy.data.imp is a nested df by country & .imp
            svy.data.imp %>%
              dplyr::mutate(
                svy.fit = purrr::map(svy.data, \(x) {
                  # x = svy.data.imp$svy.data[[1]]
                  tmp.fit <- NULL
                  # first check if ANY variance on outomce
                  run.analysis <- ifelse(var(x[["variables"]][[your.outcome]], na.rm=TRUE) > 0, TRUE, FALSE)
                  if (run.analysis) {
                    cur.country <- x[["variables"]][["COUNTRY2"]][1]

                    if(outcome.type == "linear" & is.null(family)){
                      family = stats::gaussian()
                    }
                    if (outcome.type == "RR" & is.null(family)){
                      family = stats::quasipoisson()
                    }
                    tmp.fit <- gfs_svyglm(
                      formula = fx,
                      svy.design = x,
                      family = family,
                      robust.huberM = robust.huberM,
                      robust.tune = robust.tune,
                      var.check = FALSE
                    )
                    tmp.fit
                  }
                }),
                fit.tidy = map(svy.fit, \(x) x$fit.tidy),
                fit.full = map(svy.fit, \(x) x$fit),
                fit.lasso = map(svy.fit, \(x) x$fit.lasso),
                residuals = map(svy.fit, \(x) x$residuals)
              ) %>%
              select(COUNTRY, imp_num, fit.tidy, fit.full, fit.lasso, residuals) %>%
              ungroup()
          }) |> bind_rows()

          if(!save.all){
            fitted.reg.models <- fitted.reg.models |> select(COUNTRY, imp_num, fit.tidy, fit.full)
          }

          # re-estimate basic model with the max number of PCs used to get the variable names
          tmp.dat <- .get_data(country.files[1])
          tmp.fit <- tmp.dat$data[[1]] %>% glm(fx, data = .)
          # which model doesn't matter for this step, we only need the variable names

          ## Joint term wald tests
          results.wald <- data.frame(
            term = "BLANK",
            wald.fvalue = NA_real_,
            wald.df.num = NA_real_,
            wald.df.dem = NA_real_,
            wald.p.value = NA_real_
          )
          try({
            results.wald.test <- fitted.reg.models %>%
              select(-fit.tidy) %>%
              mutate(
                wald.tests = map(fit.full, \(x){
                  xterms <- attributes(terms(x))$term.labels
                  if(!reg.centered){
                    x <- svyglm(update(x$formula, as.formula(paste(".~.-0"))), design = x$survey.design )
                  }
                  out <- lapply(xterms, FUN = function(y){
                    tryCatch({
                      survey::regTermTest(x, y, method = "Wald")
                    }, error = function(e) {
                      return(NA)
                    })
                  })
                  names(out) <- xterms
                  out
                }),
                converged = map_lgl(wald.tests, \(x){
                  !is.na(x)
                })
              ) %>%
              select(-fit.full) |>
              filter(converged)
            ## unnest by term then pool across wald tests
            results.wald <- map(names(results.wald.test$wald.tests[[1]]),\(x){
              df.wald <- data.frame(fstat=0, df1=0,df2=0)
              i <- 1
              for(i in 1:nrow(results.wald.test)){
                df.wald[i,1] <- results.wald.test$wald.tests[[i]][[x]]$Ftest
                df.wald[i,2] <- results.wald.test$wald.tests[[i]][[x]]$df
                df.wald[i,3] <- results.wald.test$wald.tests[[i]][[x]]$ddf
              }
              fit.f <- colMeans(df.wald, na.rm=TRUE)
              p <- 1 - pf(fit.f[1], fit.f[2], fit.f[3])
              data.frame(
                term = x,
                wald.fvalue = fit.f[1],
                wald.df.num = fit.f[2],
                wald.df.dem = fit.f[3],
                wald.p.value = case_when(p == 0 ~ 2.22e-16, .default=p)
              )
            })
            names(results.wald) <- names(results.wald.test$wald.tests[[1]])
            results.wald <- results.wald |> bind_rows()
          }, silent = TRUE)


          ## Pool estimtes across imputations
          results.pooled <- fitted.reg.models %>%
            select(-fit.full) %>%
            unnest(c(fit.tidy)) %>%
            ungroup() %>%
            group_by(term, COUNTRY) %>%
            nest() %>%
            mutate(
              pooled.est = map(data, \(x){
                gfs_pool_estimates(x)
              }),
              estimates.by.imp = data
            ) %>%
            unnest(c(pooled.est)) %>%
            select(-c(data)) %>%
            unique() %>%
            mutate(
              term = factor(term)
            ) %>%
            arrange(COUNTRY, term) %>%
            ungroup()

          # compute outcome & predictor SD Only used for continuous/forced continuous models
          sd.pooled <- fitted.reg.models %>%
            mutate(
              term.var = map(fit.full, \(x){
                my.matrix <- model.matrix(x)
                my.wgts <- x$weights
                var.est <- as.data.frame(matrix(apply(my.matrix, 2, FUN=function(x){
                  matrixStats::weightedVar(x, w = my.wgts)
                }),nrow=1))
                colnames(var.est) <- colnames(my.matrix)
                y.var <- data.frame(outcome.sd = matrixStats::weightedVar(x$y, w = my.wgts))
                cbind(y.var, var.est)
              })
            ) |>
            select(COUNTRY, term.var) %>%
            unnest(c(term.var)) %>%
            group_by(COUNTRY) %>%
            summarize(
              across(everything(), \(x){
                sqrt(mean(x, na.rm=TRUE))
              })
            ) %>% ungroup() |>
            pivot_longer(
              cols=-c(COUNTRY,outcome.sd),
              names_to = "term",
              values_to = "predictor.sd"
            )

          # Compute Evalues
          tmp.output <- results.pooled %>%
            left_join(sd.pooled, by = c("COUNTRY", "term")) %>%
            ungroup()
          # working version:
          tmp.output$EE <- 0
          tmp.output$ECI <- 0
          i <- 1
          for (i in 1:nrow(tmp.output)) {
            tmp.output$EE[i] <- gfs_compute_evalue(
              est = tmp.output$estimate.pooled[i],
              se = tmp.output$se.pooled[i],
              sd = tmp.output$outcome.sd[i],
              ci.low = tmp.output$ci.low[i],
              ci.up = tmp.output$ci.up[i],
              type = outcome.type,
              what = "EE"
            )
            tmp.output$ECI[i] <- gfs_compute_evalue(
              est = tmp.output$estimate.pooled[i],
              se = tmp.output$se.pooled[i],
              sd = tmp.output$outcome.sd[i],
              ci.low = tmp.output$ci.low[i],
              ci.up = tmp.output$ci.up[i],
              type = outcome.type,
              what = "ECI"
            )
          }
          # Compute standardized estimates
          # note: for binary outcomes only need to multiply by the predictor standard deviation
          output <- tmp.output %>%
            mutate(
              std.estimate.pooled = estimate.pooled * (predictor.sd / outcome.sd),
              std.se.pooled = se.pooled * (predictor.sd / outcome.sd),
              std.ci.low = case_when(
                df.approx > 1 ~ std.estimate.pooled - stats::qt(0.975, df.approx) * std.se.pooled,
                .default = NA
              ),
              std.ci.up = case_when(
                df.approx > 1 ~ std.estimate.pooled + stats::qt(0.975, df.approx) * std.se.pooled,
                .default = NA
              )
            )

          ## merge in wald tests by term
          output <- cbind(output, data.frame(wald.fvalue=NA, wald.df.num = NA, wald.df.dem = NA, wald.p.value=NA))
          i <- 1
          for(i in 1:nrow(results.wald)){
            output$wald.fvalue[str_detect(output$term, results.wald$term[i])] <- results.wald$wald.fvalue[i]
            output$wald.df.num[str_detect(output$term, results.wald$term[i])] <- results.wald$wald.df.num[i]
            output$wald.df.dem[str_detect(output$term, results.wald$term[i])] <- results.wald$wald.df.dem[i]
            output$wald.p.value[str_detect(output$term, results.wald$term[i])] <- results.wald$wald.p.value[i]
          }

          # ============================================================================ #
          # ============================================================================ #
          # Online Supplement Analyses - variable specific

          outfile <- here::here(
            res.dir,
            paste0(your.outcome,  "_svyglm_saved_results", appnd.txt.to.filename,".RData")
          )
          ## load the previously "saved" result and append results for the next country
          if(cur.country != country.vec[1] & file.exists(outfile)){
            load(outfile, env.res <- new.env())
            output <- rbind(output, env.res$output)
          }
          ## save/overwrite existing saved results file so everything is in one object
          save(output, file = outfile)
        }

      }
      ##
      walk(country.vec, \(x){
         try({ .run_internal_func(x) })
      })

      # for(x in country.vec){
      #   print(x)
      #   .run_internal_func(x)
      # }


    })
  })

}
