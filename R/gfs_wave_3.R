#' GFS Coordinated Analsyes Wave 3
#'
#' Run regression analyses for specific focal predictor using the core GFS group analysis pathway.
#'
#' @param data.dir a character string defining where data are located
#' @param fx (NULL) a formula argument that can be used instead of your.pred/your.outcome to specify the set of predictors and outcome
#' @param your.pred a character string defining focal predictor (e.g., "PHYSICAL_HLTH_Y1")
#' @param your.outcome a character string defining outcome variable (e.g., "HAPPY_Y2")
#' @param covariates a character vector defining core set of covariates (default: NULL)
#' @param pca.variables s a character vector defining set of contemporaneous exposures that are input into PCA (default: NULL)
#' @param pc.rule principal components analysis use rule (default: "omit")
#' @param pc.cutoff a numeric value, can either be a fixed whole number (e.g., keep 7 PC in all countries) OR a proportion (e.g., 0.50)
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
#' The `pc.rule` argument determines how should the number of PCs is decided:
#' exclude PCs from analysis (default: "omit");
#' a fixed number ("constant");
#' minimunm total proportion of variance explained ("mintotal"); or
#' minimum proportion explain by individual PCs ("mincomp").
#' For "constant", `pc.cutoff` must of a number between 1 and length(contemporaneous.exposures) - 1.
#' For "mintotal", `pc.cutoff` must be strictly between 0 and 1, e.g. (0.70).
#' For "mincomp", `pc.cutoff` must be strictly between 0 and 1, e.g. (0.01).
#'
#' When standardize is TRUE, standardize the outcome and predictor using the survey-adjusted mean and variance.
#'
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
#' @export
gfs_wave_3_coordinated_analysis <- function(
    data.dir = NULL,
    direct.subset = NULL,
    fx = NULL,
    your.pred = NULL,
    your.outcome = NULL,
    covariates = NULL,
    pca.variables = NULL,
    wgt = ANNUAL_WEIGHT_R2,
    psu = PSU,
    strata = STRATA,
    imp.strata = "COUNTRY", ## new variable that replaces COUNTRY throughout that identifies the variable separating strata of imputations
    # advanced options: only change if you know what you are doing
    pc.cutoff = 7,
    pc.rule = "omit",
    force.linear = FALSE,
    force.binary = FALSE,
    robust.huberM = FALSE,
    robust.tune = 1,
    res.dir = "results",
    list.composites = NULL,
    domain.subset = NULL,
    country.subset = NULL,
    family = NULL,
    appnd.txt.to.filename = "",
    save.all = FALSE,
    ...) {



  #your.outcome = "LIFE_SAT_Y3"; your.pred = "HAPPY_Y2"; data.dir = "test/ignore/data/recoded"; wgt = as.name("ANNUAL_WEIGHT_R3"); psu = as.name("PSU"); strata = as.name("STRATA"); imp.strata = "COUNTRY"; covariates =  c("MODE_ANNUAL", "COV_AGE_GRP_Y1", "COV_GENDER", "COV_RACE_PLURALITY", "COV_EDUCATION_3_Y1", "COV_EMPLOYMENT_Y1", "COV_MARITAL_STATUS_Y1", "COV_ATTEND_SVCS_Y1", "COV_BORN_COUNTRY_Y1", "COV_REL2_Y1", "COV_PARENTS_12YRS_Y1", "COV_MOTHER_RELATN_Y1", "COV_FATHER_RELATN_Y1", "COV_OUTSIDER_Y1", "COV_ABUSED_Y1","COV_HEALTH_GROWUP_Y1", "COV_INCOME_12YRS_Y1","COV_SVCS_12YRS_Y1", "COV_MOTHER_NA", "COV_FATHER_NA"); pca.variables = pca.variables; pc.rule = "constant"; pc.cutoff = 7; res.dir = "test/ignore/results-primary"; domain.subset = domain.subset = NULL; family = NULL; force.linear = FALSE; force.binary = FALSE; robust.huberM = FALSE; robust.tune = 1; direct.subset = NULL; country.subset = NULL; fx=NULL; list.composites=NULL; appnd.txt.to.filename=""

  # your.outcome = OUTCOME.VEC[1]; your.pred = "PEACE_Y1"; data.dir = "data"; wgt = as.name("ANNUAL_WEIGHT_R2"); psu = as.name("PSU"); strata = as.name("STRATA"); imp.strata = as.name("COUNTRY"); covariates = DEMO.CHILDHOOD.PRED;pca.variables =""; list.composites = get_variable_codes('LIST.COMPOSITES')[[1]]; pc.cutoff = 7; pc.rule = "omit"; res.dir = "results-primary"; appnd.txt.to.filename = "_primary_wopc"; save.all = FALSE; domain.subset = domain.subset = NULL; family = NULL; force.linear = FALSE; force.binary = FALSE; robust.huberM = FALSE; robust.tune = 1; direct.subset = NULL; country.subset = NULL
  # data.dir = "test/ignore/data";  wgt = as.name("ANNUAL_WEIGHT_R2"); psu = as.name("PSU"); strata = as.name("STRATA");  force.linear = FALSE;  force.binary = FALSE;  robust.huberM = FALSE;  robust.tune = 1;  res.dir = "test/ignore/results-primary"

  suppressMessages({
    suppressWarnings({
      ## ensure that imp.strata is identified locally
      imp.strata <- as.name(imp.strata)

      if(!is.null(fx)){
        fx.char <- as.character(fx)

        your.outcome <- case_when(
          is.null(your.outcome) ~ fx.char[2],
          .default = your.outcome
        )
        covariates <- case_when(
          is.null(covariates) ~ c(str_split(fx.char[3],pattern = " \\+ ", simplify = TRUE)),
          !is.null(covariates) ~ c(covariates, c(str_split(fx.char[3],pattern = " \\+ ", simplify = TRUE))),
          .default = covariates
        )
        covariates <- unique(covariates)
      }

      if(is.null(list.composites)){
        list.composites = get_variable_codes('LIST.COMPOSITES')[[1]]
      }

      if(!is.null(your.pred)){
        # remove focal predictor from covariate vectors
        covariates <- covariates[str_detect(str_remove(covariates, "COV_"), your.pred, negate = TRUE)]
        pca.variables <- pca.variables[str_detect(pca.variables, your.pred, negate = TRUE)]

        # additionally remove variables that are components of the focal predictor
        #   e.g., if your.pred == "COMPOSITE_FLOURISHING_SECURE", then we need to remove all the
        #   items that make up that score.
        if (str_detect(your.pred, "COMPOSITE")) {
          pca.variables <- pca.variables[!(pca.variables %in% c(list.composites[[your.pred]]))]
        }

        ## add the term "focal_predictor" to covariate vector"
        covariates <- c(covariates, your.pred)
      }
      if(is.null(your.pred)){
        your.pred <- "NA_formula_entry_"
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
      if(outcome.type == "linear" & is.null(family)){
        family = stats::gaussian()
      }
      if (outcome.type == "RR" & is.null(family)){
        family = stats::quasipoisson()
      }


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
      #x <- country.vec[1]
      #.run_internal_func <- function(x){
      walk(country.vec, \(x){
        cur.country <- x
        ###
        # country-specific files
        country.files <- df.files[str_detect(df.files, cur.country)]
        ###

        ## test
        # svy.data.imp |>
        #   mutate(
        #     fit =  purrr::pmap(list(svy.data, {{imp.strata}}), \(x, grp){
        #       print(grp)
        #     })
        #   )
        is.valid <- FALSE
        is.valid <- .check_if_valid_comb(cur.country = cur.country,
                                         your.outcome = your.outcome,
                                         your.pred = your.pred,
                                         country.subset = country.subset,
                                         covariates =  covariates)

        if(is.valid){

          ## IF country-predictor combo is China + edu3_y2, then edu3_y1 must be removed as a control variable because the values are equal.



          fitted.reg.models <- map(country.files,\(x){
            #x <- country.files[1]
            svy.data.imp <- .get_data(
              file=x,
              data.dir = data.dir,
              wgt = {{wgt}},
              psu = {{psu}},
              strata = {{strata}},
              imp.strata = {{imp.strata}},
              direct.subset = direct.subset,
              domain.subset = domain.subset,
              pc.rule = pc.rule,
              pca.variables = pca.variables
            )
            # IF: pc.rule NOT omit
            # Conduct PCA and add PCs to data.frames
            fit.pca.summary = NULL
            if( str_to_lower(pc.rule) != "omit"){
              svy.data.imp <- svy.data.imp %>%
                mutate(
                  fit.pca = map(svy.data, \(x) {
                    keep.pca.variables <- keep_variable(pca.variables, data = x[["variables"]])
                    svyprcomp(
                      reformulate(pca.variables[keep.pca.variables]),
                      design = x,
                      scale. = TRUE,
                      scores = TRUE,
                      center = TRUE
                    )
                  }),
                  fit.eigen = map(svy.data, \(x) {
                    keep.pca.variables <- keep_variable(pca.variables, data = x[["variables"]])
                    get_eigenvalues(x, pca.variables[keep.pca.variables])
                  }),
                  pc.sdev = map(fit.pca, \(x) x$sdev),
                  pca.rotation = map(fit.pca, \(x) x$rotation), pca.sdev = pc.sdev
                )
              # get summary of PCA results to save to output file
              fit.pca.summary <- svy.data.imp %>%
                select(imp_num, {{imp.strata}}, pc.sdev, fit.eigen)%>%
                unnest(c(pc.sdev, fit.eigen)) %>%
                mutate(
                  PC = 1:n()
                ) %>%
                ungroup() %>%
                group_by({{imp.strata}}, PC) %>%
                summarise(
                  pc.var = mean(pc.sdev**2, na.rm = TRUE)
                ) %>%
                ungroup() %>%
                group_by({{imp.strata}}) %>%
                mutate(
                  prop.var = pc.var / sum(pc.var),
                  prop.sum = cumsum(prop.var),
                  Cumulative_Proportion_Explained = prop.sum
                )

              # check pc.cutoff to determine which PCs to use
              if (pc.cutoff %% 1 == 0) {
                keep.num.pc <- rep(pc.cutoff, length(unique(fit.pca.summary[[as.character({{imp.strata}})]])))
                names(keep.num.pc) <- unique(fit.pca.summary[[as.character({{imp.strata}})]])
              } else {
                # number of PCs varies by counry based on the total or individual PC % of the variation in the confounders the set of PC account for.
                if (str_to_lower(pc.rule) == "mintotal") {
                  keep.num.pc0 <- fit.pca.summary %>%
                    dplyr::filter(prop.sum >= pc.cutoff) %>%
                    dplyr::filter(PC == min(PC, na.rm = TRUE))
                  keep.num.pc <- keep.num.pc0$PC
                  names(keep.num.pc) <- keep.num.pc0[[as.character({{imp.strata}})]]
                }
                if (str_to_lower(pc.rule) == "mincomp") {
                  keep.num.pc0 <- fit.pca.summary %>%
                    dplyr::filter(prop.var >= pc.cutoff)
                  if (nrow(keep.num.pc0) < 1) {
                    # cutoff fails because too stringent, switching to a default of 0.02
                    keep.num.pc0 <- fit.pca.summary %>%
                      dplyr::filter(prop.var >= 0.02)
                  }
                  keep.num.pc0 <- keep.num.pc0 %>%
                    dplyr::filter(PC == max(PC, na.rm = TRUE))
                  keep.num.pc <- keep.num.pc0$PC
                  names(keep.num.pc) <- keep.num.pc0[[as.character({{imp.strata}})]]
                }
                if (str_to_lower(pc.rule) == "omit") {
                  # this is just to avoid errors and is not used
                  keep.num.pc <- rep(0, length(unique(fit.pca.summary[[as.character({{imp.strata}})]])))
                  names(keep.num.pc) <- unique(fit.pca.summary[[as.character({{imp.strata}})]])
                }
              }

            }
            # ============================================================================================== #
            # RUN REGRESSION ANALYSIS
            # svy.data.imp is a nested df by country & .imp
            svy.data.imp %>%
              dplyr::mutate(
                svy.fit = purrr::pmap(list(svy.data, {{imp.strata}}), \(x, grp) {
                  # x = svy.data.imp$svy.data[[1]]
                  tmp.fit <- NULL
                  # first check if ANY variance on outcome
                  run.analysis <- ifelse(var(x[["variables"]][[your.outcome]], na.rm=TRUE) > 0, TRUE, FALSE)
                  if (run.analysis) {
                    #cur.country <- x[["variables"]][["COUNTRY2"]][1]
                    # Next check each variable to make sure all have at least 2 levels, if only 1, exclude
                    keep.var <- keep_variable(covariates, data = x[["variables"]], reason = "any")
                    if (str_to_lower(pc.rule) == "omit") {
                      tmp.model <- reformulate(
                        response = your.outcome,
                        termlabels = covariates[keep.var]
                      )
                    } else {
                      tmp.model <- reformulate(
                        response = your.outcome,
                        termlabels = c(covariates[keep.var], paste0("PC_", 1:(keep.num.pc[as.character(grp)])))
                      )
                    }
                    tmp.fit <- gfs_svyglm(
                      formula = tmp.model,
                      svy.design = x,
                      family = family,
                      robust.huberM = robust.huberM,
                      robust.tune = robust.tune
                    )

                    tmp.fit
                  }
                }),
                fit.tidy = purrr::map(svy.fit, \(x) x$fit.tidy),
                fit.full = purrr::map(svy.fit, \(x) x$fit),
                residuals = purrr::map(svy.fit, \(x) x$residuals),
                retained.predictors = purrr::map(svy.fit, \(x) x$retained.predictors),
                fit.cor = purrr::map(svy.data, \(x) {
                  # x = svy.data.imp$svy.data[[1]]
                  tmp.fit <- NULL
                  # first check if ANY variance on outcome
                  run.analysis <- ifelse(var(x[["variables"]][[your.outcome]], na.rm=TRUE) > 0, TRUE, FALSE)
                  if (run.analysis) {
                    # Next check each variable to make sure all have at least 2 levels, if only 1, exclude
                    keep.var <- keep_variable(your.pred, data = x[["variables"]], reason = "any")
                    if(keep.var){
                      tmp.model <- reformulate(
                        response = your.outcome,
                        termlabels = your.pred,
                      )
                      # fit 1: no weights
                      fit.dof <- stats::glm(tmp.model, data = x[["variables"]])
                      #print(fit.dof)
                      vcom <- fit.dof$df.residual
                      #fit to get product moment correct/biserial correlation
                      tmp.fit <- survey::svyglm(tmp.model, design = x, family = stats::gaussian())
                      #print(tmp.fit)
                      tmp.fit <- tidy(tmp.fit)[2,]
                      tmp.fit <- tmp.fit %>%
                        mutate(
                          f.statistic = (estimate**2) / (std.error**2),
                          df.num = 1,
                          df.dem = vcom,
                          p.value = 1 - pf(f.statistic, df.num, df.dem),
                          # see: Lumley, T. & Scott, A. Fitting Regression Models to Survey Data. Statistical Science 32, 265–278 (2017). p. 269 left column, middle paragraph
                          p.value = case_when(
                            p.value == 0 ~ 2.2e-16,
                            .default = p.value
                          ),
                          vcom = vcom
                        )
                    }
                  }
                  tmp.fit
                }),
                fit.pca.summary = list(fit.pca.summary = fit.pca.summary)
              ) %>%
              select({{imp.strata}}, imp_num, fit.full, fit.tidy, residuals, fit.cor,
                     pca.sdev, pca.rotation, fit.pca.summary) %>%
              ungroup()


          }) |> bind_rows()

          ## extract PCA summary
          fit.pca.summary <- get_pca_summary(fit = fitted.reg.models, imp.strata = {{imp.strata}})

          # re-estimate basic model with the max number of PCs used to get the variable names
          tmp.dat <- .get_data(file = country.files[1],
                               data.dir = data.dir,
                               wgt = {{wgt}},
                               psu = {{psu}},
                               strata = {{strata}},
                               imp.strata = {{imp.strata}},
                               direct.subset = direct.subset,
                               domain.subset = domain.subset,
                               pc.rule = pc.rule,
                               pca.variables = pca.variables
          )
          #cur.country = as.character(tmp.dat$COUNTRY[1])
          keep.var <- rep(FALSE, length(covariates))
          for(i in 1:length(keep.var)){
            if(covariates[i] %in% colnames(tmp.dat$data[[1]])){
              keep.var[i] <- keep_variable(covariates[i], data = tmp.dat$data[[1]], reason = "any")
            }
          }
          if (str_to_lower(pc.rule) == "omit") {
            tmp.model <- reformulate(
              response = your.outcome,
              termlabels = c(covariates[keep.var])
            )
          } else {
            # check pc.cutoff to determine which PCs to use
            if (pc.cutoff %% 1 == 0) {
              keep.num.pc <- rep(pc.cutoff, length(unique(fit.pca.summary[[as.character({{imp.strata}})]])))
              names(keep.num.pc) <- unique(fit.pca.summary[[as.character({{imp.strata}})]])
            } else {
              # number of PCs varies by counry based on the total or individual PC % of the variation in the confounders the set of PC account for.
              if (str_to_lower(pc.rule) == "mintotal") {
                keep.num.pc0 <- fit.pca.summary %>%
                  dplyr::filter(prop.sum >= pc.cutoff) %>%
                  dplyr::filter(PC == min(PC, na.rm = TRUE))
                keep.num.pc <- keep.num.pc0$PC
                names(keep.num.pc) <- keep.num.pc0[[as.character({{imp.strata}})]]
              }
              if (str_to_lower(pc.rule) == "mincomp") {
                keep.num.pc0 <- fit.pca.summary %>%
                  dplyr::filter(prop.var >= pc.cutoff)
                if (nrow(keep.num.pc0) < 23) {
                  # cutoff fails because too stringent, switching to a default of 0.02
                  keep.num.pc0 <- fit.pca.summary %>%
                    dplyr::filter(prop.var >= 0.02)
                }
                keep.num.pc0 <- keep.num.pc0 %>%
                  dplyr::filter(PC == max(PC, na.rm = TRUE))
                keep.num.pc <- keep.num.pc0$PC
                names(keep.num.pc) <- keep.num.pc0[[as.character({{imp.strata}})]]
              }
              if (str_to_lower(pc.rule) == "omit") {
                # this is just to avoid errors and is not used
                keep.num.pc <- rep(0, length(unique(fit.pca.summary[[as.character({{imp.strata}})]])))
                names(keep.num.pc) <- unique(fit.pca.summary[[as.character({{imp.strata}})]])
              }
            }
            #tmp.country <- names(keep.num.pc)[which(keep.num.pc == max(keep.num.pc))[1]]
            tmp.model <- reformulate(
              response = your.outcome,
              termlabels = c(covariates[keep.var], paste0("PC_", 1:(keep.num.pc[cur.country])))
            )
          }
          tmp.fit <- tmp.dat$data[[1]] %>% glm(tmp.model, data = .)
          # which model doesn't matter for this step, we only need the variable names

          coef.order <- names(tmp.fit$coefficients)
          coef.order <- c(
            coef.order[!(stringr::str_detect(coef.order, "(Intercept)") | stringr::str_detect(coef.order, "COV_REL1") | stringr::str_detect(coef.order, "COV_REL2"))],
            "COV_GENDER_Y1Prefer not to answer",
            "COV_REL1_Y1Islam", "COV_REL1_Y1Hinduism", "COV_REL1_Y1Judaism", "COV_REL1_Y1Buddhism",
            "COV_REL1_Y1Primal,Animist, or Folk religion", "COV_REL1_Y1Chinesefolk/traditional religion",
            "COV_REL1_Y1Christianity", "COV_REL1_Y1Combined",
            "COV_REL2_Y1Islam", "COV_REL2_Y1Hinduism", "COV_REL2_Y1Judaism", "COV_REL2_Y1Buddhism",
            "COV_REL2_Y1Primal,Animist, or Folk religion", "COV_REL2_Y1Chinesefolk/traditional religion",
            "COV_REL2_Y1Christianity", "COV_REL2_Y1Combined",
            "(Intercept)"
          )

          ## Pool estimates across imputations
          results.pooled <- fitted.reg.models %>%
            select({{imp.strata}}, imp_num, fit.tidy) %>%
            unnest(c(fit.tidy)) %>%
            ungroup() %>%
            group_by(term, {{imp.strata}}) %>%
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
            arrange({{imp.strata}}, term) %>%
            ungroup()

          ## Pool estimates of correlations
          cor.pooled <- fitted.reg.models %>%
            select({{imp.strata}}, imp_num, fit.cor) %>%
            unnest(c(fit.cor)) %>%
            ungroup() %>%
            group_by(term, {{imp.strata}}) %>%
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
            arrange({{imp.strata}}, term) %>%
            ungroup()

          ## ============================================================================ #
          ## ----- Compute outcome & predictor SD -----
          # - Need SD of all columns in design matrix
          # - Used for computing E-values & standardized estimates
          # - For continuous outcomes, need to use evalue.OLS(.) [require approx outcome standard deviation]
          sd.pooled <- fitted.reg.models %>%
            select({{imp.strata}}, imp_num, fit.full) %>%
            mutate(
              term.var = map(fit.full, \(x){
                #x <- fitted.reg.models$fit.full[[1]]
                my.matrix <- model.matrix(x)
                my.wgts <- x$survey.design$variables |>
                  select({{wgt}}) |> unlist() |> as.numeric()
                var.est <- as.data.frame(matrix(apply(my.matrix, 2, FUN=function(x){
                  matrixStats::weightedVar(x, w = my.wgts, na.rm = TRUE)
                }),nrow=1))
                colnames(var.est) <- colnames(my.matrix)
                y.var <- data.frame(outcome.sd = matrixStats::weightedVar(x$y, w = my.wgts, na.rm = TRUE))

                cbind(y.var, var.est)
              })
            ) |>
            select({{imp.strata}}, term.var) %>%
            unnest(c(term.var)) %>%
            group_by({{imp.strata}}) %>%
            summarize(
              across(everything(), \(x){
                sqrt(mean(x, na.rm=TRUE))
              })
            ) %>% ungroup() |>
            pivot_longer(
              cols=-c({{imp.strata}},outcome.sd),
              names_to = "term",
              values_to = "predictor.sd"
            )

          ## ============================================================================ #
          ## ----- Relabel output (helpful for generating documents) ----
            varlist <- stringr::str_split_1(paste0(tmp.fit$formula)[[3]], " \\+ ")
          if(pc.rule != "omit"){
            if(any(str_detect(varlist, "PC_"))){
              varlist[str_detect(varlist, "PC_")] <- "PC"
              varlist <- unique(varlist)
            }
          }
          termlist <- as.character(unique(results.pooled$term))[-1]
          base_variable <- sapply(termlist, function(b) {
            match <- sapply(varlist, function(a) {
              startsWith(b, a)
            })
            varlist[which(match)]
          })
          base_variable <- base_variable %>%
            as.data.frame() %>%
            pull(.)
          levels <- gsub(paste(unlist(base_variable), collapse = "|"), "", termlist)
          termlabels <- data.frame(
            original = c(rep("(Ref:)", length(termlist)), termlist, "(Intercept)"),
            Variable = c(rep(base_variable, 2), "(Intercept)"),
            Category = c(rep(levels, 2), "(Intercept)")
          )
          termlabels <- termlabels %>%
            mutate(
              Variable = stringr::str_remove(Variable, "COV_"),
              Category = case_when(
                stringr::str_detect(Variable, "PC_") ~ stringr::str_remove(stringr::str_sub(Variable, -2, -1), "_"),
                Variable == "MOTHER_RELATN_Y1" ~ "Very good/somewhat good",
                Variable == "FATHER_RELATN_Y1" ~ "Very good/somewhat good",
                Variable == "RACE_PLURALITY" ~ "Non-plurality groups",
                Variable == "MOTHER_NA" ~ "Mother NA flag",
                Variable == "FATHER_NA" ~ "Father NA flag",
                .default = Category
              )
            )
          ## ============================================================================ #
          ## ----- Compute Evalues ----
          tmp.output <- results.pooled %>%
            left_join(sd.pooled, by = c(as.character({{imp.strata}}), "term")) %>%
            ungroup()
          tmp.output$EE <- 0
          tmp.output$ECI <- 0
          i <- 1
          tmp.output <- tmp.output %>%
          	rowwise() %>%
          	mutate(
          		EE = gfs_compute_evalue(
          			est = estimate.pooled,
	              	se = se.pooled,
              		sd = outcome.sd,
              		ci.low = ci.low,
              		ci.up = ci.up,
              		type = outcome.type,
              		what = "EE"
              	),
              	ECI = gfs_compute_evalue(
          			est = estimate.pooled,
	              	se = se.pooled,
              		sd = outcome.sd,
              		ci.low = ci.low,
              		ci.up = ci.up,
              		type = outcome.type,
              		what = "ECI"
              	)
          	) %>% ungroup()

          output <- tmp.output %>%
            left_join(termlabels,
                      by = c("term" = "original"),
                      relationship = "many-to-many"
            )
          ## ============================================================================ #
          ## ----- Compute standardized estimates -----
          output <- output %>%
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
          ## ============================================================================ #
          ## ----- Rename colnames -----
          output <- output %>%
            rename(
              est = estimate.pooled,
              se = se.pooled,
              ci.lb = ci.low,
              ci.ub = ci.up,
              df = df.approx,
              std.est = std.estimate.pooled,
              std.se = std.se.pooled,
              std.ci.lb = std.ci.low,
              std.ci.ub = std.ci.up
            ) |>
            mutate(
              outcome = your.outcome, .before = 2
            )
          ## ============================================================================ #
          ## ----- Compute correlations: standardized regression coefficient ------
          cor.output <- cor.pooled %>%
            left_join(sd.pooled, by = c(as.character({{imp.strata}}), "term")) %>%
            ungroup()
          cor.output <- cor.output %>%
            mutate(
              cor.est = estimate.pooled * (predictor.sd / outcome.sd),
              cor.se = se.pooled * (predictor.sd / outcome.sd),
              cor.ci.low = case_when(
                df.approx > 1 ~ cor.est - stats::qt(0.975, df.approx) * cor.se,
                .default = NA
              ),
              cor.ci.up = case_when(
                df.approx > 1 ~ cor.est + stats::qt(0.975, df.approx) * cor.se,
                .default = NA
              ),
              outcome = your.outcome,
              predictor = your.pred
            ) |>
            rename(
              "cov.est" = estimate.pooled,
              "cov.se" = se.pooled,
              "cov.ci.low" = ci.low,
              "cov.ci.up" = ci.up
            ) |>
            select({{imp.strata}}, outcome, predictor, cor.est, cor.se, cor.ci.low, cor.ci.up, cov.est, cov.se, cov.ci.low, cov.ci.up, df.approx, t.statistic, f.statistic, p.value, miss.info, outcome.sd, predictor.sd, estimates.by.imp)

          # ============================================================================ #
          # ============================================================================ #
          # Meta analysis input - is a simplified data.frame with only:
          # 		country, variable, category, estimate, standard error, and global p-value
          # 		This reduced file is helpful for the meta-analysis app OR internal meta-analysis code
          # metainput <- output %>%
          #   select(
          #     {{imp.strata}},
          #     Variable,
          #     Category,
          #     estimate.pooled,
          #     se.pooled,
          #     p.value,
          #     ci.low,
          #     ci.up,
          #     df.approx,
          #     outcome.sd,
          #     predictor.sd,
          #     std.estimate.pooled,
          #     std.se.pooled,
          #     std.ci.low,
          #     std.ci.up
          #   ) %>%
          #   group_by({{imp.strata}}, Variable) %>%
          #   dplyr::filter(!(Category == "(Ref:)")) %>%
          #   dplyr::filter(str_detect(Variable,your.pred)) %>% ungroup()
          # colnames(metainput) <-
          #   c(as.character({{imp.strata}}), "Variable", "Category",
          #     "est", "se", "pvalue", "ci.lb", "ci.ub",
          #     "df", "outcome.sd", "predictor.sd",
          #     "std.est", "std.se", "std.ci.lb", "std.ci.ub")
          # metainput <- metainput %>%
          #   mutate(
          #     FOCAL_OUTCOME = your.outcome,
          #     FOCAL_PREDICTOR = your.pred,
          #     .before = Variable
          #   )

          # ============================================================================ #
          # ============================================================================ #
          # prepare output -- just some pre- relabeling to make the online supplement easier
          output <- output %>%
            group_by(Variable) %>%
            fill(Variable) %>%
            ungroup() %>%
            mutate(
              Variable = case_when(Variable == "AGE_GRP" ~ "Year of birth", .default = Variable),
              Category = case_when(
                Variable == "REL1" &
                  str_detect(Category, "Combined") ~ "Collapsed affiliations with prevalence<3%",
                Category == "25-29" ~ "1993-1998; age 25-29",
                Category == "30-39" ~ "1983-1993; age 30-39",
                Category == "40-49" ~ "1973-1983; age 40-49",
                Category == "50-59" ~ "1963-1973; age 50-59",
                Category == "60-69" ~ "1953-1963; age 60-69",
                Category == "70-79" ~ "1943-1953; age 70-79",
                Category == "80 or older" ~ "1943 or earlier; age 80+",
                Category == "(Ref:)" &
                  Variable == "AGE_GRP" ~ "(Ref: 1998-2005; current age: 18-24)",
                Category == "(Ref:)" &
                  Variable == "GENDER" ~ "(Ref: Male)",
                Category == "(Ref:)" &
                  Variable == "PARENTS_12YRS" ~ "(Ref: Parents married)",
                Category == "(Ref:)" &
                  Variable == "SVCS_12YRS" ~ "(Ref: Never)",
                # Category == "(Ref:)" & Variable == "SVCS_MOTHER" ~ "(Ref: Never)",
                # Category == "(Ref:)" & Variable == "SVCS_FATHER" ~ "(Ref: Never)",
                Category == "(Ref:)" &
                  Variable == "MOTHER_RELATN" ~ "(Ref: Very bad/somewhat bad)",
                Category == "(Ref:)" &
                  Variable == "FATHER_RELATN" ~ "(Ref: Very bad/somewhat bad)",
                # Category == "(Ref:)" & Variable == "MOTHER_LOVED" ~ "(Ref: No)",
                # Category == "(Ref:)" & Variable == "FATHER_LOVED" ~ "(Ref: No)",
                Category == "(Ref:)" &
                  Variable == "OUTSIDER" ~ "(Ref: No)",
                Category == "(Ref:)" &
                  Variable == "ABUSED" ~ "(Ref: No)",
                Category == "(Ref:)" &
                  Variable == "HEALTH_GROWUP" ~ "(Ref: Good)",
                Category == "(Ref:)" &
                  Variable == "BORN_COUNTRY" ~ "(Ref: Born in this country)",
                Category == "(Ref:)" &
                  Variable == "INCOME_12YRS" ~ "(Ref: Got by)",
                Category == "(Ref:)" &
                  Variable == "RACE_PLURALITY" ~ "(Ref: Plurality group)",
                Category == "(Ref:)" &
                  Variable == "MOTHER_NA" ~ "(Ref: Non-missing Mother Flags)",
                Category == "(Ref:)" &
                  Variable == "FATHER_NA" ~ "(Ref: Non-missing Father Flags)",
                Category == "(Ref:)" &
                   {{imp.strata}} %in% c(
                    "Argentina",
                    "Australia",
                    "Brazil",
                    "China",
                    "Germany",
                    "Hong Kong",
                    "Japan",
                    "Mexico",
                    "Poland",
                    "South Africa",
                    "Spain",
                    "Sweden",
                    "Tanzania",
                    "United Kingdom",
                    "United States"
                  ) &
                  Variable == "REL1" ~ "(Ref: No religion/Atheist/Agnostic)",
                Category == "(Ref:)" &
                  {{imp.strata}} %in% c("Egypt", "Indonesia", "Turkey", "Turkiye") &
                  Variable == "REL1" ~ "(Ref: Islam)",
                Category == "(Ref:)" &
                  {{imp.strata}} %in% c("India") &
                  Variable == "REL1" ~ "(Ref: Hinduism)",
                Category == "(Ref:)" &
                  {{imp.strata}} %in% c("Israel") &
                  Variable == "REL1" ~ "(Ref: Judaism)",
                Category == "(Ref:)" &
                  {{imp.strata}} %in% c("Kenya", "Nigeria", "Philippines") &
                  Variable == "REL1" ~ "(Ref: Christianity)",
                .default = Category
              )
            ) %>%
            #dplyr::mutate(
            #  FOCAL_OUTCOME = your.outcome,
            #  FOCAL_PREDICTOR = your.pred,
            #  .before = Variable
            #) %>%
            dplyr::mutate(
              model.family = as.character(family[[1]]),
              outcome.type = outcome.type,
              outcome.scale = get_outcome_scale(your.outcome)
            )

          outfile <- here::here(
            res.dir,
            paste0(your.pred,  "_regressed_on_", your.outcome, "_saved_results",appnd.txt.to.filename,".RData")
          )

          ## subset the fitted reg moels for outputing
          fitted.reg.models <- fitted.reg.models |> ungroup() |>
            select({{imp.strata}}, imp_num, fit.tidy, residuals, fit.cor, fit.pca.summary, pca.rotation)

          ## load the previously "saved" result and append results for the next country
          if(cur.country != country.vec[1]  & file.exists(outfile)){
            load(outfile, env.res <- new.env())
            output <- rbind(output, env.res$output)
            if(save.all){
              cor.output <- rbind(cor.output, env.res$cor.output )
              fitted.reg.models <- rbind(fitted.reg.models, env.res$fitted.reg.models)
            }
          }
          ## save/overwrite existing saved results file so everything is in one object
          if(save.all){
            save( output, cor.output , fitted.reg.models, file = outfile )
          } else {
            save( output, file = outfile )
          }

        }

      })

    })
  })

}
