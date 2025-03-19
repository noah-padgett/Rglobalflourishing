#' GFS Focal Predictor Analysis
#'
#' Run regression analyses for specific focal predictor using the core GFS group analysis pathway.
#'
#' @param data long data.frame must have COUNTRY and .imp columns (default: df.imp.long)
#' @param your.pred a character string defining focal predictor (e.g., "PHYSICAL_HLTH_Y1")
#' @param your.outcome a character string defining outcome variable (e.g., "HAPPY_Y2")
#' @param covariates a character vector defining core set of covariates (default: NULL)
#' @param contemporaneous.exposures a character vector defining set of contemporaneous exposures that are input into PCA (default: NULL)
#' @param standardize a logical (default: TRUE) of whether to standardized the predictor and outcome
#' @param pc.rule principal components analysis use rule (default: "omit")
#' @param pc.cutoff a numeric value, can either be a fixed whole number (e.g., keep 7 PC in all countries) OR a proportion (e.g., 0.50)
#' @param force.linear a logical of whether to force a linear model (default: FALSE)
#' @param robust.huberM a  logical of whether to use a robust variant of the linear regression model (default: FALSE), see below for additional details.
#' @param robust.tune a numeric value defining the tuning parameter for the robust.huberM option.
#' @param res.dir a character string defining directory to save results to.
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
gfs_run_regression_single_outcome <- function(
    data = NULL,
    your.pred = NULL,
    your.outcome = NULL,
    covariates = NULL,
    contemporaneous.exposures = NULL,
    wgt = ANNUAL_WEIGHT_R1,
    psu = PSU,
    strata = STRATA,
    # advanced options: only change if you know what you are doing
    standardize = TRUE,
    pc.cutoff = 7,
    pc.rule = "omit",
    force.linear = FALSE,
    robust.huberM = FALSE,
    robust.tune = 1,
    res.dir = NULL,
    list.composites = NULL, ...) {
  suppressMessages({
    suppressWarnings({
      # remove focal predictor from covariate vectors
      covariates <- covariates[str_detect(str_remove(covariates, "COV_"), your.pred, negate = TRUE)]
      var.cont.exposures <- contemporaneous.exposures[str_detect(contemporaneous.exposures, your.pred, negate = TRUE)]

      # additionally remove variables that are components of the focal predictor
      #   e.g., if your.pred == "COMPOSITE_FLOURISHING_SECURE", then we need to remove all the
      #   items that make up that score.
      if (str_detect(your.pred, "COMPOSITE")) {
        var.cont.exposures <- var.cont.exposures[!(var.cont.exposures %in% c(list.composites[[your.pred]]))]
      }

      if (is.null(res.dir)) {
        res.dir <- here::here(getwd(),"results")
      }
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
        force.linear ~ "linear",
        .default = outcome.type
      )

      # outcomes which need the countries to be subset...
      {
        if (str_detect(your.outcome,"APPROVE_GOVT") | str_detect(your.pred,"APPROVE_GOVT")) {
          data <- data %>%
            dplyr::filter(str_detect(COUNTRY,"Egypt", negate=TRUE)) %>%
            dplyr::filter(str_detect(COUNTRY2,"Egypt", negate=TRUE)) %>%
            mutate(
              COUNTRY = fct_drop(COUNTRY),
              COUNTRY2 = fct_drop(COUNTRY2)
            )
        }
        if (str_detect(your.outcome,"ABUSED") | str_detect(your.pred,"ABUSED")) {
          data <- data %>%
            dplyr::filter(str_detect(COUNTRY,"Israel", negate=TRUE)) %>%
            dplyr::filter(str_detect(COUNTRY2,"Israel", negate=TRUE)) %>%
            mutate(
              COUNTRY = fct_drop(COUNTRY),
              COUNTRY2 = fct_drop(COUNTRY2)
            )
        }
      }

      # convert to nested survey object
      svy.data.imp <- data %>%
        mutate(
          COUNTRY = COUNTRY2
        ) %>%
        group_by(COUNTRY, .imp) %>%
        nest() %>%
        mutate(
          data = map(data, \(x) {
            x$PRIMARY_OUTCOME <- as.numeric(x[, your.outcome, drop = TRUE])
            x
          }),
          data = map(data, \(x) {
            x$FOCAL_PREDICTOR <- as.numeric(x[, your.pred, drop = TRUE])
            x
          }),
          data = map(data, \(x) {
            x %>% mutate(
              PRIMARY_OUTCOME = case_when(
                standardize == FALSE ~ PRIMARY_OUTCOME,
                outcome.type == "linear" & standardize == TRUE ~ svy_scale(PRIMARY_OUTCOME, {{wgt}}, {{psu}}, {{strata}}),
                .default = PRIMARY_OUTCOME
              ),
              FOCAL_PREDICTOR = case_when(
                standardize == FALSE ~ FOCAL_PREDICTOR,
                outcome.type == "linear" & standardize == TRUE ~ svy_scale(FOCAL_PREDICTOR, {{wgt}}, {{psu}}, {{strata}}),
                .default = FOCAL_PREDICTOR
              )
            )
          }),
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
            # svydesign(
            #   data = x,
            #   ids = ~{{psu}},
            #   strata = ~{{strata}},
            #   weights = ~{{wgt}},
            #   calibrate.formula = ~1
            # )
          })
        )


      # IF: pc.rule NOT omit
      # Conduct PCA and add PCs to data.frames
      fit.pca.summary = NULL # need to initialize object to not throw error when saving data
      if( str_to_lower(pc.rule) != "omit"){
        svy.data.imp <- svy.data.imp %>%
          mutate(
            data = map(data, \(x) {
              keep.cont.exposures <- keep_variable(var.cont.exposures, data = x)
              append_pc_to_df(x, var = var.cont.exposures[keep.cont.exposures], std = TRUE)
            }),
            svy.data = map(svy.data, \(x) {
              keep.cont.exposures <- keep_variable(var.cont.exposures, data = x[["variables"]])
              append_pc_to_df(x, var = var.cont.exposures[keep.cont.exposures], std = TRUE)
            }),
            fit.pca = map(svy.data, \(x) {
              keep.cont.exposures <- keep_variable(var.cont.exposures, data = x[["variables"]])
              svyprcomp(
                reformulate(var.cont.exposures[keep.cont.exposures]),
                design = x,
                scale. = TRUE,
                scores = TRUE,
                center = TRUE
              )
            }),
            fit.eigen = map(svy.data, \(x) {
              keep.cont.exposures <- keep_variable(var.cont.exposures, data = x[["variables"]])
              get_eigenvalues(x, var.cont.exposures[keep.cont.exposures])
            })
          )
        # get summary of PCA results to save to output file
        fit.pca.summary <- svy.data.imp %>%
          mutate(
            pc.sdev = map(fit.pca, \(x) x$sdev),
            pc.rotation = map(fit.pca, \(x) x$rotation)
          ) %>%
          select(.imp, COUNTRY, pc.sdev, fit.eigen) %>%
          unnest(c(pc.sdev, fit.eigen)) %>%
          mutate(
            PC = 1:n()
          ) %>%
          ungroup() %>%
          group_by(COUNTRY, PC) %>%
          summarise(
            pc.var = mean(pc.sdev**2, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          group_by(COUNTRY) %>%
          mutate(
            prop.var = pc.var / sum(pc.var),
            prop.sum = cumsum(prop.var),
            Cumulative_Proportion_Explained = prop.sum
          )

        # check pc.cutoff to determine which PCs to use
        if (pc.cutoff %% 1 == 0) {
          keep.num.pc <- rep(pc.cutoff, length(unique(data$COUNTRY2)))
          names(keep.num.pc) <- unique(data$COUNTRY2)
        } else {
          # number of PCs varies by counry based on the total or individual PC % of the variation in the confounders the set of PC account for.
          if (str_to_lower(pc.rule) == "mintotal") {
            keep.num.pc0 <- fit.pca.summary %>%
              dplyr::filter(prop.sum >= pc.cutoff) %>%
              dplyr::filter(PC == min(PC, na.rm = TRUE))
            keep.num.pc <- keep.num.pc0$PC
            names(keep.num.pc) <- keep.num.pc0$COUNTRY2
          }
          if (str_to_lower(pc.rule) == "mincomp") {
            keep.num.pc0 <- fit.pca.summary %>%
              dplyr::filter(prop.var >= pc.cutoff)
            if (nrow(keep.num.pc0) < 22) {
              # cutoff fails because too stringent, switching to a default of 0.02
              keep.num.pc0 <- fit.pca.summary %>%
                dplyr::filter(prop.var >= 0.02)
            }
            keep.num.pc0 <- keep.num.pc0 %>%
              dplyr::filter(PC == max(PC, na.rm = TRUE))
            keep.num.pc <- keep.num.pc0$PC
            names(keep.num.pc) <- keep.num.pc0$COUNTRY2
          }
          if (str_to_lower(pc.rule) == "omit") {
            # this is just to avoid errors and is not used
            keep.num.pc <- rep(0, length(unique(data$COUNTRY2)))
            names(keep.num.pc) <- unique(data$COUNTRY2)
          }
        }
        tmp.df.pca <- data.frame(
          COUNTRY = names(keep.num.pc),
          NumPCAkeep = keep.num.pc
        )
        tmp.df.pca$Rule <- pc.rule
        tmp.df.pca$Cutoff <- pc.cutoff
        fit.pca.summary <- dplyr::left_join(fit.pca.summary, tmp.df.pca)
      }

      # # test:
      # tmp.data <- svy.data.imp %>%
      # filter(COUNTRY == "United States", .imp == 1)

      # get_aicc_by_pc <- function(keep.num.pc, x){
      # #x = tmp.data$svy.data[[1]]
      # #keep.num.pc = 1
      # # Next check each variable to make sure all have at least 2 levels, if only 1, exclude
      # keep.var <- unlist(lapply(covariates, check.var, dat = x[['variables']]))
      # tmp.model <- reformulate(
      # response = "PRIMARY_OUTCOME",
      # termlabels = c("FOCAL_PREDICTOR", covariates[keep.var], paste0("PC_",1:keep.num.pc))
      # )
      # tmp.fit <-  svyglm(tmp.model, design = x)
      # k <- tmp.fit$rank +1 # number of dimensions of design matrix plus residual variance
      # n <- sum(tmp.fit$weights)
      # # AICC
      # tmp.fit$aic + n*(n+k)/(n-k-2)

      # }

      # est.aicc <- map(1:(length(var.cont.exposures)-1), \(y){
      # get_aicc_by_pc(y, tmp.data$svy.data[[1]])
      # })
      # plot.dat <- data.frame(PC = 1:(length(var.cont.exposures)-1), AICC = unlist(est.aicc))

      # ggplot(plot.dat, aes(x=PC, y=AICC))+
      # geom_point()+
      # geom_line()

      # ============================================================================================== #
      # RUN REGRESSION ANALYSIS
      # svy.data.imp is a nested df by country & .imp
      svy.data.imp <- svy.data.imp %>%
        dplyr::mutate(
          fit.tidy = purrr::map(svy.data, \(x) {
            tmp.fit <- NULL
            # first check if ANY variance on outomce
            run.analysis <- ifelse(var(x[["variables"]][["PRIMARY_OUTCOME"]], na.rm=TRUE) > 0, TRUE, FALSE)
            if (run.analysis) {
              cur.country <- x[["variables"]][["COUNTRY2"]][1]
              # Next check each variable to make sure all have at least 2 levels, if only 1, exclude
              keep.var <- keep_variable(covariates, data = x[["variables"]])
              if (str_to_lower(pc.rule) == "omit") {
                tmp.model <- reformulate(
                  response = "PRIMARY_OUTCOME",
                  termlabels = c("FOCAL_PREDICTOR", covariates[keep.var])
                )
              } else {
                tmp.model <- reformulate(
                  response = "PRIMARY_OUTCOME",
                  termlabels = c("FOCAL_PREDICTOR", covariates[keep.var], paste0("PC_", 1:(keep.num.pc[cur.country])))
                )
              }

              if (outcome.type == "linear") {
                tmp.fit <- gfs_svyglm(
                  tmp.model,
                  svy.design = x,
                  family = stats::gaussian(),
                  robust.huberM = robust.huberM,
                  robust.tune = robust.tune
                )
              }
              if (outcome.type == "RR") {
                tmp.fit <- gfs_svyglm(
                  tmp.model,
                  svy.design = x,
                  family = stats::quassipoisson(),
                  robust.huberM = FALSE
                )
              }
              tmp.fit$fit.tidy
            }
          })
        ) %>%
        ungroup()

      # re-estimate basic model with the max number of PCs used to get the variable names
      tmp.dat <- svy.data.imp %>% dplyr::filter(str_detect(COUNTRY, "United States"))
      keep.var <- keep_variable(covariates, data = tmp.dat$data[[1]])
      if (str_to_lower(pc.rule) == "omit") {
        tmp.model <- reformulate(
          response = "PRIMARY_OUTCOME",
          termlabels = c("FOCAL_PREDICTOR", covariates[keep.var])
        )
      } else {
        tmp.country <- names(keep.num.pc)[which(keep.num.pc == max(keep.num.pc))[1]]
        tmp.model <- reformulate(
          response = "PRIMARY_OUTCOME",
          termlabels = c("FOCAL_PREDICTOR", covariates[keep.var], paste0("PC_", 1:(keep.num.pc[tmp.country])))
        )
      }
      tmp.fit <- tmp.dat$data[[1]] %>% glm(tmp.model, data = .)
      # which model doesn't matter for this step, we only need the variable names

      coef.order <- names(tmp.fit$coefficients)
      coef.order <- c(
        coef.order[!(stringr::str_detect(coef.order, "(Intercept)") | stringr::str_detect(coef.order, "COV_REL1"))],
        "COV_GENDER_Y1Prefer not to answer",
        "COV_REL1_Y1Islam", "COV_REL1_Y1Hinduism", "COV_REL1_Y1Judaism", "COV_REL1_Y1Buddhism",
        "COV_REL1_Y1Primal,Animist, or Folk religion", "COV_REL1_Y1Chinesefolk/traditional religion",
        "COV_REL1_Y1Christianity", "COV_REL1_Y1Combined",
        "(Intercept)"
      )

      results.pooled <- svy.data.imp %>%
        select(COUNTRY, .imp, fit.tidy) %>%
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

      ## Relabel output
      varlist <- stringr::str_split_1(paste0(tmp.fit$formula)[[3]], " \\+ ")
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

      # compute outcome & predictor SD Only used for continuous/forced continuous models
      # - For continuous outcomes, need to use evalue.OLS(.)
      # - require approx outcome standard deviation
      sd.pooled <- svy.data.imp %>%
        mutate(
          est = purrr::map_dbl(svy.data, \(x) {
          	survey::svyvar(~PRIMARY_OUTCOME, design = x, na.rm=TRUE)
          }),
          pred.var = purrr::map_dbl(svy.data, \(x) {
          	survey::svyvar(~FOCAL_PREDICTOR, design = x, na.rm=TRUE)
          })
        ) %>%
        ungroup() %>%
        select(COUNTRY, .imp, est, pred.var) %>%
        group_by(COUNTRY) %>%
        summarize(
          outcome.sd = sqrt(mean(est, na.rm=TRUE)),
          predictor.sd = sqrt(mean(pred.var, na.rm=TRUE))
        ) %>%
        select(COUNTRY, outcome.sd, predictor.sd)

      # Now, IF the outcome and predictor were first standardized, this the above isn't necessary... and needs to be overwritten by a vector of 1s
      if (standardize) {
        sd.pooled$outcome.sd <- 1
        sd.pooled$predictor.sd <- 1
      }


      # Compute Evalues
      tmp.output <- results.pooled %>%
        left_join(sd.pooled, by = "COUNTRY") %>%
        ungroup()

      # Note: I could not get the following mutate(.) to work, not sure what is wrong, but the for loop works...
      # mutate(
      #   EE  = gfs_compute_evalue(
      #    est = estimate.pooled,
      #    se = se.pooled,
      #    sd = sd.pooled,
      #    ci.low = ci.low,
      #    ci.up = ci.up,
      #    type = outcome.type,
      #    what = "EE"
      #  ),
      #  ECI =  gfs_compute_evalue(
      #    est = estimate.pooled,
      #    se = se.pooled,
      #    sd = sd.pooled,
      #    ci.low = ci.low,
      #    ci.up = ci.up,
      #    type = outcome.type,
      #    what = "ECI"
      #   )
      # )
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


      output <- tmp.output %>%
        left_join(termlabels,
                  by = c("term" = "original"),
                  relationship = "many-to-many"
        ) %>%
        arrange(Variable)


      # Meta analysis input - is a simplified data.frame with only:
      # 		country, variable, category, estimate, standard error, and global p-value
      # 		This reduced file is helpful for the meta-analysis app OR internal meta-analysis code
      metainput <- output %>%
        select(
          COUNTRY,
          Variable,
          Category,
          estimate.pooled,
          se.pooled,
          p.value,
          ci.low,
          ci.up,
          df.approx,
          outcome.sd
        ) %>%
        group_by(COUNTRY, Variable) %>%
        dplyr::filter(!(Category == "(Ref:)")) %>%
        dplyr::filter(Variable == "FOCAL_PREDICTOR")
      colnames(metainput) <-
        c("Country", "Variable", "Category", "Est", "SE", "pvalue", "ci.lb", "ci.ub", "df", "outcome.sd")
      metainput <- metainput %>%
        mutate(
          OUTCOME = your.outcome,
          FOCAL_PREDICTOR = your.pred,
          .before = Variable
        )

      # ============================================================================ #
      # ============================================================================ #
      # Online Supplement Analyses - variable specific

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
              COUNTRY %in% c(
                "Argentina",
                "Australia",
                "Brazil",
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
              COUNTRY %in% c("Egypt", "Indonesia", "Turkey") &
              Variable == "REL1" ~ "(Ref: Islam)",
            Category == "(Ref:)" &
              COUNTRY %in% c("India") &
              Variable == "REL1" ~ "(Ref: Hinduism)",
            Category == "(Ref:)" &
              COUNTRY %in% c("Israel") &
              Variable == "REL1" ~ "(Ref: Judaism)",
            Category == "(Ref:)" &
              COUNTRY %in% c("Kenya", "Nigeria", "Philippines") &
              Variable == "REL1" ~ "(Ref: Christianity)",
            .default = Category
          )
        ) %>%
        dplyr::mutate(
          OUTCOME = your.outcome,
          FOCAL_PREDICTOR = your.pred,
          .before = Variable
        ) %>%
        dplyr::mutate(
          id.Est = .round(estimate.pooled),
          id.SE = .round(se.pooled),
          id.CI = paste0("(", .round(ci.low), ",", .round(ci.up), ")"),
          rr.Est = .round(exp(estimate.pooled)),
          logrr.SE = .round(se.pooled),
          rr.CI = paste0("(", .round(exp(ci.low)), ",", .round(exp(ci.up)), ")")
        )

      save(
        output,
        metainput,
        fit.pca.summary,
        file = here::here(
          res.dir,
          paste0(your.pred,
                 "_regressed_on_",
                 your.outcome, "_saved_results.RData")
        )
      )
    })
  })

  # ======================================= #
  # ======================================= #
  # Only the metainput is DIRECTLY RETURNED #
  # ======================================= #
  # ======================================= #
  metainput
}
