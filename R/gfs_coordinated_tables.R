#' Construct GFS Main text results
#'
#' Generated a word document containing the results for the meta-analytic outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param meta.wopc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did NOT include the principal components in the country-specific regression analyses
#' @param meta.wpc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did INCLUDED the principal components in the country-specific regression analyses
#' @param focal.better.name a character that is used as the printed name in tables/captions to denote the focal predictor
#' @param p.bonferroni a number (e.g., 0.007), is internally determined based on number of rows in meta.wopc
#' @param baseline.pred a vector of characters defining which baseline characteristics were used as control variables. This can be used to force the inclusion some variable into the main text summary table.
#' @param outcome.vec a character vector of outcomes names (e.g., "HAPPY_W2") that are to be printed in the main text meta-analytic summary table. Name MUST be included in the meta.wopc (meta.wpc) nested data.frames column (OUTCOME0), otherwise the variable won't be printed.
#' @param mylabels an optional character vector that will be printed out in specific rows of tables 2/3 depending on the specification pf outcome.vec
#' @param focal.predictor.reference.value (character) describing the baseline/reference group for the focal predictor.
#' @param res.dir (character) defaults to "results", and will be created if needed to story results document
#' @param ... other arguments as needed
#' @returns a word document saved to the current 'results/' directory
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_generate_main_doc <- function(df.raw=NULL, meta.wopc=NULL, meta.wpc=NULL, focal.better.name="Focal Predictor", focal.predictor.reference.value="estimated population mean of focal predictor", focal.predictor=NULL, p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL, res.dir = "results"){

  if(is.null(p.bonferroni)){
    p.bonferroni = 0.05/nrow(meta.wopc)
  }
  if(is.null(baseline.pred)){
    baseline.pred = str_remove(
      c(
        "COV_AGE_GRP_W1",
        "COV_GENDER_W1",
        "COV_EDUCATION_3_W1",
        "COV_EMPLOYMENT_W1",
        "COV_MARITAL_STATUS_W1",
        "COV_ATTEND_SVCS_W1",
        "COV_BORN_COUNTRY_W1",
        "COV_PARENTS_12YRS_W1",
        "COV_SVCS_12YRS_W1",
        "COV_MOTHER_RELATN_W1",
        "COV_FATHER_RELATN_W1",
        "COV_OUTSIDER_W1",
        "COV_ABUSED_W1",
        "COV_HEALTH_GROWUP_W1",
        "COV_INCOME_12YRS_W1",
        "COV_REL1_W1",
        "COV_RACE_PLURALITY_W1"
      ), "COV_")
  }
  if (!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    normal_portrait <- block_section(
      prop_section(page_size = page_size(orient = "portrait"), type = "continuous")
    )
    extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 29.7 / 2.54 * 2,
        height = 29.7 / 2.54
      ),
      type = "continuous"
    ))

    landscape_three_columns <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous",
        section_columns = section_columns(widths = c(3.24,3.24,3.24))
      )
    )
    landscape_one_column <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous"
      )
    )
    # body_end_section_landscape(x, w = 21/2.54, h = 29.7/2.54)
  }
  ## DEFINE VECTOR OF OUTCOMES
  {
    if(is.null(outcome.vec)){
      OUTCOME.VEC <- c(
        # Flourishing
        'blank',
        "COMPOSITE_FLOURISHING_SECURE",
        "COMPOSITE_FLOURISHING",
        "COMPOSITE_HAPPI_LIFE_SAT",
        "COMPOSITE_HEALTH",
        "COMPOSITE_MEANING_PURPOSE",
        "COMPOSITE_CHARACTER",
        "COMPOSITE_SUBJECTIVE_SOC_CONN",
        "COMPOSITE_FINL_MAT_WORRY",

        # Psychological well-being
        'blank',
        'HAPPY',
        'LIFE_SAT',
        'WB_TODAY',
        'WB_FIVEYRS',
        'EXPECT_GOOD',
        'FREEDOM',
        'PEACE',
        'LIFE_BALANCE',
        'CAPABLE',
        'WORTHWHILE',
        'LIFE_PURPOSE',
        'MENTAL_HEALTH',

        # Social Well-Being
        'blank',
        'CONTENT',
        'SAT_RELATNSHP',
        'PEOPLE_HELP',
        'CLOSE_TO',
        'APPROVE_GOVT',
        'SAY_IN_GOVT',
        'BELONGING',
        'SAT_LIVE',
        'TRUST_PEOPLE',
        'GROUP_NOT_REL',

        # Psychological Distress
        'blank',
        'THREAT_LIFE',
        'COMPOSITE_DEPRESSION', 'DEPRESSED', 'INTEREST',
        'COMPOSITE_ANXIETY', 'FEEL_ANXIOUS', 'CONTROL_WORRY',
        'SUFFERING',

        # Social Distress
        'blank',
        'LONELY',
        'DISCRIMINATED',

        # Character & Prosocial Behavior
        'blank',
        'PROMOTE_GOOD',
        'GIVE_UP',
        'HOPE_FUTURE',
        'GRATEFUL',
        'SHOW_LOVE',
        'FORGIVE',
        'DONATED',
        'HELP_STRANGER',
        'VOLUNTEERED',

        # Physical Health & Health Behavior
        'blank',
        'PHYSICAL_HLTH',
        'HEALTH_PROB',
        'BODILY_PAIN',
        'CIGARETTES',
        'DRINKS',
        'DAYS_EXERCISE',

        # Socioeconomic Outcomes
        'blank',
        'EXPENSES',
        'WORRY_SAFETY',
        'EDUCATION_3',
        'EMPLOYMENT',
        'INCOME_FEELINGS',
        'OWN_RENT_HOME',
        'INCOME_QUINTILE',

        # Family factors
        'blank',
        'MARITAL_STATUS_EVER_MARRIED',
        'NUM_CHILDREN',

        # Personality
        'blank',
        "COMPOSITE_EXTRAVERSION",
        "COMPOSITE_OPENNESS",
        "COMPOSITE_AGREEABLENESS",
        "COMPOSITE_CONSCIENTIOUSNESS",
        "COMPOSITE_NEUROTICISM",

        # Religious & spiritual
        'blank',
        'CONNECTED_REL',
        'ATTEND_SVCS',
        'AFTER_DEATH',
        'REL_EXPERIENC',
        'SACRED_TEXTS',
        'PRAY_MEDITATE',
        'BELIEVE_GOD',
        'LIFE_APPROACH',
        'COMFORT_REL',
        'LOVED_BY_GOD',
        'GOD_PUNISH',
        'CRITICAL',
        'TELL_BELIEFS'

      )
      OUTCOME.VEC <- c(paste0(OUTCOME.VEC, "_W2"))
      # OUTCOME.VEC <- paste0(OUTCOME.VEC, "_W2")
    } else {
      OUTCOME.VEC = outcome.vec
    }
  }
  ## DEFINE VECTOR OF labels to print
  {
    if(is.null(mylabels)){
      #when outcome.vec contains 'blank' == 0, use label
      MYLABEL <- c(
        "Human Flourishing",
        "Psychological Well-Being",
        "Social Well-Being",
        "Psychological Distress",
        "Social Distress",
        "Character & Prosocial Behavior",
        "Physical Health & Health Behavior",
        "Socioeconomic Outcomes",
        "Family Factors",
        "Personality Traits",
        "Religion/Spirituality"
      )
    } else {
      MYLABEL = mylabels
    }
  }
  ## ============================================================================================ ##
  ## ====== Construct main text data summary table ============================================== ##
  {

    df.raw <- gfs_add_variable_labels(df.raw, OUTCOME.VEC)

    {
      # {
      #   temp.dat <- df.raw %>%
      #   select(
      #     COUNTRY,
      #     PSU_W1,
      #     STRATA_W1,
      #     ANNUAL_WEIGHT1_W1,
      #     {FOCAL_PREDICTOR},
      #     AGE_W1,
      #     any_of(c(OUTCOME.VEC)),
      #     any_of(c(baseline.pred))
      #   ) %>%
      #   svydesign(
      #     data = .,
      #     id =  ~ PSU_W1,
      #     strata =  ~ STRATA_W1,
      #     weights = ~ ANNUAL_WEIGHT1_W1,
      #     calibrate.formula = ~1
      #   )
      #
      # sumtab <- temp.dat %>%
      #   tbl_svysummary(
      #     include = c(
      #       {FOCAL_PREDICTOR},
      #       AGE_W1,
      #       all_of(baseline.pred),
      #       COUNTRY
      #     ),
      #     type = list(
      #       AGE_W1 ~ "continuous2",
      #       all_continuous() ~ "continuous2"
      #     ),
      #     statistic = list(
      #       all_continuous() ~ c("{mean}", "{sd}"),
      #       all_categorical() ~ "{n} ({p}%)"
      #     ),
      #     label = list(
      #       AGE_W1 ~ "Age of participant (Wave 1)",
      #       AGE_GRP_W1 ~ "Year of birth",
      #       GENDER_W1 ~ "Gender",
      #       PARENTS_12YRS_W1 ~ "Parental marital status around age 12",
      #       SVCS_12YRS_W1 ~ "Frequency of religious service attendance around age 12",
      #       MOTHER_RELATN_W1 ~ "Relationship with mother when growing up",
      #       FATHER_RELATN_W1 ~ "Relationship with father when growing up",
      #       OUTSIDER_W1 ~ "Felt like an outsider in family when growing up",
      #       ABUSED_W1 ~ "Experienced abuse when growing up",
      #       HEALTH_GROWUP_W1 ~ "Self-rated health when growing up",
      #       BORN_COUNTRY_W1 ~ "Immigration status",
      #       INCOME_12YRS_W1 ~ "Subjective financial status of family growing up",
      #       COUNTRY ~ "Country"
      #     ),
      #     digits = list(
      #       all_continuous() ~ 1,
      #       all_categorical() ~ 1
      #     ),
      #     missing_text = "    (Missing)",
      #     missing_stat = "{N_miss} ({p_miss}%)"
      #   ) %>%
      #   italicize_labels()
      # }
    }
    df.w1 <- df.raw %>%
      select(ID, COUNTRY, PSU_W1, STRATA_W1, ANNUAL_WEIGHT1_W1, contains("_W1")) %>%
      select(-COUNTRY_W1)
    colnames(df.w1) <- str_remove(colnames(df.w1), "_W1")
    df.w1$WAVE0 <- "Wave 1"
    df.w2 <- df.raw %>%
      select(ID, COUNTRY, PSU_W1, STRATA_W1, ANNUAL_WEIGHT1_W1, contains("_W2")) %>%
      select(-c(COUNTRY_W2, PSU_W2, STRATA_W2, ANNUAL_WEIGHT1_W2))
    colnames(df.w2) <- str_remove(colnames(df.w2), "_W1")
    colnames(df.w2) <- str_remove(colnames(df.w2), "_W2")
    df.w2$WAVE0 <- "Wave 2"
    df.raw.long <- full_join(df.w1, df.w2)

    focal.predictor0 <- str_remove(focal.predictor,"_W1")
    OUTCOME.VEC0 <- str_remove(OUTCOME.VEC,"_W2")
    baseline.pred0 <- str_remove(baseline.pred,"_W1")

    df.raw.long <- df.raw.long %>%
      select(
        COUNTRY,
        PSU,
        STRATA,
        ANNUAL_WEIGHT1,
        WAVE0,
        {focal.predictor0},
        AGE,
        any_of(c(OUTCOME.VEC0)),
        any_of(c(baseline.pred0))
      ) %>%
      # TO-DO, figure out a way to remove the leading values (doesn't work for)
      mutate(
        across(any_of(c({focal.predictor0}, OUTCOME.VEC0, baseline.pred0)), \(x){
          if ( is.factor(x) & str_detect(cur_column(), "AGE_GRP", negate = TRUE) ) {
            lvls <- levels(x)
            relvls <- lvls
            for (i in 1:length(lvls)) {
              if ( str_detect(lvls[i],"\\. ") ) {
                relvls[i] = stringr::str_split_fixed(lvls[i], "\\. ", 2)[,2]
              }
            }
            x = factor(x, levels = lvls, labels = relvls)
          }
          x
        })
      )

    df.raw.long <- gfs_add_variable_labels(df.raw.long, OUTCOME.VEC)

    temp.dat <- df.raw.long %>%
      svydesign(
        data = .,
        id =  ~ PSU,
        strata =  ~ STRATA,
        weights = ~ ANNUAL_WEIGHT1
      )

    sumtab <- temp.dat %>%
      tbl_svysummary(
        by = WAVE0,
        include = c(
          {focal.predictor0},
          AGE,
          all_of(baseline.pred0),
          COUNTRY
        ),
        type = list(
          AGE ~ "continuous2",
          all_continuous() ~ "continuous2"
        ),
        statistic = list(
          all_continuous() ~ c("{mean}", "{sd}", "{min}, {max}"),
          all_categorical() ~ "{n} ({p}%)"
        ),
        label = list(
          {focal.predictor0} ~ focal.better.name,
          AGE ~ "Age of participant",
          AGE_GRP ~ "Year of birth",
          GENDER ~ "Gender",
          MARITAL_STATUS ~ "Respondent marital status",
          EMPLOYMENT ~ "Employment status",
          ATTEND_SVCS ~ "Religious service attendance as an adult (now)",
          EDUCATION_3 ~ "Education (years)",
          BORN_COUNTRY ~ "Immigration status",
          PARENTS_12YRS ~ "Parental marital status around age 12",
          MOTHER_RELATN ~ "Relationship with mother when growing up",
          FATHER_RELATN ~ "Relationship with father when growing up",
          OUTSIDER ~ "Felt like an outsider in family when growing up",
          ABUSED ~ "Experienced abuse when growing up",
          HEALTH_GROWUP ~ "Self-rated health when growing up",
          INCOME_12YRS ~ "Subjective financial status of family growing up",
          SVCS_12YRS ~ "Frequency of religious service attendance around age 12",
          COUNTRY ~ "Country"
        ),
        digits = list(
          all_continuous() ~ 1,
          all_categorical() ~ 0
        ),
        missing_text = "(Missing)",
        missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      italicize_labels()

    tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

    sumtab.toprint <- sumtab %>%
      as_flex_table() %>%
      autofit() %>%
      #format_flex_table(pg.width = 21 / 2.54 - 2) %>%
      set_caption(
        paste0("Table 1. Summary statistics of the observed data (weighted).")
      ) %>%
      add_footer_row(
        values = tb.note.summarytab, top = FALSE,colwidths=3
      )
  }
  ## ============================================================================================ ##
  ## ====== Construct meta-analyzed results output table ======================================== ##
  {
    vec.id <- c("theta.rma", "theta.rma.ci","prob.lg.c","tau","global.pvalue")
    vec.rr <- c("rr.theta", "rr.theta.ci","prob.rr.c","rr.tau","global.pvalue")
    vec.wopc <- c("Est","95% CI","Pr(|yi|>thrsh)","Heterogeneity (tau)", "Global p-value")
    vec.wpc <- c("Est\r","95% CI\r","Pr(|yi|>thrsh)\r","Heterogeneity (tau)\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
    cnames <- c(
      "Outcome",
      "Reference",
      vec.wopc,
      "\r",
      vec.wpc
    )

    meta.outcomewide <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
    colnames(meta.outcomewide) <- cnames
    meta.outcomewide$"\r" <- ""
    i = ii = 1
    for (i in 1:length(OUTCOME.VEC)) {
      if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
        meta.outcomewide[i, 1] <- MYLABEL[ii]
        ii <- ii + 1
      } else {
        meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE))
        meta.outcomewide[i, 2] = case_when(
          get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ "0.00",
          .default = "1.00"
        )
        tmp.vec <- case_when(
          get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
        ## ====== Random effects meta - estimates withOUT PCs ======================================= ##
        tmp.wopc <- meta.wopc %>% ungroup() %>%
          dplyr::filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
            dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
              case_when(
                x < 0.01 ~ "< 0.01ǂ",
                x >= 0.01 ~ .round(x,2)
              )
            }),
            dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
              case_when(
                x > 0.01 ~ .round(x,4),
                x < 0.01 ~ paste0(.round(x,4),"*"),
                x < 0.001 ~ paste0(.round(x,4),"**"),
                x < 0.0001 ~ "<.0001***"
              )
            })
          )
        ## ====== Random effects meta - estimates WITH PCs ======================================= ##
        tmp.wpc <- meta.wpc %>% ungroup() %>%
          dplyr::filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
            dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
              case_when(
                x < 0.01 ~ "< 0.01ǂ",
                x >= 0.01 ~ .round(x,2)
              )
            }),
            dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
              case_when(
                x < 0.0001 ~ "<.0001***",
                x < 0.001 ~ paste0(.round(x,4),"**"),
                x < 0.01 ~ paste0(.round(x,4),"*"),
                x > 0.01 ~ .round(x,4)
              )
            })
          )
        ## ====== Add Results to output object ====================================================== ##
        if(nrow(tmp.wopc) > 0) meta.outcomewide[i,vec.wopc] <- tmp.wopc[tmp.vec]
        if(nrow(tmp.wpc) > 0) meta.outcomewide[i,vec.wpc] <- tmp.wpc[tmp.vec]
      }
    }
    #meta.outcomewide <- na.omit(meta.outcomewide)


    # footnote information:
    tb.note.meta.outcomewide <-as_paragraph(paste0("_Notes_. Reference for focal predictor: ", focal.predictor.reference.value,". Est., pooled standardized effect estimate; CI, confidence interval; Pr(|yi|>thrsh), the estimated proportion of effects [below / above] a threshold based on the calibrated effect sizes (Mathur & VanderWeele, 2020); Heterogeneity (tau), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and family factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a B, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.01*, p < 0.001**, p < 0.0001***, (Bonferroni) p < ",.round(p.bonferroni,4),". ǂEstimate of heterogeneity (tau) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects."))

    meta.outcomewide.toprint <- meta.outcomewide %>%
      flextable() %>%
      set_caption(
        paste0("Table 2. Meta-analyzed associations of _", focal.better.name ,"_ with adult well-being and other outcomes at wave 2.")
      ) %>%
      # uncomment when using all outcomes
      italic(part = "body",
             i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
             j = 1) %>%
      add_header_row(
        values = c("", "", "Model 1: Without PC", "", "Model 2: With PC"),
        colwidths = c(1,1,length(vec.wopc), 1, length(vec.wpc))
      ) %>%
      add_footer_row(
        values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
      ) %>%
      width(j=1,width=1.75)%>%
      width(j=8,width=0.20)%>%
      format_flex_table(pg.width = 29.7/2.54 - 2) %>%
      align(i = 1, j = NULL, align = "center", part = "header") %>%
      align(part = "footer", align = "left", j = 1:ncol(meta.outcomewide)) %>%
      border_remove()  %>%
      hline_bottom(part = "body") %>%
      hline_top(part = "header") %>%
      hline_bottom(part = "header") %>%
      hline(i=1,j=c(3:7,9:13), part="header")
  }
  ## ============================================================================================ ##
  ## ====== Construct meta-analytic E-values output table ======================================= ##
  {
    vec.id <- c("theta.rma.EE", "theta.rma.ECI")
    vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
    vec.wopc <- c("EE", "ECI")
    vec.wpc <- c("EE\r", "ECI\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
    cnames <- c(
      "Outcome",
      vec.wopc, "\r",
      vec.wpc
    )

    meta.evalues <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
    colnames(meta.evalues) <- cnames
    meta.evalues$"\r" <- ""
    i = ii = 1
    for (i in 1:length(OUTCOME.VEC)) {
      if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
        meta.evalues[i, 1] <- MYLABEL[ii]
        ii <- ii + 1
      } else {
        meta.evalues[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE))
        tmp.vec <- case_when(
          get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
        ## ====== Random effects meta - estimates withOUT PCs ======================================= ##
        tmp.wopc <- meta.wopc %>% ungroup() %>%
          dplyr::filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,2)),
          )
        ## ====== Random effects meta - estimates WITH PCs ======================================= ##
        tmp.wpc <- meta.wpc %>% ungroup() %>%
          dplyr::filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,2)),
          )
        ## ====== Add Results to output object ====================================================== ##
        if(nrow(tmp.wopc) > 0) meta.evalues[i,vec.wopc] <- tmp.wopc[tmp.vec]
        if(nrow(tmp.wpc) > 0) meta.evalues[i,vec.wpc] <- tmp.wpc[tmp.vec]
      }
    }
    #meta.evalues <- na.omit(meta.evalues)


    # footnote information:
    tb.note.evalues <-as_paragraph("_Notes_. EE, E-value for Estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for Estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

    meta.evalues.toprint <- meta.evalues %>%
      flextable() %>%
      #autofit() %>%
      set_caption(
        paste0("Table 3. Sensitivity analysis of meta-analyzed outcome-wide results to unmeasured confounding using E-values.")
      ) %>%
      # uncomment when using all outcomes
      italic(part = "body",
             i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
             j = 1) %>%
      add_header_row(
        values = c("", "Model 1: Without PC", "", "Model 2: With PC"),
        colwidths = c(1, length(vec.wopc), 1, length(vec.wpc))
      ) %>%
      add_footer_row(
        values = tb.note.evalues, top = FALSE, colwidths = ncol(meta.evalues)
      ) %>%
      width(j=1,width=2.5)%>%
      format_flex_table(pg.width = 21 / 2.54 - 2) %>%
      align(i = 1, j = NULL, align = "center", part = "header") %>%
      align(part = "footer", align = "left", j = 1:ncol(meta.evalues)) %>%
      border_remove()  %>%
      hline_bottom(part = "body") %>%
      hline_top(part = "header") %>%
      hline_bottom(part = "header") %>%
      hline(i=1,j=c(2:3,5:6), part="header")
  }
  ## ============================================================================================ ##
  ## ====== Forest plot for Secure Flourishing Index ============================================ ##
  {
    tb.cap.fig1 <- paste0("Figure 1. Heterogeneity in the effects of ", focal.better.name ," on composite Secure Flourishing Index scores across countries.")
    p1 <- meta.wpc %>% ungroup() %>%
      filter(OUTCOME0 == "COMPOSITE_FLOURISHING_SECURE_W2") %>%
      select(forest.plot)
    p1 <- p1[[1]]
    ggsave(
      filename = here::here(res.dir, paste0("figure_1_secure_flourishing_index.png")),
      plot=p1[[1]], units="in", width=6, height=5
    )

    tb.cap.fig2 <- paste0("Figure 2. Heterogeneity in the effects of ", focal.better.name ," on composite Flourishing Index (excludes financial indicators) scores across countries.")
    p2 <- meta.wpc %>% ungroup() %>%
      filter(OUTCOME0 == "COMPOSITE_FLOURISHING_W2") %>%
      select(forest.plot)
    p2 <- p2[[1]]
    ggsave(
      filename = here::here(res.dir, paste0("figure_2_flourishing_index.png")),
      plot=p2[[1]], units="in", width=6, height=5
    )
  }
  ## ============================================================================================ ##
  ## ====== Print out tables to formatted Word document ========================================= ##
  main_doc <- read_docx() |>
    body_add_flextable(value = sumtab.toprint) |>
    body_end_block_section(value = normal_portrait) |>
    body_add_break() |>
    body_add_flextable(value = meta.outcomewide.toprint) |>
    body_end_block_section(value = landscape_one_column) |>
    body_add_break() |>
    body_add_flextable(value = meta.evalues.toprint) |>
    body_add_break() |>
    body_add_par(tb.cap.fig1) |>
    body_add_img(
      src = here::here(res.dir, paste0("figure_1_secure_flourishing_index.png")),
      height = 5, width = 6
    )  |>
    body_add_break() |>
    body_add_par(tb.cap.fig2) |>
    body_add_img(
      src = here::here(res.dir, paste0("figure_2_flourishing_index.png")),
      height = 5, width = 6
    ) |>
    body_add_break()

  print(
    main_doc,
    target = here::here(res.dir,paste0("GFS Main Text Tables_", focal.better.name,".docx"))
  )
}


#' Construct GFS Online Supplement results
#'
#' Generated two word documents containing the supplemental results for the outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param coun.wopc a data.frame of country-specific results (e.g., input of meta-analytic) function (see gfs_meta_analysis(.)) of the results that did NOT include the principal components in the country-specific regression analyses
#' @param coun.wpc a data.frame of country-specific results (e.g., input of meta-analytic) function (see gfs_meta_analysis(.)) of the results that did INCLUDED the principal components in the country-specific regression analyses
#' @param meta.wopc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did NOT include the principal components in the country-specific regression analyses
#' @param meta.wpc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did INCLUDED the principal components in the country-specific regression analyses
#' @param focal.predictor a character defining the focal predictor
#' @param focal.better.name a character that is used as the printed name in tables/captions to denote the focal predictor
#' @param p.bonferroni a number (e.g., 0.007), is internally determined based on number of rows in coun.wopc
#' @param baseline.pred a vector of characters defining which baseline characteristics were used as control variables. This can be used to force the inclusion some variable into the main text summary table.
#' @param outcome.vec a character vector of outcomes names (e.g., "HAPPY_W2") that are to be printed in the main text meta-analytic summary table. Name MUST be included in the coun.wopc (coun.wpc) nested data.frames column (OUTCOME0), otherwise the variable won't be printed.
#' @param mylabels an optional character vector that will be printed out in specific rows of tables 2/3 depending on the specification pf outcome.vec
#' @param included.countries a character vector of which countries to include in output -- defaults to all.
#' @param res.dir (character) defaults to "results", and will be created if needed to story results document
#' @param focal.predictor.reference.value (character) describing the baseline/reference group for the focal predictor.
#' @param what (character) options include: "both", "S1", "S2"
#' @param ... other arguments as needed
#' @returns a word document saved to the current 'results/' directory
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' Generates two online supplements.
#'
#' Supplement 1:
#'
#' (1) Summary statistics of OUTCOMES by wave (raw data)
#' (2) Country-specific results
#'    - Summary statistics by wave for demographics (similar to main text Table 1)
#'    - Summary statistics by wave for outcomes (similar to Table S1)
#'    - Summary statistics of principal components by outcome (# retained, % prop explained, cumsum % prop explained)
#'    - Outcome-wide results (similar to main text Table 2)
#'    - Outcome-wide E-values (similar to main text Table 3)
#'
#' Supplement 2:
#'
#' (1) Summary statistics of demographics by country (raw data)
#' (2) Summary statistics of OUTCOMES by country (raw data)
#' (3) Forest plots of all effects
#'     - Model 1 (No PCs)
#'     - Model 2 (w/ PCs)
#'
#'
gfs_generate_supplemental_docs <- function(df.raw = NULL, meta.wopc=NULL, meta.wpc=NULL, coun.wopc=NULL, coun.wpc=NULL, coun.fit.pca=NULL,focal.predictor = NULL, focal.better.name="Focal Predictor", focal.predictor.reference.value="estimated population mean of focal predictor", p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL, res.dir = "results", included.countries=NULL, what = "both"){

  focal.predictor0 <- str_remove(focal.predictor,"_W1")

  if(is.null(p.bonferroni)){
    p.bonferroni = 0.05/nrow(coun.wopc)
  }
  if(is.null(baseline.pred)){
    baseline.pred = str_remove(
      c(
        "COV_AGE_GRP_W1",
        "COV_GENDER_W1",
        "COV_EDUCATION_3_W1",
        "COV_EMPLOYMENT_W1",
        "COV_MARITAL_STATUS_W1",
        "COV_ATTEND_SVCS_W1",
        "COV_BORN_COUNTRY_W1",
        "COV_PARENTS_12YRS_W1",
        "COV_SVCS_12YRS_W1",
        "COV_MOTHER_RELATN_W1",
        "COV_FATHER_RELATN_W1",
        "COV_OUTSIDER_W1",
        "COV_ABUSED_W1",
        "COV_HEALTH_GROWUP_W1",
        "COV_INCOME_12YRS_W1",
        "COV_REL1_W1",
        "COV_RACE_PLURALITY_W1"
      ), "COV_")
  }
  baseline.pred0 <- str_remove(baseline.pred, "_W1")
  if (!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  if(!dir.exists(here::here(res.dir, "fig"))){
    dir.create(here::here(res.dir, "fig"))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    normal_portrait <- block_section(
      prop_section(page_size = page_size(orient = "portrait"), type = "continuous")
    )
    extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 29.7 / 2.54 * 2,
        height = 29.7 / 2.54
      ),
      type = "continuous"
    ))

    landscape_three_columns <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous",
        section_columns = section_columns(widths = c(3.24,3.24,3.24))
      )
    )
    landscape_two_columns <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous",
        section_columns = section_columns(widths = c(4.8,4.8))
      )
    )
    landscape_one_column <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous"
      )
    )
    # body_end_section_landscape(x, w = 21/2.54, h = 29.7/2.54)
  }
  ## DEFINE VECTOR OF OUTCOMES
  {
    if(is.null(outcome.vec)){
      OUTCOME.VEC0 <- c(
        # Flourishing
        'blank',
        "COMPOSITE_FLOURISHING_SECURE",
        "COMPOSITE_FLOURISHING",
        "COMPOSITE_HAPPI_LIFE_SAT",
        "COMPOSITE_HEALTH",
        "COMPOSITE_MEANING_PURPOSE",
        "COMPOSITE_CHARACTER",
        "COMPOSITE_SUBJECTIVE_SOC_CONN",
        "COMPOSITE_FINL_MAT_WORRY",

        # Psychological well-being
        'blank',
        'HAPPY',
        'LIFE_SAT',
        'WB_TODAY',
        'WB_FIVEYRS',
        'EXPECT_GOOD',
        'FREEDOM',
        'PEACE',
        'LIFE_BALANCE',
        'CAPABLE',
        'WORTHWHILE',
        'LIFE_PURPOSE',
        'MENTAL_HEALTH',

        # Social Well-Being
        'blank',
        'CONTENT',
        'SAT_RELATNSHP',
        'PEOPLE_HELP',
        'CLOSE_TO',
        'APPROVE_GOVT',
        'SAY_IN_GOVT',
        'BELONGING',
        'SAT_LIVE',
        'TRUST_PEOPLE',
        'GROUP_NOT_REL',

        # Psychological Distress
        'blank',
        'THREAT_LIFE',
        'COMPOSITE_DEPRESSION', 'DEPRESSED', 'INTEREST',
        'COMPOSITE_ANXIETY', 'FEEL_ANXIOUS', 'CONTROL_WORRY',
        'SUFFERING',

        # Social Distress
        'blank',
        'LONELY',
        'DISCRIMINATED',

        # Character & Prosocial Behavior
        'blank',
        'PROMOTE_GOOD',
        'GIVE_UP',
        'HOPE_FUTURE',
        'GRATEFUL',
        'SHOW_LOVE',
        'FORGIVE',
        'DONATED',
        'HELP_STRANGER',
        'VOLUNTEERED',

        # Physical Health & Health Behavior
        'blank',
        'PHYSICAL_HLTH',
        'HEALTH_PROB',
        'BODILY_PAIN',
        'CIGARETTES',
        'DRINKS',
        'DAYS_EXERCISE',

        # Socioeconomic Outcomes
        'blank',
        'EXPENSES',
        'WORRY_SAFETY',
        'EDUCATION_3',
        'EMPLOYMENT',
        'INCOME_FEELINGS',
        'OWN_RENT_HOME',
        'INCOME_QUINTILE',

        # Family factors
        'blank',
        'MARITAL_STATUS_EVER_MARRIED',
        'NUM_CHILDREN',

        # Personality
        'blank',
        "COMPOSITE_EXTRAVERSION",
        "COMPOSITE_OPENNESS",
        "COMPOSITE_AGREEABLENESS",
        "COMPOSITE_CONSCIENTIOUSNESS",
        "COMPOSITE_NEUROTICISM",

        # Religious & spiritual
        'blank',
        'CONNECTED_REL',
        'ATTEND_SVCS',
        'AFTER_DEATH',
        'REL_EXPERIENC',
        'SACRED_TEXTS',
        'PRAY_MEDITATE',
        'BELIEVE_GOD',
        'LIFE_APPROACH',
        'COMFORT_REL',
        'LOVED_BY_GOD',
        'GOD_PUNISH',
        'CRITICAL',
        'TELL_BELIEFS'

      )
      OUTCOME.VEC <- c(paste0(OUTCOME.VEC0, "_W2"))
      # OUTCOME.VEC <- paste0(OUTCOME.VEC, "_W2")
    } else {
      OUTCOME.VEC = outcome.vec
    }
  }
  ## DEFINE VECTOR OF labels to print
  {
    if(is.null(mylabels)){
      #when outcome.vec contains 'blank' == 0, use label
      MYLABEL <- c(
        "Human Flourishing",
        "Psychological Well-Being",
        "Social Well-Being",
        "Psychological Distress",
        "Social Distress",
        "Character & Prosocial Behavior",
        "Physical Health & Health Behavior",
        "Socioeconomic Outcomes",
        "Family Factors",
        "Personality Traits",
        "Religion/Spirituality"
      )
    } else {
      MYLABEL = mylabels
    }
  }
  ## ============================================================================================ ##
  ## Restructing raw data
  {
    tmp.vec <- c(paste0(OUTCOME.VEC0, "_W2"), paste0(OUTCOME.VEC0, "_W1"))
    df.raw <- gfs_add_variable_labels(
      df = df.raw,
      vars = tmp.vec,
      include.wave = TRUE
    )

    df.w1 <- df.raw %>%
      select(ID, COUNTRY, PSU_W1, STRATA_W1, ANNUAL_WEIGHT1_W1, RACE, contains("_W1")) %>%
      select(-COUNTRY_W1)
    colnames(df.w1) <- str_remove(colnames(df.w1), "_W1")
    df.w1$WAVE0 <- "Wave 1"
    df.w2 <- df.raw %>%
      select(ID, COUNTRY, PSU_W1, STRATA_W1, ANNUAL_WEIGHT1_W1, RACE, contains("_W2")) %>%
      select(-c(COUNTRY_W2, PSU_W2, STRATA_W2, ANNUAL_WEIGHT1_W2))
    colnames(df.w2) <- str_remove(colnames(df.w2), "_W1")
    colnames(df.w2) <- str_remove(colnames(df.w2), "_W2")
    df.w2$WAVE0 <- "Wave 2"
    df.raw.long <- full_join(df.w1, df.w2)

    # will be used in country-specific loop
    df.raw.long  <- df.raw.long  %>%
      select(
        COUNTRY,
        PSU,
        STRATA,
        ANNUAL_WEIGHT1,
        WAVE0,
        {focal.predictor0},
        AGE,
        any_of(c(OUTCOME.VEC0)),
        any_of(c(baseline.pred0)),
        RACE
      ) %>%
      mutate(
        across(any_of(c({focal.predictor0}, OUTCOME.VEC0, baseline.pred0)), \(x){
          if ( is.factor(x) & str_detect(cur_column(), "AGE_GRP", negate = TRUE) ) {
            lvls <- levels(x)
            relvls <- lvls
            for (i in 1:length(lvls)) {
              if ( str_detect(lvls[i],"\\. ") ) {
                relvls[i] = paste0("    ",stringr::str_split_fixed(lvls[i], "\\. ", 2)[,2])
              }
            }
            x = factor(x, levels = lvls, labels = relvls)
          }
          x
        })
      )
    tmp.vec <- c(baseline.pred0, OUTCOME.VEC0)
    df.raw.long <- gfs_add_variable_labels( df=df.raw.long, vars=tmp.vec )

  }
  ## ============================================================================================== ##
  # Supplement 1:
  #	(1) Summary statistics of OUTCOMES by wave (raw data)
  #	(2) Country-specific results
  #     - Summary statistics by wave for demographics (similar to main text Table 1)
  #     - Summary statistics by wave for outcomes (similar to Table S1)
  #     - Summary statistics of principal components by outcome (# retained, % prop explained, cumsum % prop explained)
  #     - Outcome-wide results (similar to main text Table 2)
  #     - Outcome-wide E-values (similar to main text Table 3)
  # ========================= #
  if(what == "both" | what == "S1"){
    # initialize output object
    supp_doc <- read_docx() |>
      body_add_par("GFS Online Supplement Part 1", style="centered") %>%
      #body_add_par("...general caveats...")
      body_end_section_continuous() %>%
      body_add_break()
    # ========================= #
    #coun.wopc = LIST.RES1
    #coun.wpc = LIST.RES2
    #focal.better.name = FOCAL_PREDICTOR_BETTER_NAME

    ## ============================================================================================ ##
    ## ====== Construct outcome data summary table =============================================== ##
    {

      temp.dat <- df.raw.long %>%
        svydesign(
          data = .,
          id =  ~ PSU,
          strata =  ~ STRATA,
          weights = ~ ANNUAL_WEIGHT1
        )

      sumtab <- temp.dat %>%
        tbl_svysummary(
          by = WAVE0,
          include = c(
            {focal.predictor0},
            any_of(OUTCOME.VEC0)
          ),
          type = list(
            all_continuous() ~ "continuous2"
          ),
          statistic = list(
            all_continuous() ~ c("{mean}", "{sd}", "{min}, {max}"),
            all_categorical() ~ "{n} ({p}%)"
          ),
          label = list(
            {focal.predictor0} ~ focal.better.name
          ),
          digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ 0
          ),
          missing_text = "(Missing)",
          missing_stat = "{N_miss} ({p_miss}%)"
        ) %>%
        italicize_labels()

      tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

      sumtab.toprint <- sumtab %>%
        as_flex_table() %>%
        autofit() %>%
        format_flex_table(pg.width = 21 / 2.54 - 2) %>%
        set_caption(
          paste0("Table S1. Summary statistics of the observed data (weighted).")
        ) %>%
        add_footer_row(
          values = tb.note.summarytab, top = FALSE,colwidths=3
        )

      supp_doc <- supp_doc |>
        body_add_flextable(value = sumtab.toprint) |>
        body_end_block_section(value = normal_portrait) |>
        body_add_break()

    }
    print(
      supp_doc,
      target = here::here(res.dir,paste0("GFS Online Supplement Part 1_", focal.better.name,".docx"))
    )
    ## ============================================================================================ ##
    ## ====== COUNTRY SPECIFIC RESULTS ============================================================ ##
    {
      if(is.null(included.countries)){
        COUNTRY_LABELS <-
          sort(
            c(
              "Australia",
              "Hong Kong",
              "India",
              "Indonesia",
              "Japan",
              "Philippines",
              "Egypt",
              "Germany",
              "Israel",
              "Kenya",
              "Nigeria",
              "Poland",
              "South Africa",
              "Spain",
              "Sweden",
              "Tanzania",
              "Turkey",
              "United Kingdom",
              "United States",
              "Argentina",
              "Brazil",
              "Mexico"
            )
          )
      } else {
        COUNTRY_LABELS = included.countries
      }
    }

    i = 1; tb.num.shift = 1
    for (i in 1:length(COUNTRY_LABELS)) {
      supp_doc <- read_docx(here::here(res.dir,paste0("GFS Online Supplement Part 1_", focal.better.name,".docx")))

      cat("\nCountry:\t", COUNTRY_LABELS[i])
      ## ============================================================================================ ##
      ## ====== Table Sia. summary statistics -- demographics variables ============================= ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.long %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
            mutate(
              RACE = factor(RACE ),
              RACE = droplevels(RACE),
              RACE = case_when(is.na(RACE) ~ "    (Missing)", .default = RACE),
              RACE = factor(RACE, levels = sort(unique(RACE))),
              RACE = fct_relevel(RACE, "    (Missing)", after = Inf),
              #INCOME = droplevels(INCOME),
              #INCOME = factor(INCOME, levels = sort(unique(INCOME))),
              #INCOME = case_when(is.na(INCOME) ~ "    (Missing)", .default = INCOME),
              #INCOME = fct_relevel(INCOME, "    (Missing)", after = Inf),
            ) %>%
            svydesign(
              data = .,
              id =  ~ PSU,
              strata =  ~ STRATA,
              weights = ~ ANNUAL_WEIGHT1
            )

          sumtab <- temp.dat %>%
            tbl_svysummary(
              by = WAVE0,
              include = c(
                {focal.predictor0},
                AGE,
                any_of(c(baseline.pred0[1:16], "RACE"))
              ),
              type = list(
                AGE ~ "continuous2",
                all_continuous() ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("{mean}", "{sd}", "{min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                {focal.predictor0} ~ focal.better.name
              ),
              #  AGE ~ "Age of participant",
              #  AGE_GRP ~ "Year of birth",
              #  GENDER ~ "Gender",
              #  MARITAL_STATUS ~ "Respondent marital status",
              #  EMPLOYMENT ~ "Employment status",
              #  ATTEND_SVCS ~ "Religious service attendance as an adult (now)",
              #  EDUCATION_3 ~ "Education (years)",
              #  BORN_COUNTRY ~ "Immigration status",
              #  PARENTS_12YRS ~ "Parental marital status around age 12",
              #  MOTHER_RELATN ~ "Relationship with mother when growing up",
              #  FATHER_RELATN ~ "Relationship with father when growing up",
              #  OUTSIDER ~ "Felt like an outsider in family when growing up",
              # ABUSED ~ "Experienced abuse when growing up",
              # HEALTH_GROWUP ~ "Self-rated health when growing up",
              # INCOME_12YRS ~ "Subjective financial status of family growing up",
              # SVCS_12YRS ~ "Frequency of religious service attendance around age 12",
              # RACE ~ "Self reported race/ethnic affiliation",
              # INCOME ~ "Self reported income",
              # COUNTRY ~ "Country"
              # ),
              digits = list(
                all_continuous() ~ 1,
                all_categorical() ~ 1
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

          tbia.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              paste0("Table S",i+tb.num.shift,"a. Summary statistics of the observed data (weighted) in ", COUNTRY_LABELS[i])
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )
        })
      })
      ## ============================================================================================ ##
      ## ====== Table Sib. summary statistics -- outcome variables ================================== ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.long %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
            svydesign(
              data = .,
              id =  ~ PSU,
              strata =  ~ STRATA,
              weights = ~ ANNUAL_WEIGHT1
            )

          sumtab <- temp.dat %>%
            tbl_svysummary(
              by = WAVE0,
              include = c(
                {focal.predictor0},
                any_of(OUTCOME.VEC0)
              ),
              type = list(
                all_continuous() ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("{mean}", "{sd}", "{min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                {focal.predictor0} ~ focal.better.name
              ),
              digits = list(
                all_continuous() ~ 1,
                all_categorical() ~ 1
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

          tbib.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              paste0("Table S",i+tb.num.shift,"b. Summary statistics of outcomes (weighted) in ", COUNTRY_LABELS[i])
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )
        })
      })
      ## ============================================================================================ ##
      ## ====== Table Sic. Country specific PCA Summary ============================================= ##
      {
        vec.id <- c("prop.var", "Cumulative_Proportion_Explained")
        vec.pc <- c("Percent Explained by each PC", "Cumulative Percent Explained")
        cnames <- c(
          "PC",
          vec.pc
        )

        coun.pca <- as.data.frame(matrix(nrow = 20, ncol = length(cnames)))
        colnames(coun.pca) <- cnames
        tmp.name <- paste0(OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)][1], "_", focal.predictor)
        tmp.pca <- coun.fit.pca[[tmp.name]] %>% ungroup() %>%
          dplyr::filter(  str_detect(COUNTRY, COUNTRY_LABELS[i]), PC <= 20 ) %>%
          dplyr::select(PC,tidyr::any_of(vec.id)) %>%
          dplyr::mutate(
            across(tidyr::any_of(vec.id),\(x) .round(x*100,1) )
          )
        coun.pca$PC <- 1:20
        coun.pca[vec.pc] <- tmp.pca[vec.id]

        #coun.pca <- na.omit(coun.pca)
        # footnote information:
        tb.note.pca <- as_paragraph("_Notes_. PCA was conducted using 'survey::svyprcomp(.)' function using all available contemporaneous (with focal predictor) exposures at wave 1. All PCs were standardized prior to being used as predictors. The number of retained components for analysis was 7.")

        coun.pca.toprint <- coun.pca %>%
          flextable() %>%
          #autofit() %>%
          set_caption(
            paste0("Table S", i+tb.num.shift,"c. Summary of principal components in ", COUNTRY_LABELS[i])
          ) %>%
          add_footer_row(
            values = tb.note.pca, top = FALSE, colwidths = ncol(coun.pca)
          ) %>%
          format_flex_table(pg.width = 21 / 2.54 - 4) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(coun.pca)) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header")
      }
      ## ============================================================================================ ##
      ## ====== Table Sid. Country specific outcome wide results ==================================== ##
      {
        vec.id <- c("id.Est", "id.SE", "id.CI","p.value")
        vec.rr <- c("rr.Est", "logrr.SE", "rr.CI","p.value")
        vec.wopc <- c("Est", "SE", "95% CI", "p-value")
        vec.wpc <- c("Est\r", "SE\r", "95% CI\r", "p-value\r")
        cnames <- c(
          "Outcome",
          "Reference",
          vec.wopc,
          "\r",
          vec.wpc
        )

        coun.outcomewide <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
        colnames(coun.outcomewide) <- cnames
        coun.outcomewide$"\r" <- ""
        j = ii = 1
        for (j in 1:length(OUTCOME.VEC)) {
          if (stringr::str_detect(OUTCOME.VEC[j], "blank") ) {
            coun.outcomewide[j, 1] <- MYLABEL[ii]
            ii <- ii + 1
          } else {
            coun.outcomewide[j, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[j], include.name = FALSE))
            coun.outcomewide[j, 2] = case_when(
              get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ "0.00",
              .default = "1.00"
            )
            tmp.vec <- case_when(
              get_outcome_scale(OUTCOME.VEC[j]) == "cont" ~ vec.id,
              .default = vec.rr
            )
            tmp.name <- paste0(OUTCOME.VEC[j], "_", focal.predictor)
            ## ====== estimates withOUT PCs ======================================= ##
            if(tmp.name %in% names(coun.wopc)){
              tmp.wopc <- coun.wopc[[tmp.name]] %>% ungroup() %>%
                dplyr::filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(tidyr::any_of(c("p.value")),\(x){
                    case_when(
                      x < 0.0001 ~ "<.0001***",
                      x < 0.001 ~ paste0(.round(x,4),"**"),
                      x < 0.01 ~ paste0(.round(x,4),"*"),
                      x > 0.01 ~ .round(x,4)
                    )
                  })
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wopc) > 0) coun.outcomewide[j,vec.wopc] <- tmp.wopc[tmp.vec]
            }
            ## ====== Random effects coun - estimates WITH PCs ======================================= ##
            if(tmp.name %in% names(coun.wpc)){
              tmp.wpc <- coun.wpc[[tmp.name]] %>% ungroup() %>%
                dplyr::filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(tidyr::any_of(c("p.value")),\(x){
                    case_when(
                      x < 0.0001 ~ "<.0001***",
                      x < 0.001 ~ paste0(.round(x,4),"**"),
                      x < 0.01 ~ paste0(.round(x,4),"*"),
                      x > 0.01 ~ .round(x,4)
                    )
                  })
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wpc) > 0) coun.outcomewide[j,vec.wpc] <- tmp.wpc[tmp.vec]
            }
          }
        }
        #coun.outcomewide <- na.omit(coun.outcomewide)

        # footnote information:
        tb.note.coun.outcomewide <-as_paragraph(paste0("_Notes_. Reference for focal predictor: ", focal.predictor.reference.value,". Est., standardized effect estimate; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and family factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a B, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.01*, p < 0.001**, p < 0.0001***, (Bonferroni) p < ",.round(p.bonferroni,4),"."))

        coun.outcomewide.toprint <- coun.outcomewide %>%
          flextable() %>%
          set_caption(
            paste0("Table S",i+tb.num.shift,"d. Associations of _", focal.better.name ,"_ with adult well-being and other outcomes at wave 2 in ", COUNTRY_LABELS[i])
          ) %>%
          #uncomment when using all outcomes
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "", "Model 1: Without PC", "", "Model 2: With PC"),
            colwidths = c(1,1,length(vec.wopc), 1, length(vec.wpc))
          ) %>%
          add_footer_row(
            values = tb.note.coun.outcomewide, top = FALSE, colwidths = ncol(coun.outcomewide)
          ) %>%
          width(j=1,width=1.75)%>%
          width(j=7,width=0.20)%>%
          format_flex_table(pg.width = 29.7/2.54 - 2) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(coun.outcomewide)) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header") %>%
          hline(i=1,j=c(3:6,8:11), part="header")
      }
      ## ============================================================================================ ##
      ## ====== Table Sie. Country Specific E-values output table =================================== ##
      {
        vec.id <- c("EE", "ECI")
        vec.wopc <- c("EE", "ECI")
        vec.wpc <- c("EE\r", "ECI\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        cnames <- c(
          "Outcome",
          vec.wopc, "\r",
          vec.wpc
        )

        coun.evalues <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
        colnames(coun.evalues) <- cnames
        coun.evalues$"\r" <- ""
        j = ii = 1
        for (j in 1:length(OUTCOME.VEC)) {
          if (stringr::str_detect(OUTCOME.VEC[j], "blank") ) {
            coun.evalues[j, 1] <- MYLABEL[ii]
            ii <- ii + 1
          } else {
            coun.evalues[j, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[j], include.name = FALSE))
            tmp.vec <- vec.id
            tmp.name <- paste0(OUTCOME.VEC[j], "_", focal.predictor)
            ## ====== estimates withOUT PCs ======================================= ##
            if(tmp.name %in% names(coun.wopc)){
              tmp.wopc <- coun.wopc[[tmp.name]] %>% ungroup() %>%
                dplyr::filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(where(is.numeric),\(x) .round(x,2)),
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wopc) > 0) coun.evalues[j,vec.wopc] <- tmp.wopc[tmp.vec]

            }
            ## ====== estimates WITH PCs ======================================= ##
            if(tmp.name %in% names(coun.wpc)){
              tmp.wpc <- coun.wpc[[tmp.name]] %>% ungroup() %>%
                dplyr::filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(where(is.numeric),\(x) .round(x,2)),
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wpc) > 0) coun.evalues[j,vec.wpc] <- tmp.wpc[tmp.vec]
            }
          }
        }
        #coun.evalues <- na.omit(coun.evalues)


        # footnote information:
        tb.note.evalues <-as_paragraph("_Notes_. EE, E-value for Estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for Estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

        coun.evalues.toprint <- coun.evalues %>%
          flextable() %>%
          #autofit() %>%
          set_caption(
            paste0("Table S", i+tb.num.shift,"e. Sensitivity analysis of outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[i])
          ) %>%
          #uncomment when using all outcomes
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Model 1: Without PC", "", "Model 2: With PC"),
            colwidths = c(1, length(vec.wopc), 1, length(vec.wpc))
          ) %>%
          add_footer_row(
            values = tb.note.evalues, top = FALSE, colwidths = ncol(coun.evalues)
          ) %>%
          width(j=1,width=2.5)%>%
          format_flex_table(pg.width = 21 / 2.54 - 2) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(coun.evalues)) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header") %>%
          hline(i=1,j=c(2:3,5:6), part="header")
      }
      ## ============================================================================================ ##
      ## ====== Print out tables to formatted Word document ========================================= ##
      supp_doc <- supp_doc |>
        body_add_flextable(value = tbia.toprint) |>
        body_add_break() |>
        body_add_flextable(value = tbib.toprint) |>
        body_add_break() |>
        body_add_flextable(value = coun.pca.toprint) |>
        body_end_block_section(value = normal_portrait) |>
        body_add_break() |>
        body_add_flextable(value = coun.outcomewide.toprint) |>
        body_end_block_section(value = landscape_one_column) |>
        body_add_break() |>
        body_add_flextable(value = coun.evalues.toprint) |>
        body_end_block_section(value = normal_portrait) |>
        body_add_break()

      print(
        supp_doc,
        target = here::here(res.dir,paste0("GFS Online Supplement Part 1_", focal.better.name,".docx"))
      )
    }

  }
  ## ============================================================================================== ##
  # Supplement 2:
  # (1) Summary statistics of demographics by country (raw data)
  #	(2) Summary statistics of OUTCOMES by country (raw data)
  #	(3) Forest plots of all effects
  #     - Model 1 (No PCs)
  #     - Model 2 (w/ PCs)
  ## ============================================================================================== ##
  if(what == "both" | what == "S2"){
    supp_doc <- read_docx() |>
      body_add_par("GFS Online Supplement Part 2", style="centered") %>%
      #body_add_par("...general caveats...")
      body_end_section_continuous() %>%
      body_add_break()

    ## ============================================================================================ ##
    ## ====== Table S1. summary statistics -- demographics variables by country =================== ##
    suppressMessages({
      suppressWarnings({
        temp.dat <- df.raw %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          #filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
          svydesign(
            data = .,
            id =  ~ PSU_W1,
            strata =  ~ STRATA_W1,
            weights = ~ ANNUAL_WEIGHT1_W1
          )

        focal.predictor1 <- focal.predictor
        focal.predictor2 <- paste0(str_remove(focal.predictor, "_W1"), "_W2")

        focal.better.name1 <- paste0(focal.better.name, " (wave 1)")
        focal.better.name2 <- paste0(focal.better.name, " (wave 2)")

        sumtab <- temp.dat %>%
          tbl_svysummary(
            by = COUNTRY,
            include = c(
              {focal.predictor1},{focal.predictor2},
              AGE_W1, AGE_W2, AGE_GRP_W1, AGE_GRP_W2,
              any_of(c(paste0(baseline.pred0[2:16],"_W1"), paste0(baseline.pred0[2:16],"_W2")))
            ),
            type = list(
              AGE_W1 ~ "continuous",
              AGE_W2 ~ "continuous",
              all_continuous() ~ "continuous"
            ),
            statistic = list(
              all_continuous() ~ c("{mean} ({sd})"),
              all_categorical() ~ "{n} ({p}%)"
            ),
            label = list(
              {focal.predictor1} ~ focal.better.name1,
              {focal.predictor2} ~ focal.better.name2,
              AGE_W1 ~ "Age of participant (wave 1)",
              AGE_W2 ~ "Age of participant (wave 2)"
              ,AGE_GRP_W1 ~ "Year of birth (wave 1)"
              ,AGE_GRP_W2 ~ "Year of birth (wave 2)"
              #GENDER_W1 ~ "Gender",
              #GENDER_W2 ~ "Gender (wave 2)",
              #MARITAL_STATUS_W1 ~ "Respondent marital status",
              #MARITAL_STATUS_W2 ~ "Respondent marital status (wave 2)",
              #EMPLOYMENT_W1 ~ "Employment status",
              #EMPLOYMENT_W2 ~ "Employment status (wave 2)",
              #ATTEND_SVCS_W1 ~ "Religious service attendance as an adult (now)",
              #ATTEND_SVCS_W2 ~ "Religious service attendance as an adult (now) (wave 2)",
              #EDUCATION_3_W1 ~ "Education (years)",
              #EDUCATION_3_W2 ~ "Education (years) (wave 2)",
              #BORN_COUNTRY_W1 ~ "Immigration status",
              #BORN_COUNTRY_W2 ~ "Immigration status (wave 2)",
              #PARENTS_12YRS_W1 ~ "Parental marital status around age 12",
              #MOTHER_RELATN_W1 ~ "Relationship with mother when growing up",
              #FATHER_RELATN_W1 ~ "Relationship with father when growing up",
              #OUTSIDER_W1 ~ "Felt like an outsider in family when growing up",
              #ABUSED_W1 ~ "Experienced abuse when growing up",
              #HEALTH_GROWUP_W1 ~ "Self-rated health when growing up",
              #INCOME_12YRS_W1 ~ "Subjective financial status of family growing up",
              #SVCS_12YRS_W1 ~ "Frequency of religious service attendance around age 12"
            ),
            digits = list(
              all_continuous() ~ 1,
              all_categorical() ~ 1
            ),
            missing_text = "    (Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
          ) %>%
          italicize_labels() %>%
          modify_spanning_header(all_stat_cols() ~ paste0("_Country of Respondent_")) %>%
          add_overall(last=TRUE)

        tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

        tbia.toprint <- sumtab %>%
          as_flex_table() %>%
          autofit() %>%
          format_flex_table(pg.width = 29.7 / 2.54 * 2 - 1) %>%
          set_caption(
            paste0("Table S1. Summary statistics of the observed demographic data (weighted) across countries.")
          ) %>%
          add_footer_row(
            values = tb.note.summarytab, top = FALSE,colwidths=24
          )
      })
    })
    ## ============================================================================================ ##
    ## ====== Table S2. summary statistics -- outcome variables by country ======================== ##
    suppressMessages({
      suppressWarnings({
        temp.dat <- df.raw %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          #filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
          svydesign(
            data = .,
            id =  ~ PSU_W1,
            strata =  ~ STRATA_W1,
            weights = ~ ANNUAL_WEIGHT1_W1
          )

        sumtab <- temp.dat %>%
          tbl_svysummary(
            by = COUNTRY,
            include = c(
              {focal.predictor1}, {focal.predictor2}
              #, any_of(sort(c(paste0(OUTCOME.VEC0,"_W1"), paste0(OUTCOME.VEC0,"_W2"))))
            ),
            type = list(
              all_continuous() ~ "continuous2"
            ),
            statistic = list(
              all_continuous() ~ c("{mean}", "{sd}", "{min}, {max}"),
              all_categorical() ~ "{n} ({p}%)"
            ),
            label = list(
              {focal.predictor1} ~ focal.better.name1,
              {focal.predictor2} ~ focal.better.name2
            ),
            digits = list(
              all_continuous() ~ 1,
              all_categorical() ~ 0
            ),
            missing_text = "(Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
          ) %>%
          italicize_labels() %>%
          modify_spanning_header(all_stat_cols() ~ paste0("_Country of Respondent_")) %>%
          add_overall(last=TRUE)

        tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

        tbib.toprint <- sumtab %>%
          as_flex_table() %>%
          autofit() %>%
          format_flex_table(pg.width = 29.7 / 2.54 * 2 - 1) %>%
          set_caption(
            paste0("Table S2. Summary statistics of the observed outcome data (weighted) across countries.")
          ) %>%
          add_footer_row(
            values = tb.note.summarytab, top = FALSE,colwidths=24
          )
      })
    })

    #extra_wide_landscape <- block_section(prop_section(
    #  page_size = page_size(
    #    orient = "landscape",
    #    width = 29.7 / 2.54 * 2,
    #    height = 29.7 / 2.54
    #  ),
    #  type = "continuous"
    #))

    supp_doc <- supp_doc |>
      body_add_flextable(tbia.toprint) |>
      body_add_break() |>
      body_add_flextable(tbib.toprint) |>
      body_end_block_section(value = extra_wide_landscape)|>
      body_add_break()

    print(
      supp_doc,
      target = here::here(res.dir, paste0("GFS Online Supplement Part 2_", focal.better.name,".docx"))
    )


    tmp.vec <- OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)]
    for(i in  1:length(tmp.vec)){
      if(tmp.vec[i] %in% unique(meta.wopc$OUTCOME0) & tmp.vec[i] %in% unique(meta.wpc$OUTCOME0) ) {

        supp_doc <- read_docx(
          here::here(res.dir, paste0("GFS Online Supplement Part 2_", focal.better.name,".docx"))
        )

        myvar0.bn <- get_outcome_better_name(tmp.vec[i], include.name = FALSE)
        tmp.bn = paste0("Figure S",i,". Heterogeneity in the effects of ", focal.better.name ," on ", myvar0.bn ," scores across countries. (A) without controlling for PCs; (B) controlling for PCs.")

        ## ============================================================================================ ##
        ## ====== Forest plot for Secure Flourishing Index ============================================ ##
        {
          p1 <- meta.wopc %>% ungroup() %>%
            filter(OUTCOME0 == tmp.vec[i]) %>%
            select(forest.plot)
          p1 <- p1[[1]]
          ggsave(
            filename = here::here(res.dir, "fig", paste0("figure_s",i,"a_", tmp.vec[i],".png")),
            plot=p1[[1]], units="in", width=6, height=5
          )

          p2 <- meta.wpc %>% ungroup() %>%
            filter(OUTCOME0 == tmp.vec[i]) %>%
            select(forest.plot)
          p2 <- p2[[1]]
          ggsave(
            filename = here::here(res.dir, "fig", paste0("figure_s",i,"b_", tmp.vec[i],".png")),
            plot=p2[[1]], units="in", width=6, height=5
          )
        }

        supp_doc <- supp_doc %>%
          body_add_par(value = tmp.bn, style = "centered") |>
          body_add_par(value = "", style = "centered") |>
          body_add_par(value = "", style = "centered") |>
          body_add_par(value = "", style = "centered") |>
          body_end_block_section(value = landscape_one_column) |>
          body_add_img(
            src = here::here(res.dir, "fig", paste0("figure_s",i,"a_", tmp.vec[i],".png")),
            height = 4, width = 4.8
          ) %>%
          body_add_img(
            src = here::here(res.dir,"fig", paste0("figure_s",i,"b_", tmp.vec[i],".png")),
            height = 4, width = 4.8
          ) |>
          body_end_block_section(value = landscape_two_columns) |>
          body_add_break()

        print(
          supp_doc,
          target = here::here(res.dir, paste0("GFS Online Supplement Part 2_", focal.better.name,".docx"))
        )

      }
    }



  }

}
