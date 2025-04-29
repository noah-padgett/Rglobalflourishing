#' Construct GFS Main text results
#'
#' Generated a word document containing the results for the meta-analytic outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param meta.wopc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did NOT include the principal components in the country-specific regression analyses
#' @param meta.wpc a nested data.frame of results from the meta-analysis function (see gfs_meta_analysis(.)) of the meta-analytic results that did INCLUDED the principal components in the country-specific regression analyses
#' @param focal.better.name a character that is used as the printed name in tables/captions to denote the focal predictor
#' @param p.bonferroni a number (e.g., 0.00081), is internally determined based on number of rows in meta.wopc if not provided
#' @param baseline.pred a vector of characters defining which baseline characteristics were used as control variables. This can be used to force the inclusion some variable into the main text summary table.
#' @param outcome.vec a character vector of outcomes names (e.g., "HAPPY_Y2") that are to be printed in the main text meta-analytic summary table. Name MUST be included in the meta.wopc (meta.wpc) nested data.frames column (OUTCOME0), otherwise the variable won't be printed.
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
gfs_generate_main_doc <- function(df.raw=NULL, meta.wopc=NULL, meta.wpc=NULL, focal.better.name="Focal Predictor", focal.predictor.reference.value="estimated population mean of focal predictor", focal.predictor=NULL, p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL, res.dir = "results", wgt = as.name("WGT"), wgt1 = as.name("ANNUAL_WEIGHT_R2"), wgt2 = as.name("SAMP.ATTR.WGT"), psu = as.name("PSU"), strata = as.name("STRATA"), ci.bonferroni = FALSE){

  n.print = df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()


  if(is.null(baseline.pred)){
    baseline.pred = str_remove(
      c(
        "COV_AGE_GRP_Y1",
        "COV_GENDER",
        "COV_EDUCATION_3_Y1",
        "COV_EMPLOYMENT_Y1",
        "COV_MARITAL_STATUS_Y1",
        "COV_ATTEND_SVCS_Y1",
        "COV_BORN_COUNTRY_Y1",
        "COV_PARENTS_12YRS_Y1",
        "COV_SVCS_12YRS_Y1",
        "COV_MOTHER_RELATN_Y1",
        "COV_FATHER_RELATN_Y1",
        "COV_OUTSIDER_Y1",
        "COV_ABUSED_Y1",
        "COV_HEALTH_GROWUP_Y1",
        "COV_INCOME_12YRS_Y1",
        "COV_REL1_Y1",
        "COV_RACE_PLURALITY_Y1"
      ), "COV_")
  }
  if (!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    set_flextable_defaults(font.family = "Open Sans")

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
      OUTCOME.VEC <- c(
        # Flourishing
        'blank',
        "COMPOSITE_FLOURISHING_SECURE",
        "COMPOSITE_FLOURISHING",
        # Remove domains -> only reported in online supplement
        #"COMPOSITE_HAPPI_LIFE_SAT",
        #"COMPOSITE_HEALTH",
        #"COMPOSITE_MEANING_PURPOSE",
        #"COMPOSITE_CHARACTER",
        #"COMPOSITE_SUBJECTIVE_SOC_CONN",
        #"COMPOSITE_FINL_MAT_WORRY",

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

        # Psychological Distress
        'blank',
        'THREAT_LIFE',
        'COMPOSITE_DEPRESSION', # online supplement only -> 'DEPRESSED', 'INTEREST',
        'COMPOSITE_ANXIETY', # online supplement only -> 'FEEL_ANXIOUS', 'CONTROL_WORRY',
        'SUFFERING',

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

        # Social Participation
        'blank',
        'MARITAL_STATUS_EVER_MARRIED',
        'MARITAL_STATUS_DIVORCED',
        'NUM_CHILDREN',
        'GROUP_NOT_REL',
        'ATTEND_SVCS',

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
        'CIGARETTES_BINARY',
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
        'INCOME_QUINTILE'
      )
      OUTCOME.VEC <- c(paste0(OUTCOME.VEC, "_Y2"))
      # OUTCOME.VEC <- paste0(OUTCOME.VEC, "_Y2")
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
        "Psychological Distress",
        "Social Well-Being",
        "Social Participation",
        "Social Distress",
        "Character & Prosocial Behavior",
        "Physical Health & Health Behavior",
        "Socioeconomic Outcomes"
      )
    } else {
      MYLABEL = mylabels
    }
  }

  if(is.null(p.bonferroni)){
    p.bonferroni = 0.05/length(OUTCOME.VEC[OUTCOME.VEC != "blank"])
  }

  ## ============================================================================================ ##
  ## ====== Construct main text data summary table ============================================== ##
  tb.num <- 1
  {

    df.raw <- gfs_add_variable_labels(df.raw, OUTCOME.VEC)

    tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
    tmp00 <- tmp00[(tmp00 %in% baseline.pred)]
    df.w1 <- df.raw %>%
      select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, contains("_Y1")) %>%
      mutate(
        "{{wgt}}" := {{wgt1}}
      )
    colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
    df.w1$WAVE0 <- "Wave 1"
    df.w2 <- df.raw %>%
      filter(CASE_OBSERVED_Y2 == 1) %>%
      select(ID, COUNTRY, {{wgt2}}, {{psu}}, {{strata}}, GENDER, contains("_Y2"), any_of(tmp00)) %>%
      mutate(
        "{{wgt}}" := n() * {{wgt2}} / sum( {{wgt2}} )
      )
    colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
    colnames(df.w2) <- str_remove(colnames(df.w2), "_Y2")
    df.w2$WAVE0 <- "Wave 2"

    df.raw.long <- suppressMessages({
      full_join(df.w1, df.w2)
    })

    focal.predictor0 <- str_remove(focal.predictor,"_Y1")
    OUTCOME.VEC0 <- str_remove(OUTCOME.VEC,"_Y2")
    baseline.pred0 <- str_remove(baseline.pred,"_Y1")

    df.raw.long <- df.raw.long %>%
      select(
        COUNTRY, {{wgt}}, {{wgt1}}, {{wgt2}}, {{psu}}, {{strata}},
        WAVE0,
        AGE,
        any_of(c(focal.predictor0,OUTCOME.VEC0)),
        any_of(c(baseline.pred0))
      ) %>%
      # TO-DO, figure out a way to remove the leading values (doesn't work for)
      mutate(
        across(any_of(c("COUNTRY", focal.predictor0, OUTCOME.VEC0, baseline.pred0)), \(x){
          if(cur_column() == "COUNTRY"){
            x = factor(x)
          }
          if ( is.factor(x) & str_detect(cur_column(), "AGE_GRP", negate = TRUE) ) {
            lvls <- levels(x)
            relvls <- lvls
            for (i in 1:length(lvls)) {
              if ( str_detect(lvls[i],"\\. ") ) {
                relvls[i] = paste0("    ",stringr::str_trim(stringr::str_split_fixed(lvls[i], "\\. ", 2)[,2]))
              }
              if ( str_detect(lvls[i],"Missing") ) {
                relvls[i] = "    (Missing)"
              }
              if(cur_column() == "COUNTRY"){
                relvls[i] = paste0("    ",lvls[i])
              	if(str_detect(lvls[i], "Hong Kong")){
              	  relvls[i] = paste0("    Hong Kong (S.A.R. of China)")
              	}
              }
            }
            x = factor(x, levels = lvls, labels = relvls)
          }
          x
        })
      )

    df.raw.long <- gfs_add_variable_labels(df.raw.long, OUTCOME.VEC)

    ## add labels for focal predictor(s)
    for (i in 1:length(focal.predictor0)) {
      if(any(str_detect(colnames(df.raw.long), focal.predictor0[i]))){
        try({
          attr(df.raw.long[[focal.predictor0[i]]], which = "label") <- focal.better.name[i]
        })
      }
    }

    # temp.dat <-  svydesign(
    #     data = df.raw.long,
    #     ids = df.raw.long[[psu]],
    #     strata = df.raw.long[[strata]],
    #     weights = df.raw.long[[wgt]]
    #   )
    # TODO: figure out why the following doesn't work for this function...
    temp.dat <- df.raw.long %>%
      as_survey_design(
        ids = {{psu}},
        strata = {{strata}},
        weights = {{wgt}}
      )

    suppressWarnings({
      sumtab <- temp.dat %>%
        tbl_svysummary(
          by = WAVE0,
          include = c(
            any_of(focal.predictor0),
            #AGE,
            AGE_GRP,
            GENDER,
            EDUCATION_3,
            #any_of(baseline.pred0),
            COUNTRY
          ),
          label =  list(
            #AGE ~ "Age of participant",
            AGE_GRP ~ "Year of birth",
            GENDER ~ "Gender",
            #MARITAL_STATUS ~ "Respondent marital status",
            #EMPLOYMENT ~ "Employment status",
            #ATTEND_SVCS ~ "Current religious service attendance",
            EDUCATION_3 ~ "Education (years)",
            #BORN_COUNTRY ~ "Immigration status",
            #PARENTS_12YRS ~ "Parental marital status around age 12",
            #MOTHER_RELATN ~ "Relationship with mother when growing up",
            #FATHER_RELATN ~ "Relationship with father when growing up",
            #OUTSIDER ~ "Felt like an outsider in family when growing up",
            #ABUSED ~ "Experienced abuse when growing up",
            #HEALTH_GROWUP ~ "Self-rated health when growing up",
            #INCOME_12YRS ~ "Subjective financial status of family growing up",
            #SVCS_12YRS ~ "Religious service attendance around age 12",
            #REL1 ~ "Religious affiliation growing up",
            COUNTRY ~ "Country of respondent"
          ),
          type = list(
            #AGE ~ "continuous2",
            all_continuous() ~ "continuous2"
          ),
          statistic = list(
            all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
            all_categorical() ~ "{n} ({p}%)"
          ),
          digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ 0
          ),
          missing_text = "    (Missing)",
          missing_stat = "{N_miss} ({p_miss}%)"
        ) %>%
        italicize_labels()
    })

    tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data; cumulative percentages for variables may not add up to 100% due to rounding; S.A.R., Special Administrative Region. Expanded summary tables of all demographic characteristics and outcome variables are provided in Tables S1-2 in our online supplement.")

    sumtab.toprint <- sumtab %>%
      as_flex_table() %>%
      autofit() %>%
      format_flex_table(pg.width = 21 / 2.54 - 2) %>%
      set_caption(
        as_paragraph(
          as_chunk(paste0("Table ", tb.num ,". Weighted sample demographic summary statistics."),
                   props = fp_text_default(font.family = "Open Sans"))
        ),
        align_with_table = TRUE
      ) %>%
      add_footer_row(
        values = tb.note.summarytab, top = FALSE,colwidths=3
      )
    tb.num <- tb.num + 1
  }
  ## ============================================================================================ ##
  ## ====== Construct meta-analyzed results output table ======================================== ##
  tbl.meta.list <- list()
  f0=1
  for(f0 in 1:length(focal.predictor)){
    vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue")
    vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue")
    if(ci.bonferroni){
    	vec.id <- c("theta.rma", "theta.rma.ci.bon", "tau","global.pvalue")
    	vec.rr <- c("rr.theta", "rr.theta.ci.bon", "rr.tau","global.pvalue")
    }
    vec.wopc <- c("RR", "ES","95% CI","τ", "Global p-value")
    vec.wpc <- c("RR\r", "ES\r","95% CI\r","τ\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
    cnames <- c(
      "Outcome",
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
        meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = FALSE))
        tmp.vec <- case_when(
          get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
        ## ====== Random effects meta - estimates withOUT PCs ======================================= ##
        tmp.wopc <- load_meta_result(
          file = meta.wopc,
          predictor = focal.predictor[f0],
          outcome = OUTCOME.VEC[i],
          what = tmp.vec
        )
        tmp.wopc <- tmp.wopc %>%
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
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
        ## ====== Random effects meta - estimates WITH PCs ======================================= ##
        tmp.wpc <- load_meta_result(
          file = meta.wpc,
          predictor = focal.predictor[f0],
          outcome = OUTCOME.VEC[i],
          what = tmp.vec
        )
        tmp.wpc <- tmp.wpc %>%
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
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
        ## ====== Add Results to output object ====================================================== ##
        if(nrow(tmp.wopc) > 0){
          if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
            meta.outcomewide[i,vec.wopc[-1]] <- tmp.wopc[tmp.vec]
          }
          if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
            meta.outcomewide[i,vec.wopc[-2]] <- tmp.wopc[tmp.vec]
          }
        }
        if(nrow(tmp.wpc) > 0){
          if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
            meta.outcomewide[i,vec.wpc[-1]] <- tmp.wpc[tmp.vec]
          }
          if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
            meta.outcomewide[i,vec.wpc[-2]] <- tmp.wpc[tmp.vec]
          }
        }
      }
    }
    #meta.outcomewide <- na.omit(meta.outcomewide)


    # footnote information:
    tb.note.meta.outcomewide <- as_paragraph(paste0("_Notes_. N=", n.print, "; Reference for focal predictor: ", focal.predictor.reference.value,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; τ (Heterogeneity, tau), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', (1-p.bonferroni/2)*100,'% CIs;'), ';')," ǂEstimate of τ (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects."))

    meta.outcomewide.toprint <- meta.outcomewide %>%
      flextable() %>%
      set_caption(
        as_paragraph(
          as_chunk(paste0("Table ", tb.num,". Meta-analyzed associations of ", focal.better.name[f0] ," at Wave 1 with well-being and other outcomes at Wave 2."),
                   props = fp_text_default(font.family = "Open Sans"))
        ),
        align_with_table = TRUE
      ) %>%
      # uncomment when using all outcomes
      italic(part = "body",
             i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
             j = 1) %>%
      add_header_row(
        values = c("", "Model 1: Demographic and Childhood Variables as Controls", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (via principal components) as Controls"),
        colwidths = c(1,length(vec.wopc), 1, length(vec.wpc))
      ) %>%
      add_footer_row(
        values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
      ) %>%
      theme_meta_outcome_wide()


    tbl.meta.list[[f0]] <- meta.outcomewide.toprint
    tb.num <- tb.num + 1
  }
  ## ============================================================================================ ##
  ## ====== Construct meta-analytic E-values output table ======================================= ##
  tbl.evalues.list <- list()
  for(f0 in 1:length(focal.predictor)){
    vec.id <- c("theta.rma.EE", "theta.rma.ECI")
    vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
    vec.wopc <- c("E-Value","E-Value for CI")
    vec.wpc <- c("E-Value\r","E-Value for CI\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
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
        ## ====== Random effects meta - estimates withOUT PCs =================================== ##
        tmp.wopc <- load_meta_result(
          file = meta.wopc,
          predictor = focal.predictor[f0],
          outcome = OUTCOME.VEC[i],
          what = tmp.vec
        )
        tmp.wopc <- tmp.wopc %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,2)),
          )
        ## ====== Random effects meta - estimates WITH PCs ====================================== ##
        tmp.wpc <- load_meta_result(
          file = meta.wpc,
          predictor = focal.predictor[f0],
          outcome = OUTCOME.VEC[i],
          what = tmp.vec
        )
        tmp.wpc <- tmp.wpc %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,2)),
          )
        ## ====== Add Results to output object ================================================== ##
        if(nrow(tmp.wopc) > 0) meta.evalues[i,vec.wopc] <- tmp.wopc[tmp.vec]
        if(nrow(tmp.wpc) > 0) meta.evalues[i,vec.wpc] <- tmp.wpc[tmp.vec]
      }
    }
    #meta.evalues <- na.omit(meta.evalues)

    # footnote information:
    tb.note.evalues <-as_paragraph("_Notes_. N=", n.print, "; The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for Estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

    meta.evalues.toprint <- meta.evalues %>%
      flextable() %>%
      #autofit() %>%
      set_caption(
        as_paragraph(
          as_chunk(paste0("Table ", tb.num ,". E-value sensitivity analysis for unmeasured confounding between ", focal.better.name[f0], " and subsequent outcomes."),
                   props = fp_text_default(font.family = "Open Sans"))
        ),
        align_with_table = TRUE
      ) %>%
      # uncomment when using all outcomes
      italic(part = "body",
             i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
             j = 1) %>%
      add_header_row(
        values = c("", "Model 1: Demographic and Childhood Variables as Controls", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (via principal components) as Controls"),
        colwidths = c(1, length(vec.wopc), 1, length(vec.wpc))
      ) %>%
      add_footer_row(
        values = tb.note.evalues, top = FALSE, colwidths = ncol(meta.evalues)
      ) %>%
      theme_meta_evalues()
    tbl.evalues.list[[f0]] <- meta.evalues.toprint
    tb.num <- tb.num + 1
  }
  ## ============================================================================================ ##
  ## ====== Forest plot for Secure Flourishing Index ============================================ ##
  tb.cap.fig1 <- list()
  fig.num <- 1
  for(f0 in 1:length(focal.predictor)){
    tb.cap.fig1[[f0]] <- paste0("Figure ",fig.num,". Heterogeneity in the effects of ", focal.better.name[f0] ," at Wave 1 on composite Secure Flourishing Index scores at Wave 2 across countries(N=", n.print, "). (A) controlling for demographic and childhood variables; and (B) controlling for demographic, childhood, and other Wave 1 confounders (via principal components).")

    p1 <- load_meta_result(
      file = meta.wopc,
      predictor = focal.predictor[f0],
      outcome = "COMPOSITE_FLOURISHING_SECURE_Y2",
      what = "forest.plot"
    )
    p1 <- p1[[1]][[1]] +
    		patchwork::plot_annotation(
    			subtitle = str_wrap("(A) Controlling for demographic and childhood variables.",60),
    			title=NULL
    		)
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"A_SFI on ",focal.better.name[f0]," without PCs.png")),
      plot=p1, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"A_SFI on ",focal.better.name[f0]," without PCs.pdf")),
      plot=p1, units="in", width=6, height=5
    )

    p2 <- load_meta_result(
      file = meta.wpc,
      predictor = focal.predictor[f0],
      outcome = "COMPOSITE_FLOURISHING_SECURE_Y2",
      what = "forest.plot"
    )
    p2 <- p2[[1]][[1]] +
        patchwork::plot_annotation(
    			subtitle = str_wrap("(B) Controlling for demographic, childhood, and other Wave 1 confounders (via PCs) variables.",60),
    			title=NULL
    		)
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"B_SFI on ",focal.better.name[f0]," with PCs.png")),
      plot=p2, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"B_SFI on ",focal.better.name[f0]," with PCs.pdf")),
      plot=p2, units="in", width=6, height=5
    )
    fig.num <- fig.num + 1
  }
  ## ============================================================================================ ##
  ## ====== Print out tables to formatted Word document ========================================= ##
  main_doc <- read_docx() |>
    body_add_flextable(value = sumtab.toprint) |>
    body_end_block_section(value = normal_portrait) |>
    body_add_break()

  for(f0 in 1:length(focal.predictor)){
    main_doc <- main_doc |>
      body_add_flextable(value = tbl.meta.list[[f0]]) |>
      body_end_block_section(value = landscape_one_column) |>
      body_add_break()
  }

  for(f0 in 1:length(focal.predictor)){
    main_doc <- main_doc |>
      body_add_flextable(value = tbl.evalues.list[[f0]]) |>
      body_end_block_section(value = normal_portrait) |>
      body_add_break()
  }

  fig.num <- 1
  for(f0 in 1:length(focal.predictor)){
    tmp.bn <- fpar(ftext(tb.cap.fig1[[f0]], fp_text(font.size = 11, font.family = "Open Sans")))

    main_doc <- main_doc %>%
      body_add_fpar(tmp.bn) |>
      body_add_par(value = "", style = "centered") |>
      body_add_par(value = "", style = "centered") |>
      body_add_par(value = "", style = "centered") |>
      body_end_block_section(value = landscape_one_column) |>
      body_add_img(
        src = here::here(res.dir, paste0("figure_",fig.num,"A_SFI on ",focal.better.name[f0]," without PCs.png")),
        height = 4, width = 4.8
      ) %>%
      body_add_img(
        src = here::here(res.dir, paste0("figure_",fig.num,"B_SFI on ",focal.better.name[f0]," with PCs.png")),
        height = 4, width = 4.8
      ) |>
      body_end_block_section(value = landscape_two_columns) |>
      body_add_break()

  }

  print(
    main_doc,
    target = here::here(res.dir,paste0("GFS Main Text Tables_", paste0(focal.better.name, collapse=" "),".docx"))
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
#' @param outcome.vec a character vector of outcomes names (e.g., "HAPPY_Y2") that are to be printed in the main text meta-analytic summary table. Name MUST be included in the coun.wopc (coun.wpc) nested data.frames column (OUTCOME0), otherwise the variable won't be printed.
#' @param mylabels an optional character vector that will be printed out in specific rows of tables 2/3 depending on the specification pf outcome.vec
#' @param included.countries a character vector of which countries to include in output -- defaults to all.
#' @param res.dir (character) defaults to "results", and will be created if needed to story results document
#' @param focal.predictor.reference.value (character) describing the baseline/reference group for the focal predictor.
#' @param what (character) options include: "all", "S1", "S2", "S3".
#' @param ... other arguments as needed
#' @returns a word document saved to the current 'results/' directory
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' Generates three online supplements.
#'
#' Supplement 1: Supplemental main-text results
#'
#' (1) Summary statistics of outcomes by wave (raw data + imputed data)
#' (2) Supplemental meta-analyzed results
#'    - Compare attrition weights results with multiple-imputation results (one for each model)
#'
#' Supplement 2: Country-specific results
#'
#'    - Summary statistics by wave for demographics (similar to main text Table 1)
#'    - Summary statistics by wave for outcomes (similar to Table S1)
#'	  - Summary of attrition model and distribution of attrition weights
#'    - Summary statistics of principal components by outcome (# retained, % prop explained, cumsum % prop explained)
#'    - Outcome-wide results-attrition weights & multiple imputation (similar to main text Table 2-extra-wide p. format)
#'    - Outcome-wide E-values (similar to main text Table 3)
#'
#' Supplement 3: Restructured country-specific results + forest plots
#'
#' (1) Summary statistics of demographics by country (raw data)
#' (2) Summary statistics of outcomes by country (raw data)
#' (3) Forest plots of all effects
#'     - Model 1 (No PCs -- attrition weights)
#'     - Model 2 (w/ PCs -- attrition weights)
#'
#'
gfs_generate_supplemental_docs <- function(
    df.raw = NULL, file.imp.data = NULL,
    dir.primary="results-primary", dir.supp="results-cca", dir.unstd = "results-unstd",
    dir.attr.models = "results-attr",
    focal.predictor = NULL, focal.better.name="Focal Predictor",
    focal.predictor.reference.value="estimated population mean of focal predictor",
    p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL,
    wgt = as.name("WGT0"), wgt1 = as.name("ANNUAL_WEIGHT_R2"), wgt2 = as.name("SAMP.ATTR.WGT"),
    psu = as.name("PSU"), strata = as.name("STRATA"),
    res.dir = "results", included.countries=NULL,
    ci.bonferroni = FALSE, single.file.num.sequential = FALSE, what = "all"){

  focal.predictor0 <- str_remove(focal.predictor,"_Y1")

  if(is.null(baseline.pred)){
    baseline.pred = str_remove(
      c(
        "COV_AGE_GRP_Y1",
        "COV_GENDER",
        "COV_EDUCATION_3_Y1",
        "COV_EMPLOYMENT_Y1",
        "COV_MARITAL_STATUS_Y1",
        "COV_ATTEND_SVCS_Y1",
        "COV_BORN_COUNTRY_Y1",
        "COV_PARENTS_12YRS_Y1",
        "COV_SVCS_12YRS_Y1",
        "COV_MOTHER_RELATN_Y1",
        "COV_FATHER_RELATN_Y1",
        "COV_OUTSIDER_Y1",
        "COV_ABUSED_Y1",
        "COV_HEALTH_GROWUP_Y1",
        "COV_INCOME_12YRS_Y1",
        "COV_REL1_Y1",
        "COV_RACE_PLURALITY_Y1",
        "RACE1_Y1"
      ), "COV_")
  }
  baseline.pred0 <- str_remove(baseline.pred, "_Y1")
  if(!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  if(!dir.exists(here::here(res.dir, "fig"))){
    dir.create(here::here(res.dir, "fig"))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    # text formatting
    set_flextable_defaults(font.family = "Open Sans")
    gfs_title1_prop <- fp_text(color = "black", bold = TRUE, font.size = 14, font.family = "Open Sans")

    # page formatting
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
    extra_extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 29.7 / 2.54 * 4,
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

        # Psychological Distress
        'blank',
        'THREAT_LIFE',
        'COMPOSITE_DEPRESSION', # online supplement only ->
        'DEPRESSED', 'INTEREST',
        'COMPOSITE_ANXIETY', # online supplement only ->
        'FEEL_ANXIOUS', 'CONTROL_WORRY',
        'SUFFERING',

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

        # Social Participation
        'blank',
        'MARITAL_STATUS_EVER_MARRIED',
        'MARITAL_STATUS_DIVORCED',
        'NUM_CHILDREN',
        'GROUP_NOT_REL',
        'ATTEND_SVCS',

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
        'CIGARETTES_BINARY',
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

        # Religion & Spirituality
        'blank',
        'CONNECTED_REL',
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
      OUTCOME.VEC <- c(paste0(OUTCOME.VEC0, "_Y2"))
      # OUTCOME.VEC <- paste0(OUTCOME.VEC, "_Y2")
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
        "Psychological Distress",
        "Social Well-Being",
        "Social Participation",
        "Social Distress",
        "Character & Prosocial Behavior",
        "Physical Health & Health Behavior",
        "Socioeconomic Outcomes",
        "Religion & Spirituality"
      )
    } else {
      MYLABEL = mylabels
    }
  }
  if(is.null(p.bonferroni)){
    p.bonferroni = 0.05/length(OUTCOME.VEC[OUTCOME.VEC != "blank"])
  }
  ## ============================================================================================ ##
  ## Restructing raw data
  ## Reformat to long by wave
  suppressMessages({
    suppressWarnings({

      df.raw <- gfs_add_variable_labels(df.raw, OUTCOME.VEC)

      tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
      tmp00 <- tmp00[(tmp00 %in% baseline.pred)]
      df.w1 <- df.raw %>%
        select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, RACE1, contains("_Y1")) %>%
        mutate(
          "{{wgt}}" := {{wgt1}}
        )
      colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
      df.w1$WAVE0 <- "Wave 1"
      df.w2 <- df.raw %>%
        filter(CASE_OBSERVED_Y2 == 1) %>%
        select(ID, COUNTRY, {{wgt2}}, {{psu}}, {{strata}}, GENDER, RACE1, contains("_Y2"), any_of(tmp00)) %>%
        mutate(
          "{{wgt}}" := n() * {{wgt2}} / sum( {{wgt2}} )
        )
      colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
      colnames(df.w2) <- str_remove(colnames(df.w2), "_Y2")
      df.w2$WAVE0 <- "Wave 2"

      df.raw.long <-
        full_join(df.w1, df.w2)

      n1.print <- nrow(df.w1)
      n2.print <- nrow(df.w2)
      w1.n1.print <- df.w1 %>% group_by(COUNTRY) %>% summarize(N=n())
      w2.n2.print <- df.w2 %>% group_by(COUNTRY) %>% summarize(N=n())

      focal.predictor0 <- str_remove(focal.predictor,"_Y1")
      OUTCOME.VEC0 <- str_remove(OUTCOME.VEC,"_Y2")
      OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "CIGARETTES")] <- "CIGARETTES"
      baseline.pred0 <- str_remove(baseline.pred,"_Y1")

      df.raw.long <- df.raw.long %>%
        select(
          COUNTRY,
          {{wgt}}, {{wgt1}}, {{wgt2}}, {{psu}}, {{strata}},
          WAVE0,
          {focal.predictor0},
          AGE,
          any_of(c(OUTCOME.VEC0)),
          any_of(c(baseline.pred0)),
          INCOME, RACE1
        ) %>%
        mutate(
          INCOME = forcats::fct(INCOME),
          RACE1 = forcats::fct(RACE1),
          across(any_of(c("COUNTRY", focal.predictor0, OUTCOME.VEC0, baseline.pred0, "INCOME", "RACE1")), \(x){
            if ( is.factor(x) & str_detect(cur_column(), "AGE_GRP", negate = TRUE) ) {
              lvls <- levels(x)
              relvls <- lvls
              for (i in 1:length(lvls)) {
                if ( str_detect(lvls[i],"\\. ") ) {
                  relvls[i] = paste0("    ",stringr::str_trim(stringr::str_split_fixed(lvls[i], "\\. ", 2)[,2]))
                }
                if ( str_detect(lvls[i],"Missing") ) {
                  relvls[i] = "    (Missing)"
                }
              }
              x = factor(x, levels = lvls, labels = relvls)
            }
            x
          })
        )

      tmp.vec <- c(baseline.pred0, OUTCOME.VEC0)
      df.raw.long <- gfs_add_variable_labels( df=df.raw.long, vars=tmp.vec )

      ## add labels for focal predictor(s)
      for (i in 1:length(focal.predictor0)) {
        if(any(str_detect(colnames(df.raw.long), focal.predictor0[i]))){
          try({
            attr(df.raw.long[[focal.predictor0[i]]], which = "label") <- focal.better.name[i]
          })
        }
      }

      OUTCOME.VEC.LABELS <- list()
      OUTCOME.VEC00 <- OUTCOME.VEC0[OUTCOME.VEC0 %in% colnames(df.raw.long)]
      for(i in 1:length(OUTCOME.VEC00)){
        OUTCOME.VEC.LABELS[[OUTCOME.VEC00[i]]] <- get_outcome_better_name(
          OUTCOME.VEC00[i],
          include.name = FALSE, include.wave = FALSE
        )
        if(str_detect(OUTCOME.VEC00[i], "GROUP_NOT_REL" )){
          OUTCOME.VEC.LABELS[[OUTCOME.VEC00[i]]] <- "Secular community participation"
        }
        if(OUTCOME.VEC00[i] == "ATTEND_SVCS" ){
          OUTCOME.VEC.LABELS[[OUTCOME.VEC00[i]]] <- "Religious service attendance"
        }

      }

    })
  })
  ## Reformat to long (of wave 1 variables only) of attr/retained cases to compare wave 1 variables
  suppressMessages({
    suppressWarnings({
      # compare UNWEIGHTED data
      tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
      tmp00 <- tmp00[(tmp00 %in% baseline.pred)]
      df.w1 <- df.raw %>%
        filter(CASE_OBSERVED_Y2 == 1) %>%
        select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, RACE1, contains("_Y1")) %>%
        mutate(
          "{{wgt}}" := {{wgt1}}
        )
      colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
      df.w1$WAVE0 <- "Retained--Observed in Wave 2"
      df.w2 <- df.raw %>%
        filter(CASE_OBSERVED_Y2 == 0) %>%
        select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, RACE1, contains("_Y1")) %>%
        mutate(
          "{{wgt}}" := n() * {{wgt1}} / sum( {{wgt1}} )
        )
      colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
      df.w2$WAVE0 <- "Attritors--Not Observed in Wave 2"


      df.raw.attr.retained <-
          full_join(df.w1, df.w2)

      df.raw.attr.retained <- df.raw.attr.retained %>%
        select(
          COUNTRY,
          {{wgt}}, {{wgt1}}, {{psu}}, {{strata}},
          WAVE0,
          {focal.predictor0},
          AGE,
          any_of(c(OUTCOME.VEC0)),
          any_of(c(baseline.pred0)),
          INCOME, RACE1
        ) %>%
        mutate(
          INCOME = forcats::fct(INCOME),
          RACE1 = forcats::fct(RACE1),
          across(any_of(c("COUNTRY", focal.predictor0, OUTCOME.VEC0, baseline.pred0, "INCOME", "RACE1")), \(x){
            if ( is.factor(x) & str_detect(cur_column(), "AGE_GRP", negate = TRUE) ) {
              lvls <- levels(x)
              relvls <- lvls
              for (i in 1:length(lvls)) {
                if ( str_detect(lvls[i],"\\. ") ) {
                  relvls[i] = paste0("    ",stringr::str_trim(stringr::str_split_fixed(lvls[i], "\\. ", 2)[,2]))
                }
                if ( str_detect(lvls[i],"Missing") ) {
                  relvls[i] = "    (Missing)"
                }
              }
              x = factor(x, levels = lvls, labels = relvls)
            }
            x
          })
        )

      tmp.vec <- c(baseline.pred0, OUTCOME.VEC0)
      df.raw.attr.retained <- gfs_add_variable_labels( df=df.raw.attr.retained, vars=tmp.vec )

      ## add labels for focal predictor(s)
      for (i in 1:length(focal.predictor0)) {
        if(any(str_detect(colnames(df.raw.attr.retained), focal.predictor0[i]))){
          try({
            attr(df.raw.attr.retained[[focal.predictor0[i]]], which = "label") <- focal.better.name[i]
          })
        }
      }

    })
  })

  remove(df.raw,df.w1,df.w2)
  gc()
  ## ============================================================================================ ##
  ## ============================================================================================ ##
  tb.num <- 1
  ## ============================================================================================ ##
  # Supplement 1:
  #	(1) Summary statistics of OUTCOMES by wave (raw data)
  # ========================= #
  if(what == "all" | what == "S1"){
    # initialize output object
    supp_doc <- read_docx() |>
      body_add_fpar(
        fpar(ftext("GFS Online Supplement 1", gfs_title1_prop)), style="centered"
      ) %>%
      #body_add_par("...general caveats...")
      body_end_section_continuous() %>%
      body_add_break()
    ## ========================================================================================== ##
    ## ====== Construct summary tables ========================================================== ##
    {

      ## demographics + childhood predictors
      suppressMessages({
      suppressWarnings({
        sumtab <-  df.raw.long %>%
          as_survey_design(
            ids = {{psu}},
            strata = {{strata}},
            weights = {{wgt}}
          ) %>%
          tbl_svysummary(
            by = WAVE0,
            include = c(
              any_of(focal.predictor0),
              AGE_GRP,
              AGE,
              GENDER,
              MARITAL_STATUS,
              EDUCATION_3, EMPLOYMENT,
              ATTEND_SVCS,
              BORN_COUNTRY,
              PARENTS_12YRS, SVCS_12YRS, MOTHER_RELATN, FATHER_RELATN,
              OUTSIDER, ABUSED, HEALTH_GROWUP, INCOME_12YRS, REL1
            ),
            type = list(
              AGE ~ "continuous2",
              all_continuous() ~ "continuous2"
            ),
            statistic = list(
              all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
              all_categorical() ~ "{n} ({p}%)"
            ),
            label = list(
              AGE_GRP ~ "Year of birth",
              AGE ~ "Age of participant",
              GENDER ~ "Gender",
              #RACE1 ~ "Race/ethnicity",
              MARITAL_STATUS ~ "Respondent marital status",
              EMPLOYMENT ~ "Employment status",
              #INCOME ~ "Self-reported income",
              ATTEND_SVCS ~ "Current religious service attendance",
              EDUCATION_3 ~ "Education (years)",
              BORN_COUNTRY ~ "Immigration status",
              PARENTS_12YRS ~ "Parental marital status around age 12",
              MOTHER_RELATN ~ "Relationship with mother when growing up",
              FATHER_RELATN ~ "Relationship with father when growing up",
              OUTSIDER ~ "Felt like an outsider in family when growing up",
              ABUSED ~ "Experienced abuse when growing up",
              HEALTH_GROWUP ~ "Self-rated health when growing up",
              INCOME_12YRS ~ "Subjective financial status of family growing up",
              SVCS_12YRS ~ "Religious service attendance around age 12",
              REL1 ~ "Religious affiliation growing up"
            ),
            digits = list(
              all_continuous() ~ 1,
              n = label_style_number(digits=0),
              p = label_style_percent(digits=1)
            ),
            missing_text = "    (Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
          ) %>%
          add_stat_label(
            label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
          ) %>%
          modify_header(label ~ "**Outcome**") %>%
          italicize_labels()
      	})
      })

      tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding. Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.")

      tb.cap <- paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables.")
      tb.num = tb.num + 1

      sumtab.toprint.A <- sumtab %>%
        as_flex_table() %>%
        autofit() %>%
        format_flex_table(pg.width = 21 / 2.54 - 2) %>%
        set_caption(
          as_paragraph(
            as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
          ),
          align_with_table = TRUE
        ) %>%
        add_footer_row(
          values = tb.note.summarytab, top = FALSE,colwidths=3
        )

      ## outcomes
      suppressMessages({
      suppressWarnings({
        sumtab <-  df.raw.long %>%
          as_survey_design(
            ids = {{psu}},
            strata = {{strata}},
            weights = {{wgt}}
          ) %>%
          tbl_svysummary(
            by = WAVE0,
            include = c(
              any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
            ),
            type = list(
              all_continuous() ~ "continuous2",
              contains("NUM_CHILDREN") ~ "continuous2",
              contains("CIGARETTES") ~ "continuous2",
              contains("DAYS_EXERCISE") ~ "continuous2"
            ),
            statistic = list(
              all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
              all_categorical() ~ "{n} ({p}%)"
            ),
            label = OUTCOME.VEC.LABELS,
            digits = list(
              all_continuous() ~ 1,
              n = label_style_number(digits=0),
              p = label_style_percent(digits=1)
            ),
            missing_text = "    (Missing)",
            missing_stat = "{N_miss} ({p_miss}%)"
          ) %>%
          add_stat_label(
            label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
          ) %>%
          modify_header(label ~ "**Outcome**") %>%
          italicize_labels()
        })
      })

      tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding. Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.")

      tb.cap <- paste0("Table S",tb.num,". Weighted summary statistics of the outcome variables by Wave.")
      tb.num = tb.num + 1

      sumtab.toprint.B <- sumtab %>%
        as_flex_table() %>%
        autofit() %>%
        width(j=2,width=1.25)%>%
        width(j=3,width=1.25)%>%
        format_flex_table(pg.width = 21 / 2.54 - 2) %>%
        set_caption(
          as_paragraph(
            as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
          ),
          align_with_table = TRUE
        ) %>%
        add_footer_row(
          values = tb.note.summarytab, top = FALSE,colwidths=3
        )
    }
    gc()
    ## ========================================================================================== ##
    ## ====== Construct summary tables ========================================================== ##
    {

      ## demographics + childhood predictors
      suppressMessages({
        suppressWarnings({
          sumtab <- df.raw.attr.retained %>%
            tbl_summary(
              by = WAVE0,
              include = c(
                any_of(focal.predictor0),
                AGE_GRP,AGE,
                GENDER,
                MARITAL_STATUS,
                EDUCATION_3, EMPLOYMENT,
                ATTEND_SVCS,
                BORN_COUNTRY,
                PARENTS_12YRS, SVCS_12YRS, MOTHER_RELATN, FATHER_RELATN,
                OUTSIDER, ABUSED, HEALTH_GROWUP, INCOME_12YRS, REL1
              ),
              type = list(
                AGE ~ "continuous2",
                all_continuous() ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                AGE ~ "Age of participant",
                AGE_GRP ~ "Year of birth",
                GENDER ~ "Gender",
                #RACE1 ~ "Race/ethnicity",
                MARITAL_STATUS ~ "Respondent marital status",
                EMPLOYMENT ~ "Employment status",
                #INCOME ~ "Self-reported income",
                ATTEND_SVCS ~ "Current religious service attendance",
                EDUCATION_3 ~ "Education (years)",
                BORN_COUNTRY ~ "Immigration status",
                PARENTS_12YRS ~ "Parental marital status around age 12",
                MOTHER_RELATN ~ "Relationship with mother when growing up",
                FATHER_RELATN ~ "Relationship with father when growing up",
                OUTSIDER ~ "Felt like an outsider in family when growing up",
                ABUSED ~ "Experienced abuse when growing up",
                HEALTH_GROWUP ~ "Self-rated health when growing up",
                INCOME_12YRS ~ "Subjective financial status of family growing up",
                SVCS_12YRS ~ "Religious service attendance around age 12",
                REL1 ~ "Religious affiliation growing up"
              ),
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            italicize_labels()
        })
      })

      tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

      tb.cap <- paste0("Table S",tb.num,". Unweighted sample summary statistics of demographic and childhood predictor variables by retention status.")
      tb.num = tb.num + 1

      sumtab.toprint.C <- sumtab %>%
        as_flex_table() %>%
        autofit() %>%
        format_flex_table(pg.width = 21 / 2.54 - 2) %>%
        set_caption(
          as_paragraph(
            as_chunk( tb.cap,props = fp_text_default(font.family = "Open Sans"))
          ),
          align_with_table = TRUE
        ) %>%
        add_footer_row(
          values = tb.note.summarytab, top = FALSE,colwidths=3
        )

      ## outcomes
      suppressMessages({
        suppressWarnings({
          sumtab <- df.raw.attr.retained %>%
            tbl_summary(
              by = WAVE0,
              include = c(
                any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
              ),
              type = list(
                all_continuous() ~ "continuous2",
                contains("NUM_CHILDREN") ~ "continuous2",
                contains("CIGARETTES") ~ "continuous2",
                contains("DAYS_EXERCISE") ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = OUTCOME.VEC.LABELS,
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            modify_header(label ~ "**Outcome**") %>%
            italicize_labels()
        })
      })

      tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")
      tb.cap <- paste0("Table S",tb.num,". Unweighted sample summary statistics of outcome variables at Wave 1 by retention status.")
      tb.num = tb.num + 1

      sumtab.toprint.D <- sumtab %>%
        as_flex_table() %>%
        autofit() %>%
        width(j=2,width=1.25)%>%
        width(j=3,width=1.25)%>%
        format_flex_table(pg.width = 21 / 2.54 - 2) %>%
        set_caption(
          as_paragraph(
            as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
          )  ,
          align_with_table = TRUE
        ) %>%
        add_footer_row(
          values = tb.note.summarytab, top = FALSE,colwidths=3
        )

    }
    remove(sumtab)
    gc()
    ## ========================================================================================== ##
    meta.outcomewide.toprint.A <- list()
    meta.outcomewide.toprint.B <- list()
    meta.outcomewide.toprint.C <- list()
    meta.outcomewide.toprint.D <- list()
    f0 = 1
    for(f0 in 1:length(focal.predictor)){
      ## ======================================================================================== ##
      ## ====== Meta-analyzed Results - MI & Attrition Weight =================================== ##
      {
        vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue")
        vec.id <- c("theta.rma", "theta.rma.ci", "tau","global.pvalue")
        vec.rr <- c("rr.theta", "rr.theta.ci", "rr.tau","global.pvalue")
        vec.a <- c("RR", "ES","95% CI","τ", "Global p-value")
        vec.b <- c("RR\r", "ES\r","95% CI\r","τ\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        if(ci.bonferroni){
          vec.a <- c("RR", "ES",paste0(.round(p.bonferroni*100,1),"% CI"),"τ", "Global p-value")
          vec.b <- c("RR\r", "ES\r",paste0(.round(p.bonferroni*100,1),"% CI\r"),"τ\r", "Global p-value\r")
        }
        cnames <- c(
          "Outcome",
          vec.a,
          "\r",
          vec.b
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
            meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = TRUE))
            tmp.vec <- case_when(
              get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
              .default = vec.rr
            )
            ## ====== Random effects meta - estimates withOUT PCs ======================================= ##
            tmp.a <- load_meta_result(
              file = here::here(dir.primary, "0_meta_analyzed_results_primary_wopc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Supplemental random effects meta - estimates withOUT PCs ==================== ##
            tmp.b <- load_meta_result(
              file = here::here(dir.supp, "0_meta_analyzed_results_cca_wopc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.a) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.a[-1]] <- tmp.a[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.a[-2]] <- tmp.a[tmp.vec]
              }
            }
            if(nrow(tmp.b) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.b[-1]] <- tmp.b[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.b[-2]] <- tmp.b[tmp.vec]
              }
            }
          }
        }
        #meta.outcomewide <- na.omit(meta.outcomewide)


        # footnote information:
        tb.note.meta.outcomewide <-as_paragraph(paste0("_Notes_. N_{multiple imputation}=", n1.print ,"; N_{complete-case}=",n2.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; τ (Heterogeneity, tau), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries;  ^(a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; ^(b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; ^(c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; ^(d) item part of the Character & Virtue domain of the Secure Flourishing Index; ^(e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; ^(f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', (1-p.bonferroni/2)*100,'% CIs;'), ';')," ǂEstimate of τ (tau, heterogeneity) is likely unstable."))


        tb.cap = paste0("Table S",tb.num,". Meta-analyzed associations of ", focal.better.name[f0] ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 1 by how missingness was accounted for (multiple imputation vs. complete case with attrition weights).")
        tb.num = tb.num + 1
        meta.outcomewide.toprint <- meta.outcomewide %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ) ,
            align_with_table = TRUE
          ) %>%
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Multiple Imputation", "", "Complete Case with Attrition Weights"),
            colwidths = c(1,length(vec.a), 1, length(vec.b))
          ) %>%
          add_footer_row(
            values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
          ) %>%
          theme_meta_outcome_wide()

        meta.outcomewide.toprint.A[[f0]] <- meta.outcomewide.toprint
      }
      {
        vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue")
        vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue")
        vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue")
        vec.a <- c("RR", "ES","95% CI","τ", "Global p-value")
        vec.b <- c("RR\r", "ES\r","95% CI\r","τ\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        cnames <- c(
          "Outcome",
          vec.a,
          "\r",
          vec.b
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
            meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = TRUE))
            tmp.vec <- case_when(
              get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
              .default = vec.rr
            )
            ## ====== Random effects meta - estimates with PCs ======================================= ##
            tmp.a <- load_meta_result(
              file = here::here(dir.primary, "0_meta_analyzed_results_primary_wpc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Supplemental random effects meta - estimates with PCs ==================== ##
            tmp.b <- load_meta_result(
              file = here::here(dir.supp, "0_meta_analyzed_results_cca_wpc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.a) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.a[-1]] <- tmp.a[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.a[-2]] <- tmp.a[tmp.vec]
              }
            }
            if(nrow(tmp.b) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.b[-1]] <- tmp.b[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.b[-2]] <- tmp.b[tmp.vec]
              }
            }
          }
        }
        #meta.outcomewide <- na.omit(meta.outcomewide)


        # footnote information:
        tb.note.meta.outcomewide <-as_paragraph(paste0("_Notes_. N_{multiple imputation}=", n1.print ,"; N_{complete-case}=",n2.print ,"; Reference for focal predictor: ", paste0(focal.predictor.reference.value, collapse="; "),"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; τ (Heterogeneity, tau), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries;  ^(a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; ^(b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; ^(c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; ^(d) item part of the Character & Virtue domain of the Secure Flourishing Index; ^(e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; ^(f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. The first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', (1-p.bonferroni/2)*100,'% CIs;'), ';')," ǂEstimate of τ (tau, heterogeneity) is likely unstable."))

        tb.cap = paste0("Table S",tb.num,".  Meta-analyzed associations of ", focal.better.name[f0] ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 2 by how missingness was accounted for (multiple imputation vs. complete case with attrition weights).")
        tb.num = tb.num + 1

        meta.outcomewide.toprint <- meta.outcomewide %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans") )
            ),
            align_with_table = TRUE
          ) %>%
          # uncomment when using all outcomes
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Multiple Imputation", "", "Complete Case with Attrition Weights"),
            colwidths = c(1,length(vec.a), 1, length(vec.b))
          ) %>%
          add_footer_row(
            values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
          ) %>%
          theme_meta_outcome_wide()

        meta.outcomewide.toprint.B[[f0]] <- meta.outcomewide.toprint
      }
      ## ======================================================================================== ##
      ## ====== Construct meta-analytic E-values output table =================================== ##
      {
        vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue")
        vec.id <- c("theta.rma.EE", "theta.rma.ECI")
        vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
        vec.wopc.attr <- c("EE", "ECI")
        vec.wopc.mi <- c("EE\r", "ECI\r")
        vec.wpc.attr <- c("EE\r\r", "ECI\r\r")
        vec.wpc.mi <- c("EE\r\r\r", "ECI\r\r\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        cnames <- c(
          "Outcome",
          vec.wopc.attr, "\r", vec.wpc.attr, "\r\r",
          vec.wopc.mi, "\r\r\r", vec.wpc.mi
        )

        meta.evalues <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
        colnames(meta.evalues) <- cnames
        meta.evalues$"\r" <- ""
        meta.evalues$"\r\r" <- ""
        meta.evalues$"\r\r\r" <- ""
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
            ## ====== Attr wgt - random effects meta - estimates withOUT PCs ====================== ##
            tmp.wopc.attr <- load_meta_result(
              file = here::here(dir.primary, "0_meta_analyzed_results_primary_wopc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = tmp.vec
            ) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Attr wgt - random effects meta - estimates WITH PCs ========================= ##
            tmp.wpc.attr <- load_meta_result(
              file = here::here(dir.primary, "0_meta_analyzed_results_primary_wpc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = tmp.vec
            ) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Supplement MI - random effects meta - estimates withOUT PCs ================= ##
            tmp.wopc.mi <- load_meta_result(
              file = here::here(dir.supp, "0_meta_analyzed_results_cca_wopc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = tmp.vec
            )%>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Supplement MI - random effects meta - estimates WITH PCs ==================== ##
            tmp.wpc.mi <- load_meta_result(
              file = here::here(dir.supp, "0_meta_analyzed_results_cca_wpc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = tmp.vec
            ) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wopc.attr) > 0) meta.evalues[i,vec.wopc.attr] <- tmp.wopc.attr[tmp.vec]
            if(nrow(tmp.wpc.attr) > 0) meta.evalues[i,vec.wpc.attr] <- tmp.wpc.attr[tmp.vec]
            if(nrow(tmp.wopc.mi) > 0) meta.evalues[i,vec.wopc.mi] <- tmp.wopc.mi[tmp.vec]
            if(nrow(tmp.wpc.mi) > 0) meta.evalues[i,vec.wpc.mi] <- tmp.wpc.mi[tmp.vec]
          }
        }
        #meta.evalues <- na.omit(meta.evalues)


        # footnote information:
        tb.note.evalues <-as_paragraph("_Notes_. N_{multiple imputation}=", n1.print ,"; N_{complete-case}=",n2.print ,"; EE, E-value for Estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for Estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

        tb.cap = paste0("Table S",tb.num,". ", focal.better.name[f0], " for comparing estimated E-values across models and how missingness at wave 2 was handled.")
        tb.num = tb.num + 1

        meta.evalues.toprint <- meta.evalues %>%
          flextable() %>%
          #autofit() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Model 1: Demographics and Childhood Variables as Controls", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (via principal components) as Controls", "", "Model 1: Demographics and Childhood Variables as Controls", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (via principal components) as Controls"),
            colwidths = c(1, length(vec.wopc.attr), 1, length(vec.wpc.attr), 1, length(vec.wopc.mi), 1, length(vec.wpc.mi)),
            top = TRUE
          ) %>%
          add_header_row(
            values = c("", "Multiple Imputation", "", "Complete Case w/ Attrition Weights" ),
            colwidths = c(1, length(vec.wopc.attr)+1+length(vec.wpc.attr), 1, length(vec.wopc.mi)+1+length(vec.wpc.mi)),
            top = TRUE
          ) %>%
          add_footer_row(
            values = tb.note.evalues, top = FALSE, colwidths = ncol(meta.evalues)
          ) %>%
          width(j=1,width=2.5)%>%
          format_flex_table(pg.width = 29.7/2.54 - 2) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(meta.evalues)) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header") %>%
          hline(i=1,j=c(2:6,8:12), part="header") %>%
          hline(i=2,j=c(2:3,5:6,8:9,11:12), part="header")

        meta.outcomewide.toprint.C[[f0]] <- meta.evalues.toprint
      }
      ## ======================================================================================== ##
      ## ====== Meta-analyzed Results - Unstandardized Etimates =================================== ##
      {
        vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue")
        vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue")
        vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue")
        vec.a <- c("RR", "ES","95% CI","τ", "Global p-value")
        vec.b <- c("RR\r", "ES\r","95% CI\r","τ\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        cnames <- c(
          "Outcome",
          vec.a,
          "\r",
          vec.b
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
            meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = TRUE))
            tmp.vec <- case_when(
              get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
              .default = vec.rr
            )
            ## ====== Random effects meta - estimates without PCs ======================================= ##
            tmp.a <- load_meta_result(
              file = here::here(dir.unstd, "0_meta_analyzed_results_unstd_wopc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Random effects meta - estimates with PCs ==================== ##
            tmp.b <- load_meta_result(
              file = here::here(dir.unstd, "0_meta_analyzed_results_unstd_wpc.rds"),
              predictor = focal.predictor[f0],
              outcome = OUTCOME.VEC[i],
              what = vec.get
            )  %>%
              dplyr::mutate(
                theta.rma.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se, 2), ",",
                                         .round(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se, 2) ,")"),
                  .default = paste0("(",.round(theta.rma - qnorm(1-0.05/2)*theta.rma.se, 2), ",",
                                    .round(theta.rma + qnorm(1-0.05/2)*theta.rma.se, 2) ,")")
                ),
                rr.theta.ci = case_when(
                  ci.bonferroni ~ paste0("(",.round(exp(theta.rma - qnorm(1-p.bonferroni/2)*theta.rma.se), 2), ",",
                                         .round(exp(theta.rma + qnorm(1-p.bonferroni/2)*theta.rma.se), 2) ,")"),
                  .default = paste0("(",.round(exp(theta.rma - qnorm(1-0.05/2)*theta.rma.se), 2), ",",
                                    .round(exp(theta.rma + qnorm(1-0.05/2)*theta.rma.se), 2) ,")")
                ),
                dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,2)),
                dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
                  case_when(
                    x < 0.01 ~ "< 0.01ǂ",
                    x >= 0.01 ~ .round(x,2)
                  )
                }),
                dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	 x < 0.005 ~ paste0(.round_p(x),"**"),
                	 x < 0.05 ~ paste0(.round_p(x),"*"),
                	 x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.a) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.a[-1]] <- tmp.a[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.a[-2]] <- tmp.a[tmp.vec]
              }
            }
            if(nrow(tmp.b) > 0){
              if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
                meta.outcomewide[i,vec.b[-1]] <- tmp.b[tmp.vec]
              }
              if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
                meta.outcomewide[i,vec.b[-2]] <- tmp.b[tmp.vec]
              }
            }
          }
        }
        #meta.outcomewide <- na.omit(meta.outcomewide)


        # footnote information:
        tb.note.meta.outcomewide <-as_paragraph(paste0("_Notes_. N_{multiple imputation}=", n1.print ,"; N_{complete-case}=",n2.print ,"; Reference for focal predictor: a value of `0` on the predcitor; RR, risk-ratio, null effect is 1.00; ES, effect size measure for regression coefficient, null effect is 0.00; CI, confidence interval; τ (Heterogeneity, tau), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries;  ^(a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; ^(b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; ^(c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; ^(d) item part of the Character & Virtue domain of the Secure Flourishing Index; ^(e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; ^(f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. The first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, and no standardization was conducted prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', (1-p.bonferroni/2)*100,'% CIs;'), ';')," ǂEstimate of τ (tau, heterogeneity) is likely unstable."))

        tb.cap = paste0("Table S",tb.num,". Unstandardized effects sizes for the raw score of ", focal.better.name[f0]," (multiple imputation results only).")
        tb.num = tb.num + 1

        meta.outcomewide.toprint <- meta.outcomewide %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Model 1: Demographic and Childhood Variables as Controls", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (via principal components) as Controls"),
            colwidths = c(1,length(vec.a), 1, length(vec.b))
          ) %>%
          add_footer_row(
            values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
          ) %>%
          theme_meta_outcome_wide()

        meta.outcomewide.toprint.D[[f0]] <- meta.outcomewide.toprint

      }
    }
        ## ========================================================================================== ##
    ## ====== Construct within-between plot of effects (placed at end of file) ================== ##
    {
      # need: meta.wpc, coun.wpc
      # ================================================================================================ #
      # ================================================================================================ #
      ## test, within-between plot
      # Plot all effects as risk-ratios

      # from standardized effect size to RR:
      # RR = exp(0.91*STD_Est);
      # plot.dat <- load_meta_result(
#   file = here::here(dir.primary, "0_meta_analyzed_results_primary_wopc.rds"),
#   predictor = NULL,
#   outcome = NULL,
#   what = c("OUTCOME0", "FOCAL_PREDICTOR", "data", "theta.rma", "theta.rma.se", "theta.rma.ci")
# )
# plot.dat$type <- ""
# for(i in 1:nrow(plot.dat)){
#   plot.dat$type[i] = get_outcome_scale(plot.dat$OUTCOME0[i])
# }
# plot.dat <- plot.dat %>%
#   mutate(
#     type = case_when(
#       type == "cont" ~ "Std. Est",
#       .default = "log(RR)"
#     )
#   ) %>%
#   unnest(c(data)) %>%
#   mutate(
#     est.rr = case_when(
#       type == "log(RR)" ~ exp(Est),
#       type == "Std. Est" ~ exp(0.91*Est)
#     )
#   )
# minY <- min(plot.dat$est.rr,na.rm=TRUE) - 0.10
# maxY <- max(plot.dat$est.rr,na.rm=TRUE) + 0.10
# p <- plot.dat %>%
#   group_by(Country) %>%
#   mutate(
#     avg.rr = mean(est.rr, na.rm=TRUE),
#     var.rr = var(est.rr, na.rm=TRUE),
#     FOCAL_PREDICTOR = get_outcome_better_name(FOCAL_PREDICTOR, FALSE, FALSE, FALSE)
#   ) %>% ungroup() %>%
#   ggplot(aes(x=reorder(Country, avg.rr), y = est.rr)) +
#   geom_jitter(position = position_jitter(width = 0.25, height = 0, seed = 31415)) +
#   geom_hline(yintercept = c(0.90, 1.10), linetype="dashed") +
#   labs(y="Estimated Risk-Ratio", x="",
#        title=stringr::str_wrap("Heterogenetiy in estimated effects within and between countries",50),
#        subtitle="Model estimated controlling for 7 principal components") +
#   scale_x_discrete(guide = guide_axis(angle = 60)) +
#   scale_y_continuous(limits = c(minY,maxY))+
#   facet_grid(FOCAL_PREDICTOR~.) +
#   theme_Publication()
#
# ggsave(
#   filename = here::here(res.dir, paste0("figure_S1_heterogeneity_plot.png")),
#   plot=p, units="in", width=6, height=5
# )
#
# tb.cap.figS1 <- paste0("Figure S1. Heterogeneity in the effects of ", paste0(focal.better.name, collapse=", ") ," on composite wellbeing and other outcomes across countries (N=", n1.print, "). The estimated effects of focal predictor on all wave 2 outcomes are reported after converting to risk-ratios (RR). For continuous outcome, the standardized beta is converted to RR using `exp(0.91*est)` to approximate a RR. The displayed estimates are based on the multiple-imputation results controlling for demographics, childhood predictors, and the first 7 principal components of contemporaneous confounders. Points in scatterplot are jittered horizontally (`position_jitter(width = 0.25, height = 0, seed = 31415)`) to avoid overlap.")
#
# remove(p,minY,maxY,plot.dat)



      }
    ## ========================================================================================== ##
    ## ====== Write tables to file  ======================================= ##
    supp_doc <- supp_doc |>
      body_add_flextable(value = sumtab.toprint.A) |>
      body_end_block_section(value = normal_portrait) |>
      body_add_break() |>
      body_add_flextable(value = sumtab.toprint.B) |>
      body_end_block_section(value = normal_portrait) |>
      body_add_break() |>
      body_add_flextable(value = sumtab.toprint.C) |>
      body_end_block_section(value = normal_portrait) |>
      body_add_break() |>
      body_add_flextable(value = sumtab.toprint.D) |>
      body_end_block_section(value = normal_portrait) |>
      body_add_break()

    for(f0 in 1:length(focal.predictor)){
      supp_doc <- supp_doc |>
        body_add_flextable(value = meta.outcomewide.toprint.A[[f0]]) |>
        body_end_block_section(value = landscape_one_column) |>
        body_add_break() |>
        body_add_flextable(value =  meta.outcomewide.toprint.B[[f0]]) |>
        body_end_block_section(value = landscape_one_column) |>
        body_add_break() |>
        body_add_flextable(value =  meta.outcomewide.toprint.C[[f0]]) |>
        body_end_block_section(value = landscape_one_column) |>
        body_add_break() |>
        body_add_flextable(value =  meta.outcomewide.toprint.D[[f0]]) |>
        body_end_block_section(value = landscape_one_column) |>
        body_add_break()
    }
    ## add heterogeneity plot
    #supp_doc <- supp_doc |>
    #  body_add_par(
    #    as_paragraph(
    #      as_chunk( tb.cap.figS1, props = fp_text_default(font.family = "Open Sans") )
    #    )
    #  ) |>
    #    body_add_img(
    #      src = here::here(res.dir, paste0("figure_S1_heterogeneity_plot.png")),
    #      height = 5, width = 6
    #    )  |>
    #    body_add_break()

    if(single.file.num.sequential){
      out.file <- here::here(res.dir,paste0("GFS-Wave 2 Online Supplement_", paste0(focal.better.name, collapse=" "),".docx"))
    } else {
      out.file <- here::here(res.dir,paste0("GFS-S1 Online Supplement Part 1_", paste0(focal.better.name, collapse=" "),".docx"))
    }

    print(
      supp_doc,
      target = out.file
    )

  }
  ## ============================================================================================== ##
  # Supplement 2: Country-specific results
  #     - Summary statistics by wave for demographics (similar to main text Table S1)
  #     - Summary statistics by wave for outcomes (similar to Table S2)
  #     - Summary statistics by retention status of wave 1 variables (Table S3/4)
  #	    - Summary of attrition model (first imputed dataset)
  #     - Summary statistics of principal components by outcome (# retained, % prop explained, cumsum % prop explained)
  #     - Outcome-wide results (similar to main text Table 2)
  #     - Outcome-wide E-values (similar to main text Table 3)
  # ========================= #
  if(what == "all" | what == "S2"){
    if(single.file.num.sequential){
      out.file <- here::here(res.dir,paste0("GFS-Wave 2 Online Supplement_", paste0(focal.better.name, collapse=" "),".docx"))
    } else {
      # initialize objects as needed
      tb.num <- 1
      out.file <- here::here(res.dir,paste0("GFS-S2 Online Supplement Part 2_", paste0(focal.better.name, collapse=" "),".docx"))
      supp_doc <- read_docx() |>
        body_add_fpar(
          fpar(ftext("GFS Online Supplement Part 2", gfs_title1_prop)), style="centered") %>%
        #body_add_par("...general caveats...")
        body_end_section_continuous() %>%
        body_add_break() |>
        print(target=out.file)
    }


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
              "Mexico",
              "China"
            )
          )
      } else {
        COUNTRY_LABELS = included.countries
      }
      }

    i = 1; tb.num.shift = 0;
    for (i in 1:length(COUNTRY_LABELS)) {
      supp_doc <- read_docx(out.file)

      cat("\nCountry:\t", COUNTRY_LABELS[i])
      ## get country sample size(s)
      if(!single.file.num.sequential){
        tb.num <- 1 # in "not sequential" this is used to shift which letter is pasted to each table #.
      }
      # get country sample sizes
      country.n1.print <- w1.n1.print %>%ungroup() %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
        select(N) %>% as.numeric()
      country.n2.print <- w2.n2.print %>% ungroup() %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
        select(N) %>% as.numeric()

      ## ======================================================================================== ##
      ## ====== Table Sia. summary statistics -- demographics variables ========================= ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.long %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
            mutate(
              #RACE1 = factor(RACE1),
              RACE1 = droplevels(RACE1),
              RACE1 = case_when(is.na(RACE1) ~ "    (Missing)", .default = RACE1),
              RACE1 = factor(RACE1, levels = sort(unique(RACE1))),
              RACE1 = fct_relevel(RACE1, "    (Missing)", after = Inf),
              #INCOME = case_when(INCOME == "(Missing)" ~ "    (Missing)", .default = INCOME),
              #INCOME = factor(INCOME),
              INCOME = droplevels(INCOME),
              INCOME = factor(INCOME, levels = sort(unique(INCOME))),
              INCOME = case_when(is.na(INCOME) ~ "    (Missing)", .default = INCOME),
              INCOME = fct_relevel(INCOME, "    (Missing)", after = Inf),
            ) %>%
            as_survey_design(
              ids = {{psu}},
              strata = {{strata}},
              weights = {{wgt}}
            )

          sumtab <- temp.dat %>%
            tbl_svysummary(
              by = WAVE0,
              include = c(
                any_of(focal.predictor0),
                AGE_GRP, AGE, GENDER, RACE1, MARITAL_STATUS,
                EDUCATION_3, EMPLOYMENT, INCOME,
                ATTEND_SVCS,  BORN_COUNTRY,
                PARENTS_12YRS, SVCS_12YRS, MOTHER_RELATN, FATHER_RELATN,
                OUTSIDER, ABUSED, HEALTH_GROWUP, INCOME_12YRS, REL1
              ),
              type = list(
                AGE ~ "continuous2",
                all_continuous() ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                AGE ~ "Age of participant",
                AGE_GRP ~ "Year of birth",
                GENDER ~ "Gender",
                RACE1 ~ "Race/ethnicity",
                MARITAL_STATUS ~ "Respondent marital status",
                EMPLOYMENT ~ "Employment status",
                INCOME ~ "Self-reported income",
                ATTEND_SVCS ~ "Current religious service attendance",
                EDUCATION_3 ~ "Education (years)",
                BORN_COUNTRY ~ "Immigration status",
                PARENTS_12YRS ~ "Parental marital status around age 12",
                MOTHER_RELATN ~ "Relationship with mother when growing up",
                FATHER_RELATN ~ "Relationship with father when growing up",
                OUTSIDER ~ "Felt like an outsider in family when growing up",
                ABUSED ~ "Experienced abuse when growing up",
                HEALTH_GROWUP ~ "Self-rated health when growing up",
                INCOME_12YRS ~ "Subjective financial status of family growing up",
                SVCS_12YRS ~ "Religious service attendance around age 12",
                REL1 ~ "Religious affiliation growing up"
              ),
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding. Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout to maintain nationally representative estimates for Wave 2 characteristics using the reduced sample.")
          if(!single.file.num.sequential){
            tb.cap <- paste0("Table S",i+tb.num.shift, letters[tb.num],". Weighted demographic and childhood variables summary statistics in ", COUNTRY_LABELS[i])
          }
          if(single.file.num.sequential){
            tb.cap <-  paste0("Table S",tb.num+tb.num.shift,". Weighted demographic and childhood variables summary statistics in ", COUNTRY_LABELS[i])
          }
          tb.num <- tb.num + 1

          tbia.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            width(j=2,width=1.5)%>%
            width(j=3,width=1.5)%>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              as_paragraph(
                as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
              ),
              align_with_table = TRUE
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )
        })
      })
      ## ======================================================================================== ##
      ## ====== Table Sib. summary statistics -- outcome variables ============================== ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.long %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
            as_survey_design(
              ids = {{psu}},
              strata = {{strata}},
              weights = {{wgt}}
            )

          sumtab <- temp.dat %>%
            tbl_svysummary(
              by = WAVE0,
              include = c(
                any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
              ),
              type = list(
                all_continuous() ~ "continuous2",
                contains("NUM_CHILDREN") ~ "continuous2",
                contains("CIGARETTES") ~ "continuous2",
                contains("DAYS_EXERCISE") ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = OUTCOME.VEC.LABELS,
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            modify_header(label ~ "**Outcome**") %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding. Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout to maintain nationally representative estimates for Wave 2 characteristics using the reduced sample.")
          if(!single.file.num.sequential){
            tb.cap <- paste0("Table S",i+tb.num.shift,letters[tb.num],". Weighted summary statistics of outcomes in ", COUNTRY_LABELS[i])
          }
          if(single.file.num.sequential){
            tb.cap <-  paste0("Table S",tb.num+tb.num.shift,". Weighted summary statistics of outcomes in ", COUNTRY_LABELS[i])
          }
          tb.num <- tb.num + 1
          tbib.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            width(j=2,width=1.5)%>%
            width(j=3,width=1.5)%>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              as_paragraph(
                as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans") )
              ),
              align_with_table = TRUE
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )
        })
      })
      ## ======================================================================================== ##
      ## ====== Table Sic. Unweighted summary statistics -- demo + child by retention status ==== ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.attr.retained %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i])) %>%
            mutate(
              #RACE1 = factor(RACE1),
              RACE1 = droplevels(RACE1),
              RACE1 = case_when(is.na(RACE1) ~ "    (Missing)", .default = RACE1),
              RACE1 = factor(RACE1, levels = sort(unique(RACE1))),
              RACE1 = fct_relevel(RACE1, "    (Missing)", after = Inf),
              #INCOME = case_when(INCOME == "(Missing)" ~ "    (Missing)", .default = INCOME),
              #INCOME = factor(INCOME),
              INCOME = droplevels(INCOME),
              INCOME = factor(INCOME, levels = sort(unique(INCOME))),
              INCOME = case_when(is.na(INCOME) ~ "    (Missing)", .default = INCOME),
              INCOME = fct_relevel(INCOME, "    (Missing)", after = Inf),
            )

          sumtab <- temp.dat %>%
            tbl_summary(
              by = WAVE0,
              include = c(
                any_of(focal.predictor0),
                 AGE_GRP, AGE, GENDER, RACE1, MARITAL_STATUS,
                EDUCATION_3, EMPLOYMENT, INCOME,
                ATTEND_SVCS,  BORN_COUNTRY,
                PARENTS_12YRS, SVCS_12YRS, MOTHER_RELATN, FATHER_RELATN,
                OUTSIDER, ABUSED, HEALTH_GROWUP, INCOME_12YRS, REL1
              ),
              type = list(
                AGE ~ "continuous2",
                all_continuous() ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = list(
                AGE ~ "Age of participant",
                AGE_GRP ~ "Year of birth",
                GENDER ~ "Gender",
                RACE1 ~ "Race/ethnicity",
                MARITAL_STATUS ~ "Respondent marital status",
                EMPLOYMENT ~ "Employment status",
                INCOME ~ "Self-reported income",
                ATTEND_SVCS ~ "Current religious service attendance",
                EDUCATION_3 ~ "Education (years)",
                BORN_COUNTRY ~ "Immigration status",
                PARENTS_12YRS ~ "Parental marital status around age 12",
                MOTHER_RELATN ~ "Relationship with mother when growing up",
                FATHER_RELATN ~ "Relationship with father when growing up",
                OUTSIDER ~ "Felt like an outsider in family when growing up",
                ABUSED ~ "Experienced abuse when growing up",
                HEALTH_GROWUP ~ "Self-rated health when growing up",
                INCOME_12YRS ~ "Subjective financial status of family growing up",
                SVCS_12YRS ~ "Religious service attendance around age 12",
                REL1 ~ "Religious affiliation growing up"
              ),
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")
          if(!single.file.num.sequential){
            tb.cap <- paste0("Table S",i+tb.num.shift, letters[tb.num],". Unweighted demographic and childhood variable summary statistics in ", COUNTRY_LABELS[i]," by retention status")
          }
          if(single.file.num.sequential){
            tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Unweighted demographic and childhood variable summary statistics in ", COUNTRY_LABELS[i]," by retention status")
          }
          tb.num <- tb.num + 1
          tbic.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            width(j=2,width=1.5)%>%
            width(j=3,width=1.5)%>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              as_paragraph(
                as_chunk(tb.cap,
                  props = fp_text_default(font.family = "Open Sans")
                )
              ),
              align_with_table = TRUE
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )
        })
      })
      ## ======================================================================================== ##
      ## == Table Sid. Unweighted summary statistics -- outcome variables by retention status === ##
      suppressMessages({
        suppressWarnings({
          temp.dat <- df.raw.attr.retained %>%
            mutate(COUNTRY = str_trim(COUNTRY)) %>%
            filter(str_detect(COUNTRY, COUNTRY_LABELS[i]))

          sumtab <- temp.dat %>%
            tbl_summary(
              by = WAVE0,
              include = c(
                any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
              ),
              type = list(
                all_continuous() ~ "continuous2",
                contains("NUM_CHILDREN") ~ "continuous2",
                contains("CIGARETTES") ~ "continuous2",
                contains("DAYS_EXERCISE") ~ "continuous2"
              ),
              statistic = list(
                all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                all_categorical() ~ "{n} ({p}%)"
              ),
              label = OUTCOME.VEC.LABELS,
              digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
              ),
              missing_text = "    (Missing)",
              missing_stat = "{N_miss} ({p_miss}%)"
            ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ) %>%
            modify_header(label ~ "**Outcome**") %>%
            italicize_labels()

          tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

          if(!single.file.num.sequential){
            tb.cap <- paste0("Table S",i+tb.num.shift,letters[tb.num],". Unweighted outcome variable summary statistics in ", COUNTRY_LABELS[i], " by retention status.")
          }
          if(single.file.num.sequential){
            tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Unweighted outcome variable summary statistics in ", COUNTRY_LABELS[i], " by retention status.")
          }
          tb.num <- tb.num + 1
          tbid.toprint <- sumtab %>%
            as_flex_table() %>%
            autofit() %>%
            width(j=2,width=1.5)%>%
            width(j=3,width=1.5)%>%
            format_flex_table(pg.width = 21 / 2.54 - 2) %>%
            set_caption(
              as_paragraph(
                as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
              ),
              align_with_table = TRUE
            ) %>%
            add_footer_row(
              values = tb.note.summarytab, top = FALSE,colwidths=3
            )

        })
      })
      ## ======================================================================================== ##
      ## ====== Table Sie. Summary of Attrition Model =========================================== ##
      {


        tmp.attr.mod <- get_fitted_attrition_model(dir.attr.models, COUNTRY_LABELS[i])
        tmp.included.vars0 <- attr(tmp.attr.mod$terms,"term.labels")
        tmp.included.vars <- str_remove(tmp.included.vars0, "COV_")
        lab.list <- list()
        for(ii in 1:length(tmp.included.vars)){
        	  ## manually adjust for specific variables
          if(str_detect(tmp.included.vars0[ii], "INCOME")){
          	if(COUNTRY_LABELS[i] %in% c("United States", "Australia")){
          		lab.list[[tmp.included.vars0[ii]]] = "Annual household income"
            	tmp.included.vars[ii] <- "Annual household income"
          	} else {
          		lab.list[[tmp.included.vars0[ii]]] = "Monthly household income"
            	tmp.included.vars[ii] <- "Monthly household income"
          	}
          } else if(str_detect(tmp.included.vars0[ii], "ATTEND_SVCS")){
          	lab.list[[tmp.included.vars0[ii]]] = "Religious service attendance"
            tmp.included.vars[ii] <- "Religious service attendance"
          } else if(str_detect(tmp.included.vars0[ii], "EMPLOYMENT")){
          	lab.list[[tmp.included.vars0[ii]]] = "Employment status"
            tmp.included.vars[ii] <- "Employment status"
          } else {
          	lab.list[[tmp.included.vars0[ii]]] = get_outcome_better_name(tmp.included.vars[ii], include.name = FALSE)
            tmp.included.vars[ii] <- get_outcome_better_name(tmp.included.vars[ii], include.name = FALSE)
          }


        }
        tb.note <- as_paragraph(paste0("_Notes_. N=",country.n1.print,"; attrition weights were estimated using the 'survey::svyglm(family=quasibinomial('logit'))' function. All continuous predictors were standardized and all categorical predictors used the most common category as the reference group. Reported p-values are based on the fitted regression model and no adjustments for multiple testing were done within this table."))

        if(!single.file.num.sequential){
          tb.cap <- paste0("Table S",i+tb.num.shift,letters[tb.num],". Summary of fitted attrition model in ", COUNTRY_LABELS[i])
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Summary of fitted attrition model in ", COUNTRY_LABELS[i])
        }
        tb.num <- tb.num + 1

        attr.fit.toprint <- tbl_regression(
          tmp.attr.mod, exponentiate = TRUE,
          pvalue_fun = function(x) {
            if_else(
              is.na(x),
              NA_character_,
              if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = F))
            )
          },
          label = lab.list
        )  |>
          bold_labels() |>
          italicize_levels() |>
          modify_header(estimate = "**Odds Ratio**") |>
          as_flex_table() |>
         autofit() |>
          format_flex_table(pg.width = 21 / 2.54 - 2) |>
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          add_footer_row(
            values = tb.note, top = FALSE, colwidths=4
          )

      }
      ## ======================================================================================== ##
      ## ====== Table Sif. Country specific PCA Summary ========================================= ##
      {
        coun.fit.pca <- get_country_pca_summary(
          res.dir = dir.primary,
          country = COUNTRY_LABELS[i],
          outcome = OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)][1],
          predictor = focal.predictor[1],
          "_primary_wpc"
        )

        vec.id <- c("prop.var", "Cumulative_Proportion_Explained")
        vec.pc <- c("Percent Explained by each PC", "Cumulative Percent Explained")
        cnames <- c(
          "PC",
          vec.pc
        )

        coun.pca <- as.data.frame(matrix(nrow = 20, ncol = length(cnames)))
        colnames(coun.pca) <- cnames
        tmp.pca <- coun.fit.pca %>%
          dplyr::filter( PC <= 20 ) %>%
          dplyr::select(PC,tidyr::any_of(vec.id)) %>%
          dplyr::mutate(
            across(tidyr::any_of(vec.id),\(x) .round(x*100,1) )
          )
        coun.pca$PC <- 1:20
        coun.pca[vec.pc] <- tmp.pca[vec.id]

        #coun.pca <- na.omit(coun.pca)
        # footnote information:
        tb.note.pca <- as_paragraph("_Notes_.  N=",country.n1.print,"; PCA was conducted using 'survey::svyprcomp(.)' function using all available contemporaneous (with focal predictor) exposures at wave 1. All PCs were standardized prior to being used as predictors. The bolded row represented the number of retained components for analysis was 7.")

        if(!single.file.num.sequential){
          tb.cap <- paste0("Table S", i+tb.num.shift,letters[tb.num],". Summary of principal components in ", COUNTRY_LABELS[i])
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Summary of principal components in ", COUNTRY_LABELS[i])
        }
        tb.num <- tb.num + 1

        coun.pca.toprint <- coun.pca %>%
          flextable() %>%
          #autofit() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          add_footer_row(
            values = tb.note.pca, top = FALSE, colwidths = ncol(coun.pca)
          ) %>%
          format_flex_table(pg.width = 21 / 2.54 - 4) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(coun.pca)) %>%
          bold(i=7,j=1:3) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header")
      }
      ## ======================================================================================== ##
      ## ====== Table Sig-x. Country specific outcome wide results ================================ ##
      tbl.sid.list <- list()
      tbl.sie.list <- list()
      tbl.sif.list <- list()
      f0 <- 1
      for(f0 in 1:length(focal.predictor)){

        vec.id <- c("id.Est","id.CI", "id.SE","p.value")
        vec.rr <- c("rr.Est", "rr.CI", "logrr.SE","p.value")
        vec.wopc <- c("RR", "ES", "95% CI", "SE", "p-value")
        vec.wpc <- c("RR\r", "ES\r", "95% CI\r", "SE\r", "p-value\r")
        cnames <- c(
          "Outcome",
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

          coun.outcomewide[j, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[j], include.name = FALSE, include.fid = TRUE))

          if ( (str_detect(OUTCOME.VEC[j],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
          (str_detect(OUTCOME.VEC[j],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
          (str_detect(OUTCOME.VEC[j],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
          (str_detect(OUTCOME.VEC[j],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
          ) {
          	  coun.outcomewide[j, c(2:6,8:12)] <- "-"
          	  next
          	} else {

            tmp.vec <- case_when(
              get_outcome_scale(OUTCOME.VEC[j]) == "cont" ~ vec.id,
              .default = vec.rr
            )
            tmp.name <- paste0(OUTCOME.VEC[j], "_", focal.predictor[f0])
            ## ====== estimates withOUT PCs ======================================= ##
            coun.wopc <- get_country_specific_regression_results(
              res.dir = dir.primary,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_primary_wopc"
            )

            tmp.wopc <- coun.wopc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(tidyr::any_of(c("p.value")),\(x){
                  case_when(
                  	x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	x < 0.005 ~ paste0(.round_p(x),"**"),
               		x < 0.05 ~ paste0(.round_p(x),"*"),
                	x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wopc) > 0)
              if(get_outcome_scale(OUTCOME.VEC[j]) == "cont"){
                coun.outcomewide[j,vec.wopc[-1]] <- tmp.wopc[tmp.vec]
              }
            if(get_outcome_scale(OUTCOME.VEC[j]) != "cont"){
              coun.outcomewide[j,vec.wopc[-2]] <- tmp.wopc[tmp.vec]
            }

            ## ====== Random effects coun - estimates WITH PCs ======================================= ##
            coun.wpc <- get_country_specific_regression_results(
              res.dir = dir.primary,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_primary_wpc"
            )

            tmp.wpc <- coun.wpc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(tidyr::any_of(c("p.value")),\(x){
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                	x < 0.005 ~ paste0(.round_p(x),"**"),
               		x < 0.05 ~ paste0(.round_p(x),"*"),
                	x > 0.05 ~ .round_p(x)
                  )
                })
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wpc) > 0)
              if(get_outcome_scale(OUTCOME.VEC[j]) == "cont"){
                coun.outcomewide[j,vec.wpc[-1]] <- tmp.wpc[tmp.vec]
              }
            if(get_outcome_scale(OUTCOME.VEC[j]) != "cont"){
              coun.outcomewide[j,vec.wpc[-2]] <- tmp.wpc[tmp.vec]
            }
          	}

          }
        }
        #coun.outcomewide <- na.omit(coun.outcomewide)

        # footnote information:
        tb.note.coun.outcomewide <-as_paragraph(paste0("_Notes_. Reference for focal predictor: ", focal.predictor.reference.value,". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero;  ^(a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; ^(b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; ^(c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; ^(d) item part of the Character & Virtue domain of the Secure Flourishing Index; ^(e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; ^(f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a B, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold."))

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S",i+tb.num.shift,letters[tb.num],". Associations of _", focal.better.name[f0] ,"_ with adult well-being and other outcomes at wave 2 in ", COUNTRY_LABELS[i])
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Associations of _", focal.better.name[f0] ,"_ with adult well-being and other outcomes at wave 2 in ", COUNTRY_LABELS[i])
        }
        tb.num <- tb.num + 1

        coun.outcomewide.toprint <- coun.outcomewide %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          #uncomment when using all outcomes
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Model 1: Demographic and Childhood Variables as Controls", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounders (via principal components) as Controls"),
            colwidths = c(1,length(vec.wopc), 1, length(vec.wpc))
          ) %>%
          add_footer_row(
            values = tb.note.coun.outcomewide, top = FALSE, colwidths = ncol(coun.outcomewide)
          ) %>%
          theme_meta_outcome_wide()

        tbl.sid.list[[f0]] <- coun.outcomewide.toprint

        ## Results based on complete-case analysis w/ attrition weights

        vec.id <- c("id.Est","id.CI", "id.SE","p.value")
        vec.rr <- c("rr.Est", "rr.CI", "logrr.SE","p.value")
        vec.wopc <- c("RR", "ES", "95% CI", "SE", "p-value")
        vec.wpc <- c("RR\r", "ES\r", "95% CI\r", "SE\r", "p-value\r")
        cnames <- c(
          "Outcome",
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

            coun.outcomewide[j, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[j], include.name = FALSE, include.fid = TRUE))

            if ( (str_detect(OUTCOME.VEC[j],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
                 (str_detect(OUTCOME.VEC[j],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
                 (str_detect(OUTCOME.VEC[j],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
                 (str_detect(OUTCOME.VEC[j],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
            ) {
              coun.outcomewide[j, c(2:6,8:12)] <- "-"
              next
            } else {

              tmp.vec <- case_when(
                get_outcome_scale(OUTCOME.VEC[j]) == "cont" ~ vec.id,
                .default = vec.rr
              )
              tmp.name <- paste0(OUTCOME.VEC[j], "_", focal.predictor[f0])
              ## ====== estimates withOUT PCs ======================================= ##
              coun.wopc <- get_country_specific_regression_results(
                res.dir = dir.supp,
                country = COUNTRY_LABELS[i],
                predictor = focal.predictor[f0],
                outcome =  OUTCOME.VEC[j],
                appnd.txt = "_cca_wopc"
              )

              tmp.wopc <- coun.wopc %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(tidyr::any_of(c("p.value")),\(x){
                    case_when(
                      x < p.bonferroni ~ paste0(.round_p(x),"***"),
                      x < 0.005 ~ paste0(.round_p(x),"**"),
                      x < 0.05 ~ paste0(.round_p(x),"*"),
                      x > 0.05 ~ .round_p(x)
                    )
                  })
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wopc) > 0)
                if(get_outcome_scale(OUTCOME.VEC[j]) == "cont"){
                  coun.outcomewide[j,vec.wopc[-1]] <- tmp.wopc[tmp.vec]
                }
              if(get_outcome_scale(OUTCOME.VEC[j]) != "cont"){
                coun.outcomewide[j,vec.wopc[-2]] <- tmp.wopc[tmp.vec]
              }

              ## ====== Random effects coun - estimates WITH PCs ======================================= ##
              coun.wpc <- get_country_specific_regression_results(
                res.dir = dir.supp,
                country = COUNTRY_LABELS[i],
                predictor = focal.predictor[f0],
                outcome =  OUTCOME.VEC[j],
                appnd.txt = "_cca_wpc"
              )

              tmp.wpc <- coun.wpc %>%
                dplyr::select(tidyr::any_of(tmp.vec)) %>%
                dplyr::mutate(
                  dplyr::across(tidyr::any_of(c("p.value")),\(x){
                    case_when(
                      x < p.bonferroni ~ paste0(.round_p(x),"***"),
                      x < 0.005 ~ paste0(.round_p(x),"**"),
                      x < 0.05 ~ paste0(.round_p(x),"*"),
                      x > 0.05 ~ .round_p(x)
                    )
                  })
                )
              ## ====== Add Results to output object ====================================================== ##
              if(nrow(tmp.wpc) > 0)
                if(get_outcome_scale(OUTCOME.VEC[j]) == "cont"){
                  coun.outcomewide[j,vec.wpc[-1]] <- tmp.wpc[tmp.vec]
                }
              if(get_outcome_scale(OUTCOME.VEC[j]) != "cont"){
                coun.outcomewide[j,vec.wpc[-2]] <- tmp.wpc[tmp.vec]
              }
            }

          }
        }
        #coun.outcomewide <- na.omit(coun.outcomewide)

        # footnote information:
        tb.note.coun.outcomewide <-as_paragraph(paste0("_Notes_. Reference for focal predictor: ", focal.predictor.reference.value,". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero;  ^(a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; ^(b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; ^(c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; ^(d) item part of the Character & Virtue domain of the Secure Flourishing Index; ^(e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; ^(f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a B, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold."))

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S",i+tb.num.shift,letters[tb.num],". Associations of _", focal.better.name[f0] ,"_ with adult well-being and other outcomes at wave 2 in ", COUNTRY_LABELS[i], " using complete-case analyses with attrition weights.")
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Associations of _", focal.better.name[f0] ,"_ with adult well-being and other outcomes at wave 2 in ", COUNTRY_LABELS[i], " using complete-case analyses with attrition weights.")
        }
        tb.num <- tb.num + 1

        coun.outcomewide.toprint <- coun.outcomewide %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          #uncomment when using all outcomes
          italic(part = "body",
                 i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
                 j = 1) %>%
          add_header_row(
            values = c("", "Model 1: Demographic and Childhood Variables as Controls", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounders (via principal components) as Controls"),
            colwidths = c(1,length(vec.wopc), 1, length(vec.wpc))
          ) %>%
          add_footer_row(
            values = tb.note.coun.outcomewide, top = FALSE, colwidths = ncol(coun.outcomewide)
          ) %>%
          theme_meta_outcome_wide()

        tbl.sie.list[[f0]] <- coun.outcomewide.toprint

        ## ======================================================================================== ##
        ## ====== Table Sie. Country Specific E-values output table =============================== ##

        vec.id <- c("EE", "ECI")
        vec.wopc <- c("E-values", "E-values for CI")
        vec.wpc <- c("E-values\r", "E-values for CI\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
        vec.wopc2 <- c("E-values\r\r", "E-values for CI\r\r")
        vec.wpc2 <- c("E-values\r\r\r", "E-values for CI\r\r\r")
        cnames <- c(
          "Outcome",
          vec.wopc, "\r",
          vec.wpc, "\r\r",
          vec.wopc2, "\r\r\r",
          vec.wpc2
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
           if ( (str_detect(OUTCOME.VEC[j],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
          (str_detect(OUTCOME.VEC[j],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
          (str_detect(OUTCOME.VEC[j],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
          (str_detect(OUTCOME.VEC[j],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
          ) {
             coun.evalues[j, c(2:3,5:6, 8:9, 11:12)] <- "-"
          	  next
          	} else {
            tmp.vec <- vec.id
            tmp.name <- paste0(OUTCOME.VEC[j], "_", focal.predictor[f0])
            ## ====== estimates withOUT PCs ======================================= ##
            coun.wopc <- get_country_specific_regression_results(
              res.dir = dir.primary,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_primary_wopc"
            )

            tmp.wopc <- coun.wopc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wopc) > 0) coun.evalues[j,vec.wopc] <- tmp.wopc[tmp.vec]

            ## ====== estimates WITH PCs ======================================= ##
            coun.wpc <- get_country_specific_regression_results(
              res.dir = dir.primary,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_primary_wpc"
            )
            tmp.wpc <- coun.wpc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wpc) > 0) coun.evalues[j,vec.wpc] <- tmp.wpc[tmp.vec]


            ## ====== estimates withOUT PCs ======================================= ##
            coun.wopc <- get_country_specific_regression_results(
              res.dir = dir.supp,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_cca_wopc"
            )

            tmp.wopc <- coun.wopc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wopc) > 0) coun.evalues[j,vec.wopc2] <- tmp.wopc[tmp.vec]

            ## ====== estimates WITH PCs ======================================= ##
            coun.wpc <- get_country_specific_regression_results(
              res.dir = dir.supp,
              country = COUNTRY_LABELS[i],
              predictor = focal.predictor[f0],
              outcome =  OUTCOME.VEC[j],
              appnd.txt = "_cca_wpc"
            )
            tmp.wpc <- coun.wpc %>%
              dplyr::select(tidyr::any_of(tmp.vec)) %>%
              dplyr::mutate(
                dplyr::across(where(is.numeric),\(x) .round(x,2)),
              )
            ## ====== Add Results to output object ====================================================== ##
            if(nrow(tmp.wpc) > 0) coun.evalues[j,vec.wpc2] <- tmp.wpc[tmp.vec]

		  }
          }
        }
        #coun.evalues <- na.omit(coun.evalues)


        # footnote information:
        tb.note.evalues <-as_paragraph("_Notes_. EE, E-value for Estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for Estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S", i+tb.num.shift,letters[tb.num],". Sensitivity analysis of ", focal.better.name[f0] ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[i])
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num+tb.num.shift,". Sensitivity analysis of ", focal.better.name[f0] ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[i])
        }
        tb.num <- tb.num + 1

        coun.evalues.toprint <- coun.evalues %>%
          flextable() %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap,props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          add_header_row(
            values = c("", "Model 1: Demographics and Childhood Variables as Controls", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (via principal components) as Controls", "", "Model 1: Demographics and Childhood Variables as Controls", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (via principal components) as Controls"),
            colwidths = c(1, 2, 1, 2, 1, 2, 1, 2),
            top = TRUE
          ) %>%
          add_header_row(
            values = c("", "Multiple Imputation", "", "Complete Case w/ Attrition Weights" ),
            colwidths = c(1, 2+1+2, 1, 2+1+2),
            top = TRUE
          ) %>%
          add_footer_row(
            values = tb.note.evalues, top = FALSE, colwidths = ncol(coun.evalues)
          ) %>%
          width(j=1,width=2.5)%>%
          format_flex_table(pg.width = 29.7/2.54 - 2) %>%
          align(i = 1, j = NULL, align = "center", part = "header") %>%
          align(part = "footer", align = "left", j = 1:ncol(coun.evalues)) %>%
          border_remove()  %>%
          hline_bottom(part = "body") %>%
          hline_top(part = "header") %>%
          hline_bottom(part = "header") %>%
          hline(i=1,j=c(2:6,8:12), part="header") %>%
          hline(i=2,j=c(2:3,5:6,8:9,11:12), part="header")

        tbl.sif.list[[f0]] <-  coun.evalues.toprint

      }
      ## ======================================================================================== ##
      ## ====== Print out tables to formatted Word document ===================================== ##
      {

        supp_doc <- supp_doc |>
          body_add_flextable(value = tbia.toprint) |>
          body_add_break() |>
          body_add_flextable(value = tbib.toprint) |>
          body_add_break() |>
          body_add_flextable(value = tbic.toprint) |>
          body_add_break() |>
          body_add_flextable(value = tbid.toprint) |>
          body_add_break() |>
          body_add_flextable(value = attr.fit.toprint) |>
          body_add_break() |>
          body_add_flextable(value = coun.pca.toprint) |>
          body_end_block_section(value = normal_portrait) |>
          body_add_break()

        for(f0 in 1:length(focal.predictor)){
          supp_doc <- supp_doc |>
            body_add_flextable(value = tbl.sid.list[[f0]]) |>
            body_end_block_section(value = landscape_one_column) |>
            body_add_break() |>
            body_add_flextable(value = tbl.sie.list[[f0]]) |>
            body_end_block_section(value = landscape_one_column) |>
            body_add_break() |>
            body_add_flextable(value = tbl.sif.list[[f0]]) |>
            body_end_block_section(value = landscape_one_column) |>
            body_add_break()
        }

        ## Print out tables for current country
        print(supp_doc, target = out.file)
      }
    }

  }
  ## ============================================================================================== ##
  # Supplement 3:
  # (1) Summary statistics of demographics by country & wave (raw data)
  #	(2) Summary statistics of OUTCOMES by country & wave (raw data)
  #	(3) Forest plots of all effects
  #     - Model 1 (No PCs)
  #     - Model 2 (w/ PCs)
  ## ============================================================================================== ##
  if(what == "all" | what == "S3"){

    if(single.file.num.sequential){
      out.file <- here::here(res.dir,paste0("GFS-Wave 2 Online Supplement_", paste0(focal.better.name, collapse=" "),".docx"))
    } else {
      # initialize objects as needed
      tb.num <- 1
      out.file <- here::here(res.dir,paste0("GFS-S3 Online Supplement Part 3_", paste0(focal.better.name, collapse=" "),".docx"))
      supp_doc <- read_docx() |>
        body_add_fpar(fpar(ftext("GFS Online Supplement Part 3", gfs_title1_prop)), style="centered") %>%
        #body_add_par("...general caveats...")
        body_end_section_continuous() %>%
        body_add_break() |>
        print(target=out.file)
    }



    ## ========================================================================================== ##
    ## ====== Table S1. summary statistics -- demographics variables by country ================= ##
    suppressMessages({
      suppressWarnings({

        sumtab <- df.raw.long %>%
          select(!any_of(c("RACE1", "RACE2", "INCOME"))) %>%
          as_survey_design(
            ids = {{psu}},
            strata = {{strata}},
            weights = {{wgt}}
          ) %>%
          tbl_strata(
            strata = COUNTRY,
            .tbl_fun = ~ .x |>
              tbl_svysummary(
                by = WAVE0,
                include = c(
                  any_of(focal.predictor0),
                  any_of(baseline.pred0)
                ),
                type = list(
                  all_continuous() ~ "continuous2"
                ),
                statistic = list(
                  all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                  all_categorical() ~ "{n} ({p}%)"
                ),
                label = list(
                  AGE_GRP ~ "Year of birth",
                  GENDER ~ "Gender",
                  MARITAL_STATUS ~ "Respondent marital status",
                  EMPLOYMENT ~ "Employment status",
                  ATTEND_SVCS ~ "Current religious service attendance",
                  EDUCATION_3 ~ "Education (years)",
                  BORN_COUNTRY ~ "Immigration status",
                  PARENTS_12YRS ~ "Parental marital status around age 12",
                  MOTHER_RELATN ~ "Relationship with mother when growing up",
                  FATHER_RELATN ~ "Relationship with father when growing up",
                  OUTSIDER ~ "Felt like an outsider in family when growing up",
                  ABUSED ~ "Experienced abuse when growing up",
                  HEALTH_GROWUP ~ "Self-rated health when growing up",
                  INCOME_12YRS ~ "Subjective financial status of family growing up",
                  SVCS_12YRS ~ "Religious service attendance around age 12",
                  REL1 ~ "Religious affiliation growing up"
                ),
                digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
                ),
                missing_text = "    (Missing)",
                missing_stat = "{N_miss} ({p_miss}%)"
              ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ),
            .header = "**{strata}**"
          ) %>%
          italicize_labels()

        tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S1. Weighted summary statistics of demographic and childhood variables data across countries.")
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num,". Weighted summary statistics of demographic and childhood variables data across countries.")
        }
        tb.num <- tb.num + 1

        tbia.toprint <- sumtab %>%
          as_flex_table() %>%
          autofit() %>%
          format_flex_table(pg.width = 29.7 / 2.54 * 2 - 1) %>%
          set_caption(
            as_paragraph(
              as_chunk( tb.cap, props = fp_text_default(font.family = "Open Sans") )
            ),
            align_with_table = TRUE
          ) %>%
          add_footer_row(
            values = tb.note.summarytab, top = FALSE,colwidths=23+24
          )
      })
    })
    ## ========================================================================================== ##
    ## ====== Table S2. summary statistics -- outcome variables by country ====================== ##
    suppressMessages({
      suppressWarnings({
        sumtab <- df.raw.long %>%
          as_survey_design(
            ids = {{psu}},
            strata = {{strata}},
            weights = {{wgt}}
          ) %>%
          tbl_strata(
            strata = COUNTRY,
            .tbl_fun = ~ .x |>
              tbl_svysummary(
                by = WAVE0,
                include = c(
                  any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
                ),
                label = OUTCOME.VEC.LABELS,
                type = list(
                  all_continuous() ~ "continuous2",
                  contains("NUM_CHILDREN") ~ "continuous2",
                  contains("CIGARETTES") ~ "continuous2",
                  contains("DAYS_EXERCISE") ~ "continuous2"
                ),
                statistic = list(
                  all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {max}"),
                  all_categorical() ~ "{n} ({p}%)"
                ),
                digits = list(
                all_continuous() ~ 1,
                n = label_style_number(digits=0),
                p = label_style_percent(digits=1)
                ),
                missing_text = "    (Missing)",
                missing_stat = "{N_miss} ({p_miss}%)"
              ) %>%
            add_stat_label(
              label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
            ),
            .header = "**{strata}**"
          ) %>%
          modify_header(label ~ "**Outcome**") %>%
            italicize_labels()


        tb.note.summarytab <- as_paragraph("_Note._ N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding.")

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S2. Weighted summary statistics of demographic and childhood variables data across countries.")
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num,". Weighted summary statistics of demographic and childhood variables data across countries.")
        }
        tb.num <- tb.num + 1

        tbib.toprint <- sumtab %>%
          as_flex_table() %>%
          autofit() %>%
          format_flex_table(pg.width = 29.7 / 2.54 * 2 - 1) %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          add_footer_row(
            values = tb.note.summarytab, top = FALSE,colwidths=23+24
          )
      })
    })
    ## ========================================================================================== ##
    ## ====== Table S3. Reformatted outcome-wide results by country ============================= ##
    suppressMessages({
      suppressWarnings({

        vec.get <- c("OUTCOME0", "FOCAL_PREDICTOR", "theta.rma", "rr.theta")

        df.tmp <- get_country_specific_output(
          res.dir = dir.primary,
          outcomes =  OUTCOME.VEC[str_detect(OUTCOME.VEC,"blank",negate=TRUE)],
          predictors = focal.predictor,
          appnd.txt = "_primary_wpc"
        ) %>%
          filter(Variable == "FOCAL_PREDICTOR") %>%
          select(COUNTRY, OUTCOME, id.Est, rr.Est)
        for(i in 1:nrow(df.tmp)){
          if(get_outcome_scale(df.tmp$OUTCOME[i]) == "cont"){
            df.tmp$rr.Est[i] <- NA
          }
          if(get_outcome_scale(df.tmp$OUTCOME[i]) != "cont"){
            df.tmp$id.Est[i] <- NA
          }
          df.tmp$OUTCOME[i] <- get_outcome_better_name(df.tmp$OUTCOME[i], FALSE, FALSE, include.fid = TRUE)
        }
        df.tmp <- df.tmp %>%
          pivot_wider(
            names_from = COUNTRY,
            values_from = c(id.Est, rr.Est)
          )
        df.tmp <- df.tmp[, c(1, c(rbind(2:24, 25:47)) )]

        header_l1 <- sort(unique(str_split(colnames(df.tmp), "_",simplify = T)[-1,2]))
        colnames(df.tmp) <- c("Outcome", rep(c("ES", "RR"), length(header_l1)))
        for(i in 2:length(colnames(df.tmp))){
          colnames(df.tmp)[i] <- paste0(colnames(df.tmp)[i], paste0(rep("\r",i-2), collapse = ""))
        }


        tb.note.outcomewide <-as_paragraph(paste0("_Notes_. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; Self-rated feelings about income growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. For Models with PC (principal components), the first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a ES, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model."))

        if(!single.file.num.sequential){
          tb.cap <-  paste0("Table S3. Model 2 outcome-wide results--point estimates of effect sizes only--re-structured for comparison across countries.")
        }
        if(single.file.num.sequential){
          tb.cap <- paste0("Table S",tb.num,". Model 2 outcome-wide results--point estimates of effect sizes only--re-structured for comparison across countries.")
        }
        tb.num <- tb.num + 1

        tb.outcomewide <- df.tmp %>%
          flextable() %>%
          autofit() %>%
          width(j=1,3) %>%
          format_flex_table(pg.width = 29.7 / 2.54 * 2 - 1) %>%
          set_caption(
            as_paragraph(
              as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
            ),
            align_with_table = TRUE
          ) %>%
          add_header_row(
            values = c("", header_l1),
            colwidths = c(1, rep(2, length(header_l1)))
          ) %>%

          bg(
            j = c(2 + seq(0,ncol(df.tmp),4), 3 + seq(0,ncol(df.tmp),4)), bg="grey90",part = "all"
          ) %>%
          add_footer_row(
            values = tb.note.outcomewide, top = FALSE,colwidths=ncol(df.tmp)
          )

      })
    })
    ## ========================================================================================== ##
    ## ====== Print out to file ======================== ##
    supp_doc <- read_docx(out.file)
    supp_doc <- supp_doc |>
      body_add_flextable(tbia.toprint) |>
      body_add_break() |>
      body_add_flextable(tbib.toprint) |>
      body_add_break() |>
      body_add_flextable(tb.outcomewide) |>
      body_end_block_section(value = extra_wide_landscape)|>
      body_add_break()

    print(supp_doc, target = out.file)
    # create comparable files but as CSV
    tmp.file <- here::here(res.dir,paste0("GFS-S3_Outcomewide_Results_Comparison_", paste0(focal.better.name, collapse=" "),".csv"))
    readr::write_csv(df.tmp, file=tmp.file)

    try({
      tmp.file <- here::here(res.dir,paste0("GFS-S3_Country_Comparison_", paste0(focal.better.name, collapse=" "),".csv"))
      readr::write_csv(sumtab, file=tmp.file)
    })
    ## ========================================================================================== ##
    ## ====== Supplemental Forest plots ========================================================= ##

    tmp.vec <- OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)]
    fig.num <- 1
    i = 1
    for(i in  1:length(tmp.vec)){

      fp.wopc <- load_meta_result(
        file = here::here(dir.primary, "0_meta_analyzed_results_primary_wopc.rds"),
        predictor = focal.predictor,
        outcome = tmp.vec[i],
        what = c("OUTCOME0", "FOCAL_PREDICTOR", "forest.plot")
      )

      fp.wpc <- load_meta_result(
        file = here::here(dir.primary, "0_meta_analyzed_results_primary_wpc.rds"),
        predictor = focal.predictor,
        outcome = tmp.vec[i],
        what = c("OUTCOME0", "FOCAL_PREDICTOR", "forest.plot")
      )

      if(tmp.vec[i] %in% unique(fp.wopc$OUTCOME0) & tmp.vec[i] %in% unique(fp.wpc$OUTCOME0) ) {

        f0 <- 1
        for(f0 in 1:length(focal.predictor)){
          supp_doc <- read_docx(out.file)

          myvar0.bn <- get_outcome_better_name(tmp.vec[i], include.name = FALSE)

          tmp.bn = fpar(ftext(paste0("Figure S",fig.num,". Heterogeneity in the effects of ", focal.better.name[f0] ," on ", myvar0.bn ," scores across countries. (A) without controlling for PCs (left); (B) controlling for PCs (right); N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country. Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects. Squares represent the point estimate for each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing."), fp_text(font.size = 11, font.family = "Open Sans")))

          ## ============================================================================================ ##
          ## ====== Forest plot for Secure Flourishing Index ============================================ ##
          {
            p1 <- fp.wopc %>% ungroup() %>%
              filter(OUTCOME0 == tmp.vec[i]) %>%
              select(forest.plot)
            p1 <- p1[[1]][[1]]
            ggsave(
              filename = here::here(res.dir, "fig", paste0("figure_s",fig.num,"_", tmp.vec[i],"_regressed_on_", focal.predictor[f0], "_wopc.png")),
              plot=p1, units="in", width=6, height=5, dpi = 1000
            )

            p2 <- fp.wpc %>% ungroup() %>%
              filter(OUTCOME0 == tmp.vec[i]) %>%
              select(forest.plot)
            p2 <- p2[[1]][[1]]
            ggsave(
              filename = here::here(res.dir, "fig", paste0("figure_s",fig.num,"_", tmp.vec[i],"_regressed_on_", focal.predictor[f0], "_wpc.png")),
              plot=p2, units="in", width=6, height=5, dpi = 1000
            )
          }

          supp_doc <- supp_doc %>%
            body_add_fpar(tmp.bn, style = "centered") |>
            body_add_par(value = "", style = "centered") |>
            body_add_par(value = "", style = "centered") |>
            body_add_par(value = "", style = "centered") |>
            body_end_block_section(value = landscape_one_column) |>
            body_add_img(
              src = here::here(res.dir, "fig", paste0("figure_s",fig.num,"_", tmp.vec[i],"_regressed_on_", focal.predictor[f0], "_wopc.png")),
              height = 4, width = 4.8
            ) %>%
            body_add_img(
              src = here::here(res.dir,"fig", paste0("figure_s",fig.num,"_", tmp.vec[i],"_regressed_on_", focal.predictor[f0], "_wpc.png")),
              height = 4, width = 4.8
            ) |>
            body_end_block_section(value = landscape_two_columns) |>
            body_add_break()

          print(supp_doc,  target = out.file)

          fig.num = fig.num + 1

        }

      }
    }
  }

}
