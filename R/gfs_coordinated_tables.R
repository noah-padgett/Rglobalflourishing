#' Construct GFS Main text results
#'
#' Generated a word document containing the results for the meta-analytic outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param dir.meta subdirectory where primary results are stored ('results-primary')
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results without principal components
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results with principal components included
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
gfs_generate_main_doc <- function(df.raw=NULL, dir.meta = "results-primary", file.wopc = "0_meta_analyzed_results_primary_wopc.rds", file.wpc = "0_meta_analyzed_results_primary_wpc.rds", focal.better.name="Focal Predictor", focal.predictor.reference.value="estimated population mean of focal predictor", focal.predictor=NULL, p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL, res.dir = "results", wgt = WGT0, wgt1 = ANNUAL_WEIGHT_R2, wgt2 = AVG.SAMP.ATTR.WGT, psu = PSU, strata = STRATA, ci.bonferroni = FALSE){
  cat("\n **Starting...**\n")
  run.start.time <- Sys.time()

  n.print = df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()
  if (!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    set_flextable_defaults(font.family = "Open Sans",font.size = 10)

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
  ## VECTOR OF BASELINE VARIABLES
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

  n.print <- df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()


  ## ============================================================================================ ##
  ## ====== Construct main text data summary table ============================================== ##

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

  remove(df.raw, df.w1, df.w2)
  gc()

  ## =============================================================================== ##
  ## =============================================================================== ##
  ## Main text table 1
  tb.num <- 1
  params.tb1 <- list(
    df.raw.long = df.raw.long,
    focal.predictor0 = focal.predictor0,
    wgt = as.name("WGT0"),
    psu = as.name("PSU"),
    strata = as.name("STRATA"),
    tb.num = tb.num,
    cache.file = here::here(res.dir, "main-text", paste0("cache-tb-sumtb.RData")),
    start.time = run.start.time
  )
  rmarkdown::render(
    input = system.file("rmd", "main_text_1_summary_table.Rmd", package = "Rglobalflourishing"),
    output_format = c("pdf_document","word_document"),
    output_file = "main_text_tbl_1",
    output_dir = here::here(res.dir, "main-text"),
    params = params.tb1
  )
  tb.num <- tb.num + 1
  remove(params.tb1)
  gc()
  ## =============================================================================== ##
  ## =============================================================================== ##
  ## Main text meta-analytic outcome-wide
  f0=1
  for(f0 in 1:length(focal.predictor)){

    params.tb2 <- list(
      OUTCOME.VEC = OUTCOME.VEC,
      MYLABEL = MYLABEL,
      focal.predictor = focal.predictor[f0],
      focal.better.name = focal.better.name[f0],
      focal.predictor.reference.value = focal.predictor.reference.value[f0],
      dir = dir.meta ,
      file.wopc = file.wopc,
      file.wpc = file.wpc,
      ci.bonferroni =ci.bonferroni,
      p.bonferroni = p.bonferroni,
      tb.num = tb.num,
      n.print = n.print,
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")),
      start.time = run.start.time
    )

    rmarkdown::render(
      input = system.file("rmd", "main_text_2_meta_results.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document","word_document"),
      output_file = paste0("main_text_tbl_",tb.num),
      output_dir = here::here(res.dir, "main-text"),
      params = params.tb2
    )
    tb.num <- tb.num + 1
  }
  remove(params.tb2)
  gc()
  ## =============================================================================== ##
  ## =============================================================================== ##
  ## Main text E-values
  f0=1
  for(f0 in 1:length(focal.predictor)){

    params.tb3 <- list(
      OUTCOME.VEC = OUTCOME.VEC,
      MYLABEL = MYLABEL,
      focal.predictor = focal.predictor[f0],
      focal.better.name = focal.better.name[f0],
      dir = dir.meta ,
      file.wopc = file.wopc,
      file.wpc = file.wpc,
      tb.num = tb.num,
      n.print = n.print,
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-evalues-",f0,".RData")),
      start.time = run.start.time
    )

    rmarkdown::render(
      input = system.file("rmd", "main_text_3_evalues.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document","word_document"),
      output_file = paste0("main_text_tbl_",tb.num),
      output_dir = here::here(res.dir, "main-text"),
      params = params.tb3
    )
    tb.num <- tb.num + 1
  }
  remove(params.tb3)
  gc()
  ## =============================================================================== ##
  ## =============================================================================== ##
  ## Main text figures
  if("COMPOSITE_FLOURISHING_SECURE_Y2" %in% OUTCOME.VEC){
    f0=1
    for(f0 in 1:length(focal.predictor)){

      params.fig <- list(
        OUTCOME.VEC = OUTCOME.VEC,
        MYLABEL = MYLABEL,
        focal.predictor = focal.predictor[f0],
        focal.better.name = focal.better.name[f0],
        dir = dir.meta ,
        file.wopc = file.wopc,
        file.wpc = file.wpc,
        fig.num = f0,
        res.dir = res.dir,
        n.print = n.print,
        cache.file = here::here(res.dir, "main-text", paste0("cache-fig-",f0,".RData")),
        start.time = run.start.time
      )
      rmarkdown::render(
        input = system.file("rmd", "main_text_4_figures.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = paste0("main_text_figures"),
        output_dir = here::here(res.dir, "main-text"),
        params = params.fig
      )
    }
    remove(params.fig)
    gc()
  }


  ## Word version
  out.file <- here::here(res.dir, paste0("GFS Main Text Tables_", paste0(focal.better.name, collapse=" "), ".docx"))
  main.text.docx <- list.files(here::here(res.dir, "main-text"),full.names = TRUE)
  main.text.docx <- main.text.docx[str_detect( main.text.docx, ".docx")]
  # make sure ordered correctly
  main.text.docx <- c(
    main.text.docx[str_detect( main.text.docx, "figures", negate=TRUE)],
    main.text.docx[str_detect( main.text.docx, "figures")]
  )
  main_doc <- read_docx()
  print(main_doc, target=out.file)
  i = 1
  for(i in 1:length(main.text.docx)){
    tmp_doc <- read_docx(main.text.docx[i])
    sec.prop <- tmp_doc$sect_dim
    if(length(sec.prop$page) == 2){
      ps <- prop_section(
        page_size = page_size(
          orient = ifelse(sec.prop$landscape, "landscape", "portrait"),
          width = sec.prop$page[1]/1440,
          height = sec.prop$page[2]/1440,
          unit = "in"
        )
      )
    } else {
      ps <- prop_section(
        page_size = page_size(
          orient = "portrait"
        )
      )
    }
    main_doc <- read_docx(path = out.file) |>
      body_add_docx(main.text.docx[i]) |>
      body_end_block_section(value = block_section(ps))

    print(main_doc, target=out.file)
  }

  ## PDF version
  out.file <- here::here(res.dir, paste0("GFS Main Text Tables_", paste0(focal.better.name, collapse=" "), ".pdf"))
  main.text.pdf <- list.files(here::here(res.dir, "main-text"),full.names = TRUE)
  main.text.pdf <- main.text.pdf[str_detect( main.text.pdf, ".pdf")]
  # make sure ordered correctly
  main.text.pdf <- c(
    main.text.pdf[str_detect( main.text.pdf, "figures", negate=TRUE)],
    main.text.pdf[str_detect( main.text.pdf, "figures")]
  )
  qpdf::pdf_combine(input = main.text.pdf, output=out.file)
  cat("\n **Complete.**\n")
}


#' Construct GFS Online Supplement results
#'
#' Generated two word documents containing the supplemental results for the outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
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
    df.raw = NULL,
    focal.predictor = NULL, focal.better.name="Focal Predictor",
    focal.predictor.reference.value="estimated population mean of focal predictor",
    dir.primary="results-primary", dir.supp="results-cca", dir.attr.models = "results-attr",
    file.primary.wopc = "0_meta_analyzed_results_primary_wopc.rds",
    file.primary.wpc = "0_meta_analyzed_results_primary_wpc.rds",
    file.unstd.wopc = "0_meta_analyzed_results_unstd_wopc.rds",
    file.unstd.wpc = "0_meta_analyzed_results_unstd_wpc.rds",
    file.cca.wopc = "0_meta_analyzed_results_cca_wopc.rds",
    file.cca.wpc = "0_meta_analyzed_results_cca_wpc.rds",
    p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL,
    wgt = WGT0, wgt1 = ANNUAL_WEIGHT_R2, wgt2 = AVG.SAMP.ATTR.WGT, psu = PSU, strata = STRATA,
    # wgt = as.name("WGT0"); wgt1 =  as.name("ANNUAL_WEIGHT_R2"); wgt2 = as.name("AVG.SAMP.ATTR.WGT"); psu =  as.name("PSU"); strata =  as.name("STRATA");
    res.dir = "results", included.countries=NULL,
    ci.bonferroni = FALSE, num.sequential = FALSE, what = "all"){

  cat("\n **Starting...**\n")
  run.start.time <- Sys.time()
  focal.predictor0 <- str_remove(focal.predictor,"_Y1")

  if(!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  if(!dir.exists(here::here(res.dir, "fig"))){
    dir.create(here::here(res.dir, "fig"))
  }
  if(!dir.exists(here::here(res.dir, "supplement-text"))){
    dir.create(here::here(res.dir, "supplement-text"))
  }
  ## ============================================================================================ ##
  ## ====== INTERNAL VECTORS FOR PRINTING ======================================================= ##
  ## Initialize internal word document formatting functions
  {
    tmp.file <- here::here(res.dir,"tmp_doc.docx")
    tmp.file.pdf <- here::here(res.dir,"tmp_doc.pdf")
    tmp.file.pdf2 <- here::here(res.dir,"tmp_doc2.pdf")
    # text formatting
    set_flextable_defaults(font.family = "Open Sans",font.size = 10)
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
  ## DEFINE VARIABLE NAME VECTORS
  {
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
          OUTCOME.VEC.LABELS[[OUTCOME.VEC00[i]]] <- "Community participation"
        }
        if(OUTCOME.VEC00[i] == "ATTEND_SVCS" ){
          OUTCOME.VEC.LABELS[[OUTCOME.VEC00[i]]] <- "Religious attendance"
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
          "{{wgt}}" := n() * {{wgt1}} / sum( {{wgt1}} )
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
          UNITWGT = 1,
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
  out.file.docx <- paste0("GFS_Wave_2_Online_Supplement_", paste0(focal.predictor, collapse="_"),".docx")
  out.file.pdf <- paste0("GFS_Wave_2_Online_Supplement_", paste0(focal.predictor, collapse="_"),".pdf")
  tb.num <- 1
  fig.num <- 1
  ## ============================================================================================ ##
  # Supplement 1:
  #	(1) Summary statistics of OUTCOMES by wave (raw data)
  # ========================= #
  if(what == "all" | what == "S1"){
    cat("Starting part 1 - supplemental meta-analysis results\n")
    ## ========================================================================================== ##
    ## ====== Construct summary tables ========================================================== ##
    {
      ## S1. Supplemental summary of sample demographics by wave
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.long,
        focal.predictor0 = focal.predictor0,
        wgt = as.name("WGT0"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.num = tb.num,
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_sample_by_x.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = "supplement_tbl_1",
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## Table S2. summary of outcomes by wave
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.long,
        OUTCOME.VEC0 = OUTCOME.VEC0,
        OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
        wgt = as.name("WGT0"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.num = tb.num,
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for outcome variables by Wave."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_outcomes_by_x.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = "supplement_tbl_2",
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## S3. Supplemental summary of sample demographics (at wave 1) by retention status
      params.tb <- list(
        data = df.raw.attr.retained,
        focal.predictor0 = focal.predictor0,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.num = tb.num,
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood  variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_sample_by_x.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = "supplement_tbl_3",
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## Table S4. summary of outcomes (at wave 1) by retention status
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        OUTCOME.VEC0 = OUTCOME.VEC0,
        OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.num = tb.num,
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for outcome variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_outcomes_by_x.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = "supplement_tbl_4",
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ========================================================================================== ##
    ## Supplemental meta-analytic results
    ## -- looped around whether there are multiple focal variables
    f0 = 1
    for(f0 in 1:length(focal.predictor)){
      ## ======================================================================================== ##
      ## Model 1 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", focal.better.name[f0] ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 1 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; tau (heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of tau (heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC,
        MYLABEL = MYLABEL,
        focal.predictor = focal.predictor[f0],
        focal.better.name = focal.better.name[f0],
        focal.predictor.reference.value = focal.predictor.reference.value[f0],
        dir.a = dir.primary,
        dir.b = dir.supp,
        file.a = file.primary.wopc,
        file.b = file.cca.wopc,
        country.i = "",
        ci.bonferroni = ci.bonferroni,
        p.bonferroni = p.bonferroni,
        p.ci = ifelse(ci.bonferroni, p.bonferroni, 0.05),
        tb.num = tb.num,
        tb.cap = tb.cap.i,
        header.a = "Multiple Imputation",
        header.b = "Complete Case with Attrition Weights",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-a",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_outcomewide.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document", "word_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 2 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", focal.better.name[f0] ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 2 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; tau (heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of tau (heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC,
        MYLABEL = MYLABEL,
        focal.predictor = focal.predictor[f0],
        focal.better.name = focal.better.name[f0],
        focal.predictor.reference.value = focal.predictor.reference.value[f0],
        dir.a = dir.primary,
        dir.b = dir.supp,
        file.a = file.primary.wpc,
        file.b = file.cca.wpc,
        country.i = "",
        ci.bonferroni = ci.bonferroni,
        p.bonferroni = p.bonferroni,
        p.ci = 0.05,
        tb.cap = tb.cap.i,
        header.a = "Multiple Imputation",
        header.b = "Complete Case with Attrition Weights",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-b",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_outcomewide.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## E-values for estimates ================================================================= ##
      tb.cap.i = paste0("Table S",tb.num,". ", focal.better.name[f0], " for comparing estimated E-values across models and how missingness at Wave 2 was handled.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC,
        MYLABEL = MYLABEL,
        focal.predictor = focal.predictor[f0],
        focal.better.name = focal.better.name[f0],
        focal.predictor.reference.value = focal.predictor.reference.value[f0],
        dir.a = dir.primary,
        dir.b = dir.primary,
        dir.c = dir.supp,
        dir.d = dir.supp,
        file.a = file.primary.wopc,
        file.b = file.primary.wpc,
        file.c = file.cca.wopc,
        file.d = file.cca.wpc,
        country.i = "",
        ci.bonferroni = ci.bonferroni,
        p.bonferroni = p.bonferroni,
        p.ci = 0.05,
        tb.cap = tb.cap.i,
        tb.num = tb.num,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_evalues.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 1 & 2 - Meta-analyzed unstandardized estimates ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Unstandardized effects sizes for the raw score of ", focal.better.name[f0]," (multiple imputation results only).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for unstandardized regression coefficient, null effect is 0.00; CI, confidence interval; tau (heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, and no standardization was conducted prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of tau (heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC,
        MYLABEL = MYLABEL,
        focal.predictor = focal.predictor[f0],
        focal.better.name = focal.better.name[f0],
        focal.predictor.reference.value = focal.predictor.reference.value[f0],
        dir.a = dir.primary,
        dir.b = dir.supp,
        file.a = file.primary.wpc,
        file.b = file.cca.wpc,
        country.i = "",
        ci.bonferroni = ci.bonferroni,
        p.bonferroni = p.bonferroni,
        p.ci = 0.05,
        tb.cap = tb.cap.i,
        tb.num = tb.num,
        header.a = "Model 1: Demographic and Childhood Variables as Covariates",
        header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_outcomewide.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ====== Write tables to file  ============================================================= ##
    file.copy(
      here::here("data", "supp_page_1.docx"),
      here::here(res.dir, "supplement-text"),
      overwrite=TRUE
    )
    file.rename(
      here::here(res.dir, "supplement-text", "supp_page_1.docx"),
      here::here(res.dir, "supplement-text", "supplement_tbl_0.docx")
    )
    file.copy(
      here::here("data", "supp_page_1.pdf"),
      here::here(res.dir, "supplement-text"),
      overwrite=TRUE
    )
    file.rename(
      here::here(res.dir, "supplement-text", "supp_page_1.pdf"),
      here::here(res.dir, "supplement-text", "supplement_tbl_0.pdf")
    )

    ## Word version
    read_docx() |> print(target = here::here(res.dir,out.file.docx))
    supp.text.docx <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
    supp.text.docx <- supp.text.docx[str_detect( supp.text.docx, ".docx")]
    i = 1
    for(i in 1:length(supp.text.docx)){
      tmp_doc <- read_docx(supp.text.docx[i])
      sec.prop <- tmp_doc$sect_dim
      if(length(sec.prop$page) == 2){
        ps <- prop_section(
          page_size = page_size(
            orient = "portrait",
            width = sec.prop$page[1]/1440,
            height = sec.prop$page[2]/1440,
            unit = "in"
          )
        )
      } else {
        ps <- prop_section(
          page_size = page_size(
            orient = "portrait"
          )
        )
      }
      supp_doc <- read_docx(path = here::here(res.dir,out.file.docx)) |>
        body_add_docx(supp.text.docx[i]) |>
        body_end_block_section(value = block_section(ps))
      print(supp_doc, target=here::here(res.dir,out.file.docx))
    }

    ## PDF version (this method works for part 1, need a difference method for after out.file.pdf is created)
    supp.text.pdf <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
    supp.text.pdf <- supp.text.pdf[str_detect( supp.text.pdf, ".pdf")]
    qpdf::pdf_combine(input = supp.text.pdf, output=here::here(res.dir,out.file.pdf))

    cat("Part 1 complete.\n")
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
    cat("Starting part 2 -- country-specific results\n")
    i = 1;
    for (i in 1:length(COUNTRY_LABELS)) {
      run.start.time.i <- Sys.time()
      tb.let = 1
      cat("\nCountry:\t", COUNTRY_LABELS[i])
      ## get country sample size(s)
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
      {

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[i])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[i])
          tb.let <- tb.let + 1
        }
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
          )

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          focal.predictor0 = focal.predictor0,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          tb.num = tb.num,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_sample_by_x.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_1",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()
        }
      ## ======================================================================================== ##
      ## ====== Table Sib. summary statistics -- outcome variables ============================== ##
      {
        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for outcome variables in ", COUNTRY_LABELS[i])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for outcome variables  in ", COUNTRY_LABELS[i])
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.long %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[i]))

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          OUTCOME.VEC0 = OUTCOME.VEC0,
          OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          tb.num = tb.num,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_outcomes_by_x.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_2",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Sic. Unweighted summary statistics -- demo + child by retention status ==== ##
      {

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[i]," by retention status")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[i]," by retention status")
          tb.let <- tb.let + 1
        }
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

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          focal.predictor0 = focal.predictor0,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          tb.num = tb.num,
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_sample_by_x.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_3",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## == Table Sid. Unweighted summary statistics -- outcome variables by retention status === ##
      {
        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for outcome variables in ", COUNTRY_LABELS[i], " by retention status.")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for outcome variables  in ", COUNTRY_LABELS[i], " by retention status.")
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.attr.retained %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[i]))

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          OUTCOME.VEC0 = OUTCOME.VEC0,
          OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          tb.num = tb.num,
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_outcomes_by_x.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_4",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Sie. Summary of Attrition Model =========================================== ##
      {

        tb.note <- as_paragraph(paste0("Notes. N=",country.n1.print,"; attrition weights were estimated using the 'survey::svyglm(family=quasibinomial('logit'))' function. All continuous predictors were standardized and all categorical predictors used the most common category as the reference group. Reported p-values are based on the fitted regression model and no adjustments for multiple testing were done within this table."))

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Summary of fitted attrition model in ", COUNTRY_LABELS[i])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Summary of fitted attrition model in ", COUNTRY_LABELS[i])
          tb.let <- tb.let + 1
        }

        params.tb <- list(
          dir = dir.attr.models,
          country.i = COUNTRY_LABELS[i],
          tb.cap = tb.cap.i,
          tb.num = tb.num,
          fn.txt = tb.note,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sie.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_attr_model.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_5",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## ====== Table Sif. Country specific PCA Summary ========================================= ##
      {


        # footnote information:
        tb.note <- as_paragraph("Notes.  N=",country.n1.print,"; PCA was conducted using 'survey::svyprcomp(.)' function using all available contemporaneous exposures at wave 1. All PCs were standardized prior to being used as predictors. The bolded row represented the number of retained components for analysis was 7.")

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Summary of principal components in ", COUNTRY_LABELS[i])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Summary of principal components in ", COUNTRY_LABELS[i])
          tb.let <- tb.let + 1
        }

        params.tb <- list(
          dir = dir.primary,
          country.i = COUNTRY_LABELS[i],
          OUTCOME.VEC = OUTCOME.VEC,
          focal.predictor = focal.predictor,
          tb.cap = tb.cap.i,
          tb.num = tb.num,
          fn.txt = tb.note,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_tb_pca_summary.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = "tmp_tbl_6",
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.tb
        )
        remove(params.tb)
        gc()


      }
      ## ======================================================================================== ##
      ## ====== Table Sig-x. Country specific outcome wide results ============================== ##
      f0 <- 1
      for(f0 in 1:length(focal.predictor)){
        ##======================================================================================= ##
        ## Model estimated using multiple imputation
        {
          if(num.sequential){
            tb.cap.i <- paste0("Table S",tb.num,". Associations of ", focal.better.name[f0] ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[i])
            tb.num <- tb.num + 1
          } else {
            tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Associations of ", focal.better.name[f0] ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[i])
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n1.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold.")

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC,
            MYLABEL = MYLABEL,
            focal.predictor = focal.predictor[f0],
            focal.better.name = focal.better.name[f0],
            focal.predictor.reference.value = focal.predictor.reference.value[f0],
            dir.a = dir.primary,
            dir.b = dir.primary,
            file.a = "_primary_wopc",
            file.b = "_primary_wpc",
            country.i = COUNTRY_LABELS[i],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            tb.num = tb.num,
            header.a = "Model 1: Demographic and Childhood Variables as Covariates",
            header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE
          )
          rmarkdown::render(
            input = system.file("rmd", "supplement_tb_outcomewide.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document","word_document"),
            output_file = paste0("tmp_tbl_7",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = params.tb
          )
          remove(params.tb)
          gc()
        }
        ##======================================================================================= ##
        ## Results based on complete-case analysis w/ attrition weights
        {
          if(num.sequential){
            tb.cap <- paste0("Table S",tb.num,". Associations of ", focal.better.name[f0] ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[i], " using complete-case analyses with attrition weights.")
            tb.num <- tb.num + 1
          } else{
            tb.cap <- paste0("Table S",tb.num, letters[tb.let],". Associations of ", focal.better.name[f0] ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[i], " using complete-case analyses with attrition weights.")
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n2.print ,"; Reference for focal predictor: ", focal.predictor.reference.value[f0],". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Attrition weights were computed to adjust the complete case data (those who responded at Wave 2 to at least 50% of the questions) and multiple imputation was used to impute missing data on all remaining within wave on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold.")

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC,
            MYLABEL = MYLABEL,
            focal.predictor = focal.predictor[f0],
            focal.better.name = focal.better.name[f0],
            focal.predictor.reference.value = focal.predictor.reference.value[f0],
            dir.a = dir.supp,
            dir.b = dir.supp,
            file.a = "_cca_wopc",
            file.b = "_cca_wpc",
            country.i = COUNTRY_LABELS[i],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            tb.num = tb.num,
            header.a = "Model 1: Demographic and Childhood Variables as Covariates",
            header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE
          )
          rmarkdown::render(
            input = system.file("rmd", "supplement_tb_outcomewide.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document","word_document"),
            output_file = paste0("tmp_tbl_8",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = params.tb
          )
          remove(params.tb)
          gc()
        }
        ## ====================================================================================== ##
        ## Country Specific E-values output table =============================== ##
        {
          if(num.sequential){
            tb.cap <- paste0("Table S",tb.num,". Sensitivity analysis of ", focal.better.name[f0] ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[i])
            tb.num <- tb.num + 1
          } else{
            tb.cap <-  paste0("Table S",tb.num,letters[tb.let],". Sensitivity analysis of ", focal.better.name[f0] ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[i])
            tb.let <- tb.let + 1
          }

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC,
            MYLABEL = MYLABEL,
            focal.predictor = focal.predictor[f0],
            focal.better.name = focal.better.name[f0],
            focal.predictor.reference.value = focal.predictor.reference.value[f0],
            dir.a = dir.primary,
            dir.b = dir.primary,
            dir.c = dir.supp,
            dir.d = dir.supp,
            file.a = "_primary_wopc",
            file.b = "_primary_wpc",
            file.c = "_cca_wopc",
            file.d = "_cca_wpc",
            country.i = COUNTRY_LABELS[i],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            tb.num = tb.num,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sih-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE
          )
          rmarkdown::render(
            input = system.file("rmd", "supplement_tb_evalues.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document","word_document"),
            output_file = paste0("tmp_tbl_9",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = params.tb
          )
          remove(params.tb)
          gc()
        }

      }
      ## ======================================================================================== ##
      ## ====== Print out tables to formatted Word document ===================================== ##
      {

        ## Word version
        supp.text.docx <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
        supp.text.docx <- supp.text.docx[str_detect(supp.text.docx, "tmp_tbl_")]
        supp.text.docx <- supp.text.docx[str_detect( supp.text.docx, ".docx")]

        tmp.txt <- fpar(
          ftext(
            paste0(COUNTRY_LABELS[i], " Specific Results"),
            prop = fp_text(font.family = "Open Sans", font.size = 14, bold = TRUE)
          )
        )
        read_docx(path = here::here(res.dir,out.file.docx)) |>
          body_add_break() |>
          body_add_fpar(tmp.txt) |>
          print(target=here::here(res.dir,out.file.docx))

        j = 1
        for(j in 1:length(supp.text.docx)){
          tmp_doc <- read_docx(supp.text.docx[j])
          sec.prop <- tmp_doc$sect_dim
          if(length(sec.prop$page) == 2){
            ps <- prop_section(
              page_size = page_size(
                orient = ifelse(sec.prop$landscape, "landscape", "portrait"),
                width = sec.prop$page[1]/1440,
                height = sec.prop$page[2]/1440,
                unit = "in"
              )
            )
          } else {
            ps <- prop_section(
              page_size = page_size(
                orient = "portrait"
              )
            )
          }
          supp_doc <- read_docx(path = here::here(res.dir,out.file.docx)) |>
            body_add_docx(supp.text.docx[j]) |>
            body_end_block_section(value = block_section(ps))
          print(supp_doc, target=here::here(res.dir,out.file.docx))
        }

        ## PDF version (this method works for part 1, need a difference method for after out.file.pdf is created)
        supp.text.pdf <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
        supp.text.pdf <- supp.text.pdf[str_detect(supp.text.pdf, "tmp_tbl_")]
        supp.text.pdf <- supp.text.pdf[str_detect( supp.text.pdf, ".pdf")]
        qpdf::pdf_combine(input = supp.text.pdf, output = here::here(res.dir, "tmp_pdf_1.pdf"))
        gfs_append_pdf(res.dir, out.file.pdf, add = here::here(res.dir, "tmp_pdf_1.pdf"))

      }

      ## need to update table number if NOT sequentially updated
      if(!num.sequential){
        tb.num <- tb.num + 1
      }
    }
    cat("Part 2 complete.\n")
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
    cat("Starting part 3 - extra-wide format and forest plots\n")

    ## ========================================================================================== ##
    ## ====== Table S1. summary statistics -- demographics variables by country ================= ##
    tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics of demographic and childhood variables data across countries.")
    params.tb <- list(
      x = as.name("WAVE0"),
      y = as.name("COUNTRY"),
      data = df.raw.long %>%
        mutate(
          WAVE0 = case_when(
            WAVE0 == "Wave 1" ~ "W1",
            WAVE0 == "Wave 2" ~ "W2"
          )
        ),
      focal.predictor0 = focal.predictor0,
      baseline.pred0 = baseline.pred0,
      wgt = as.name("WGT0"),
      psu = as.name("PSU"),
      strata = as.name("STRATA"),
      countries.included = COUNTRY_LABELS,
      tb.cap = tb.cap.i,
      fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
      cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-1.RData")),
      start.time = run.start.time,
      ignore.cache = FALSE
    )
    rmarkdown::render(
      input = system.file("rmd", "supplement_tb_sample_extra_wide.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document","word_document"),
      output_file = "supplement_tbl_w1",
      output_dir = here::here(res.dir, "supplement-text"),
      params = params.tb
    )
    tb.num <- tb.num + 1
    remove(params.tb)
    gc()


    ## ========================================================================================== ##
    ## ====== Table S2. summary statistics -- outcome variables by country ====================== ##
    tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics of outcomes variables data across countries.")
    params.tb <- list(
      x = as.name("WAVE0"),
      y = as.name("COUNTRY"),
      data = df.raw.long %>%
        mutate(
          WAVE0 = case_when(
            WAVE0 == "Wave 1" ~ "W1",
            WAVE0 == "Wave 2" ~ "W2"
          )
        ),
      OUTCOME.VEC0 = OUTCOME.VEC0,
      OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
      wgt = as.name("WGT0"),
      psu = as.name("PSU"),
      strata = as.name("STRATA"),
      countries.included = COUNTRY_LABELS,
      tb.cap = tb.cap.i,
      fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
      cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-2.RData")),
      start.time = run.start.time,
      ignore.cache = FALSE
    )
    rmarkdown::render(
      input = system.file("rmd", "supplement_tb_outcomes_extra_wide.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document","word_document"),
      output_file = "supplement_tbl_w2",
      output_dir = here::here(res.dir, "supplement-text"),
      params = params.tb
    )
    tb.num <- tb.num + 1
    remove(params.tb)
    gc()
    ## ========================================================================================== ##
    ## ====== Table S3. Reformatted outcome-wide results by country ============================= ##
    f0 = 1
    for(f0 in 1:length(focal.predictor)){

      tb.cap.i <- paste0("Table S",tb.num,". Model 2 outcome-wide results for ", focal.better.name[f0] ,"--point estimates of effect sizes only--re-structured for comparison across countries.")

      fn.txt.i <- paste0("Notes. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00. Please review the country-specific results tables or forest plots to evaluate the uncertainty in all estimated effects.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors: Relationship with mother growing up; Relationship with father growing up; parent marital status around age 12; Experienced abuse growing up (except for Israel); Felt like an outsider in family growing up; Self-rated health growing up; subjective financial status growing up growing up; Immigration status; Frequency of religious service attendance around age 12; year of birth; gender; religious affiliation at age 12; and racial/ethnic identity when available. The first seven principal components of the full set of contemporaneous confounders were included as additional predictors of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate a ES, where all continuous outcomes were standardized using the within country mean and standard deviation prior to estimating the model.")

      params.tb <- list(
        dir = dir.primary,
        res.dir = res.dir,
        OUTCOME.VEC = OUTCOME.VEC,
        focal.predictor = focal.predictor[f0],
        file = "_primary_wpc",
        tb.cap = tb.cap.i,
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-3.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE
      )
      rmarkdown::render(
        input = system.file("rmd", "supplement_tb_country_pnt_est.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document","word_document"),
        output_file = paste0("supplement_tbl_w3",letters[f0]),
        output_dir = here::here(res.dir, "supplement-text"),
        params = params.tb
      )
      tb.num = tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ====== build docs ========================================================= ##
    {

      ## Word version
      tmp.txt <- fpar(
        ftext(
          paste0("Restructure Results for Comparison"),
          prop = fp_text(font.family = "Open Sans", font.size = 14, bold = TRUE)
        )
      )
      read_docx(path = here::here(res.dir,out.file.docx)) |>
        body_add_break() |>
        body_add_fpar(tmp.txt) |>
        print(target=here::here(res.dir,out.file.docx))

      supp.text.docx <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
      supp.text.docx <- supp.text.docx[str_detect(supp.text.docx, "supplement_tbl_w")]
      supp.text.docx <- supp.text.docx[str_detect( supp.text.docx, ".docx")]
      j = 1
      for(j in 1:length(supp.text.docx)){
        tmp_doc <- read_docx(supp.text.docx[j])
        sec.prop <- tmp_doc$sect_dim
        if(length(sec.prop$page) == 2){
          ps <- prop_section(
            page_size = page_size(
              orient = ifelse(sec.prop$landscape, "landscape", "portrait"),
              width = sec.prop$page[1]/1440,
              height = sec.prop$page[2]/1440,
              unit = "in"
            )
          )
        } else {
          ps <- prop_section(
            page_size = page_size(
              orient = "portrait"
            )
          )
        }
        supp_doc <- read_docx(path = here::here(res.dir,out.file.docx)) |>
          body_add_docx(supp.text.docx[j]) |>
          body_end_block_section(value = block_section(ps))
        print(supp_doc, target=here::here(res.dir,out.file.docx))
      }

      ## PDF version (this method works for part 1, need a difference method for after out.file.pdf is created)
      supp.text.pdf <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
      supp.text.pdf <- supp.text.pdf[str_detect(supp.text.pdf, "supplement_tbl_w")]
      supp.text.pdf <- supp.text.pdf[str_detect( supp.text.pdf, ".pdf")]
      qpdf::pdf_combine(input = supp.text.pdf, output = here::here(res.dir, "tmp_pdf_1.pdf"))
      gfs_append_pdf(res.dir, out.file.pdf, add = here::here(res.dir, "tmp_pdf_1.pdf"))

    }

    ## ========================================================================================== ##
    ## ====== Supplemental Forest plots ========================================================= ##
    tmp.txt <- fpar(
      ftext(
        paste0("Forest Plots of Estimated Effects across Countries"),
        prop = fp_text(font.family = "Open Sans", font.size = 14, bold = TRUE)
      )
    )
    read_docx(path = here::here(res.dir,out.file.docx)) |>
      body_add_break() |>
      body_add_fpar(tmp.txt) |>
      print(target=here::here(res.dir,out.file.docx))

    tmp.out <- OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)]
    i = 1
    for(i in  1:length(tmp.out)){
      run.start.time.i <- Sys.time()
      f0=1
      for(f0 in 1:length(focal.predictor)){
        myvar0.bn <- str_to_lower(get_outcome_better_name(tmp.out[i], include.name = FALSE))
        fig.cap.i <- paste0("Figure S",fig.num,". Heterogeneity in the effects of ", str_to_lower(focal.better.name[f0]) ," on ", myvar0.bn ," scores across countries. (Panel A) without controlling for PCs (left); (Panel B) controlling for PCs (right); N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country. Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects. Squares represent the point estimate for each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing.")

        params.fig <- list(
          OUTCOME.VEC = OUTCOME.VEC,
          MYLABEL = MYLABEL,
          focal.predictor = focal.predictor[f0],
          focal.better.name = focal.better.name[f0],
          outcome = tmp.out[i],
          dir = dir.primary ,
          file.a = file.primary.wopc,
          file.b = file.primary.wpc,
          fig.num = fig.num,
          res.dir = here::here(res.dir),
          n.print = n.print,
          fig.cap = fig.cap.i,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE
        )
        rmarkdown::render(
          input = system.file("rmd", "supplement_text_forest_plot.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document","word_document"),
          output_file = paste0("tmp_fig"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = params.fig
        )
        fig.num = fig.num + 1

        ## Word version
        tmp.file <- here::here(res.dir, "supplement-text","tmp_fig.docx")
        tmp_doc <- read_docx(tmp.file)
        sec.prop <- tmp_doc$sect_dim
        if(length(sec.prop$page) == 2){
          ps <- prop_section(
            page_size = page_size(
              orient = "landscape",
              width = sec.prop$page[1]/1440,
              height = sec.prop$page[2]/1440,
              unit = "in"
            )
          )
        } else {
          ps <- prop_section(
            page_size = page_size(
              orient = "landscape"
            )
          )
        }
        supp_doc <- read_docx(path = here::here(res.dir,out.file.docx)) |>
          body_add_docx(tmp.file) |>
          body_end_block_section(value = block_section(ps))
        print(supp_doc, target=here::here(res.dir,out.file.docx))

        ## PDF version
        gfs_append_pdf(
          dir = res.dir,
          cur.doc = out.file.pdf,
          add = here::here(res.dir, "supplement-text", "tmp_fig.pdf")
        )

      }
      remove(params.fig)
      gc()
    }
  }

  cat("\n **Complete.**\n")

}
