#' Construct GFS Main text results
#'
#' Generated a word document containing the results for the meta-analytic outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param dir.meta subdirectory where primary results are stored ('results-primary')
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results without principal components
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results with principal components included
#' @param focal.better.name a character that is used as the printed name in tables/captions to denote the focal predictor
#' @param p.bonferroni a number (e.g., 0.00081), is internally determined based on length of outcome.vec if not provided
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
gfs_generate_main_doc <- function(df.raw=NULL, dir.meta = "results-primary", file.wopc = "0_meta_analyzed_results_primary_wopc.rds", file.wpc = "0_meta_analyzed_results_primary_wpc.rds", focal.better.name="Focal Predictor", focal.predictor.reference.value="estimated population mean of focal predictor", focal.predictor=NULL, p.bonferroni = NULL, baseline.pred = NULL, outcome.vec = NULL, mylabels = NULL, res.dir = "results", wgt = WGT0, wgt1 = ANNUAL_WEIGHT_R2, wgt2 = AVG.SAMP.ATTR.WGT, psu = PSU, strata = STRATA, ci.bonferroni = FALSE, forest.plots.inc.est = FALSE, digits=2){

  # dir.meta = "results-primary"; file.wopc = "0_meta_analyzed_results_primary_wopc.rds"; file.wpc = "0_meta_analyzed_results_primary_wpc.rds"; focal.predictor = FOCAL_PREDICTOR; focal.better.name = FOCAL_PREDICTOR_BETTER_NAME; focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE; p.bonferroni = NULL; baseline.pred = NULL; outcome.vec = NULL; mylabels = NULL; res.dir = "results"; wgt = as.name("WGT0"); wgt1 =  as.name("ANNUAL_WEIGHT_R2"); wgt2 = as.name("AVG.SAMP.ATTR.WGT"); psu =  as.name("PSU"); strata =  as.name("STRATA"); res.dir = "results"; ci.bonferroni = FALSE; forest.plots.inc.est = FALSE;

  cat("\n **Starting...**\n")
  run.start.time <- Sys.time()

  n.print = df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()
  if (!dir.exists(here::here(res.dir))) {
    dir.create(here::here(res.dir))
  }
  if(!dir.exists(here::here(res.dir, "main-text"))){
    dir.create(here::here(res.dir, "main-text"))
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
    p.bonferroni = 0.05/length(OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank",negate=TRUE)])
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
        attr(df.raw.long[[focal.predictor0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
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
  ## build the table
  Rglobalflourishing:::build_tbl_1(params.tb1)
  ## generate the files
  rmarkdown::render(
    input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
    output_format = c("pdf_document"),
    output_file = "main_text_tbl_1",
    output_dir = here::here(res.dir, "main-text"),
    params = list(cache.file = here::here(res.dir, "main-text", paste0("cache-tb-sumtb.RData")))
  )
  Rglobalflourishing:::generate_docx_normal_portrait(
    cache.file = here::here(res.dir, "main-text", paste0("cache-tb-sumtb.RData")),
    print.file = here::here(res.dir, "main-text", "main_text_tbl_1.docx")
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
      start.time = run.start.time,
      digits = digits
    )
    ## build the table
    Rglobalflourishing:::build_tbl_2(params.tb2)

    rmarkdown::render(
      input = system.file("rmd", "pdf_normal_landscape.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document"),
      output_file = paste0("main_text_tbl_",tb.num),
      output_dir = here::here(res.dir, "main-text"),
      params = list(cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")))
    )
    Rglobalflourishing:::generate_docx_normal_landscape(
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")),
      print.file = here::here(res.dir, "main-text", paste0("main_text_tbl_",tb.num,".docx"))
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
      start.time = run.start.time,
      digits = digits
    )

    ## build the table
    Rglobalflourishing:::build_tbl_3(params.tb3)

    rmarkdown::render(
      input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document"),
      output_file = paste0("main_text_tbl_",tb.num),
      output_dir = here::here(res.dir, "main-text"),
      params = list(cache.file = here::here(res.dir, "main-text", paste0("cache-tb-evalues-",f0,".RData")))
    )
    Rglobalflourishing:::generate_docx_normal_portrait(
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-evalues-",f0,".RData")),
      print.file = here::here(res.dir, "main-text", paste0("main_text_tbl_",tb.num, ".docx"))
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
          focal.predictor = focal.predictor[f0],
          focal.better.name = focal.better.name[f0],
          dir = dir.meta ,
          file.wopc = file.wopc,
          file.wpc = file.wpc,
          fig.num = f0,
          res.dir = res.dir,
          n.print = n.print,
          cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
          start.time = run.start.time,
          include.estimates = forest.plots.inc.est
        )

        Rglobalflourishing:::build_fig_1(params.fig)

        rmarkdown::render(
          input = system.file("rmd", "pdf_figures.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("main_text_figures_combined-",f0),
          output_dir = here::here(res.dir, "main-text"),
          params = list(
            cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
            fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.better.name[f0],".pdf"))
          )
        )
        Rglobalflourishing:::generate_docx_fig(
          cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
          fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.better.name[f0],".png")),
          print.file = here::here(res.dir, "main-text", paste0("main_text_figures_combined-",f0, ".docx")),
          orient = "p",
          w = 5, h = 6
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
#' ## Generate online supplement in parts
#' #
#' # ## Part 1. Global summary statistics and supplemental meta-analytic results
#' # # set what = "S1"
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S1"
#' # )
#' #
#' # ## Part 2. Country specific results
#' # # set what = "S2"
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S2"
#' # )
#'
#' ## Note. This can also be done is subcomponents if the above crashes
#' ##   Uses the 'included.countries' arguments.
#'
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S2",
#' #   included.countries = c('Argentina', 'Australia', 'Brazil', 'China', 'Egypt')
#' # )
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S2",
#' #   included.countries = c('Germany', 'Hong Kong', 'India', 'Indonesia', 'Israel', 'Japan', 'Kenya', 'Mexico')
#' # )
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S2",
#' #   included.countries = c('Nigeria', 'Philippines', 'Poland', 'South Africa', 'Spain', 'Sweden')
#' # )
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S2",
#' #   included.countries = c('Tanzania', 'Turkey', 'United Kingdom', 'United States')
#' # )
#'
#' #
#' # ## Part 3. Summary statistics by country + wave in LARGE tables
#' # # set what = "S3"
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S3"
#' # )
#' #
#' # ## Part 4. Forest plots -- can be done is pieces as well.
#' # # set what = "S4"
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S4"
#' # )
#'
#' ## only a subset of figures at a time
#' ##   Please note: this requires specifying the outcome AND the figure number
#'
#' ## Example 1. Same the above because it uses the entire set of outcomes
#'
#' # my.vec <- c( "COMPOSITE_FLOURISHING_SECURE_Y2", "COMPOSITE_FLOURISHING_Y2", "COMPOSITE_HAPPI_LIFE_SAT_Y2", "COMPOSITE_HEALTH_Y2", "COMPOSITE_MEANING_PURPOSE_Y2", "COMPOSITE_CHARACTER_Y2", "COMPOSITE_SUBJECTIVE_SOC_CONN_Y2", "COMPOSITE_FINL_MAT_WORRY_Y2", 'HAPPY_Y2',  'LIFE_SAT_Y2', 'WB_TODAY_Y2', 'WB_FIVEYRS_Y2', 'EXPECT_GOOD_Y2', 'FREEDOM_Y2', 'PEACE_Y2', 'LIFE_BALANCE_Y2', 'CAPABLE_Y2', 'WORTHWHILE_Y2', 'LIFE_PURPOSE_Y2', 'MENTAL_HEALTH_Y2', 'THREAT_LIFE_Y2', 'COMPOSITE_DEPRESSION_Y2', 'DEPRESSED_Y2', 'INTEREST_Y2', 'COMPOSITE_ANXIETY_Y2', 'FEEL_ANXIOUS_Y2', 'CONTROL_WORRY_Y2', 'SUFFERING_Y2', 'CONTENT_Y2', 'SAT_RELATNSHP_Y2', 'PEOPLE_HELP_Y2', 'CLOSE_TO_Y2', 'APPROVE_GOVT_Y2', 'SAY_IN_GOVT_Y2', 'BELONGING_Y2', 'SAT_LIVE_Y2', 'TRUST_PEOPLE_Y2', 'MARITAL_STATUS_EVER_MARRIED_Y2', 'MARITAL_STATUS_DIVORCED_Y2', 'NUM_CHILDREN_Y2', 'GROUP_NOT_REL_Y2', 'ATTEND_SVCS_Y2', 'LONELY_Y2', 'DISCRIMINATED_Y2', 'PROMOTE_GOOD_Y2', 'GIVE_UP_Y2', 'HOPE_FUTURE_Y2', 'GRATEFUL_Y2', 'SHOW_LOVE_Y2', 'FORGIVE_Y2', 'DONATED_Y2', 'HELP_STRANGER_Y2', 'VOLUNTEERED_Y2', 'PHYSICAL_HLTH_Y2', 'HEALTH_PROB_Y2', 'BODILY_PAIN_Y2', 'CIGARETTES_BINARY_Y2', 'DRINKS_Y2', 'DAYS_EXERCISE_Y2', 'EXPENSES_Y2', 'WORRY_SAFETY_Y2', 'EDUCATION_3_Y2', 'EMPLOYMENT_Y2', 'INCOME_FEELINGS_Y2', 'OWN_RENT_HOME_Y2', 'INCOME_QUINTILE_Y2', 'CONNECTED_REL_Y2', 'AFTER_DEATH_Y2', 'REL_EXPERIENC_Y2', 'SACRED_TEXTS_Y2', 'PRAY_MEDITATE_Y2', 'BELIEVE_GOD_Y2', 'LIFE_APPROACH_Y2', 'COMFORT_REL_Y2', 'LOVED_BY_GOD_Y2', 'GOD_PUNISH_Y2', 'CRITICAL_Y2', 'TELL_BELIEFS_Y2' )
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S4",
#' #   outcome.vec = my.vec,
#' #   fig.num.start = 1
#' # )
#'
#' #
#' # ## Example 2. figures in pieces
#' # # the first 8 "composite outcomes forest plots were created. The below specification adds the next 12 figures
#' # my.vec <- c( 'HAPPY_Y2',  'LIFE_SAT_Y2', 'WB_TODAY_Y2', 'WB_FIVEYRS_Y2', 'EXPECT_GOOD_Y2', 'FREEDOM_Y2', 'PEACE_Y2', 'LIFE_BALANCE_Y2', 'CAPABLE_Y2', 'WORTHWHILE_Y2', 'LIFE_PURPOSE_Y2', 'MENTAL_HEALTH_Y2')
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S4",
#' #   outcome.vec = my.vec,
#' #   fig.num.start = 9 # 8 were already created, so start with figure number 9
#' # )
#' #
#' # ## create remaining figures
#' # my.vec <- c('THREAT_LIFE_Y2', 'COMPOSITE_DEPRESSION_Y2', 'DEPRESSED_Y2', 'INTEREST_Y2', 'COMPOSITE_ANXIETY_Y2', 'FEEL_ANXIOUS_Y2', 'CONTROL_WORRY_Y2', 'SUFFERING_Y2', 'CONTENT_Y2', 'SAT_RELATNSHP_Y2', 'PEOPLE_HELP_Y2', 'CLOSE_TO_Y2', 'APPROVE_GOVT_Y2', 'SAY_IN_GOVT_Y2', 'BELONGING_Y2', 'SAT_LIVE_Y2', 'TRUST_PEOPLE_Y2', 'MARITAL_STATUS_EVER_MARRIED_Y2', 'MARITAL_STATUS_DIVORCED_Y2', 'NUM_CHILDREN_Y2', 'GROUP_NOT_REL_Y2', 'ATTEND_SVCS_Y2', 'LONELY_Y2', 'DISCRIMINATED_Y2', 'PROMOTE_GOOD_Y2', 'GIVE_UP_Y2', 'HOPE_FUTURE_Y2', 'GRATEFUL_Y2', 'SHOW_LOVE_Y2', 'FORGIVE_Y2', 'DONATED_Y2', 'HELP_STRANGER_Y2', 'VOLUNTEERED_Y2', 'PHYSICAL_HLTH_Y2', 'HEALTH_PROB_Y2', 'BODILY_PAIN_Y2', 'CIGARETTES_BINARY_Y2', 'DRINKS_Y2', 'DAYS_EXERCISE_Y2', 'EXPENSES_Y2', 'WORRY_SAFETY_Y2', 'EDUCATION_3_Y2', 'EMPLOYMENT_Y2', 'INCOME_FEELINGS_Y2', 'OWN_RENT_HOME_Y2', 'INCOME_QUINTILE_Y2', 'CONNECTED_REL_Y2', 'AFTER_DEATH_Y2', 'REL_EXPERIENC_Y2', 'SACRED_TEXTS_Y2', 'PRAY_MEDITATE_Y2', 'BELIEVE_GOD_Y2', 'LIFE_APPROACH_Y2', 'COMFORT_REL_Y2', 'LOVED_BY_GOD_Y2', 'GOD_PUNISH_Y2', 'CRITICAL_Y2', 'TELL_BELIEFS_Y2' )
#' # gfs_generate_supplemental_docs(
#' #   df.raw = df.raw,
#' #   focal.predictor = FOCAL_PREDICTOR,
#' #   focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
#' #   focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
#' #   what = "S4",
#' #   outcome.vec = my.vec,
#' #   fig.num.start = 21 # 8 + 12 figures already created, start with 21
#' # )
#' #
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
    ci.bonferroni = FALSE, num.sequential = FALSE, forest.plot.type = "combined", what = "all", only.figs=FALSE, fig.num.start = 0, tb.start.num = NULL, digits=2, replace.cntry.file.start = NULL){


  # focal.predictor = FOCAL_PREDICTOR; focal.better.name = FOCAL_PREDICTOR_BETTER_NAME; focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE; dir.primary="results-primary"; dir.supp="results-cca"; dir.attr.models = "results-attr"; file.primary.wopc = "0_meta_analyzed_results_primary_wopc.rds";file.primary.wpc = "0_meta_analyzed_results_primary_wpc.rds";  file.unstd.wopc = "0_meta_analyzed_results_unstd_wopc.rds";  file.unstd.wpc = "0_meta_analyzed_results_unstd_wpc.rds"; file.cca.wopc = "0_meta_analyzed_results_cca_wopc.rds";  file.cca.wpc = "0_meta_analyzed_results_cca_wpc.rds";  p.bonferroni = NULL; baseline.pred = NULL; outcome.vec = NULL; mylabels = NULL; wgt = as.name("WGT0"); wgt1 =  as.name("ANNUAL_WEIGHT_R2"); wgt2 = as.name("AVG.SAMP.ATTR.WGT"); psu =  as.name("PSU"); strata =  as.name("STRATA"); res.dir = "results"; included.countries=NULL;  ci.bonferroni = FALSE; num.sequential = FALSE; forest.plot.type = "combined"; what = "all"; only.figs=TRUE; fig.num.start = 0; digits=2; replace.cntry.file.start = NULL;
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
  ## clear all the "cache" files from folders so that things run smoothly
  ls.files <- list.files(here::here(res.dir, "supplement-text"), full.names = TRUE)
  file.remove(ls.files)
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
      if (any(str_detect(focal.predictor,"ABUSED"))) {
        COUNTRY_LABELS <- COUNTRY_LABELS[COUNTRY_LABELS != "Israel"]
      }
      if (any(str_detect(focal.predictor,"BELIEVE_GOD")) |
          any(str_detect(focal.predictor,"APPROVE_GOVT"))){
        COUNTRY_LABELS <- COUNTRY_LABELS[COUNTRY_LABELS != "Egypt"]
      }
      if( any(str_detect(focal.predictor, "COVID_DEATH"))  |
          any(str_detect(focal.predictor,"BELONGING"))  |
          any(str_detect(focal.predictor,"SAY_IN_GOVT")) |
          any(str_detect(focal.predictor,"APPROVE_GOVT"))){
        COUNTRY_LABELS <- COUNTRY_LABELS[COUNTRY_LABELS != "China"]
      }
      start.country = 1
    } else {

      COUNTRY_LABELS = included.countries
      start.country = 1 ## set to 0 unless updated below
      if(what == "S2"){
        base.countries <- sort( c( "Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil", "Mexico", "China") )
        ## get what country number to start with to adjust table number in S2
        start.country = which(base.countries == COUNTRY_LABELS[1])
        if(num.sequential){
          start.country = start.country*9 #adjust by multiple of 9 if numbered sequentially
        }

      }

    }
  }
  if(is.null(p.bonferroni)){
    p.bonferroni = 0.05/length(OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank",negate=TRUE)])
  }
  ## ============================================================================================ ##
  ## Restructing raw data
  ## Reformat to long by wave
  suppressMessages({
    suppressWarnings({

      df.raw <- gfs_add_variable_labels(df.raw, OUTCOME.VEC)
      df.raw <- df.raw |>
        filter(COUNTRY %in% COUNTRY_LABELS) |>
        mutate(
          COUNTRY = fct_drop(COUNTRY)
        )

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
            attr(df.raw.long[[focal.predictor0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
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
            attr(df.raw.attr.retained[[focal.predictor0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
          })
        }
      }

    })
  })

  remove(df.raw,df.w1,df.w2)
  gc()
  ## ============================================================================================ ##
  ## ============================================================================================ ##
  out.file.docx <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_", paste0(focal.predictor, collapse="_"),".docx"), " ", "_")
  out.file.pdf <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_", paste0(focal.predictor, collapse="_"),".pdf"), " ", "_")
  out.file.xlsx <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_", paste0(focal.predictor, collapse="_"),".xlsx"), " ", "_")
  if(what == "all" | what == "S1"){
    suppressWarnings({
      file.remove(here::here(res.dir,out.file.docx))
      file.remove(here::here(res.dir,out.file.pdf))
      file.remove(here::here(res.dir,out.file.xlsx))
    })
  }

  tb.num <- 1
  fig.num <- 1
  ## ============================================================================================ ##
  # Supplement 1:
  #	(1) Summary statistics of OUTCOMES by wave (raw data)
  # ========================= #
  if(what == "all" | what == "S1"){
    cat("Starting part 1 - supplemental meta-analysis results\n")
    if(what == "S1"){
    if(!is.null(tb.start.num)){
      tb.num <- tb.start.num
    }
    }
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
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_1",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData"))
        )
      )
      Rglobalflourishing:::generate_docx_normal_portrait(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_1.docx"))
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
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for outcome variables by Wave."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_2",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData"))
        )
      )
      Rglobalflourishing:::generate_docx_normal_portrait(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_2.docx"))
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## S3. Supplemental summary of sample demographics (at wave 1) by retention status
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        focal.predictor0 = focal.predictor0,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood  variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_3",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData"))
        )
      )
      Rglobalflourishing:::generate_docx_normal_portrait(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_3.docx"))
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## Table S4. summary of wave 1 outcomes by retention status
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        OUTCOME.VEC0 = OUTCOME.VEC0,
        OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for Wave 1 outcome variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_4",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData"))
        )
      )
      Rglobalflourishing:::generate_docx_normal_portrait(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_4.docx"))
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
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", str_to_lower(focal.better.name[f0]) ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 1 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", str_to_lower(focal.predictor.reference.value[f0]),"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

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
        tb.cap = tb.cap.i,
        header.a = "Multiple Imputation",
        header.b = "Complete Case with Attrition Weights",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-a",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )

      Rglobalflourishing:::build_tbl_outcomewide(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-a",f0,".RData"))
        )
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-meta-a",f0,".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_",tb.num,".docx"))
      )


      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 2 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", str_to_lower(focal.better.name[f0]) ," at Wave 1 with well-being and other outcomes at Wave 2 for Model 2 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", str_to_lower(focal.predictor.reference.value[f0]),"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

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
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )

      Rglobalflourishing:::build_tbl_outcomewide(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-b",f0,".RData"))
        )
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-meta-b",f0,".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_",tb.num,".docx"))
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## E-values for estimates ================================================================= ##
      tb.cap.i = paste0("Table S",tb.num,". ", str_to_sentence(focal.better.name[f0]), " for comparing estimated E-values across models and how missingness at Wave 2 was handled.")

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
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )
      Rglobalflourishing:::build_tbl_evalues(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData"))
        )
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_",tb.num,".docx"))
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 1 & 2 - Meta-analyzed unstandardized estimates ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Unstandardized effects sizes for the raw score of ", str_to_lower(focal.better.name[f0])," (multiple imputation results only).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; Reference for focal predictor: ", str_to_lower(focal.predictor.reference.value[f0]),"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for unstandardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES, and no standardization was conducted prior to estimating the model.

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

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
        header.a = "Model 1: Demographic and Childhood Variables as Covariates",
        header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )

      Rglobalflourishing:::build_tbl_outcomewide(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData"))
        )
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_",tb.num,".docx"))
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ====== Write tables to file  ============================================================= ##
    if(num.sequential){
      file.copy(
        here::here("data", "supp_page_num_seq.docx"),
        here::here(res.dir, "supplement-text"),
        overwrite=TRUE
      )
      file.rename(
        here::here(res.dir, "supplement-text", "supp_page_num_seq.docx"),
        here::here(res.dir, "supplement-text", "supplement_tbl_0.docx")
      )
      file.copy(
        here::here("data", "supp_page_num_seq.pdf"),
        here::here(res.dir, "supplement-text"),
        overwrite=TRUE
      )
      file.rename(
        here::here(res.dir, "supplement-text", "supp_page_num_seq.pdf"),
        here::here(res.dir, "supplement-text", "supplement_tbl_0.pdf")
      )
    } else {
      file.copy(
        here::here("data", "supp_page_default.docx"),
        here::here(res.dir, "supplement-text"),
        overwrite=TRUE
      )
      file.rename(
        here::here(res.dir, "supplement-text", "supp_page_default.docx"),
        here::here(res.dir, "supplement-text", "supplement_tbl_0.docx")
      )
      file.copy(
        here::here("data", "supp_page_default.pdf"),
        here::here(res.dir, "supplement-text"),
        overwrite=TRUE
      )
      file.rename(
        here::here(res.dir, "supplement-text", "supp_page_default.pdf"),
        here::here(res.dir, "supplement-text", "supplement_tbl_0.pdf")
      )
    }

    ## Word version
    read_docx() |> print(target = here::here(res.dir,out.file.docx))
    supp.text.docx <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
    supp.text.docx <- supp.text.docx[str_detect( supp.text.docx, ".docx")]
    supp.text.docx <- supp.text.docx[order(as.numeric(str_remove(str_sub(supp.text.docx, -7,-6),"_")))]
    i = 1
    for(i in 1:length(supp.text.docx)){
      tmp_doc <- read_docx(supp.text.docx[i])
      sec.prop <- tmp_doc$sect_dim
      if(length(sec.prop$page) == 2){
        ps <- prop_section(
          page_size = page_size(
            orient = ifelse(!sec.prop$landscape, "portrait", "landscape"),
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
    supp.text.pdf <- supp.text.pdf[order(as.numeric(str_remove(str_sub(supp.text.pdf, -6,-5),"_")))]
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
    if(what == "S2"){
      tb.num <- 4 + 4*length(focal.predictor) + start.country
      if(!is.null(tb.start.num)){
        tb.num <- tb.start.num
      }
    }
    iter = 1;
    for (iter in 1:length(COUNTRY_LABELS)) {
      run.start.time.i <- Sys.time()
      tb.let = 1
      cat("\nCountry:\t", COUNTRY_LABELS[iter])
      ## get country sample size(s)
      country.n1.print <- w1.n1.print %>%ungroup() %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
        select(N) %>% as.numeric()
      country.n2.print <- w2.n2.print %>% ungroup() %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
        select(N) %>% as.numeric()
      ## ======================================================================================== ##
      ## ====== Table Si-a. summary statistics -- demographics variables ======================== ##
      {

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.long %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
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
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )
        Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_1"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_1.docx"))
        )
        remove(params.tb)
        gc()
        }
      ## ======================================================================================== ##
      ## ====== Table Si-b. summary statistics -- outcome variables ============================= ##
      {
        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for outcome variables in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for outcome variables  in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.long %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[iter]))

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          OUTCOME.VEC0 = OUTCOME.VEC0,
          OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )

        Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_2"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_2.docx"))
        )

        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Si-c. Unweighted summary statistics -- demo + child by retention status === ##
      {

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter]," by retention status")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter]," by retention status")
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.attr.retained %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
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
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )
        Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_3"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_3.docx"))
        )
        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## ====== Table Si-d. Unweighted summary statistics -- outcome vars by retention status === ##
      {
        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for Wave 1 outcome variables in ", COUNTRY_LABELS[iter], " by retention status.")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for Wave 1 outcome variables  in ", COUNTRY_LABELS[iter], " by retention status.")
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.attr.retained %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[iter]))

        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          OUTCOME.VEC0 = OUTCOME.VEC0,
          OUTCOME.VEC.LABELS = OUTCOME.VEC.LABELS,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )

        Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_4"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_4.docx"))
        )

        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Si-e. Summary of Attrition Model ========================================== ##
      {

        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Summary of fitted attrition model in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Summary of fitted attrition model in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }


        tb.note <- paste0("Notes. N=",country.n1.print,"; attrition weights were estimated using the 'survey::svyglm(family=quasibinomial('logit'))' function. All continuous predictors were standardized and all categorical predictors used the most common category as the reference group. Reported p-values are based on the fitted regression model and no adjustments for multiple testing were done within this table.")

        params.tb <- list(
          dir = dir.attr.models,
          country.i = COUNTRY_LABELS[iter],
          tb.cap = tb.cap.i,
          fn.txt = tb.note,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sie.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx),
          digits = digits,
          replace.cntry.file.start = replace.cntry.file.start
        )

        Rglobalflourishing:::build_tbl_attr_model(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_5"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sie.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sie.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_5.docx"))
        )

        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## ====== Table Si-f. Country specific PCA Summary ======================================== ##
      {
        if(num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Summary of principal components in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Summary of principal components in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }

        # footnote information:
        tb.note <- paste0("Notes.  N=",country.n1.print,"; PCA was conducted using 'survey::svyprcomp(.)' function using all available contemporaneous exposures at wave 1. All PCs were standardized prior to being used as predictors. The bolded row represented the number of retained components for analysis was 7.")

        params.tb <- list(
          dir = dir.primary,
          country.i = COUNTRY_LABELS[iter],
          OUTCOME.VEC = OUTCOME.VEC,
          focal.predictor = focal.predictor,
          tb.cap = tb.cap.i,
          fn.txt = tb.note,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx),
          digits = digits,
          replace.cntry.file.start = replace.cntry.file.start
        )

        Rglobalflourishing:::build_tbl_pca_summary(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_6"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif.RData"))
          )
        )
        Rglobalflourishing:::generate_docx_normal_portrait(
          cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sif.RData")),
          print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_6.docx"))
        )

        remove(params.tb)
        gc()


      }
      ## ======================================================================================== ##
      ## ====== Table Si-ghi. Country specific outcome wide results ============================= ##
      f0 <- 1
      for(f0 in 1:length(focal.predictor)){
        ##======================================================================================= ##
        ## Model estimated using multiple imputation
        {
          if(num.sequential){
            tb.cap.i <- paste0("Table S",tb.num,". Associations of ", str_to_lower(focal.better.name[f0]) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter])
            tb.num <- tb.num + 1
          } else {
            tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Associations of ", str_to_lower(focal.better.name[f0]) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter])
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n1.print ,"; Reference for focal predictor: ", str_to_lower(focal.predictor.reference.value[f0]),". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

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
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            header.a = "Model 1: Demographic and Childhood Variables as Covariates",
            header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits,
            replace.cntry.file.start = replace.cntry.file.start
          )
          Rglobalflourishing:::build_tbl_outcomewide(params.tb)

          rmarkdown::render(
            input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file = paste0("tmp_tbl_7",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData"))
            )
          )
          Rglobalflourishing:::generate_docx_wide_landscape(
            cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData")),
            print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_7",letters[f0],".docx"))
          )
          remove(params.tb)
          gc()
        }
        ##======================================================================================= ##
        ## Results based on complete-case analysis w/ attrition weights
        {
          if(num.sequential){
            tb.cap.i <- paste0("Table S",tb.num,". Associations of ", str_to_lower(focal.better.name[f0]) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter], " using complete-case analyses with attrition weights.")
            tb.num <- tb.num + 1
          } else{
            tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Associations of ", str_to_lower(focal.better.name[f0]) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter], " using complete-case analyses with attrition weights.")
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n2.print ,"; Reference for focal predictor: ", str_to_lower(focal.predictor.reference.value[f0]),". RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Attrition weights were computed to adjust the complete case data (those who responded at Wave 2 to at least 50% of the questions) and multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

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
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            header.a = "Model 1: Demographic and Childhood Variables as Covariates",
            header.b = "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sih-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits,
            replace.cntry.file.start = replace.cntry.file.start
          )
          Rglobalflourishing:::build_tbl_outcomewide(params.tb)

          rmarkdown::render(
            input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file = paste0("tmp_tbl_8",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sih-",f0,".RData"))
            )
          )
          Rglobalflourishing:::generate_docx_wide_landscape(
            cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sih-",f0,".RData")),
            print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_8",letters[f0],".docx"))
          )
          remove(params.tb)
          gc()
        }
        ## ====================================================================================== ##
        ## Country Specific E-values output table =============================== ##
        {
          if(num.sequential){
            tb.cap.i <- paste0("Table S",tb.num,". Sensitivity analysis of ", str_to_lower(focal.better.name[f0]) ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[iter])
            tb.num <- tb.num + 1
          } else{
            tb.cap.i <-  paste0("Table S",tb.num,letters[tb.let],". Sensitivity analysis of ", str_to_lower(focal.better.name[f0])," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[iter])
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
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = ci.bonferroni,
            p.bonferroni = p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sii-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits,
            replace.cntry.file.start = replace.cntry.file.start
          )

          Rglobalflourishing:::build_tbl_evalues(params.tb)

          rmarkdown::render(
            input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file = paste0("tmp_tbl_9",letters[f0]),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sii-",f0,".RData"))
            )
          )
          Rglobalflourishing:::generate_docx_wide_landscape(
            cache.file= here::here(res.dir, "supplement-text", paste0("cache-tb-sii-",f0,".RData")),
            print.file = here::here(res.dir, "supplement-text", paste0("tmp_tbl_9",letters[f0],".docx"))
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
        file.ord <- supp.text.docx |>
          str_sub(-7,-5) |>
          str_remove("_") |>
          str_remove("\\.") |>
          mixedsort() |>
          order()
        supp.text.docx <- supp.text.docx[file.ord]

        tmp.txt <- fpar(
          ftext(
            paste0(COUNTRY_LABELS[iter], " Specific Results"),
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
        file.ord <- supp.text.pdf |>
          str_sub(-7,-5) |>
          str_remove("_") |>
          str_remove("\\.") |>
          mixedsort() |>
          order()
        supp.text.pdf <- supp.text.pdf[file.ord]
        qpdf::pdf_combine(input = supp.text.pdf, output = here::here(res.dir, "tmp_pdf_1.pdf"))
        gfs_append_pdf(res.dir, out.file.pdf, add = here::here(res.dir, "tmp_pdf_1.pdf"))

      }
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
  ## ============================================================================================== ##
  if(what == "all" | what == "S3"){
    cat("Starting part 3 - extra-wide format tables\n")
    if(what == "S3"){
      tb.num <- ifelse(num.sequential, 216, 32) # makes sure table number starts in the right number when only generating S3
      if(!is.null(tb.start.num)){
        tb.num <- tb.start.num
      }
    }

      ## ========================================================================================== ##
      ## ====== Table S32. summary statistics -- demographics variables by country ================= ##
      {
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
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          countries.included = COUNTRY_LABELS,
          tb.cap = tb.cap.i,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_R2; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_R2, multiplied by the created attrition weight to account for dropout, to maintain nationally representative estimates for Wave 2 characteristics.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-1.RData")),
          start.time = run.start.time,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx),
          stored.file = system.file("tbls", "saved_sample_tb.RData", package = "Rglobalflourishing")
        )
        Rglobalflourishing:::build_tbl_outcomes_exta_wide(params.tb)
        rmarkdown::render(
          input = system.file("rmd", "pdf_42_by_25.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = "supplement_tbl_w1",
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-1.RData"))
          )
        )
        # Rglobalflourishing:::generate_docx_wide_landscape(
        #   cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-1.RData")),
        #   print.file = here::here(res.dir, "supplement-text", "supplement_tbl_w1.docx")
        # )
        tb.num <- tb.num + 1
        remove(params.tb)
        gc()
      }
      ## ========================================================================================== ##
      ## ====== Table S33. summary statistics -- outcome variables by country ====================== ##
      {
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
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        stored.file = system.file("tbls", "saved_outcomes_tb.RData", package = "Rglobalflourishing")
      )
      Rglobalflourishing:::build_tbl_outcomes_exta_wide(params.tb)
      rmarkdown::render(
        input = system.file("rmd", "pdf_42_by_75.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_w2",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-2.RData"))
        )
      )
      # Rglobalflourishing:::generate_docx_wide_landscape(
      #   cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-2.RData")),
      #   print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_2.docx"))
      # )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      }
    ## ========================================================================================== ##
    ## ====== Table S34-x. Reformatted outcome-wide results by country ============================= ##
    f0 = 1
    for(f0 in 1:length(focal.predictor)){
      tb.cap.i <- paste0("Table S",tb.num,". Model 1 (controlling for demographic and childhood) outcome-wide results for ", str_to_lower(focal.better.name[f0]) ,"--point estimates of effect sizes only--re-structured for comparison across countries.")

      fn.txt.i <- paste0("Notes. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00. Please review the country-specific results tables or forest plots to evaluate the uncertainty in all estimated effects.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".")

      params.tb <- list(
        dir = dir.primary,
        res.dir = res.dir,
        OUTCOME.VEC = OUTCOME.VEC,
        focal.predictor = focal.predictor[f0],
        file = "_primary_wopc",
        tb.cap = tb.cap.i,
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-3",letters[f0],".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        mylabels = MYLABEL,
        countries.included = COUNTRY_LABELS,
        digits = digits,
        replace.cntry.file.start = replace.cntry.file.start
      )
      Rglobalflourishing:::build_tbl_country_point_estimates(params.tb)
      rmarkdown::render(
        input = system.file("rmd", "pdf_20_by_32.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_w3",letters[f0]),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(cache.file=here::here(res.dir, "supplement-text", paste0("cache-tb-extra-3",letters[f0],".RData")))
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file=here::here(res.dir, "supplement-text", paste0("cache-tb-extra-3",letters[f0],".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_w3",letters[f0],".docx"))
      )
      tb.num = tb.num + 1
      remove(params.tb)
      gc()

      ## Model 2 results

      tb.cap.i <- paste0("Table S",tb.num,". Model 2 (controlling for demographic, childhood, and contemporaneous exposures) outcome-wide results for ", str_to_lower(focal.better.name[f0]) ,"--point estimates of effect sizes only--re-structured for comparison across countries.")

      fn.txt.i <- paste0("Notes. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00. Please review the country-specific results tables or forest plots to evaluate the uncertainty in all estimated effects.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate an RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor[f0]) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".")

      params.tb <- list(
        dir = dir.primary,
        res.dir = res.dir,
        OUTCOME.VEC = OUTCOME.VEC,
        focal.predictor = focal.predictor[f0],
        file = "_primary_wpc",
        tb.cap = tb.cap.i,
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-4",letters[f0],".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        mylabels = MYLABEL,
        countries.included = COUNTRY_LABELS,
        digits = digits,
        replace.cntry.file.start = replace.cntry.file.start
      )
      Rglobalflourishing:::build_tbl_country_point_estimates(params.tb)
      rmarkdown::render(
        input = system.file("rmd", "pdf_20_by_32.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_w4",letters[f0]),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(cache.file=here::here(res.dir, "supplement-text", paste0("cache-tb-extra-4",letters[f0],".RData")))
      )
      Rglobalflourishing:::generate_docx_wide_landscape(
        cache.file=here::here(res.dir, "supplement-text", paste0("cache-tb-extra-4",letters[f0],".RData")),
        print.file = here::here(res.dir, "supplement-text", paste0("supplement_tbl_w4",letters[f0],".docx"))
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

      ## PDF version
      supp.text.pdf <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
      supp.text.pdf <- supp.text.pdf[str_detect(supp.text.pdf, "supplement_tbl_w")]
      supp.text.pdf <- supp.text.pdf[str_detect( supp.text.pdf, ".pdf")]
      qpdf::pdf_combine(input = supp.text.pdf, output = here::here(res.dir, "tmp_pdf_1.pdf"))
      gfs_append_pdf(res.dir, out.file.pdf, add = here::here(res.dir, "tmp_pdf_1.pdf"))

    }

  }

  ## ============================================================================================== ##
  # Supplement 4:
  # (1) Supplemental forest plots
  ## ============================================================================================== ##
  if(what == "all" | what == "S4"){
    cat("Starting part 4 - forest plots\n")

    ## ========================================================================================== ##
    ## ====== Supplemental Forest plots ========================================================= ##
    gc() ## clean up junk prior to forest plots, helps run faster.

      if(fig.num.start %in% 0:1){
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

      }

      tmp.out <- OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)]
      iter = 1
      if(fig.num.start > 0){
        fig.num = fig.num.start
      }
      for(iter in  1:length(tmp.out)){
        run.start.time.i <- Sys.time()
        f0=1
        for(f0 in 1:length(focal.predictor)){

          params.fig <- list(
            OUTCOME.VEC = OUTCOME.VEC,
            MYLABEL = MYLABEL,
            focal.predictor = focal.predictor[f0],
            focal.better.name = str_to_sentence(focal.better.name[f0]),
            outcome = tmp.out[iter],
            dir = dir.primary ,
            file.a = file.primary.wopc,
            file.b = file.primary.wpc,
            fig.num0 = fig.num,
            res.dir = res.dir,
            n.print = n1.print,
            fig.cap = "Figure.",
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
            start.time = run.start.time.i,
            ignore.cache = FALSE,
            digits = digits
          )


#
#           if(forest.plot.type == "panelled"){
#             myvar0.bn <- str_to_lower(get_outcome_better_name(tmp.out[iter], include.name = FALSE))
#             fig.cap.i <- paste0("**Figure S",fig.num,".** *Heterogeneity in the effects of ", str_to_lower(focal.better.name[f0]) ," on ", myvar0.bn ," scores across countries*. (Panel A) without controlling for PCs (left); (Panel B) controlling for PCs (right); N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country. Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects. Squares represent the point estimate for each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing.")
#             params.fig[['fig.cap']] <- fig.cap.i
#             ## build plot
#             #
#             gfs_supp_forest_plot(params.fig, forest.plot.type)
#             #
#             ## print to pdf/word file
#             rmarkdown::render(
#               input = system.file("rmd", "supplement_fig_forest_plot_panelled.Rmd", package = "Rglobalflourishing"),
#               output_format = c("pdf_document","word_document"),
#               output_file = paste0("tmp_fig"),
#               output_dir = here::here(res.dir, "supplement-text"),
#               params = params.fig
#             )
#           }
#
          if(forest.plot.type == "combined"){
            myvar0.bn <- str_to_lower(get_outcome_better_name(tmp.out[iter], include.name = FALSE))
            fig.cap.i <- paste0("**Figure S",fig.num,".** *Heterogeneity in the effects of ", str_to_lower(focal.better.name[f0]) ," on ", myvar0.bn ," scores across countries.* N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country. Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects. The plot compares the estimates between Model 1 which controls for demographic and childhood variables only and Model 2 which controls for demographic variables, childhood variances, and the entire set of Wave 1 potential confounders. The potential confounders were included using principal components. The points represent the estimated effect size in each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing.")
            params.fig[['fig.cap']] <- fig.cap.i
            ## build plot
            #
            gfs_supp_forest_plot(params.fig, forest.plot.type)
            #
            ## print to pdf/word file
            rmarkdown::render(
              input = system.file("rmd", "pdf_figures.Rmd", package = "Rglobalflourishing"),
              output_format = c("pdf_document"),
              output_file = paste0("tmp_fig"),
              output_dir = here::here(res.dir, "supplement-text"),
              params = list(
                cache.file =  here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
                fig.file = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", tmp.out[iter],"_regressed_on_", focal.predictor[f0],".pdf"))
              )
            )
            Rglobalflourishing:::generate_docx_fig(
              cache.file =  here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
              fig.file = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", tmp.out[iter],"_regressed_on_", focal.predictor[f0],".png")),
              print.file = here::here(res.dir, "supplement-text","tmp_fig.docx"),
              orient = "l",
              w = 10, h = 6
            )

          }
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
        gc() ## clean up between forest plots.
      }

  }

  cat("\n **Complete.**\n")

}
