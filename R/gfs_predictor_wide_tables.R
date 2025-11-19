
#' @export
gfs_predictor_wide_online_supplement <- function(
    df.raw = NULL,
    predictor.vec = NULL, outcome.vec = NULL,
    dir.primary="results-primary", dir.supp="results-cca", dir.attr.models = "results-attr",
    file.primary.wopc = "0_meta_analyzed_results_primary_wopc.rds",
    file.primary.wpc = "0_meta_analyzed_results_primary_wpc.rds",
    file.unstd.wopc = "0_meta_analyzed_results_unstd_wopc.rds",
    file.unstd.wpc = "0_meta_analyzed_results_unstd_wpc.rds",
    file.cca.wopc = "0_meta_analyzed_results_cca_wopc.rds",
    file.cca.wpc = "0_meta_analyzed_results_cca_wpc.rds",
    p.bonferroni = NULL, baseline.pred = NULL,  mylabels = NULL,
    wgt = WGT0, wgt1 = ANNUAL_WEIGHT_R2, wgt2 = AVG.SAMP.ATTR.WGT, psu = PSU, strata = STRATA,
    res.dir = "results", included.countries=NULL,
    ci.bonferroni = FALSE, num.sequential = FALSE, forest.plot.type = "combined", what = "all", only.figs=FALSE, fig.num.start = 0, tb.start.num = NULL, digits=2,
    predictor.vec2 = NULL, outcome.vec2 = NULL){


  # predictor.vec = FOCAL_PREDICTOR; dir.primary="results-primary"; dir.supp="results-cca"; dir.attr.models = "results-attr"; file.primary.wopc = "0_meta_analyzed_results_primary_wopc.rds";file.primary.wpc = "0_meta_analyzed_results_primary_wpc.rds";  file.unstd.wopc = "0_meta_analyzed_results_unstd_wopc.rds";  file.unstd.wpc = "0_meta_analyzed_results_unstd_wpc.rds"; file.cca.wopc = "0_meta_analyzed_results_cca_wopc.rds";  file.cca.wpc = "0_meta_analyzed_results_cca_wpc.rds";  p.bonferroni = NULL; baseline.pred = NULL; outcome.vec = NULL; mylabels = NULL; wgt = as.name("WGT0"); wgt1 =  as.name("ANNUAL_WEIGHT_R2"); wgt2 = as.name("AVG.SAMP.ATTR.WGT"); psu =  as.name("PSU"); strata =  as.name("STRATA"); res.dir = "results"; included.countries=NULL;  ci.bonferroni = FALSE; num.sequential = FALSE; forest.plot.type = "combined"; what = "all"; only.figs=TRUE; fig.num.start = 0; digits=2;
  cat("\n **Starting...**\n")
  run.start.time <- Sys.time()
  predictor.vec0 <- str_remove(predictor.vec,"_Y1")

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
    OUTCOME.VEC2 = outcome.vec2

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
    p.bonferroni = 0.05/length(predictor.vec[str_detect(predictor.vec, "blank",negate=TRUE)])
  }
  ## ============================================================================================ ##
  ## Restructing raw data
  ## Reformat to long by wave
  suppressMessages({
    suppressWarnings({

      df.raw <- gfs_add_variable_labels(df.raw, predictor.vec)
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

      predictor.vec0 <- unique(c(str_remove(predictor.vec,"_Y1"), str_remove(predictor.vec2,"_Y1")))
      OUTCOME.VEC0 <- unique(c(str_remove(OUTCOME.VEC,"_Y2"), str_remove(OUTCOME.VEC2,"_Y2")))
      OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "CIGARETTES")] <- "CIGARETTES"
      baseline.pred0 <- str_remove(baseline.pred,"_Y1")

      df.raw.long <- df.raw.long %>%
        select(
          COUNTRY,
          {{wgt}}, {{wgt1}}, {{wgt2}}, {{psu}}, {{strata}},
          WAVE0,
          AGE,
          any_of(c(predictor.vec0)),
          any_of(c(OUTCOME.VEC0)),
          any_of(c(baseline.pred0)),
          INCOME, RACE1
        ) %>%
        mutate(
          INCOME = forcats::fct(INCOME),
          RACE1 = forcats::fct(RACE1),
          across(any_of(c("COUNTRY", predictor.vec0, OUTCOME.VEC0, baseline.pred0, "INCOME", "RACE1")), \(x){
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

      tmp.vec <- c(baseline.pred0, predictor.vec0, OUTCOME.VEC0)
      df.raw.long <- gfs_add_variable_labels( df=df.raw.long, vars=tmp.vec )

      ## add labels for focal predictor(s)
      # for (i in 1:length(predictor.vec0)) {
      #   if(any(str_detect(colnames(df.raw.long), predictor.vec0[i]))){
      #     try({
      #       attr(df.raw.long[[predictor.vec0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
      #     })
      #   }
      # }


      OUTCOME.VEC.LABELS <- list()
      OUTCOME.VEC00 <- OUTCOME.VEC0[OUTCOME.VEC0 %in% colnames(df.raw.long)]
      for(i in 1:length(OUTCOME.VEC00)){
        if(OUTCOME.VEC00[i] %in% outcome.vec2){
          OUTCOME.VEC00[i] <- str_remove(OUTCOME.VEC00[i],"SFI_")
        }
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
          AGE,
          any_of(c(predictor.vec0)),
          any_of(c(OUTCOME.VEC0)),
          any_of(c(baseline.pred0)),
          INCOME, RACE1
        ) %>%
        mutate(
          UNITWGT = 1,
          INCOME = forcats::fct(INCOME),
          RACE1 = forcats::fct(RACE1),
          across(any_of(c("COUNTRY", predictor.vec0, OUTCOME.VEC0, baseline.pred0, "INCOME", "RACE1")), \(x){
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

      tmp.vec <- c(baseline.pred0,predictor.vec0, OUTCOME.VEC0)
      df.raw.attr.retained <- gfs_add_variable_labels( df=df.raw.attr.retained, vars=tmp.vec )

      # ## add labels for focal predictor(s)
      # for (i in 1:length(predictor.vec0)) {
      #   if(any(str_detect(colnames(df.raw.attr.retained), predictor.vec0[i]))){
      #     try({
      #       attr(df.raw.attr.retained[[predictor.vec0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
      #     })
      #   }
      # }

    })
  })

  remove(df.raw,df.w1,df.w2)
  gc()
  ## ============================================================================================ ##
  ## ============================================================================================ ##
  out.file.docx <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_",OUTCOME.VEC[1],".docx"), " ", "_")
  out.file.pdf <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_",OUTCOME.VEC[1],".pdf"), " ", "_")
  out.file.xlsx <- stringr::str_replace_all(paste0("GFS_Wave_2_Online_Supplement_",OUTCOME.VEC[1],".xlsx"), " ", "_")
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
        focal.predictor0 = OUTCOME.VEC[1],
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
      labs <- map(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)],~get_outcome_better_name(.,FALSE))
      names(labs) <- predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.long,
        OUTCOME.VEC0 = c(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]),
        OUTCOME.VEC.LABELS = labs,
        wgt = as.name("WGT0"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for exposure and outcome variables by Wave."),
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
        focal.predictor0 = OUTCOME.VEC[1],
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
      labs <- map(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)],~get_outcome_better_name(.,FALSE))
      names(labs) <- predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        OUTCOME.VEC0 = c(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]),
        OUTCOME.VEC.LABELS = labs,
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
    for(f0 in 1:length(OUTCOME.VEC)){
      ## ======================================================================================== ##
      ## Model 1 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," at Wave 2 regressed on well-being and other variables at Wave 1 for Model 1 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC[f0],
        MYLABEL = MYLABEL,
        PREDICTOR.VEC = predictor.vec,
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

      Rglobalflourishing:::build_tbl_predictorwide(params.tb)

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
      tb.cap.i <- paste0("Table S",tb.num,". Meta-analyzed associations of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," at Wave 2 regressed on well-being and other variables at Wave 1 for Model 2 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC[f0],
        MYLABEL = MYLABEL,
        PREDICTOR.VEC = predictor.vec,
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

      Rglobalflourishing:::build_tbl_predictorwide(params.tb)

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
      tb.cap.i = paste0("Table S",tb.num,". ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)), " for comparing estimated E-values across models and how missingness at Wave 2 was handled.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC[f0],
        MYLABEL = MYLABEL,
        PREDICTOR.VEC = predictor.vec,
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
      Rglobalflourishing:::build_tbl_predictorwide_evalues(params.tb)

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
      tb.cap.i <- paste0("Table S",tb.num,". Unstandardized effects sizes for the raw scores on ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," (multiple imputation results only).")

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for unstandardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable.")

      params.tb <- list(
        is.meta = TRUE,
        OUTCOME.VEC = OUTCOME.VEC[f0],
        MYLABEL = MYLABEL,
        PREDICTOR.VEC = predictor.vec,
        dir.a = dir.primary,
        dir.b = dir.primary,
        file.a = file.unstd.wopc,
        file.b = file.unstd.wpc,
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

      Rglobalflourishing:::build_tbl_predictorwide(params.tb)

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
      tb.num <- 4 + 4*length(predictor.vec) + start.country
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
          focal.predictor0 = OUTCOME.VEC[1],
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
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for exposure and outcome variables in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for exposure and outcome variables  in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }
        temp.dat <- df.raw.long %>%
          mutate(COUNTRY = str_trim(COUNTRY)) %>%
          filter(str_detect(COUNTRY, COUNTRY_LABELS[iter]))

        labs <- map(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)],~get_outcome_better_name(.,FALSE))
        names(labs) <- predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]
        params.tb <- list(
          x = as.name("WAVE0"),
          data = temp.dat,
          OUTCOME.VEC0 = c(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]),
          OUTCOME.VEC.LABELS = labs,
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
          focal.predictor0 = OUTCOME.VEC[1],
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
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for Wave 1 exposure and outcome variables in ", COUNTRY_LABELS[iter], " by retention status.")
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
          OUTCOME.VEC0 = c(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]),
          OUTCOME.VEC.LABELS = labs,
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
          digits = digits
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
          OUTCOME.VEC = OUTCOME.VEC[1],
          focal.predictor = predictor.vec[11],
          tb.cap = tb.cap.i,
          fn.txt = tb.note,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx),
          digits = digits
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
      for(f0 in 1:length(OUTCOME.VEC)){
        outcome.better.name <- get_outcome_better_name(OUTCOME.VEC[f0],FALSE,include.wave = TRUE)
        ##======================================================================================= ##
        ## Model estimated using multiple imputation
        {
          if(num.sequential){
            tb.cap.i <- paste0("Table S",tb.num,". Associations of ",  outcome.better.name ," with adult well-being and other predictors at Wave 1 in ", COUNTRY_LABELS[iter])
            tb.num <- tb.num + 1
          } else {
            tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Associations of ",  outcome.better.name ," with adult well-being and other predictors at Wave 1 in ", COUNTRY_LABELS[iter])
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold.")

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC[f0],
            MYLABEL = MYLABEL,
            PREDICTOR.VEC = predictor.vec,
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
            digits = digits
          )
          Rglobalflourishing:::build_tbl_predictorwide(params.tb)

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
            tb.cap.i <- paste0("Table S",tb.num,". Associations of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter], " using complete-case analyses with attrition weights.")
            tb.num <- tb.num + 1
          } else{
            tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Associations of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," with adult well-being and other outcomes at Wave 2 in ", COUNTRY_LABELS[iter], " using complete-case analyses with attrition weights.")
            tb.let <- tb.let + 1
          }
          # footnote information:
          fn.txt.i <- paste0("Notes. N=", country.n2.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; SE, standard error, the SE reported for binary/Likert-type outcomes where risk-ratios are on the log(RR) scale; CI, confidence interval; p-value, a Wald-type test of the null hypothesis that the effect of the focal predictor is zero; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Attrition weights were computed to adjust the complete case data (those who responded at Wave 2 to at least 50% of the questions) and multiple imputation was used to impute missing data on all remaining within wave on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round_p(p.bonferroni),"***, correction for multiple testing using Bonferroni adjusted significant threshold.")

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC[f0],
            MYLABEL = MYLABEL,
            PREDICTOR.VEC = predictor.vec,
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
            digits = digits
          )
          Rglobalflourishing:::build_tbl_predictorwide(params.tb)

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
            tb.cap.i <- paste0("Table S",tb.num,". Sensitivity analysis of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE)) ," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[iter])
            tb.num <- tb.num + 1
          } else{
            tb.cap.i <-  paste0("Table S",tb.num,letters[tb.let],". Sensitivity analysis of ", str_to_lower(get_outcome_better_name(OUTCOME.VEC[f0], FALSE))," outcome-wide results to unmeasured confounding using E-values in ", COUNTRY_LABELS[iter])
            tb.let <- tb.let + 1
          }

          params.tb <- list(
            is.meta = FALSE,
            OUTCOME.VEC = OUTCOME.VEC[f0],
            MYLABEL = MYLABEL,
            PREDICTOR.VEC = predictor.vec,
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
            digits = digits
          )

          Rglobalflourishing:::build_tbl_predictorwide_evalues(params.tb)

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
    ## ====== Table S33. summary statistics -- predictor variables by country ====================== ##
    {
      tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics of exposure and outcomes variables data across countries.")

      labs <- map(predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)],~get_outcome_better_name(.,FALSE))
      names(labs) <- predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)]
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
        OUTCOME.VEC0 = predictor.vec0[str_detect(predictor.vec0,"blank",negate=TRUE)],
        OUTCOME.VEC.LABELS = labs,
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
    for(f0 in 1:length(OUTCOME.VEC)){
      outcome.better.name <- get_outcome_better_name(OUTCOME.VEC[f0],FALSE,include.wave = TRUE)

      tb.cap.i <- paste0("Table S",tb.num,". Model 1 (controlling for demographic and childhood) outcome-wide results for ", outcome.better.name ,"--point estimates of effect sizes only--re-structured for comparison across countries.")

      fn.txt.i <- paste0("Notes. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00. Please review the country-specific results tables or forest plots to evaluate the uncertainty in all estimated effects.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".")

      params.tb <- list(
        dir = dir.primary,
        res.dir = res.dir,
        OUTCOME.VEC = OUTCOME.VEC,
        PREDICTOR.VEC = predictor.vec,
        mylabels = MYLABEL,
        file = "_primary_wopc",
        tb.cap = tb.cap.i,
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-3",letters[f0],".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        countries.included = COUNTRY_LABELS,
        digits = digits
      )
      Rglobalflourishing:::build_tbl_predictorwide_pnt_est_wide(params.tb)
      rmarkdown::render(
        input = system.file("rmd", "pdf_20_by_22.Rmd", package = "Rglobalflourishing"),
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

      tb.cap.i <- paste0("Table S",tb.num,". Model 2 (controlling for demographic, childhood, and contemporaneous exposures) predictor-wide results for ", outcome.better.name ,"--point estimates of effect sizes only--re-structured for comparison across countries.")

      fn.txt.i <- paste0("Notes. N =", n1.print ,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00. Please review the country-specific results tables or forest plots to evaluate the uncertainty in all estimated effects.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; religious affiliation at age 12; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; and racial/ethnic identity when available. For the PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An predictor-wide analytic approach was used, and a separate model was run for each predictor. An identical model was run for the focal outcome with the only varying predictor being the predictor described in each row. The outcome is ", ifelse(get_outcome_scale(OUTCOME.VEC[f0]) == "cont", " approximately continuous and a weighted multiple linear regression analysis was conducted to estimate the change in the outcome for a unit change in the predictor", " is treated is a binary variable or dichotomized to be binary and a weighted modified Poisson multiple regression analysis was conducted to estimate a risk-ratio"), ".")

      params.tb <- list(
        dir = dir.primary,
        res.dir = res.dir,
        OUTCOME.VEC = OUTCOME.VEC,
        PREDICTOR.VEC = predictor.vec,
        mylabels = MYLABEL,
        file = "_primary_wpc",
        tb.cap = tb.cap.i,
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-extra-4",letters[f0],".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        countries.included = COUNTRY_LABELS,
        digits = digits
      )
      Rglobalflourishing:::build_tbl_predictorwide_pnt_est_wide(params.tb)
      rmarkdown::render(
        input = system.file("rmd", "pdf_20_by_22.Rmd", package = "Rglobalflourishing"),
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
    tmp.pred <- predictor.vec[str_detect(predictor.vec, "blank", negate=TRUE)]
    iter = 1
    if(fig.num.start > 0){
      fig.num = fig.num.start
    }
    for(iter in  1:length(tmp.out)){
      run.start.time.i <- Sys.time()
      f0=1
      for(f0 in 1:length(tmp.pred)){

        focal.better.name = str_to_sentence(get_outcome_better_name(tmp.pred[f0],include.name = FALSE, TRUE))

        params.fig <- list(
          OUTCOME.VEC = tmp.out[iter],
          MYLABEL = MYLABEL,
          focal.predictor = tmp.pred[f0],
          focal.better.name = focal.better.name,
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
          myvar0.bn <- get_outcome_better_name(tmp.out[iter], include.name = FALSE, include.wave = TRUE)
          fig.cap.i <- paste0("**Figure S",fig.num,".** *Heterogeneity in the effects of ", focal.better.name ," on ", myvar0.bn ," scores across countries.* N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country. Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects. The plot compares the estimates between Model 1 which controls for demographic and childhood variables only and Model 2 which controls for demographic variables, childhood variables, and the entire set of Wave 1 potential confounders. The potential confounders were included using principal components. The points represent the estimated effect size in each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing.")
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
              fig.file = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", tmp.out[iter],"_regressed_on_", tmp.pred[f0],".pdf"))
            )
          )
          Rglobalflourishing:::generate_docx_fig(
            cache.file =  here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
            fig.file = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", tmp.out[iter],"_regressed_on_", tmp.pred[f0],".png")),
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
