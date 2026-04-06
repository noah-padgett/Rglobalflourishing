#' Construct GFS Main text results (Wave 3)
#'
#' Generated a word document containing the results for the meta-analytic outcome wide results for the GFS coordinated analyses.
#'
#' @param df.raw a data.frame containing the raw data with values coded as labels
#' @param dir.meta subdirectory where primary results are stored ('results-primary')
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results without principal components
#' @param file.wopc file containing the nested-data.frame(.) containing the meta-analyzed results with principal components included
#' @param focal.better.name a character that is used as the printed name in tables/captions to denote the focal predictor
#' @param p.bonferroni a number (e.g., 0.00081), is internally determined based on length of tbl.row.vec if not provided
#' @param baseline.pred a vector of characters defining which baseline characteristics were used as control variables. This can be used to force the inclusion some variable into the main text summary table.
#' @param tbl.row.vec a character vector of outcomes names (e.g., "HAPPY_Y2") that are to be printed in the main text meta-analytic summary table. Name MUST be included in the meta.wopc (meta.wpc) nested data.frames column (OUTCOME0), otherwise the variable won't be printed.
#' @param mylabels an optional character vector that will be printed out in specific rows of tables 2/3 depending on the specification pf tbl.row.vec
#' @param focal.variable.reference.value (character) describing the baseline/reference group for the focal predictor.
#' @param res.dir (character) defaults to "results", and will be created if needed to story results document
#' @param ... other arguments as needed
#' @returns a word document saved to the current 'results/' directory
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_wave_3_generate_main_doc <- function(
    df.raw=NULL, focal.variable = NULL,
    focal.better.name="Focal Variable",
    focal.variable.reference.value = "0",
    digits=2,
    control = list(study = "exposurewide", filetype = "main")){

  # df.raw = df.raw;
  # focal.variable = FOCAL_VARIABLE;
  # focal.better.name= FOCAL_VARIABLE_BETTER_NAME;
  # digits=2;
  # control = list(study = "exposurewide", filetype = "main", tbl.row.vec =predictors,
  #                dir.meta = "test/ignore/results-primary",
  #                file.primary = "0_meta_analyzed_results_primary.rds",
  #                res.dir = "test/ignore/results",
  #                wgt1 = as.name("ANNUAL_WEIGHT_C1"),
  #                wgt2 = as.name("ANNUAL_WEIGHT_R2"),
  #                wgt3 = as.name("ANNUAL_WEIGHT_R3"))

  control <- get_defaults_w3(control)
  ## now, unnest the control parameters
  study <- control[['study']]
  filetype <- control[['filetype']]
  dir.meta <- control[['dir.meta']]
  file.primary  <- control[['file.primary']]
  p.bonferroni  <- control[['p.bonferroni']]
  baseline.pred <- control[['baseline.pred']]
  tbl.row.vec <- control[['tbl.row.vec']]
  mylabels <- control[['mylabels']]
  res.dir <- control[['res.dir']]
  wgt <- control[['wgt']]
  wgt1 <- control[['wgt1']]
  wgt2 <- control[['wgt2']]
  wgt3 <- control[['wgt3']]
  psu <- control[['psu']]
  strata <- control[['strata']]
  ci.bonferroni <- control[['ci.bonferroni']]
  tb.footnote <- control[['tb.footnote']]
  tb.title <- control[['tb.title']]
  fig.title <- control[['fig.title']]
  forest.plots.inc.est <- control[['forest.plots.inc.est']]


  # dir.meta = "results-primary"; file.wopc = "0_meta_analyzed_results_primary_wopc.rds"; file.wpc = "0_meta_analyzed_results_primary_wpc.rds"; focal.variable = FOCAL_PREDICTOR; focal.better.name = FOCAL_PREDICTOR_BETTER_NAME; focal.variable.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE; p.bonferroni = NULL; baseline.pred = NULL; tbl.row.vec = NULL; mylabels = NULL; res.dir = "results"; wgt = as.name("WGT0"); wgt1 =  as.name("ANNUAL_WEIGHT_R2"); wgt2 = as.name("AVG.SAMP.ATTR.WGT"); psu =  as.name("PSU"); strata =  as.name("STRATA"); res.dir = "results"; ci.bonferroni = FALSE; forest.plots.inc.est = FALSE;  digits=2; include.cor = FALSE; file.cor = "0_meta_analyzed_cor.rds"

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
  ## total effective sample size
  n.print <- df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()

  ## ============================================================================================ ##
  ## ----- INTERNAL VECTORS FOR PRINTING -----
  ## Initialize internal document formatting functions
  {
    set_flextable_defaults(font.family = "Open Sans",font.size = 10)

    normal_portrait <- block_section(
      prop_section(page_size = page_size(orient = "portrait", width=8.5, height=11), type = "continuous")
    )
    extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 22,
        height = 11
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
  ## ============================================================================================ ##
  ## ----- Construct main text data for summarizing -----

  df.raw <- gfs_add_variable_labels(df.raw, tbl.row.vec)

  # ------- Wave 1 -------
  tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
  tmp00 <- tmp00[(tmp00 %in% baseline.pred)]
  df.w1 <- df.raw %>%
    select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := {{wgt1}}
    )
  colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
  df.w1$WAVE0 <- "Wave 1"
  # ------- Wave 2 -------
  df.w2 <- df.raw %>%
    filter(CASE_OBSERVED_Y2 == 1) %>%
    select(ID, COUNTRY, {{wgt2}}, {{psu}}, {{strata}}, GENDER, contains("_Y2"), any_of(tmp00)) %>%
    mutate(
      "{{wgt}}" := n() * {{wgt2}} / sum( {{wgt2}} )
    )
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y2")
  df.w2$WAVE0 <- "Wave 2"
  # ------- Wave 3 -------
  df.w3 <- df.raw %>%
    filter(CASE_OBSERVED_Y3 == 1) %>%
    select(ID, COUNTRY, {{wgt3}}, {{psu}}, {{strata}}, GENDER, contains("_Y3"), any_of(tmp00)) %>%
    mutate(
      "{{wgt}}" := n() * {{wgt3}} / sum( {{wgt3}} )
    )
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y1")
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y2")
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y3")
  df.w3$WAVE0 <- "Wave 3"
  # ------- Combine into "long data" -------
  df.raw.long <- suppressMessages({
    df.w1|>
      full_join(df.w2) |>
      full_join(df.w3)
  })

  focal.variable0 <- focal.variable |> str_remove("_Y1") |> str_remove("_Y2") |> str_remove("_Y3")
  tbl.row.vec0 <- tbl.row.vec |> str_remove("_Y2")
  baseline.pred0 <- str_remove(baseline.pred,"_Y1")

  df.raw.long <- df.raw.long %>%
    select(
      COUNTRY, {{wgt}}, {{wgt1}}, {{wgt2}}, {{wgt3}}, {{psu}}, {{strata}},
      WAVE0,
      AGE,
      any_of(c(focal.variable0, tbl.row.vec0)),
      any_of(c(baseline.pred0))
    ) %>%
    # TO-DO, figure out a way to remove the leading values (doesn't work for)
    mutate(
      across(any_of(c("COUNTRY", focal.variable0, tbl.row.vec0, baseline.pred0)), \(x){
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

  df.raw.long <- gfs_add_variable_labels(df.raw.long, tbl.row.vec)

  ## add labels for focal variable(s)
  for (i in 1:length(focal.variable0)) {
    if(any(str_detect(colnames(df.raw.long), focal.variable0[i]))){
      try({
        attr(df.raw.long[[focal.variable0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
      })
    }
  }

  remove(df.raw, df.w1, df.w2, df.w3)
  gc()

  ## =============================================================================== ##
  ## =============================================================================== ##
  ## -----  Main text table 1 (weighted summary statistics) -----
  tb.num <- 1
  params.tb1 <- list(
    df.raw.long = df.raw.long,
    focal.variable0 = focal.variable0,
    wgt = as.name("WGT0"),
    psu = as.name("PSU"),
    strata = as.name("STRATA"),
    tb.num = tb.num,
    cache.file = here::here(res.dir, "main-text", paste0("cache-tb-sumtb.RData")),
    start.time = run.start.time
  )
  ## build the table
  Rglobalflourishing:::gfs_wave_3_build_tbl_1(params.tb1)
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
  ## ----- Main text meta-analytic summary table -----
  f0=1
  for(f0 in 1:length(focal.variable)){


    if(is.null(tb.footnote)){

    if(str_detect(str_to_lower(study), "exposure") ){

      tmp <- ifelse(
        get_outcome_scale(focal.variable[f0]) == "cont",
        "ES, effect size measure for standardized regression coefficient, null effect is 0.00;",
        "RR, risk-ratio, null effect is 1.00;"
      )
      tbl.ft1 = paste0(tmp )
      tbl.ft2 <- ifelse(
        get_outcome_scale(focal.variable[f0]) == "cont",
        "%-Metric (% < -0.10 | % > 0.10), percent of effect sizes below a lower bound (< -0.10) and above an upper bound (> 0.10)",
        "%-Metric (% < 0.90 | % > 1.10), percent of effect sizes below a lower bound (< 0.90) and above an upper bound (> 1.10)")
      tbl.ft3 = paste0("An exposure-wide analytic approach was used, and a separate model was run for each exposure. ", ifelse(get_outcome_scale(focal.variable[f0]) == "cont", "A weighted linear regression model was used to estimate an ES", "A weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR") ,".")

    }
    if(str_detect(str_to_lower(study), "outcome") ){
      tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
      tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
      tbl.ft2 = "Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10"
      tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
    }

    tb.footnote <- paste0("Notes. N=", n.print, "; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")

    }
    if(is.null(tb.title)){

      if(str_detect(str_to_lower(study), "exposure") ){
        tb.title <- paste0("Table ", tb.num,". Exposure-wide estimates of associations between wave 2 variables and ",str_to_lower(focal.better.name), " assessed at wave 3.")
      }
      if(str_detect(str_to_lower(study), "outcome") ){
        tb.title <- paste0("Table ", tb.num,". Outcome-wide estimates of associations between ",str_to_lower(focal.better.name), " assessed at wave 2 and subsequent outcomes at wave 3.")
      }

    }

    params.tb2 <- list(
      study = study,
      tbl.row.vec = tbl.row.vec,
      mylabels = mylabels,
      focal.variable = focal.variable[f0],
      tbl.footnote = tb.footnote,
      tbl.title = tb.title,
      dir = dir.meta ,
      file.primary = file.primary,
      ci.bonferroni = ci.bonferroni,
      p.bonferroni = p.bonferroni,
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")),
      start.time = run.start.time,
      digits = digits
    )
    ## build the table
    Rglobalflourishing:::gfs_wave_3_build_tbl_2(params.tb2)

    rmarkdown::render(
      input = system.file("rmd", "pdf_normal_portrait.Rmd", package = "Rglobalflourishing"),
      output_format = c("pdf_document"),
      output_file = paste0("main_text_tbl_",tb.num),
      output_dir = here::here(res.dir, "main-text"),
      params = list(cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")))
    )
    Rglobalflourishing:::generate_docx_normal_portrait(
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-meta-",f0,".RData")),
      print.file = here::here(res.dir, "main-text", paste0("main_text_tbl_",tb.num,".docx"))
    )

    tb.num <- tb.num + 1
  }
  remove(params.tb2)
  gc()
  ## =============================================================================== ##
  ## =============================================================================== ##
  ## ----- Main text E-values -----
  f0=1
  for(f0 in 1:length(focal.variable)){

    tbl.footnote <- "Notes. EE, E-value for estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates."

    if(str_detect(str_to_lower(study), "exposure") ){
    tbl.title <- paste0("Table ",tb.num,". E-value sensitivity analysis for unmeasured confounding for the association between well-being and other variables at Wave 2 and ", focal.better.name, " at Wave 3.")
    }
    if(str_detect(str_to_lower(study), "outcome") ){
      tbl.title <- paste0("Table ",tb.num,". E-value sensitivity analysis for unmeasured confounding for the association between ", focal.better.name, " and subsequent well-being and other outcomes.")
    }


    params.tb3 <- list(
      study = study,
      tbl.row.vec = tbl.row.vec,
      mylabels = mylabels,
      focal.variable = focal.variable[f0],
      tbl.footnote = tbl.footnote,
      tbl.title = tbl.title,
      dir = dir.meta ,
      file.primary = file.primary,
      cache.file = here::here(res.dir, "main-text", paste0("cache-tb-evalues-",f0,".RData")),
      start.time = run.start.time,
      digits = digits
    )

    ## build the table
    Rglobalflourishing:::gfs_wave_3_build_tbl_3(params.tb3)

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
  ## ----- Main text figures (ONLY FOR OUTCOMEWIDE Study) -----
  if(str_detect(str_to_lower(study), "outcome") ){
    if("COMPOSITE_FLOURISHING_SECURE_Y3" %in% tbl.row.vec){
      f0=1
      fig.num <- 1
      for(f0 in 1:length(focal.variable)){

        if(is.null(fig.title)){
          fig.title = paste0("**Figure ",fig.num,"**. *Heterogeneity in the effects of ", focal.better.name[f0] ," at Wave 2 on composite Secure Flourishing Index scores at Wave 3 across countries (N=", n.print, ").*\n The points represent the estimated effect size in each country. The lines represented the confidence interval obtained via est+/-t(df)*SE, standard error; the overall pooled mean is represented by the points and intervals in the 'overall' row near the bottom. See our online supplemental material for more information regarding the tests of heterogeneity.")
        }



        params.fig <- list(
          focal.variable = focal.variable[f0],
          dir = dir.meta ,
          file.primary = file.primary,
          fig.title = fig.title,
          res.dir = res.dir,
          cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
          start.time = run.start.time,
          include.estimates = forest.plots.inc.est
        )

        Rglobalflourishing:::gfs_wave_3_build_fig_1(params.fig)

        rmarkdown::render(
          input = system.file("rmd", "pdf_figures.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("main_text_figures_combined-",f0),
          output_dir = here::here(res.dir, "main-text"),
          params = list(
            cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
            fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.variable[f0],".pdf"))
          )
        )
        Rglobalflourishing:::generate_docx_fig(
          cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
          fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.variable[f0],".png")),
          print.file = here::here(res.dir, "main-text", paste0("main_text_figures_combined-",f0, ".docx")),
          orient = "p",
          w = 5, h = 6
        )


        fig.num <- fig.num + 1
      }
      remove(params.fig)

      gc()
    }
  }

  ## =============================================================================== ##
  ## =============================================================================== ##
  ## ------ PRINT OUT TO FILES ------
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
    ps <- prop_section(
      page_size = page_size(
        orient = "portrait"
      )
    )
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



#' Specific table functions
#' Generate the caches table object for printing: Main text summary table
#'
#' @param params a list of parameters that were originally passed as parameters in the .Rmd files. Kept for legacy and to reduce need to rewrite code.
#' @param font.name "Open Sans"
#' @param font.size 10
#'
#' @export
#' @rdname build-functions
gfs_wave_3_build_tbl_1 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  df.raw.long = params$df.raw.long
  focal.variable0 = params$focal.variable0
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.num = params$tb.num
  cache.file = params$cache.file
  start.time = params$start.time

  ## create table
  suppressWarnings({
    sumtab <- df.raw.long %>%
      as_survey_design(
        ids = {{psu}},
        strata = {{strata}},
        weights = {{wgt}}
      ) %>%
      tbl_svysummary(
        by = WAVE0,
        include = c(
          any_of(focal.variable0),
          AGE_GRP,
          GENDER,
          EDUCATION_3,
          COUNTRY
        ),
        label =  list(
          AGE_GRP ~ "Year of birth",
          GENDER ~ "Gender",
          EDUCATION_3 ~ "Education (years)",
          COUNTRY ~ "Country of respondent"
        ),
        type = list(
          all_continuous() ~ "continuous2"
        ),
        statistic = list(
          all_continuous() ~ c("    {mean} ({sd})", "    {p25}, {p75}"),
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          all_continuous() ~ c(2,2,2,2),
          all_categorical() ~ list(label_style_number(digits=1), label_style_percent0(digits = 1))
          #n = label_style_number(digits=0),
          #p = label_style_percent(suffix = "%", digits = 2)
        ),
        missing_text = "    (Missing)",
        missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      italicize_labels() %>%
      modify_header(label ~ "**Characteristic**") %>%
      add_stat_label(
        label = all_continuous() ~ c("    Mean (Standard Deviation)", "    Q1, Q3")
      )
  })

  tb.note.summarytab <- as_paragraph(as_chunk("Note. N (%); this table is based on non-imputed data; cumulative percentages for variables may not add up to 100% due to rounding; S.A.R., Special Administrative Region. Expanded summary tables of all demographic characteristics and outcome variables are provided the online supplement in Tables S1-2.", props = fp_text_default(font.family = "Open Sans", font.size = 9)))


  print.tb <- sumtab %>%
    as_flex_table() %>%
    autofit() %>%
    format_flex_table(pg.width = 21 / 2.54 - 2) %>%
    add_header_lines(as_paragraph(
      as_chunk(paste0("Table ", tb.num ,". Weighted sample demographic summary statistics by wave."),
               props = fp_text_default(font.family = "Open Sans", font.size = 11))
    )) %>%
    add_footer_lines(
      values = tb.note.summarytab, top = FALSE
    )

  save(print.tb, file=cache.file)
}
#' @export
#' @rdname build-functions
gfs_wave_3_build_tbl_2 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  study = params$study
  focal.variable = params$focal.variable
  tbl.row.vec = params$tbl.row.vec
  mylabels = params$mylabels
  tbl.footnote = params$tbl.footnote
  tbl.title = params$tbl.title
  dir = params$dir
  file.primary = params$file.primary
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  cache.file = params$cache.file
  start.time = params$start.time
  digits = params$digits

  if( !( str_detect(str_to_lower(study), "exposure") | str_detect(str_to_lower(study), "outcome")) ){
    stop("STUDY TYPE ERROR. CANNOT CONSTRUCT META SUMMARY TABLE.")
  }
  ## columns to keep from meta-analysis results object
  vec.col <- c('outcome', 'term', 'theta.rma', 'theta.rma.se', 'theta.lb', 'theta.ub', 'tau', 'I2', 'prob.leqneq0.1', 'prob.geq0.1', 'theta.pred.int.lb', 'theta.pred.int.ub', 'rr.tau', 'rr.prob.0.90', 'rr.prob.1.10', 'rr.theta.pred.int', 'global.pvalue')

  df.main <- load_meta_result(
    file = here::here(dir, file.primary),
    predictor = unique(c(focal.variable, tbl.row.vec)),
    outcome = unique(c(focal.variable, tbl.row.vec)),
    what = vec.col,
    filter.var.out = "outcome",
    filter.var.pred = "term"
  )

  ## column names for printing
  cnames <- c("")
  ## partially depends on study
  if( str_detect(str_to_lower(study), "exposure") ){
    meta.filter.var = as.name("term")
    # get outcome scale -- determines which columns are printed out
    tmp.vec <- case_when(
      get_outcome_scale(focal.variable) == "cont" ~ "ES",
      get_outcome_scale(focal.variable) != "cont" ~ "RR",
      .default = ""
    )
    cnames <- c("Exposure", tmp.vec, "95% CI", "Pred. Int.", "%-Metric", "\u03c4", "Global p-value")
    tbl.header.width = c(3, 4)
  }
  if(str_detect(str_to_lower(study), "outcome") ){
    meta.filter.var = as.name("outcome")
    # get outcome scale -- determines which columns are printed out
    cnames <- c("Outcome", "ES", "RR", "95% CI", "Pred. Int.", "%-Metric", "\u03c4", "Global p-value")
    tbl.header.width = c(4, 4)
  }


  if( str_detect(str_to_lower(study), "exposure") ){
    is.cont <- get_outcome_scale(focal.variable) == "cont"
  }

  meta.outcomewide <- as.data.frame(matrix(nrow = length(tbl.row.vec), ncol = length(cnames)))
  colnames(meta.outcomewide) <- cnames
  i = ii = 1
  for (i in 1:length(tbl.row.vec)) {
    if (stringr::str_detect(tbl.row.vec[i], "blank") ) {
      meta.outcomewide[i, 1] <- mylabels[ii]
      ii <- ii + 1
    } else {
      meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(tbl.row.vec[i], include.name = FALSE, include.fid = FALSE, rm.text="Composite"))

      if( str_detect(str_to_lower(study), "outcome") ){
        is.cont <- get_outcome_scale(tbl.row.vec[i]) == "cont"
      }

      ## ====== Random effects meta ======================================= ##
      tmp.row <- df.main |>
        filter({{meta.filter.var}} == tbl.row.vec[i])
      tmp.row <- tmp.row %>%
        dplyr::mutate(
          est = case_when(
            is.cont ~ theta.rma,
            !is.cont ~ exp(theta.rma)
          ),
          ci = case_when(
            is.cont ~ paste0("(",.round(theta.lb, digits),", ",.round(theta.ub, digits),")"),
            !is.cont ~ paste0("(",.round(exp(theta.lb), digits),", ",.round(exp(theta.ub), digits),")")
          ),
          prop.metric = case_when(
            is.cont ~ paste0(.round(prob.leqneq0.1*100, min(0,digits-2)),"% | ", .round(prob.geq0.1*100, min(0,digits-2)),"%"),
            !is.cont ~ paste0(.round(rr.prob.0.90*100, min(0,digits-2)),"% | ", .round(rr.prob.1.10*100, min(0,digits-2)),"%")
          ),
          pred.int = case_when(
            is.cont ~ paste0("(",.round(theta.pred.int.lb, digits),", ",.round(theta.pred.int.ub, digits),")"),
            !is.cont ~ paste0("(",.round(exp(theta.pred.int.lb), digits),", ",.round(exp(theta.pred.int.ub), digits),")")
          ),
          tau = case_when(
            is.cont ~ tau,
            !is.cont ~ rr.tau
          ),
          tau =  case_when(
            tau < 0.01 ~ "<0.01\u2020",
            tau >= 0.01 ~ .round(tau,digits)
          ),
          dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
            case_when(
              x < p.bonferroni ~ paste0(.round_p(x),"***"),
              x < 0.005 ~ paste0(.round_p(x),"**"),
              x < 0.05 ~ paste0(.round(x,3),"*"),
              x > 0.05 ~ .round(x,3)
            )
          }),
          dplyr::across(where(is.numeric), \(x) .round(x, digits))
        ) |>
        select(est, ci, pred.int, prop.metric, tau, global.pvalue)
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.row) == 1){
        if(str_detect(str_to_lower(study), "exposure")){
          meta.outcomewide[i, -1] <- tmp.row
        }
        if(str_detect(str_to_lower(study), "outcome")){
          if(get_outcome_scale(tbl.row.vec[i]) == "cont"){
            meta.outcomewide[i,-c(1,3)] <- tmp.row
          }
          if(get_outcome_scale(tbl.row.vec[i]) != "cont"){
            meta.outcomewide[i,-c(1,2)] <- tmp.row
          }
        }

      }
    }
  }
  #meta.outcomewide <- na.omit(meta.outcomewide)

  # footnote information:
  tb.note.meta.outcomewide <- as_paragraph(tbl.footnote)

  print.tb <- meta.outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(tbl.row.vec, "blank"))),
           j = 1) %>%
    add_footer_row(
      values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
    ) %>%
    add_header_row(
      values = c("", "Heterogeneity Metrics"),
      colwidths = tbl.header.width
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(paste0(tbl.title),
                 props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_main_wave_3(study = study)

  save(print.tb, file=cache.file)

}
#' @export
#' @rdname build-functions
gfs_wave_3_build_tbl_3 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  study = params$study
  focal.variable = params$focal.variable
  tbl.row.vec = params$tbl.row.vec
  mylabels = params$mylabels
  tbl.footnote = params$tbl.footnote
  tbl.title = params$tbl.title
  dir = params$dir
  file.primary = params$file.primary
  cache.file = params$cache.file
  start.time = params$start.time
  digits = params$digits

  if( !( str_detect(str_to_lower(study), "exposure") | str_detect(str_to_lower(study), "outcome")) ){
    stop("STUDY TYPE ERROR. CANNOT CONSTRUCT META SUMMARY TABLE.")
  }
  ## columns to keep from meta-analysis results object
  vec.col <- c('outcome', 'term', "theta.rma.EE", "theta.rma.ECI", "rr.theta.EE", "rr.theta.ECI")
  vec.wopc <- c("E-value","E-value for CI")

  df.main <- load_meta_result(
    file = here::here(dir, file.primary),
    predictor = unique(c(focal.variable, tbl.row.vec)),
    outcome = unique(c(focal.variable, tbl.row.vec)),
    what = vec.col,
    filter.var.out = "outcome",
    filter.var.pred = "term"
  )

  ## column names for printing
  cnames <- c("")
  ## partially depends on study
  if( str_detect(str_to_lower(study), "exposure") ){
    meta.filter.var = as.name("term")
    cnames <- c("Exposure", "E-value fo Estimate","E-value for CI")
  }
  if(str_detect(str_to_lower(study), "outcome") ){
    meta.filter.var = as.name("outcome")
    # get outcome scale -- determines which columns are printed out
    cnames <- c("Outcome", "E-value fo Estimate","E-value for CI")
  }

  meta.outcomewide <- as.data.frame(matrix(nrow = length(tbl.row.vec), ncol = length(cnames)))
  colnames(meta.outcomewide) <- cnames
  i = ii = 1
  for (i in 1:length(tbl.row.vec)) {
    if (stringr::str_detect(tbl.row.vec[i], "blank") ) {
      meta.outcomewide[i, 1] <- mylabels[ii]
      ii <- ii + 1
    } else {
      meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(tbl.row.vec[i], include.name = FALSE, include.fid = FALSE, rm.text="Composite"))

      is.cont <- get_outcome_scale(tbl.row.vec[i]) == "cont"

      ## ====== Random effects meta ======================================= ##
      tmp.row <- df.main |>
        filter({{meta.filter.var}} == tbl.row.vec[i])
      tmp.row <- tmp.row %>%
        dplyr::mutate(
          #"theta.rma.EE", "theta.rma.ECI", "rr.theta.EE", "rr.theta.ECI"
          EE = case_when(
            is.cont ~ theta.rma.EE,
            !is.cont ~ rr.theta.EE
          ),
          ECI = case_when(
            is.cont ~ theta.rma.ECI,
            !is.cont ~ rr.theta.ECI
          ),
          dplyr::across(where(is.numeric), \(x) .round(x, digits))
        ) |>
        select(EE, ECI)
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.row) == 1){
        meta.outcomewide[i, -1] <- tmp.row
      }
    }
  }
  #meta.outcomewide <- na.omit(meta.outcomewide)

  # footnote information:
  tb.note.meta.outcomewide <- as_paragraph(tbl.footnote)

  print.tb <- meta.outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(tbl.row.vec, "blank"))),
           j = 1) %>%
    add_footer_row(
      values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(paste0(tbl.title),
                 props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_apa() %>%
    font(part = "all", fontname = "Open Sans") %>%
    fontsize(part = "header", size = 10) %>%
    fontsize(part = "body", size = 9) %>%
    line_spacing(space = 0.95, part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all")  %>%
    width(j=1,width=2.60)%>%
    width(j=c(2:3),width=1.25) %>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:3) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    hline_bottom(part = "header") %>%
    hline(i=1, part="header")

  save(print.tb, file=cache.file)

}

#' @export
#' @rdname build-functions
gfs_wave_3_build_fig_1 <- function(params){

  focal.variable = params$focal.variable
  dir = params$dir
  file.primary = params$file.primary
  fig.title = params$fig.title
  res.dir = params$res.dir
  cache.file = params$cache.file
  start.time = params$start.time
  include.estimates = params$include.estimates

  fig.cap = fig.title
  ALL.COUNTRIES <- c(
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

  meta_res <- load_meta_result(
    file = here::here(dir, file.primary),
    predictor = focal.variable,
    outcome = "COMPOSITE_FLOURISHING_SECURE_Y3",
    #what = vec.col,
    filter.var.out = "outcome",
    filter.var.pred = "term"
  )
  meta_res <-  meta_res |>
    filter(outcome == "COMPOSITE_FLOURISHING_SECURE_Y3")

  # meta fit objects
  fit <- meta_res$meta.rma[[1]]
  plot_df <- meta_res$data[[1]]

  # boring stuff for variable names...

  p.title = fig.title
  # identify countries omitted from meta-analysis
  tmp.included.countries = ""
  if("Country" %in% colnames(plot_df)){
    tmp.included.countries <- plot_df$Country
    tmp.included.countries <- str_replace(tmp.included.countries, "_", " ")
    tmp.included.countries <- str_trim(tmp.included.countries, "both")
    tmp.excluded.countries <- ALL.COUNTRIES[!(ALL.COUNTRIES %in% tmp.included.countries)]
    tmp.excluded.countries <- ifelse(
      !is_empty(tmp.excluded.countries),
      paste0("Excluded countries: ", paste0(tmp.excluded.countries, collapse = ", ")),
      ""
    )
  }
  xLab <- "Effect Size"

  # make sure to use the (*)i variables in data so that the correct estimates are being plotted.\
  # Noah: I switch the ordering to be by the overly conservative estimates with PC control
  plot_df <- plot_df |>
    mutate(
      COUNTRY = factor(COUNTRY, levels = COUNTRY[order(meta_res$data[[1]]$yi, decreasing = FALSE)], ordered=TRUE),
      est_lab = paste0(.round(yi), " (", .round(ci.lb.i), ", ", .round(ci.ub.i), ")")
    )
  # make sure bounds also contains 0
  xlims <- c(min(plot_df$ci.lb.i) - .05,max(plot_df$ci.ub.i) + .05)
  xlims[1] <- ifelse(xlims[1] > -0.05, -0.05, xlims[1])
  xlims[2] <- ifelse(xlims[2] < 0.05, 0.05, xlims[2])

  # DATA FOR PLOT
  dat.below <- data.frame(
    COUNTRY = c("Overall"),
    yi = c(as.numeric(fit$b)),
    ci.lb.i = c(as.numeric(fit$ci.lb)),
    ci.ub.i = c(as.numeric(fit$ci.ub))
  ) |>
    mutate(
      ci = paste0("(", .round(ci.lb.i), ",", .round(ci.ub.i), ")"),
      CI = paste0(.round(yi), " ", ci)
    )

  p_mid <- plot_df |>
    ggplot(aes(y = COUNTRY)) +
    Rglobalflourishing:::.geom_stripes() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
    geom_point(aes(x = yi),
               size = 2) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i)) +
    #scale_color_manual(values = c("#a6cee3", "#1f78b4")) +
    #scale_shape_manual(values = c(17,16)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(),
      axis.line.y = element_blank(),
      axis.text.x = element_blank()
    ) +
    xlim(xlims)

  p_right <- plot_df |>
    ggplot(aes(y = COUNTRY)) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    Rglobalflourishing:::.geom_stripes() +
    theme_void()

  p_below <- dat.below %>%
    ggplot(aes(x = yi, y = COUNTRY)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(x = yi),
               size = 3
    ) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(),
      axis.line.y = element_blank(),
    ) +
    xlim(xlims) +
    labs(x = xLab, y = NULL)

  p_below_right <- dat.below |>
    ggplot(aes(y = COUNTRY)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()

  ## plot without estimates

  if(include.estimates){
    ## plot with estimates
    p <- (p_mid + plot_spacer() + p_right  +
            plot_spacer() + plot_spacer() + plot_spacer()  +
            p_below + plot_spacer() + p_below_right ) +
      plot_layout(
        byrow = TRUE,
        widths = c(2, -0.1, 1),
        heights = c(10, -0.75, 1)
      )

    # paste0("figure_",fig.num,"A_SFI on ",focal.better.name," without PCs.png")
    ggsave(
      filename = here::here(res.dir,paste0("figure_",fig.num,"_SFI on ",focal.better.name,".pdf")),

      plot = p, height = 6, width = 10, units = "in"
    )
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"_SFI on ",focal.better.name,".png")),
      plot = p, height = 6, width = 10, units = "in", dpi = 1000
    )

  } else {
    p <- (p_mid / p_below / p_legend) +
      plot_layout(heights = c(10, 1, 1))

    # paste0("figure_",fig.num,"A_SFI on ",focal.better.name," without PCs.png")
    ggsave(
      filename = here::here(res.dir,paste0("figure_",fig.num,"_SFI on ",focal.better.name,".pdf")),
      plot = p, height = 6, width = 5, units = "in"
    )
    ggsave(
      filename = here::here(res.dir, paste0("figure_",fig.num,"_SFI on ",focal.better.name,".png")),
      plot = p, height = 6, width = 5, units = "in", dpi = 1000
    )
  }

  save(p, fig.cap, file=cache.file)
}
