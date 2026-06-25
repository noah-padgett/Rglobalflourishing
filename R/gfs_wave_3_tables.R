# -------- Main Text File ------
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
    control = list(study = "exposurewide")){

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
  control0 <- control # save a local verion of the input to use in the loop later

  control <- get_defaults_w3(control0, filetype = "main")
  ## now, unnest the control parameters
  study <- control[['study']]
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
  ## clear all the "cache" files from folders so that things run smoothly
  ls.files <- list.files(here::here(res.dir, "main-text"), full.names = TRUE)
  file.remove(ls.files)

  ## total effective sample size
  n.print <- df.raw %>%
    summarize(
      N = sum({{wgt1}}, na.rm=TRUE)
    ) %>% as.numeric() %>% round()

  ## ============================================================================================ ##
  ## ----- INTERNAL VECTORS FOR PRINTING
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

  # ------- Wave 1
  tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
  tmp00 <- tmp00[(tmp00 %in% baseline.pred)]
  df.w1 <- df.raw %>%
    select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := {{wgt1}}
    )
  colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
  df.w1$WAVE0 <- "Wave 1"
  # ------- Wave 2
  df.w2 <- df.raw %>%
    filter(CASE_OBSERVED_Y2 == 1) %>%
    select(ID, COUNTRY, {{wgt2}}, {{psu}}, {{strata}}, GENDER, contains("_Y2"), any_of(tmp00)) %>%
    mutate(
      "{{wgt}}" := n() * {{wgt2}} / sum( {{wgt2}} )
    )
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y2")
  df.w2$WAVE0 <- "Wave 2"
  # ------- Wave 3
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
  # ------- Combine into "long data"
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
    # update control parameters based on focal variable ending
    if(str_detect(focal.variable[f0],, "_Y2")){
      control0[['study']] <- "outcomewide"
    } else if(str_detect(focal.variable[f0],, "_Y3")){
      control0[['study']] <- "exposurewide"
    }
    control <- get_defaults_w3(control0, filetype = "main")
    ## now, unnest the control parameters
    study <- control[['study']]
    baseline.pred <- control[['baseline.pred']]
    tbl.row.vec <- control[['tbl.row.vec']]
    mylabels <- control[['mylabels']]


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

      tb.footnote <- paste0("Notes. N=", n.print, "; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country, A prediction interval can have near zero-width when the estimate of heterogeneity (tau) is nearly zero, pulling the calibrated effect sizes to the meta-analytic mean effect size; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

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

    # update control parameters based on focal variable ending
    if(str_detect(focal.variable[f0],, "_Y2")){
      control0[['study']] <- "outcomewide"
    } else if(str_detect(focal.variable[f0],, "_Y3")){
      control0[['study']] <- "exposurewide"
    }
    control <- get_defaults_w3(control0, filetype = "main")
    ## now, unnest the control parameters
    study <- control[['study']]
    baseline.pred <- control[['baseline.pred']]
    tbl.row.vec <- control[['tbl.row.vec']]
    mylabels <- control[['mylabels']]

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
  # ## ----- Main text figures (ONLY FOR OUTCOMEWIDE Study) -----
  # if(str_detect(str_to_lower(study), "outcome") ){
  #   if("COMPOSITE_FLOURISHING_SECURE_Y3" %in% tbl.row.vec){
  #     f0=1
  #     fig.num <- 1
  #     for(f0 in 1:length(focal.variable)){
  #
  #       if(is.null(fig.title)){
  #         fig.title = paste0("**Figure ",fig.num,"**. *Heterogeneity in the effects of ", focal.better.name[f0] ," at Wave 2 on composite Secure Flourishing Index scores at Wave 3 across countries (N=", n.print, ").*\n The points represent the estimated effect size in each country. The lines represented the confidence interval obtained via est+/-t(df)*SE, standard error; the overall pooled mean is represented by the points and intervals in the 'overall' row near the bottom. See our online supplemental material for more information regarding the tests of heterogeneity.")
  #       }
  #
  #
  #
  #       params.fig <- list(
  #         focal.variable = focal.variable[f0],
  #         dir = dir.meta ,
  #         file.primary = file.primary,
  #         fig.title = fig.title,
  #         res.dir = res.dir,
  #         cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
  #         start.time = run.start.time,
  #         include.estimates = forest.plots.inc.est,
  #         fig.num = fig.num
  #       )
  #
  #       Rglobalflourishing:::gfs_wave_3_build_fig_1(params.fig)
  #
  #       rmarkdown::render(
  #         input = system.file("rmd", "pdf_figures.Rmd", package = "Rglobalflourishing"),
  #         output_format = c("pdf_document"),
  #         output_file = paste0("main_text_figures_combined-",f0),
  #         output_dir = here::here(res.dir, "main-text"),
  #         params = list(
  #           cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
  #           fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.variable[f0],".pdf"))
  #         )
  #       )
  #       Rglobalflourishing:::generate_docx_fig(
  #         cache.file = here::here(res.dir, "main-text", paste0("cache-fig-combined-",f0,".RData")),
  #         fig.file = here::here(res.dir,paste0("figure_",f0,"_SFI on ",focal.variable[f0],".png")),
  #         print.file = here::here(res.dir, "main-text", paste0("main_text_figures_combined-",f0, ".docx")),
  #         orient = "p",
  #         w = 6, h = 5
  #       )
  #
  #
  #       fig.num <- fig.num + 1
  #     }
  #     remove(params.fig)
  #
  #     gc()
  #   }
  # }

  ## =============================================================================== ##
  ## =============================================================================== ##
  ## ------ PRINT OUT TO FILES ------
  ## PDF version
  out.file <- here::here(res.dir, paste0("GFS Main Text Tables_", paste0(focal.variable, collapse=" "), ".pdf"))
  main.text.pdf <- list.files(here::here(res.dir, "main-text"),full.names = TRUE)
  main.text.pdf <- main.text.pdf[str_detect( main.text.pdf, ".pdf")]
  # make sure ordered correctly
  main.text.pdf <- c(
    main.text.pdf[str_detect( main.text.pdf, "figures", negate=TRUE)],
    main.text.pdf[str_detect( main.text.pdf, "figures")]
  )
  qpdf::pdf_combine(input = main.text.pdf, output=out.file)

  ## Word version
  out.file <- here::here(res.dir, paste0("GFS Main Text Tables_", paste0(focal.variable, collapse=" "), ".docx"))
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

  cat("\n **Complete.**\n")
}

#' Specific table functions
#' Generate the caches table object for printing: Main text summary table
#'
#' @param params a list of parameters that were originally passed as parameters in the .Rmd files. Kept for legacy and to reduce need to rewrite code.
#' @param font.name "Open Sans"
#' @param font.size 10
#'
#' @keywords internal
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
          all_categorical() ~ list(label_style_number(digits=0), label_style_percent0(digits = 1))
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
#' @keywords internal
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
          prop.metric = pad_around_divider(prop.metric, "|"),
          pred.int = case_when(
            is.cont ~ paste0("(",.round(theta.pred.int.lb, digits),", ",.round(theta.pred.int.ub, digits),")"),
            !is.cont ~ paste0("(",.round(exp(theta.pred.int.lb), digits),", ",.round(exp(theta.pred.int.ub), digits),")")
          ),
          tau = case_when(
            is.cont ~ tau,
            !is.cont ~ rr.tau
          ),
          tau =  case_when(
            tau < 0.001 ~ "<0.001\u2020",
            tau >= 0.001 ~ .round(tau,max(digits,3))
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
#' @keywords internal
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
    cnames <- c("Exposure", "E-value for Estimate","E-value for CI")
  }
  if(str_detect(str_to_lower(study), "outcome") ){
    meta.filter.var = as.name("outcome")
    # get outcome scale -- determines which columns are printed out
    cnames <- c("Outcome", "E-value for Estimate","E-value for CI")
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
#' @keywords internal
gfs_wave_3_build_fig_1 <- function(params){

  focal.variable = params$focal.variable
  focal.better.name = params$focal.better.name
  dir = params$dir
  file.primary = params$file.primary
  fig.title = params$fig.title
  res.dir = params$res.dir
  cache.file = params$cache.file
  start.time = params$start.time
  include.estimates = params$include.estimates
  fig.num = params$fig.num

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


# ------ Supplemental File Code ------
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

gfs_wave_3_generate_supplemental_docs <- function(df.raw=NULL, focal.variable = NULL,
                                                  focal.better.name="Focal Variable",
                                                  focal.variable.reference.value = "0",
                                                  digits=2,
                                                  control = list(study = "exposurewide",
                                                                 filetype = "main")){

  # df.raw = df.raw;
  # focal.variable = FOCAL_VARIABLE;
  # focal.better.name= FOCAL_VARIABLE_BETTER_NAME;
  # focal.variable.reference.value = "0"
  # digits=2;
  # control = list(study = "exposurewide",
  #                res.dir = "test/ignore/results-files-2",
  #                dir.meta = "test/ignore/results-primary",
  #                dir.primary = "test/ignore/results-primary",
  #                dir.supp = "test/ignore/results-supp"
  # )

  control0 <- control
  control <- get_defaults_w3(control, filetype = "supp")
  ## now, unnest the control parameters
  study <- control[['study']]
  res.dir <- control[['res.dir']]
  wgt <- control[['wgt']]
  wgt1 <- control[['wgt1']]
  wgt2 <- control[['wgt2']]
  wgt3 <- control[['wgt3']]
  psu <- control[['psu']]
  strata <- control[['strata']]


  cat("\n **Starting...**\n")
  run.start.time <- Sys.time()
  focal.variable0 <- focal.variable |>
    str_remove("_Y1") |> str_remove("_Y2") |> str_remove("_Y3")

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
      prop_section(page_size = page_size(orient = "portrait",
                                         width = 8.5, height=11), type = "continuous")
    )
    extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 22,
        height = 17
      ),
      type = "continuous"
    ))
    extra_extra_wide_landscape <- block_section(prop_section(
      page_size = page_size(
        orient = "landscape",
        width = 44,
        height = 17
      ),
      type = "continuous"
    ))

    landscape_three_columns <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous",
        section_columns = section_columns(widths = c(3.25,3.25,3.25))
      )
    )
    landscape_two_columns <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous",
        section_columns = section_columns(widths = c(4.5,4.5))
      )
    )
    landscape_one_column <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape",
                              width = 11, height=8.5), type = "continuous"
      )
    )
    # body_end_section_landscape(x, w = 21/2.54, h = 29.7/2.54)
  }
  ## DEFINE VARIABLE NAME VECTORS
  {
    if(is.null(control$included.countries)){
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
      if (any(str_detect(focal.variable,"ABUSED"))) {
        COUNTRY_LABELS <- COUNTRY_LABELS[COUNTRY_LABELS != "Israel"]
      }
      if (any(str_detect(focal.variable,"BELIEVE_GOD")) |
          any(str_detect(focal.variable,"APPROVE_GOVT"))){
        COUNTRY_LABELS <- COUNTRY_LABELS[COUNTRY_LABELS != "Egypt"]
      }
      if( any(str_detect(focal.variable, "COVID_DEATH"))  |
          any(str_detect(focal.variable,"BELONGING"))  |
          any(str_detect(focal.variable,"SAY_IN_GOVT")) |
          any(str_detect(focal.variable,"APPROVE_GOVT"))){
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
        if(control$num.sequential){
          start.country = start.country*9 #adjust by multiple of 9 if numbered sequentially
        }

      }

    }
  }
  ## ============================================================================================ ##
  ## Restructing raw data
  ## ----- Construct main text data for summarizing -----

  df.raw <- gfs_add_variable_labels(df.raw, control$tbl.row.vec)
  df.raw <- df.raw |>
    filter(COUNTRY %in% COUNTRY_LABELS) |>
    mutate(
      COUNTRY = fct_drop(COUNTRY)
    )

  # ------- Wave 1
  tmp00 <- colnames(df.raw)[get_wave_flag(colnames(df.raw)) == "Y1"]
  tmp00 <- tmp00[(tmp00 %in% control$baseline.pred)]
  df.w1 <- df.raw %>%
    select(ID, COUNTRY, {{wgt1}}, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" :=  n() * {{wgt1}} / sum( {{wgt1}} )
    )
  colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
  df.w1$WAVE0 <- "Wave 1"
  # ------- Wave 2
  df.w2 <- df.raw %>%
    filter(CASE_OBSERVED_Y2 == 1) %>%
    select(ID, COUNTRY, {{wgt2}}, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y2"), any_of(tmp00)) %>%
    mutate(
      "{{wgt}}" := n() * {{wgt2}} / sum( {{wgt2}} )
    )
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y2")
  df.w2$WAVE0 <- "Wave 2"
  # ------- Wave 3
  df.w3 <- df.raw %>%
    filter(CASE_OBSERVED_Y3 == 1) %>%
    select(ID, COUNTRY, {{wgt3}}, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y3"), any_of(tmp00)) %>%
    mutate(
      "{{wgt}}" := n() * {{wgt3}} / sum( {{wgt3}} )
    )
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y1")
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y2")
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y3")
  df.w3$WAVE0 <- "Wave 3"
  # ------- Combine into "long data"
  df.raw.long <- suppressMessages({
    df.w1|>
      full_join(df.w2) |>
      full_join(df.w3)
  })

  n1.print <- nrow(df.w1) |> format(big.mark = ",")
  n2.print <- nrow(df.raw |> filter(CASE_OBSERVED_ALL == 1)) |> format(big.mark = ",")

  w1.n1.print <- df.w1 %>% group_by(COUNTRY) %>% summarize(N=format(n(), big.mark=","))
  w2.n2.print <- df.raw |> filter(CASE_OBSERVED_ALL == 1) %>% group_by(COUNTRY) %>% summarize(N=format(n(), big.mark=","))


  focal.variable0 <- focal.variable |> str_remove("_Y1") |> str_remove("_Y2") |> str_remove("_Y3")
  tbl.row.vec0 <- control$tbl.row.vec |> str_remove("_Y2") |> str_remove("_Y3")
  baseline.pred0 <- str_remove(control$baseline.pred,"_Y1")

  df.raw.long <- df.raw.long %>%
    select(
      COUNTRY, {{wgt}}, {{wgt1}}, {{wgt2}}, {{wgt3}}, {{psu}}, {{strata}},
      WAVE0,
      AGE,
      any_of(c(focal.variable0, tbl.row.vec0)),
      any_of(c(baseline.pred0)),
      RACE, INCOME
    ) %>%
    # TO-DO, figure out a way to remove the leading values (doesn't work for)
    mutate(
      across(any_of(c("COUNTRY", focal.variable0, tbl.row.vec0, baseline.pred0, "RACE", "INCOME")), \(x){
        if(cur_column() == "COUNTRY"){
          x = factor(x)
        }
        if(cur_column() == "RACE"){
          x <- dplyr::case_when(x %in% get_missing_codes("SELFID1") ~ NA, .default = x)
          x <- recode_labels(x, "SELFID1")
          x <- recode_to_type(x, "SELFID1")
          x = factor(x)
        }
        if(cur_column() == "INCOME"){
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

  df.raw.long <- gfs_add_variable_labels(df.raw.long, tbl.row.vec0, include.fid=TRUE)

  ## add labels for focal variable(s)
  for (i in 1:length(focal.variable0)) {
    if(any(str_detect(colnames(df.raw.long), focal.variable0[i]))){
      try({
        attr(df.raw.long[[focal.variable0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
      })
    }
  }
  ## Reformat to long (of wave 1 variables only) of attr/retained cases to compare wave 1 variables
  # compare UNWEIGHTED data
  df.w1 <- df.raw %>%
    filter(CASE_OBSERVED_ALL == 1) %>%
    select(ID, COUNTRY, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := 1
    )
  colnames(df.w1) <- str_remove(colnames(df.w1), "_Y1")
  df.w1$WAVE0 <- "Retained--Observed in All Waves"

  df.w2 <- df.raw %>%
    filter(CASE_OBSERVED_Y2 == 0) %>%
    select(ID, COUNTRY, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := 1
    )
  colnames(df.w2) <- str_remove(colnames(df.w2), "_Y1")
  df.w2$WAVE0 <- "Attritors--Not Observed in Wave 2"

  df.w3 <- df.raw %>%
    filter(CASE_OBSERVED_Y3 == 0) %>%
    select(ID, COUNTRY, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := 1
    )
  colnames(df.w3) <- str_remove(colnames(df.w3), "_Y1")
  df.w3$WAVE0 <- "Attritors--Not Observed in Wave 3"

  df.w4 <- df.raw %>%
    filter(CASE_OBSERVED_Y3 == 1 & CASE_OBSERVED_Y2 == 0) %>%
    select(ID, COUNTRY, {{psu}}, {{strata}}, GENDER, RACE, contains("_Y1")) %>%
    mutate(
      "{{wgt}}" := 1
    )
  colnames(df.w4) <- str_remove(colnames(df.w4), "_Y1")
  df.w4$WAVE0 <- "Attritors--Observed in Wave 3 but not Wave 2"

  suppressMessages({

    df.raw.attr.retained <-
      full_join(df.w1, df.w2) |> full_join(df.w3) |>
      full_join(df.w4)
  })

  df.raw.attr.retained <- df.raw.attr.retained %>%
    select(
      COUNTRY,
      {{wgt}}, {{psu}}, {{strata}},
      WAVE0,
      {focal.variable0},
      AGE, RACE,
      any_of(c(focal.variable0, tbl.row.vec0)),
      any_of(c(baseline.pred0)),
      INCOME, RACE
    ) %>%
    mutate(
      UNITWGT = 1,
      INCOME = forcats::fct(INCOME)
    ) %>%
    # TO-DO, figure out a way to remove the leading values (doesn't work for)
    mutate(
      across(any_of(c("COUNTRY", "RACE", focal.variable0, tbl.row.vec0, baseline.pred0)), \(x){
        if(cur_column() == "COUNTRY"){
          x = factor(x)
        }
        if(cur_column() == "RACE"){
          x <- dplyr::case_when(x %in% get_missing_codes("SELFID1") ~ NA, .default = x)
          x <- recode_labels(x, "SELFID1")
          x <- recode_to_type(x, "SELFID1")
          x = factor(x)
        }

        if(cur_column() == "INCOME"){
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

  df.raw.attr.retained <- gfs_add_variable_labels(df.raw.attr.retained, tbl.row.vec0, include.fid=TRUE)

  ## add labels for focal variable(s)
  for (i in 1:length(focal.variable0)) {
    if(any(str_detect(colnames(df.raw.attr.retained), focal.variable0[i]))){
      try({
        attr(df.raw.attr.retained[[focal.variable0[i]]], which = "label") <- str_to_sentence(focal.better.name[i])
      })
    }
  }


  remove(df.w1,df.w2, df.w3, df.w4)
  gc()
  ## ============================================================================================ ##
  ## ============================================================================================ ##
  out.file.pdf <- stringr::str_replace_all(paste0("GFS_Wave_3_Online_Supplement_", paste0(focal.variable, collapse="_"),".pdf"), " ", "_")
  out.file.xlsx <- stringr::str_replace_all(paste0("GFS_Wave_3_Online_Supplement_", paste0(focal.variable, collapse="_"),".xlsx"), " ", "_")
  if(control$what == "all" | control$what == "S1"){
    suppressWarnings({
      file.remove(here::here(res.dir,out.file.pdf))
      file.remove(here::here(res.dir,out.file.xlsx))
    })
  }

  tb.num <- 1
  fig.num <- 1
  ## ============================================================================================ ##
  # ---- P1 Full sample summary and meta-results ----
  #	(1) Summary statistics of OUTCOMES by wave (raw data)
  # ========================= #
  if(control$what == "all" | control$what == "S1"){
    cat("Starting part 1 - supplemental meta-analysis results\n")
    if(control$what == "S1"){
      if(!is.null(control$tb.start.num)){
        tb.num <- control$tb.start.num
      }
    }
    ## ========================================================================================== ##
    ## ====== Construct summary tables ========================================================== ##
    {
      ## S1. Supplemental summary of sample demographics by wave
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.long,
        focal.variable0 = focal.variable0,
        focal.predictor0 = NULL,
        wgt = as.name("WGT0"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_C1; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C2, Wave 3 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C3.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_portrait_25L.Rmd", package = "Rglobalflourishing"), # portrait_long25
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_1",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s1.RData"))
        )
      )
      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## Table S2. summary of (xx)-wide by wave
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.long,
        included.variables = tbl.row.vec0[str_detect(tbl.row.vec0, "blank", negate=TRUE)],
        OUTCOME.VEC0 = NULL,
        OUTCOME.VEC.LABELS = NULL,
        wgt = as.name("WGT0"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Weighted summary statistics for outcome variables by Wave."),
        fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_C1; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C2, Wave 3 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C3.",
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_portrait_70L.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_2",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s2.RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## S3. Supplemental summary of sample demographics (at wave 1) by retention status
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        focal.variable0 = focal.variable0,
        focal.predictor0 = NULL,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood  variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_sample_by_x(params.tb, pg.width=8.5)

      rmarkdown::render(
        input = system.file("rmd", "pdf_portrait_25_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_3",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s3.RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()

      ## Table S4. summary of wave 1 outcomes by retention status
      params.tb <- list(
        x = as.name("WAVE0"),
        data = df.raw.attr.retained,
        included.variables = tbl.row.vec0[str_detect(tbl.row.vec0, "blank", negate=TRUE)],
        OUTCOME.VEC0 = NULL,
        OUTCOME.VEC.LABELS = NULL,
        wgt = as.name("UNITWGT"),
        psu = as.name("PSU"),
        strata = as.name("STRATA"),
        tb.cap = paste0("Table S",tb.num,". Unweighted summary statistics for Wave 1 outcome variables by retention status."),
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx)
      )

      Rglobalflourishing:::build_tbl_outcome_by_x(params.tb, pg.width=8.5)

      rmarkdown::render(
        input = system.file("rmd", "pdf_portrait_70_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = "supplement_tbl_4",
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-s4.RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ========================================================================================== ##
    ## Supplemental meta-analytic results
    ## looped around whether there are multiple focal variables
    f0 = 1
    for(f0 in 1:length(focal.variable)){

      # update control parameters based on focal variable ending
      if(str_detect(focal.variable[f0],, "_Y2")){
        control0[['study']] <- "outcomewide"
      } else if(str_detect(focal.variable[f0],, "_Y3")){
        control0[['study']] <- "exposurewide"
      }
      control <- get_defaults_w3(control0, filetype = "supp")
      study <- control[['study']]

      ## ======================================================================================== ##
      ## Model 1 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      if(str_detect(str_to_lower(study), "exposure") ){

        tb.cap.i <- paste0("Table S",tb.num,". Primary analysis model--Meta-analyzed associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

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

        tb.cap.i <- paste0("Table S",tb.num,". Primary analysis model--Meta-analyzed associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2  with well-being and other variables at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

        tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
        tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
        tbl.ft2 = "Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10"
        tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
      }

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")


      params.tb <- list(
        is.meta = TRUE,
        study = study,
        focal.variable = focal.variable[f0],
        tbl.row.vec = control$tbl.row.vec,
        mylabels = control$mylabels,
        focal.better.name = focal.better.name[f0],
        focal.reference.value = focal.variable.reference.value[f0],
        dir.a = control$dir.primary,
        dir.b = control$dir.supp,
        file.a = control$file.mod1.mi,
        file.b = control$file.mod1.cca,
        country.i = "",
        ci.bonferroni = control$ci.bonferroni,
        p.bonferroni = control$p.bonferroni,
        p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
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

      gfs_wave_3_build_supp_tbl(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_18_by_25.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-a",f0,".RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 2 - Meta-analyzed Results - MI & Attrition Weight ================================ ##

      if(str_detect(str_to_lower(study), "exposure") ){

        tb.cap.i <- paste0("Table S",tb.num,". Supplemental analysis model: explicit control of wave 1 value of each exposure--Meta-analyzed associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

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

        tb.cap.i <- paste0("Table S",tb.num,". Supplemental analysis model: explicit control of wave 1 of each outcome--Meta-analyzed associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2  with well-being and other variables at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

        tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
        tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
        tbl.ft2 = "Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10"
        tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
      }

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")

      params.tb <- list(
        is.meta = TRUE,
        study = study,
        focal.variable = focal.variable[f0],
        tbl.row.vec = control$tbl.row.vec,
        mylabels = control$mylabels,
        focal.better.name = focal.better.name[f0],
        focal.reference.value = focal.variable.reference.value[f0],
        dir.a = control$dir.supp,
        dir.b = control$dir.supp,
        file.a = control$file.mod2.mi,
        file.b = control$file.mod2.cca,
        country.i = "",
        ci.bonferroni = control$ci.bonferroni,
        p.bonferroni = control$p.bonferroni,
        p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
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

      gfs_wave_3_build_supp_tbl(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_18_by_25.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-b",f0,".RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 3 - Meta-analyzed Results - MI & Attrition Weight ================================ ##
      if(control$include.mod3){
        if(str_detect(str_to_lower(study), "exposure") ){

          tb.cap.i <- paste0("Table S",tb.num,". Supplemental analysis model: explicit control of wave 1 value of each exposure AND outcome--Meta-analyzed associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

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

          tb.cap.i <- paste0("Table S",tb.num,". Supplemental analysis model: explicit control of wave 1 of each outcome--Meta-analyzed associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2  with well-being and other variables at Wave 3 by approach to address missingness (multiple imputation vs. complete case with attrition weights).")

          tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
          tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
          tbl.ft2 = "Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10"
          tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
        }

        fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")

        params.tb <- list(
          is.meta = TRUE,
          study = study,
          focal.variable = focal.variable[f0],
          tbl.row.vec = control$tbl.row.vec,
          mylabels = control$mylabels,
          focal.better.name = focal.better.name[f0],
          focal.reference.value = focal.variable.reference.value[f0],
          dir.a = control$dir.supp,
          dir.b = control$dir.supp,
          file.a = control$file.mod3.mi,
          file.b = control$file.mod3.cca,
          country.i = "",
          ci.bonferroni = control$ci.bonferroni,
          p.bonferroni = control$p.bonferroni,
          p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
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

        gfs_wave_3_build_supp_tbl(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_18_by_25.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("supplement_tbl_",tb.num),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-b",f0,".RData"))
          )
        )

        tb.num <- tb.num + 1
        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## E-values for estimates ================================================================= ##

      if(str_detect(str_to_lower(study), "exposure") ){
        tbl.ft1 = "exposure at Wave 2"

      }
      if(str_detect(str_to_lower(study), "outcome") ){
        tbl.ft1 = "outcome at Wave 3"
      }

      tb.cap.i = paste0("Table S",tb.num,". Comparing estimated E-values for sensitivity to unmeasured confounding across models and how missingness was handled")

      tb.fn.i <- paste0("Notes. EE, E-value for estimate; ECI, E-value for the limit of the confidence interval; Model 1, primary analysis model where all non demographic variable included through the use of principal components analyses; Model 2, supplemental model where wave 1 value of the varying ",tbl.ft1," included explicitly as a control variable and not through the principal components. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

      params.tb <- list(
        is.meta = TRUE,
        study = study,
        focal.variable = focal.variable[f0],
        focal.better.name = focal.better.name[f0],
        focal.predictor.reference.value = focal.variable.reference.value[f0],
        tbl.row.vec = control$tbl.row.vec,
        mylabels = control$mylabels,
        dir.a = control$dir.primary,
        dir.b = control$dir.sup,
        dir.c = control$dir.supp,
        dir.d = control$dir.supp,
        file.a = control$file.mod1.mi,
        file.b = control$file.mod2.mi,
        file.c = control$file.mod1.cca,
        file.d = control$file.mod2.cca,
        header.a = "Model 1",
        header.b = "Model 2",
        header.c = "Model 1",
        header.d = "Model 2",
        country.i = "",
        ci.bonferroni = control$ci.bonferroni,
        p.bonferroni = control$p.bonferroni,
        p.ci = 0.05,
        tb.cap = tb.cap.i,
        fn.txt = tb.fn.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )
      gfs_wave_3_build_supp_tbl_evalues(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_21_by_11.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-evalues-",f0,".RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
      ## ======================================================================================== ##
      ## Model 1 & 2 - Meta-analyzed unstandardized estimates ================================ ##
      if(str_detect(str_to_lower(study), "exposure") ){

        tb.cap.i <- paste0("Table S",tb.num,". Unstandardized estimates of effect sizes--Meta-analyzed associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 by analysis model (Model 1 vs. Model 2).")

        tmp <- ifelse(
          get_outcome_scale(focal.variable[f0]) == "cont",
          "ES, effect size measure for UNstandardized regression coefficient, null effect is 0.00;",
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

        tb.cap.i <- paste0("Table S",tb.num,". Unstandardized estimates of effect sizes--Meta-analyzed associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2  with well-being and other variables at Wave 3 by analysis model (Model 1 vs. Model 2).")

        tmp <- "ES, effect size measure for UNstandardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
        tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
        tbl.ft2 = "Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10"
        tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES.")
      }

      fn.txt.i <- paste0("Notes. N(multiple imputation)=", n1.print ,"; N(complete-case)=",n2.print ,"; ", tbl.ft1 ," CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country; ", tbl.ft2,"; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")

      params.tb <- list(
        is.meta = TRUE,
        study = study,
        focal.variable = focal.variable[f0],
        tbl.row.vec = control$tbl.row.vec,
        mylabels = control$mylabels,
        focal.better.name = focal.better.name[f0],
        focal.reference.value = focal.variable.reference.value[f0],
        dir.a = control$dir.primary,
        dir.b = control$dir.supp,
        file.a = control$file.mod1.mi.unstd,
        file.b = control$file.mod2.mi.unstd,
        country.i = "",
        ci.bonferroni = control$ci.bonferroni,
        p.bonferroni = control$p.bonferroni,
        p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
        tb.cap = tb.cap.i,
        header.a = "Model 1 - implicit control of baseline value of exposure",
        header.b = "Model 2 - explicit control of baseline value of exposure",
        fn.txt = fn.txt.i,
        cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData")),
        start.time = run.start.time,
        ignore.cache = FALSE,
        file.xlsx = here::here(res.dir, out.file.xlsx),
        digits = digits
      )

      gfs_wave_3_build_supp_tbl(params.tb)

      rmarkdown::render(
        input = system.file("rmd", "pdf_18_by_25.Rmd", package = "Rglobalflourishing"),
        output_format = c("pdf_document"),
        output_file = paste0("supplement_tbl_",tb.num),
        output_dir = here::here(res.dir, "supplement-text"),
        params = list(
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-meta-c",f0,".RData"))
        )
      )

      tb.num <- tb.num + 1
      remove(params.tb)
      gc()
    }
    ## ========================================================================================== ##
    ## ========================================================================================== ##
    ## ========================================================================================== ##
    ## ====== Write tables to file  ============================================================= ##
    # if(control$num.sequential){
    #   file.copy(
    #     here::here("data", "supp_page_num_seq.pdf"),
    #     overwrite=TRUE
    #   )
    #   file.rename(
    #     here::here(res.dir, "supplement-text", "supp_page_num_seq.pdf")
    #   )
    # } else {
    #   file.copy(
    #     here::here("data", "supp_page_default.pdf"),
    #     overwrite=TRUE
    #   )
    #   file.rename(
    #     here::here(res.dir, "supplement-text", "supp_page_default.pdf")
    #   )
    # }
    ## PDF version (this method works for part 1, need a difference method for after out.file.pdf is created)
    supp.text.pdf <- list.files(here::here(res.dir, "supplement-text"),full.names = TRUE)
    supp.text.pdf <- supp.text.pdf[str_detect( supp.text.pdf, ".pdf")]
    supp.text.pdf <- supp.text.pdf[order(as.numeric(str_remove(str_sub(supp.text.pdf, -6,-5),"_")))]
    qpdf::pdf_combine(input = supp.text.pdf, output=here::here(res.dir,out.file.pdf))

    cat("Part 1 complete.\n")
  }
  ## ============================================================================================ ##
  # ---- P2 Forest plots ----
  if(control$what == "all" | control$what == "S2"){
    cat("Starting part 2 - forest plots\n")

    ## ========================================================================================== ##
    ## ====== Supplemental Forest plots ========================================================= ##
    gc() ## clean up junk prior to forest plots, helps run faster.

    if(control$fig.num.start %in% 0:1){
      tmp.txt <- fpar(
        ftext(
          paste0("Forest Plots of Estimated Effects across Countries"),
          prop = fp_text(font.family = "Open Sans", font.size = 14, bold = TRUE)
        )
      )
      fig.num <- 1

    }


    iter = 1
    if(control$fig.num.start > 0){
      fig.num = control$fig.num.start
    }
    run.start.time.i <- Sys.time()
    f0=1
    for(f0 in 1:length(focal.variable)){

      # update control parameters based on focal variable ending
      if(str_detect(focal.variable[f0],, "_Y2")){
        control0[['study']] <- "outcomewide"
      } else if(str_detect(focal.variable[f0],, "_Y3")){
        control0[['study']] <- "exposurewide"
      }
      control <- get_defaults_w3(control0, filetype = "supp")
      study <- control[['study']]

      tmp.out <- control$tbl.row.vec[str_detect(control$tbl.row.vec, "blank", negate=TRUE)]

      iter <- 1
      for(iter in  1:length(tmp.out)){

        myvar0.bn <- str_to_lower(get_outcome_better_name(tmp.out[iter], include.name = FALSE))
        cat("\nCurrent variable:\t", myvar0.bn)

        if(str_detect(study, "exposure")){
          fig.txt.1 <- paste0(myvar0.bn," at Wave 2 on ", str_to_lower(focal.better.name[f0]) , " at Wave 3 ")
          outcome.i <- focal.variable[f0]
          predictor.i <- tmp.out[iter]
        } else if(str_detect(study, "outcome")){
          fig.txt.1 <- paste0(str_to_lower(focal.better.name[f0]) ," at Wave 2 on ", myvar0.bn, " at Wave 3 ")
          outcome.i <-  tmp.out[iter]
          predictor.i <- focal.variable[f0]
        }

        out.scale <- get_outcome_scale(outcome.i)
        fit.fn.2 <- if(out.scale == "cont"){
          "Effect size estimate is based on the weighted least squares model for an approximately continuous outcome: for a one unit change in the predictor, how much on average, does the outcome change"
        } else {
          "Effect size estimate is based on the weighted modified Poisson model for a binary outcome: for a one unit change in the predictor, how much on average, does the log-risk-ratio for being 1.0 on the outcome change (check main text for how outcome was coded)"
        }

        fig.cap.i <- paste0("**Figure S",fig.num,".** *Heterogeneity in the effects of ", fig.txt.1, " scores across countries.*")
        fig.fn.i <- paste0("Notes. N=", n1.print, "; estimated effects computed accounting for the complex sampling design separately by country; ", fit.fn.2, "; Analyses conducted for this plot: Random-effects meta-analysis of country-specific effects; LOO, leave-one-country-out (the country in each row is omitted) estimates of the random-effect meta-analysis are also provided; RESID(stud) studentized residual for each country; Cooks's D-like metric of the influence on the point estimate; COVRATIO, the ratio of the estimated tau (heterogeneity) when row i's country is omitted over all countries estimate of tau. The plot provides the estimates from the primary analysis model that controls for demographic and childhood variables and the entire set of Wave 1 potential confounders via using principal components. The points represent the estimated effect size in each country. The lines represented the +/-t(df)*SE, standard error, around the estimate; the overall pooled mean is represented by the diamond. The reported p-value for Q-statistics is necessarily 1-sided because of the use of the chi-squared distribution to test whether heterogeneity is greater than zero (i.e., a two-sided test is not applicable). No adjustments were made for multiple testing.")

        params.fig <- list(
          study = study,
          dir = control$dir.primary ,
          file = control$file.mod1.mi,
          res.dir = res.dir,
          predictor.i = predictor.i,
          outcome.i = outcome.i,
          fig.num = fig.num,
          fig.cap = fig.cap.i,
          fig.fn = fig.fn.i,
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          digits = digits
        )
        ## build plot
        #
        gfs_wave_3_build_supp_forest_plot(params.fig)
        #
        ## print to pdf/word file
        rmarkdown::render(
          input = system.file("rmd", "pdf_figures_wave_3.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_fig"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file =  here::here(res.dir, "supplement-text", paste0("cache-fig-i-",f0,".RData")),
            fig.file = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", outcome.i,"_regressed_on_", predictor.i,".pdf"))
          )
        )

        ## PDF version
        gfs_append_pdf(
          dir = res.dir,
          cur.doc = out.file.pdf,
          add = here::here(res.dir, "supplement-text", "tmp_fig.pdf")
        )


        fig.num = fig.num + 1

        remove(params.fig)
        gc() ## clean up between forest plots.
      }
    }

  }


  ## ============================================================================================== ##
  # ---- P3 Country-specific results ----
  #     - Summary statistics by wave for demographics (similar to main text Table S1)
  #     - Summary statistics by wave for outcomes (similar to Table S2)
  #     - Summary statistics by retention status of wave 1 variables (Table S3/4)
  #     - Outcome-wide results (similar to main text Table 2)
  #     - Outcome-wide E-values (similar to main text Table 3)
  # ========================= #
  if(control$what == "all" | control$what == "S3"){
    cat("Starting part 2 -- country-specific results\n")
    if(control$what == "S3"){
      tb.num <- 4
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
        select(N)
      country.n2.print <- w2.n2.print %>% ungroup() %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
        select(N)

      df.raw.country.i <- df.raw.long %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
        mutate(
          #RACE1 = factor(RACE1),
          RACE1 = droplevels(RACE),
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

      df.attr.country.i <- df.raw.attr.retained %>%
        mutate(COUNTRY = str_trim(COUNTRY)) %>%
        filter(str_detect(COUNTRY, COUNTRY_LABELS[iter])) %>%
        mutate(
          #RACE1 = factor(RACE1),
          RACE1 = droplevels(RACE),
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
      ## ======================================================================================== ##
      ## ====== Table Si-a. summary statistics -- demographics variables ======================== ##
      {

        if(control$num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }

        params.tb <- list(
          x = as.name("WAVE0"),
          data = df.raw.country.i,
          focal.variable0 = focal.variable0,
          focal.predictor0 = NULL,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_C1; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C2, Wave 3 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C3.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )
        Rglobalflourishing:::build_tbl_sample_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_portrait_25L.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_a"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sia.RData"))
          )
        )
        remove(params.tb)
        gc()
        }
      ## ======================================================================================== ##
      ## ====== Table Si-b. summary statistics -- outcome variables ============================= ##
      {
        if(control$num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Weighted summary statistics for outcome variables in ", COUNTRY_LABELS[iter])
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Weighted summary statistics for outcome variables  in ", COUNTRY_LABELS[iter])
          tb.let <- tb.let + 1
        }
        params.tb <- list(
          x = as.name("WAVE0"),
          data = df.raw.country.i,
          included.variables = tbl.row.vec0[str_detect(tbl.row.vec0, "blank", negate=TRUE)],
          OUTCOME.VEC0 = NULL,
          OUTCOME.VEC.LABELS = NULL,
          wgt = as.name("WGT0"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "Wave 1 characteristics weighted using the Gallup provided sampling weight, ANNUAL_WEIGHT_C1; Wave 2 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C2, Wave 3 characteristics weighted accounting for attrition by using the adjusted Wave 1 weight, ANNUAL_WEIGHT_C3.",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )

        Rglobalflourishing:::build_tbl_outcome_by_x(params.tb)

        rmarkdown::render(
          input = system.file("rmd", "pdf_portrait_70L.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_b"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sib.RData"))
          )
        )
        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Si-c. Unweighted summary statistics -- demo + child by retention status === ##
      {

        if(control$num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter]," by retention status")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i <- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for demographic and childhood variables in ", COUNTRY_LABELS[iter]," by retention status")
          tb.let <- tb.let + 1
        }

        params.tb <- list(
          x = as.name("WAVE0"),
          data = df.attr.country.i,
          focal.variable0 = focal.variable0,
          focal.predictor0 = NULL,
          wgt = as.name("UNITWGT"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )
        Rglobalflourishing:::build_tbl_sample_by_x(params.tb, pg.width = 8.5)

        rmarkdown::render(
          input = system.file("rmd", "pdf_portrait_25_11.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_c"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sic.RData"))
          )
        )
        remove(params.tb)
        gc()
      }
      ## ======================================================================================== ##
      ## ====== Table Si-d. Unweighted summary statistics -- outcome vars by retention status === ##
      {
        if(control$num.sequential){
          tb.cap.i <- paste0("Table S",tb.num,". Unweighted summary statistics for Wave 1 outcome variables in ", COUNTRY_LABELS[iter], " by retention status.")
          tb.num <- tb.num + 1
        } else {
          tb.cap.i<- paste0("Table S",tb.num, letters[tb.let],". Unweighted summary statistics for Wave 1 outcome variables  in ", COUNTRY_LABELS[iter], " by retention status.")
          tb.let <- tb.let + 1
        }
        params.tb <- list(
          x = as.name("WAVE0"),
          data = df.attr.country.i,
          included.variables = tbl.row.vec0[str_detect(tbl.row.vec0, "blank", negate=TRUE)],
          OUTCOME.VEC0 = NULL,
          OUTCOME.VEC.LABELS = NULL,
          wgt = as.name("UNITWGT"),
          psu = as.name("PSU"),
          strata = as.name("STRATA"),
          tb.cap = tb.cap.i ,
          fn.txt = "",
          cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData")),
          start.time = run.start.time.i,
          ignore.cache = FALSE,
          file.xlsx = here::here(res.dir, out.file.xlsx)
        )

        Rglobalflourishing:::build_tbl_outcome_by_x(params.tb, pg.width=8.5)

        rmarkdown::render(
          input = system.file("rmd", "pdf_portrait_70_11.Rmd", package = "Rglobalflourishing"),
          output_format = c("pdf_document"),
          output_file = paste0("tmp_tbl_d"),
          output_dir = here::here(res.dir, "supplement-text"),
          params = list(
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sid.RData"))
          )
        )

        remove(params.tb)
        gc()

      }
      ## ======================================================================================== ##
      ## ====== Table Si-efg. Country specific outcome wide results ============================= ##
      f0 <- 1
      for(f0 in 1:length(focal.variable)){

        # update control parameters based on focal variable ending
        if(str_detect(focal.variable[f0],, "_Y2")){
          control0[['study']] <- "outcomewide"
        } else if(str_detect(focal.variable[f0],, "_Y3")){
          control0[['study']] <- "exposurewide"
        }
        control <- get_defaults_w3(control0, filetype = "supp")
        study <- control[['study']]

        ##======================================================================================= ##
        ## Model estimated using multiple imputation
        {
          if(control$num.sequential){
            tb.num.i <- tb.num
            tb.num <- tb.num + 1
          } else {
            tb.num.i <- paste0(tb.num, letters[tb.let])
            tb.let <- tb.let + 1
          }
          if(str_detect(str_to_lower(study), "exposure") ){

            tb.cap.i <- paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " analyses of the associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 using the full panel sample (n=",n1.print,") with multiple imputation by analysis model (Model 1 vs. Model 2).")

            tmp <- ifelse(
              get_outcome_scale(focal.variable[f0]) == "cont",
              "ES, effect size measure for standardized regression coefficient, null effect is 0.00;",
              "RR, risk-ratio, null effect is 1.00;"
            )
            tbl.ft1 = paste0(tmp )
            tbl.ft2 <- ""
            tbl.ft3 = paste0("An exposure-wide analytic approach was used, and a separate model was run for each exposure. ", ifelse(get_outcome_scale(focal.variable[f0]) == "cont", "A weighted linear regression model was used to estimate an ES", "A weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR") ,".")

          }
          if(str_detect(str_to_lower(study), "outcome") ){

            tb.cap.i <- paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " analyses of the associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2  with well-being and other variables at Wave 3 using the full panel sample (n=",n1.print,") with multiple imputation by analysis model (Model 1 vs. Model 2).")

            tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
            tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
            tbl.ft2 = ""
            tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
          }

          fn.txt.i <- paste0("Notes. N(full panel sample)=", n1.print ,"; ", tbl.ft1 ," CI, confidence interval; p-value, test of the null hypothesis that the association is null; (a) item part of the Happiness & Life Satisfaction domain of the Secure Flourishing Index; (b) item part of the Physical & Mental Health domain of the Secure Flourishing Index; (c) item part of the Meaning & Purpose domain of the Secure Flourishing Index; (d) item part of the Character & Virtue domain of the Secure Flourishing Index; (e) item part of the Subjective Social Connectedness domain of the Secure Flourishing Index; (f) item part of the Financial & Material Security domain of the Secure Flourishing Index.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")


          params.tb <- list(
            is.meta = FALSE,
            study = study,
            focal.variable = focal.variable[f0],
            tbl.row.vec = control$tbl.row.vec,
            mylabels = control$mylabels,
            focal.better.name = focal.better.name[f0],
            focal.reference.value = focal.variable.reference.value[f0],
            dir.a = control$dir.primary,
            dir.b = control$dir.supp,
            file.a = control$file.mod1.mi,
            file.b = control$file.mod2.mi,
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = control$ci.bonferroni,
            p.bonferroni = control$p.bonferroni,
            p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
            tb.cap = tb.cap.i,
            header.a = "Model 1 - implicit control of baseline value of exposure",
            header.b = "Model 2 - explicit control of baseline value of exposure",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sie-",f0,".RData")),
            start.time = run.start.time,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits
          )

          gfs_wave_3_build_supp_tbl(params.tb, pg.width = 16)

          rmarkdown::render(
            input = system.file("rmd", "pdf_19_by_19.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file =  paste0("tmp_tbl_e", f0),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sie-",f0,".RData"))
            )
          )
          remove(params.tb)
          gc()
        }
        ##======================================================================================= ##
        ## Model estimated using complete-case-analysis
        {
          if(control$num.sequential){
            tb.num.i <- tb.num
            tb.num <- tb.num + 1
          } else {
            tb.num.i <- paste0(tb.num, letters[tb.let])
            tb.let <- tb.let + 1
          }
          if(str_detect(str_to_lower(study), "exposure") ){

            tb.cap.i <- paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " analyses of the associations of well-being and other variables at Wave 2 with ", str_to_lower(focal.better.name[f0]) ," at Wave 3 using only participants who completed all waves (n=",n2.print,", semi-complete-case-analysis) by analysis model (Model 1 vs. Model 2).")

            tmp <- ifelse(
              get_outcome_scale(focal.variable[f0]) == "cont",
              "ES, effect size measure for standardized regression coefficient, null effect is 0.00;",
              "RR, risk-ratio, null effect is 1.00;"
            )
            tbl.ft1 = paste0(tmp )
            tbl.ft2 <- ""
            tbl.ft3 = paste0("An exposure-wide analytic approach was used, and a separate model was run for each exposure. ", ifelse(get_outcome_scale(focal.variable[f0]) == "cont", "A weighted linear regression model was used to estimate an ES", "A weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR") ,".")

          }
          if(str_detect(str_to_lower(study), "outcome") ){

            tb.cap.i <- paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " analyses of the associations of  ", str_to_lower(focal.better.name[f0]) ," at Wave 2 with well-being and other variables at Wave 3 using only participants who completed all waves (n=",n2.print,", semi-complete-case-analysis) by analysis model (Model 1 vs. Model 2).")

            tmp <- "ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00;"
            tbl.ft1 = paste0("Reference for focal predictor: ", focal.variable.reference.value[f0],"; ", tmp )
            tbl.ft2 = ""
            tbl.ft3 = paste0("An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.")
          }

          fn.txt.i <- paste0("Notes. N(complete-case sample)=", n1.print ,"; ", tbl.ft1 ," ES, effect size; SE, standard error; CI, confidence interval; p-value, test of the null hypothesis that the association is null. Flourishing domains, depression symptoms, anxiety symptoms, and religion/spirituality variables included here as supplements results in addition to those reported in the main text.

Reliability corrected estimates (r, reliability) is assessed at three values for the mostly single-item assessed used: high at r=0.70, moderate r=0.55, and low r=0.40. The estimates of effect sizes are disattenuated for unreliability using Fisher's method. Sensitivity of null effects to unreliability identified by the highest level of reliability needed to make the effect non-null.

Analysis based on a 'complete-case-analysis' where we subset to only those participants whoc ompleted all 3 waves. Remaining within wave missingness accounted for using multiple imputation. Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

",tbl.ft3,"

P-value thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(control$p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(control$ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-control$p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.")


          params.tb <- list(
            is.meta = FALSE,
            study = study,
            focal.variable = focal.variable[f0],
            tbl.row.vec = control$tbl.row.vec,
            mylabels = control$mylabels,
            focal.better.name = focal.better.name[f0],
            focal.reference.value = focal.variable.reference.value[f0],
            dir.a = control$dir.supp,
            dir.b = control$dir.supp,
            file.a = control$file.mod1.cca,
            file.b = control$file.mod2.cca,
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = control$ci.bonferroni,
            p.bonferroni = control$p.bonferroni,
            p.ci = ifelse(control$ci.bonferroni, control$p.bonferroni, 0.05),
            tb.cap = tb.cap.i,
            header.a = "Model 1 - implicit control of baseline value of exposure",
            header.b = "Model 2 - explicit control of baseline value of exposure",
            fn.txt = fn.txt.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif-",f0,".RData")),
            start.time = run.start.time,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits
          )

          gfs_wave_3_build_supp_tbl(params.tb, pg.width = 16)

          rmarkdown::render(
            input = system.file("rmd", "pdf_19_by_19.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file =  paste0("tmp_tbl_f", f0),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sif-",f0,".RData"))
            )
          )
          remove(params.tb)
          gc()
        }
        ## ====================================================================================== ##
        ## Country Specific E-values output table =============================== ##
        {

          if(control$num.sequential){
            tb.num.i <- tb.num
            tb.num <- tb.num + 1
          } else {
            tb.num.i <- paste0(tb.num, letters[tb.let])
            tb.let <- tb.let + 1
          }

          if(str_detect(str_to_lower(study), "exposure") ){
            tbl.ft1 = "exposure at Wave 2"

            tb.cap.i <-  paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " sensitivity analysis of ", str_to_lower(focal.better.name[f0])," exposure-wide results to unmeasured confounding using E-values across models and how missingness at Wave 2 was handled.")

          }
          if(str_detect(str_to_lower(study), "outcome") ){
            tbl.ft1 = "outcome at Wave 3"
            tb.cap.i <-  paste0("Table S",tb.num.i,". ", COUNTRY_LABELS[iter], " sensitivity analysis of ", str_to_lower(focal.better.name[f0])," outcome-wide results to unmeasured confounding using E-values across models and how missingness at Wave 2 was handled.")
          }

          tb.fn.i <- paste0("Notes. EE, E-value for estimate; ECI, E-value for the limit of the confidence interval; Model 1, primary analysis model where all non demographic variable included through the use of principal components analyses; Model 2, supplemental model where wave 1 value of the varying ",tbl.ft1," included explicitly as a control variable and not through the principal components. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

          params.tb <- list(
            is.meta = FALSE,
            study = study,
            focal.variable = focal.variable[f0],
            focal.better.name = focal.better.name[f0],
            focal.predictor.reference.value = focal.variable.reference.value[f0],
            tbl.row.vec = control$tbl.row.vec,
            mylabels = control$mylabels,
            dir.a = control$dir.primary,
            dir.b = control$dir.sup,
            dir.c = control$dir.supp,
            dir.d = control$dir.supp,
            file.a = control$file.mod1.mi,
            file.b = control$file.mod2.mi,
            file.c = control$file.mod1.cca,
            file.d = control$file.mod2.cca,
            header.a = "Model 1",
            header.b = "Model 2",
            header.c = "Model 1",
            header.d = "Model 2",
            country.i = COUNTRY_LABELS[iter],
            ci.bonferroni = control$ci.bonferroni,
            p.bonferroni = control$p.bonferroni,
            p.ci = 0.05,
            tb.cap = tb.cap.i,
            fn.txt = tb.fn.i,
            cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData")),
            start.time = run.start.time,
            ignore.cache = FALSE,
            file.xlsx = here::here(res.dir, out.file.xlsx),
            digits = digits
          )
          gfs_wave_3_build_supp_tbl_evalues(params.tb)

          rmarkdown::render(
            input = system.file("rmd", "pdf_19_by_11.Rmd", package = "Rglobalflourishing"),
            output_format = c("pdf_document"),
            output_file = paste0("tmp_tbl_g", f0),
            output_dir = here::here(res.dir, "supplement-text"),
            params = list(
              cache.file = here::here(res.dir, "supplement-text", paste0("cache-tb-sig-",f0,".RData"))
            )
          )

          remove(params.tb)
          gc()
        }

      }
      ## ======================================================================================== ##
      ## ====== Print out tables to formatted PDF document ===================================== ##
      {

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
      if(!control$num.sequential){
        tb.num <- tb.num + 1
      }
    }
    cat("Part 3 complete.\n")
  }

  cat("\n **Complete.**\n")

}



#' @keywords internal
gfs_wave_3_build_supp_tbl <- function(params, font.name = "Open Sans", font.size = 10, pg.width = 20){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  is.meta = params$is.meta
  study = params$study
  focal.variable = params$focal.variable
  tbl.row.vec = params$tbl.row.vec
  mylabels = params$mylabels
  focal.better.name = params$focal.better.name
  focal.reference.value = params$focal.reference.value
  dir.a = params$dir.a
  dir.b = params$dir.b
  file.a = params$file.a
  file.b = params$file.b
  country.i = params$country.i
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  p.ci = params$p.ci
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  header.a = params$header.a
  header.b = params$header.b
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

  COUNTRY_LABELS <- sort(c(
    "Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil", "Mexico", "China"
  ) )

  if(is.meta){
    vec.col <- c('outcome', 'term', 'theta.rma', 'theta.rma.se', 'theta.lb', 'theta.ub', 'tau', 'I2', 'prob.leqneq0.1', 'prob.geq0.1', 'theta.pred.int.lb', 'theta.pred.int.ub', 'rr.tau', 'rr.prob.0.90', 'rr.prob.1.10', 'rr.theta.pred.int', 'global.pvalue', "calibrated.yi", "sens.reliability")

    df.a <- load_meta_result(
      file = here::here(dir.a,file.a),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vec.col,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )

    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vec.col,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )

    if( str_detect(str_to_lower(study), "exposure") ){
      meta.filter.var = as.name("term")
      # get outcome scale -- determines which columns are printed out
      tmp.vec <- case_when(
        get_outcome_scale(focal.variable) == "cont" ~ "ES",
        get_outcome_scale(focal.variable) != "cont" ~ "RR",
        .default = ""
      )
      cnames <- c(tmp.vec, "SE", "95% CI", "\r", "Pred. Int.", "%-Metric", "\u03c4", "Global p-value", "\r\r\r", "High (r=0.70)", "Moderate (r=0.55)", "Low (r=0.40)")
      cnames <- c("Exposure", cnames, "\r\r\r\r\r", paste0(cnames, "\r"))

      cols.a <- c(2:4,6:9,11:13)
      cols.b <- c(15:17,19:22,24:26)

    }
    if(str_detect(str_to_lower(study), "outcome") ){
      meta.filter.var = as.name("outcome")
      # get outcome scale -- determines which columns are printed out
      cnames <- c("ES", "RR", "SE", "95% CI", "\r", "Pred. Int.", "%-Metric", "\u03c4", "Global p-value", "\r\r\r", "High (r=0.70)", "Moderate (r=0.55)", "Low (r=0.40)")
      cnames <- c("Outcome", cnames, "\r\r\r\r\r", paste0(cnames, "\r"))
      cols.a <- c(2:5,7:10,12:14)
      cols.b <- c(16:19,21:24,26:28)
    }

  } else {

    vec.col <- c('outcome', 'term', 'data')

    df.a <- load_meta_result(
      file = here::here(dir.a,file.a),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vec.col,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    ) |> unnest(c(data))  |>
      filter(group == country.i) |>
      mutate(
        sens.reliability = pmap(list(yi,sei), \(x,y){
          reliability_corrected_estimates(
            theta=as.numeric(x),
            se=as.numeric(y),
            crit = qnorm(1-p.ci/2),
            lambda = c(0.40,0.55, 0.70)
          )
        })
      )

    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vec.col,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )|> unnest(c(data)) |>
      filter(group == country.i) |>
      mutate(
        sens.reliability = pmap(list(yi,sei), \(x,y){
          reliability_corrected_estimates(
            theta=as.numeric(x),
            se=as.numeric(y),
            crit = qnorm(1-p.ci/2),
            lambda = c(0.40,0.55, 0.70)
          )
        })
      ) |>
      filter(group == country.i)

    if( str_detect(str_to_lower(study), "exposure") ){
      filter.var = as.name("term")
      # get outcome scale -- determines which columns are printed out
      tmp.vec <- case_when(
        get_outcome_scale(focal.variable) == "cont" ~ "ES",
        get_outcome_scale(focal.variable) != "cont" ~ "RR",
        .default = ""
      )
      cnames <- c(tmp.vec, "SE", "95% CI", "p-value", "\r", "High (r=0.70)", "Moderate (r=0.55)", "Low (r=0.40)")
      cnames <- c("Exposure", cnames, "\r\r\r", paste0(cnames, "\r"))
      cols.a <- c(2:5,7:9)
      cols.b <- c(11:14,16:18)
    }
    if(str_detect(str_to_lower(study), "outcome") ){
      filter.var = as.name("outcome")
      # get outcome scale -- determines which columns are printed out
      cnames <- c("ES", "RR", "SE", "95% CI", "p-value", "\r", "High (r=0.70)", "Moderate (r=0.55)", "Low (r=0.40)")
      cnames <- c("Outcome",cnames, "\r\r\r", paste0(cnames, "\r"))
      cols.a <- c(2:6,8:10)
      cols.b <- c(12:16,18:20)
    }

  }

  # need to add whitespace to the end of these columns so that flextable doesn't throw the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.


  if( str_detect(str_to_lower(study), "exposure") ){
    is.cont <- get_outcome_scale(focal.variable) == "cont"
  }

  outcomewide <- as.data.frame(matrix(nrow = length(tbl.row.vec), ncol = length(cnames)))
  colnames(outcomewide) <- cnames
  i = ii = 1
  for (i in 1:length(tbl.row.vec)) {
    if (stringr::str_detect(tbl.row.vec[i], "blank") ) {
      outcomewide[i, 1] <- str_trim(mylabels[ii])
      ii <- ii + 1
    } else {
      outcomewide[i, 1] = paste0("    ",get_outcome_better_name(tbl.row.vec[i], include.name = FALSE, include.fid = TRUE))

      if(!is.meta){
        if ( (str_detect(tbl.row.vec[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(tbl.row.vec[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(tbl.row.vec[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(tbl.row.vec[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }



      if(str_detect(str_to_lower(study), "outcome")){
        is.cont = get_outcome_scale(tbl.row.vec[i]) == "cont"
      }


      ## ====== Panel A ======================================= ##
      try({
        if(is.meta){
          tmp.a <- df.a %>%
            filter({{meta.filter.var}} == tbl.row.vec[i])
          tmp.a <- tmp.a %>%
            mutate(
              est = case_when(
                is.cont ~ theta.rma,
                !is.cont ~ exp(theta.rma)
              ),
              se = .round(theta.rma.se, digits),
              ci = if(is.cont){
                paste0("(",.round(theta.lb, digits),", ",.round(theta.ub, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(theta.lb), digits),", ",.round(exp(theta.ub), digits),")")
              } else {NA},
              prop.metric = if(is.cont){
                paste0(.round(prob.leqneq0.1*100, min(0,digits-2)),"% | ", .round(prob.geq0.1*100, min(0,digits-2)),"%")} else if(!is.cont){
                  paste0(.round(rr.prob.0.90*100, min(0,digits-2)),"% | ", .round(rr.prob.1.10*100, min(0,digits-2)),"%")} else {NA},
              prop.metric = pad_around_divider(prop.metric, "|"),
              pred.int = if(is.cont){
                paste0("(",.round(theta.pred.int.lb, digits),", ",.round(theta.pred.int.ub, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(theta.pred.int.lb), digits),", ",.round(exp(theta.pred.int.ub), digits),")")
              } else {NA},
              tau = case_when(
                is.cont ~ tau,
                !is.cont ~ rr.tau
              ),
              tau =  case_when(
                tau < 0.01 ~ "<0.01\u2020",
                tau >= 0.01 ~ .round(tau,digits)
              ),
              dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                try({
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                    x < 0.005 ~ paste0(.round_p(x),"**"),
                    x < 0.05 ~ paste0(.round(x,3),"*"),
                    x > 0.05 ~ .round(x,3)
                  )
                })
              }),
              sens.reliability = map(sens.reliability, \(x){
                x |>
                  mutate(
                    out = if(is.cont){
                      paste0( .round(corrected.theta, digits),
                              " (", .round(corrected.theta.lb, digits),", ",
                              .round(corrected.theta.ub, digits),")" )
                    } else if(!is.cont){
                      paste0( .round(exp(corrected.theta), digits),
                              " (", .round(exp(corrected.theta.lb), digits),", ",
                              .round(exp(corrected.theta.ub), digits),")" )
                    } else {NA}
                  )
              }),
              rcor70 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.70) |> select(out) |> as.character()
              }),
              rcor55 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.55) |> select(out) |> as.character()
              }),
              rcor40 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.40) |> select(out) |> as.character()
              }),
              across(where(is.numeric), ~.round(., digits))
            ) |>
            select(est, se, ci, pred.int,prop.metric,tau,global.pvalue,rcor70, rcor55, rcor40)
        } else {
          tmp.a <- df.a %>%
            filter({{filter.var}} == tbl.row.vec[i])
          tmp.a <- tmp.a %>%
            mutate(
              est = case_when(
                is.cont ~ yi,
                !is.cont ~ exp(yi)
              ),
              se = .round(sei, digits),
              ci = if(is.cont){
                paste0("(",.round(ci.lb.i, digits),", ",.round(ci.ub.i, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(ci.lb.i), digits),", ",.round(exp(ci.ub.i), digits),")")
              } else {NA},
              dplyr::across(tidyr::any_of(c("pvalue")),\(x){
                try({
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                    x < 0.005 ~ paste0(.round_p(x),"**"),
                    x < 0.05 ~ paste0(.round(x,3),"*"),
                    x > 0.05 ~ .round(x,3)
                  )
                })
              }),
              sens.reliability = map(sens.reliability, \(x){
                x |>
                  mutate(
                    out = if(is.cont){
                      paste0( .round(corrected.theta, digits),
                              " (", .round(corrected.theta.lb, digits),", ",
                              .round(corrected.theta.ub, digits),")" )
                    } else if(!is.cont){
                      paste0( .round(exp(corrected.theta), digits),
                              " (", .round(exp(corrected.theta.lb), digits),", ",
                              .round(exp(corrected.theta.ub), digits),")" )
                    } else {NA}
                  )
              }),
              rcor70 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.70) |> select(out) |> as.character()
              }),
              rcor55 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.55) |> select(out) |> as.character()
              }),
              rcor40 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.40) |> select(out) |> as.character()
              }),
              across(where(is.numeric), ~.round(., digits))
            ) |>
            select(est, se, ci, pvalue, rcor70, rcor55, rcor40)
        }
      })
      ## ====== Panel B ======================================= ##
      try({
        if(is.meta){
          tmp.b <- df.b %>%
            filter({{meta.filter.var}} == tbl.row.vec[i])
          tmp.b <- tmp.b  %>%
            mutate(
              est = case_when(
                is.cont ~ theta.rma,
                !is.cont ~ exp(theta.rma)
              ),
              se = .round(theta.rma.se, digits),
              ci = if(is.cont){
                paste0("(",.round(theta.lb, digits),", ",.round(theta.ub, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(theta.lb), digits),", ",.round(exp(theta.ub), digits),")")
              } else {NA},
              prop.metric = if(is.cont){
                paste0(.round(prob.leqneq0.1*100, min(0,digits-2)),"% | ", .round(prob.geq0.1*100, min(0,digits-2)),"%")} else if(!is.cont){
                  paste0(.round(rr.prob.0.90*100, min(0,digits-2)),"% | ", .round(rr.prob.1.10*100, min(0,digits-2)),"%")} else {NA},
              prop.metric = pad_around_divider(prop.metric, "|"),
              pred.int = if(is.cont){
                paste0("(",.round(theta.pred.int.lb, digits),", ",.round(theta.pred.int.ub, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(theta.pred.int.lb), digits),", ",.round(exp(theta.pred.int.ub), digits),")")
              } else {NA},
              tau = case_when(
                is.cont ~ tau,
                !is.cont ~ rr.tau
              ),
              tau =  case_when(
                tau < 0.01 ~ "<0.01\u2020",
                tau >= 0.01 ~ .round(tau,digits)
              ),
              dplyr::across(tidyr::any_of(c("global.pvalue")),\(x){
                try({
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                    x < 0.005 ~ paste0(.round_p(x),"**"),
                    x < 0.05 ~ paste0(.round(x,3),"*"),
                    x > 0.05 ~ .round(x,3)
                  )
                })
              }),
              sens.reliability = map(sens.reliability, \(x){
                x |>
                  mutate(
                    out = if(is.cont){
                      paste0( .round(corrected.theta, digits),
                              " (", .round(corrected.theta.lb, digits),", ",
                              .round(corrected.theta.ub, digits),")" )
                    } else if(!is.cont){
                      paste0( .round(exp(corrected.theta), digits),
                              " (", .round(exp(corrected.theta.lb), digits),", ",
                              .round(exp(corrected.theta.ub), digits),")" )
                    } else {NA}
                  )
              }),
              rcor70 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.70) |> select(out) |> as.character()
              }),
              rcor55 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.55) |> select(out) |> as.character()
              }),
              rcor40 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.40) |> select(out) |> as.character()
              }),
              across(where(is.numeric), ~.round(., digits))
            ) |>
            select(est, se, ci, pred.int,prop.metric,tau,global.pvalue,rcor70, rcor55, rcor40)

        } else {
          tmp.b <- df.b %>%
            filter({{filter.var}} == tbl.row.vec[i])
          tmp.b <- tmp.b %>%
            mutate(
              est = case_when(
                is.cont ~ yi,
                !is.cont ~ exp(yi)
              ),
              se = .round(sei, digits),
              ci = if(is.cont){
                paste0("(",.round(ci.lb.i, digits),", ",.round(ci.ub.i, digits),")")
              } else if(!is.cont){
                paste0("(",.round(exp(ci.lb.i), digits),", ",.round(exp(ci.ub.i), digits),")")
              } else {NA},
              dplyr::across(tidyr::any_of(c("pvalue")),\(x){
                try({
                  case_when(
                    x < p.bonferroni ~ paste0(.round_p(x),"***"),
                    x < 0.005 ~ paste0(.round_p(x),"**"),
                    x < 0.05 ~ paste0(.round(x,3),"*"),
                    x > 0.05 ~ .round(x,3)
                  )
                })
              }),
              sens.reliability = map(sens.reliability, \(x){
                x |>
                  mutate(
                    out = if(is.cont){
                      paste0( .round(corrected.theta, digits),
                              " (", .round(corrected.theta.lb, digits),", ",
                              .round(corrected.theta.ub, digits),")" )
                    } else if(!is.cont){
                      paste0( .round(exp(corrected.theta), digits),
                              " (", .round(exp(corrected.theta.lb), digits),", ",
                              .round(exp(corrected.theta.ub), digits),")" )
                    } else {NA}
                  )
              }),
              rcor70 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.70) |> select(out) |> as.character()
              }),
              rcor55 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.55) |> select(out) |> as.character()
              }),
              rcor40 = map_chr(sens.reliability, \(x){
                x |> filter(reliability == 0.40) |> select(out) |> as.character()
              }),
              across(where(is.numeric), ~.round(., digits))
            ) |>
            select(est, se, ci, pvalue, rcor70, rcor55, rcor40)
        }
      })
      ## ====== Add Results to output object ====================================================== ##

      try({
        if(nrow(tmp.a) > 0){
          if(str_detect(str_to_lower(study), "exposure")){
            outcomewide[i, cols.a] <- tmp.a
          }
          if(str_detect(str_to_lower(study), "outcome")){
            if(get_outcome_scale(tbl.row.vec[i]) == "cont"){
              outcomewide[i,cols.a[-1]] <- tmp.a
            }
            if(get_outcome_scale(tbl.row.vec[i]) != "cont"){
              outcomewide[i,cols.a[-2]] <- tmp.a
            }
          }
        }
        if(nrow(tmp.b) > 0){
          if(str_detect(str_to_lower(study), "exposure")){
            outcomewide[i, cols.b] <- tmp.b
          }
          if(str_detect(str_to_lower(study), "outcome")){
            if(get_outcome_scale(tbl.row.vec[i]) == "cont"){
              outcomewide[i,cols.b[-1]] <- tmp.b
            }
            if(get_outcome_scale(tbl.row.vec[i]) != "cont"){
              outcomewide[i,cols.b[-2]] <- tmp.b
            }
          }
        }
      })
    }
  }

  if(is.meta){
    ft.header.values <- c("", "Pooled Estimate", "", "Heterogeneity","", "Reliability Corrected Est. (95% CI)",
                          "", "Pooled Estimate","", "Heterogeneity","", "Reliability Corrected Est. (95% CI)")
    if(str_detect(str_to_lower(study), "exposure") ){
      ft.header.lengths <- c(1, 3, 1,4,1, 3, 1, 3,1, 4,1, 3)
      cwh.a <- cwh.b <- 12
    } else if(str_detect(str_to_lower(study), "outcome") ){
      ft.header.lengths <-  c(1, 4, 1,4,1, 3, 1, 4,1, 4,1, 3)
      cwh.a <- cwh.b <- 13
    }

  } else {
    ft.header.values <- c("", "Estimates", "", "Reliability Corrected Est. (95% CI)",
                          "", "Estimates", "", "Reliability Corrected Est. (95% CI)")
    if(str_detect(str_to_lower(study), "exposure") ){
      ft.header.lengths <- c(1, 4, 1,3, 1, 4,1, 3)
      cwh.a <- cwh.b <- 8
    } else if(str_detect(str_to_lower(study), "outcome") ){
      ft.header.lengths <- c(1, 5, 1,3, 1, 5,1, 3)
      cwh.a <- cwh.b <- 9
    }

  }


  # footnote information:
  tb.note.outcomewide <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(tbl.row.vec, "blank"))),
           j = 1) %>%
    add_footer_lines(
      values = tb.note.outcomewide, top = FALSE
    ) %>%
    add_header_row(
      values = ft.header.values,
      colwidths = ft.header.lengths
    ) %>%
    add_header_row(
      values = c("", header.a, "", header.b),
      colwidths = c(1, cwh.a, 1, cwh.b)
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_supp_meta_wave_3(study = study, is.meta=is.meta, pg.width = pg.width) %>%
    autofit()

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @keywords internal
gfs_wave_3_build_supp_tbl_evalues <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  study = params$study
  is.meta = params$is.meta
  focal.variable = params$focal.variable
  focal.better.name = params$focal.better.name
  tbl.row.vec = params$tbl.row.vec
  mylabel= params$mylabel
  dir.a = params$dir.a
  dir.b = params$dir.b
  dir.c = params$dir.c
  dir.d = params$dir.d
  file.a = params$file.a
  file.b = params$file.b
  file.c = params$file.c
  file.d = params$file.d
  header.a = params$header.a
  header.b = params$header.b
  header.c = params$header.c
  header.d = params$header.d
  country.i = params$country.i
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  p.ci = params$p.ci
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

  COUNTRY_LABELS <- sort(c(
    "Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil", "Mexico", "China"
  ) )

  if(is.meta){
    vec.id <- c("theta.rma.EE", "theta.rma.ECI")
    vec.rr <- c("rr.theta.EE", "rr.theta.ECI")
  } else {
    vec.id <- c("EE", "ECI")
  }

  vec.a <- c("EE", "ECI")
  vec.b <- c("EE\r", "ECI\r")
  vec.c <- c("EE\r\r", "ECI\r\r")
  vec.d <- c("EE\r\r\r", "ECI\r\r\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Outcome",
    vec.a, "\r\r\r", vec.b, "\r\r",
    vec.c, "\r", vec.d
  )

  ## pre-load in slow objects
  if(is.meta){
    vars.to.get <- c(unique(c(vec.id, vec.rr)), "term", "outcome")
    df.a <- load_meta_result(
      file = here::here(dir.a, file.a),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )
    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )
    df.c <- load_meta_result(
      file = here::here(dir.c, file.c),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )
    df.d <- load_meta_result(
      file = here::here(dir.d, file.d),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )
  } else {
    vars.to.get <- c('data', "term", "outcome")
    df.a <- load_meta_result(
      file = here::here(dir.a, file.a),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    ) |> unnest(c(data)) |>
      filter(group == country.i)

    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )|> unnest(c(data)) |>
      filter(group == country.i)

    df.c <- load_meta_result(
      file = here::here(dir.c, file.c),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )|> unnest(c(data)) |>
      filter(group == country.i)

    df.d <- load_meta_result(
      file = here::here(dir.d, file.d),
      predictor = unique(c(focal.variable, tbl.row.vec)),
      outcome = unique(c(focal.variable, tbl.row.vec)),
      what = vars.to.get,
      filter.var.out = "outcome",
      filter.var.pred = "term"
    )|> unnest(c(data)) |>
      filter(group == country.i)

  }

  # depending on the study, it changes which variable we use to "filter" within the loop
  if( str_detect(str_to_lower(study), "exposure") ){
    filter.var = as.name("term")
  }
  if(str_detect(str_to_lower(study), "outcome") ){
    filter.var = as.name("outcome")
  }
  evalues <- as.data.frame(matrix(nrow = length(tbl.row.vec), ncol = length(cnames)))
  colnames(evalues) <- cnames
  evalues$"\r" <- ""
  evalues$"\r\r" <- ""
  evalues$"\r\r\r" <- ""
  i = ii = 1
  for (i in 1:length(tbl.row.vec)) {
    if (stringr::str_detect(tbl.row.vec[i], "blank") ) {
      evalues[i, 1] <- mylabel[ii]
      ii <- ii + 1
    } else {
      evalues[i, 1] = paste0("    ",get_outcome_better_name(tbl.row.vec[i], include.name = FALSE))

      if(!is.meta){
        if ( (str_detect(tbl.row.vec[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(tbl.row.vec[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(tbl.row.vec[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(tbl.row.vec[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }

      if(is.meta){
        tmp.vec <- case_when(
          get_outcome_scale(tbl.row.vec[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
      } else {
        tmp.vec <- vec.id
      }
      ## ====== Primary MI  ====================== ##
      tmp.a <- df.a %>%
        filter({{filter.var}} == tbl.row.vec[i]) %>%
        select(all_of(tmp.vec)) %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Primary MI - random effects meta - estimates WITH PCs ========================= ##
      tmp.b <- df.b %>%
        filter({{filter.var}} == tbl.row.vec[i]) %>%
        select(all_of(tmp.vec)) %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Supplement ATTR WGT - random effects meta - estimates withOUT PCs ================= ##
      tmp.c <- df.c %>%
        filter({{filter.var}} == tbl.row.vec[i]) %>%
        select(all_of(tmp.vec)) %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Supplement ATTR WGT - random effects meta - estimates WITH PCs ==================== ##
      tmp.d <- df.d %>%
        filter({{filter.var}} == tbl.row.vec[i]) %>%
        select(all_of(tmp.vec)) %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.a) > 0) evalues[i,vec.a] <- tmp.a[tmp.vec]
      if(nrow(tmp.b) > 0) evalues[i,vec.b] <- tmp.b[tmp.vec]
      if(nrow(tmp.c) > 0) evalues[i,vec.c] <- tmp.c[tmp.vec]
      if(nrow(tmp.d) > 0) evalues[i,vec.d] <- tmp.d[tmp.vec]
    }
  }

  print.tb <- evalues %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(tbl.row.vec, "blank"))),
           j = 1) %>%
    add_header_row(
      values = c("", header.a, "", header.b, "", header.c, "", header.d),
      colwidths = c(1, length(vec.a), 1, length(vec.b), 1, length(vec.c), 1, length(vec.d)),
      top = TRUE
    ) %>%
    add_header_row(
      values = c("", "Multiple Imputation", "", "Complete Case w/ Attrition Weights" ),
      colwidths = c(1, length(vec.a)+1+length(vec.b), 1, length(vec.c)+1+length(vec.d)),
      top = TRUE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    add_footer_lines(
      as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9))), top = FALSE
    ) %>%
    width(j=1,width=3.5)%>%
    width(j=c(2,5,8,11),width=0.75)%>%
    width(j=c(3,6,9,12),width=1.0)%>%
    width(j=c(4,7,10),width=0.10)%>%
    format_flex_table(pg.width = 9) %>%
    align(i = 2:3, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:ncol(evalues)) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    hline_bottom(part = "header") %>%
    hline(i=1, part="header") %>%
    hline(i=2,j=c(2:6,8:12), part="header") %>%
    hline(i=3,j=c(2:3,5:6,8:9,11:12), part="header")


  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @keywords internal
gfs_wave_3_build_supp_forest_plot <- function(params, ...) {

  set_flextable_defaults(font.family = "Open Sans",font.size = 10)

  study = params$study
  dir = params$dir
  file = params$file
  res.dir = params$res.dir
  predictor.i = params$predictor.i
  outcome.i = params$outcome.i
  fig.cap = params$fig.cap
  fig.fn = params$fig.fn
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  digits = params$digits
  fig.num = params$fig.num

  ALL.COUNTRIES <- c("Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil",    "Mexico",  "China"  )

  df.main <- load_meta_result(
    file = here::here(dir, file),
    predictor = predictor.i,
    outcome = outcome.i ,
    filter.var.out = "outcome",
    filter.var.pred = "term"
  )
  df.main <- df.main |>
    filter(outcome == outcome.i, term == predictor.i)

  # meta fit objects
  fit <- df.main$meta.rma[[1]]
  fit.influence <- meta_loo_inf(fit)
  # plot data
  plot_df <- df.main$data[[1]]

  # identify countries omitted from meta-analysis
  tmp.included.countries <- plot_df$group
  tmp.included.countries <- str_replace(tmp.included.countries, "_", " ")
  tmp.included.countries <- str_trim(tmp.included.countries, "both")
  tmp.excluded.countries <- ALL.COUNTRIES[!(ALL.COUNTRIES %in% tmp.included.countries)]
  tmp.excluded.countries <- ifelse(
    !is_empty(tmp.excluded.countries),
    paste0("Excluded countries: ", paste0(tmp.excluded.countries, collapse = ", ")),
    ""
  )

  ## construct heterogeneity statements
  build_het_statement <- function(fit, txt=""){
    myci <- confint(fit, type = "PL")
    paste0(txt,
           "Heterogeneity (tau)=", .round(sqrt(fit$tau2), max(digits,3)),
           "; Q-profile 95% CI [", .round(myci$random[2, 2], max(digits,3)), ", ", .round(myci$random[2, 3], max(digits,3)), "]",
           "; Q(df=", fit$k - fit$QMdf[1], ")=",
           .round(fit$QE), ", p=", format.pval(fit$QEp, digits=max(digits,3), scientific=TRUE),
           "; I^2=", .round(fit$I2, digits),
           ifelse(tmp.excluded.countries == "","", ";\n"), tmp.excluded.countries
    )
  }
  tmp.het <- build_het_statement(fit)

  # make sure to use the (*)i variables in data so that the correct estimates are being plotted.\
  # Noah: I switch the ordering to be by the overly conservative estimates with PC control
  plot_df <- plot_df |>
    mutate(
      est_lab = paste0(.round(yi,digits), " (", .round(ci.lb.i,digits), ", ", .round(ci.ub.i,digits), ")")
    )

  plot_df <- left_join(plot_df,  fit.influence, by = "group") |>
    mutate(
      loo_label = paste0(.round(loo.estimate, digits), " (", .round(loo.ci.lb,digits), ", ", .round(loo.ci.ub,digits), ")"),
      inf_label = paste0("RESID(stud) = ", .round(rstudent,2),"; Cooks' D = ",.round(cook.d, 2),"; COVRATIO = ",.round(cov.r, 2))
    )


  # make sure bounds also contains 0
  xlims <- c(min(plot_df$ci.lb.i) - .05,max(plot_df$ci.ub.i) + .05)
  xlims[1] <- ifelse(xlims[1] > -0.05, -0.05, xlims[1])
  xlims[2] <- ifelse(xlims[2] < 0.05, 0.05, xlims[2])

  # DATA FOR PLOT
  dat.below <- data.frame(
    label = "Overall",
    est = as.numeric(fit$b),
    ci.lb = as.numeric(fit$ci.lb),
    ci.ub = as.numeric(fit$ci.ub)
  ) |>
    mutate(
      ci = paste0("(", .round(ci.lb), ",", .round(ci.ub), ")"),
      CI = paste0(.round(est), " ", ci)
    )
  # below are the actual plots being constructed...
  p_mid <- plot_df |>
    ggplot(aes(y = reorder(group, yi))) +
    geom_point(aes(x = yi), shape = 15, size = 3) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = dat.below$est[1], linetype = "dashed", color="grey50") +
    .geom_stripes() +
    labs(x = NULL) +
    lims(x = c(xlims)) +
    theme_classic() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(face = "bold"),
      axis.line.y = element_blank(),
      axis.text.x = element_blank(),
      plot.background = element_rect(
        colour = "grey90",
        linewidth = 1,
        fill = "grey90"
      )
    )

  # right side of plot - estimates
  p_right <- plot_df  |>
    ggplot(aes(y = reorder(group, yi))) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    .geom_stripes() +
    theme_void() +
    theme(
      panel.background = element_rect(fill="white"),
      plot.background = element_rect(
        colour = "grey90" ,
        linewidth = 1,
        fill = "grey90"
      )
    )

  p_below <- dat.below %>%
    ggplot(aes(x = est, y = label)) +
    geom_point(shape = 18, size = 5) +
    geom_linerange(aes(xmin = ci.lb, xmax = ci.ub)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = dat.below$est[1], linetype = "dashed", color="grey50") +
    labs(x = "Effect Size Measure", y = NULL) +
    lims(x = c(xlims)) +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(face = "bold"),
      plot.background = element_rect(
        colour = "grey90" ,
        linewidth = 1,
        fill = "grey90"
      )

    )

  p_below_right <-
    dat.below |>
    ggplot(aes(y = label)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    labs(y=NULL, x = "Estimates")+
    theme_void() +
    theme(
      axis.title.x = element_text(),
      panel.background = element_rect(fill="white"),
      plot.background = element_rect(
        colour = "grey90" ,
        linewidth = 1,
        fill = "grey90"
      )
    )

  p_right_loo <- plot_df  |>
    ggplot(aes(y = reorder(group, yi))) +
    geom_text(aes(x = 0, label =loo_label), hjust = 0.45) +
    .geom_stripes() +
    theme_void()

  p_right_inf <- plot_df  |>
    ggplot(aes(y = reorder(group, yi))) +
    geom_text(aes(x = 0, label = inf_label), hjust = 0.50) +
    .geom_stripes() +
    theme_void()

  p_below_right_loo <-
    dat.below |>
    ggplot(aes(y = label)) +
    geom_text(aes(x = 0, label = "LOO RMA Estimate"), hjust = 0.45) +
    theme_void()

  p_below_right_inf <-
    dat.below |>
    ggplot(aes(y = label)) +
    geom_text(aes(x = 0, label = "Influence Statistics"), hjust = 0.45) +
    theme_void()


  p <- (p_mid + plot_spacer() + p_right+ plot_spacer() +p_right_loo+ plot_spacer() + p_right_inf +
          plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer()+
          p_below + plot_spacer() + p_below_right + plot_spacer() + p_below_right_loo + plot_spacer() + p_below_right_inf) +
    plot_layout(
      byrow = TRUE,
      widths = c(4, -0.25, 1.75, -0.15,1.75, -0.25,4),
      heights = c(10, -0.5, 1)
    ) +
    plot_annotation(caption = tmp.het)
  p
  ggsave(
    filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", outcome.i,"_regressed_on_", predictor.i,".pdf")),
    plot = p, height = 6.5, width = 13.5, units = "in"
  )
  ggsave(
    filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"_", outcome.i,"_regressed_on_", predictor.i,".png")),
    plot = p, height = 6.5, width = 13.5, units = "in", dpi = 250
  )

  save(p, fig.cap, fig.fn, file = cache.file)


}

#' @keywords internal
gfs_wave_3_build_supp_wide_tbl <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  study = params$study
  focal.variable = params$focal.variable
  tbl.row.vec = params$tbl.row.vec
  mylabels = params$mylabels
  tbl.footnote = params$tb.fn
  tbl.title = params$tb.cap
  tbl.num = params$tb.num
  dir = params$dir
  file = params$file
  cache.file = params$cache.file
  file.xlsx = params$file.xlsx
  digits = params$digits

  if( !( str_detect(str_to_lower(study), "exposure") | str_detect(str_to_lower(study), "outcome")) ){
    stop("STUDY TYPE ERROR. CANNOT CONSTRUCT META SUMMARY TABLE.")
  }
  ## columns to keep from meta-analysis results object
  vec.col <- c('outcome', 'term', 'data')

  df.main <- load_meta_result(
    file = here::here(dir, file),
    predictor = unique(c(focal.variable, tbl.row.vec)),
    outcome = unique(c(focal.variable, tbl.row.vec)),
    what = vec.col,
    filter.var.out = "outcome",
    filter.var.pred = "term"
  ) |>
    unnest(c(data)) |>
    select(all_of(c('outcome.scale', 'outcome', 'term', 'group', 'yi')))

  if(str_detect(study, "exposure")){
    # only 1 scale, so just transform if needed
    is.rr <- df.main$outcome.scale[1] != "cont"
    if(is.rr){
      df.main$yi <- exp(df.main$yi)
    }

    df.main <- df.main |>
      pivot_wider(
        names_from ='group',
        values_from = "yi"
      )

  }



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
          prop.metric = pad_around_divider(prop.metric, "|"),
          pred.int = case_when(
            is.cont ~ paste0("(",.round(theta.pred.int.lb, digits),", ",.round(theta.pred.int.ub, digits),")"),
            !is.cont ~ paste0("(",.round(exp(theta.pred.int.lb), digits),", ",.round(exp(theta.pred.int.ub), digits),")")
          ),
          tau = case_when(
            is.cont ~ tau,
            !is.cont ~ rr.tau
          ),
          tau =  case_when(
            tau < 0.001 ~ "<0.001\u2020",
            tau >= 0.001 ~ .round(tau,max(digits,3))
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
