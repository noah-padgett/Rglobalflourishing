#' Main text tables for Wave 2 Outcome-wide Analyses
#'
#' Generate the caches table object for printing: Main text summary table
#'
#' @param params a list of parameters that were originally passed as parameters in the .Rmd files. Kept for legacy and to reduce need to rewrite code.
#' @param font.name "Open Sans"
#' @param font.size 10
#'
#' @export
#' @rdname build-functions
build_tbl_1 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  df.raw.long = params$df.raw.long
  focal.predictor0 = params$focal.predictor0
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
          any_of(focal.predictor0),
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
          all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {p25}, {p75}⁠, {max}"),
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          all_continuous() ~ c(1,1,1,1,1,1),
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
        label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
      )
  })

  tb.note.summarytab <- as_paragraph(as_chunk("Note. N (%); this table is based on non-imputed data; cumulative percentages for variables may not add up to 100% due to rounding; S.A.R., Special Administrative Region. Expanded summary tables of all demographic characteristics and outcome variables are provided the online supplement in Tables S1-2 aggregated over the full sample and Tables S9a-32a and S9b-32b are summary tables by country.", props = fp_text_default(font.family = "Open Sans", font.size = 9)))


  print.tb <- sumtab %>%
    as_flex_table() %>%
    autofit() %>%
    format_flex_table(pg.width = 21 / 2.54 - 2) %>%
    add_header_lines(as_paragraph(
      as_chunk(paste0("Table ", tb.num ,". Weighted sample demographic summary statistics."),
               props = fp_text_default(font.family = "Open Sans", font.size = 11))
    )) %>%
    add_footer_row(
      values = tb.note.summarytab, top = FALSE,colwidths=3
    )

  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_2 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  focal.predictor.reference.value = params$focal.predictor.reference.value
  dir = params$dir
  file.wopc = params$file.wopc
  file.wpc = params$file.wpc
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  tb.num = params$tb.num
  n.print = params$n.print
  cache.file = params$cache.file
  start.time = params$start.time
  digits = params$digits
  include.cor = params$include.cor
  file.cor = params$file.cor

  vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue")
  vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue")
  if(ci.bonferroni){
    vec.id <- c("theta.rma", "theta.rma.ci.bon", "tau","global.pvalue")
    vec.rr <- c("rr.theta", "rr.theta.ci.bon", "rr.tau","global.pvalue")
  }
  vec.wopc <- c("RR", "ES","95% CI","\u03c4", "Global p-value")
  vec.wpc <- c("RR\r", "ES\r","95% CI\r","\u03c4\r", "Global p-value\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Outcome",
    vec.wopc,
    "\r",
    vec.wpc
  )

  df.wopc <- load_meta_result(
    file = here::here(dir, file.wopc),
    predictor = focal.predictor,
    outcome = OUTCOME.VEC,
    what = c("OUTCOME0", unique(c(vec.id, vec.rr)), "theta.lb", "theta.ub")
  )
  df.wpc <- load_meta_result(
    file = here::here(dir, file.wpc),
    predictor = focal.predictor,
    outcome = OUTCOME.VEC,
    what = c("OUTCOME0", unique(c(vec.id, vec.rr)), "theta.lb", "theta.ub")
  )
  if(include.cor){
    cnames <- c(
      "Outcome", "r", "\r\r",
      vec.wopc,
      "\r",
      vec.wpc
    )
    df.cor <- load_meta_result(
      file = here::here(dir, file.cor),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", "theta.rma")
    )
  }


  meta.outcomewide <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
  colnames(meta.outcomewide) <- cnames
  meta.outcomewide$"\r" <- ""
  if(include.cor){
    meta.outcomewide$"\r\r" <- ""
  }
  i = ii = 1
  for (i in 1:length(OUTCOME.VEC)) {
    if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
      meta.outcomewide[i, 1] <- MYLABEL[ii]
      ii <- ii + 1
    } else {
      meta.outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = FALSE, rm.text="Composite"))
      tmp.vec <- case_when(
        get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
        .default = vec.rr
      )
      ## ====== Random effects meta - estimates withOUT PCs ======================================= ##
      tmp.wopc <- df.wopc |>
        filter(OUTCOME0 == OUTCOME.VEC[i]) |>
        select(all_of( c(tmp.vec, "theta.lb", "theta.ub")))
      tmp.wopc <- tmp.wopc %>%
        dplyr::mutate(
          theta.rma.ci = paste0("(",.round(theta.lb, digits),",",.round(theta.ub, digits),")"),
          rr.theta.ci = paste0("(",.round(exp(theta.lb), digits),",",.round(exp(theta.ub), digits),")"),
          dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,digits)),
          dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
            case_when(
              x < 0.01 ~ "<0.01\u2020",
              x >= 0.01 ~ .round(x,digits)
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
      tmp.wpc <- df.wpc |>
        filter(OUTCOME0 == OUTCOME.VEC[i]) |>
        select(all_of(c(tmp.vec, "theta.lb", "theta.ub")))
      tmp.wpc <- tmp.wpc %>%
        dplyr::mutate(
          theta.rma.ci = paste0("(",.round(theta.lb, digits),",",.round(theta.ub, digits),")"),
          rr.theta.ci = paste0("(",.round(exp(theta.lb), digits),",",.round(exp(theta.ub), digits),")"),
          dplyr::across(tidyr::any_of(c("theta.rma", "rr.theta")),\(x) .round(x,digits)),
          dplyr::across(tidyr::any_of(c("tau", "rr.tau")),\(x){
            case_when(
              x < 0.01 ~ "<0.01\u2020",
              x >= 0.01 ~ .round(x,digits)
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
      ## ====== Random effects meta - Correlation Coefficieint ==================================== ##
      if(include.cor){
        tmp.cor <- df.cor |>
          filter(OUTCOME0 == OUTCOME.VEC[i]) |>
          mutate(
            r = .round(theta.rma, digits)
          )
        meta.outcomewide[i,"r"] <- tmp.cor["r"]
      }

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
  cor.footnote <- ""
  if(include.cor){
    cor.footnote <- paste0("r, random-effects meta-analytic average Pearson-product-moment correlation between ", focal.better.name, " and each outcome; ")
  }
  tb.note.meta.outcomewide <- as_paragraph(paste0("Notes. N=", n.print, "; Reference for focal predictor: ", focal.predictor.reference.value,"; ",cor.footnote,"RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-
printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability."))


  print.tb <- meta.outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
           j = 1) %>%
    add_footer_row(
      values = tb.note.meta.outcomewide, top = FALSE, colwidths = ncol(meta.outcomewide)
    ) %>%
    add_header_row(
      values = c("", "Model 1: Demographic and Childhood Variables as Covariates", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates"),
      colwidths = c(1,length(vec.wopc), 1, length(vec.wpc))
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(paste0("Table ", tb.num,". Meta-analyzed associations of ", focal.better.name ," at Wave 1 with well-being and other outcomes at Wave 2."),
                 props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_outcome_wide()

  save(print.tb, file=cache.file)

}

#' @export
#' @rdname build-functions
build_tbl_3 <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  dir = params$dir
  file.wopc = params$file.wopc
  file.wpc = params$file.wpc
  tb.num = params$tb.num
  n.print = params$n.print
  cache.file = params$cache.file
  start.time = params$start.time
  digits = params$digits

  vec.id <- c("theta.rma.EE", "theta.rma.ECI")
  vec.rr <- c("rr.theta.EE", "rr.theta.ECI")
  vec.wopc <- c("E-value","E-value for CI")
  vec.wpc <- c("E-value\r","E-value for CI\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Outcome",
    vec.wopc, "\r",
    vec.wpc
  )

  df.wopc <- load_meta_result(
    file = here::here(dir, file.wopc),
    predictor = focal.predictor,
    outcome = OUTCOME.VEC,
    what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
  )
  df.wpc <- load_meta_result(
    file = here::here(dir, file.wpc),
    predictor = focal.predictor,
    outcome = OUTCOME.VEC,
    what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
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
      meta.evalues[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, rm.text="Composite"))
      tmp.vec <- case_when(
        get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
        .default = vec.rr
      )
      ## ====== Random effects meta - estimates withOUT PCs =================================== ##
      tmp.wopc <- df.wopc |>
        filter(OUTCOME0 == OUTCOME.VEC[i]) |>
        select(all_of(tmp.vec))
      tmp.wopc <- tmp.wopc %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Random effects meta - estimates WITH PCs ====================================== ##
      tmp.wpc <- df.wpc |>
        filter(OUTCOME0 == OUTCOME.VEC[i]) |>
        select(all_of(tmp.vec))
      tmp.wpc <- tmp.wpc %>%
        dplyr::mutate(
          dplyr::across(where(is.numeric),\(x) .round(x,digits)),
        )
      ## ====== Add Results to output object ================================================== ##
      if(nrow(tmp.wopc) > 0) meta.evalues[i,vec.wopc] <- tmp.wopc[tmp.vec]
      if(nrow(tmp.wpc) > 0) meta.evalues[i,vec.wpc] <- tmp.wpc[tmp.vec]
    }
  }

  # footnote information:
  tb.note.evalues <-as_paragraph("Notes. N=", .round(n.print,0), "; The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.")

  print.tb <- meta.evalues %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
           j = 1) %>%
    add_footer_row(
      values = tb.note.evalues, top = FALSE, colwidths = ncol(meta.evalues)
    ) %>%
    add_header_row(
      values = c("", "Model 1: Demographic and Childhood Variables as Covariates", "", "Model 2: Demographic, Childhood, and Other Wave 1 Confounding Variables (Via Principal Components) as Covariates"),
      colwidths = c(1, length(vec.wopc), 1, length(vec.wpc))
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(paste0("Table ", tb.num ,". E-value sensitivity analysis for unmeasured confounding for the association between ", focal.better.name, " and subsequent well-being and other outcomes."),
                 props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_evalues()

  save(print.tb, file=cache.file)
}
