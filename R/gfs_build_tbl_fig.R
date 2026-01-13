#' Internal Build Table Functions
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
    meta.outcomewide <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
    colnames(meta.outcomewide) <- cnames
    meta.outcomewide$"\r" <- ""
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
  tb.note.meta.outcomewide <- as_paragraph(paste0("Notes. N=", n.print, "; Reference for focal predictor: ", focal.predictor.reference.value,"; RR, risk-ratio, null effect is 1.00; ES, effect size measure for standardized regression coefficient, null effect is 0.00; CI, confidence interval; \u03c4 (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries.

Multiple imputation was performed to impute missing data on the covariates, exposure, and outcomes. All models controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; and racial/ethnic identity when available. For Model 2 with PC (principal components), the first seven principal components of the entire set of contemporaneous confounders assessed at Wave 1 were included as additional covariates of the outcomes at Wave 2.

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized. For continuous outcomes, the ES represents the change in SD on the outcome ", ifelse(get_outcome_scale(focal.predictor) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),". For binary outcomes, the RR represents the change in risk of being in the upper category compared to the lower category ", ifelse(get_outcome_scale(focal.predictor) == "cont", "for a 1 SD increase in the focal predictor", "between the lower and upper categories of the binary focal predictor"),".

P-value significance thresholds: p < 0.05*, p < 0.005**, (Bonferroni) p < ",.round(p.bonferroni,5),"***, correction for multiple testing to significant threshold",ifelse(ci.bonferroni, paste0('; reported confidence intervals for meta-analytic estimates are based on the Bonferroni adjusted significance level to construct ', .round((1-p.bonferroni/2)*100,1),'% CIs;'), ';')," \u2020 Estimate of \u03c4 (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects."))


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
  vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
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

#' @export
#' @rdname build-functions
build_tbl_sample_by_x <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  ## extract parameters
  x = params$x
  data = params$data
  focal.predictor0 = params$focal.predictor0
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx

    ## create table
    suppressWarnings({

      sumtab <-  data %>%
        as_survey_design(
          #ids = {{psu}},
          strata = {{strata}},
          weights = {{wgt}}
        ) %>%
        tbl_svysummary(
          by = {{x}},
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
          type = list(
            AGE ~ "continuous2",
            all_continuous() ~ "continuous2"
          ),
          statistic = list(
            all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {p25}, {p75}⁠, {max}"),
            all_categorical() ~ "{n} ({p}%)"
          ),
          digits = list(
            all_continuous() ~ c(1,1,1,1,1,1),
            all_categorical() ~ list(label_style_number(digits=0), label_style_percent0(digits = 1))
          ),
          missing_text = "    (Missing)",
          missing_stat = "{N_miss} ({p_miss}%)"
        ) %>%
        add_stat_label(
          label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
        ) %>%
        modify_header(label ~ "**Characteristic**") %>%
        italicize_labels()
    })

  footnote.text <- "Note. N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding."
  footnote.text <- paste(footnote.text, fn.txt)

  tb.note.summarytab <- as_paragraph(as_chunk(footnote.text, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- sumtab %>%
    as_flex_table() %>%
    autofit() %>%
    format_flex_table(pg.width = 21 / 2.54 - 2)  %>%
    add_footer_lines(
      values = tb.note.summarytab, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
      )
    )
  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_outcome_by_x <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  ## extract parameters
  data = params$data
  x = params$x
  OUTCOME.VEC0 = params$OUTCOME.VEC0
  OUTCOME.VEC.LABELS = params$OUTCOME.VEC.LABELS
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx

  ## create table
  suppressWarnings({
  sumtab <-  data %>%
    as_survey_design(
      #ids = {{psu}},
      strata = {{strata}},
      weights = {{wgt}}
    ) %>%
    tbl_svysummary(
      by = {{x}},
      include = c(
        any_of(OUTCOME.VEC0[str_detect(OUTCOME.VEC0, "INCOME_QUINTILE", negate=TRUE)])
      ),
      label = OUTCOME.VEC.LABELS,
      type = list(
        all_continuous() ~ "continuous2",
        contains("NUM_CHILDREN") ~ "continuous2",
        contains("CIGARETTES") ~ "continuous2",
        contains("DAYS_EXERCISE") ~ "continuous2",
        contains("DRINKS") ~ "continuous2"
      ),
      statistic = list(
        all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {p25}, {p75}⁠, {max}"),
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(
        all_continuous() ~ c(1,1,1,1,1,1),
        all_categorical() ~ list(label_style_number(digits=0), label_style_percent0(digits = 1))
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

  footnote.text <- "*Note*. N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding."
  footnote.text <- paste(footnote.text, fn.txt)

  tb.note.summarytab <- as_paragraph(as_chunk(footnote.text, props = fp_text_default(font.family = "Open Sans", font.size = 9)))


print.tb <- sumtab %>%
  as_flex_table() %>%
  autofit() %>%
  width(j=2,width=1.5)%>%
  width(j=3,width=1.5)%>%
  format_flex_table(pg.width = 21 / 2.54 - 2)  %>%
  add_footer_lines(
    values = tb.note.summarytab, top = FALSE
  ) %>%
  add_header_lines(
    as_paragraph(
      as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
    )
  )

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_variable_by_x <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  ## extract parameters
  data = params$data
  x = params$x
  OUTCOME.VEC0 = params$OUTCOME.VEC0
  OUTCOME.VEC.LABELS = params$OUTCOME.VEC.LABELS
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx

  ## create table
  suppressWarnings({
    sumtab <-  data %>%
      as_survey_design(
        #ids = {{psu}},
        strata = {{strata}},
        weights = {{wgt}}
      ) %>%
      tbl_svysummary(
        by = {{x}},
        include = c( any_of(OUTCOME.VEC0) ),
        label = OUTCOME.VEC.LABELS,
        type = list(
          all_continuous() ~ "continuous2"
        ),
        statistic = list(
          all_continuous() ~ c("    {mean}", "    {sd}", "    {min}, {p25}, {p75}⁠, {max}"),
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          all_continuous() ~ c(1,1,1,1,1,1),
          all_categorical() ~ list(label_style_number(digits=0), label_style_percent0(digits = 1))
        ),
        missing_text = "    (Missing)",
        missing_stat = "{N_miss} ({p_miss}%)"
      ) %>%
      add_stat_label(
        label = all_continuous() ~ c("    Mean", "    Standard Deviation", "    Min, Max")
      ) %>%
      modify_header(label ~ "**Variable**") %>%
      italicize_labels()

  })

  footnote.text <- "*Note*. N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding."
  footnote.text <- paste(footnote.text, fn.txt)

  tb.note.summarytab <- as_paragraph(as_chunk(footnote.text, props = fp_text_default(font.family = "Open Sans", font.size = 9)))


  print.tb <- sumtab %>%
    as_flex_table() %>%
    autofit() %>%
    #width(j=2,width=1.5)%>%
    #width(j=3,width=1.5)%>%
    format_flex_table(pg.width = 21 / 2.54 - 2)  %>%
    add_footer_lines(
      values = tb.note.summarytab, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
      )
    )

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_attr_model <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  dir = params$dir
  country.i = params$country.i
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

  tmp.attr.mod <- get_fitted_attrition_model(dir, country.i)
  tmp.included.vars0 <- attr(tmp.attr.mod$terms,"term.labels")
  tmp.included.vars <- str_remove(tmp.included.vars0, "COV_")
  lab.list <- list()
  for(ii in 1:length(tmp.included.vars)){
    ## manually adjust for specific variables
    if(str_detect(tmp.included.vars0[ii], "INCOME")){
      if(country.i %in% c("United States", "Australia")){
        lab.list[[tmp.included.vars0[ii]]] = "Annual household income"
        tmp.included.vars[ii] <- "Annual household income"
      } else {
        lab.list[[tmp.included.vars0[ii]]] = "Monthly household income"
        tmp.included.vars[ii] <- "Monthly household income"
      }
    } else if(str_detect(tmp.included.vars0[ii], "ATTEND_SVCS")){
      lab.list[[tmp.included.vars0[ii]]] = "Religious attendance"
      tmp.included.vars[ii] <- "Religious attendance"
    } else if(str_detect(tmp.included.vars0[ii], "EMPLOYMENT")){
      lab.list[[tmp.included.vars0[ii]]] = "Employment status"
      tmp.included.vars[ii] <- "Employment status"
    } else {
      lab.list[[tmp.included.vars0[ii]]] = get_outcome_better_name(tmp.included.vars[ii], include.name = FALSE)
      tmp.included.vars[ii] <- get_outcome_better_name(tmp.included.vars[ii], include.name = FALSE)
    }


  }
  suppressWarnings({
  attr.model = tbl_regression(
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
    modify_header(estimate = "**Odds Ratio**")
})
  tb.note <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <-  attr.model |>
    as_flex_table() |>
    autofit() |>
    format_flex_table(pg.width = 21 / 2.54 - 2) |>
    add_footer_lines(
      values = tb.note, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
      )
    )

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_country_point_estimates <- function(params, font.name = "Open Sans", font.size = 9){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  dir = params$dir
  res.dir = params$res.dir
  OUTCOME.VEC = params$OUTCOME.VEC
  focal.predictor = params$focal.predictor
  file = params$file
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  mylabels = params$mylabels
  countries.included = params$countries.included
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

  vec.get <- c("OUTCOME0", "FOCAL_PREDICTOR", "theta.rma", "rr.theta")

  nC <- 1 + 2*length(countries.included)

  df.tmp <- get_country_specific_output(
    res.dir = dir,
    outcomes =  OUTCOME.VEC[str_detect(OUTCOME.VEC,"blank",negate=TRUE)],
    predictors = focal.predictor,
    appnd.txt = file,
    replace.cntry.file.start = replace.cntry.file.start,
    keep.terms = "FOCAL_PREDICTOR"
  ) %>%
    #filter(Variable == focal.predictor) %>%
    select(COUNTRY, OUTCOME, std.estimate.pooled, id.Std.Est, rr.Std.Est) |>
    arrange(COUNTRY)


  for(i in 1:nrow(df.tmp)){
    if(length(df.tmp$OUTCOME[i]) > 0){
      if(get_outcome_scale(df.tmp$OUTCOME[i]) == "cont"){
        df.tmp$id.Std.Est[i] <- .round(df.tmp$std.estimate.pooled[i], digits)
        df.tmp$rr.Std.Est[i] <- NA
      }
      if(get_outcome_scale(df.tmp$OUTCOME[i]) != "cont"){
        df.tmp$id.Std.Est[i] <- NA
        df.tmp$rr.Std.Est[i] <- .round(exp(df.tmp$std.estimate.pooled[i]), digits)
      }

      df.tmp$OUTCOME[i] <- get_outcome_better_name(df.tmp$OUTCOME[i], FALSE, FALSE, include.fid = TRUE)
    }
  }
  df.tmp.wide <- df.tmp %>%
    select(COUNTRY, OUTCOME, id.Std.Est, rr.Std.Est) %>%
    pivot_wider(
      names_from = COUNTRY,
      values_from = c(id.Std.Est, rr.Std.Est)
    )

  df.tmp.wide2 <- df.tmp.wide[, c(1, c(rbind(2:( (nC-1)/2 +1 ), ((nC-1)/2+2):nC)) )]

  df.tmp.wide2[is.na(df.tmp.wide2)] <- "-"

  df.tmp0 <- as.data.frame(matrix(ncol=ncol(df.tmp.wide2), nrow=length(OUTCOME.VEC)))
  colnames(df.tmp0) <- colnames(df.tmp.wide2)
  i <- ii <- 1
  for(i in seq_along(OUTCOME.VEC)){
    if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
      df.tmp0[i, 1] <- mylabels[ii]
      ii <- ii + 1
    } else {
      df.tmp0[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = TRUE))
      tmp.dat <- subset(df.tmp.wide2, str_trim(df.tmp.wide2$OUTCOME) == str_trim(df.tmp0[i, 1]))
      df.tmp0[i, -1] = tmp.dat[,-1]
    }
  }

  df.tmp <- df.tmp0
  header_l1 <- unique(str_split(colnames(df.tmp), "_",simplify = T)[-1,2])
  colnames(df.tmp) <- c("Outcome", rep(c("ES", "RR"), length(header_l1)))
  for(i in 2:length(colnames(df.tmp))){
    colnames(df.tmp)[i] <- paste0(colnames(df.tmp)[i], paste0(rep("\r",i-2), collapse = ""))
  }

  # create comparable files but as CSV
  readr::write_csv(df.tmp0, file=here::here(res.dir, paste0("GFS_Outcomewide_Results_Comparison_", paste0(focal.predictor, collapse="_"),".csv")))


  tb.note <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))


ft <- df.tmp %>%
  flextable() %>%
  italic(part = "body",
         i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
         j = 1) %>%
  autofit() %>%
  format_flex_table(pg.width = 30) %>%
  width(j=1,width=3.5)%>%
  width(j=2:ncol(df.tmp), width = 0.55)%>%
  add_header_row(
    values = c("", header_l1),
    colwidths = c(1, rep(2, length(header_l1)))
  ) %>%
  bg(
    j = c(seq(2,ncol(df.tmp),4), seq(3,ncol(df.tmp),4)), bg="grey90",part = "all"
  ) %>%
  add_footer_lines(
    values = tb.note, top = FALSE
  ) %>%
  add_header_lines(
    as_paragraph(
      as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
    )
  ) %>%
  hline(i=1, part = "header") %>%
  align(i=2, align = "center", part = "header")

  print.tb  <- width( ft, width = dim(ft)$widths * 30/ (flextable_dim(ft)$widths) )


  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_evalues <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  is.meta = params$is.meta
  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  focal.predictor.reference.value = params$focal.predictor.reference.value
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
  n.print = params$n.print
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

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

  if(is.meta){
    vec.id <- c("theta.rma.EE", "theta.rma.ECI")
    vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
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
    df.a <- load_meta_result(
      file = here::here(dir.a, file.a),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
    )
    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
    )
    df.c <- load_meta_result(
      file = here::here(dir.c, file.c),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
    )
    df.d <- load_meta_result(
      file = here::here(dir.d, file.d),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", unique(c(vec.id, vec.rr)))
    )
  }

  evalues <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
  colnames(evalues) <- cnames
  evalues$"\r" <- ""
  evalues$"\r\r" <- ""
  evalues$"\r\r\r" <- ""
  i = ii = 1
  for (i in 1:length(OUTCOME.VEC)) {
    if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
      evalues[i, 1] <- MYLABEL[ii]
      ii <- ii + 1
    } else {
      evalues[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE))

      if(!is.meta){
        if ( (str_detect(OUTCOME.VEC[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(OUTCOME.VEC[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(OUTCOME.VEC[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(OUTCOME.VEC[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }

      if(is.meta){
        tmp.vec <- case_when(
          get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
      } else {
        tmp.vec <- vec.id
      }
      ## ====== Primary MI - random effects meta - estimates withOUT PCs ====================== ##
      if(is.meta){
        tmp.a <- df.a %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(all_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.a <- get_country_specific_regression_results(
          res.dir = dir.a,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.a,
          replace.cntry.file.start = replace.cntry.file.start
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Primary MI - random effects meta - estimates WITH PCs ========================= ##
      if(is.meta){
        tmp.b <- df.b %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(all_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.b <- get_country_specific_regression_results(
          res.dir = dir.b,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.b,
          replace.cntry.file.start = replace.cntry.file.start
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Supplement ATTR WGT - random effects meta - estimates withOUT PCs ================= ##
      if(is.meta){
        tmp.c <- df.c %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(all_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }  else {
        tmp.c <- get_country_specific_regression_results(
          res.dir = dir.c,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.c,
          replace.cntry.file.start = replace.cntry.file.start
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Supplement ATTR WGT - random effects meta - estimates WITH PCs ==================== ##
      if(is.meta){
        tmp.d <- df.d %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(all_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.d <- get_country_specific_regression_results(
          res.dir = dir.d,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.d,
          replace.cntry.file.start = replace.cntry.file.start
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.a) > 0) evalues[i,vec.a] <- tmp.a[tmp.vec]
      if(nrow(tmp.b) > 0) evalues[i,vec.b] <- tmp.b[tmp.vec]
      if(nrow(tmp.c) > 0) evalues[i,vec.c] <- tmp.c[tmp.vec]
      if(nrow(tmp.d) > 0) evalues[i,vec.d] <- tmp.d[tmp.vec]
    }
  }


  # footnote information:
  tb.note.evalues <- as_paragraph(as_chunk("Notes. EE, E-value for estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.", props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- evalues %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
           j = 1) %>%
    add_footer_lines(
      values = tb.note.evalues, top = FALSE
    ) %>%
    add_header_row(
      values = c("", "Model 1: Demographics and Childhood Variables as Covariates", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (Via Principal Components) as Covariates", "", "Model 1: Demographics and Childhood Variables as Covariates", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (Via Principal Components) as Covariates"),
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


#' @export
#' @rdname build-functions
build_tbl_outcomewide <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  is.meta = params$is.meta
  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  focal.predictor.reference.value = params$focal.predictor.reference.value
  dir.a = params$dir.a
  dir.b = params$dir.b
  file.a = params$file.a
  file.b = params$file.b
  country.i = params$country.i
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  p.ci = params$p.ci
  n.print = params$n.print
  tb.cap = params$tb.cap
  header.a = params$header.a
  header.b = params$header.b
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start

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

  if(is.meta){
    vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue", "calibrated.yi")
    vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue", "theta.rma.se")
    vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue", "theta.rma.se")
    vec.a <- c("RR", "ES","95% CI","\u03c4", "Global p-value")
    vec.b <- c("RR\r", "ES\r","95% CI\r","\u03c4\r", "Global p-value\r")
  } else {
    vec.id <- c("id.Est","id.CI", "id.SE","p.value")
    vec.rr <- c("rr.Est", "rr.CI", "logrr.SE","p.value")
    vec.a <- c("RR", "ES", "95% CI", "SE", "p-value")
    vec.b <- c("RR\r", "ES\r", "95% CI\r", "SE\r", "p-value\r")
  }
  # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Outcome",
    vec.a,
    "\r",
    vec.b
  )

  ## pre-load in slow objects
  if(is.meta){
    df.a <- load_meta_result(
      file = here::here(dir.a, file.a),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", vec.get)
    )
    df.b <- load_meta_result(
      file = here::here(dir.b, file.b),
      predictor = focal.predictor,
      outcome = OUTCOME.VEC,
      what = c("OUTCOME0", vec.get)
    )
  }

  outcomewide <- as.data.frame(matrix(nrow = length(OUTCOME.VEC), ncol = length(cnames)))
  colnames(outcomewide) <- cnames
  outcomewide$"\r" <- ""
  i = ii = 1
  for (i in 1:length(OUTCOME.VEC)) {
    if (stringr::str_detect(OUTCOME.VEC[i], "blank") ) {
      outcomewide[i, 1] <- MYLABEL[ii]
      ii <- ii + 1
    } else {
      outcomewide[i, 1] = paste0("    ",get_outcome_better_name(OUTCOME.VEC[i], include.name = FALSE, include.fid = TRUE))

      if(!is.meta){
        if ( (str_detect(OUTCOME.VEC[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(OUTCOME.VEC[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(OUTCOME.VEC[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(OUTCOME.VEC[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }

      tmp.vec <- case_when(
        get_outcome_scale(OUTCOME.VEC[i]) == "cont" ~ vec.id,
        .default = vec.rr
      )
      ## ====== Panel A ======================================= ##
      if(is.meta){
        tmp.a <- df.a %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(-OUTCOME0)
        tmp.a <- tmp.a %>%
          dplyr::mutate(
            theta.rma.ci = paste0(
              "(",.round(theta.rma - qnorm(1-p.ci/2)*theta.rma.se, digits), ",",
              .round(theta.rma + qnorm(1-p.ci/2)*theta.rma.se, digits) ,")"
            ),
            rr.theta.ci = paste0(
              "(",.round(exp(theta.rma - qnorm(1-p.ci/2)*theta.rma.se), digits), ",",
              .round(exp(theta.rma + qnorm(1-p.ci/2)*theta.rma.se), digits) ,")"
            ),
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
      } else {
        tmp.a <- get_country_specific_regression_results(
          res.dir = dir.a,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.a,
          replace.cntry.file.start = replace.cntry.file.start
        )
        tmp.a <- tmp.a %>%
          dplyr::mutate(
            id.Est = .round(std.estimate.pooled, digits),
            id.SE = .round(std.se.pooled, digits),
            id.CI = paste0(
              "(",.round(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled, digits), ",",
              .round(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled, digits) ,")"
            ),
            rr.Est = .round(exp(std.estimate.pooled), digits),
            logrr.SE = .round(std.se.pooled, digits),
            rr.CI = paste0(
              "(",.round(exp(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled), digits), ",",
              .round(exp(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled), digits) ,")"
            ),
            dplyr::across(tidyr::any_of(c("p.value")),\(x){
              case_when(
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
      }
      ## ====== Panel B ======================================= ##
      if(is.meta){
        tmp.b <- df.b %>%
          filter(OUTCOME0 == OUTCOME.VEC[i]) %>%
          select(-OUTCOME0)
        tmp.b <- tmp.b %>%
          dplyr::mutate(
            theta.rma.ci = paste0(
              "(",.round(theta.rma - qnorm(1-p.ci/2)*theta.rma.se, digits), ",",
              .round(theta.rma + qnorm(1-p.ci/2)*theta.rma.se, digits) ,")"
            ),
            rr.theta.ci = paste0(
              "(",.round(exp(theta.rma - qnorm(1-p.ci/2)*theta.rma.se), digits), ",",
              .round(exp(theta.rma + qnorm(1-p.ci/2)*theta.rma.se), digits) ,")"
            ),
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
      } else {
        tmp.b <- get_country_specific_regression_results(
          res.dir = dir.b,
          country = country.i,
          predictor = focal.predictor,
          outcome =  OUTCOME.VEC[i],
          appnd.txt = file.b,
          replace.cntry.file.start = replace.cntry.file.start
        )

        tmp.b <- tmp.b %>%
          dplyr::mutate(
            id.Est = .round(std.estimate.pooled, digits),
            id.SE = .round(std.se.pooled, digits),
            id.CI = paste0(
              "(",.round(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled, digits), ",",
              .round(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled, digits) ,")"
            ),
            rr.Est = .round(exp(std.estimate.pooled), digits),
            logrr.SE = .round(std.se.pooled, digits),
            rr.CI = paste0(
              "(",.round(exp(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled), digits), ",",
              .round(exp(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled), digits) ,")"
            ),
            dplyr::across(tidyr::any_of(c("p.value")),\(x){
              case_when(
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
      }
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.a) > 0){
        if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
          outcomewide[i,vec.a[-1]] <- tmp.a[tmp.vec[1:4]]
        }
        if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
          outcomewide[i,vec.a[-2]] <- tmp.a[tmp.vec[1:4]]
        }
      }
      if(nrow(tmp.b) > 0){
        if(get_outcome_scale(OUTCOME.VEC[i]) == "cont"){
          outcomewide[i,vec.b[-1]] <- tmp.b[tmp.vec[1:4]]
        }
        if(get_outcome_scale(OUTCOME.VEC[i]) != "cont"){
          outcomewide[i,vec.b[-2]] <- tmp.b[tmp.vec[1:4]]
        }
      }
    }
  }

  # footnote information:
  tb.note.outcomewide <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
           j = 1) %>%
    add_footer_lines(
      values = tb.note.outcomewide, top = FALSE
    ) %>%
    add_header_row(
      values = c("", header.a, "", header.b),
      colwidths = c(1,length(vec.a), 1, length(vec.b))
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_outcome_wide()

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_predictorwide <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  is.meta = params$is.meta
  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  PREDICTOR.VEC = params$PREDICTOR.VEC
  dir.a = params$dir.a
  dir.b = params$dir.b
  file.a = params$file.a
  file.b = params$file.b
  country.i = params$country.i
  ci.bonferroni = params$ci.bonferroni
  p.bonferroni = params$p.bonferroni
  p.ci = params$p.ci
  n.print = params$n.print
  tb.cap = params$tb.cap
  header.a = params$header.a
  header.b = params$header.b
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits

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

  if(is.meta){
    vec.get <- c("theta.rma", "theta.rma.se", "tau","global.pvalue", "rr.theta", "rr.theta.se", "rr.tau","global.pvalue")
    vec.id <- c("theta.rma", "theta.rma.ci","tau","global.pvalue", "theta.rma.se")
    vec.rr <- c("rr.theta", "rr.theta.ci","rr.tau","global.pvalue", "theta.rma.se")
    vec.a <- c("RR", "ES","95% CI","\u03c4", "Global p-value")
    vec.b <- c("RR\r", "ES\r","95% CI\r","\u03c4\r", "Global p-value\r")
  } else {
    vec.id <- c("id.Est","id.CI", "id.SE","p.value")
    vec.rr <- c("rr.Est", "rr.CI", "logrr.SE","p.value")
    vec.a <- c("RR", "ES", "95% CI", "SE", "p-value")
    vec.b <- c("RR\r", "ES\r", "95% CI\r", "SE\r", "p-value\r")
  }
  # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Exposure",
    vec.a,
    "\r",
    vec.b
  )

  zz.scale = get_outcome_scale(OUTCOME.VEC[1])
    cnames = case_when(
      zz.scale == "cont" ~ cnames[str_detect(cnames, "RR", negate=TRUE)],
      zz.scale != "cont" ~ cnames[str_detect(cnames, "ES", negate=TRUE)]
    )

  outcomewide <- as.data.frame(matrix(nrow = length(PREDICTOR.VEC), ncol = length(cnames)))
  colnames(outcomewide) <- cnames
  outcomewide$"\r" <- ""
  i = ii = 1
  for (i in 1:length(PREDICTOR.VEC)) {
    if (stringr::str_detect(PREDICTOR.VEC[i], "blank") ) {
      outcomewide[i, 1] <- MYLABEL[ii]
      ii <- ii + 1
    } else {
      outcomewide[i, 1] = paste0("    ",get_outcome_better_name(PREDICTOR.VEC[i], include.name = FALSE, include.fid = TRUE))

      if(!is.meta){
        if ( (str_detect(PREDICTOR.VEC[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(PREDICTOR.VEC[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(PREDICTOR.VEC[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(PREDICTOR.VEC[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }

      tmp.vec <- case_when(
        get_outcome_scale(OUTCOME.VEC[1]) == "cont" ~ vec.id,
        .default = vec.rr
      )
      ## ====== Panel A ======================================= ##
      if(is.meta){
        tmp.a <- load_meta_result(
          file = here::here(dir.a, file.a),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = vec.get
        )
        tmp.a <- tmp.a %>%
          dplyr::mutate(
            theta.rma.ci = paste0(
              "(",.round(theta.rma - qnorm(1-p.ci/2)*theta.rma.se, digits), ",",
              .round(theta.rma + qnorm(1-p.ci/2)*theta.rma.se, digits) ,")"
            ),
            rr.theta.ci = paste0(
              "(",.round(exp(theta.rma - qnorm(1-p.ci/2)*theta.rma.se), digits), ",",
              .round(exp(theta.rma + qnorm(1-p.ci/2)*theta.rma.se), digits) ,")"
            ),
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
      } else {
        tmp.a <- get_country_specific_regression_results(
          res.dir = dir.a,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.a
        )
        tmp.a <- tmp.a %>%
          dplyr::mutate(
            id.Est = .round(std.estimate.pooled, digits),
            id.SE = .round(std.se.pooled, digits),
            id.CI = paste0(
              "(",.round(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled, digits), ",",
              .round(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled, digits) ,")"
            ),
            rr.Est = .round(exp(std.estimate.pooled), digits),
            logrr.SE = .round(std.se.pooled, digits),
            rr.CI = paste0(
              "(",.round(exp(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled), digits), ",",
              .round(exp(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled), digits) ,")"
            ),
            dplyr::across(tidyr::any_of(c("p.value")),\(x){
              case_when(
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
      }
      ## ====== Panel B ======================================= ##
      if(is.meta){
        tmp.b <- load_meta_result(
          file = here::here(dir.b, file.b),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = vec.get
        )
        tmp.b <- tmp.b %>%
          dplyr::mutate(
            theta.rma.ci = paste0(
              "(",.round(theta.rma - qnorm(1-p.ci/2)*theta.rma.se, digits), ",",
              .round(theta.rma + qnorm(1-p.ci/2)*theta.rma.se, digits) ,")"
            ),
            rr.theta.ci = paste0(
              "(",.round(exp(theta.rma - qnorm(1-p.ci/2)*theta.rma.se), digits), ",",
              .round(exp(theta.rma + qnorm(1-p.ci/2)*theta.rma.se), digits) ,")"
            ),
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
      } else {
        tmp.b <- get_country_specific_regression_results(
          res.dir = dir.b,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.b
        )

        tmp.b <- tmp.b %>%
          dplyr::mutate(
            id.Est = .round(std.estimate.pooled, digits),
            id.SE = .round(std.se.pooled, digits),
            id.CI = paste0(
              "(",.round(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled, digits), ",",
              .round(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled, digits) ,")"
            ),
            rr.Est = .round(exp(std.estimate.pooled), digits),
            logrr.SE = .round(std.se.pooled, digits),
            rr.CI = paste0(
              "(",.round(exp(std.estimate.pooled - qnorm(1-p.ci/2)*std.se.pooled), digits), ",",
              .round(exp(std.estimate.pooled + qnorm(1-p.ci/2)*std.se.pooled), digits) ,")"
            ),
            dplyr::across(tidyr::any_of(c("p.value")),\(x){
              case_when(
                x < p.bonferroni ~ paste0(.round_p(x),"***"),
                x < 0.005 ~ paste0(.round_p(x),"**"),
                x < 0.05 ~ paste0(.round(x,3),"*"),
                x > 0.05 ~ .round(x,3)
              )
            })
          )
      }
      ## ====== Add Results to output object ====================================================== ##
      zz = case_when(
        zz.scale == "cont" ~ 1,
        zz.scale != "cont" ~ 2
      )
        if(nrow(tmp.a) > 0){
          outcomewide[i,vec.a[-zz]] <- tmp.a[tmp.vec[1:4]]
        }
        if(nrow(tmp.b) > 0){
          outcomewide[i,vec.b[-zz]] <- tmp.b[tmp.vec[1:4]]
        }
    }
  }

  # footnote information:
  tb.note.outcomewide <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- outcomewide %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(PREDICTOR.VEC, "blank"))),
           j = 1) %>%
    add_footer_lines(
      values = tb.note.outcomewide, top = FALSE
    ) %>%
    add_header_row(
      values = c("", header.a, "", header.b),
      colwidths = c(1,length(vec.a)-1, 1, length(vec.b)-1)
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    theme_meta_predictor_wide()

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_predictorwide_evalues <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  is.meta = params$is.meta
  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  PREDICTOR.VEC = params$PREDICTOR.VEC
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
  n.print = params$n.print
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits

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

  if(is.meta){
    vec.id <- c("theta.rma.EE", "theta.rma.ECI")
    vec.rr <- c("theta.rr.EE", "theta.rr.ECI")
  } else {
    vec.id <- c("EE", "ECI")
  }

  vec.a <- c("EE", "ECI")
  vec.b <- c("EE\r", "ECI\r")
  vec.c <- c("EE\r\r", "ECI\r\r")
  vec.d <- c("EE\r\r\r", "ECI\r\r\r") # need to add whitespace to the end of these columns so that flextable doesn't through the "duplicate column keys" error (see https://stackoverflow.com/questions/50748232/same-column-names-in-flextable-in-r) for more details on other approaches.
  cnames <- c(
    "Exposure",
    vec.a, "\r\r\r", vec.b, "\r\r",
    vec.c, "\r", vec.d
  )

  evalues <- as.data.frame(matrix(nrow = length(PREDICTOR.VEC), ncol = length(cnames)))
  colnames(evalues) <- cnames
  evalues$"\r" <- ""
  evalues$"\r\r" <- ""
  evalues$"\r\r\r" <- ""
  i = ii = 1
  for (i in 1:length(PREDICTOR.VEC)) {
    if (stringr::str_detect(PREDICTOR.VEC[i], "blank") ) {
      evalues[i, 1] <- MYLABEL[ii]
      ii <- ii + 1
    } else {
      evalues[i, 1] = paste0("    ",get_outcome_better_name(PREDICTOR.VEC[i], include.name = FALSE))

      if(!is.meta){
        if ( (str_detect(PREDICTOR.VEC[i],"APPROVE_GOVT") & COUNTRY_LABELS[i] %in% c("China","Egypt") ) |
             (str_detect(PREDICTOR.VEC[i],"BELIEVE_GOD") & COUNTRY_LABELS[i] %in% c("Egypt") ) |
             (str_detect(PREDICTOR.VEC[i],"BELONGING") & COUNTRY_LABELS[i] %in% c("China") )   |
             (str_detect(PREDICTOR.VEC[i],"SAY_IN_GOVT") & COUNTRY_LABELS[i] %in% c("China") )
        ) {
          outcomewide[i, c(2:6,8:12)] <- "-"
          next
        }
      }

      if(is.meta){
        tmp.vec <- case_when(
          get_outcome_scale(PREDICTOR.VEC[i]) == "cont" ~ vec.id,
          .default = vec.rr
        )
      } else {
        tmp.vec <- vec.id
      }
      ## ====== Primary MI - random effects meta - estimates withOUT PCs ====================== ##
      if(is.meta){
        tmp.a <- load_meta_result(
          file = here::here(dir.a, file.a),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = tmp.vec
        ) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.a <- get_country_specific_regression_results(
          res.dir = dir.a,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.a
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Primary MI - random effects meta - estimates WITH PCs ========================= ##
      if(is.meta){
        tmp.b <- load_meta_result(
          file = here::here(dir.b, file.b),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = tmp.vec
        ) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.b <- get_country_specific_regression_results(
          res.dir = dir.b,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.b
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Supplement ATTR WGT - random effects meta - estimates withOUT PCs ================= ##
      if(is.meta){
        tmp.c <- load_meta_result(
          file = here::here(dir.c, file.c),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = tmp.vec
        ) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }  else {
        tmp.c <- get_country_specific_regression_results(
          res.dir = dir.c,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.c
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Supplement ATTR WGT - random effects meta - estimates WITH PCs ==================== ##
      if(is.meta){
        tmp.d <- load_meta_result(
          file = here::here(dir.d, file.d),
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          what = tmp.vec
        ) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      } else {
        tmp.d <- get_country_specific_regression_results(
          res.dir = dir.d,
          country = country.i,
          predictor = PREDICTOR.VEC[i],
          outcome = OUTCOME.VEC[1],
          appnd.txt = file.d
        ) %>%
          dplyr::select(tidyr::any_of(tmp.vec)) %>%
          dplyr::mutate(
            dplyr::across(where(is.numeric),\(x) .round(x,digits)),
          )
      }
      ## ====== Add Results to output object ====================================================== ##
      if(nrow(tmp.a) > 0) evalues[i,vec.a] <- tmp.a[tmp.vec]
      if(nrow(tmp.b) > 0) evalues[i,vec.b] <- tmp.b[tmp.vec]
      if(nrow(tmp.c) > 0) evalues[i,vec.c] <- tmp.c[tmp.vec]
      if(nrow(tmp.d) > 0) evalues[i,vec.d] <- tmp.d[tmp.vec]
    }
  }


  # footnote information:
  tb.note.evalues <- as_paragraph(as_chunk("Notes. EE, E-value for estimate; ECI, E-value for the limit of the confidence interval. The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.", props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- evalues %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(OUTCOME.VEC, "blank"))),
           j = 1) %>%
    add_footer_lines(
      values = tb.note.evalues, top = FALSE
    ) %>%
    add_header_row(
      values = c("", "Model 1: Demographics and Childhood Variables as Covariates", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (Via Principal Components) as Covariates", "", "Model 1: Demographics and Childhood Variables as Covariates", "", "Model 2: Demographics, Childhood, and Other Wave 1 Confounders (Via Principal Components) as Covariates"),
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

#' @export
#' @rdname build-functions
build_tbl_predictorwide_pnt_est_wide <- function(params, font.name = "Open Sans", font.size = 9){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  dir = params$dir
  res.dir = params$res.dir
  OUTCOME.VEC = params$OUTCOME.VEC
  mylabels = params$mylabels
  PREDICTOR.VEC = params$PREDICTOR.VEC
  file = params$file
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  countries.included = params$countries.included
  digits = params$digits

  vec.get <- c("OUTCOME0", "FOCAL_PREDICTOR", "theta.rma", "rr.theta")

  nC <- 1 + length(countries.included)

  df.tmp <- get_country_specific_output(
    res.dir = dir,
    outcomes =  OUTCOME.VEC,
    predictors = PREDICTOR.VEC[str_detect(PREDICTOR.VEC,"blank",negate=TRUE)],
    appnd.txt = file
  ) %>%
    filter(Variable == "FOCAL_PREDICTOR") %>%
    select(COUNTRY, FOCAL_PREDICTOR, id.Std.Est, rr.Std.Est) |>
    arrange(COUNTRY)

  if(get_outcome_scale(OUTCOME.VEC) == "cont"){
    df.tmp <- df.tmp %>% select(-rr.Std.Est)
  }
  if(get_outcome_scale(OUTCOME.VEC) != "cont"){
    df.tmp <- df.tmp %>% select(-id.Std.Est)
  }
  colnames(df.tmp) <- c("COUNTRY", "FOCAL_PREDICTOR", "Est")

  for(i in 1:nrow(df.tmp)){
    df.tmp$FOCAL_PREDICTOR[i] <- get_outcome_better_name(df.tmp$FOCAL_PREDICTOR[i], FALSE, FALSE, include.fid = TRUE)
  }
  df.tmp.wide <- df.tmp %>%
    select(COUNTRY, FOCAL_PREDICTOR, Est) %>%
    pivot_wider(
      names_from = COUNTRY,
      values_from = c(Est)
    )
  df.tmp.wide2 <- df.tmp.wide
  df.tmp.wide2[is.na(df.tmp.wide2)] <- "-"

  df.tmp0 <- as.data.frame(matrix(ncol=ncol(df.tmp.wide2), nrow=length(PREDICTOR.VEC)))
  colnames(df.tmp0) <- colnames(df.tmp.wide2)
  i <- ii <- 1
  for(i in seq_along(PREDICTOR.VEC)){
    if (stringr::str_detect(PREDICTOR.VEC[i], "blank") ) {
      df.tmp0[i, 1] <- mylabels[ii]
      ii <- ii + 1
    } else {
      df.tmp0[i, 1] = paste0("    ",get_outcome_better_name(PREDICTOR.VEC[i], include.name = FALSE, include.fid = TRUE))
      tmp.dat <- subset(df.tmp.wide2, str_trim(df.tmp.wide2$FOCAL_PREDICTOR) == str_trim(df.tmp0[i, 1]))
      df.tmp0[i, -1] = tmp.dat[,-1]
    }
  }

  df.tmp <- df.tmp0
  colnames(df.tmp)[1] <- c("Focal Exposure")

  # create comparable files but as CSV
  readr::write_csv(df.tmp, file=here::here(res.dir, paste0("GFS_Outcomewide_Results_Comparison_", paste0(OUTCOME.VEC, collapse="_"),".csv")))


  tb.note <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))


  ft <- df.tmp %>%
    flextable() %>%
    italic(part = "body",
           i = c(which(stringr::str_detect(PREDICTOR.VEC, "blank"))),
           j = 1) %>%
    autofit() %>%
    format_flex_table(pg.width = 20) %>%
    width(j=1,width=3.5)%>%
    width(j=2:ncol(df.tmp), width = 0.55)%>%
    bg(
      j = c(seq(2,ncol(df.tmp),2)), bg="grey90",part = "all"
    ) %>%
    add_footer_lines(
      values = tb.note, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans"))
      )
    ) %>%
    hline(i=1, part = "header") %>%
    align(i=2, align = "center", part = "header")

  print.tb  <- width( ft, width = dim(ft)$widths * 20/ (flextable_dim(ft)$widths) )

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_outcomes_exta_wide <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  ## extract parameters
  x = params$x
  y = params$y
  data = params$data
  OUTCOME.VEC0 = params$OUTCOME.VEC0
  OUTCOME.VEC.LABELS = params$OUTCOME.VEC.LABELS
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  countries.included = params$countries.included
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  stored.file = params$stored.file

  nC <- 1 + 2*length(countries.included)
  ## load in package saved memory intensive table

  load(stored.file)

  footnote.text <- "Note. N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding."
  footnote.text <- paste(footnote.text, fn.txt)

  tb.note <- as_paragraph(as_chunk(footnote.text, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- sumtab %>%
    #as_flex_table() %>%
    autofit() %>%
    format_flex_table(pg.width = 40) %>%
    bg(
      j = c(2 + seq(0,nC-2,2)), bg="grey90",part = "all"
    ) %>%
    add_footer_lines(
      values = tb.note, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
      )
    ) %>%
    hline(i=1, part = "header") %>%
    align(i=2, align = "center", part = "header")

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)

  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_sample_extra_wide <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  ## extract parameters
  x = params$x
  data = params$data
  focal.predictor0 = params$focal.predictor0
  focal.better.name = params$focal.better.name
  baseline.pred0 = params$baseline.pred0
  wgt = params$wgt
  psu = params$psu
  strata = params$strata
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  countries.included = params$countries.included
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  stored.file = params$stored.file


  nC <- 1 + 2*length(countries.included)

  ## load in package saved memory intensive table

  load(stored.file)

  footnote.text <- "Note. N (%); this table is based on non-imputed data. Cumulative percentages for variables may not add up to 100% due to rounding."
  footnote.text <- paste(footnote.text, fn.txt)

  tb.note <- as_paragraph(as_chunk(footnote.text, props = fp_text_default(font.family = "Open Sans", font.size = 9)))

  print.tb <- NULL
    print.tb <- sumtab %>%
      #as_flex_table() %>%
      autofit() %>%
      format_flex_table(pg.width = 40) %>%
      bg(
        j = c(2 + seq(0,nC-2,2)), bg="grey90",part = "all"
      ) %>%
      add_footer_lines(
        values = tb.note, top = FALSE
      ) %>%
      add_header_lines(
        as_paragraph(
          as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
        )
      ) %>%
      hline(i=1, part = "header") %>%
      align(i=2, align = "center", part = "header")
    gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}

#' @export
#' @rdname build-functions
build_tbl_pca_summary <- function(params, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  dir = params$dir
  country.i = params$country.i
  OUTCOME.VEC = params$OUTCOME.VEC
  focal.predictor = params$focal.predictor
  tb.cap = params$tb.cap
  fn.txt = params$fn.txt
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  file.xlsx = params$file.xlsx
  digits = params$digits
  replace.cntry.file.start = params$replace.cntry.file.start


  coun.fit.pca <- get_country_pca_summary(
    res.dir = dir,
    country = country.i,
    outcome = OUTCOME.VEC[str_detect(OUTCOME.VEC, "blank", negate=TRUE)][1],
    predictor = focal.predictor[1],
    appnd.txt =  "_primary_wpc",
    replace.cntry.file.start = replace.cntry.file.start
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
      across(tidyr::any_of(vec.id),\(x) .round(x*100,digits) )
    )
  coun.pca$PC <- 1:20
  coun.pca[vec.pc] <- tmp.pca[vec.id]



  tb.note <- as_paragraph(as_chunk(fn.txt, props = fp_text_default(font.family = "Open Sans", font.size = 9)))


  print.tb <- coun.pca %>%
    flextable() %>%
    add_footer_lines(
      values = tb.note, top = FALSE
    ) %>%
    add_header_lines(
      as_paragraph(
        as_chunk(tb.cap, props = fp_text_default(font.family = "Open Sans", font.size = 11))
      )
    ) %>%
    format_flex_table(pg.width = 21 / 2.54 - 4) %>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:ncol(coun.pca)) %>%
    bold(i=7,j=1:3) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    hline(i=1,part = "header") %>%
    hline_bottom(part = "header")

  gfs_append_to_xlsx(file.xlsx, print.tb, tb.cap)
  save(print.tb, file=cache.file)
}


#' @export
#' @rdname build-functions
build_fig_1 <- function(params, forest.plots.inc.est = FALSE, font.name = "Open Sans", font.size = 10){

  set_flextable_defaults(font.family = font.name,font.size = font.size)

  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  dir = params$dir
  file.wopc = params$file.wopc
  file.wpc = params$file.wpc
  fig.num = params$fig.num
  n.print = params$n.print
  res.dir <- params$res.dir
  cache.file = params$cache.file
  start.time = params$start.time
  include.estimates = params$include.estimates

  fig.cap <- paste0("**Figure ",fig.num,"**. *Heterogeneity in the effects of ", focal.better.name ," at Wave 1 on composite Secure Flourishing Index scores at Wave 2 across countries (N=", n.print, ").*\n The plot compares the estimates between Model 1, which controls for demographic and childhood variables only, and Model 2, which controls for demographic variables, childhood variables, and the entire set of Wave 1 potential confounders. The potential confounders were included using principal components. The points represent the estimated effect size in each country. The lines represented the confidence interval obtained via est+/-t(df)*SE, standard error; the overall pooled mean is represented by the points and intervals in the 'overall' row near the bottom. See our online supplemental material for more information regarding the tests of heterogeneity.")

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

  meta_res_wo <- meta_res_wo <- load_meta_result(
    file = here::here(dir, file.wopc),
    predictor = focal.predictor,
    outcome = "COMPOSITE_FLOURISHING_SECURE_Y2"
  )
  meta_res_w <- load_meta_result(
    file = here::here(dir, file.wpc),
    predictor = focal.predictor,
    outcome = "COMPOSITE_FLOURISHING_SECURE_Y2"
  )
  # meta fit objects
  fit_wo <- meta_res_wo$meta.rma[[1]]
  fit_w <- meta_res_w$meta.rma[[1]]
  # plot data
  meta_res_w$data[[1]]$model_type <- "Included"
  meta_res_wo$data[[1]]$model_type <- "Excluded"
  plot_df <- bind_rows(meta_res_w$data[[1]], meta_res_wo$data[[1]])

  # boring stuff for variable names...
  if("FOCAL_PREDICTOR" %in% colnames(plot_df)){

    focal.pred <- plot_df$FOCAL_PREDICTOR[1]
    if (!is.null(focal.better.name)) {
      focal.pred <- focal.better.name
    } else {
      focal.pred <- get_outcome_better_name(focal.pred, include.name = FALSE)
    }
    tmp.outcome.scale <- get_outcome_scale( plot_df$OUTCOME[1])
    tmp.outcome <-  plot_df$OUTCOME[1]
    tmp.outcome <- get_outcome_better_name(tmp.outcome, include.name = FALSE, include.wave = TRUE)

    focal.pred <- str_to_sentence(focal.pred)
    tmp.outcome <- str_to_sentence(tmp.outcome)

  }
  p.title = paste0("`", focal.pred, "` predicts `", tmp.outcome, "`")
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
  xLab <- case_when(
    tmp.outcome.scale == "cont" ~ "Effect Size",
    tmp.outcome.scale != "cont" ~ "log(Risk-Ratio)"
  )

  ## construct heterogeneity statements
  build_het_statement <- function(fit, txt){
    myci <- confint(fit, type = "PL")
    paste0(txt,
           "\u03c4 =", .round(sqrt(fit$tau2), 3),
           "; Q-profile 95% CI [", .round(myci$random[2, 2], 3), ", ", .round(myci$random[2, 3], 3), "]",
           "; Q(df=", fit$k - fit$QMdf[1], ")=",
           .round(fit$QE), ", p=", format.pval(fit$QEp, digits=3, scientific=TRUE),
           "; I^2=", .round(fit$I2),
           ifelse(tmp.excluded.countries == "","", ";\n"), tmp.excluded.countries
    )
  }
  tmp.het.wo <- build_het_statement(fit_wo, "PCs Excluded;")
  tmp.het.w <- build_het_statement(fit_w, "PCs Included;")

  # make sure to use the (*)i variables in data so that the correct estimates are being plotted.\
  # Noah: I switch the ordering to be by the overly conservative estimates with PC control
  plot_df <- plot_df |>
    mutate(
      Country = factor(Country, levels = Country[order(meta_res_w$data[[1]]$yi, decreasing = FALSE)], ordered=TRUE),
      est_lab = paste0(.round(yi), " (", .round(ci.lb.i), ", ", .round(ci.ub.i), ")")
    )
  # make sure bounds also contains 0
  xlims <- c(min(plot_df$ci.lb.i) - .05,max(plot_df$ci.ub.i) + .05)
  xlims[1] <- ifelse(xlims[1] > -0.05, -0.05, xlims[1])
  xlims[2] <- ifelse(xlims[2] < 0.05, 0.05, xlims[2])

  # DATA FOR PLOT
  dat.below <- data.frame(
    Country = c("Overall", "Overall"),
    yi = c(as.numeric(fit_wo$b), as.numeric(fit_w$b)),
    ci.lb.i = c(as.numeric(fit_wo$ci.lb), as.numeric(fit_w$ci.lb)),
    ci.ub.i = c(as.numeric(fit_wo$ci.ub), as.numeric(fit_w$ci.ub)),
    model_type = c("Excluded","Included")
  ) |>
    mutate(
      ci = paste0("(", .round(ci.lb.i), ",", .round(ci.ub.i), ")"),
      CI = paste0(.round(yi), " ", ci)
    )

  p_mid <- plot_df |>
    ggplot(aes(y = Country)) +
    Rglobalflourishing:::.geom_stripes() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
    geom_point(aes(x = yi, color = model_type, shape = model_type),
               position=ggstance::position_dodgev(height=0.5),
               size = 2) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                   position=ggstance::position_dodgev(height=0.5)) +
    scale_color_manual(values = c("#a6cee3", "#1f78b4")) +
    scale_shape_manual(values = c(17,16)) +
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

  p_right_w <- plot_df |> filter(model_type == "Included") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    Rglobalflourishing:::.geom_stripes() +
    theme_void()

  p_right_wo <- plot_df |> filter(model_type == "Excluded") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    Rglobalflourishing:::.geom_stripes() +
    theme_void()

  p_below <- dat.below %>%
    ggplot(aes(x = yi, y = Country)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(x = yi, color = model_type, shape = model_type, size = model_type),
               position = ggstance::position_dodgev(height = 0.5)
    ) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                   position = ggstance::position_dodgev(height = 0.5)) +
    scale_color_manual(values = c("#fcb360", "#ed6f0e")) +
    scale_shape_manual(values = c(18,15)) +
    scale_size_manual(values = c(4,3)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(),
      axis.line.y = element_blank(),
    ) +
    xlim(xlims) +
    labs(x = xLab, y = NULL)

  p_below_right_w <- dat.below |> filter(model_type == "Included") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()

  p_below_right_wo <- dat.below |> filter(model_type == "Excluded") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()


  x_dist <- xlims[2] - xlims[1]

  legend_x1 <- xlims[1] + (x_dist/20)
  legend_x2 <- xlims[1] + 11*(x_dist/20)
  ci_len <- x_dist/40
  text_x1 <- legend_x1 + x_dist/4.5
  text_x2 <- legend_x2 + x_dist/4

  legend_df <- data.frame(legend_shape = c("circ", "square", "tri", "diam"),
                          legend_color = c("darkbl", "darkor", "lightbl", "lightor"),
                          point_x = rep(c(legend_x1, legend_x2), each = 2),
                          point_y = rep(c(2, 1), times = 2)) %>%
    mutate(ci_upper = point_x + ci_len,
           ci_lower = point_x - ci_len)

  p_legend <- ggplot(legend_df, aes(x = point_x, y = point_y)) +
    geom_point(aes(shape = legend_color, color = legend_color, size = legend_color)) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, color = legend_color)) +
    scale_color_manual(values = c("#1f78b4", "#ed6f0e", "#a6cee3","#fcb360")) +
    scale_shape_manual(values = c(16, 15, 17, 18)) +
    scale_size_manual(values = c(2.5, 3, 2.5, 4)) +
    annotate("text",
             label = "With PC Controls",
             x = text_x1, y = 1.55,
             size = 10/.pt) +
    annotate("text",
             label = "Without PC Controls",
             x = text_x2, y = 1.55,
             size = 10/.pt) +
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank()) +
    xlim(xlims) +
    ylim(c(.5, 2.5))

  p_legend_w <- ggplot() +
    theme_void() +
    annotate("text", label = "With PC Controls", x = 0, y = 0)
  p_legend_wo <- ggplot() +
    theme_void() +
    annotate("text", label = "Without PC Controls", x = 0, y = 0)

  ## plot without estimates

  if(include.estimates){
    ## plot with estimates
    p <- (p_mid + plot_spacer() + p_right_w + plot_spacer() + p_right_wo +
            plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
            p_below + plot_spacer() + p_below_right_w + plot_spacer() + p_below_right_wo +
            p_legend + plot_spacer() + p_legend_w + plot_spacer() + p_legend_wo) +
      plot_layout(
        byrow = TRUE,
        widths = c(2, -0.1, 1, -0.1, 1),
        heights = c(10, -0.75, 1, -0.75, 1)
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


#' @param fit fitted results object from the metafor package (rma)
#' @param better.name a manually supplied name for the focal predictor
#' @param p.title a string passed to title of the forest plot
#' @param p.subtitle a string passed to subtitle of the forest plot.
#' @param ... additional arguments as needed
#' @return a ggplot object
#' @examples
#' # TO-DO
#'
#' @export
#' @rdname build-functions
gfs_supp_forest_plot <- function(params, plot.type = "combined", ...) {

  set_flextable_defaults(font.family = "Open Sans",font.size = 10)

  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  outcome = params$outcome
  dir = params$dir
  file.a = params$file.a
  file.b = params$file.b
  fig.num0 = params$fig.num0
  fig.cap = params$fig.cap
  n.print = params$n.print
  res.dir = params$res.dir
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache
  digits = params$digits

  if(plot.type == "panelled"){
    # panelled is the default "raw" plot for each individual outcome/analyss type
    p1 <- load_meta_result(
      file = here::here(dir, file.a),
      predictor = focal.predictor,
      outcome = outcome,
      what = "forest.plot"
    )
    p1 <- p1[[1]][[1]]
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"A_WOPC_", outcome,"_regressed_on_", focal.predictor,".png")),
      plot=p1, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"A_WOPC_", outcome,"_regressed_on_", focal.predictor,".pdf")),
      plot=p1, units="in", width=6, height=5
    )

    p2 <- load_meta_result(
      file = here::here(dir, file.b),
      predictor = focal.predictor,
      outcome = outcome,
      what = "forest.plot"
    )
    p2 <- p2[[1]][[1]]
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"B_WPC_", outcome,"_regressed_on_", focal.predictor,".png")),
      plot=p2, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"B_WPC_", outcome,"_regressed_on_", focal.predictor,".pdf")),
      plot=p2, units="in", width=6, height=5
    )


  }

  if(plot.type == "combined"){

    ALL.COUNTRIES <- c("Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil",    "Mexico",  "China"  )


    meta_res_wo <- meta_res_wo <- load_meta_result(
      file = here::here(dir, file.a),
      predictor = focal.predictor,
      outcome = outcome
    )
    meta_res_w <- load_meta_result(
      file = here::here(dir, file.b),
      predictor = focal.predictor,
      outcome = outcome
    )
    # meta fit objects
    fit_wo <- meta_res_wo$meta.rma[[1]]
    fit_w <- meta_res_w$meta.rma[[1]]
    # plot data
    meta_res_w$data[[1]]$model_type <- "Included"
    meta_res_wo$data[[1]]$model_type <- "Excluded"
    plot_df <- bind_rows(meta_res_w$data[[1]], meta_res_wo$data[[1]])

    # boring stuff for variable names...
    if("FOCAL_PREDICTOR" %in% colnames(plot_df)){

      focal.pred <- plot_df$FOCAL_PREDICTOR[1]
      if (!is.null(focal.better.name)) {
        focal.pred <- focal.better.name
      } else {
        focal.pred <- get_outcome_better_name(focal.pred, include.name = FALSE)
      }
      tmp.outcome.scale <- get_outcome_scale( plot_df$OUTCOME[1])
      tmp.outcome <-  plot_df$OUTCOME[1]
      tmp.outcome <- get_outcome_better_name(tmp.outcome, include.name = FALSE, include.wave = TRUE)

      focal.pred <- str_to_sentence(focal.pred)
      tmp.outcome <- str_to_sentence(tmp.outcome)

    }
    p.title = paste0("`", focal.pred, "` predicts `", tmp.outcome, "`")
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
    xLab <- case_when(
      tmp.outcome.scale == "cont" ~ "Effect Size",
      tmp.outcome.scale != "cont" ~ "log(Risk-Ratio)"
    )

    ## construct heterogeneity statements
    build_het_statement <- function(fit, txt){
      myci <- confint(fit, type = "PL")
      paste0(txt,
             "  \u03c4 (tau)=", .round(sqrt(fit$tau2), 3),
             "; Q-profile 95% CI [", .round(myci$random[2, 2], 3), ", ", .round(myci$random[2, 3], 3), "]",
             "; Q(df=", fit$k - fit$QMdf[1], ")=",
             .round(fit$QE), ", p=", format.pval(fit$QEp, digits=3, scientific=TRUE),
             "; I^2=", .round(fit$I2),
             ifelse(tmp.excluded.countries == "","", ";\n"), tmp.excluded.countries
      )
    }
    tmp.het.wo <- build_het_statement(fit_wo, "PCs Excluded;")
    tmp.het.w <- build_het_statement(fit_w, "PCs Included;")

    # make sure to use the (*)i variables in data so that the correct estimates are being plotted.\
    # Noah: I switch the ordering to be by the overly conservative estimates with PC control
    plot_df <- plot_df |>
      mutate(
        Country = factor(Country, levels = Country[order(meta_res_w$data[[1]]$yi, decreasing = FALSE)], ordered=TRUE),
        est_lab = paste0(.round(yi), " (", .round(ci.lb.i), ", ", .round(ci.ub.i), ")")
      )
    # make sure bounds also contains 0
    xlims <- c(min(plot_df$ci.lb.i) - .05,max(plot_df$ci.ub.i) + .05)
    xlims[1] <- ifelse(xlims[1] > -0.05, -0.05, xlims[1])
    xlims[2] <- ifelse(xlims[2] < 0.05, 0.05, xlims[2])

    # DATA FOR PLOT
    dat.below <- data.frame(
      Country = c("Overall", "Overall"),
      yi = c(as.numeric(fit_wo$b), as.numeric(fit_w$b)),
      ci.lb.i = c(as.numeric(fit_wo$ci.lb), as.numeric(fit_w$ci.lb)),
      ci.ub.i = c(as.numeric(fit_wo$ci.ub), as.numeric(fit_w$ci.ub)),
      model_type = c("Excluded","Included")
    ) |>
      mutate(
        ci = paste0("(", .round(ci.lb.i, digits), ",", .round(ci.ub.i, digits), ")"),
        CI = paste0(.round(yi, digits), " ", ci)
      )

    p_mid <- plot_df |>
      ggplot(aes(y = Country)) +
      Rglobalflourishing:::.geom_stripes() +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
      geom_point(aes(x = yi, color = model_type, shape = model_type),
                 position=ggstance::position_dodgev(height=0.5),
                 size = 2) +
      geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                     position=ggstance::position_dodgev(height=0.5)) +
      scale_color_manual(values = c("#a6cee3", "#1f78b4")) +
      scale_shape_manual(values = c(17,16)) +
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

    p_right_w <- plot_df |> filter(model_type == "Included") |>
      ggplot(aes(y = Country)) +
      geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
      Rglobalflourishing:::.geom_stripes() +
      theme_void()

    p_right_wo <- plot_df |> filter(model_type == "Excluded") |>
      ggplot(aes(y = Country)) +
      geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
      Rglobalflourishing:::.geom_stripes() +
      theme_void()

    p_below <- dat.below %>%
      ggplot(aes(x = yi, y = Country)) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_point(aes(x = yi, color = model_type, shape = model_type, size = model_type),
                 position = ggstance::position_dodgev(height = 0.5)
      ) +
      geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                     position = ggstance::position_dodgev(height = 0.5)) +
      scale_color_manual(values = c("#fcb360", "#ed6f0e")) +
      scale_shape_manual(values = c(18,15)) +
      scale_size_manual(values = c(4,3)) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(),
        axis.line.y = element_blank(),
      ) +
      xlim(xlims) +
      labs(x = xLab, y = NULL)

    p_below_right_w <- dat.below |> filter(model_type == "Included") |>
      ggplot(aes(y = Country)) +
      geom_text(aes(x = 0, label = CI), hjust = 0.45) +
      theme_void()

    p_below_right_wo <- dat.below |> filter(model_type == "Excluded") |>
      ggplot(aes(y = Country)) +
      geom_text(aes(x = 0, label = CI), hjust = 0.45) +
      theme_void()

    x_dist <- xlims[2] - xlims[1]
    legend_x1 <- xlims[1] + (x_dist/20)
    legend_x2 <- xlims[1] + 11*(x_dist/20)
    ci_len <- x_dist/40
    text_x1 <- legend_x1 + x_dist/4.5
    text_x2 <- legend_x2 + x_dist/4

    legend_df <- data.frame(legend_shape = c("circ", "square", "tri", "diam"),
                            legend_color = c("darkbl", "darkor", "lightbl", "lightor"),
                            point_x = rep(c(legend_x1, legend_x2), each = 2),
                            point_y = rep(c(2, 1), times = 2)) %>%
      mutate(ci_upper = point_x + ci_len,
             ci_lower = point_x - ci_len)

    p_legend <- ggplot(legend_df, aes(x = point_x, y = point_y)) +
      geom_point(aes(shape = legend_color, color = legend_color, size = legend_color)) +
      geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, color = legend_color)) +
      scale_color_manual(values = c("#1f78b4", "#ed6f0e", "#a6cee3","#fcb360")) +
      scale_shape_manual(values = c(16, 15, 17, 18)) +
      scale_size_manual(values = c(2.5, 3, 2.5, 4)) +
      annotate("text",
               label = "With PC Controls",
               x = text_x1, y = 1.55,
               size = 10/.pt) +
      annotate("text",
               label = "Without PC Controls",
               x = text_x2, y = 1.55,
               size = 10/.pt) +
      theme_bw() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank()) +
      xlim(xlims) +
      ylim(c(.5, 2.5))

    p_legend_w <- ggplot() +
      theme_void() +
      annotate("text", label = "With PC Controls", x = 0, y = 0)
    p_legend_wo <- ggplot() +
      theme_void() +
      annotate("text", label = "Without PC Controls", x = 0, y = 0)


    p <- (p_mid + plot_spacer() + p_right_w + plot_spacer() + p_right_wo +
            plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
            p_below + plot_spacer() + p_below_right_w + plot_spacer() + p_below_right_wo +
            p_legend + plot_spacer() + p_legend_w + plot_spacer() + p_legend_wo) +
      plot_layout(
        byrow = TRUE,
        widths = c(2, -0.1, 1, -0.1, 1),
        heights = c(10, -0.75, 1, -0.75, 1)
      )  +
      plot_annotation(
        # title=str_wrap(paste0("Figure S",k+k.shift,f.tag," Forest plot for `", tmp.var,"`-`", tmp.cat, "` effect"), 75),
        title = str_wrap(p.title, 80),
        #subtitle = p.subtitle,
        caption = paste0(c(tmp.het.wo, tmp.het.w), collapse = "\n")
      )

    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num0,"_", outcome,"_regressed_on_", focal.predictor,".pdf")),
      plot = p, height = 6, width = 10, units = "in"
    )
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num0,"_", outcome,"_regressed_on_", focal.predictor,".png")),
      plot = p, height = 6, width = 10, units = "in", dpi = 1000
    )

  }

  save(p, fig.cap, file = cache.file)


}

#' @export
#' @rdname build-functions
generate_docx_normal_portrait <- function(cache.file, print.file){

  normal_portrait <- block_section(
    prop_section(page_size = page_size(orient = "portrait"), type = "continuous")
  )

  load(cache.file)

  read_docx() |>
    body_add_flextable(print.tb) |>
    body_end_block_section(normal_portrait) |>
    print(target = print.file)

}

#' @export
#' @rdname build-functions
generate_docx_normal_landscape <- function(cache.file, print.file){

  landscape_one_column <- block_section(
    prop_section(
      page_size = page_size(orient = "landscape"), type = "continuous"
    )
  )

  load(cache.file)

  read_docx() |>
    body_add_flextable(print.tb) |>
    body_end_block_section(landscape_one_column) |>
    print(target = print.file)

}

#' @export
#' @rdname build-functions
generate_docx_wide_landscape <- function(cache.file, print.file){

  extra_wide_landscape <- block_section(prop_section(
    page_size = page_size(
      orient = "landscape",
      width = 29.7 / 2.54 * 2,
      height = 29.7 / 2.54
    ),
    type = "continuous"
  ))

  load(cache.file)

  read_docx() |>
    body_add_flextable(print.tb) |>
    body_end_block_section( extra_wide_landscape) |>
    print(target = print.file)

}

#' @export
#' @rdname build-functions
generate_docx_fig <- function(cache.file, print.file, fig.file, orient = "p", h = 6, w = 5){

  if(orient == "p"){
    style <- block_section(
      prop_section(page_size = page_size(orient = "portrait"), type = "continuous")
    )
  }
  if(orient == "l"){
    style <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"), type = "continuous"
      )
    )
  }

  load(cache.file)

  read_docx() |>
    body_add_fpar(
      fpar(
        ftext(
          fig.cap, prop = fp_text(font.family = "Open Sans", font.size = 11)
        )
      )
    ) |>
    body_add_img(src=fig.file, height = h, width = w) |>
    body_end_block_section(style) |>
    print(target = print.file)

}
