# Script: main.R
# Created by: R. Noah Padgett & Chris Felton
# Last edited on: 2024-03-13

# WARNING: The package was set up to be as user-friendly as possible for researchers
#	part of the GFS core team who mainly have experience with other statistical analysis
#	software such as STATA, SAS, and SPSS.
#	This package and implementation of the analyses for Wave 2 of the Global Flourishing
#	Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

# install.packages("remotes")
#remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)
#library(Rglobalflourishing)

# Analysis Set-Up

# Add the directory where the dataset is stored on your computer
data.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave2-data"
 dataset.name <- "gfs_all_countries_wave2.sav"

# Specify where you want to output results
# Can be left blank, and the results will output to the same directory as the data.
out.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/3-GFS-Core-Wave-2/test-package"

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- "EDUCATION_3_Y1"
FOCAL_PREDICTOR_BETTER_NAME <- "Educational Attainment"
FOCAL_PREDICTOR_REFERENCE_VALUE <- "up to 15 years of education"

# IF your predictor (focal exposure) is binary/categorical, use the code below to define how you
#   want it to be categorized. Categorization must result in a binary variable 0/1 for
#   consistency across studies.
VALUES_DEFINING_UPPER_CATEGORY <- c(3)
VALUES_DEFINING_LOWER_CATEGORY <- c(1,2)
# Note 1: if your focal predictor is continuous (all items with 7+ response options), you can force the responses
# 	to be categorized as 0/1 using the above with the below option changed to TRUE. This can be useful
# 	when testing the sensitivity of results or for composite outcomes such as anxiety (sum of
#   feel_anxious and control_worry)  or depression (sum of depressed and interest) that have a
# 	history of being dichotomized.
FORCE_BINARY <- FALSE
# Note 2: if your focal predictor is categorical/binary, you can use the responses as if they were continuous.
# 	The provided (straightforward-ish) approach implemented is to reverse code all
#   ordered-categorical variables (reverse code from what is reported in the codebook), and
#   standardized as if continuous. This approach is not applicable for variables with nominal
#   response categories such as employment. This is employed using the option below.
FORCE_CONTINUOUS <- FALSE

# ================================================================================================ #
# ================================================================================================ #
# Data Prep

if (is.null(out.dir)) {
  out.dir <- data.dir
}
setwd(out.dir)
# Note:
# The following function loads the required packages for the remainder of the script to work.
load_packages()
# global options
options(
  survey.lonely.psu = "certainty"
)
# outcome vectors
LIST.COMPOSITES <- get_variable_codes('LIST.COMPOSITES')
RECODE.DEFAULTS <- list(
  FOCAL_PREDICTOR = FOCAL_PREDICTOR,
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
    get_variable_codes("GENDER.RACE", appnd=""),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y1"),
    get_variable_codes("RETROSPECTIVE.VARS", appnd="_Y1")
  ),
  VARIABLES.VEC = c(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2")),
  FORCE_BINARY = FORCE_BINARY,
  FORCE_CONTINUOUS = FORCE_CONTINUOUS,
  VALUES_DEFINING_UPPER_CATEGORY = VALUES_DEFINING_UPPER_CATEGORY,
  VALUES_DEFINING_LOWER_CATEGORY = VALUES_DEFINING_LOWER_CATEGORY,
  USE_DEFAULT = !(FORCE_BINARY | FORCE_CONTINUOUS)
)

# get "raw data"
df.raw <- gfs_get_labelled_raw_data(
  file = here::here(data.dir, dataset.name),
  list.composites = LIST.COMPOSITES
)

# ================================================================================================ #
# ================================================================================================ #
# Imputing missing data
{
  run.imp <- TRUE
  if (run.imp) {
    pred0 <- c(
      'ANNUAL_WEIGHT_R2', 'MODE_RECRUIT',
      'AGE_Y1', 'GENDER_Y1', 'RACE_PLURALITY1', 'MARITAL_STATUS_Y1',
      'EMPLOYMENT_Y1', 'EDUCATION_3_Y1', 'ATTEND_SVCS_Y1',
      'URBAN_RURAL_Y1', 'BORN_COUNTRY_Y1', 'REL2_Y1',
      'ABUSED_Y1', 'OUTSIDER_Y1',
      'HEALTH_GROWUP_Y1', 'INCOME_12YRS_Y1',
      'FATHER_RELATN_Y1', 'MOTHER_RELATN_Y1',
      'PARENTS_12YRS_Y1'
    )
    df.imp <- run_impute_data(
      data =  df.raw,
      data.dir = data.dir,
      Nimp = 2,
      Miter = 1,
      pred.vars = pred0,
      file.name = "gfs_w2_imputed_data_2imp_test.RData"
    )
  }

  load(here::here(data.dir, "gfs_w2_imputed_data_2imp_test.RData"))
  # ~~
  df.imp.long <- recode_imputed_data(
    df.imp,
    list.default = RECODE.DEFAULTS,
    list.composites = LIST.COMPOSITES,
    wgt = "ANNUAL_WEIGHT_R2"
  )
}
# ================================================================================================ #
# ================================================================================================ #
# CHECK VARIABLE CODING/COLLAPSING
#
# the following checks the imputed data back with the raw data
tmp.dat1 <- df.raw %>%
  arrange(ID)
tmp.dat2 <- df.imp.long %>%
  filter(.imp == 1) %>%
  arrange(ID)
dnn0 <- c("Raw Data", "Recoded Imputed Data (.imp==1)")
table(tmp.dat1[[FOCAL_PREDICTOR]], tmp.dat2[[FOCAL_PREDICTOR]], dnn = dnn0, useNA = "ifany")
table(tmp.dat1[["BELIEVE_GOD_Y2"]], tmp.dat2[["BELIEVE_GOD_Y2"]], dnn = dnn0, useNA = "ifany")

# ================================================================================================ #
# ================================================================================================ #
# Attrition Weights


df.imp.long <- run_attrition_model(
  data = df.imp.long,
  obs.id.var = "CASE_OBSERVED_Y2",
  attr.pred = c(
    "ANNUAL_WEIGHT_R2", "MODE_RECRUIT",
    'COMPOSITE_HAPPI_LIFE_SAT_Y1', 'COMPOSITE_HEALTH_Y1', 'COMPOSITE_MEANING_PURPOSE_Y1',
    'COMPOSITE_CHARACTER_Y1', 'COMPOSITE_SUBJECTIVE_SOC_CONN_Y1', 'COMPOSITE_FINL_MAT_WORRY_Y1',
    'COMPOSITE_EXTRAVERSION_Y1', 'COMPOSITE_OPENNESS_Y1', 'COMPOSITE_AGREEABLENESS_Y1',
    'COMPOSITE_CONSCIENTIOUSNESS_Y1', 'COMPOSITE_NEUROTICISM_Y1',
    'DEPRESSED_Y1', 'LONELY_Y1', 'DAYS_EXERCISE_Y1',
    'COV_AGE_GRP_Y1', 'COV_GENDER', 'COV_MARITAL_STATUS_Y1', 'COV_EMPLOYMENT_Y1',
    'COV_ATTEND_SVCS_Y1', 'COV_EDUCATION_3_Y1', 'COV_BORN_COUNTRY_Y1', "COV_RACE_PLURALITY",
    "COV_URBAN_RURAL_Y1", 'COV_INCOME_Y1'
  ),
  wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU",	
  replace = TRUE
)

readr::write_rds(df.imp.long, here::here(data.dir, "gfs_imputed_data_formatted_wwgts.RData"))
# ================================================================================================ #
# ================================================================================================ #
# Run primary country-wise analyses -- Full imputation based approach
df.imp.long <- readr::read_rds(here::here(data.dir, "gfs_imputed_data_formatted_wwgts.RData"))

VARIABLES.VEC <- RECODE.DEFAULTS[['VARIABLES.VEC']]
OUTCOME.VEC <- VARIABLES.VEC[str_detect(VARIABLES.VEC, "_Y2")]
CONTEMPORANEOUS.EXPOSURES.VEC <- VARIABLES.VEC[str_detect(VARIABLES.VEC, "COMPOSITE", negate = TRUE)]
CONTEMPORANEOUS.EXPOSURES.VEC <- CONTEMPORANEOUS.EXPOSURES.VEC[str_detect(CONTEMPORANEOUS.EXPOSURES.VEC, "_Y1")]
DEMO.CHILDHOOD.PRED <- c(
  "COV_AGE_GRP_Y1",
  "COV_GENDER_Y1",
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
  "COV_RACE_PLURALITY",
  "COV_MOTHER_NA",
  "COV_FATHER_NA"
)
# Run country-specific regression analyses for ALL wave 2 outcomes
OUTCOME.VEC0 <- OUTCOME.VEC # c(1, 8, 24)+76,
your.outcome <- OUTCOME.VEC0[2]

# Model 1: Run without principal components
LIST.RES1 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    your.outcome = x,
    data = df.imp.long,
    wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
    psu = PSU, #psu = as.name("PSU")
    strata = STRATA, # strata = as.name("STRATA")
    your.pred = FOCAL_PREDICTOR,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    list.composites = LIST.COMPOSITES[[1]],
    standardize = TRUE,
    res.dir = "results-wopc",
    pc.rule = "omit"
  )
}, .progress = TRUE)


#LIST.RES1 <- construct_meta_input_from_saved_results("results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES1 %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

META.RES1 <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Excluded -- Full Imputation Approach"
)
readr::write_rds(
  META.RES1,
  file = here::here(out.dir, "results-wopc", "0_meta_analyzed_results_wopc.rds"),
  compress = "gz"
)

# Model 2: Run with principal components
LIST.RES2 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
    psu = PSU, #psu = as.name("PSU")
    strata = STRATA, # strata = as.name("STRATA")
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    list.composites = LIST.COMPOSITES[[1]],
    standardize = TRUE,
    res.dir = "results-wpc",
    pc.cutoff = 7,
    pc.rule = "constant"
  )
}, .progress = TRUE)

#LIST.RES2 <- construct_meta_input_from_saved_results("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES2 %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

META.RES2 <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Included -- Full Imputation Approach"
)

readr::write_rds(
  META.RES2,
  file = here::here(out.dir, "results-wpc","0_meta_analyzed_results_wpc.rds"),
  compress = "gz"
)

# ================================================================================================ #
# ================================================================================================ #
# Run supplemental country-wise analyses -- attrition-weight adjusted sampling weights


  # Supplemental analysis set 1: Run without principal components
  SUPP.LIST.RES1 <- map(OUTCOME.VEC0, \(x){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      data = df.imp.long %>% filter(CASE_OBSERVED_Y2 == 1),
      wgt = SAMP.ATTR.WGT,
      psu = PSU,
      strata = STRATA,
      your.pred = FOCAL_PREDICTOR,
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      res.dir = "supp-results-wopc",
      pc.rule = "omit"
    )
  }, .progress = TRUE)
  SUPP.LIST.RES1 <- construct_meta_input_from_saved_results("supp-results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
  meta.input <- SUPP.LIST.RES1 %>%
    bind_rows() %>%
    mutate(OUTCOME0 = OUTCOME) %>%
    group_by(OUTCOME0) %>%
    nest()
  SUPP.META.RES1 <- gfs_meta_analysis(
    meta.input,
    better.name = FOCAL_PREDICTOR_BETTER_NAME,
    p.subtitle = "Principal Components Excluded -- Attrition Weights"
  )
  readr::write_rds(
    SUPP.META.RES1,
    file = here::here(out.dir, "supp-results-wopc", "0_meta_analyzed_results_wopc.rds"),
    compress = "gz"
  )

  # Analysis set 2: Run with principal components
  SUPP.LIST.RES2 <- map(OUTCOME.VEC0, \(x){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      data = df.imp.long %>% filter(CASE_OBSERVED_Y2 == 1),
      wgt = SAMP.ATTR.WGT,
      psu = PSU,
      strata = STRATA,
      your.pred = FOCAL_PREDICTOR,
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      res.dir = "supp-results-wpc",
      pc.cutoff = 7,
      pc.rule = "constant"
    )
  }, .progress = TRUE)
  SUPP.LIST.RES2 <- construct_meta_input_from_saved_results("supp-results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
  meta.input <- SUPP.LIST.RES2 %>%
    bind_rows() %>%
    mutate(OUTCOME0 = OUTCOME) %>%
    group_by(OUTCOME0) %>%
    nest()
  SUPP.META.RES2 <- gfs_meta_analysis(
    meta.input,
    better.name = FOCAL_PREDICTOR_BETTER_NAME,
    p.subtitle = "Principal Components Included -- Attrition Weights"
  )
  readr::write_rds(
    SUPP.META.RES2,
    file = here::here(out.dir, "supp-results-wpc", "0_meta_analyzed_results_wpc.rds"),
    compress = "gz"
  )

# ================================================================================================ #
# ================================================================================================ #
# Construct manuscript tables

LIST.COMPOSITES <- get_variable_codes('LIST.COMPOSITES')
RECODE.DEFAULTS <- list(
  FOCAL_PREDICTOR = FOCAL_PREDICTOR,
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
    get_variable_codes("GENDER.RACE", appnd=""),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y1"),
    get_variable_codes("RETROSPECTIVE.VARS", appnd="_Y1")
  ),
  VARIABLES.VEC = c(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2")),
  FORCE_BINARY = FORCE_BINARY,
  FORCE_CONTINUOUS = FORCE_CONTINUOUS,
  VALUES_DEFINING_UPPER_CATEGORY = VALUES_DEFINING_UPPER_CATEGORY,
  VALUES_DEFINING_LOWER_CATEGORY = VALUES_DEFINING_LOWER_CATEGORY,
  USE_DEFAULT = !(FORCE_BINARY | FORCE_CONTINUOUS)
)

# needed results
df.raw <- gfs_get_labelled_raw_data(
  here::here(data.dir, dataset.name),
  list.composites = LIST.COMPOSITES,
  add.whitespace = TRUE
)

df.raw <- append_attrition_weights_to_df(data=df.raw)
VARIABLES.VEC <- RECODE.DEFAULTS[['VARIABLES.VEC']]
OUTCOME.VEC0 <- VARIABLES.VEC[str_detect(VARIABLES.VEC, "_Y2")]
COUN.RES.WOPC <- get_country_specific_regression_results("results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
COUN.RES.WPC <- get_country_specific_regression_results("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
SUPP.COUN.RES.WOPC <- get_country_specific_regression_results("supp-results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
SUPP.COUN.RES.WPC <- get_country_specific_regression_results("supp-results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
FIT.PCA.SUM <- get_country_specific_pca_summary("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
FIT.ATTR <- get_fitted_attrition_models("results-attr")
META.RES1 <- readr::read_rds(file = here::here(out.dir, "results-wopc", "0_meta_analyzed_results_wopc.rds"))
META.RES2 <- readr::read_rds(file = here::here(out.dir, "results-wpc", "0_meta_analyzed_results_wpc.rds"))
SUPP.META.RES1 <- readr::read_rds(file = here::here(out.dir, "supp-results-wopc", "0_meta_analyzed_results_wopc.rds"))
SUPP.META.RES2 <- readr::read_rds(file = here::here(out.dir, "supp-results-wpc", "0_meta_analyzed_results_wpc.rds"))

# main text
gfs_generate_main_doc(
  df.raw = df.raw,
  meta.wopc = META.RES1,
  meta.wpc = META.RES2,
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name = FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
  res.dir = "results",
  wgt = WGT0,
  wgt1 = ANNUAL_WEIGHT_R2,
  wgt2 = SAMP.ATTR.WGT,
  psu = PSU,
  strata = STRATA,
  n.print="207,919"
)

# online supplemental files (there's too much to pack into 1 file, separated into 3 files... for now.)
gfs_generate_supplemental_docs(
  df.raw = df.raw,
  meta.wopc = META.RES1, meta.wpc = META.RES2,
  coun.wopc =  COUN.RES.WOPC , coun.wpc = COUN.RES.WPC , coun.fit.pca = FIT.PCA.SUM ,
  attr.models = FIT.ATTR,
  supp.meta.wopc = SUPP.META.RES1, supp.meta.wpc = SUPP.META.RES2,
  supp.coun.wopc = SUPP.COUN.RES.WOPC , supp.coun.wpc = SUPP.COUN.RES.WPC ,
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
  res.dir = "results",
  wgt = WGT0,
  wgt1 = ANNUAL_WEIGHT_R2,
  wgt2 = SAMP.ATTR.WGT,
  psu = PSU,
  strata = STRATA,
  what = "all",
  n.print="207,919"
)


# ================================================================================================ #
# ================================================================================================ #
# Code to fiddle/tinker with forest plots (main text plots)
{
  META.RES1 <- readr::read_rds(file = here::here(out.dir, "results-wopc", "0_meta_analyzed_results_wopc.rds"))
  META.RES2 <- readr::read_rds(file = here::here(out.dir, "results-wpc", "0_meta_analyzed_results_wpc.rds"))

  # Base plot:
  p1 <- META.RES2 %>% ungroup() %>%
    filter(OUTCOME0 == "COMPOSITE_FLOURISHING_SECURE_W2") %>%
    select(forest.plot)
  p1

  # IF desired, the code to re-create the figure and fiddle as desired is below:
  tmp.fit <- META.RES2 %>% ungroup() %>%
    filter(OUTCOME0 == "COMPOSITE_FLOURISHING_SECURE_Y2")
  tmp.fit <- tmp.fit$fit[[1]]
  tmp.dat <- tmp.fit$data %>%
    mutate(
      yi = Est
    )
  ALL.COUNTRIES <- c(
    "Australia","Hong Kong","India","Indonesia","Japan","Philippines","Egypt","Germany","Israel",
    "Kenya","Nigeria","Poland", "South Africa","Spain", "Sweden","Tanzania","Turkey", "United Kingdom",
    "United States","Argentina","Brazil","Mexico", "China"
  )

  focal.pred <- tmp.dat$FOCAL_PREDICTOR[1]
  focal.pred <- get_outcome_better_name(focal.pred, include.name = FALSE)
  if (!is.null(better.name)) {
    focal.pred <- better.name
  }
  tmp.outcome <- tmp.dat$OUTCOME[1]
  tmp.outcome <- get_outcome_better_name(tmp.outcome, include.name = FALSE, include.wave = TRUE)

  focal.pred <- str_to_sentence(focal.pred)
  tmp.outcome <- str_to_sentence(tmp.outcome)

  for (i in 1:length(tmp.dat$Country)) {
    tmp.dat$Country[i] <- stringr::str_split_fixed(tmp.dat$Country[i], "\\. ", 2)[,2]
  }
  # identify countries omitted from meta-analysis
  tmp.included.countries <- tmp.dat$Country
  tmp.included.countries <- str_replace(tmp.included.countries, "_", " ")
  tmp.included.countries <- str_trim(tmp.included.countries, "both")
  tmp.excluded.countries <- ALL.COUNTRIES[!(ALL.COUNTRIES %in% tmp.included.countries)]
  tmp.excluded.countries <- ifelse(
    !is_empty(tmp.excluded.countries),
    paste0("Excluded countries: ", paste0(tmp.excluded.countries, collapse = ", ")),
    ""
  )

  # X-axis label
  xLab <- "Standardized Effect"

  # construct heterogeneity statement...
  myci <- confint(tmp.fit, type = "PL")
  tmp.het <- paste0(
    "τ=", .round(sqrt(tmp.fit$tau2), 3),
    "; Q-profile 95% CI [", .round(myci$random[2, 2], 3), ", ", .round(myci$random[2, 3], 3), "]",
    "; Q(df=", tmp.fit$k - tmp.fit$QMdf[1], ")=",
    .round(tmp.fit$QE), ", p", ifelse(tmp.fit$QEp > 0.001, .round(tmp.fit$QEp, 3), "<.001"),
    "; I^2=", .round(tmp.fit$I2),
    ";\n",
    tmp.excluded.countries
  )
  xlims <- max(c(abs(tmp.dat$ci.lb), abs(tmp.dat$ci.ub))) + 0.20
  tmp.dat <- tmp.dat |>
    mutate(
      est_lab = paste0(.round(yi), " (", .round(ci.lb), ", ", .round(ci.ub), ")")
    )

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
  p_mid <-
    tmp.dat |>
    ggplot(aes(y = reorder(Country, yi))) +
    geom_point(aes(x = yi), shape = 15, size = 3) +
    geom_linerange(aes(xmin = ci.lb, xmax = ci.ub)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    .geom_stripes() +
    labs(x = xLab) +
    lims(x = c(-xlims, xlims)) +
    theme_classic() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(face = "bold"),
      axis.line.y = element_blank(),
      axis.text.x = element_blank()
    )

  # right side of plot - estimates
  p_right <-
    tmp.dat |>
    ggplot(aes(y = reorder(Country, yi))) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    .geom_stripes() +
    theme_void()

  p_below <- dat.below %>%
    ggplot(aes(x = est, y = label)) +
    geom_point(shape = 18, size = 5) +
    geom_linerange(aes(xmin = ci.lb, xmax = ci.ub)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = xLab, y = NULL) +
    lims(x = c(-xlims, xlims)) +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(face = "bold")
    )

  p_below_right <-
    dat.below |>
    ggplot(aes(y = label)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()

  # Pull everything together
  p <- (p_mid + plot_spacer() + p_right +
          plot_spacer() + plot_spacer() + plot_spacer() +
          p_below + plot_spacer() + p_below_right) +
    plot_layout(
      byrow = TRUE,
      widths = c(2, -0.175, 1),
      heights = c(10, -0.75, 1)
    ) +
    plot_annotation(
      title = str_wrap(paste0("`", focal.pred, "` predicts `", tmp.outcome, "`"), 75),
      subtitle = p.subtitle,
      caption = tmp.het
    )

  p
  ggsave(
    filename = here::here(paste0("figure_1_secure_flourishing_index.png")),
    plot = p,
    units = "in", width = 6, height = 5
  )
  ggsave(
    filename = here::here(paste0("figure_1_secure_flourishing_index.pdf")),
    plot = p,
    units = "in", width = 6, height = 5
  )
}

##
# Code to tinker as needed with the within-between supplemental plot

plot.dat <- META.RES2 %>%
        select(OUTCOME0, data, theta.rma, theta.rma.se, theta.rma.ci) %>%
        mutate(
          type = get_outcome_scale(OUTCOME0),
          type = case_when(
            type == "cont" ~ "Std. Est",
            .default = "log(RR)"
          )
        ) %>%
        unnest(c(data)) %>%
        mutate(
          est.rr = case_when(
            type == "log(RR)" ~ exp(Est),
            type == "Std. Est" ~ exp(0.91*Est)
          )
        )
      p <- plot.dat %>%
        group_by(Country) %>%
        mutate(
          avg.rr = mean(est.rr, na.rm=TRUE),
          var.rr = var(est.rr, na.rm=TRUE)
        ) %>% ungroup() %>%
        ggplot(aes(x=reorder(Country, avg.rr), y = est.rr)) +
        geom_point() +
        geom_hline(yintercept = c(0.90, 1.10), linetype="dashed") +
        labs(y="Estimated Risk-Ratio", x="",
             title="Heterogenetiy in estimated effects within and between countries",
             subtitle="Model estimated controlling for 7 principal components") +
        scale_x_discrete(guide = guide_axis(angle = 60)) +
        scale_y_continuous(limits = c(0.25,4))+
        theme_Publication()

p
ggsave(
    filename = here::here(paste0("figure_S1_heterogeneity_plot.png")),
    plot = p,
    units = "in", width = 6, height = 5
  )
  ggsave(
    filename = here::here(paste0("figure_S1_heterogeneity_plot.pdf")),
    plot = p,
    units = "in", width = 6, height = 5
  )
