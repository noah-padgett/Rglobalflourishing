library(tictoc)

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
library(Rglobalflourishing)

# Analysis Set-Up

# Add the directory where the dataset is stored on your computer
data.dir <- "/Users/chris_felton/Documents/GFSw20"
dataset.name <- "gfs_all_countries_wave2.sav"

# Specify where you want to output results
# Can be left blank, and the results will output to the same directory as the data.
#out.dir <- getwd()

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- c("PHYSICAL_HLTH_Y1")
FOCAL_PREDICTOR_BETTER_NAME <- c("Self-rated physical health")
FOCAL_PREDICTOR_REFERENCE_VALUE <- c("mean rating within country")

# IF your predictor (focal exposure) is binary/categorical, use the code below to define how you
#   want it to be categorized. Categorization must result in a binary variable 0/1 for
#   consistency across studies.
VALUES_DEFINING_UPPER_CATEGORY <- list(NA)
VALUES_DEFINING_LOWER_CATEGORY <- list(NA)
# Note 1: if your focal predictor is continuous (all items with 7+ response options), you can force the responses
# 	to be categorized as 0/1 using the above with the below option changed to TRUE. This can be useful
# 	when testing the sensitivity of results or for composite outcomes such as anxiety (sum of
#   feel_anxious and control_worry)  or depression (sum of depressed and interest) that have a
# 	history of being dichotomized.
FORCE_BINARY <- c(FALSE)
# Note 2: if your focal predictor is categorical/binary, you can use the responses as if they were continuous.
# 	The provided (straightforward-ish) approach implemented is to reverse code all
#   ordered-categorical variables (reverse code from what is reported in the codebook), and
#   standardized as if continuous. This approach is not applicable for variables with nominal
#   response categories such as employment. This is employed using the option below.
FORCE_CONTINUOUS <- c(FALSE)
# Note 3: if you need to define a subpopulation for domain analysis. (in-development)
SUBPOPULATION <- list(NULL)

names(FORCE_CONTINUOUS) <- names(FORCE_BINARY) <- names(VALUES_DEFINING_UPPER_CATEGORY)  <- names(VALUES_DEFINING_LOWER_CATEGORY) <- names(SUBPOPULATION) <- FOCAL_PREDICTOR
# ================================================================================================ #
# ================================================================================================ #
# Data Prep

if (!exists("out.dir")) {
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

## MULTISESSION
num_cores <- parallel::detectCores() - 1
plan("multisession", workers = num_cores)
handlers(global = TRUE)
#handlers("progress")



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

tic.clearlog()
tic()

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
    Nimp = 20,
    Miter = 2,
    pred.vars = pred0
  )
}

toc(log = TRUE)
imp_log <- tic.log()
saveRDS(imp_log, here(data.dir, "timelog", "imp_log.rds"))
rm(imp_log)

# ================================================================================================ #
# ================================================================================================ #
# Recode the imputed data for easily use in analyses
# ~~

tic.clearlog()
tic()

recode_imp_by_country(
  data.dir,
  list.default = RECODE.DEFAULTS,
  list.composites = LIST.COMPOSITES,
  wgt = "ANNUAL_WEIGHT_R2",
  .nimp = 20
)
# ================================================================================================ #
# ================================================================================================ #
# CHECK VARIABLE CODING/COLLAPSING
#
# the following checks the imputed data back with the raw data
tmp.dat1 <- df.raw %>%
  filter(COUNTRY == "United States") %>%
  arrange(ID)
tmp.dat2 <- readr::read_rds(here::here(data.dir, "recoded_imputed_data_obj_United States.rds")) %>%
  filter(.imp == 1) %>%
  arrange(ID)
dnn0 <- c("Raw Data", "Recoded Imputed Data (.imp==1)")
for(i in 1:length(FOCAL_PREDICTOR)){
  print(FOCAL_PREDICTOR[i])
  print(table(tmp.dat1[[FOCAL_PREDICTOR[i]]], tmp.dat2[[FOCAL_PREDICTOR[i]]], dnn = dnn0, useNA = "ifany"))
}

toc(log = TRUE)
recode_log <- tic.log()
saveRDS(recode_log, here(data.dir, "timelog", "recode_log.rds"))
rm(recode_log)

# ================================================================================================ #
# ================================================================================================ #
# Attrition Weights

########

rm(df.raw)
rm(tmp.dat1)
rm(tmp.dat2)
gc()

tic.clearlog()
tic()

run_attrition_model_by_country(
  data.dir,
  obs.id.var = "CASE_OBSERVED_Y2",
  attr.pred = c(
    "ANNUAL_WEIGHT_R2", "MODE_RECRUIT",
    'COMPOSITE_HAPPI_LIFE_SAT_Y1', 'COMPOSITE_HEALTH_Y1', 'COMPOSITE_MEANING_PURPOSE_Y1',
    'COMPOSITE_CHARACTER_Y1', 'COMPOSITE_SUBJECTIVE_SOC_CONN_Y1', 'COMPOSITE_FINL_MAT_WORRY_Y1',
    'COMPOSITE_EXTRAVERSION_Y1', 'COMPOSITE_OPENNESS_Y1', 'COMPOSITE_AGREEABLENESS_Y1',
    'COMPOSITE_CONSCIENTIOUSNESS_Y1', 'COMPOSITE_NEUROTICISM_Y1',
    'COMPOSITE_DEPRESSION_Y1', 'COMPOSITE_ANXIETY_Y1', 'LONELY_Y1', 'DAYS_EXERCISE_Y1',
    'COV_AGE_GRP_Y1', 'COV_GENDER', 'COV_MARITAL_STATUS_Y1', 'COV_EMPLOYMENT_Y1',
    'COV_ATTEND_SVCS_Y1', 'COV_EDUCATION_3_Y1', 'COV_BORN_COUNTRY_Y1', "COV_RACE_PLURALITY",
    "COV_URBAN_RURAL_Y1", 'COV_INCOME_Y1'
  ),
  wgt = "ANNUAL_WEIGHT_R2", strata = "STRATA", psu = "PSU"
)

## append attrition weights to imputed data
append_attr_wgts_to_imp_data(data.dir, attr.dir = "results-attr")

toc(log = TRUE)
attr_log <- tic.log()
saveRDS(attr_log, here(data.dir, "timelog", "attr_log.rds"))
rm(attr_log)

########

tic.clearlog()
tic()

# ================================================================================================ #
# ================================================================================================ #
# Run primary country-wise analyses -- Full imputation based approach

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
LIST.RES <- map(OUTCOME.VEC0, \(x){
  map(FOCAL_PREDICTOR, \(y){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = y,
      data.dir = data.dir,
      wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
      psu = PSU, #psu = as.name("PSU")
      strata = STRATA, # strata = as.name("STRATA")
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      pc.rule = "omit",
      res.dir = "results-primary",
      appnd.txt.to.filename = "_primary_wopc"
    )
  }) }, .progress = TRUE)


LIST.RES <- construct_meta_input_from_saved_results("results-primary", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_primary_wopc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Excluded -- Full Imputation Approach"
)
readr::write_rds(
  META.RES,
  file = here::here(data.dir, "results-primary", "0_meta_analyzed_results_primary_wopc.rds"),
  compress = "gz"
)
remove(LIST.RES, meta.input, META.RES)

toc(log = TRUE)
wopc_log <- tic.log()
saveRDS(wopc_log, here(data.dir, "timelog", "wopc_log.rds"))
rm(wopc_log)

########

tic.clearlog()
tic()


# Model 2: Run with principal components



furrr::future_walk(OUTCOME.VEC0, \(x){
  load_packages()
  options(
    survey.lonely.psu = "certainty"
  )
  gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = FOCAL_PREDICTOR,
      data.dir = data.dir,
      wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
      psu = PSU, #psu = as.name("PSU")
      strata = STRATA, # strata = as.name("STRATA")
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      pc.cutoff = 7,
      pc.rule = "constant",
      res.dir = "results-primary",
      appnd.txt.to.filename = "_primary_wpc"
    )
})


LIST.RES <- construct_meta_input_from_saved_results("results-primary", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_primary_wpc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  dplyr::mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Included -- Full Imputation Approach"
)

readr::write_rds(
  META.RES,
  file = here::here(data.dir, "results-primary","0_meta_analyzed_results_primary_wpc.rds"),
  compress = "gz"
)
remove(meta.input, LIST.RES, META.RES)

toc(log = TRUE)
wpc_log <- tic.log()
saveRDS(wpc_log, here(data.dir, "timelog", "wpc_log.rds"))
rm(wpc_log)

########

tic.clearlog()
tic()

# ================================================================================================ #
# ================================================================================================ #
# Run supplemental country-wise analyses -- "Complete Case Analysis"
# - Uses attrition-weight adjusted sampling weights

# Supplemental analysis set 1: Run without principal components
SUPP.LIST.RES <- map(OUTCOME.VEC0, \(x){
  map(FOCAL_PREDICTOR, \(y){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = y,
      data.dir = data.dir,
      direct.subset = expr(CASE_OBSERVED_Y2 == 1),
      wgt = SAMP.ATTR.WGT,
      psu = PSU,
      strata = STRATA,
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      pc.rule = "omit",
      res.dir = "results-cca",
      appnd.txt.to.filename = "_cca_wopc"
    )
  }) }, .progress = TRUE)
#SUPP.LIST.RES <- construct_meta_input_from_saved_results("results-cca", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_cca_wopc")
meta.input <- SUPP.LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()
SUPP.META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Excluded -- Complete Case Analysis"
)
readr::write_rds(
  SUPP.META.RES,
  file = here::here("results-cca", "0_meta_analyzed_results_cca_wopc.rds"),
  compress = "gz"
)
remove(meta.input, SUPP.LIST.RES, SUPP.META.RES)

toc(log = TRUE)
supp1_log <- tic.log()
saveRDS(supp1_log, here(data.dir, "timelog", "supp1_log.rds"))
rm(supp1_log)

########

tic.clearlog()
tic()

# Analysis set 2: Run with principal components
SUPP.LIST.RES <- map(OUTCOME.VEC0, \(x){
  map(FOCAL_PREDICTOR, \(y){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = y,
      data.dir = data.dir,
      direct.subset = expr(CASE_OBSERVED_Y2 == 1),
      wgt = SAMP.ATTR.WGT,
      psu = PSU,
      strata = STRATA,
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = TRUE,
      pc.cutoff = 7,
      pc.rule = "constant",
      res.dir = "results-cca",
      appnd.txt.to.filename = "_cca_wpc"
    )
  }) }, .progress = TRUE)
SUPP.LIST.RES <- construct_meta_input_from_saved_results(res.dir="results-cca", outcomes=OUTCOME.VEC0, predictors=FOCAL_PREDICTOR, appnd.txt = "_cca_wpc")
meta.input <- SUPP.LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()
SUPP.META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Included -- Complete Case Analysis"
)
readr::write_rds(
  SUPP.META.RES,
  file = here::here("results-cca", "0_meta_analyzed_results_cca_wpc.rds"),
  compress = "gz"
)
remove(meta.input, SUPP.LIST.RES, SUPP.META.RES)

toc(log = TRUE)
supp2_log <- tic.log()
saveRDS(supp2_log, here(data.dir, "timelog", "supp2_log.rds"))
rm(supp2_log)

########

tic.clearlog()
tic()

# ================================================================================================ #
# ================================================================================================ #
# Re-run primary analysis but get UNSTANDARDIZED estimated

# Model 1: Run without principal components
LIST.RES <- map(OUTCOME.VEC0, \(x){
  map(FOCAL_PREDICTOR, \(y){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = y,
      data.dir = data.dir,
      wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
      psu = PSU, #psu = as.name("PSU")
      strata = STRATA, # strata = as.name("STRATA")
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = FALSE,
      pc.rule = "omit",
      res.dir = "results-unstd",
      appnd.txt.to.filename = "_unstd_wopc"
    )
  }) }, .progress = TRUE)


LIST.RES <- construct_meta_input_from_saved_results("results-unstd", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_unstd_wopc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Excluded -- Unstandardized Results"
)
readr::write_rds(
  META.RES,
  file = here::here("results-unstd", "0_meta_analyzed_results_unstd_wopc.rds"),
  compress = "gz"
)
remove(LIST.RES, meta.input, META.RES)

toc(log = TRUE)
supp3_log <- tic.log()
saveRDS(supp3_log, here(data.dir, "timelog", "supp3_log.rds"))
rm(supp3_log)

########

tic.clearlog()
tic()

# Model 2: Run with principal components
LIST.RES <- map(OUTCOME.VEC0, \(x){
  map(FOCAL_PREDICTOR, \(y){
    gfs_run_regression_single_outcome(
      your.outcome = x,
      your.pred = y,
      data.dir = data.dir,
      wgt = ANNUAL_WEIGHT_R2,
      psu = PSU,
      strata = STRATA,
      covariates = DEMO.CHILDHOOD.PRED,
      contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
      list.composites = LIST.COMPOSITES[[1]],
      standardize = FALSE,
      pc.cutoff = 7,
      pc.rule = "constant",
      res.dir = "results-unstd",
      appnd.txt.to.filename = "_unstd_wpc"
    )
  }) }, .progress = TRUE)

LIST.RES <- construct_meta_input_from_saved_results("results-unstd", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_unstd_wpc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input,
  p.subtitle = "Principal Components Included -- Unstandardized Results"
)

readr::write_rds(
  META.RES,
  file = here::here("results-unstd","0_meta_analyzed_results_unstd_wpc.rds"),
  compress = "gz"
)
remove(meta.input, LIST.RES, META.RES)

toc(log = TRUE)
supp4_log <- tic.log()
saveRDS(supp4_log, here(data.dir, "timelog", "supp4_log.rds"))
rm(supp4_log)

# ================================================================================================ #
# ================================================================================================ #
gc()
# ================================================================================================ #
# ================================================================================================ #
# Construct manuscript tables

########

tic.clearlog()
tic()

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
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  add.whitespace = TRUE
)
df.raw <- append_attrition_weights_to_df(data=df.raw)

# main text
gfs_generate_main_doc(
  df.raw = df.raw,
  meta.wopc = here::here(out.dir, "results-primary", "0_meta_analyzed_results_primary_wopc.rds"),
  meta.wpc = here::here(out.dir, "results-primary", "0_meta_analyzed_results_primary_wpc.rds"),
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name = FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
  res.dir = "results",
  wgt = WGT0,
  wgt1 = ANNUAL_WEIGHT_R2,
  wgt2 = AVG.SAMP.ATTR.WGT,
  psu = PSU,
  strata = STRATA
)

toc(log = TRUE)
supp5_log <- tic.log()
saveRDS(supp5_log, here(data.dir, "timelog", "supp5_log.rds"))
rm(supp5_log)

## Generate online supplements

########

tic.clearlog()
tic()

# online supplemental files (there's too much to pack into 1 file, separated into 3 files... for now.)
gfs_generate_supplemental_docs(
  df.raw = df.raw,
  dir.primary="results-primary",
  dir.supp="results-cca",
  dir.unstd = "results-unstd",
  dir.attr.models = "results-attr",
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE,
  res.dir = "results",
  wgt = WGT0,
  wgt1 = ANNUAL_WEIGHT_R2,
  wgt2 = AVG.SAMP.ATTR.WGT,
  psu = PSU,
  strata = STRATA,
  what = "S1",
  outcome.vec = OUTCOME.VEC0[73:79]
)

toc(log = TRUE)
supp6_log <- tic.log()
saveRDS(supp6_log, here(data.dir, "timelog", "supp6_log.rds"))
rm(supp6_log)
