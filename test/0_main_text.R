# Script: main.R
# Created by: R. Noah Padgett & Chris Felton
# Last edited on: 2024-05-05

# WARNING: The package was set up to be as user-friendly as possible for researchers
#	part of the GFS core team who mainly have experience with other statistical analysis
#	software such as STATA, SAS, and SPSS.
#	This package and implementation of the analyses for Wave 2 of the Global Flourishing
#	Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

# install.packages(c("remotes", "gitcreds"))
# gitcreds::gitcreds_set()
# remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)
library(Rglobalflourishing)

# Add the directory where the dataset is stored on your computer
data.dir <- "data"
data.dir <- "test/ignore/data"
dataset.name <- "gfs_all_countries_wave2.sav"

# ================================================================================================ #
# ================================================================================================ #
# environment prep
# The following function loads the required packages for the remainder of the script to work.
load_packages()
# global options
options(
  survey.lonely.psu = "certainty",
  future.plan = "multisession"
)
handlers(global = TRUE)
num_cores <- availableCores(constraints = "connections")

## TEST ONLY BUILD DOCUMENTS
df.raw <- gfs_get_labelled_raw_data(
  here::here(data.dir, dataset.name),
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  add.whitespace = TRUE, reverse.code.cont = TRUE
)
df.raw <- append_attrition_weights_to_df(data=df.raw)

# main text
gfs_generate_main_doc(
  df.raw = df.raw,
  focal.predictor = "PEACE_Y1", focal.better.name = "Inner peace", focal.predictor.reference.value = "rarely or never",
  dir.meta = "test/ignore/results-primary",
  wgt1 =  ANNUAL_WEIGHT_R2, wgt2 = ANNUAL_WEIGHT_R2, psu =  PSU, strata =  STRATA,
  include.cor = FALSE, file.cor = "0_meta_analyzed_cor.rds"
)
## Generate online supplement
gfs_generate_supplemental_docs(
  df.raw = df.raw,
  focal.predictor = "PEACE_Y1", focal.better.name = "Inner peace", focal.predictor.reference.value = "rarely or never",
  dir.primary="test/ignore/results-primary", dir.supp="test/ignore/results-cca", dir.attr.models = "test/ignore/results-attr",
  wgt1 =  ANNUAL_WEIGHT_R2, wgt2 = ANNUAL_WEIGHT_R2, psu =  PSU, strata =  STRATA,
  res.dir = "test/ignore/results",
  file.cor = "0_meta_analyzed_cor.rds"
)





# ================================================================================================ #
# ================================================================================================ #
# Set up what your focal exposure/predictor is from Wave 1

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- c("PEACE_Y1") # MUST be the variable name from the GFS Codebook for the variable in the dataset. The name must end in "_Y1"
FOCAL_PREDICTOR_BETTER_NAME <- c("Inner peace") # label/title you want to give your variable. Note, most table, by default already contain "... at Wave 1..." in the title so you do not need to add that here.
FOCAL_PREDICTOR_REFERENCE_VALUE <- c("rarely or never") # what category labels or value (e.g., mean) represents the reference group/value for your focal predictor

# IF your predictor (focal exposure) is binary/categorical, use the code below to define how you
#   want it to be categorized. Categorization must result in a binary variable 0/1 for
#   consistency across studies.
VALUES_DEFINING_UPPER_CATEGORY <- list(c(1:2) )
VALUES_DEFINING_LOWER_CATEGORY <- list(c(3:4) )

# Advanced options
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

names(FORCE_CONTINUOUS) <-
  names(FORCE_BINARY) <-
  names(VALUES_DEFINING_UPPER_CATEGORY)  <-
  names(VALUES_DEFINING_LOWER_CATEGORY) <-
  names(SUBPOPULATION) <-
  names(FOCAL_PREDICTOR_BETTER_NAME) <- FOCAL_PREDICTOR

# outcome vectors
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
  list.composites = get_variable_codes('LIST.COMPOSITES')
)

# ================================================================================================ #
# ================================================================================================ #
# Imputing missing data

run.imp <- FALSE
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
    Miter = 5,
    pred.vars = pred0
  )
}


# ================================================================================================ #
# ================================================================================================ #
# Recode the imputed data for easily use in analyses
# ~~
recode_imp_by_country(
  data.dir,
  nimp = 20,
  list.default = RECODE.DEFAULTS,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  wgt = "ANNUAL_WEIGHT_R2"
)
# ================================================================================================ #
# ================================================================================================ #
# CHECK VARIABLE CODING/COLLAPSING
#
# the following checks the imputed data back with the raw data
tmp.dat1 <- df.raw %>%
  filter(COUNTRY == "United States") %>%
  arrange(ID)
tmp.dat2 <- readr::read_rds(here::here(data.dir, "recoded_imputed_data_obj_United States_imp1.rds")) %>%
  arrange(ID)
dnn0 <- c("Raw Data", "Recoded Imputed Data (.imp==1)")
for(i in 1:length(FOCAL_PREDICTOR)){
  print(FOCAL_PREDICTOR[i])
  print(table(tmp.dat1[[FOCAL_PREDICTOR[i]]], tmp.dat2[[FOCAL_PREDICTOR[i]]], dnn = dnn0, useNA = "ifany"))
}

# ================================================================================================ #
# ================================================================================================ #
# Attrition Weights

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

# Model 1: Run without principal components
plan("multisession", workers = num_cores)
with_progress({
  p <- progressor(along = OUTCOME.VEC0)
  furrr::future_walk(OUTCOME.VEC0, \(x){
    load_packages()
    options(survey.lonely.psu = "certainty")

    walk(FOCAL_PREDICTOR, \(y){
      gfs_run_regression_single_outcome(
        your.outcome = x,
        your.pred = y,
        data.dir = data.dir,
        wgt = ANNUAL_WEIGHT_R2, # wgt = as.name("ANNUAL_WEIGHT_R2")
        psu = PSU, #psu = as.name("PSU")
        strata = STRATA, # strata = as.name("STRATA")
        covariates = DEMO.CHILDHOOD.PRED,
        contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
        list.composites = get_variable_codes('LIST.COMPOSITES'),
        pc.rule = "omit",
        res.dir = "results-primary",
        appnd.txt.to.filename = "_primary_wopc"
      )
    })
    p(sprintf("x= %s", x))
  },.options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())


LIST.RES <- construct_meta_input_from_saved_results("results-primary", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_primary_wopc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

## Primary result - standardized effect sizes
# if predictor & outcome are continuous, change in SD on outcome for 1 SD increase in predictor
# if predictor is continuous & outcome is binary, change in risk of being in upper category for 1 SD increase in predictor
# if predictor & outcome are binary, change in risk of being in upper category for being in upper category on predictor compared to lower category
META.RES <- gfs_meta_analysis(
  meta.input, yi = std.est, sei = std.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Excluded -- Full Imputation Approach"
)
readr::write_rds(
  META.RES,
  file = here::here("results-primary", "0_meta_analyzed_results_primary_wopc.rds"),
  compress = "gz"
)

## Supplemental result - UNstandardized effect sizes
# if predictor & outcome are continuous, change in on outcome for 1 unit increase in predictor
# if predictor is continuous & outcome is binary, change in risk of being in upper category for 1 unit increase in predictor
# if predictor & outcome are binary, change in risk of being in upper category for being in upper category on predictor compared to lower category
META.RES <- gfs_meta_analysis(
  meta.input, yi = est, sei = se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Excluded -- Full Imputation Approach"
)
readr::write_rds(
  META.RES,
  file = here::here("results-primary", "0_meta_analyzed_results_unstd_wopc.rds"),
  compress = "gz"
)
remove(LIST.RES, meta.input, META.RES)


## correlations
construct_meta_input_cor <- function(res.dir, outcomes, predictors, appnd.txt="") {
  local({
    res.dir <- here::here(res.dir)
    tmp.list <- list()
    for (your.outcome in outcomes) {
      for (your.pred in predictors) {
        try({
          load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results",appnd.txt,".RData")))
          tmp.list[[paste0(your.outcome, "_", your.pred)]] <- cor.output
        })
      }
    }
    return(tmp.list)
  })
}
LIST.RES <- construct_meta_input_cor("results-primary", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_primary_wopc")
meta.input.cor <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = outcome,
    OUTCOME = OUTCOME0,
    FOCAL_PREDICTOR0 = predictor,
    FOCAL_PREDICTOR = FOCAL_PREDICTOR0,
    df = df.approx,
    pvalue = p.value,
    Country = COUNTRY
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()
META.RES <- gfs_meta_analysis(
  meta.input.cor, yi = cor.est, sei = cor.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Full Imputation Approach: Pearson-product-moment-correlation"
)
readr::write_rds(
  META.RES,
  file = here::here("results-primary", "0_meta_analyzed_cor.rds"),
  compress = "gz"
)
META.RES|> select(OUTCOME0,theta.rma,theta.rma.se) |>
  mutate(OUTCOME.NAME = get_outcome_better_name(OUTCOME0, FALSE)) |>
  write_csv(file="ip_cors_primary.csv")

# Model 2: Run with principal components
plan("multisession", workers = num_cores)
with_progress({
  p <- progressor(along = OUTCOME.VEC0)
  furrr::future_walk(OUTCOME.VEC0, \(x){
    load_packages()
    options(survey.lonely.psu = "certainty")

    walk(FOCAL_PREDICTOR, \(y){
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
        pc.cutoff = 7,
        pc.rule = "constant",
        res.dir = "results-primary",
        appnd.txt.to.filename = "_primary_wpc"
      )
    })
    p(sprintf("x= %s", x))
  },.options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())

LIST.RES <- construct_meta_input_from_saved_results("results-primary", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_primary_wpc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input, yi = std.est, sei = std.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Included -- Full Imputation Approach"
)
readr::write_rds(
  META.RES,
  file = here::here("results-primary", "0_meta_analyzed_results_primary_wpc.rds"),
  compress = "gz"
)

## Supplemental result - UNstandardized effect sizes
# if predictor & outcome are continuous, change in on outcome for 1 unit increase in predictor
# if predictor is continuous & outcome is binary, change in risk of being in upper category for 1 unit increase in predictor
# if predictor & outcome are binary, change in risk of being in upper category for being in upper category on predictor compared to lower category
META.RES <- gfs_meta_analysis(
  meta.input, yi = est, sei = se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Included -- Full Imputation Approach"
)
readr::write_rds(
  META.RES,
  file = here::here("results-primary", "0_meta_analyzed_results_unstd_wpc.rds"),
  compress = "gz"
)
remove(meta.input, LIST.RES, META.RES)

# ================================================================================================ #
# ================================================================================================ #
# Run supplemental country-wise analyses -- "Complete Case Analysis"
# - Uses attrition-weight adjusted sampling weights

# Supplemental analysis set 1: Run without principal components
plan("multisession", workers = num_cores)
with_progress({
  p <- progressor(along = OUTCOME.VEC0)
  furrr::future_walk(OUTCOME.VEC0, \(x){
    load_packages()
    options(survey.lonely.psu = "certainty")

    walk(FOCAL_PREDICTOR, \(y){
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
        pc.rule = "omit",
        res.dir = "results-cca",
        appnd.txt.to.filename = "_cca_wopc"
      )
    })
    p(sprintf("x= %s", x))
  },.options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())

LIST.RES <- construct_meta_input_from_saved_results("results-cca", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_cca_wopc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input, yi = std.est, sei = std.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Excluded -- Complete Case Analysis"
)
readr::write_rds(
  META.RES,
  file = here::here("results-cca", "0_meta_analyzed_results_cca_wopc.rds"),
  compress = "gz"
)

remove(meta.input, LIST.RES, META.RES)

## correlations
LIST.RES <- construct_meta_input_cor("results-cca", OUTCOME.VEC0, FOCAL_PREDICTOR, appnd.txt = "_cca_wopc")
meta.input.cor <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = outcome,
    OUTCOME = OUTCOME0,
    FOCAL_PREDICTOR0 = predictor,
    FOCAL_PREDICTOR = FOCAL_PREDICTOR0,
    df = df.approx,
    pvalue = p.value,
    Country = COUNTRY
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()
META.RES <- gfs_meta_analysis(
  meta.input.cor, yi = cor.est, sei = cor.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Complete Case Analysis: Pearson-product-moment-correlation"
)
readr::write_rds(
  META.RES,
  file = here::here("results-cca", "0_meta_analyzed_cor.rds"),
  compress = "gz"
)

# Analysis set 2: Run with principal components
plan("multisession", workers = num_cores)
with_progress({
  p <- progressor(along = OUTCOME.VEC0)
  furrr::future_walk(OUTCOME.VEC0, \(x){
    load_packages()
    options(survey.lonely.psu = "certainty")

    walk(FOCAL_PREDICTOR, \(y){
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
        pc.cutoff = 7,
        pc.rule = "constant",
        res.dir = "results-cca",
        appnd.txt.to.filename = "_cca_wpc"
      )
    })
    p(sprintf("x= %s", x))
  },.options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())

LIST.RES <- construct_meta_input_from_saved_results(res.dir="results-cca", outcomes=OUTCOME.VEC0, predictors=FOCAL_PREDICTOR, appnd.txt = "_cca_wpc")
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(
    OUTCOME0 = OUTCOME,
    FOCAL_PREDICTOR0 = FOCAL_PREDICTOR
  ) %>%
  group_by(OUTCOME0, FOCAL_PREDICTOR0) %>%
  nest()

META.RES <- gfs_meta_analysis(
  meta.input, yi = std.est, sei = std.se,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal Components Included -- Complete Case Analysis"
)
readr::write_rds(
  META.RES,
  file = here::here("results-cca", "0_meta_analyzed_results_cca_wpc.rds"),
  compress = "gz"
)

remove(meta.input, LIST.RES, META.RES)

# ================================================================================================ #
# ================================================================================================ #
gc()
# ================================================================================================ #
# ================================================================================================ #
# Construct manuscript tables

# needed results
df.raw <- gfs_get_labelled_raw_data(
  here::here(data.dir, dataset.name),
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  add.whitespace = TRUE, reverse.code.cont = TRUE
)
df.raw <- append_attrition_weights_to_df(data=df.raw)


# main text
gfs_generate_main_doc(
  df.raw = df.raw,
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name = FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE
)


## Generate online supplement
gfs_generate_supplemental_docs(
  df.raw = df.raw,
  focal.predictor = FOCAL_PREDICTOR,
  focal.better.name =  FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value = FOCAL_PREDICTOR_REFERENCE_VALUE
)

