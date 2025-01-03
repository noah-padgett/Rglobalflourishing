# Script: main.R
# Created by: R. Noah Padgett
# Last edited on: 2024-01-02

# WARNING: The package was set up to be user-friendly for researchers part of the GFS core
#   team who mainly have experience with other statistical analysis software such as STATA,
#   SAS, and SPSS. This package and implementation of the analyses for Wave 2 of the Global
#   Flourishing Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

# Analysis Set-Up

# Add the directory where the dataset is stored on your computer
data.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave1-data/"
dataset.name <- "gfs_test_2_waves.sav" # "gfs_all_countries_wave1.sav"

# Specify where you want to output results
# Can be left blank, and the results will output to the same directory as the data.
out.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/3-Rglobalflourishing/"

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- "PHYSICAL_HLTH_W1"
FOCAL_PREDICTOR_BETTER_NAME <- "Self-rated physical health at wave 1"

# IF your predictor is binary/categorical, use the code below to define how you want it to be
# 	categorized. Categorization must result in a binary variable 0/1 for consistency across studies.
# 	Please report how you have categorized your variable to Noah (npadgett@hsph.harvard.edu)
VALUES_DEFINING_UPPER_CATEGORY <- c(NULL)
VALUES_DEFINING_LOWER_CATEGORY <- c(NULL)
# Note 1: if your focal predictor is continuous (all items with 7+ response options), you can force the responses
# 	to be categorized as 0/1 using the above with the below option changed to TRUE. This can be useful
# 	when testing the sensitivity of results or for composite outcomes such as anxiety (sum of
# feel_anxious and control_worry)  or depression (sum of depressed and interest) that have a
# 	history of being dichotomized.
FORCE_BINARY <- FALSE
# Note 2: if your focal predictor is categorical/binary, you can use the responses as if they were continuous.
# 	This can be done in several ways, but the provided (straightforward-ish) approach is to reverse
# 	code all ordered-categorical variables (reverse code from what is reported in the codebook), and
# standardized as if continuous. This approach is not applicable for variables with nominal
# response categories such as employment. This is employed using the option below.
FORCE_CONTINUOUS <- FALSE

# ================================================================================================ #
# ================================================================================================ #
# Data Prep
{
  if (is.null(out.dir)) {
    out.dir <- data.dir
  }

  # setwd(out.dir)
  # Note:
  # The following function loads the required packages for the remainder of the script to work.
  load_packages()

  # global options
  options(
    survey.lonely.psu = "certainty"
  )

  # outcome vectors
  OUTCOME.VEC <- get_variable_codes()[['OUTCOME.VEC']]
  LIST.COMPOSITES <- get_variable_codes()[['LIST.COMPOSITES']]
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC <- get_variable_codes()[['DEMOGRAPHICS.CHILDHOOD.PRED.VEC']]
  # get "raw data"
  df.raw <- gfs_get_labelled_raw_data(
    paste0(data.dir, dataset.name),
    list.composites = LIST.COMPOSITES
  )
}

# ================================================================================================ #
# ================================================================================================ #
# Imputing missing data
{
  run.imp <- TRUE
  if (run.imp) {
    df.tmp <- run_attrition_model(
      df.raw,
      attr.pred = c(
        "ANNUAL_WEIGHT1_W1", "MODE_RECRUIT_W1", "AGE_W1", "GENDER_W1", "EDUCATION_3_W1",
        "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1"
      )
    )
    df.imp <- run_impute_data(
      data = df.tmp,
      data.dir = data.dir,
      Nimp = 20,
      Miter = 2
    )
  }
  load(paste0(data.dir, "/gfs_imputed_data_test.RData"))

  RECODE.DEFAULTS <- list(
    FOCAL_PREDICTOR = FOCAL_PREDICTOR,
    DEMOGRAPHICS.CHILDHOOD.PRED.VEC = DEMOGRAPHICS.CHILDHOOD.PRED.VEC,
    OUTCOME.VEC = OUTCOME.VEC,
    FORCE_BINARY = FORCE_BINARY,
    FORCE_CONTINUOUS = FORCE_CONTINUOUS,
    VALUES_DEFINING_UPPER_CATEGORY = VALUES_DEFINING_UPPER_CATEGORY,
    VALUES_DEFINING_LOWER_CATEGORY = VALUES_DEFINING_LOWER_CATEGORY,
    USE_DEFAULT = !(FORCE_BINARY | FORCE_CONTINUOUS)
  )
  df.imp.long <- recode_imputed_data(
    df.imp,
    list.default = RECODE.DEFAULTS, list.composites = LIST.COMPOSITES
  )
}

# ================================================================================================ #
# ================================================================================================ #

DEMO.CHILDHOOD.PRED <-
  c(
    "COV_AGE_GRP_W1",
    "COV_GENDER_W1",
    "COV_EDUCATION_3_W1",
    "COV_EMPLOYMENT_W1",
    "COV_MARITAL_STATUS_W1",
    "COV_ATTEND_SVCS_W1",
    "COV_BORN_COUNTRY_W1",
    "COV_PARENTS_12YRS_W1",
    "COV_SVCS_12YRS_W1",
    "COV_MOTHER_RELATN_W1",
    "COV_FATHER_RELATN_W1",
    "COV_OUTSIDER_W1",
    "COV_ABUSED_W1",
    "COV_HEALTH_GROWUP_W1",
    "COV_INCOME_12YRS_W1",
    "COV_REL1_W1",
    "COV_RACE_PLURALITY",
    "COV_MOTHER_NA",
    "COV_FATHER_NA"
  )
CONTEMPORANEOUS.EXPOSURES.VEC <- OUTCOME.VEC[str_detect(OUTCOME.VEC, "COMPOSITE", negate = TRUE)]


# system.time({
# gfs_run_regression_single_outcome(
#    data = df.imp.long,
#    your.pred = "PHYSICAL_HLTH",
#    your.outcome = OUTCOME.VEC[1], # happy (continuous)
#    covariates=DEMO.CHILDHOOD.PRED,
#    contemporaneous.exposures=CONTEMPORANEOUS.EXPOSURES.VEC,
#    # advanced options: only change if you know what you are doing
#    standardize = TRUE,
#    force.linear = FALSE,
#    robust_huberM = FALSE,
#    robust.tune=2,
#    res.dir=NULL,
#    pc.cutoff = 7,
#    pc.rule = "constant"
# )
# })
# with all 20 imputed datasets...
# runtimes: ~6.5 minutes for HAPPY (continuous)
# runtimes: ~7.5 minutes for LIFE_BALANCE (binary)

# Run country-specific regression analyses for ALL wave 2 outcomes
your.outcome <- OUTCOME.VEC[1]
OUTCOME.VEC0 <- OUTCOME.VEC[c(1, 8, 24)+76]

# Analysis set 1: Run without principal components
LIST.RES <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    # advanced options: only change if you know what you are doing
    standardize = TRUE,
    force.linear = FALSE,
    robust_huberM = FALSE,
    robust.tune = 2,
    res.dir = "results-wopc/",
    pc.rule = "omit"
  )
}, .progress = TRUE)

# Analysis set 2: Run with principal components
LIST.RES0 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    # advanced options: only change if you know what you are doing
    standardize = TRUE,
    force.linear = FALSE,
    robust.huberM = FALSE,
    robust.tune = 2,
    res.dir = "results-wpc/",
    pc.cutoff = 7,
    pc.rule = "constant"
  )
}, .progress = TRUE)

# ================================================================================================ #
# ================================================================================================ #
# run meta-analyses

# Analysis set 1: country-specific results without adjusting for principal components
LIST.RES <- construct_meta_input_from_saved_results("results-wopc/", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

res <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal components excluded"
)
readr::write_rds(
  res,
  file = paste0(out.dir, "results-wopc/0_meta_analyzed_results_wopc.rds"),
  compress = "gz"
)

# Analysis set 2: country-specific results adjusting for principal components
LIST.RES <- construct_meta_input_from_saved_results("results-wpc/", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

res <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal components included"
)

readr::write_rds(
  res,
  file = paste0(out.dir, "results-wpc/0_meta_analyzed_results_wpc.rds"),
  compress = "gz"
)
