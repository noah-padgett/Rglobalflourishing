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

# install.packages("remotes")
#remotes::install_github("noah-padgett/Rglobalflourishing")
#library(Rglobalflourishing)

# Analysis Set-Up

# Add the directory where the dataset is stored on your computer
data.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave1-data"
dataset.name <- "gfs_all_countries_wave1.sav"
#dataset.name <- "gfs_wave_2_test_long_format.sav"

# Specify where you want to output results
# Can be left blank, and the results will output to the same directory as the data.
out.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/3-Rglobalflourishing"

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- "PHYSICAL_HLTH"
FOCAL_PREDICTOR_BETTER_NAME <- "Self-rated physical health"
FOCAL_PREDICTOR_REFERENCE_VALE <- "estimate population mean of self rated physical health"

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
  LIST.COMPOSITES <- get_variable_codes('LIST.COMPOSITES')
  # get "raw data"
  df.raw <- gfs_get_labelled_raw_data(
    file = here::here(data.dir, dataset.name),
    list.composites = LIST.COMPOSITES,
    wave = 1,
    wgt = "ANNUAL_WEIGHT1",
    strata = "STRATA",
    psu = "PSU",
    .test=FALSE
  )

# ================================================================================================ #
# ================================================================================================ #
# Imputing missing data
{
  run.imp <- TRUE
  if (run.imp) {
    df.imp <- run_impute_data(
      data = df.raw,
      data.dir = data.dir,
      Nimp = 3,
      Miter = 1,
      file.name = "gfs_imp_wave1_only.RData"
    )
  }
  load(here::here(data.dir, "gfs_imp_wave1_only.RData"))
  # ~~
  RECODE.DEFAULTS <- list(
    FOCAL_PREDICTOR = FOCAL_PREDICTOR,
    DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
      get_variable_codes("DEMOGRAPHIC.VARS"),
      get_variable_codes("RETROSPECTIVE.VARS")
    ),
    VARIABLES.VEC = c(get_variable_codes("VARS0")),
    FORCE_BINARY = FORCE_BINARY,
    FORCE_CONTINUOUS = FORCE_CONTINUOUS,
    VALUES_DEFINING_UPPER_CATEGORY = VALUES_DEFINING_UPPER_CATEGORY,
    VALUES_DEFINING_LOWER_CATEGORY = VALUES_DEFINING_LOWER_CATEGORY,
    USE_DEFAULT = !(FORCE_BINARY | FORCE_CONTINUOUS)
  )
  df.imp.long <- recode_imputed_data(
    df.imp, wave = 1,
    list.default = RECODE.DEFAULTS,
    list.composites = LIST.COMPOSITES
  )
}

# the following checks the imputed data back with the raw data
tmp.dat1 <- df.raw %>%
  arrange(ID)
tmp.dat2 <- df.imp.long %>%
  filter(.imp == 1) %>%
  arrange(ID)
dnn0 <- c("Raw Data", "Recoded Imputed Data (.imp==1)")
table(tmp.dat1[[FOCAL_PREDICTOR]], tmp.dat2[[FOCAL_PREDICTOR]], dnn=dnn0)

# ================================================================================================ #
# ================================================================================================ #
{
DEMO.CHILDHOOD.PRED <-
  c(
    "COV_AGE_GRP",
    "COV_GENDER",
    "COV_EDUCATION_3",
    "COV_EMPLOYMENT",
    "COV_MARITAL_STATUS",
    "COV_ATTEND_SVCS",
    "COV_BORN_COUNTRY",
    "COV_PARENTS_12YRS",
    "COV_SVCS_12YRS",
    "COV_MOTHER_RELATN",
    "COV_FATHER_RELATN",
    "COV_OUTSIDER",
    "COV_ABUSED",
    "COV_HEALTH_GROWUP",
    "COV_INCOME_12YRS",
    "COV_REL1",
    "COV_RACE_PLURALITY",
    "COV_MOTHER_NA",
    "COV_FATHER_NA"
  )
  OUTCOME.VEC <- RECODE.DEFAULTS[['VARIABLES.VEC']]
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
OUTCOME.VEC0 <- OUTCOME.VEC # c(1, 8, 24)+76,
your.outcome <- OUTCOME.VEC0[17]

# Analysis set 1: Run without principal components
LIST.RES1 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    list.composites = get_variable_codes('LIST.COMPOSITES')[[1]],
    # advanced options: only change if you know what you are doing
    standardize = TRUE,
    res.dir = "results-wopc",
    pc.rule = "omit"
  )
}, .progress = TRUE)

# Analysis set 2: Run with principal components
LIST.RES2 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    list.composites = get_variable_codes('LIST.COMPOSITES')[[1]],
    # advanced options: only change if you know what you are doing
    standardize = TRUE,
    res.dir = "results-wpc",
    pc.cutoff = 7,
    pc.rule = "constant"
  )
}, .progress = TRUE)
}
# ================================================================================================ #
# ================================================================================================ #
# run meta-analyses
{
# Analysis set 1: country-specific results without adjusting for principal components
LIST.RES1 <- construct_meta_input_from_saved_results("results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES1 %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

META.RES1 <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal components excluded"
)
readr::write_rds(
  META.RES1,
  file = here::here(out.dir, "results-wopc", "0_meta_analyzed_results_wopc.rds"),
  compress = "gz"
)

# Analysis set 2: country-specific results adjusting for principal components
LIST.RES2 <- construct_meta_input_from_saved_results("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
meta.input <- LIST.RES2 %>%
  bind_rows() %>%
  mutate(OUTCOME0 = OUTCOME) %>%
  group_by(OUTCOME0) %>%
  nest()

META.RES2 <- gfs_meta_analysis(
  meta.input,
  better.name = FOCAL_PREDICTOR_BETTER_NAME,
  p.subtitle = "Principal components included"
)

readr::write_rds(
  META.RES2,
  file = here::here(out.dir, "results-wpc","0_meta_analyzed_results_wpc.rds"),
  compress = "gz"
)
}
# ================================================================================================ #
# ================================================================================================ #
# Construct manuscript tables

df.raw <- gfs_get_labelled_raw_data(
  here::here(data.dir, dataset.name),
  list.composites = LIST.COMPOSITES,
  add.whitespace = TRUE, .test=TRUE
)
COUN.RES.WOPC <- get_country_specific_regression_results("results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
COUN.RES.WPC <- get_country_specific_regression_results("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
FIT.PCA.SUM <- get_country_specific_pca_summary("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)
META.RES1 <- readr::read_rds(file = here::here(out.dir, "results-wopc", "0_meta_analyzed_results_wopc.rds"))
META.RES2 <- readr::read_rds(file = here::here(out.dir, "results-wpc", "0_meta_analyzed_results_wpc.rds"))

# main text
gfs_generate_main_doc(
  df.raw = df.raw,
  meta.wopc=META.RES1,
  meta.wpc=META.RES2,
	focal.predictor=FOCAL_PREDICTOR,
	focal.better.name=FOCAL_PREDICTOR_BETTER_NAME,
	focal.predictor.reference.value=FOCAL_PREDICTOR_REFERENCE_VALE,
	res.dir = "results"
)

# online supplement 1 (additional meta-analysis tables + country specific results)
gfs_generate_supplemental_docs(
  df.raw = df.raw,
	meta.wopc=META.RES1,
  meta.wpc=META.RES2,
	coun.wopc=COUN.RES.WOPC ,
  coun.wpc=COUN.RES.WPC ,
	coun.fit.pca = FIT.PCA.SUM ,
	focal.predictor=FOCAL_PREDICTOR,
	focal.better.name=FOCAL_PREDICTOR_BETTER_NAME,
	focal.predictor.reference.value=FOCAL_PREDICTOR_REFERENCE_VALE,
	res.dir = "results",
  what = "S1"
)


# online supplement 2 (re-oriented summary tables tables + forest plots)
gfs_generate_supplemental_docs(
  df.raw = df.raw,
  meta.wopc=META.RES1,
  meta.wpc=META.RES2,
  coun.wopc=COUN.RES.WOPC ,
  coun.wpc=COUN.RES.WPC ,
  coun.fit.pca = FIT.PCA.SUM ,
  focal.predictor=FOCAL_PREDICTOR,
  focal.better.name=FOCAL_PREDICTOR_BETTER_NAME,
  focal.predictor.reference.value=FOCAL_PREDICTOR_REFERENCE_VALE,
  res.dir = "results",
  what = "S2"
)
