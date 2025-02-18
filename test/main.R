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
data.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave2-data"
#dataset.name <- "gfs_all_countries_wave1.sav"
dataset.name <- "df_w2_sim_long.sav"

# Specify where you want to output results
# Can be left blank, and the results will output to the same directory as the data.
out.dir <- "/Users/noahp/Documents/GitHub/global-flourishing-study/3-Rglobalflourishing"

# Here is YOUR wave 1 construct variable
FOCAL_PREDICTOR <- "PHYSICAL_HLTH_W1"
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
    .test = TRUE
  )

# ================================================================================================ #
# ================================================================================================ #
# Imputing missing data
{
  run.imp <- TRUE
  if (run.imp) {
    df.tmp <- run_attrition_model(
      data = df.raw,
      obs.id.var = "CASE_OBSERVED_W2",
      attr.pred = c(
        "ANNUAL_WEIGHT_R1", "STRATA", "MODE_ANNUAL_W1",
        "AGE_W1", "GENDER_W1", "EDUCATION_3_W1", "INCOME_QUINTILE_W1",
        "EMPLOYMENT_W1", "MARITAL_STATUS_W1", "RACE_PLURALITY_W1"
      ),
      wgt = "ANNUAL_WEIGHT_R1", strata = "STRATA", psu = "PSU"
    )
    df.imp <- run_impute_data(
      data = df.tmp,
      data.dir = data.dir,
      Nimp = 3,
      Miter = 1
    )
  }
  load(here::here(data.dir, "gfs_imputed_data_test.RData"))
  # ~~
  RECODE.DEFAULTS <- list(
    FOCAL_PREDICTOR = FOCAL_PREDICTOR,
    DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
      get_variable_codes("DEMOGRAPHIC.VARS", appnd="_W1"),
      get_variable_codes("RETROSPECTIVE.VARS", appnd="_W1")
    ),
    VARIABLES.VEC = c(get_variable_codes("VARS.W1"), get_variable_codes("VARS.W2")),
    FORCE_BINARY = FORCE_BINARY,
    FORCE_CONTINUOUS = FORCE_CONTINUOUS,
    VALUES_DEFINING_UPPER_CATEGORY = VALUES_DEFINING_UPPER_CATEGORY,
    VALUES_DEFINING_LOWER_CATEGORY = VALUES_DEFINING_LOWER_CATEGORY,
    USE_DEFAULT = !(FORCE_BINARY | FORCE_CONTINUOUS)
  )
  df.imp.long <- recode_imputed_data(
    df.imp,
    list.default = RECODE.DEFAULTS,
    list.composites = LIST.COMPOSITES,
    wgt = "ANNUAL_WEIGHT_R1"
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
table(tmp.dat1[[FOCAL_PREDICTOR]], tmp.dat2[[FOCAL_PREDICTOR]], dnn=dnn0, useNA="ifany")

# ================================================================================================ #
# ================================================================================================ #
{
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
  VARIABLES.VEC <- RECODE.DEFAULTS[['VARIABLES.VEC']]

  OUTCOME.VEC <- VARIABLES.VEC[str_detect(VARIABLES.VEC, "_W2")]
  CONTEMPORANEOUS.EXPOSURES.VEC <- VARIABLES.VEC[str_detect(VARIABLES.VEC, "COMPOSITE", negate = TRUE)]
  CONTEMPORANEOUS.EXPOSURES.VEC <- CONTEMPORANEOUS.EXPOSURES.VEC[str_detect(CONTEMPORANEOUS.EXPOSURES.VEC, "_W1")]

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
OUTCOME.VEC0 <- OUTCOME.VEC[1:10] # c(1, 8, 24)+76,
your.outcome <- OUTCOME.VEC0[1]

# Analysis set 1: Run without principal components
LIST.RES1 <- map(OUTCOME.VEC0, \(x){
  gfs_run_regression_single_outcome(
    data = df.imp.long,
    your.pred = FOCAL_PREDICTOR,
    your.outcome = x,
    covariates = DEMO.CHILDHOOD.PRED,
    contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
    list.composites = LIST.COMPOSITES[[1]],
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
    list.composites = LIST.COMPOSITES[[1]],
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
  add.whitespace = TRUE, .test=FALSE
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




plot.dat <- META.RES2 %>%
	select(OUTCOME0, data, theta.rma, theta.rma.se, theta.rma.ci) %>%
	mutate(
		type = get_outcome_scale(OUTCOME0),
		type = case_when(
			type == "cont" ~ "Std. Est",
			.default = "log(RR)"
		)
	) %>%
	unnest(c(data))


p <- plot.dat %>%
	group_by(Country) %>%
	mutate(
		Est.avg = mean(Est)
	) %>%
	ggplot(aes(x=reorder(Country, -Est.avg), y = Est)) +
	geom_jitter(alpha = 0.6, shape = 16, size = 2,
              position = position_jitter(height = 0, width = .2, seed = 1)) +
  facet_wrap(.~type, ncol=1) +
  ylab("Estimated Treatment Effect") +
  xlab("") +
  scale_x_discrete(guide = guide_axis(angle = 60))
              p



outcomes = OUTCOME.VEC0
predictors = FOCAL_PREDICTOR
res.dir = "results-wpc"
tmp.list <- list()
  for (your.outcome in outcomes) {
    for (your.pred in predictors) {
      load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results.RData")))
      # create new columns in output to help with constructing tables
      output <- output
      tmp.list[[paste0(your.outcome, "_", your.pred)]] <- output
    }
  }

tmp <- get_country_specific_output("results-wpc", OUTCOME.VEC0, FOCAL_PREDICTOR)

tmp %>%
filter( !(term %in% c("(Intercept)", "FOCAL_PREDICTOR"))) %>%
filter(EE < quantile(EE, 0.99)) %>%
ggplot(aes(x=EE)) +
	geom_density()

tmp1 <- get_country_specific_output("results-wopc", OUTCOME.VEC0, FOCAL_PREDICTOR)
dat1 <- tmp1 %>% filter( (term %in% "FOCAL_PREDICTOR"))
dat2 <- tmp %>% filter( (term %in% paste0("PC_",1:7)))
dat3 <- tmp %>% filter( !(term %in% c("(Intercept)", "FOCAL_PREDICTOR",paste0("PC_",1:7))))  %>%
filter(EE < quantile(EE, 0.99))

ggplot(dat1, aes(x=EE, group=COUNTRY)) +
	geom_vline(aes(xintercept=EE)) +
	geom_density(data=dat2, aes(x=EE), color="blue", alpha=0.5) +
	geom_density(data=dat3, aes(x=EE), color="red", alpha=0.5)



