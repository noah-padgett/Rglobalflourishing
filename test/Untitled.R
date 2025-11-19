## script to test conducting subgroup means with Rglobalflourishing package

## ============================================================================================== ##
## ============================================================================================== ##
## Part 1. R environment preparation

#install.packages("remotes")
#remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)
#library(Rglobalflourishing)

load_packages()
# global options
options(
  survey.lonely.psu = "certainty",
  future.plan = "multisession"
)
handlers(global = TRUE)
num_cores <- availableCores(constraints = "connections")

## ============================================================================================== ##
## ============================================================================================== ##
## Part 3. Load in "raw data"
df.raw <- gfs_get_labelled_raw_data(
  file = here::here("data", "gfs_all_countries_wave2.sav" ),
  list.composites = get_variable_codes("LIST.COMPOSITES")
)

## ============================================================================================== ##
## ============================================================================================== ##
## Part 4. Imputing missing data

run.imp <- FALSE # turn back to TRUE after chris runs it.
if (run.imp) {
  # Default set of baseline predictors.
  # Note. Internally, all (*)_Y1 variables are also used to predict (*)_Y2 variables
  pred0 <- c(
    'ANNUAL_WEIGHT_C1', 'MODE_RECRUIT',
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

## ============================================================================================== ##
## ============================================================================================== ##
## Part 5. Recode the imputed data for easily use in analyses
# ~~
recode_imp_by_country(
  data.dir,
  list.default = RECODE.DEFAULTS,
  list.composites = LIST.COMPOSITES,
  wgt = "ANNUAL_WEIGHT_C1"
)

# ============================================================================================== ##
## ============================================================================================== ##
## Part 6. run analyses by religious subpopulation


OUTCOME.VEC <- get_variable_codes("VARS.Y1")
DEMO.CHILDHOOD.PRED <- c(
  "COV_AGE_GRP_Y1",
  "COV_GENDER",
  "COV_EDUCATION_3_Y1",
  "COV_EMPLOYMENT_Y1",
  "COV_MARITAL_STATUS_Y1",
  "COV_ATTEND_SVCS_Y1",
  "COV_BORN_COUNTRY_Y1"
)


## Buddhism

plan("multisession", workers = num_cores)
x <- OUTCOME.VEC[1]
y <- DEMO.CHILDHOOD.PRED[2]
mycond <- expand.grid(OUTCOME= OUTCOME.VEC, DEM=DEMO.CHILDHOOD.PRED)
with_progress({
  p <- progressor(along = mycond[1:10,1])
  furrr::future_walk2(mycond[1:10,1],mycond[1:10,2], \(x,y){
    #library(Rglobalflourishing)
    devtools::load_all()
    load_packages()
    options(survey.lonely.psu = "certainty")
      gfs_svyglm_mi(
        fx = as.formula(paste0(x, " ~ 0 + ", y)), #COMPOSITE_FLOURISHING_SECURE_Y1 ~ 0 + COV_AGE_GRP_Y1,
        country.subset = c("United States", "China", "Hong Kong", "India", "Japan"),
        domain.subset = quote(REL1_Y1 == 4), # buddhism
        data.dir = "test/ignore/data",
        wgt = ANNUAL_WEIGHT_C1, # wgt = as.name("ANNUAL_WEIGHT_R2")
        psu = PSU, #psu = as.name("PSU")
        strata = STRATA, # strata = as.name("STRATA"),
        res.dir = "test/ignore/results-buddhism",
        appnd.txt.to.filename = paste0("_",y),
        force.linear = TRUE
      )
    p(sprintf("x= %s", x))
  })
})

future::resetWorkers(plan())


