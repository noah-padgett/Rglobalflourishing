devtools::load_all()

library(here)

load_packages()

data.dir <- "/Users/chris_felton/Documents/GFSw20"
dataset.name <- "gfs_all_countries_wave2.sav"


FOCAL_PREDICTOR <- "PHYSICAL_HLTH_Y1"
FOCAL_PREDICTOR_BETTER_NAME <- "Self-Rated Physical Health"
FOCAL_PREDICTOR_REFERENCE_VALUE <- "mean rating within country"

VALUES_DEFINING_UPPER_CATEGORY <- NULL
VALUES_DEFINING_LOWER_CATEGORY <- NULL
FORCE_BINARY <- FALSE
FORCE_CONTINUOUS <- FALSE

if (!exists("out.dir")) {
  out.dir <- data.dir
}
setwd(out.dir)

options(
  survey.lonely.psu = "certainty"
)

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

df.raw <- gfs_get_labelled_raw_data(
  file = here::here(data.dir, dataset.name),
  list.composites = LIST.COMPOSITES
)
