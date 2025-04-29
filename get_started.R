devtools::load_all()
load_packages()


data.dir <- "/Users/chris_felton/Documents/GFSw20"
dataset.name <- "gfs_all_countries_wave2.sav"

FOCAL_PREDICTOR <- c("PHYSICAL_HLTH_Y1")
FOCAL_PREDICTOR_BETTER_NAME <- c("Self-rated physical health")
FOCAL_PREDICTOR_REFERENCE_VALUE <- c("mean rating within country")

VALUES_DEFINING_UPPER_CATEGORY <- list(NA)
VALUES_DEFINING_LOWER_CATEGORY <- list(NA)

FORCE_BINARY <- c(FALSE)

FORCE_CONTINUOUS <- c(FALSE)

SUBPOPULATION <- list(NULL)

names(FORCE_CONTINUOUS) <- names(FORCE_BINARY) <- names(VALUES_DEFINING_UPPER_CATEGORY)  <- names(VALUES_DEFINING_LOWER_CATEGORY) <- names(SUBPOPULATION) <- FOCAL_PREDICTOR

if (!exists("out.dir")) {
  out.dir <- data.dir
}
setwd(out.dir)

load_packages()

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

library(tictoc)
