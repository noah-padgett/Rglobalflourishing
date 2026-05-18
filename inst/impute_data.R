# Imputing Data Script
# Script: impute_data.R
# Created by: R. Noah Padgett & Chris Felton
# Last edited on: 2026-04-13

# WARNING: The package was set up to be as user-friendly as possible for researchers
#	part of the GFS core team who mainly have experience with other statistical analysis
#	software such as STATA, SAS, and SPSS.
#	This package and implementation of the analyses for Wave 3 of the Global Flourishing
#	Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

## ============================================================================================== ##
## ============================================================================================== ##
# ---- Part 1. R environment preparation ----

#install.packages("remotes")
#remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)
#

library(Rglobalflourishing)

# Add the directory where the dataset is stored on your computer
data.dir <- "test/ignore/data"
dataset.name <- "gfs_all_countries_Y1_Y2_Y3.sav"

# The following function loads the required packages for the remainder of the script to work.
load_packages()
# global options
options(
  survey.lonely.psu = "certainty",
  future.plan = "multisession"
)
handlers(global = TRUE)
if (availableCores(constraints = "connections") == 2) {
  num_cores <- 1
} else if (availableCores(constraints = "connections") %% 2 == 0) {
  num_cores <- availableCores(constraints = "connections")/2
} else if (availableCores(constraints = "connections") %% 2 == 1) {
  num_cores <- availableCores(constraints = "connections")/2 - 0.5
}

# read in data
df.raw <- gfs_get_labelled_raw_data(
  file = here::here(data.dir, dataset.name),
  wave = 3,
  list.composites = get_variable_codes('LIST.COMPOSITES')
)

df.imp <- run_impute_data(
  data =  df.raw |> filter(COUNTRY %in% c("Hong Kong", "South Africa", "Turkey")),
  data.dir = data.dir,
  save.method = "combined",
  Nimp = 20,
  Miter = 5, use.parallel = TRUE
)
