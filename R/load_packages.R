#' Load Packages
#'
#' Function to load the required packages into the environment
#'
#' @param ... no direct parameters
#' @returns nothing (only background global environment changes)
#' @examples {
#'   load_packages()
#' }
#' @export
load_packages <- function(...) {
  suppressMessages({
    suppressWarnings({
        # list of packages
  packages <- c(
    # Core packages
    "survey",
    "gtsummary",
    "mice",
    "EValue",
    "metafor",
    "harmonicmeanp",

    # "robust" variants of analyses
    "robsurvey",
    "robustbase",

    # formatting and output packages
    "knitr",
    "flextable",
    "officer",
    "patchwork",

    # utility packages
    "tidyverse",
    "dplyr",
    "tidyselect",
    "future",
    "forcats",
    "fastDummies",
    "haven",
    "ranger",
    "here",
    "remotes"
  )
  new.packages <- packages[!(packages %in% utils::installed.packages()[, "Package"])]
  if (length(new.packages)) utils::install.packages(new.packages)
  # Load packages
  lapply(packages, library, character.only = TRUE, quietly = TRUE)

    })
  })

}
