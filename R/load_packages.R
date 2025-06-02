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
    "survey", "srvyr",
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
    "ggthemes",
    "ggstance",
    "pandoc",
    "qpdf",
    "flexlsx",
    "openxlsx2",

    # utility packages
    "tidyverse",
    "dplyr",
    "tidyselect",
    "forcats",
    "fastDummies",
    "haven",
    "ranger",
    "here",
    "remotes",
    "future",
    "furrr",
    "progressr",
    "memuse"
  )
  new.packages <- packages[!(packages %in% utils::installed.packages()[, "Package"])]
  if (length(new.packages)) utils::install.packages(new.packages)
  # Load packages
  purrr::quietly(lapply(packages, library, character.only = TRUE, quietly = TRUE))

    })
  })

}
