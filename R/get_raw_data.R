#' Estimate and Save Attrition Weights
#'
#' Estimate (save fitted model) and append attrition weights to data.frame
#'
#' @param file name of file to get data from
#' @param list.composites a lit of lists to construct composite variables
#' @param ... other arguments
#' @returns a dataset with attrition weights appended
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_get_labelled_raw_data <- function(file, list.composites = NULL, ...) {
  # IF SPSS file format
  if (stringr::str_detect(stringr::str_to_lower(file), ".sav")) {
    df.original0 <- haven::read_spss(file)
    df.original <- haven::zap_labels(df.original0)
  }
  if (stringr::str_detect(stringr::str_to_lower(file), ".dta")) {
    df.original0 <- haven::read_spss(file)
    df.original <- haven::zap_labels(df.original0)
  }

  if (stringr::str_detect(stringr::str_to_lower(file), ".csv")) {
    df.original0 <- haven::read_dta(file)
    df.original <- haven::zap_labels(df.original0)
  }

  # Restructure to "wide" data
  df.original <- gsf_data_to_wide(df.original)

  df.original <- df.original %>%
    dplyr::mutate(
      COUNTRY = recode_labels(COUNTRY_W1, "COUNTRY_W1"),
      COUNTRY = factor(COUNTRY),
      AGE_GRP_W1 = dplyr::case_when(
        AGE_W1 >= 18 & AGE_W1 < 25 ~ "18-24",
        AGE_W1 >= 25 & AGE_W1 < 30 ~ "25-29",
        AGE_W1 >= 30 & AGE_W1 < 40 ~ "30-39",
        AGE_W1 >= 40 & AGE_W1 < 50 ~ "40-49",
        AGE_W1 >= 50 & AGE_W1 < 60 ~ "50-59",
        AGE_W1 >= 60 & AGE_W1 < 70 ~ "60-69",
        AGE_W1 >= 70 ~ "70plus",
        .default = "(Missing)"
      ),
      AGE_GRP_W1 = factor(AGE_GRP_W1),
      AGE_GRP_W1 = relevel(AGE_GRP_W1, ref = "18-24"),
    )

  # ============================================================================ #
  # ============================================================================ #

  # Create composites IF any have been specified in the outcome_variables file
  if (!is.null(list.composites)) {
    LIST.OUTCOME.COMPOSITES <- list.composites[["LIST.OUTCOME.COMPOSITES"]]
    LIST.COMPOSITE.COMBINE.METHOD <- list.composites[["LIST.COMPOSITE.COMBINE.METHOD"]]
    COMPOSITE.VEC <- list.composites[["COMPOSITE.VEC"]]

    for (i in 1:length(LIST.OUTCOME.COMPOSITES)) {
      # create a temporary variable then rename
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "mean") {
        df.original <- df.original %>%
          dplyr::mutate(
            temp.var.i = rowMeans(
              dplyr::across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column())
                x
              }),
              na.rm = FALSE
            )
          )
      }
      if (LIST.COMPOSITE.COMBINE.METHOD[[i]] == "sum") {
        df.original <- df.original %>%
          dplyr::mutate(
            temp.var.i = rowSums(
              dplyr::across(all_of(LIST.OUTCOME.COMPOSITES[[i]]), \(x){
                x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
                x <- recode_to_type(x, cur_column())
                x <- reorder_levels(x, cur_column())
                x <- recode_to_numeric(x, cur_column(), TRUE)
                x
              }),
              na.rm = FALSE
            )
          )
      }
      colnames(df.original)[length(colnames(df.original))] <- COMPOSITE.VEC[i]
    }
  }

  df.original <- df.original %>%
    mutate(
      COUNTRY2 = COUNTRY_W1, # having access to country within map(.) is sometimes useful when referencing objects outside data.frame(.)
      dplyr::across(!(contains("COMPOSITE_") | contains("COUNTRY") | AGE_GRP_W1), \(x){
        x <- dplyr::case_when(x %in% get_missing_codes(cur_column()) ~ NA, .default = x)
        x <- recode_labels(x, cur_column())
        x <- recode_to_type(x, cur_column())
        x
      }, .names = "{.col}")
    )

  # Recode RACE (imputation datasets only)
  df.original <- df.original %>%
    dplyr::mutate(
      RACE = SELFID1_W1,
      RACE_PLURALITY_W1 = dplyr::case_when(
        COUNTRY == "Argentina" & stringr::str_detect(RACE, "White") ~ 0,
        COUNTRY == "Argentina" & stringr::str_detect(RACE, "White", negate = TRUE) ~ 1,
        COUNTRY == "Australia" & stringr::str_detect(RACE, "Australian") ~ 0,
        COUNTRY == "Australia" & stringr::str_detect(RACE, "Australian", negate = TRUE) ~ 1,
        COUNTRY == "Brazil" & stringr::str_detect(RACE, "Branca") ~ 0,
        COUNTRY == "Brazil" & stringr::str_detect(RACE, "Branca", negate = TRUE) ~ 1,
        COUNTRY == "Egypt" & stringr::str_detect(RACE, "Arab") ~ 0,
        COUNTRY == "Egypt" & stringr::str_detect(RACE, "Arab", negate = TRUE) ~ 1,
        COUNTRY == "Germany" ~ NA,
        COUNTRY == "Hong Kong" & stringr::str_detect(RACE, "Chinese (Cantonese)") ~ 0,
        COUNTRY == "Hong Kong" & stringr::str_detect(RACE, "Chinese (Cantonese)", negate = TRUE) ~ 1,
        COUNTRY == "India" & stringr::str_detect(RACE, "Other backward caste") ~ 0,
        COUNTRY == "India" & stringr::str_detect(RACE, "Other backward caste", negate = TRUE) ~ 1,
        COUNTRY == "Indonesia" & stringr::str_detect(RACE, "Jawa") ~ 0,
        COUNTRY == "Indonesia" & stringr::str_detect(RACE, "Jawa", negate = TRUE) ~ 1,
        COUNTRY == "Israel" & stringr::str_detect(RACE, "Jewish") ~ 0,
        COUNTRY == "Israel" & stringr::str_detect(RACE, "Jewish", negate = TRUE) ~ 1,
        COUNTRY == "Japan" ~ NA,
        COUNTRY == "Kenya" & stringr::str_detect(RACE, "Kikuyu") ~ 0,
        COUNTRY == "Kenya" & stringr::str_detect(RACE, "Kikuyu", negate = TRUE) ~ 1,
        COUNTRY == "Mexico" & stringr::str_detect(RACE, "Mestizo") ~ 0,
        COUNTRY == "Mexico" & stringr::str_detect(RACE, "Mestizo", negate = TRUE) ~ 1,
        COUNTRY == "Nigeria" & stringr::str_detect(RACE, "Hausa") ~ 0,
        COUNTRY == "Nigeria" & stringr::str_detect(RACE, "Hausa", negate = TRUE) ~ 1,
        COUNTRY == "Philippines" & stringr::str_detect(RACE, "Tagalog") ~ 0,
        COUNTRY == "Philippines" & stringr::str_detect(RACE, "Tagalog", negate = TRUE) ~ 1,
        COUNTRY == "Poland" & stringr::str_detect(RACE, "Polish") ~ 0,
        COUNTRY == "Poland" & stringr::str_detect(RACE, "Polish", negate = TRUE) ~ 1,
        COUNTRY == "South Africa" & stringr::str_detect(RACE, "Black") ~ 0,
        COUNTRY == "South Africa" & stringr::str_detect(RACE, "Black", negate = TRUE) ~ 1,
        COUNTRY == "Spain" ~ NA,
        COUNTRY == "Sweden" ~ NA,
        COUNTRY == "Tanzania" & stringr::str_detect(RACE, "African") ~ 0,
        COUNTRY == "Tanzania" & stringr::str_detect(RACE, "African", negate = TRUE) ~ 1,
        COUNTRY == "Turkiye" & stringr::str_detect(RACE, "Turkish") ~ 0,
        COUNTRY == "Turkiye" & stringr::str_detect(RACE, "Turkish", negate = TRUE) ~ 1,
        COUNTRY == "United Kingdom" & stringr::str_detect(RACE, "White") ~ 0,
        COUNTRY == "United Kingdom" & stringr::str_detect(RACE, "White", negate = TRUE) ~ 1,
        COUNTRY == "United States" & stringr::str_detect(RACE, "White") ~ 0,
        COUNTRY == "United States" & stringr::str_detect(RACE, "White", negate = TRUE) ~ 1
      ),
      WGT = ANNUAL_WEIGHT1_W1
    )

  df.original <- df.original %>%
    dplyr::mutate(
      CASE_MISSING_W2 = rowSums(across(contains("_W2"), \(x){
        is.na(x)
      })),
      CASE_MISSING_W2 = dplyr::case_when(CASE_MISSING_W2 > 50 ~ 1, .default = 0)
    )


  df.original
}
