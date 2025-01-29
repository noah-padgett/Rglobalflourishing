# Demographic + childhood predictor variables

#' Get variable codes for demographic and childhood variables
#'
#' A simple function to get a list of demographic or childhood predictor variables.
#' @param ... argument not used
#' @returns a character string
#' @examples {
#'   get_demo_childhood_variables()
#' }
#' @export
get_demo_childhood_variables <- function(...) {
  c(
    "GENDER_W1",
    "MARITAL_STATUS_W1",
    "EMPLOYMENT_W1",
    "ATTEND_SVCS_W1",
    "EDUCATION_3_W1",
    "BORN_COUNTRY_W1",
    "REL2_W1",
    "PARENTS_12YRS_W1",
    "SVCS_12YRS_W1",
    "MOTHER_RELATN_W1",
    "FATHER_RELATN_W1",
    "OUTSIDER_W1",
    "ABUSED_W1",
    "HEALTH_GROWUP_W1",
    "INCOME_12YRS_W1",
    "REL1_W1"
  )
}

#' Get variable codes other
#'
#' A simple function to get a list of variable codes used throughout the package
#'
#' @param ... argument not used
#' @returns a character string
#' @examples {
#'   get_variable_codes()
#' }
#' @export
get_variable_codes <- function(...){

  DEMOGRAPHICS.CHILDHOOD.PRED.VEC <- c(
    "GENDER_W1",
    "MARITAL_STATUS_W1",
    "EMPLOYMENT_W1",
    "ATTEND_SVCS_W1",
    "EDUCATION_3_W1",
    "BORN_COUNTRY_W1",
    "REL2_W1",
    "PARENTS_12YRS_W1",
    "SVCS_12YRS_W1",
    "MOTHER_RELATN_W1",
    "FATHER_RELATN_W1",
    "OUTSIDER_W1",
    "ABUSED_W1",
    "HEALTH_GROWUP_W1",
    "INCOME_12YRS_W1",
    "REL1_W1"
  )

  # DEFINE VECTOR OF OUTCOMES
  {
    OUTCOME.VEC <- c(
      "HAPPY",
      "LIFE_SAT",
      "WB_TODAY",
      "WB_FIVEYRS",
      "EXPECT_GOOD",
      "FREEDOM",
      "PEACE",
      "LIFE_BALANCE",
      "CAPABLE",
      "WORTHWHILE",
      "LIFE_PURPOSE",
      "MENTAL_HEALTH",
      "CONTENT",
      "SAT_RELATNSHP",
      "PEOPLE_HELP",
      "CLOSE_TO",
      "APPROVE_GOVT",
      "SAY_IN_GOVT",
      "BELONGING",
      "SAT_LIVE",
      "TRUST_PEOPLE",
      "GROUP_NOT_REL",
      "THREAT_LIFE",
      "DEPRESSED",
      "INTEREST",
      "FEEL_ANXIOUS",
      "CONTROL_WORRY",
      "SUFFERING",
      "LONELY",
      "DISCRIMINATED",
      "PROMOTE_GOOD",
      "GIVE_UP",
      "HOPE_FUTURE",
      "GRATEFUL",
      "SHOW_LOVE",
      "FORGIVE",
      "DONATED",
      "HELP_STRANGER",
      "VOLUNTEERED",
      "PHYSICAL_HLTH",
      "HEALTH_PROB",
      "BODILY_PAIN",
      "CIGARETTES",
      "DRINKS",
      "DAYS_EXERCISE",
      "EXPENSES",
      "WORRY_SAFETY",
      "EDUCATION_3",
      "EMPLOYMENT",
      "INCOME_FEELINGS",
      "OWN_RENT_HOME",

      "CONNECTED_REL",
      "ATTEND_SVCS",
      "AFTER_DEATH",
      "REL_EXPERIENC",
      "SACRED_TEXTS",
      "PRAY_MEDITATE",
      "BELIEVE_GOD",
      "LIFE_APPROACH",
      "COMFORT_REL",
      "LOVED_BY_GOD",
      "GOD_PUNISH",
      "CRITICAL",
      "TELL_BELIEFS",
      "TRAITS1",
      "TRAITS6",
      "TRAITS5",
      "TRAITS10",
      "TRAITS2",
      "TRAITS7",
      "TRAITS3",
      "TRAITS8",
      "TRAITS4",
      "TRAITS9",

      # marital status is manually replicated...
      # MARITAL_STATUS_W2_2,
      # MARITAL_STATUS_W2_4,
      "MARITAL_STATUS_EVER_MARRIED",
      "NUM_CHILDREN"
    )
    OUTCOME.VEC <- c(paste0(OUTCOME.VEC, "_W1"), paste0(OUTCOME.VEC, "_W2"))
    # OUTCOME.VEC <- paste0(OUTCOME.VEC, "_W2")
  }

  ## ========================================================================= ##
  ## ========================================================================= ##
  # IF any, create list of composites
  LIST.OUTCOME.COMPOSITES <- list(
    c("DEPRESSED_W2", "INTEREST_W2"),
    c("FEEL_ANXIOUS_W2", "CONTROL_WORRY_W2"),
    c(
      "DEPRESSED_W2",
      "INTEREST_W2",
      "FEEL_ANXIOUS_W2",
      "CONTROL_WORRY_W2"
    ),
    c("TRAITS1_W2", "TRAITS6_W2"),
    c("TRAITS5_W2", "TRAITS10_W2"),
    c("TRAITS2_W2", "TRAITS7_W2"),
    c("TRAITS3_W2", "TRAITS8_W2"),
    c("TRAITS4_W2", "TRAITS9_W2"),
    c(
      "LIFE_SAT_W2",
      "HAPPY_W2",
      "PHYSICAL_HLTH_W2",
      "MENTAL_HEALTH_W2",
      "WORTHWHILE_W2",
      "LIFE_PURPOSE_W2",
      "PROMOTE_GOOD_W2",
      "GIVE_UP_W2",
      "CONTENT_W2",
      "SAT_RELATNSHP_W2"
    ),
    c(
      "LIFE_SAT_W2",
      "HAPPY_W2",
      "PHYSICAL_HLTH_W2",
      "MENTAL_HEALTH_W2",
      "WORTHWHILE_W2",
      "LIFE_PURPOSE_W2",
      "PROMOTE_GOOD_W2",
      "GIVE_UP_W2",
      "CONTENT_W2",
      "SAT_RELATNSHP_W2",
      "EXPENSES_W2",
      "WORRY_SAFETY_W2"
    ),
    c("HAPPY_W2", "LIFE_SAT_W2"),
    c("PHYSICAL_HLTH_W2", "MENTAL_HEALTH_W2"),
    c("LIFE_PURPOSE_W2", "WORTHWHILE_W2"),
    c("PROMOTE_GOOD_W2", "GIVE_UP_W2"),
    c("CONTENT_W2", "SAT_RELATNSHP_W2"),
    c("EXPENSES_W2", "WORRY_SAFETY_W2"),
    
    c("DEPRESSED_W1", "INTEREST_W1"),
    c("FEEL_ANXIOUS_W1", "CONTROL_WORRY_W1"),
    c(
      "DEPRESSED_W1",
      "INTEREST_W1",
      "FEEL_ANXIOUS_W1",
      "CONTROL_WORRY_W1"
    ),
    c("TRAITS1_W1", "TRAITS6_W1"),
    c("TRAITS5_W1", "TRAITS10_W1"),
    c("TRAITS2_W1", "TRAITS7_W1"),
    c("TRAITS3_W1", "TRAITS8_W1"),
    c("TRAITS4_W1", "TRAITS9_W1"),
    c(
      "LIFE_SAT_W1",
      "HAPPY_W1",
      "PHYSICAL_HLTH_W1",
      "MENTAL_HEALTH_W1",
      "WORTHWHILE_W1",
      "LIFE_PURPOSE_W1",
      "PROMOTE_GOOD_W1",
      "GIVE_UP_W1",
      "CONTENT_W1",
      "SAT_RELATNSHP_W1"
    ),
    c(
      "LIFE_SAT_W1",
      "HAPPY_W1",
      "PHYSICAL_HLTH_W1",
      "MENTAL_HEALTH_W1",
      "WORTHWHILE_W1",
      "LIFE_PURPOSE_W1",
      "PROMOTE_GOOD_W1",
      "GIVE_UP_W1",
      "CONTENT_W1",
      "SAT_RELATNSHP_W1",
      "EXPENSES_W1",
      "WORRY_SAFETY_W1"
    ),
    c("HAPPY_W1", "LIFE_SAT_W1"),
    c("PHYSICAL_HLTH_W1", "MENTAL_HEALTH_W1"),
    c("LIFE_PURPOSE_W1", "WORTHWHILE_W1"),
    c("PROMOTE_GOOD_W1", "GIVE_UP_W1"),
    c("CONTENT_W1", "SAT_RELATNSHP_W1"),
    c("EXPENSES_W1", "WORRY_SAFETY_W1")
  )
  LIST.COMPOSITE.COMBINE.METHOD <- list(
    "sum",
    "sum",
    "sum",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    
    "sum",
    "sum",
    "sum",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean",
    "mean"
  )
  COMPOSITE.VEC <- c(
    "COMPOSITE_DEPRESSION",
    "COMPOSITE_ANXIETY",
    "COMPOSITE_DEP_ANX_COMBO",
    "COMPOSITE_EXTRAVERSION",
    "COMPOSITE_OPENNESS",
    "COMPOSITE_AGREEABLENESS",
    "COMPOSITE_CONSCIENTIOUSNESS",
    "COMPOSITE_NEUROTICISM",
    "COMPOSITE_FLOURISHING",
    "COMPOSITE_FLOURISHING_SECURE",
    "COMPOSITE_HAPPI_LIFE_SAT",
    "COMPOSITE_HEALTH",
    "COMPOSITE_MEANING_PURPOSE",
    "COMPOSITE_CHARACTER",
    "COMPOSITE_SUBJECTIVE_SOC_CONN",
    "COMPOSITE_FINL_MAT_WORRY"
  )
  
  COMPOSITE.VEC <- c(paste0(COMPOSITE.VEC, "_W2"),paste0(COMPOSITE.VEC, "_W1"))
  names(LIST.OUTCOME.COMPOSITES) <- COMPOSITE.VEC

  LIST.COMPOSITES <- list(
    LIST.OUTCOME.COMPOSITES = LIST.OUTCOME.COMPOSITES,
    COMPOSITE.VEC = COMPOSITE.VEC,
    LIST.COMPOSITE.COMBINE.METHOD = LIST.COMPOSITE.COMBINE.METHOD
  )
  
  OUTCOME.VEC <- c(OUTCOME.VEC, COMPOSITE.VEC)

  out <- list(
    LIST.COMPOSITES = LIST.COMPOSITES,
    OUTCOME.VEC = OUTCOME.VEC,
    DEMOGRAPHICS.CHILDHOOD.PRED.VEC = DEMOGRAPHICS.CHILDHOOD.PRED.VEC
  )
  out
}
