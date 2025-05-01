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
    "GENDER_Y1",
    "MARITAL_STATUS_Y1",
    "EMPLOYMENT_Y1",
    "ATTEND_SVCS_Y1",
    "EDUCATION_3_Y1",
    "BORN_COUNTRY_Y1",
    "REL2_Y1",
    "PARENTS_12YRS_Y1",
    "SVCS_12YRS_Y1",
    "MOTHER_RELATN_Y1",
    "FATHER_RELATN_Y1",
    "OUTSIDER_Y1",
    "ABUSED_Y1",
    "HEALTH_GROWUP_Y1",
    "INCOME_12YRS_Y1",
    "REL1_Y1"
  )
}

#' Get variable codes other
#'
#' A simple function to get a list of variable codes used throughout the package
#'
#' @param what to return (options: "all", 'OUTCOME.VEC', 'LIST.COMPOSITES', 'DEMOGRAPHICS.CHILDHOOD.PRED.VEC')
#' @param appnd an optional character top append to end of all variable strings, does not apply to composites
#' @param ... argument not used
#' @returns a character string
#' @examples {
#'   #TODO
#' }
#' @export
get_variable_codes <- function(what = "all", appnd="", ...) {

  # DEFINE VECTOR OF Groups of Variables
  {
    # Demographics
    DEMOGRAPHIC.VARS <- c(
      "GENDER",
      'SELFID1',

      "MARITAL_STATUS",
      "EMPLOYMENT",
      "ATTEND_SVCS",
      "EDUCATION_3",
      "BORN_COUNTRY",
      "REL2",
      'REGION1',

      'EDUCATION',
      "INCOME"
    )
    RETROSPECTIVE.VARS <- c(
      'ABUSED',
      'FATHER_LOVED',
      'FATHER_RELATN',
      'HEALTH_GROWUP',
      'INCOME_12YRS',
      'MOTHER_LOVED',
      'MOTHER_RELATN',
      'OUTSIDER',
      'PARENTS_12YRS',
      'REL1',
      'SVCS_12YRS',
      'SVCS_FATHER',
      'SVCS_MOTHER'
    )

    VARS.Y1 <- paste0(
      c(
        'AFTER_DEATH',
        'APPROVE_GOVT',
        'ATTEND_SVCS',
        'BELIEVE_GOD',
        'BELONGING',
        'BODILY_PAIN',
        'CAPABLE',
        'CIGARETTES_BINARY',
        'CLOSE_TO',
        'COMFORT_REL',
        'CONNECTED_REL',
        'CONTENT',
        'CONTROL_WORRY',
        'CRITICAL',
        'DAYS_EXERCISE',
        'DEPRESSED',
        'DISCRIMINATED',
        'DONATED',
        'DRINKS',
        "EDUCATION_3",
        'EMPLOYMENT',
        'EXPECT_GOOD',
        'EXPENSES',
        'FEEL_ANXIOUS',
        'FORGIVE',
        'FREEDOM',
        'GIVE_UP',
        'GOD_PUNISH',
        'GRATEFUL',
        'GROUP_NOT_REL',
        'HAPPY',
        'HEALTH_PROB',
        'HELP_STRANGER',
        'HOPE_FUTURE',
        'INCOME_QUINTILE',
        'INCOME_DIFF',
        'INCOME_FEELINGS',
        'INTEREST',
        'LIFE_APPROACH',
        'LIFE_BALANCE',
        'LIFE_PURPOSE',
        'LIFE_SAT',
        'LONELY',
        'LOVED_BY_GOD',
        'MARITAL_STATUS_EVER_MARRIED',
        'MARITAL_STATUS_DIVORCED',
        'MENTAL_HEALTH',
        'NUM_CHILDREN',
        'NUM_HOUSEHOLD',
        'OBEY_LAW',
        'OUTSIDER',
        'OWN_RENT_HOME',
        'PEACE',
        'PEOPLE_HELP',
        'PHYSICAL_HLTH',
        'PRAY_MEDITATE',
        'PROMOTE_GOOD',
        'REL_EXPERIENC',
        'REL_IMPORTANT',
        'SACRED_TEXTS',
        'SAT_LIVE',
        'SAT_RELATNSHP',
        'SAY_IN_GOVT',
        'SHOW_LOVE',
        'SUFFERING',
        'TELL_BELIEFS',
        'THREAT_LIFE',
        'TRAITS1',
        'TRAITS2',
        'TRAITS3',
        'TRAITS4',
        'TRAITS5',
        'TRAITS6',
        'TRAITS7',
        'TRAITS8',
        'TRAITS9',
        'TRAITS10',
        'TRUST_PEOPLE',
        'URBAN_RURAL',
        'VOLUNTEERED',
        'WB_FIVEYRS',
        'WB_TODAY',
        'WORRY_SAFETY',
        'WORTHWHILE'
      ),
      "_Y1"
    )

    VARS.Y2 <- paste0(
      c(
        'AFTER_DEATH',
        'APPROVE_GOVT',
        'ATTEND_SVCS',
        'BELIEVE_GOD',
        'BELONGING',
        'BODILY_PAIN',
        'CAPABLE',
        'CIGARETTES_BINARY',
        'CLOSE_TO',
        'COMFORT_REL',
        'CONNECTED_REL',
        'CONTENT',
        'CONTROL_WORRY',
        'CRITICAL',
        'DAYS_EXERCISE',
        'DEPRESSED',
        'DISCRIMINATED',
        'DONATED',
        'DRINKS',
        'EDUCATION_3',
        'EMPLOYMENT',
        'EXPECT_GOOD',
        'EXPENSES',
        'FEEL_ANXIOUS',
        'FORGIVE',
        'FREEDOM',
        'GIVE_UP',
        'GOD_PUNISH',
        'GRATEFUL',
        'GROUP_NOT_REL',
        'HAPPY',
        'HEALTH_PROB',
        'HELP_STRANGER',
        'HOPE_FUTURE',
        'INCOME_FEELINGS',
        'INCOME_QUINTILE',
        'INTEREST',
        'LIFE_APPROACH',
        'LIFE_BALANCE',
        'LIFE_PURPOSE',
        'LIFE_SAT',
        'LONELY',
        'LOVED_BY_GOD',
        'MARITAL_STATUS_EVER_MARRIED',
        'MARITAL_STATUS_DIVORCED',
        'MENTAL_HEALTH',
        'NUM_CHILDREN',
        'OWN_RENT_HOME',
        'PEACE',
        'PEOPLE_HELP',
        'PHYSICAL_HLTH',
        'PRAY_MEDITATE',
        'PROMOTE_GOOD',
        'REL_EXPERIENC',
        'SACRED_TEXTS',
        'SAT_LIVE',
        'SAT_RELATNSHP',
        'SAY_IN_GOVT',
        'SHOW_LOVE',
        'SUFFERING',
        'TELL_BELIEFS',
        'THREAT_LIFE',
        'TRUST_PEOPLE',
        'VOLUNTEERED',
        'WB_FIVEYRS',
        'WB_TODAY',
        'WORRY_SAFETY',
        'WORTHWHILE'
      ),
      "_Y2"
    )

  }

  ## ========================================================================= ##
  ## ========================================================================= ##
  # Composites for wave 1 only data
  {
  LIST.OUTCOME.COMPOSITES0 <- list(
    COMPOSITE_DEPRESSION = c("DEPRESSED", "INTEREST"),
    COMPOSITE_ANXIETY = c("FEEL_ANXIOUS", "CONTROL_WORRY"),
    COMPOSITE_DEP_ANX_COMBO = c(
      "DEPRESSED",
      "INTEREST",
      "FEEL_ANXIOUS",
      "CONTROL_WORRY"
    ),
    COMPOSITE_FLOURISHING = c(
      "LIFE_SAT",
      "HAPPY",
      "PHYSICAL_HLTH",
      "MENTAL_HEALTH",
      "WORTHWHILE",
      "LIFE_PURPOSE",
      "PROMOTE_GOOD",
      "GIVE_UP",
      "CONTENT",
      "SAT_RELATNSHP"
    ),
    COMPOSITE_FLOURISHING_SECURE = c(
      "LIFE_SAT",
      "HAPPY",
      "PHYSICAL_HLTH",
      "MENTAL_HEALTH",
      "WORTHWHILE",
      "LIFE_PURPOSE",
      "PROMOTE_GOOD",
      "GIVE_UP",
      "CONTENT",
      "SAT_RELATNSHP",
      "EXPENSES",
      "WORRY_SAFETY"
    ),
    COMPOSITE_HAPPI_LIFE_SAT = c("HAPPY", "LIFE_SAT"),
    COMPOSITE_HEALTH = c("PHYSICAL_HLTH", "MENTAL_HEALTH"),
    COMPOSITE_MEANING_PURPOSE = c("LIFE_PURPOSE", "WORTHWHILE"),
    COMPOSITE_CHARACTER = c("PROMOTE_GOOD", "GIVE_UP"),
    COMPOSITE_SUBJECTIVE_SOC_CONN = c("CONTENT", "SAT_RELATNSHP"),
    COMPOSITE_FINL_MAT_WORRY = c("EXPENSES", "WORRY_SAFETY"),

    COMPOSITE_EXTRAVERSION = c("TRAITS1", "TRAITS6"),
    COMPOSITE_OPENNESS = c("TRAITS5", "TRAITS10"),
    COMPOSITE_AGREEABLENESS = c("TRAITS2", "TRAITS7"),
    COMPOSITE_CONSCIENTIOUSNESS = c("TRAITS3", "TRAITS8"),
    COMPOSITE_NEUROTICISM = c("TRAITS4", "TRAITS9")
  )
  LIST.COMPOSITE.COMBINE.METHOD0 <- list(
    COMPOSITE_DEPRESSION = "sum",
    COMPOSITE_ANXIETY = "sum",
    COMPOSITE_DEP_ANX_COMBO = "sum",
    COMPOSITE_FLOURISHING = "mean",
    COMPOSITE_FLOURISHING_SECURE = "mean",
    COMPOSITE_HAPPI_LIFE_SAT = "mean",
    COMPOSITE_HEALTH = "mean",
    COMPOSITE_MEANING_PURPOSE = "mean",
    COMPOSITE_CHARACTER = "mean",
    COMPOSITE_SUBJECTIVE_SOC_CONN = "mean",
    COMPOSITE_FINL_MAT_WORRY = "mean",

    COMPOSITE_EXTRAVERSION = "mean",
    COMPOSITE_OPENNESS = "mean",
    COMPOSITE_AGREEABLENESS = "mean",
    COMPOSITE_CONSCIENTIOUSNESS = "mean",
    COMPOSITE_NEUROTICISM = "mean"
  )
  COMPOSITE.VEC0 <- names(LIST.OUTCOME.COMPOSITES0)
  }

  # Compoisites for waves 1 & 2 data (wide format)
  {
  LIST.OUTCOME.COMPOSITES <- list(
    COMPOSITE_DEPRESSION_Y2 = c("DEPRESSED_Y2", "INTEREST_Y2"),
    COMPOSITE_ANXIETY_Y2 = c("FEEL_ANXIOUS_Y2", "CONTROL_WORRY_Y2"),
    COMPOSITE_DEP_ANX_COMBO_Y2 = c(
      "DEPRESSED_Y2",
      "INTEREST_Y2",
      "FEEL_ANXIOUS_Y2",
      "CONTROL_WORRY_Y2"
    ),
    COMPOSITE_FLOURISHING_Y2 = c(
      "LIFE_SAT_Y2",
      "HAPPY_Y2",
      "PHYSICAL_HLTH_Y2",
      "MENTAL_HEALTH_Y2",
      "WORTHWHILE_Y2",
      "LIFE_PURPOSE_Y2",
      "PROMOTE_GOOD_Y2",
      "GIVE_UP_Y2",
      "CONTENT_Y2",
      "SAT_RELATNSHP_Y2"
    ),
    COMPOSITE_FLOURISHING_SECURE_Y2 = c(
      "LIFE_SAT_Y2",
      "HAPPY_Y2",
      "PHYSICAL_HLTH_Y2",
      "MENTAL_HEALTH_Y2",
      "WORTHWHILE_Y2",
      "LIFE_PURPOSE_Y2",
      "PROMOTE_GOOD_Y2",
      "GIVE_UP_Y2",
      "CONTENT_Y2",
      "SAT_RELATNSHP_Y2",
      "EXPENSES_Y2",
      "WORRY_SAFETY_Y2"
    ),
    COMPOSITE_HAPPI_LIFE_SAT_Y2 = c("HAPPY_Y2", "LIFE_SAT_Y2"),
    COMPOSITE_HEALTH_Y2 = c("PHYSICAL_HLTH_Y2", "MENTAL_HEALTH_Y2"),
    COMPOSITE_MEANING_PURPOSE_Y2 = c("LIFE_PURPOSE_Y2", "WORTHWHILE_Y2"),
    COMPOSITE_CHARACTER_Y2 = c("PROMOTE_GOOD_Y2", "GIVE_UP_Y2"),
    COMPOSITE_SUBJECTIVE_SOC_CONN_Y2 = c("CONTENT_Y2", "SAT_RELATNSHP_Y2"),
    COMPOSITE_FINL_MAT_WORRY_Y2 = c("EXPENSES_Y2", "WORRY_SAFETY_Y2"),

    COMPOSITE_DEPRESSION_Y1 = c("DEPRESSED_Y1", "INTEREST_Y1"),
    COMPOSITE_ANXIETY_Y1 = c("FEEL_ANXIOUS_Y1", "CONTROL_WORRY_Y1"),
    COMPOSITE_DEP_ANX_COMBO_Y1 = c(
      "DEPRESSED_Y1",
      "INTEREST_Y1",
      "FEEL_ANXIOUS_Y1",
      "CONTROL_WORRY_Y1"
    ),
    COMPOSITE_FLOURISHING_Y1 = c(
      "LIFE_SAT_Y1",
      "HAPPY_Y1",
      "PHYSICAL_HLTH_Y1",
      "MENTAL_HEALTH_Y1",
      "WORTHWHILE_Y1",
      "LIFE_PURPOSE_Y1",
      "PROMOTE_GOOD_Y1",
      "GIVE_UP_Y1",
      "CONTENT_Y1",
      "SAT_RELATNSHP_Y1"
    ),
    COMPOSITE_FLOURISHING_SECURE_Y1 = c(
      "LIFE_SAT_Y1",
      "HAPPY_Y1",
      "PHYSICAL_HLTH_Y1",
      "MENTAL_HEALTH_Y1",
      "WORTHWHILE_Y1",
      "LIFE_PURPOSE_Y1",
      "PROMOTE_GOOD_Y1",
      "GIVE_UP_Y1",
      "CONTENT_Y1",
      "SAT_RELATNSHP_Y1",
      "EXPENSES_Y1",
      "WORRY_SAFETY_Y1"
    ),
    COMPOSITE_HAPPI_LIFE_SAT_Y1 = c("HAPPY_Y1", "LIFE_SAT_Y1"),
    COMPOSITE_HEALTH_Y1 = c("PHYSICAL_HLTH_Y1", "MENTAL_HEALTH_Y1"),
    COMPOSITE_MEANING_PURPOSE_Y1 = c("LIFE_PURPOSE_Y1", "WORTHWHILE_Y1"),
    COMPOSITE_CHARACTER_Y1 = c("PROMOTE_GOOD_Y1", "GIVE_UP_Y1"),
    COMPOSITE_SUBJECTIVE_SOC_CONN_Y1 = c("CONTENT_Y1", "SAT_RELATNSHP_Y1"),
    COMPOSITE_FINL_MAT_WORRY_Y1 = c("EXPENSES_Y1", "WORRY_SAFETY_Y1"),

    COMPOSITE_EXTRAVERSION_Y1 = c("TRAITS1_Y1", "TRAITS6_Y1"),
    COMPOSITE_OPENNESS_Y1 = c("TRAITS5_Y1", "TRAITS10_Y1"),
    COMPOSITE_AGREEABLENESS_Y1 = c("TRAITS2_Y1", "TRAITS7_Y1"),
    COMPOSITE_CONSCIENTIOUSNESS_Y1 = c("TRAITS3_Y1", "TRAITS8_Y1"),
    COMPOSITE_NEUROTICISM_Y1 = c("TRAITS4_Y1", "TRAITS9_Y1")
  )
  LIST.COMPOSITE.COMBINE.METHOD <- list(
    COMPOSITE_DEPRESSION_Y2 = "sum",
    COMPOSITE_ANXIETY_Y2 = "sum",
    COMPOSITE_DEP_ANX_COMBO_Y2 = "sum",
    COMPOSITE_FLOURISHING_Y2 = "mean",
    COMPOSITE_FLOURISHING_SECURE_Y2 = "mean",
    COMPOSITE_HAPPI_LIFE_SAT_Y2 = "mean",
    COMPOSITE_HEALTH_Y2 = "mean",
    COMPOSITE_MEANING_PURPOSE_Y2 = "mean",
    COMPOSITE_CHARACTER_Y2 = "mean",
    COMPOSITE_SUBJECTIVE_SOC_CONN_Y2 = "mean",
    COMPOSITE_FINL_MAT_WORRY_Y2 = "mean",

    COMPOSITE_DEPRESSION_Y1 = "sum",
    COMPOSITE_ANXIETY_Y1 = "sum",
    COMPOSITE_DEP_ANX_COMBO_Y1 = "sum",
    COMPOSITE_FLOURISHING_Y1 = "mean",
    COMPOSITE_FLOURISHING_SECURE_Y1 = "mean",
    COMPOSITE_HAPPI_LIFE_SAT_Y1 = "mean",
    COMPOSITE_HEALTH_Y1 = "mean",
    COMPOSITE_MEANING_PURPOSE_Y1 = "mean",
    COMPOSITE_CHARACTER_Y1 = "mean",
    COMPOSITE_SUBJECTIVE_SOC_CONN_Y1 = "mean",
    COMPOSITE_FINL_MAT_WORRY_Y1 = "mean",

    COMPOSITE_EXTRAVERSION_Y1 = "mean",
    COMPOSITE_OPENNESS_Y1 = "mean",
    COMPOSITE_AGREEABLENESS_Y1 = "mean",
    COMPOSITE_CONSCIENTIOUSNESS_Y1 = "mean",
    COMPOSITE_NEUROTICISM_Y1 = "mean"
  )
  COMPOSITE.VEC <- names(LIST.OUTCOME.COMPOSITES)
  }

  LIST.COMPOSITES <- list(
    LIST.OUTCOME.COMPOSITES = LIST.OUTCOME.COMPOSITES,
    COMPOSITE.VEC = COMPOSITE.VEC,
    LIST.COMPOSITE.COMBINE.METHOD = LIST.COMPOSITE.COMBINE.METHOD,

    LIST.OUTCOME.COMPOSITES0 = LIST.OUTCOME.COMPOSITES0,
    COMPOSITE.VEC0 = COMPOSITE.VEC0,
    LIST.COMPOSITE.COMBINE.METHOD0 = LIST.COMPOSITE.COMBINE.METHOD0
  )

  out <- list(
    LIST.COMPOSITES = LIST.COMPOSITES,
    VARS.Y1 = c(VARS.Y1, COMPOSITE.VEC[str_detect(COMPOSITE.VEC, "_Y1")]),
    VARS.Y2 = c(VARS.Y2, COMPOSITE.VEC[str_detect(COMPOSITE.VEC, "_Y2")]),
    VARS0 = str_remove(c(VARS.Y1), "_Y1"),
    OUTCOME.VEC = str_remove(c(VARS.Y1, COMPOSITE.VEC[str_detect(COMPOSITE.VEC, "_Y1")]), "_Y1"),
    GENDER.RACE = paste0(DEMOGRAPHIC.VARS[c(1:2)], appnd),
    DEMOGRAPHIC.VARS = paste0(DEMOGRAPHIC.VARS[-c(1:2)], appnd),
    RETROSPECTIVE.VARS = paste0(RETROSPECTIVE.VARS, appnd)
  )
  what = ifelse(what == "all", c(names(out)), what)
  out[[what]]
}
