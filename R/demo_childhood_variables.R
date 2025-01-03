# Demographic + childhood predictor variables

get_demo_childhood_variables <- function() {
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

get_variable_codes <- function(){

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
      "HAPPY_W",
      "LIFE_SAT_W",
      "WB_TODAY_W",
      "WB_FIVEYRS_W",
      "EXPECT_GOOD_W",
      "FREEDOM_W",
      "PEACE_W",
      "LIFE_BALANCE_W",
      "CAPABLE_W",
      "WORTHWHILE_W",
      "LIFE_PURPOSE_W",
      "MENTAL_HEALTH_W",
      "CONTENT_W",
      "SAT_RELATNSHP_W",
      "PEOPLE_HELP_W",
      "CLOSE_TO_W",
      "APPROVE_GOVT_W",
      "SAY_IN_GOVT_W",
      "BELONGING_W",
      "SAT_LIVE_W",
      "TRUST_PEOPLE_W",
      "GROUP_NOT_REL_W",
      "THREAT_LIFE_W",
      "DEPRESSED_W",
      "INTEREST_W",
      "FEEL_ANXIOUS_W",
      "CONTROL_WORRY_W",
      "SUFFERING_W",
      "LONELY_W",
      "DISCRIMINATED_W",
      "PROMOTE_GOOD_W",
      "GIVE_UP_W",
      "HOPE_FUTURE_W",
      "GRATEFUL_W",
      "SHOW_LOVE_W",
      "FORGIVE_W",
      "DONATED_W",
      "HELP_STRANGER_W",
      "VOLUNTEERED_W",
      "PHYSICAL_HLTH_W",
      "HEALTH_PROB_W",
      "BODILY_PAIN_W",
      "CIGARETTES_W",
      "DRINKS_W",
      "DAYS_EXERCISE_W",
      "EXPENSES_W",
      "WORRY_SAFETY_W",
      "EDUCATION_3_W",
      "EMPLOYMENT_W",
      "INCOME_FEELINGS_W",
      "OWN_RENT_HOME_W",
      #' INCOME',


      "CONNECTED_REL_W",
      "ATTEND_SVCS_W",
      "AFTER_DEATH_W",
      "REL_EXPERIENC_W",
      "SACRED_TEXTS_W",
      "PRAY_MEDITATE_W",
      "BELIEVE_GOD_W",
      "LIFE_APPROACH_W",
      "COMFORT_REL_W",
      "LOVED_BY_GOD_W",
      "GOD_PUNISH_W",
      "CRITICAL_W",
      "TELL_BELIEFS_W",
      "TRAITS1_W",
      "TRAITS6_W",
      "TRAITS5_W",
      "TRAITS10_W",
      "TRAITS2_W",
      "TRAITS7_W",
      "TRAITS3_W",
      "TRAITS8_W",
      "TRAITS4_W",
      "TRAITS9_W",

      # marital status is manually replicated...
      #' MARITAL_STATUS_W2_2',
      #' MARITAL_STATUS_W2_4',
      "MARITAL_STATUS_EVER_MARRIED_W",
      "NUM_CHILDREN_W"
    )
    OUTCOME.VEC <- c(paste0(OUTCOME.VEC, "1"), paste0(OUTCOME.VEC, "2"))
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
    c("EXPENSES_W2", "WORRY_SAFETY_W2")
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
  COMPOSITE.VEC <- paste0(COMPOSITE.VEC, "_W2")
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
