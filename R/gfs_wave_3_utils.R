get_defaults_w3 <- function(x){

  control.list = list(
    dir.meta = "results-primary",
    file.primary = "0_meta_analyzed_results_primary.rds",
    p.bonferroni = NULL,
    baseline.pred = NULL,
    tbl.row.vec = NULL,
    mylabels = NULL,
    res.dir = "results",
    wgt = as.name("WGT0"), ##
    wgt1 = as.name("ANNUAL_WEIGHT_R3"),
    wgt2 = as.name("AVG_SAMP_ATTR_WGT2"),
    wgt3 = as.name("AVG_SAMP_ATTR_WGT3"),
    psu = as.name("PSU"),
    strata = as.name("STRATA"),
    ci.bonferroni = FALSE,
    tb.footnote = NULL,
    study = "exposurewide", filetype = "main"
  )
  cvnames <- names(control)
  for(cv in cvnames){
    control.list[[cv]] <- control[[cv]]
  }
  ## ---- update elements in control.list depending on what is supplied ----
  ## VECTOR OF BASELINE VARIABLES
  if(is.null(control.list[['baseline.pred']])){
    control.list[['baseline.pred']] = c(
        "AGE_GRP_Y1",
        "GENDER",
        "EDUCATION_3_Y1",
        "EMPLOYMENT_Y1",
        "MARITAL_STATUS_Y1",
        "ATTEND_SVCS_Y1",
        "BORN_COUNTRY_Y1",
        "PARENTS_12YRS_Y1",
        "SVCS_12YRS_Y1",
        "MOTHER_RELATN_Y1",
        "FATHER_RELATN_Y1",
        "OUTSIDER_Y1",
        "ABUSED_Y1",
        "HEALTH_GROWUP_Y1",
        "INCOME_12YRS_Y1",
        "REL2_Y1",
        "RACE_PLURALITY_Y1"
      )
  }
  ## DEFINE VECTOR OF OUTCOMES
  if(is.null(control.list[['tbl_row_vec']])){
    control.list[['tbl_row_vec']] <- get_tbl_row_vec(
      study = control.list[['study']],
      filetype = control.list[['filetype']]
    )[['tbl.row.vec']]
  }
  ## DEFINE VECTOR OF LABELS
    if(is.null(control.list[['mylabels']])){
      control.list[['mylabels']] <- get_tbl_row_vec(
        study = control.list[['study']],
        filetype = control.list[['filetype']]
      )[['row.labels']]
    }
  ## Bonferroni correction p-value
  if(is.null(control.list[['p.bonferroni']])){
    control.list[['p.bonferroni']] = 0.05/length(control.list[['tbl_row_vec']][str_detect(control.list[['tbl_row_vec']], "blank",negate=TRUE)])
  }

 control.list
}

#' utility function to get the prespecified set of variables used in the rows of the tables.
get_tbl_row_vec <- function(study="exposurewide", filetype = "main"){
  all.labels <- c(
    "Human Flourishing",
    "Psychological Well-Being",
    "Psychological Distress",
    "Social Well-Being",
    "Social Participation",
    "Social Distress",
    "Character & Prosocial Behavior",
    "Physical Health & Health Behavior",
    "Socioeconomic Outcomes"
  )
  out.labels <- all.labels[-1]

  out <- NA
  out <- c(
      # Flourishing
      #'blank',
      #"COMPOSITE_FLOURISHING_SECURE", "COMPOSITE_FLOURISHING", "COMPOSITE_HAPPI_LIFE_SAT", "COMPOSITE_HEALTH", "COMPOSITE_MEANING_PURPOSE", "COMPOSITE_CHARACTER", "COMPOSITE_SUBJECTIVE_SOC_CONN", "COMPOSITE_FINL_MAT_WORRY",
      # Psychological well-being
      'blank',
      'HAPPY', 'LIFE_SAT', 'WB_TODAY','WB_FIVEYRS', 'EXPECT_GOOD', 'FREEDOM', 'PEACE', 'LIFE_BALANCE', 'CAPABLE', 'WORTHWHILE', 'LIFE_PURPOSE', 'MENTAL_HEALTH',
      # Psychological Distress
      'blank',
      'THREAT_LIFE', 'COMPOSITE_DEPRESSION', 'COMPOSITE_ANXIETY',  'SUFFERING',
      # Social Well-Being
      'blank',
      'CONTENT', 'SAT_RELATNSHP', 'PEOPLE_HELP', 'CLOSE_TO', 'APPROVE_GOVT', 'SAY_IN_GOVT', 'BELONGING', 'SAT_LIVE', 'TRUST_PEOPLE',
      # Social Participation
      'blank',
      'MARITAL_STATUS_EVER_MARRIED', 'MARITAL_STATUS_DIVORCED',  'NUM_CHILDREN', 'GROUP_NOT_REL', 'ATTEND_SVCS',
      # Social Distress
      'blank',
      'LONELY', 'DISCRIMINATED',
      # Character & Prosocial Behavior
      'blank',
      'PROMOTE_GOOD', 'GIVE_UP', 'HOPE_FUTURE', 'GRATEFUL', 'SHOW_LOVE', 'FORGIVE', 'DONATED', 'HELP_STRANGER', 'VOLUNTEERED',
      # Physical Health & Health Behavior
      'blank',
     'PHYSICAL_HLTH', 'HEALTH_PROB', 'BODILY_PAIN', 'CIGARETTES_BINARY', 'DRINKS', 'DAYS_EXERCISE',
      # Socioeconomic Outcomes
      'blank',
      'EXPENSES', 'WORRY_SAFETY','EDUCATION_3', 'EMPLOYMENT', 'INCOME_FEELINGS', 'OWN_RENT_HOME', 'INCOME_QUINTILE'
    )
    if(filetype == "supp"){
      out.labels <- all.labels
      out <- c(
        # Flourishing domains
        'blank',
        "COMPOSITE_HAPPI_LIFE_SAT", "COMPOSITE_HEALTH", "COMPOSITE_MEANING_PURPOSE", "COMPOSITE_CHARACTER", "COMPOSITE_SUBJECTIVE_SOC_CONN", "COMPOSITE_FINL_MAT_WORRY",
        # Psychological well-being
        'blank',
        'HAPPY', 'LIFE_SAT', 'WB_TODAY','WB_FIVEYRS', 'EXPECT_GOOD', 'FREEDOM', 'PEACE', 'LIFE_BALANCE', 'CAPABLE', 'WORTHWHILE', 'LIFE_PURPOSE', 'MENTAL_HEALTH',
        # Psychological Distress
        'blank',
        'THREAT_LIFE', 'COMPOSITE_DEPRESSION', 'DEPRESSED', 'INTEREST', 'COMPOSITE_ANXIETY', 'FEEL_ANXIOUS', 'CONTROL_WORRY', 'SUFFERING',
        # Social Well-Being
        'blank',
        'CONTENT', 'SAT_RELATNSHP', 'PEOPLE_HELP', 'CLOSE_TO', 'APPROVE_GOVT', 'SAY_IN_GOVT', 'BELONGING', 'SAT_LIVE', 'TRUST_PEOPLE',
        # Social Participation
        'blank',
        'MARITAL_STATUS_EVER_MARRIED', 'MARITAL_STATUS_DIVORCED',  'NUM_CHILDREN', 'GROUP_NOT_REL', 'ATTEND_SVCS',
        # Social Distress
        'blank',
        'LONELY', 'DISCRIMINATED',
        # Character & Prosocial Behavior
        'blank',
        'PROMOTE_GOOD', 'GIVE_UP', 'HOPE_FUTURE', 'GRATEFUL', 'SHOW_LOVE', 'FORGIVE', 'DONATED', 'HELP_STRANGER', 'VOLUNTEERED',
        # Physical Health & Health Behavior
        'blank',
        'PHYSICAL_HLTH', 'HEALTH_PROB', 'BODILY_PAIN', 'CIGARETTES_BINARY', 'DRINKS', 'DAYS_EXERCISE',
        # Socioeconomic Outcomes
        'blank',
        'EXPENSES', 'WORRY_SAFETY','EDUCATION_3', 'EMPLOYMENT', 'INCOME_FEELINGS', 'OWN_RENT_HOME', 'INCOME_QUINTILE'
      )
    }


  if(str_detect(stringr::str_to_lower(study),"exposure")){
    out <- c(paste0(out, "_Y2"))
  }
  if(str_detect(stringr::str_to_lower(study),"outcome")){
    out.labels <- all.labels
    if(filetype == "main"){
      out <- c(
        'blank', "COMPOSITE_FLOURISHING_SECURE", "COMPOSITE_FLOURISHING",
        out
      )
    }
    if(filetype == "supp"){
      out <- c(
        'blank', "COMPOSITE_FLOURISHING_SECURE", "COMPOSITE_FLOURISHING",
        out[-1]
      )
    }
    out <- c(paste0(out, "_Y3"))
  }

  list(tbl.row.vec = out, row.labels = out.labels)
}

#'
