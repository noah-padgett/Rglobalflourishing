#' Get outcome name (better)
#'
#' A relatively simple switch function to get a better name for each variable
#'
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a character string
#' @examples {
#'   get_outcome_better_name("HAPPY_W1")
#' }
#' @export
get_outcome_better_name <- function(var) {
  switch(var,
    ID = "(No Label) (ID)",
    COUNTRY_W1 = "Country of Respondent (COUNTRY_W1)",
    WAVE_W1 = "WAVE (WAVE_W1)",
    MODE_RECRUIT_W1 = "Recruitment Survey Mode (MODE_RECRUIT_W1)",
    MODE_ANNUAL_W1 = "Annual Survey Mode (MODE_ANNUAL_W1)",
    RECRUIT_TYPE_W1 = "Recruit Type (RECRUIT_TYPE_W1)",
    DOI_RECRUIT_W1 = "End Date of Interview - Recruit Survey (DOI_RECRUIT_W1)",
    DOI_ANNUAL_W1 = "End Date of Interview - Annual Survey (DOI_ANNUAL_W1)",
    ABUSED_W1 = "Physically or Sexually Abused When Growing Up (ABUSED_W1)",
    AFTER_DEATH_W1 = "Believe in Life After Death (AFTER_DEATH_W1)",
    AGE_W1 = "Age of Respondent (AGE_W1)",
    APPROVE_GOVT_W1 = "Job Performance of National Government (APPROVE_GOVT_W1)",
    ATTEND_SVCS_W1 = "How Often You Attend Religious Services (ATTEND_SVCS_W1)",
    BELIEVE_GOD_W1 = "Believe in One God More Than One God an Impersonal Spiritual Force or None of These (BELIEVE_GOD_W1)",
    BELONGING_W1 = "Sense of Belonging in Your Country (BELONGING_W1)",
    BODILY_PAIN_W1 = "Bodily Pain in Past 4 Weeks (BODILY_PAIN_W1)",
    BORN_COUNTRY_W1 = "Born in This Country (BORN_COUNTRY_W1)",
    CAPABLE_W1 = "Feel Very Capable in Most Things You Do in Life (CAPABLE_W1)",
    CIGARETTES_W1 = "Number of Cigarettes Smoked Each Day (CIGARETTES_W1)",
    CLOSE_TO_W1 = "Know One Special Person You Feel Very Close To (CLOSE_TO_W1)",
    CNTRY_REL_BUD_W1 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_BUD_W1)",
    CNTRY_REL_CHI_W1 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_CHI_W1)",
    CNTRY_REL_CHR_W1 = "The Teachings of Christianity Are Very Important in Your Life (Christianity is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_CHR_W1)",
    CNTRY_REL_HIN_W1 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_HIN_W1)",
    CNTRY_REL_ISL_W1 = "The Teachings of Islam Are Very Important in Your Life (Islam is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_ISL_W1)",
    CNTRY_REL_JUD_W1 = "The Teachings of Judaism Are Very Important in Your Life (Judaism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_JUD_W1)",
    CNTRY_REL_SHI_W1 = "The Teachings of Shinto Are Very Important in Your Life (Shinto is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_SHI_W1)",
    COMFORT_REL_W1 = "Find Strength or Comfort in Your Religion or Spirituality (COMFORT_REL_W1)",
    CONNECTED_REL_W1 = "Feel Connected to a Religion or Form of Spirituality (CONNECTED_REL_W1)",
    CONTENT_W1 = "Relational Contentment (CONTENT_W1)",
    CONTROL_WORRY_W1 = "Been Bothered in Last Two Weeks By: Not Being Able to Stop or Control Worrying (CONTROL_WORRY_W1)",
    COVID_DEATH_W1 = "Family Member or Close Friend Died From Coronavirus (COVID_DEATH_W1)",
    CRITICAL_W1 = "People in Your Religious Community are Critical of You or Your Lifestyle (CRITICAL_W1)",
    DAYS_EXERCISE_W1 = "Number of Days You Exercised in the Past Week (DAYS_EXERCISE_W1)",
    DEPRESSED_W1 = "Been Bothered in Last Two Weeks By: Feeling Down Depressed or Hopeless (DEPRESSED_W1)",
    DISCRIMINATED_W1 = "Feel Discriminated Against Because of Any Group You Are Part of (DISCRIMINATED_W1)",
    DONATED_W1 = "Charitable Giving (DONATED_W1)",
    DRINKS_W1 = "Number of Alcoholic Drinks You Drank in Past Seven Days (DRINKS_W1)",
    EDUCATION_W1 = "Highest Completed Level of Education (EDUCATION_W1)",
    EDUCATION_3_W1 = "Highest Completed Level of Education (Three Levels) (EDUCATION_3_W1)",
    EMPLOYMENT_W1 = "Employment Status (EMPLOYMENT_W1)",
    EXPECT_GOOD_W1 = "You Expect More Good Things to Happen to You Than Bad (EXPECT_GOOD_W1)",
    EXPENSES_W1 = "Worry About Being Able to Meet Normal Monthly Living Expenses (EXPENSES_W1)",
    FATHER_LOVED_W1 = "Felt Loved by Your Father When Growing Up (FATHER_LOVED_W1)",
    FATHER_RELATN_W1 = "Relationship With Your Father When Growing Up (FATHER_RELATN_W1)",
    FEEL_ANXIOUS_W1 = "Been Bothered in Last Two Weeks By: Feeling Nervous Anxious or on Edge (FEEL_ANXIOUS_W1)",
    FORGIVE_W1 = "How Often You Have Forgiven Those Who Have Hurt You (FORGIVE_W1)",
    FREEDOM_W1 = "You Have Freedom in Your Life to Pursue the Things Most Important to You (FREEDOM_W1)",
    GENDER_W1 = "Gender of Respondent (GENDER_W1)",
    GIVE_UP_W1 = "You Are Always Able to Give Up Some Happiness Now For Greater Happiness Later (GIVE_UP_W1)",
    GOD_PUNISH_W1 = "You feel God a God or a Spiritual Force Is Punishing You (GOD_PUNISH_W1)",
    GRATEFUL_W1 = "If You Listed Everything You Felt Grateful For It Would Be a Long List (GRATEFUL_W1)",
    GROUP_NOT_REL_W1 = "Participate in Groups That Are Not Religious (GROUP_NOT_REL_W1)",
    HAPPY_W1 = "How Happy You Usually Feel (HAPPY_W1)",
    HEALTH_GROWUP_W1 = "Your Health When Growing Up (HEALTH_GROWUP_W1)",
    HEALTH_PROB_W1 = "Health Problems Prevent You From Doing Things People Your Age Normally Do (HEALTH_PROB_W1)",
    HELP_STRANGER_W1 = "Helped a Stranger or Someone You Didnt Know in the Past Month (HELP_STRANGER_W1)",
    HOPE_FUTURE_W1 = "You Always Remain Hopeful About the Future (HOPE_FUTURE_W1)",
    INCOME_W1 = "Monthly Household Income (Annual Household Income in U.S. and Australia) (INCOME_W1)",
    INCOME_12YRS_W1 = "Feelings About Familys Household Income When Growing Up (INCOME_12YRS_W1)",
    INCOME_DIFF_W1 = "Government Should Take Measures to Reduce Differences in Income Levels (INCOME_DIFF_W1)",
    INCOME_FEELINGS_W1 = "Feelings About Household Income (INCOME_FEELINGS_W1)",
    INTEREST_W1 = "Been Bothered in Last Two Weeks By: Little Interest or Pleasure in Doing Things (INTEREST_W1)",
    LIFE_APPROACH_W1 = "Your Religious Beliefs and Practices Are What Lie Behind Your Whole Approach to Life (LIFE_APPROACH_W1)",
    LIFE_BALANCE_W1 = "The Various Aspects of Your Life Are in Balance (LIFE_BALANCE_W1)",
    LIFE_PURPOSE_W1 = "You Understand Your Purpose in Life (LIFE_PURPOSE_W1)",
    LIFE_SAT_W1 = "How Satisfied Are You With Life as a Whole These Days (LIFE_SAT_W1)",
    LONELY_W1 = "How Often You Feel Lonely (LONELY_W1)",
    LOVED_BY_GOD_W1 = "You Feel Loved by God the Main God You Worship or the Spiritual Force That Guides Your Life (LOVED_BY_GOD_W1)",
    MARITAL_STATUS_W1 = "Marital Status (MARITAL_STATUS_W1)",
    MENTAL_HEALTH_W1 = "Mental Health (MENTAL_HEALTH_W1)",
    MOTHER_LOVED_W1 = "Felt Loved by Your Mother When Growing Up (MOTHER_LOVED_W1)",
    MOTHER_RELATN_W1 = "Relationship With Your Mother When Growing Up (MOTHER_RELATN_W1)",
    NUM_CHILDREN_W1 = "Number of Children Under 18 Years of Age in Household (NUM_CHILDREN_W1)",
    NUM_HOUSEHOLD_W1 = "Number of People 18 or Older Who Currently Live In Household (NUM_HOUSEHOLD_W1)",
    OBEY_LAW_W1 = "Whatever The Circumstances The Law Should Always Be Obeyed (OBEY_LAW_W1)",
    OUTSIDER_W1 = "Felt Like an Outsider in Your Family When Growing Up (OUTSIDER_W1)",
    OWN_RENT_HOME_W1 = "Rent or Own the Home You Live in (OWN_RENT_HOME_W1)",
    PARENTS_12YRS_W1 = "Parents Married to Each Other When You Were Around 12 Years Old (PARENTS_12YRS_W1)",
    PEACE_W1 = "You Feel You Are at Peace With Your Thoughts and Feelings (PEACE_W1)",
    PEOPLE_HELP_W1 = "You Could Count on People in Your Life to Help You if You Were in Trouble (PEOPLE_HELP_W1)",
    PHYSICAL_HLTH_W1 = "Physical Health (PHYSICAL_HLTH_W1)",
    POLITICAL_ID_W1 = "Political Party (POLITICAL_ID_W1)",
    PRAY_MEDITATE_W1 = "How Often You Pray or Meditate (PRAY_MEDITATE_W1)",
    PROMOTE_GOOD_W1 = "You Always Act to Promote Good in All Circumstances (PROMOTE_GOOD_W1)",
    REGION1_W1 = "Regional Level 1 (REGION1_W1)",
    REGION2_W1 = "Regional Level 2 (REGION2_W1)",
    REGION3_W1 = "Regional Level 3 (REGION3_W1)",
    REL_EXPERIENC_W1 = "Had a Profound Religious or Spiritual Awakening or Experience (REL_EXPERIENC_W1)",
    REL_IMPORTANT_W1 = "Religion an Important Part of Your Daily Life (REL_IMPORTANT_W1)",
    REL1_W1 = "Religion When Twelve Years Old (REL1_W1)",
    REL2_W1 = "Current Religion (REL2_W1)",
    REL3_W1 = "Christian Denomination or Church You Most Identify With (REL3_W1)",
    REL4_W1 = "Islam Sect or School You Most Identify With (REL4_W1)",
    REL5_W1 = "Hindu Sect or Denomination You Most Identify With (REL5_W1)",
    REL6_W1 = "Jewish Denomination or Tradition You Most Identify With (REL6_W1)",
    REL7_W1 = "Best Described as Atheist Agnostic or Neither (REL7_W1)",
    REL8_W1 = "Are You Spiritual Religious Both or Neither (REL8_W1)",
    REL9_W1 = "Buddhist Sect You Most Identify With (REL9_W1)",
    SACRED_TEXTS_W1 = "Read or Listen to Sacred Texts/Religious Literature (SACRED_TEXTS_W1)",
    SAT_LIVE_W1 = "Satisfaction With City or Area Where You Live (SAT_LIVE_W1)",
    SAT_RELATNSHP_W1 = "Your Relationships Are as Satisfying As You Want Them to Be (SAT_RELATNSHP_W1)",
    SAY_IN_GOVT_W1 = "People Like You Have a Say About What the Government Does (SAY_IN_GOVT_W1)",
    SELFID1_W1 = "First Identified Race/Ethnicity/Nationality of Respondent (SELFID1_W1)",
    SELFID2_W1 = "Second Identified Race/Ethnicity/Nationality of Respondent (SELFID2_W1)",
    SHOW_LOVE_W1 = "You Show Someone in Your Life You Love or Care for Them (SHOW_LOVE_W1)",
    SUFFERING_W1 = "The Extent to Which You Are Suffering (SUFFERING_W1)",
    SVCS_12YRS_W1 = "How Often You Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_12YRS_W1)",
    SVCS_FATHER_W1 = "How Often Your Father Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_FATHER_W1)",
    SVCS_MOTHER_W1 = "How Often Your Mother Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_MOTHER_W1)",
    TEACHINGS_1_W1 = "The Teachings of Christianity Are Very Important in Your Life (Christianity Identified as Current Religion) (TEACHINGS_1_W1)",
    TEACHINGS_2_W1 = "The Teachings of Islam Are Very Important in Your Life (Islam Identified as Current Religion) (TEACHINGS_2_W1)",
    TEACHINGS_3_W1 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism Identified as Current Religion) (TEACHINGS_3_W1)",
    TEACHINGS_4_W1 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism Identified as Current Religion) (TEACHINGS_4_W1)",
    TEACHINGS_5_W1 = "The Teachings of Judaism Are Very Important in Your Life (Judaism Identified as Current Religion) (TEACHINGS_5_W1)",
    TEACHINGS_6_W1 = "The Teachings of Sikhism Are Very Important in Your Life (Sikhism Identified as Current Religion) (TEACHINGS_6_W1)",
    TEACHINGS_7_W1 = "The Teachings of Bahai Are Very Important in Your Life (Bahai Identified as Current Religion) (TEACHINGS_7_W1)",
    TEACHINGS_8_W1 = "The Teachings of Jainism Are Very Important in Your Life (Jainism Identified as Current Religion) (TEACHINGS_8_W1)",
    TEACHINGS_9_W1 = "The Teachings of Shinto Are Very Important in Your Life (Shinto Identified as Current Religion) (TEACHINGS_9_W1)",
    TEACHINGS_10_W1 = "The Teachings of Taoism Are Very Important in Your Life (Taoism Identified as Current Religion) (TEACHINGS_10_W1)",
    TEACHINGS_11_W1 = "The Teachings of Confucianism Are Very Important in Your Life (Confucianism Identified as Current Religion) (TEACHINGS_11_W1)",
    TEACHINGS_12_W1 = "The Teachings of Primal Animist or Folk Religion Are Very Important in Your Life (Primal Animist or Folk Identified as Current Religion) (TEACHINGS_12_W1)",
    TEACHINGS_13_W1 = "The Teachings of Spiritism Are Very Important in Your Life (Spiritism Identified as Current Religion) (TEACHINGS_13_W1)",
    TEACHINGS_14_W1 = "The Teachings of African-Derived Religions Are Very Important in Your Life (African-Derived Religions Identified as Current Religion) (TEACHINGS_14_W1)",
    TEACHINGS_15_W1 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion Identified as Current Religion) (TEACHINGS_15_W1)",
    TELL_BELIEFS_W1 = "You Tell Other People About Your Religion or Spirituality Even When They Have Different Beliefs (TELL_BELIEFS_W1)",
    THREAT_LIFE_W1 = "How Much Youve Been Bothered by the Biggest Threat to Life Youve Witnessed or Experienced (THREAT_LIFE_W1)",
    TRAITS1_W1 = "Pair of Traits Applies To You: Extroverted Enthusiastic (TRAITS1_W1)",
    TRAITS2_W1 = "Pair of Traits Applies To You: Critical Quarrelsome (TRAITS2_W1)",
    TRAITS3_W1 = "Pair of Traits Applies To You: Dependable Self-disciplined (TRAITS3_W1)",
    TRAITS4_W1 = "Pair of Traits Applies To You: Anxious Easily Upset (TRAITS4_W1)",
    TRAITS5_W1 = "Pair of Traits Applies To You: Open to New Experiences Complex (TRAITS5_W1)",
    TRAITS6_W1 = "Pair of Traits Applies To You: Reserved Quiet (TRAITS6_W1)",
    TRAITS7_W1 = "Pair of Traits Applies To You: Sympathetic Warm (TRAITS7_W1)",
    TRAITS8_W1 = "Pair of Traits Applies To You: Disorganized Careless (TRAITS8_W1)",
    TRAITS9_W1 = "Pair of Traits Applies To You: Calm Emotionally Stable (TRAITS9_W1)",
    TRAITS10_W1 = "Pair of Traits Applies To You: Conventional Uncreative (TRAITS10_W1)",
    TRUST_PEOPLE_W1 = "People in This Country Trust One Another (TRUST_PEOPLE_W1)",
    URBAN_RURAL_W1 = "Urban/Rural (URBAN_RURAL_W1)",
    VOLUNTEERED_W1 = "Volunteered Your Time to an Organization in Past Month (VOLUNTEERED_W1)",
    WB_FIVEYRS_W1 = "Life Evaluation: Five Years From Now (WB_FIVEYRS_W1)",
    WB_TODAY_W1 = "Life Evaluation: Today (WB_TODAY_W1)",
    WORRY_SAFETY_W1 = "Worry About Safety Food or Housing (WORRY_SAFETY_W1)",
    WORTHWHILE_W1 = "The Things You Do in Your Life Are Worthwhile (WORTHWHILE_W1)",
    ANNUAL_WEIGHT1_W1 = "Annual Weight Year 1 (ANNUAL_WEIGHT1_W1)",
    STRATA_W1 = "Strata (STRATA_W1)",
    PSU_W1 = "PSU (PSU_W1)",
    FULL_PARTIAL_W1 = "Completion Status: (FULL_PARTIAL_W1)",
    COUNTRY_W2 = "Country of Respondent (COUNTRY_W2)",
    WAVE_W2 = "WAVE (WAVE_W2)",
    MODE_RECRUIT_W2 = "Recruitment Survey Mode (MODE_RECRUIT_W2)",
    MODE_ANNUAL_W2 = "Annual Survey Mode (MODE_ANNUAL_W2)",
    RECRUIT_TYPE_W2 = "Recruit Type (RECRUIT_TYPE_W2)",
    DOI_RECRUIT_W2 = "End Date of Interview - Recruit Survey (DOI_RECRUIT_W2)",
    DOI_ANNUAL_W2 = "End Date of Interview - Annual Survey (DOI_ANNUAL_W2)",
    ABUSED_W2 = "Physically or Sexually Abused When Growing Up (ABUSED_W2)",
    AFTER_DEATH_W2 = "Believe in Life After Death (AFTER_DEATH_W2)",
    AGE_W2 = "Age of Respondent (AGE_W2)",
    APPROVE_GOVT_W2 = "Job Performance of National Government (APPROVE_GOVT_W2)",
    ATTEND_SVCS_W2 = "How Often You Attend Religious Services (ATTEND_SVCS_W2)",
    BELIEVE_GOD_W2 = "Believe in One God More Than One God an Impersonal Spiritual Force or None of These (BELIEVE_GOD_W2)",
    BELONGING_W2 = "Sense of Belonging in Your Country (BELONGING_W2)",
    BODILY_PAIN_W2 = "Bodily Pain in Past 4 Weeks (BODILY_PAIN_W2)",
    BORN_COUNTRY_W2 = "Born in This Country (BORN_COUNTRY_W2)",
    CAPABLE_W2 = "Feel Very Capable in Most Things You Do in Life (CAPABLE_W2)",
    CIGARETTES_W2 = "Number of Cigarettes Smoked Each Day (CIGARETTES_W2)",
    CLOSE_TO_W2 = "Know One Special Person You Feel Very Close To (CLOSE_TO_W2)",
    CNTRY_REL_BUD_W2 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_BUD_W2)",
    CNTRY_REL_CHI_W2 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_CHI_W2)",
    CNTRY_REL_CHR_W2 = "The Teachings of Christianity Are Very Important in Your Life (Christianity is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_CHR_W2)",
    CNTRY_REL_HIN_W2 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_HIN_W2)",
    CNTRY_REL_ISL_W2 = "The Teachings of Islam Are Very Important in Your Life (Islam is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_ISL_W2)",
    CNTRY_REL_JUD_W2 = "The Teachings of Judaism Are Very Important in Your Life (Judaism is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_JUD_W2)",
    CNTRY_REL_SHI_W2 = "The Teachings of Shinto Are Very Important in Your Life (Shinto is Common Religion in Country Yet Not Identified as Current Religion) (CNTRY_REL_SHI_W2)",
    COMFORT_REL_W2 = "Find Strength or Comfort in Your Religion or Spirituality (COMFORT_REL_W2)",
    CONNECTED_REL_W2 = "Feel Connected to a Religion or Form of Spirituality (CONNECTED_REL_W2)",
    CONTENT_W2 = "Relational Contentment (CONTENT_W2)",
    CONTROL_WORRY_W2 = "Been Bothered in Last Two Weeks By: Not Being Able to Stop or Control Worrying (CONTROL_WORRY_W2)",
    COVID_DEATH_W2 = "Family Member or Close Friend Died From Coronavirus (COVID_DEATH_W2)",
    CRITICAL_W2 = "People in Your Religious Community are Critical of You or Your Lifestyle (CRITICAL_W2)",
    DAYS_EXERCISE_W2 = "Number of Days You Exercised in the Past Week (DAYS_EXERCISE_W2)",
    DEPRESSED_W2 = "Been Bothered in Last Two Weeks By: Feeling Down Depressed or Hopeless (DEPRESSED_W2)",
    DISCRIMINATED_W2 = "Feel Discriminated Against Because of Any Group You Are Part of (DISCRIMINATED_W2)",
    DONATED_W2 = "Charitable Giving (DONATED_W2)",
    DRINKS_W2 = "Number of Alcoholic Drinks You Drank in Past Seven Days (DRINKS_W2)",
    EDUCATION_W2 = "Highest Completed Level of Education (EDUCATION_W2)",
    EDUCATION_3_W2 = "Highest Completed Level of Education (Three Levels) (EDUCATION_3_W2)",
    EMPLOYMENT_W2 = "Employment Status (EMPLOYMENT_W2)",
    EXPECT_GOOD_W2 = "You Expect More Good Things to Happen to You Than Bad (EXPECT_GOOD_W2)",
    EXPENSES_W2 = "Worry About Being Able to Meet Normal Monthly Living Expenses (EXPENSES_W2)",
    FATHER_LOVED_W2 = "Felt Loved by Your Father When Growing Up (FATHER_LOVED_W2)",
    FATHER_RELATN_W2 = "Relationship With Your Father When Growing Up (FATHER_RELATN_W2)",
    FEEL_ANXIOUS_W2 = "Been Bothered in Last Two Weeks By: Feeling Nervous Anxious or on Edge (FEEL_ANXIOUS_W2)",
    FORGIVE_W2 = "How Often You Have Forgiven Those Who Have Hurt You (FORGIVE_W2)",
    FREEDOM_W2 = "You Have Freedom in Your Life to Pursue the Things Most Important to You (FREEDOM_W2)",
    GENDER_W2 = "Gender of Respondent (GENDER_W2)",
    GIVE_UP_W2 = "You Are Always Able to Give Up Some Happiness Now For Greater Happiness Later (GIVE_UP_W2)",
    GOD_PUNISH_W2 = "You feel God a God or a Spiritual Force Is Punishing You (GOD_PUNISH_W2)",
    GRATEFUL_W2 = "If You Listed Everything You Felt Grateful For It Would Be a Long List (GRATEFUL_W2)",
    GROUP_NOT_REL_W2 = "Participate in Groups That Are Not Religious (GROUP_NOT_REL_W2)",
    HAPPY_W2 = "How Happy You Usually Feel (HAPPY_W2)",
    HEALTH_GROWUP_W2 = "Your Health When Growing Up (HEALTH_GROWUP_W2)",
    HEALTH_PROB_W2 = "Health Problems Prevent You From Doing Things People Your Age Normally Do (HEALTH_PROB_W2)",
    HELP_STRANGER_W2 = "Helped a Stranger or Someone You Didnt Know in the Past Month (HELP_STRANGER_W2)",
    HOPE_FUTURE_W2 = "You Always Remain Hopeful About the Future (HOPE_FUTURE_W2)",
    INCOME_W2 = "Monthly Household Income (Annual Household Income in U.S. and Australia) (INCOME_W2)",
    INCOME_12YRS_W2 = "Feelings About Familys Household Income When Growing Up (INCOME_12YRS_W2)",
    INCOME_DIFF_W2 = "Government Should Take Measures to Reduce Differences in Income Levels (INCOME_DIFF_W2)",
    INCOME_FEELINGS_W2 = "Feelings About Household Income (INCOME_FEELINGS_W2)",
    INTEREST_W2 = "Been Bothered in Last Two Weeks By: Little Interest or Pleasure in Doing Things (INTEREST_W2)",
    LIFE_APPROACH_W2 = "Your Religious Beliefs and Practices Are What Lie Behind Your Whole Approach to Life (LIFE_APPROACH_W2)",
    LIFE_BALANCE_W2 = "The Various Aspects of Your Life Are in Balance (LIFE_BALANCE_W2)",
    LIFE_PURPOSE_W2 = "You Understand Your Purpose in Life (LIFE_PURPOSE_W2)",
    LIFE_SAT_W2 = "How Satisfied Are You With Life as a Whole These Days (LIFE_SAT_W2)",
    LONELY_W2 = "How Often You Feel Lonely (LONELY_W2)",
    LOVED_BY_GOD_W2 = "You Feel Loved by God the Main God You Worship or the Spiritual Force That Guides Your Life (LOVED_BY_GOD_W2)",
    MARITAL_STATUS_W2 = "Marital Status (MARITAL_STATUS_W2)",
    MENTAL_HEALTH_W2 = "Mental Health (MENTAL_HEALTH_W2)",
    MOTHER_LOVED_W2 = "Felt Loved by Your Mother When Growing Up (MOTHER_LOVED_W2)",
    MOTHER_RELATN_W2 = "Relationship With Your Mother When Growing Up (MOTHER_RELATN_W2)",
    NUM_CHILDREN_W2 = "Number of Children Under 18 Years of Age in Household (NUM_CHILDREN_W2)",
    NUM_HOUSEHOLD_W2 = "Number of People 18 or Older Who Currently Live In Household (NUM_HOUSEHOLD_W2)",
    OBEY_LAW_W2 = "Whatever The Circumstances The Law Should Always Be Obeyed (OBEY_LAW_W2)",
    OUTSIDER_W2 = "Felt Like an Outsider in Your Family When Growing Up (OUTSIDER_W2)",
    OWN_RENT_HOME_W2 = "Rent or Own the Home You Live in (OWN_RENT_HOME_W2)",
    PARENTS_12YRS_W2 = "Parents Married to Each Other When You Were Around 12 Years Old (PARENTS_12YRS_W2)",
    PEACE_W2 = "You Feel You Are at Peace With Your Thoughts and Feelings (PEACE_W2)",
    PEOPLE_HELP_W2 = "You Could Count on People in Your Life to Help You if You Were in Trouble (PEOPLE_HELP_W2)",
    PHYSICAL_HLTH_W2 = "Physical Health (PHYSICAL_HLTH_W2)",
    POLITICAL_ID_W2 = "Political Party (POLITICAL_ID_W2)",
    PRAY_MEDITATE_W2 = "How Often You Pray or Meditate (PRAY_MEDITATE_W2)",
    PROMOTE_GOOD_W2 = "You Always Act to Promote Good in All Circumstances (PROMOTE_GOOD_W2)",
    REGION1_W2 = "Regional Level 1 (REGION1_W2)",
    REGION2_W2 = "Regional Level 2 (REGION2_W2)",
    REGION3_W2 = "Regional Level 3 (REGION3_W2)",
    REL_EXPERIENC_W2 = "Had a Profound Religious or Spiritual Awakening or Experience (REL_EXPERIENC_W2)",
    REL_IMPORTANT_W2 = "Religion an Important Part of Your Daily Life (REL_IMPORTANT_W2)",
    REL1_W2 = "Religion When Twelve Years Old (REL1_W2)",
    REL2_W2 = "Current Religion (REL2_W2)",
    REL3_W2 = "Christian Denomination or Church You Most Identify With (REL3_W2)",
    REL4_W2 = "Islam Sect or School You Most Identify With (REL4_W2)",
    REL5_W2 = "Hindu Sect or Denomination You Most Identify With (REL5_W2)",
    REL6_W2 = "Jewish Denomination or Tradition You Most Identify With (REL6_W2)",
    REL7_W2 = "Best Described as Atheist Agnostic or Neither (REL7_W2)",
    REL8_W2 = "Are You Spiritual Religious Both or Neither (REL8_W2)",
    REL9_W2 = "Buddhist Sect You Most Identify With (REL9_W2)",
    SACRED_TEXTS_W2 = "Read or Listen to Sacred Texts/Religious Literature (SACRED_TEXTS_W2)",
    SAT_LIVE_W2 = "Satisfaction With City or Area Where You Live (SAT_LIVE_W2)",
    SAT_RELATNSHP_W2 = "Your Relationships Are as Satisfying As You Want Them to Be (SAT_RELATNSHP_W2)",
    SAY_IN_GOVT_W2 = "People Like You Have a Say About What the Government Does (SAY_IN_GOVT_W2)",
    SELFID1_W2 = "First Identified Race/Ethnicity/Nationality of Respondent (SELFID1_W2)",
    SELFID2_W2 = "Second Identified Race/Ethnicity/Nationality of Respondent (SELFID2_W2)",
    SHOW_LOVE_W2 = "You Show Someone in Your Life You Love or Care for Them (SHOW_LOVE_W2)",
    SUFFERING_W2 = "The Extent to Which You Are Suffering (SUFFERING_W2)",
    SVCS_12YRS_W2 = "How Often You Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_12YRS_W2)",
    SVCS_FATHER_W2 = "How Often Your Father Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_FATHER_W2)",
    SVCS_MOTHER_W2 = "How Often Your Mother Attended Religious Services or Worshiped When You Were Around 12 Years Old (SVCS_MOTHER_W2)",
    TEACHINGS_1_W2 = "The Teachings of Christianity Are Very Important in Your Life (Christianity Identified as Current Religion) (TEACHINGS_1_W2)",
    TEACHINGS_2_W2 = "The Teachings of Islam Are Very Important in Your Life (Islam Identified as Current Religion) (TEACHINGS_2_W2)",
    TEACHINGS_3_W2 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism Identified as Current Religion) (TEACHINGS_3_W2)",
    TEACHINGS_4_W2 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism Identified as Current Religion) (TEACHINGS_4_W2)",
    TEACHINGS_5_W2 = "The Teachings of Judaism Are Very Important in Your Life (Judaism Identified as Current Religion) (TEACHINGS_5_W2)",
    TEACHINGS_6_W2 = "The Teachings of Sikhism Are Very Important in Your Life (Sikhism Identified as Current Religion) (TEACHINGS_6_W2)",
    TEACHINGS_7_W2 = "The Teachings of Bahai Are Very Important in Your Life (Bahai Identified as Current Religion) (TEACHINGS_7_W2)",
    TEACHINGS_8_W2 = "The Teachings of Jainism Are Very Important in Your Life (Jainism Identified as Current Religion) (TEACHINGS_8_W2)",
    TEACHINGS_9_W2 = "The Teachings of Shinto Are Very Important in Your Life (Shinto Identified as Current Religion) (TEACHINGS_9_W2)",
    TEACHINGS_10_W2 = "The Teachings of Taoism Are Very Important in Your Life (Taoism Identified as Current Religion) (TEACHINGS_10_W2)",
    TEACHINGS_11_W2 = "The Teachings of Confucianism Are Very Important in Your Life (Confucianism Identified as Current Religion) (TEACHINGS_11_W2)",
    TEACHINGS_12_W2 = "The Teachings of Primal Animist or Folk Religion Are Very Important in Your Life (Primal Animist or Folk Identified as Current Religion) (TEACHINGS_12_W2)",
    TEACHINGS_13_W2 = "The Teachings of Spiritism Are Very Important in Your Life (Spiritism Identified as Current Religion) (TEACHINGS_13_W2)",
    TEACHINGS_14_W2 = "The Teachings of African-Derived Religions Are Very Important in Your Life (African-Derived Religions Identified as Current Religion) (TEACHINGS_14_W2)",
    TEACHINGS_15_W2 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion Identified as Current Religion) (TEACHINGS_15_W2)",
    TELL_BELIEFS_W2 = "You Tell Other People About Your Religion or Spirituality Even When They Have Different Beliefs (TELL_BELIEFS_W2)",
    THREAT_LIFE_W2 = "How Much Youve Been Bothered by the Biggest Threat to Life Youve Witnessed or Experienced (THREAT_LIFE_W2)",
    TRAITS1_W2 = "Pair of Traits Applies To You: Extroverted Enthusiastic (TRAITS1_W2)",
    TRAITS2_W2 = "Pair of Traits Applies To You: Critical Quarrelsome (TRAITS2_W2)",
    TRAITS3_W2 = "Pair of Traits Applies To You: Dependable Self-disciplined (TRAITS3_W2)",
    TRAITS4_W2 = "Pair of Traits Applies To You: Anxious Easily Upset (TRAITS4_W2)",
    TRAITS5_W2 = "Pair of Traits Applies To You: Open to New Experiences Complex (TRAITS5_W2)",
    TRAITS6_W2 = "Pair of Traits Applies To You: Reserved Quiet (TRAITS6_W2)",
    TRAITS7_W2 = "Pair of Traits Applies To You: Sympathetic Warm (TRAITS7_W2)",
    TRAITS8_W2 = "Pair of Traits Applies To You: Disorganized Careless (TRAITS8_W2)",
    TRAITS9_W2 = "Pair of Traits Applies To You: Calm Emotionally Stable (TRAITS9_W2)",
    TRAITS10_W2 = "Pair of Traits Applies To You: Conventional Uncreative (TRAITS10_W2)",
    TRUST_PEOPLE_W2 = "People in This Country Trust One Another (TRUST_PEOPLE_W2)",
    URBAN_RURAL_W2 = "Urban/Rural (URBAN_RURAL_W2)",
    VOLUNTEERED_W2 = "Volunteered Your Time to an Organization in Past Month (VOLUNTEERED_W2)",
    WB_FIVEYRS_W2 = "Life Evaluation: Five Years From Now (WB_FIVEYRS_W2)",
    WB_TODAY_W2 = "Life Evaluation: Today (WB_TODAY_W2)",
    WORRY_SAFETY_W2 = "Worry About Safety Food or Housing (WORRY_SAFETY_W2)",
    WORTHWHILE_W2 = "The Things You Do in Your Life Are Worthwhile (WORTHWHILE_W2)",
    ANNUAL_WEIGHT1_W2 = "Annual Weight Year 1 (ANNUAL_WEIGHT1_W2)",
    STRATA_W2 = "Strata (STRATA_W2)",
    PSU_W2 = "PSU (PSU_W2)",
    FULL_PARTIAL_W2 = "Completion Status: (FULL_PARTIAL_W2)",
  )
}

#' Get Outcome Name (better) Wihout Label
#'
#' A relatively simple switch function to get a better name for each variable
#'
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a character string
#' @examples {
#'   get_outcome_better_name_no_code("HAPPY_W1")
#' }
#' @export
get_outcome_better_name_no_code <- function(var) {
  switch(var,
    ID = "(No Label)",
    COUNTRY_W1 = "Country of Respondent",
    WAVE_W1 = "WAVE",
    MODE_RECRUIT_W1 = "Recruitment Survey Mode",
    MODE_ANNUAL_W1 = "Annual Survey Mode",
    RECRUIT_TYPE_W1 = "Recruit Type",
    DOI_RECRUIT_W1 = "End Date of Interview - Recruit Survey",
    DOI_ANNUAL_W1 = "End Date of Interview - Annual Survey",
    ABUSED_W1 = "Physically or Sexually Abused When Growing Up",
    AFTER_DEATH_W1 = "Believe in Life After Death",
    AGE_W1 = "Age of Respondent",
    APPROVE_GOVT_W1 = "Job Performance of National Government",
    ATTEND_SVCS_W1 = "How Often You Attend Religious Services",
    BELIEVE_GOD_W1 = "Believe in One God More Than One God an Impersonal Spiritual Force or None of These",
    BELONGING_W1 = "Sense of Belonging in Your Country",
    BODILY_PAIN_W1 = "Bodily Pain in Past 4 Weeks",
    BORN_COUNTRY_W1 = "Born in This Country",
    CAPABLE_W1 = "Feel Very Capable in Most Things You Do in Life",
    CIGARETTES_W1 = "Number of Cigarettes Smoked Each Day",
    CLOSE_TO_W1 = "Know One Special Person You Feel Very Close To",
    CNTRY_REL_BUD_W1 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_CHI_W1 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_CHR_W1 = "The Teachings of Christianity Are Very Important in Your Life (Christianity is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_HIN_W1 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_ISL_W1 = "The Teachings of Islam Are Very Important in Your Life (Islam is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_JUD_W1 = "The Teachings of Judaism Are Very Important in Your Life (Judaism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_SHI_W1 = "The Teachings of Shinto Are Very Important in Your Life (Shinto is Common Religion in Country Yet Not Identified as Current Religion)",
    COMFORT_REL_W1 = "Find Strength or Comfort in Your Religion or Spirituality",
    CONNECTED_REL_W1 = "Feel Connected to a Religion or Form of Spirituality",
    CONTENT_W1 = "Relational Contentment",
    CONTROL_WORRY_W1 = "Been Bothered in Last Two Weeks By: Not Being Able to Stop or Control Worrying",
    COVID_DEATH_W1 = "Family Member or Close Friend Died From Coronavirus",
    CRITICAL_W1 = "People in Your Religious Community are Critical of You or Your Lifestyle",
    DAYS_EXERCISE_W1 = "Number of Days You Exercised in the Past Week",
    DEPRESSED_W1 = "Been Bothered in Last Two Weeks By: Feeling Down Depressed or Hopeless",
    DISCRIMINATED_W1 = "Feel Discriminated Against Because of Any Group You Are Part of",
    DONATED_W1 = "Charitable Giving",
    DRINKS_W1 = "Number of Alcoholic Drinks You Drank in Past Seven Days",
    EDUCATION_W1 = "Highest Completed Level of Education",
    EDUCATION_3_W1 = "Highest Completed Level of Education (Three Levels)",
    EMPLOYMENT_W1 = "Employment Status",
    EXPECT_GOOD_W1 = "You Expect More Good Things to Happen to You Than Bad",
    EXPENSES_W1 = "Worry About Being Able to Meet Normal Monthly Living Expenses",
    FATHER_LOVED_W1 = "Felt Loved by Your Father When Growing Up",
    FATHER_RELATN_W1 = "Relationship With Your Father When Growing Up",
    FEEL_ANXIOUS_W1 = "Been Bothered in Last Two Weeks By: Feeling Nervous Anxious or on Edge",
    FORGIVE_W1 = "How Often You Have Forgiven Those Who Have Hurt You",
    FREEDOM_W1 = "You Have Freedom in Your Life to Pursue the Things Most Important to You",
    GENDER_W1 = "Gender of Respondent",
    GIVE_UP_W1 = "You Are Always Able to Give Up Some Happiness Now For Greater Happiness Later",
    GOD_PUNISH_W1 = "You feel God a God or a Spiritual Force Is Punishing You",
    GRATEFUL_W1 = "If You Listed Everything You Felt Grateful For It Would Be a Long List",
    GROUP_NOT_REL_W1 = "Participate in Groups That Are Not Religious",
    HAPPY_W1 = "How Happy You Usually Feel",
    HEALTH_GROWUP_W1 = "Your Health When Growing Up",
    HEALTH_PROB_W1 = "Health Problems Prevent You From Doing Things People Your Age Normally Do",
    HELP_STRANGER_W1 = "Helped a Stranger or Someone You Didnt Know in the Past Month",
    HOPE_FUTURE_W1 = "You Always Remain Hopeful About the Future",
    INCOME_W1 = "Monthly Household Income (Annual Household Income in U.S. and Australia)",
    INCOME_12YRS_W1 = "Feelings About Familys Household Income When Growing Up",
    INCOME_DIFF_W1 = "Government Should Take Measures to Reduce Differences in Income Levels",
    INCOME_FEELINGS_W1 = "Feelings About Household Income",
    INTEREST_W1 = "Been Bothered in Last Two Weeks By: Little Interest or Pleasure in Doing Things",
    LIFE_APPROACH_W1 = "Your Religious Beliefs and Practices Are What Lie Behind Your Whole Approach to Life",
    LIFE_BALANCE_W1 = "The Various Aspects of Your Life Are in Balance",
    LIFE_PURPOSE_W1 = "You Understand Your Purpose in Life",
    LIFE_SAT_W1 = "How Satisfied Are You With Life as a Whole These Days",
    LONELY_W1 = "How Often You Feel Lonely",
    LOVED_BY_GOD_W1 = "You Feel Loved by God the Main God You Worship or the Spiritual Force That Guides Your Life",
    MARITAL_STATUS_W1 = "Marital Status",
    MENTAL_HEALTH_W1 = "Mental Health",
    MOTHER_LOVED_W1 = "Felt Loved by Your Mother When Growing Up",
    MOTHER_RELATN_W1 = "Relationship With Your Mother When Growing Up",
    NUM_CHILDREN_W1 = "Number of Children Under 18 Years of Age in Household",
    NUM_HOUSEHOLD_W1 = "Number of People 18 or Older Who Currently Live In Household",
    OBEY_LAW_W1 = "Whatever The Circumstances The Law Should Always Be Obeyed",
    OUTSIDER_W1 = "Felt Like an Outsider in Your Family When Growing Up",
    OWN_RENT_HOME_W1 = "Rent or Own the Home You Live in",
    PARENTS_12YRS_W1 = "Parents Married to Each Other When You Were Around 12 Years Old",
    PEACE_W1 = "You Feel You Are at Peace With Your Thoughts and Feelings",
    PEOPLE_HELP_W1 = "You Could Count on People in Your Life to Help You if You Were in Trouble",
    PHYSICAL_HLTH_W1 = "Physical Health",
    POLITICAL_ID_W1 = "Political Party",
    PRAY_MEDITATE_W1 = "How Often You Pray or Meditate",
    PROMOTE_GOOD_W1 = "You Always Act to Promote Good in All Circumstances",
    REGION1_W1 = "Regional Level 1",
    REGION2_W1 = "Regional Level 2",
    REGION3_W1 = "Regional Level 3",
    REL_EXPERIENC_W1 = "Had a Profound Religious or Spiritual Awakening or Experience",
    REL_IMPORTANT_W1 = "Religion an Important Part of Your Daily Life",
    REL1_W1 = "Religion When Twelve Years Old",
    REL2_W1 = "Current Religion",
    REL3_W1 = "Christian Denomination or Church You Most Identify With",
    REL4_W1 = "Islam Sect or School You Most Identify With",
    REL5_W1 = "Hindu Sect or Denomination You Most Identify With",
    REL6_W1 = "Jewish Denomination or Tradition You Most Identify With",
    REL7_W1 = "Best Described as Atheist Agnostic or Neither",
    REL8_W1 = "Are You Spiritual Religious Both or Neither",
    REL9_W1 = "Buddhist Sect You Most Identify With",
    SACRED_TEXTS_W1 = "Read or Listen to Sacred Texts/Religious Literature",
    SAT_LIVE_W1 = "Satisfaction With City or Area Where You Live",
    SAT_RELATNSHP_W1 = "Your Relationships Are as Satisfying As You Want Them to Be",
    SAY_IN_GOVT_W1 = "People Like You Have a Say About What the Government Does",
    SELFID1_W1 = "First Identified Race/Ethnicity/Nationality of Respondent",
    SELFID2_W1 = "Second Identified Race/Ethnicity/Nationality of Respondent",
    SHOW_LOVE_W1 = "You Show Someone in Your Life You Love or Care for Them",
    SUFFERING_W1 = "The Extent to Which You Are Suffering",
    SVCS_12YRS_W1 = "How Often You Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    SVCS_FATHER_W1 = "How Often Your Father Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    SVCS_MOTHER_W1 = "How Often Your Mother Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    TEACHINGS_1_W1 = "The Teachings of Christianity Are Very Important in Your Life (Christianity Identified as Current Religion)",
    TEACHINGS_2_W1 = "The Teachings of Islam Are Very Important in Your Life (Islam Identified as Current Religion)",
    TEACHINGS_3_W1 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism Identified as Current Religion)",
    TEACHINGS_4_W1 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism Identified as Current Religion)",
    TEACHINGS_5_W1 = "The Teachings of Judaism Are Very Important in Your Life (Judaism Identified as Current Religion)",
    TEACHINGS_6_W1 = "The Teachings of Sikhism Are Very Important in Your Life (Sikhism Identified as Current Religion)",
    TEACHINGS_7_W1 = "The Teachings of Bahai Are Very Important in Your Life (Bahai Identified as Current Religion)",
    TEACHINGS_8_W1 = "The Teachings of Jainism Are Very Important in Your Life (Jainism Identified as Current Religion)",
    TEACHINGS_9_W1 = "The Teachings of Shinto Are Very Important in Your Life (Shinto Identified as Current Religion)",
    TEACHINGS_10_W1 = "The Teachings of Taoism Are Very Important in Your Life (Taoism Identified as Current Religion)",
    TEACHINGS_11_W1 = "The Teachings of Confucianism Are Very Important in Your Life (Confucianism Identified as Current Religion)",
    TEACHINGS_12_W1 = "The Teachings of Primal Animist or Folk Religion Are Very Important in Your Life (Primal Animist or Folk Identified as Current Religion)",
    TEACHINGS_13_W1 = "The Teachings of Spiritism Are Very Important in Your Life (Spiritism Identified as Current Religion)",
    TEACHINGS_14_W1 = "The Teachings of African-Derived Religions Are Very Important in Your Life (African-Derived Religions Identified as Current Religion)",
    TEACHINGS_15_W1 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion Identified as Current Religion)",
    TELL_BELIEFS_W1 = "You Tell Other People About Your Religion or Spirituality Even When They Have Different Beliefs",
    THREAT_LIFE_W1 = "How Much Youve Been Bothered by the Biggest Threat to Life Youve Witnessed or Experienced",
    TRAITS1_W1 = "Pair of Traits Applies To You: Extroverted Enthusiastic",
    TRAITS2_W1 = "Pair of Traits Applies To You: Critical Quarrelsome",
    TRAITS3_W1 = "Pair of Traits Applies To You: Dependable Self-disciplined",
    TRAITS4_W1 = "Pair of Traits Applies To You: Anxious Easily Upset",
    TRAITS5_W1 = "Pair of Traits Applies To You: Open to New Experiences Complex",
    TRAITS6_W1 = "Pair of Traits Applies To You: Reserved Quiet",
    TRAITS7_W1 = "Pair of Traits Applies To You: Sympathetic Warm",
    TRAITS8_W1 = "Pair of Traits Applies To You: Disorganized Careless",
    TRAITS9_W1 = "Pair of Traits Applies To You: Calm Emotionally Stable",
    TRAITS10_W1 = "Pair of Traits Applies To You: Conventional Uncreative",
    TRUST_PEOPLE_W1 = "People in This Country Trust One Another",
    URBAN_RURAL_W1 = "Urban/Rural",
    VOLUNTEERED_W1 = "Volunteered Your Time to an Organization in Past Month",
    WB_FIVEYRS_W1 = "Life Evaluation: Five Years From Now",
    WB_TODAY_W1 = "Life Evaluation: Today",
    WORRY_SAFETY_W1 = "Worry About Safety Food or Housing",
    WORTHWHILE_W1 = "The Things You Do in Your Life Are Worthwhile",
    ANNUAL_WEIGHT1_W1 = "Annual Weight Year 1",
    STRATA_W1 = "Strata",
    PSU_W1 = "PSU",
    FULL_PARTIAL_W1 = "Completion Status:",
    COUNTRY_W2 = "Country of Respondent",
    WAVE_W2 = "WAVE",
    MODE_RECRUIT_W2 = "Recruitment Survey Mode",
    MODE_ANNUAL_W2 = "Annual Survey Mode",
    RECRUIT_TYPE_W2 = "Recruit Type",
    DOI_RECRUIT_W2 = "End Date of Interview - Recruit Survey",
    DOI_ANNUAL_W2 = "End Date of Interview - Annual Survey",
    ABUSED_W2 = "Physically or Sexually Abused When Growing Up",
    AFTER_DEATH_W2 = "Believe in Life After Death",
    AGE_W2 = "Age of Respondent",
    APPROVE_GOVT_W2 = "Job Performance of National Government",
    ATTEND_SVCS_W2 = "How Often You Attend Religious Services",
    BELIEVE_GOD_W2 = "Believe in One God More Than One God an Impersonal Spiritual Force or None of These",
    BELONGING_W2 = "Sense of Belonging in Your Country",
    BODILY_PAIN_W2 = "Bodily Pain in Past 4 Weeks",
    BORN_COUNTRY_W2 = "Born in This Country",
    CAPABLE_W2 = "Feel Very Capable in Most Things You Do in Life",
    CIGARETTES_W2 = "Number of Cigarettes Smoked Each Day",
    CLOSE_TO_W2 = "Know One Special Person You Feel Very Close To",
    CNTRY_REL_BUD_W2 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_CHI_W2 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_CHR_W2 = "The Teachings of Christianity Are Very Important in Your Life (Christianity is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_HIN_W2 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_ISL_W2 = "The Teachings of Islam Are Very Important in Your Life (Islam is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_JUD_W2 = "The Teachings of Judaism Are Very Important in Your Life (Judaism is Common Religion in Country Yet Not Identified as Current Religion)",
    CNTRY_REL_SHI_W2 = "The Teachings of Shinto Are Very Important in Your Life (Shinto is Common Religion in Country Yet Not Identified as Current Religion)",
    COMFORT_REL_W2 = "Find Strength or Comfort in Your Religion or Spirituality",
    CONNECTED_REL_W2 = "Feel Connected to a Religion or Form of Spirituality",
    CONTENT_W2 = "Relational Contentment",
    CONTROL_WORRY_W2 = "Been Bothered in Last Two Weeks By: Not Being Able to Stop or Control Worrying",
    COVID_DEATH_W2 = "Family Member or Close Friend Died From Coronavirus",
    CRITICAL_W2 = "People in Your Religious Community are Critical of You or Your Lifestyle",
    DAYS_EXERCISE_W2 = "Number of Days You Exercised in the Past Week",
    DEPRESSED_W2 = "Been Bothered in Last Two Weeks By: Feeling Down Depressed or Hopeless",
    DISCRIMINATED_W2 = "Feel Discriminated Against Because of Any Group You Are Part of",
    DONATED_W2 = "Charitable Giving",
    DRINKS_W2 = "Number of Alcoholic Drinks You Drank in Past Seven Days",
    EDUCATION_W2 = "Highest Completed Level of Education",
    EDUCATION_3_W2 = "Highest Completed Level of Education (Three Levels)",
    EMPLOYMENT_W2 = "Employment Status",
    EXPECT_GOOD_W2 = "You Expect More Good Things to Happen to You Than Bad",
    EXPENSES_W2 = "Worry About Being Able to Meet Normal Monthly Living Expenses",
    FATHER_LOVED_W2 = "Felt Loved by Your Father When Growing Up",
    FATHER_RELATN_W2 = "Relationship With Your Father When Growing Up",
    FEEL_ANXIOUS_W2 = "Been Bothered in Last Two Weeks By: Feeling Nervous Anxious or on Edge",
    FORGIVE_W2 = "How Often You Have Forgiven Those Who Have Hurt You",
    FREEDOM_W2 = "You Have Freedom in Your Life to Pursue the Things Most Important to You",
    GENDER_W2 = "Gender of Respondent",
    GIVE_UP_W2 = "You Are Always Able to Give Up Some Happiness Now For Greater Happiness Later",
    GOD_PUNISH_W2 = "You feel God a God or a Spiritual Force Is Punishing You",
    GRATEFUL_W2 = "If You Listed Everything You Felt Grateful For It Would Be a Long List",
    GROUP_NOT_REL_W2 = "Participate in Groups That Are Not Religious",
    HAPPY_W2 = "How Happy You Usually Feel",
    HEALTH_GROWUP_W2 = "Your Health When Growing Up",
    HEALTH_PROB_W2 = "Health Problems Prevent You From Doing Things People Your Age Normally Do",
    HELP_STRANGER_W2 = "Helped a Stranger or Someone You Didnt Know in the Past Month",
    HOPE_FUTURE_W2 = "You Always Remain Hopeful About the Future",
    INCOME_W2 = "Monthly Household Income (Annual Household Income in U.S. and Australia)",
    INCOME_12YRS_W2 = "Feelings About Familys Household Income When Growing Up",
    INCOME_DIFF_W2 = "Government Should Take Measures to Reduce Differences in Income Levels",
    INCOME_FEELINGS_W2 = "Feelings About Household Income",
    INTEREST_W2 = "Been Bothered in Last Two Weeks By: Little Interest or Pleasure in Doing Things",
    LIFE_APPROACH_W2 = "Your Religious Beliefs and Practices Are What Lie Behind Your Whole Approach to Life",
    LIFE_BALANCE_W2 = "The Various Aspects of Your Life Are in Balance",
    LIFE_PURPOSE_W2 = "You Understand Your Purpose in Life",
    LIFE_SAT_W2 = "How Satisfied Are You With Life as a Whole These Days",
    LONELY_W2 = "How Often You Feel Lonely",
    LOVED_BY_GOD_W2 = "You Feel Loved by God the Main God You Worship or the Spiritual Force That Guides Your Life",
    MARITAL_STATUS_W2 = "Marital Status",
    MENTAL_HEALTH_W2 = "Mental Health",
    MOTHER_LOVED_W2 = "Felt Loved by Your Mother When Growing Up",
    MOTHER_RELATN_W2 = "Relationship With Your Mother When Growing Up",
    NUM_CHILDREN_W2 = "Number of Children Under 18 Years of Age in Household",
    NUM_HOUSEHOLD_W2 = "Number of People 18 or Older Who Currently Live In Household",
    OBEY_LAW_W2 = "Whatever The Circumstances The Law Should Always Be Obeyed",
    OUTSIDER_W2 = "Felt Like an Outsider in Your Family When Growing Up",
    OWN_RENT_HOME_W2 = "Rent or Own the Home You Live in",
    PARENTS_12YRS_W2 = "Parents Married to Each Other When You Were Around 12 Years Old",
    PEACE_W2 = "You Feel You Are at Peace With Your Thoughts and Feelings",
    PEOPLE_HELP_W2 = "You Could Count on People in Your Life to Help You if You Were in Trouble",
    PHYSICAL_HLTH_W2 = "Physical Health",
    POLITICAL_ID_W2 = "Political Party",
    PRAY_MEDITATE_W2 = "How Often You Pray or Meditate",
    PROMOTE_GOOD_W2 = "You Always Act to Promote Good in All Circumstances",
    REGION1_W2 = "Regional Level 1",
    REGION2_W2 = "Regional Level 2",
    REGION3_W2 = "Regional Level 3",
    REL_EXPERIENC_W2 = "Had a Profound Religious or Spiritual Awakening or Experience",
    REL_IMPORTANT_W2 = "Religion an Important Part of Your Daily Life",
    REL1_W2 = "Religion When Twelve Years Old",
    REL2_W2 = "Current Religion",
    REL3_W2 = "Christian Denomination or Church You Most Identify With",
    REL4_W2 = "Islam Sect or School You Most Identify With",
    REL5_W2 = "Hindu Sect or Denomination You Most Identify With",
    REL6_W2 = "Jewish Denomination or Tradition You Most Identify With",
    REL7_W2 = "Best Described as Atheist Agnostic or Neither",
    REL8_W2 = "Are You Spiritual Religious Both or Neither",
    REL9_W2 = "Buddhist Sect You Most Identify With",
    SACRED_TEXTS_W2 = "Read or Listen to Sacred Texts/Religious Literature",
    SAT_LIVE_W2 = "Satisfaction With City or Area Where You Live",
    SAT_RELATNSHP_W2 = "Your Relationships Are as Satisfying As You Want Them to Be",
    SAY_IN_GOVT_W2 = "People Like You Have a Say About What the Government Does",
    SELFID1_W2 = "First Identified Race/Ethnicity/Nationality of Respondent",
    SELFID2_W2 = "Second Identified Race/Ethnicity/Nationality of Respondent",
    SHOW_LOVE_W2 = "You Show Someone in Your Life You Love or Care for Them",
    SUFFERING_W2 = "The Extent to Which You Are Suffering",
    SVCS_12YRS_W2 = "How Often You Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    SVCS_FATHER_W2 = "How Often Your Father Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    SVCS_MOTHER_W2 = "How Often Your Mother Attended Religious Services or Worshiped When You Were Around 12 Years Old",
    TEACHINGS_1_W2 = "The Teachings of Christianity Are Very Important in Your Life (Christianity Identified as Current Religion)",
    TEACHINGS_2_W2 = "The Teachings of Islam Are Very Important in Your Life (Islam Identified as Current Religion)",
    TEACHINGS_3_W2 = "The Teachings of Hinduism Are Very Important in Your Life (Hinduism Identified as Current Religion)",
    TEACHINGS_4_W2 = "The Teachings of Buddhism Are Very Important in Your Life (Buddhism Identified as Current Religion)",
    TEACHINGS_5_W2 = "The Teachings of Judaism Are Very Important in Your Life (Judaism Identified as Current Religion)",
    TEACHINGS_6_W2 = "The Teachings of Sikhism Are Very Important in Your Life (Sikhism Identified as Current Religion)",
    TEACHINGS_7_W2 = "The Teachings of Bahai Are Very Important in Your Life (Bahai Identified as Current Religion)",
    TEACHINGS_8_W2 = "The Teachings of Jainism Are Very Important in Your Life (Jainism Identified as Current Religion)",
    TEACHINGS_9_W2 = "The Teachings of Shinto Are Very Important in Your Life (Shinto Identified as Current Religion)",
    TEACHINGS_10_W2 = "The Teachings of Taoism Are Very Important in Your Life (Taoism Identified as Current Religion)",
    TEACHINGS_11_W2 = "The Teachings of Confucianism Are Very Important in Your Life (Confucianism Identified as Current Religion)",
    TEACHINGS_12_W2 = "The Teachings of Primal Animist or Folk Religion Are Very Important in Your Life (Primal Animist or Folk Identified as Current Religion)",
    TEACHINGS_13_W2 = "The Teachings of Spiritism Are Very Important in Your Life (Spiritism Identified as Current Religion)",
    TEACHINGS_14_W2 = "The Teachings of African-Derived Religions Are Very Important in Your Life (African-Derived Religions Identified as Current Religion)",
    TEACHINGS_15_W2 = "The Teachings of Chinese Folk/Traditional Religion are Very Important in Your Life (Chinese Folk/Traditional Religion Identified as Current Religion)",
    TELL_BELIEFS_W2 = "You Tell Other People About Your Religion or Spirituality Even When They Have Different Beliefs",
    THREAT_LIFE_W2 = "How Much Youve Been Bothered by the Biggest Threat to Life Youve Witnessed or Experienced",
    TRAITS1_W2 = "Pair of Traits Applies To You: Extroverted Enthusiastic",
    TRAITS2_W2 = "Pair of Traits Applies To You: Critical Quarrelsome",
    TRAITS3_W2 = "Pair of Traits Applies To You: Dependable Self-disciplined",
    TRAITS4_W2 = "Pair of Traits Applies To You: Anxious Easily Upset",
    TRAITS5_W2 = "Pair of Traits Applies To You: Open to New Experiences Complex",
    TRAITS6_W2 = "Pair of Traits Applies To You: Reserved Quiet",
    TRAITS7_W2 = "Pair of Traits Applies To You: Sympathetic Warm",
    TRAITS8_W2 = "Pair of Traits Applies To You: Disorganized Careless",
    TRAITS9_W2 = "Pair of Traits Applies To You: Calm Emotionally Stable",
    TRAITS10_W2 = "Pair of Traits Applies To You: Conventional Uncreative",
    TRUST_PEOPLE_W2 = "People in This Country Trust One Another",
    URBAN_RURAL_W2 = "Urban/Rural",
    VOLUNTEERED_W2 = "Volunteered Your Time to an Organization in Past Month",
    WB_FIVEYRS_W2 = "Life Evaluation: Five Years From Now",
    WB_TODAY_W2 = "Life Evaluation: Today",
    WORRY_SAFETY_W2 = "Worry About Safety Food or Housing",
    WORTHWHILE_W2 = "The Things You Do in Your Life Are Worthwhile",
    ANNUAL_WEIGHT1_W2 = "Annual Weight Year 1",
    STRATA_W2 = "Strata",
    PSU_W2 = "PSU",
    FULL_PARTIAL_W2 = "Completion Status",
  )
}

#' Get Outcome Response Scale
#'
#' A relatively simple switch function to get the scale ('bin', 'cont')
#'
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a character string
#' @examples {
#'   get_outcome_scale("HAPPY_W1")
#' }
#' @export
get_outcome_scale <- function(var) {
  switch(var,
    ID = "(system variable)",
    COUNTRY_W1 = "likert",
    WAVE_W1 = "(system variable)",
    MODE_RECRUIT_W1 = "likert",
    MODE_ANNUAL_W1 = "likert",
    RECRUIT_TYPE_W1 = "likert",
    DOI_RECRUIT_W1 = "(system variable)",
    DOI_ANNUAL_W1 = "(system variable)",
    ABUSED_W1 = "bin",
    AFTER_DEATH_W1 = "bin",
    AGE_W1 = "cont",
    APPROVE_GOVT_W1 = "likert",
    ATTEND_SVCS_W1 = "likert",
    BELIEVE_GOD_W1 = "likert",
    BELONGING_W1 = "cont",
    BODILY_PAIN_W1 = "likert",
    BORN_COUNTRY_W1 = "likert",
    CAPABLE_W1 = "likert",
    CIGARETTES_W1 = "cont",
    CLOSE_TO_W1 = "bin",
    CNTRY_REL_BUD_W1 = "cont",
    CNTRY_REL_CHI_W1 = "cont",
    CNTRY_REL_CHR_W1 = "cont",
    CNTRY_REL_HIN_W1 = "cont",
    CNTRY_REL_ISL_W1 = "cont",
    CNTRY_REL_JUD_W1 = "cont",
    CNTRY_REL_SHI_W1 = "cont",
    COMFORT_REL_W1 = "likert",
    CONNECTED_REL_W1 = "likert",
    CONTENT_W1 = "cont",
    CONTROL_WORRY_W1 = "likert",
    COVID_DEATH_W1 = "bin",
    CRITICAL_W1 = "likert",
    DAYS_EXERCISE_W1 = "likert",
    DEPRESSED_W1 = "likert",
    DISCRIMINATED_W1 = "likert",
    DONATED_W1 = "bin",
    DRINKS_W1 = "cont",
    EDUCATION_W1 = "nominal",
    EDUCATION_3_W1 = "likert",
    EMPLOYMENT_W1 = "likert",
    EXPECT_GOOD_W1 = "cont",
    EXPENSES_W1 = "cont",
    FATHER_LOVED_W1 = "bin",
    FATHER_RELATN_W1 = "likert",
    FEEL_ANXIOUS_W1 = "likert",
    FORGIVE_W1 = "likert",
    FREEDOM_W1 = "cont",
    GENDER_W1 = "likert",
    GIVE_UP_W1 = "cont",
    GOD_PUNISH_W1 = "likert",
    GRATEFUL_W1 = "cont",
    GROUP_NOT_REL_W1 = "likert",
    HAPPY_W1 = "cont",
    HEALTH_GROWUP_W1 = "likert",
    HEALTH_PROB_W1 = "bin",
    HELP_STRANGER_W1 = "bin",
    HOPE_FUTURE_W1 = "cont",
    INCOME_W1 = "nominal",
    INCOME_12YRS_W1 = "likert",
    INCOME_DIFF_W1 = "likert",
    INCOME_FEELINGS_W1 = "likert",
    INTEREST_W1 = "likert",
    LIFE_APPROACH_W1 = "likert",
    LIFE_BALANCE_W1 = "likert",
    LIFE_PURPOSE_W1 = "cont",
    LIFE_SAT_W1 = "cont",
    LONELY_W1 = "cont",
    LOVED_BY_GOD_W1 = "likert",
    MARITAL_STATUS_W1 = "likert",
    MENTAL_HEALTH_W1 = "cont",
    MOTHER_LOVED_W1 = "bin",
    MOTHER_RELATN_W1 = "likert",
    NUM_CHILDREN_W1 = "cont",
    NUM_HOUSEHOLD_W1 = "cont",
    OBEY_LAW_W1 = "likert",
    OUTSIDER_W1 = "bin",
    OWN_RENT_HOME_W1 = "likert",
    PARENTS_12YRS_W1 = "likert",
    PEACE_W1 = "likert",
    PEOPLE_HELP_W1 = "cont",
    PHYSICAL_HLTH_W1 = "cont",
    POLITICAL_ID_W1 = "cont",
    PRAY_MEDITATE_W1 = "likert",
    PROMOTE_GOOD_W1 = "cont",
    REGION1_W1 = "nominal",
    REGION2_W1 = "nominal",
    REGION3_W1 = "nominal",
    REL_EXPERIENC_W1 = "bin",
    REL_IMPORTANT_W1 = "bin",
    REL1_W1 = "likert",
    REL2_W1 = "likert",
    REL3_W1 = "likert",
    REL4_W1 = "likert",
    REL5_W1 = "likert",
    REL6_W1 = "likert",
    REL7_W1 = "likert",
    REL8_W1 = "likert",
    REL9_W1 = "likert",
    SACRED_TEXTS_W1 = "likert",
    SAT_LIVE_W1 = "likert",
    SAT_RELATNSHP_W1 = "cont",
    SAY_IN_GOVT_W1 = "likert",
    SELFID1_W1 = "nominal",
    SELFID2_W1 = "nominal",
    SHOW_LOVE_W1 = "cont",
    SUFFERING_W1 = "likert",
    SVCS_12YRS_W1 = "likert",
    SVCS_FATHER_W1 = "likert",
    SVCS_MOTHER_W1 = "likert",
    TEACHINGS_1_W1 = "cont",
    TEACHINGS_2_W1 = "cont",
    TEACHINGS_3_W1 = "cont",
    TEACHINGS_4_W1 = "cont",
    TEACHINGS_5_W1 = "cont",
    TEACHINGS_6_W1 = "cont",
    TEACHINGS_7_W1 = "cont",
    TEACHINGS_8_W1 = "cont",
    TEACHINGS_9_W1 = "cont",
    TEACHINGS_10_W1 = "cont",
    TEACHINGS_11_W1 = "cont",
    TEACHINGS_12_W1 = "cont",
    TEACHINGS_13_W1 = "cont",
    TEACHINGS_14_W1 = "cont",
    TEACHINGS_15_W1 = "cont",
    TELL_BELIEFS_W1 = "likert",
    THREAT_LIFE_W1 = "likert",
    TRAITS1_W1 = "likert",
    TRAITS2_W1 = "likert",
    TRAITS3_W1 = "likert",
    TRAITS4_W1 = "likert",
    TRAITS5_W1 = "likert",
    TRAITS6_W1 = "likert",
    TRAITS7_W1 = "likert",
    TRAITS8_W1 = "likert",
    TRAITS9_W1 = "likert",
    TRAITS10_W1 = "likert",
    TRUST_PEOPLE_W1 = "likert",
    URBAN_RURAL_W1 = "likert",
    VOLUNTEERED_W1 = "bin",
    WB_FIVEYRS_W1 = "cont",
    WB_TODAY_W1 = "cont",
    WORRY_SAFETY_W1 = "cont",
    WORTHWHILE_W1 = "cont",
    ANNUAL_WEIGHT1_W1 = "(system variable)",
    STRATA_W1 = "(system variable)",
    PSU_W1 = "(system variable)",
    FULL_PARTIAL_W1 = "(system variable)",
    COUNTRY_W2 = "likert",
    WAVE_W2 = "(system variable)",
    MODE_RECRUIT_W2 = "likert",
    MODE_ANNUAL_W2 = "likert",
    RECRUIT_TYPE_W2 = "likert",
    DOI_RECRUIT_W2 = "(system variable)",
    DOI_ANNUAL_W2 = "(system variable)",
    ABUSED_W2 = "bin",
    AFTER_DEATH_W2 = "bin",
    AGE_W2 = "cont",
    APPROVE_GOVT_W2 = "likert",
    ATTEND_SVCS_W2 = "likert",
    BELIEVE_GOD_W2 = "likert",
    BELONGING_W2 = "cont",
    BODILY_PAIN_W2 = "likert",
    BORN_COUNTRY_W2 = "likert",
    CAPABLE_W2 = "likert",
    CIGARETTES_W2 = "cont",
    CLOSE_TO_W2 = "bin",
    CNTRY_REL_BUD_W2 = "cont",
    CNTRY_REL_CHI_W2 = "cont",
    CNTRY_REL_CHR_W2 = "cont",
    CNTRY_REL_HIN_W2 = "cont",
    CNTRY_REL_ISL_W2 = "cont",
    CNTRY_REL_JUD_W2 = "cont",
    CNTRY_REL_SHI_W2 = "cont",
    COMFORT_REL_W2 = "likert",
    CONNECTED_REL_W2 = "likert",
    CONTENT_W2 = "cont",
    CONTROL_WORRY_W2 = "likert",
    COVID_DEATH_W2 = "bin",
    CRITICAL_W2 = "likert",
    DAYS_EXERCISE_W2 = "likert",
    DEPRESSED_W2 = "likert",
    DISCRIMINATED_W2 = "likert",
    DONATED_W2 = "bin",
    DRINKS_W2 = "cont",
    EDUCATION_W2 = "nominal",
    EDUCATION_3_W2 = "likert",
    EMPLOYMENT_W2 = "likert",
    EXPECT_GOOD_W2 = "cont",
    EXPENSES_W2 = "cont",
    FATHER_LOVED_W2 = "bin",
    FATHER_RELATN_W2 = "likert",
    FEEL_ANXIOUS_W2 = "likert",
    FORGIVE_W2 = "likert",
    FREEDOM_W2 = "cont",
    GENDER_W2 = "likert",
    GIVE_UP_W2 = "cont",
    GOD_PUNISH_W2 = "likert",
    GRATEFUL_W2 = "cont",
    GROUP_NOT_REL_W2 = "likert",
    HAPPY_W2 = "cont",
    HEALTH_GROWUP_W2 = "likert",
    HEALTH_PROB_W2 = "bin",
    HELP_STRANGER_W2 = "bin",
    HOPE_FUTURE_W2 = "cont",
    INCOME_W2 = "nominal",
    INCOME_12YRS_W2 = "likert",
    INCOME_DIFF_W2 = "likert",
    INCOME_FEELINGS_W2 = "likert",
    INTEREST_W2 = "likert",
    LIFE_APPROACH_W2 = "likert",
    LIFE_BALANCE_W2 = "likert",
    LIFE_PURPOSE_W2 = "cont",
    LIFE_SAT_W2 = "cont",
    LONELY_W2 = "cont",
    LOVED_BY_GOD_W2 = "likert",
    MARITAL_STATUS_W2 = "likert",
    MENTAL_HEALTH_W2 = "cont",
    MOTHER_LOVED_W2 = "bin",
    MOTHER_RELATN_W2 = "likert",
    NUM_CHILDREN_W2 = "cont",
    NUM_HOUSEHOLD_W2 = "cont",
    OBEY_LAW_W2 = "likert",
    OUTSIDER_W2 = "bin",
    OWN_RENT_HOME_W2 = "likert",
    PARENTS_12YRS_W2 = "likert",
    PEACE_W2 = "likert",
    PEOPLE_HELP_W2 = "cont",
    PHYSICAL_HLTH_W2 = "cont",
    POLITICAL_ID_W2 = "cont",
    PRAY_MEDITATE_W2 = "likert",
    PROMOTE_GOOD_W2 = "cont",
    REGION1_W2 = "nominal",
    REGION2_W2 = "nominal",
    REGION3_W2 = "nominal",
    REL_EXPERIENC_W2 = "bin",
    REL_IMPORTANT_W2 = "bin",
    REL1_W2 = "likert",
    REL2_W2 = "likert",
    REL3_W2 = "likert",
    REL4_W2 = "likert",
    REL5_W2 = "likert",
    REL6_W2 = "likert",
    REL7_W2 = "likert",
    REL8_W2 = "likert",
    REL9_W2 = "likert",
    SACRED_TEXTS_W2 = "likert",
    SAT_LIVE_W2 = "likert",
    SAT_RELATNSHP_W2 = "cont",
    SAY_IN_GOVT_W2 = "likert",
    SELFID1_W2 = "nominal",
    SELFID2_W2 = "nominal",
    SHOW_LOVE_W2 = "cont",
    SUFFERING_W2 = "likert",
    SVCS_12YRS_W2 = "likert",
    SVCS_FATHER_W2 = "likert",
    SVCS_MOTHER_W2 = "likert",
    TEACHINGS_1_W2 = "cont",
    TEACHINGS_2_W2 = "cont",
    TEACHINGS_3_W2 = "cont",
    TEACHINGS_4_W2 = "cont",
    TEACHINGS_5_W2 = "cont",
    TEACHINGS_6_W2 = "cont",
    TEACHINGS_7_W2 = "cont",
    TEACHINGS_8_W2 = "cont",
    TEACHINGS_9_W2 = "cont",
    TEACHINGS_10_W2 = "cont",
    TEACHINGS_11_W2 = "cont",
    TEACHINGS_12_W2 = "cont",
    TEACHINGS_13_W2 = "cont",
    TEACHINGS_14_W2 = "cont",
    TEACHINGS_15_W2 = "cont",
    TELL_BELIEFS_W2 = "likert",
    THREAT_LIFE_W2 = "likert",
    TRAITS1_W2 = "likert",
    TRAITS2_W2 = "likert",
    TRAITS3_W2 = "likert",
    TRAITS4_W2 = "likert",
    TRAITS5_W2 = "likert",
    TRAITS6_W2 = "likert",
    TRAITS7_W2 = "likert",
    TRAITS8_W2 = "likert",
    TRAITS9_W2 = "likert",
    TRAITS10_W2 = "likert",
    TRUST_PEOPLE_W2 = "likert",
    URBAN_RURAL_W2 = "likert",
    VOLUNTEERED_W2 = "bin",
    WB_FIVEYRS_W2 = "cont",
    WB_TODAY_W2 = "cont",
    WORRY_SAFETY_W2 = "cont",
    WORTHWHILE_W2 = "cont",
    ANNUAL_WEIGHT1_W2 = "(system variable)",
    STRATA_W2 = "(system variable)",
    PSU_W2 = "(system variable)",
    FULL_PARTIAL_W2 = "(system variable)",
    AGE_GRP_W1 = "likert",
    AGE_GRP_W2 = "likert",
    MARITAL_STATUS_W2_1 = "bin",
    MARITAL_STATUS_W2_2 = "bin",
    MARITAL_STATUS_W2_3 = "bin",
    MARITAL_STATUS_W2_4 = "bin",
    MARITAL_STATUS_W2_5 = "bin",
    MARITAL_STATUS_W2_6 = "bin",
    MARITAL_STATUS_W2_7 = "bin",
    MARITAL_STATUS_EVER_MARRIED_W2 = "bin",
    MARITAL_STATUS_EVER_MARRIED_W1 = "bin",
    COMPOSITE_DEPRESSION_W2 = "cont",
    COMPOSITE_ANXIETY_W2 = "cont",
    COMPOSITE_DEP_ANX_COMBO_W2 = "cont",
    COMPOSITE_EXTRAVERSION_W2 = "cont",
    COMPOSITE_OPENNESS_W2 = "cont",
    COMPOSITE_AGREEABLENESS_W2 = "cont",
    COMPOSITE_CONSCIENTIOUSNESS_W2 = "cont",
    COMPOSITE_NEUROTICISM_W2 = "cont",
    COMPOSITE_FLOURISHING_W2 = "cont",
    COMPOSITE_FLOURISHING_SECURE_W2 = "cont",
    COMPOSITE_HAPPI_LIFE_SAT_W2 = "cont",
    COMPOSITE_HEALTH_W2 = "cont",
    COMPOSITE_MEANING_PURPOSE_W2 = "cont",
    COMPOSITE_CHARACTER_W2 = "cont",
    COMPOSITE_SUBJECTIVE_SOC_CONN_W2 = "cont",
    COMPOSITE_FINL_MAT_WORRY_W2 = "cont",
    COMPOSITE_DEPRESSION_W1 = "cont",
    COMPOSITE_ANXIETY_W1 = "cont",
    COMPOSITE_DEP_ANX_COMBO_W1 = "cont",
    COMPOSITE_EXTRAVERSION_W1 = "cont",
    COMPOSITE_OPENNESS_W1 = "cont",
    COMPOSITE_AGREEABLENESS_W1 = "cont",
    COMPOSITE_CONSCIENTIOUSNESS_W1 = "cont",
    COMPOSITE_NEUROTICISM_W1 = "cont",
    COMPOSITE_FLOURISHING_W1 = "cont",
    COMPOSITE_FLOURISHING_SECURE_W1 = "cont",
    COMPOSITE_HAPPI_LIFE_SAT_W1 = "cont",
    COMPOSITE_HEALTH_W1 = "cont",
    COMPOSITE_MEANING_PURPOSE_W1 = "cont",
    COMPOSITE_CHARACTER_W1 = "cont",
    COMPOSITE_SUBJECTIVE_SOC_CONN_W1 = "cont",
    COMPOSITE_FINL_MAT_WORRY_W1 = "cont",
    "cont"
  )
}

#' Get Outcome Missingness Codes
#'
#' A relatively simple switch function to get the missingness codes (-98, 98, 99, etc.)
#'
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a character string
#' @examples {
#'   get_missing_codes("HAPPY_W1")
#' }
#' @export
get_missing_codes <- function(var) {
  switch(var,
    ID = c(-98),
    COUNTRY_W1 = c(-98),
    WAVE_W1 = c(-98),
    MODE_RECRUIT_W1 = c(-98),
    MODE_ANNUAL_W1 = c(-98),
    RECRUIT_TYPE_W1 = c(-98),
    DOI_RECRUIT_W1 = c(-98),
    DOI_ANNUAL_W1 = c(-98),
    ABUSED_W1 = c(-98, 98, 99),
    AFTER_DEATH_W1 = c(-98, 99),
    AGE_W1 = c(-998, 998, 999),
    APPROVE_GOVT_W1 = c(-98, 98, 99),
    ATTEND_SVCS_W1 = c(-98, 98, 99),
    BELIEVE_GOD_W1 = c(-98, 99),
    BELONGING_W1 = c(-98, 98, 99),
    BODILY_PAIN_W1 = c(-98, 98, 99),
    BORN_COUNTRY_W1 = c(-98, 98, 99),
    CAPABLE_W1 = c(-98, 98, 99),
    CIGARETTES_W1 = c(-98, 98, 99),
    CLOSE_TO_W1 = c(-98, 98, 99),
    CNTRY_REL_BUD_W1 = c(-98, 98, 99),
    CNTRY_REL_CHI_W1 = c(-98, 98, 99),
    CNTRY_REL_CHR_W1 = c(-98, 98, 99),
    CNTRY_REL_HIN_W1 = c(-98, 98, 99),
    CNTRY_REL_ISL_W1 = c(-98, 98, 99),
    CNTRY_REL_JUD_W1 = c(-98, 98, 99),
    CNTRY_REL_SHI_W1 = c(-98, 98, 99),
    COMFORT_REL_W1 = c(-98, 99),
    CONNECTED_REL_W1 = c(-98, 98, 99),
    CONTENT_W1 = c(-98, 98, 99),
    CONTROL_WORRY_W1 = c(-98, 98, 99),
    COVID_DEATH_W1 = c(-98, 98, 99),
    CRITICAL_W1 = c(-98, 99),
    DAYS_EXERCISE_W1 = c(-98, 98, 99),
    DEPRESSED_W1 = c(-98, 98, 99),
    DISCRIMINATED_W1 = c(-98, 98, 99),
    DONATED_W1 = c(-98, 98, 99),
    DRINKS_W1 = c(-98, 98, 99),
    EDUCATION_W1 = c(-9998, 9998, 9999),
    EDUCATION_3_W1 = c(-98, 98, 99),
    EMPLOYMENT_W1 = c(-98, 98, 99),
    EXPECT_GOOD_W1 = c(-98, 98, 99),
    EXPENSES_W1 = c(-98, 98, 99),
    FATHER_LOVED_W1 = c(-98, 98, 99),
    FATHER_RELATN_W1 = c(-98, 98, 99),
    FEEL_ANXIOUS_W1 = c(-98, 98, 99),
    FORGIVE_W1 = c(-98, 98, 99),
    FREEDOM_W1 = c(-98, 98, 99),
    GENDER_W1 = c(-98, 98, 99),
    GIVE_UP_W1 = c(-98, 98, 99),
    GOD_PUNISH_W1 = c(-98, 99),
    GRATEFUL_W1 = c(-98, 98, 99),
    GROUP_NOT_REL_W1 = c(-98, 98, 99),
    HAPPY_W1 = c(-98, 98, 99),
    HEALTH_GROWUP_W1 = c(-98, 98, 99),
    HEALTH_PROB_W1 = c(-98, 98, 99),
    HELP_STRANGER_W1 = c(-98, 98, 99),
    HOPE_FUTURE_W1 = c(-98, 98, 99),
    INCOME_W1 = c(-9998, 9998, 9999),
    INCOME_12YRS_W1 = c(-98, 98, 99),
    INCOME_DIFF_W1 = c(-98, 98, 99),
    INCOME_FEELINGS_W1 = c(-98, 98, 99),
    INTEREST_W1 = c(-98, 98, 99),
    LIFE_APPROACH_W1 = c(-98, 99),
    LIFE_BALANCE_W1 = c(-98, 98, 99),
    LIFE_PURPOSE_W1 = c(-98, 98, 99),
    LIFE_SAT_W1 = c(-98, 98, 99),
    LONELY_W1 = c(-98, 98, 99),
    LOVED_BY_GOD_W1 = c(-98, 99),
    MARITAL_STATUS_W1 = c(-98, 98, 99),
    MENTAL_HEALTH_W1 = c(-98, 98, 99),
    MOTHER_LOVED_W1 = c(-98, 98, 99),
    MOTHER_RELATN_W1 = c(-98, 98, 99),
    NUM_CHILDREN_W1 = c(-98, 98, 99),
    NUM_HOUSEHOLD_W1 = c(-98, 98, 99),
    OBEY_LAW_W1 = c(-98, 98, 99),
    OUTSIDER_W1 = c(-98, 97, 98, 99),
    OWN_RENT_HOME_W1 = c(-98, 98, 99),
    PARENTS_12YRS_W1 = c(-98, 98, 99),
    PEACE_W1 = c(-98, 98, 99),
    PEOPLE_HELP_W1 = c(-98, 98, 99),
    PHYSICAL_HLTH_W1 = c(-98, 98, 99),
    POLITICAL_ID_W1 = c(-9998, 9998, 9999),
    PRAY_MEDITATE_W1 = c(-98, 98, 99),
    PROMOTE_GOOD_W1 = c(-98, 98, 99),
    REGION1_W1 = c(-9998, 9998, 9999),
    REGION2_W1 = c(-9998, 9998, 9999),
    REGION3_W1 = c(-9998, 9998, 9999),
    REL_EXPERIENC_W1 = c(-98, 98, 99),
    REL_IMPORTANT_W1 = c(-98, 98, 99),
    REL1_W1 = c(-98, 98, 99),
    REL2_W1 = c(-98, 98, 99),
    REL3_W1 = c(-98, 98, 99),
    REL4_W1 = c(-98, 98, 99),
    REL5_W1 = c(-98, 98, 99),
    REL6_W1 = c(-98, 98, 99),
    REL7_W1 = c(-98, 98, 99),
    REL8_W1 = c(-98, 98, 99),
    REL9_W1 = c(-98, 98, 99),
    SACRED_TEXTS_W1 = c(-98, 98, 99),
    SAT_LIVE_W1 = c(-98, 99),
    SAT_RELATNSHP_W1 = c(-98, 98, 99),
    SAY_IN_GOVT_W1 = c(-98, 99),
    SELFID1_W1 = c(-9998, 9998, 9999),
    SELFID2_W1 = c(-9998, 9998, 9999),
    SHOW_LOVE_W1 = c(-98, 98, 99),
    SUFFERING_W1 = c(-98, 98, 99),
    SVCS_12YRS_W1 = c(-98, 98, 99),
    SVCS_FATHER_W1 = c(-98, 98, 99),
    SVCS_MOTHER_W1 = c(-98, 98, 99),
    TEACHINGS_1_W1 = c(-98, 98, 99),
    TEACHINGS_2_W1 = c(-98, 98, 99),
    TEACHINGS_3_W1 = c(-98, 98, 99),
    TEACHINGS_4_W1 = c(-98, 98, 99),
    TEACHINGS_5_W1 = c(-98, 98, 99),
    TEACHINGS_6_W1 = c(-98, 98, 99),
    TEACHINGS_7_W1 = c(-98, 98, 99),
    TEACHINGS_8_W1 = c(-98, 98, 99),
    TEACHINGS_9_W1 = c(-98, 98, 99),
    TEACHINGS_10_W1 = c(-98, 98, 99),
    TEACHINGS_11_W1 = c(-98, 98, 99),
    TEACHINGS_12_W1 = c(-98, 98, 99),
    TEACHINGS_13_W1 = c(-98, 98, 99),
    TEACHINGS_14_W1 = c(-98, 98, 99),
    TEACHINGS_15_W1 = c(-98, 98, 99),
    TELL_BELIEFS_W1 = c(-98, 98, 99),
    THREAT_LIFE_W1 = c(-98, 98, 99),
    TRAITS1_W1 = c(-98, 98, 99),
    TRAITS2_W1 = c(-98, 98, 99),
    TRAITS3_W1 = c(-98, 98, 99),
    TRAITS4_W1 = c(-98, 98, 99),
    TRAITS5_W1 = c(-98, 98, 99),
    TRAITS6_W1 = c(-98, 98, 99),
    TRAITS7_W1 = c(-98, 98, 99),
    TRAITS8_W1 = c(-98, 98, 99),
    TRAITS9_W1 = c(-98, 98, 99),
    TRAITS10_W1 = c(-98, 98, 99),
    TRUST_PEOPLE_W1 = c(-98, 98, 99),
    URBAN_RURAL_W1 = c(-98, 98, 99),
    VOLUNTEERED_W1 = c(-98, 98, 99),
    WB_FIVEYRS_W1 = c(-98, 98, 99),
    WB_TODAY_W1 = c(-98, 98, 99),
    WORRY_SAFETY_W1 = c(-98, 98, 99),
    WORTHWHILE_W1 = c(-98, 98, 99),
    ANNUAL_WEIGHT1_W1 = c(-98),
    STRATA_W1 = c(-98),
    PSU_W1 = c(-98),
    FULL_PARTIAL_W1 = c(-98),
    COUNTRY_W2 = c(-98),
    WAVE_W2 = c(-98),
    MODE_RECRUIT_W2 = c(-98),
    MODE_ANNUAL_W2 = c(-98),
    RECRUIT_TYPE_W2 = c(-98),
    DOI_RECRUIT_W2 = c(-98),
    DOI_ANNUAL_W2 = c(-98),
    ABUSED_W2 = c(-98, 98, 99),
    AFTER_DEATH_W2 = c(-98, 99),
    AGE_W2 = c(-998, 998, 999),
    APPROVE_GOVT_W2 = c(-98, 98, 99),
    ATTEND_SVCS_W2 = c(-98, 98, 99),
    BELIEVE_GOD_W2 = c(-98, 99),
    BELONGING_W2 = c(-98, 98, 99),
    BODILY_PAIN_W2 = c(-98, 98, 99),
    BORN_COUNTRY_W2 = c(-98, 98, 99),
    CAPABLE_W2 = c(-98, 98, 99),
    CIGARETTES_W2 = c(-98, 98, 99),
    CLOSE_TO_W2 = c(-98, 98, 99),
    CNTRY_REL_BUD_W2 = c(-98, 98, 99),
    CNTRY_REL_CHI_W2 = c(-98, 98, 99),
    CNTRY_REL_CHR_W2 = c(-98, 98, 99),
    CNTRY_REL_HIN_W2 = c(-98, 98, 99),
    CNTRY_REL_ISL_W2 = c(-98, 98, 99),
    CNTRY_REL_JUD_W2 = c(-98, 98, 99),
    CNTRY_REL_SHI_W2 = c(-98, 98, 99),
    COMFORT_REL_W2 = c(-98, 99),
    CONNECTED_REL_W2 = c(-98, 98, 99),
    CONTENT_W2 = c(-98, 98, 99),
    CONTROL_WORRY_W2 = c(-98, 98, 99),
    COVID_DEATH_W2 = c(-98, 98, 99),
    CRITICAL_W2 = c(-98, 99),
    DAYS_EXERCISE_W2 = c(-98, 98, 99),
    DEPRESSED_W2 = c(-98, 98, 99),
    DISCRIMINATED_W2 = c(-98, 98, 99),
    DONATED_W2 = c(-98, 98, 99),
    DRINKS_W2 = c(-98, 98, 99),
    EDUCATION_W2 = c(-9998, 9998, 9999),
    EDUCATION_3_W2 = c(-98, 98, 99),
    EMPLOYMENT_W2 = c(-98, 98, 99),
    EXPECT_GOOD_W2 = c(-98, 98, 99),
    EXPENSES_W2 = c(-98, 98, 99),
    FATHER_LOVED_W2 = c(-98, 98, 99),
    FATHER_RELATN_W2 = c(-98, 98, 99),
    FEEL_ANXIOUS_W2 = c(-98, 98, 99),
    FORGIVE_W2 = c(-98, 98, 99),
    FREEDOM_W2 = c(-98, 98, 99),
    GENDER_W2 = c(-98, 98, 99),
    GIVE_UP_W2 = c(-98, 98, 99),
    GOD_PUNISH_W2 = c(-98, 99),
    GRATEFUL_W2 = c(-98, 98, 99),
    GROUP_NOT_REL_W2 = c(-98, 98, 99),
    HAPPY_W2 = c(-98, 98, 99),
    HEALTH_GROWUP_W2 = c(-98, 98, 99),
    HEALTH_PROB_W2 = c(-98, 98, 99),
    HELP_STRANGER_W2 = c(-98, 98, 99),
    HOPE_FUTURE_W2 = c(-98, 98, 99),
    INCOME_W2 = c(-9998, 9998, 9999),
    INCOME_12YRS_W2 = c(-98, 98, 99),
    INCOME_DIFF_W2 = c(-98, 98, 99),
    INCOME_FEELINGS_W2 = c(-98, 98, 99),
    INTEREST_W2 = c(-98, 98, 99),
    LIFE_APPROACH_W2 = c(-98, 99),
    LIFE_BALANCE_W2 = c(-98, 98, 99),
    LIFE_PURPOSE_W2 = c(-98, 98, 99),
    LIFE_SAT_W2 = c(-98, 98, 99),
    LONELY_W2 = c(-98, 98, 99),
    LOVED_BY_GOD_W2 = c(-98, 99),
    MARITAL_STATUS_W2 = c(-98, 98, 99),
    MENTAL_HEALTH_W2 = c(-98, 98, 99),
    MOTHER_LOVED_W2 = c(-98, 98, 99),
    MOTHER_RELATN_W2 = c(-98, 98, 99),
    NUM_CHILDREN_W2 = c(-98, 98, 99),
    NUM_HOUSEHOLD_W2 = c(-98, 98, 99),
    OBEY_LAW_W2 = c(-98, 98, 99),
    OUTSIDER_W2 = c(-98, 97, 98, 99),
    OWN_RENT_HOME_W2 = c(-98, 98, 99),
    PARENTS_12YRS_W2 = c(-98, 98, 99),
    PEACE_W2 = c(-98, 98, 99),
    PEOPLE_HELP_W2 = c(-98, 98, 99),
    PHYSICAL_HLTH_W2 = c(-98, 98, 99),
    POLITICAL_ID_W2 = c(-9998, 9998, 9999),
    PRAY_MEDITATE_W2 = c(-98, 98, 99),
    PROMOTE_GOOD_W2 = c(-98, 98, 99),
    REGION1_W2 = c(-9998, 9998, 9999),
    REGION2_W2 = c(-9998, 9998, 9999),
    REGION3_W2 = c(-9998, 9998, 9999),
    REL_EXPERIENC_W2 = c(-98, 98, 99),
    REL_IMPORTANT_W2 = c(-98, 98, 99),
    REL1_W2 = c(-98, 98, 99),
    REL2_W2 = c(-98, 98, 99),
    REL3_W2 = c(-98, 98, 99),
    REL4_W2 = c(-98, 98, 99),
    REL5_W2 = c(-98, 98, 99),
    REL6_W2 = c(-98, 98, 99),
    REL7_W2 = c(-98, 98, 99),
    REL8_W2 = c(-98, 98, 99),
    REL9_W2 = c(-98, 98, 99),
    SACRED_TEXTS_W2 = c(-98, 98, 99),
    SAT_LIVE_W2 = c(-98, 99),
    SAT_RELATNSHP_W2 = c(-98, 98, 99),
    SAY_IN_GOVT_W2 = c(-98, 99),
    SELFID1_W2 = c(-9998, 9998, 9999),
    SELFID2_W2 = c(-9998, 9998, 9999),
    SHOW_LOVE_W2 = c(-98, 98, 99),
    SUFFERING_W2 = c(-98, 98, 99),
    SVCS_12YRS_W2 = c(-98, 98, 99),
    SVCS_FATHER_W2 = c(-98, 98, 99),
    SVCS_MOTHER_W2 = c(-98, 98, 99),
    TEACHINGS_1_W2 = c(-98, 98, 99),
    TEACHINGS_2_W2 = c(-98, 98, 99),
    TEACHINGS_3_W2 = c(-98, 98, 99),
    TEACHINGS_4_W2 = c(-98, 98, 99),
    TEACHINGS_5_W2 = c(-98, 98, 99),
    TEACHINGS_6_W2 = c(-98, 98, 99),
    TEACHINGS_7_W2 = c(-98, 98, 99),
    TEACHINGS_8_W2 = c(-98, 98, 99),
    TEACHINGS_9_W2 = c(-98, 98, 99),
    TEACHINGS_10_W2 = c(-98, 98, 99),
    TEACHINGS_11_W2 = c(-98, 98, 99),
    TEACHINGS_12_W2 = c(-98, 98, 99),
    TEACHINGS_13_W2 = c(-98, 98, 99),
    TEACHINGS_14_W2 = c(-98, 98, 99),
    TEACHINGS_15_W2 = c(-98, 98, 99),
    TELL_BELIEFS_W2 = c(-98, 98, 99),
    THREAT_LIFE_W2 = c(-98, 98, 99),
    TRAITS1_W2 = c(-98, 98, 99),
    TRAITS2_W2 = c(-98, 98, 99),
    TRAITS3_W2 = c(-98, 98, 99),
    TRAITS4_W2 = c(-98, 98, 99),
    TRAITS5_W2 = c(-98, 98, 99),
    TRAITS6_W2 = c(-98, 98, 99),
    TRAITS7_W2 = c(-98, 98, 99),
    TRAITS8_W2 = c(-98, 98, 99),
    TRAITS9_W2 = c(-98, 98, 99),
    TRAITS10_W2 = c(-98, 98, 99),
    TRUST_PEOPLE_W2 = c(-98, 98, 99),
    URBAN_RURAL_W2 = c(-98, 98, 99),
    VOLUNTEERED_W2 = c(-98, 98, 99),
    WB_FIVEYRS_W2 = c(-98, 98, 99),
    WB_TODAY_W2 = c(-98, 98, 99),
    WORRY_SAFETY_W2 = c(-98, 98, 99),
    WORTHWHILE_W2 = c(-98, 98, 99),
    ANNUAL_WEIGHT1_W2 = c(-98),
    STRATA_W2 = c(-98),
    PSU_W2 = c(-98),
    FULL_PARTIAL_W2 = c(-98),
  )
}

#' Recode Variable to Type
#'
#' A relatively simple switch function to transform variables to factors, numeric, etc.
#'
#' @param x a vector
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a vector
#' @examples {
#'   # to-do
#' }
#' @export
recode_to_type <- function(x, var) {
  switch(var,
    ID = c(x),
    COUNTRY_W1 = factor(x),
    WAVE_W1 = as.numeric(x),
    MODE_RECRUIT_W1 = factor(x),
    MODE_ANNUAL_W1 = factor(x),
    RECRUIT_TYPE_W1 = factor(x),
    DOI_RECRUIT_W1 = as.Date(x),
    DOI_ANNUAL_W1 = as.Date(x),
    ABUSED_W1 = factor(x),
    AFTER_DEATH_W1 = factor(x),
    AGE_W1 = as.numeric(x),
    APPROVE_GOVT_W1 = factor(x),
    ATTEND_SVCS_W1 = factor(x),
    BELIEVE_GOD_W1 = factor(x),
    BELONGING_W1 = as.numeric(x),
    BODILY_PAIN_W1 = factor(x),
    BORN_COUNTRY_W1 = factor(x),
    CAPABLE_W1 = factor(x),
    CIGARETTES_W1 = as.numeric(x),
    CLOSE_TO_W1 = factor(x),
    CNTRY_REL_BUD_W1 = as.numeric(x),
    CNTRY_REL_CHI_W1 = as.numeric(x),
    CNTRY_REL_CHR_W1 = as.numeric(x),
    CNTRY_REL_HIN_W1 = as.numeric(x),
    CNTRY_REL_ISL_W1 = as.numeric(x),
    CNTRY_REL_JUD_W1 = as.numeric(x),
    CNTRY_REL_SHI_W1 = as.numeric(x),
    COMFORT_REL_W1 = factor(x),
    CONNECTED_REL_W1 = factor(x),
    CONTENT_W1 = as.numeric(x),
    CONTROL_WORRY_W1 = factor(x),
    COVID_DEATH_W1 = factor(x),
    CRITICAL_W1 = factor(x),
    DAYS_EXERCISE_W1 = factor(x),
    DEPRESSED_W1 = factor(x),
    DISCRIMINATED_W1 = factor(x),
    DONATED_W1 = factor(x),
    DRINKS_W1 = as.numeric(x),
    EDUCATION_W1 = factor(x),
    EDUCATION_3_W1 = factor(x),
    EMPLOYMENT_W1 = factor(x),
    EXPECT_GOOD_W1 = as.numeric(x),
    EXPENSES_W1 = as.numeric(x),
    FATHER_LOVED_W1 = factor(x),
    FATHER_RELATN_W1 = factor(x),
    FEEL_ANXIOUS_W1 = factor(x),
    FORGIVE_W1 = factor(x),
    FREEDOM_W1 = as.numeric(x),
    GENDER_W1 = factor(x),
    GIVE_UP_W1 = as.numeric(x),
    GOD_PUNISH_W1 = factor(x),
    GRATEFUL_W1 = as.numeric(x),
    GROUP_NOT_REL_W1 = factor(x),
    HAPPY_W1 = as.numeric(x),
    HEALTH_GROWUP_W1 = factor(x),
    HEALTH_PROB_W1 = factor(x),
    HELP_STRANGER_W1 = factor(x),
    HOPE_FUTURE_W1 = as.numeric(x),
    INCOME_W1 = factor(x),
    INCOME_12YRS_W1 = factor(x),
    INCOME_DIFF_W1 = factor(x),
    INCOME_FEELINGS_W1 = factor(x),
    INTEREST_W1 = factor(x),
    LIFE_APPROACH_W1 = factor(x),
    LIFE_BALANCE_W1 = factor(x),
    LIFE_PURPOSE_W1 = as.numeric(x),
    LIFE_SAT_W1 = as.numeric(x),
    LONELY_W1 = as.numeric(x),
    LOVED_BY_GOD_W1 = factor(x),
    MARITAL_STATUS_W1 = factor(x),
    MENTAL_HEALTH_W1 = as.numeric(x),
    MOTHER_LOVED_W1 = factor(x),
    MOTHER_RELATN_W1 = factor(x),
    NUM_CHILDREN_W1 = as.numeric(x),
    NUM_HOUSEHOLD_W1 = as.numeric(x),
    OBEY_LAW_W1 = factor(x),
    OUTSIDER_W1 = factor(x),
    OWN_RENT_HOME_W1 = factor(x),
    PARENTS_12YRS_W1 = factor(x),
    PEACE_W1 = factor(x),
    PEOPLE_HELP_W1 = as.numeric(x),
    PHYSICAL_HLTH_W1 = as.numeric(x),
    POLITICAL_ID_W1 = as.numeric(x),
    PRAY_MEDITATE_W1 = factor(x),
    PROMOTE_GOOD_W1 = as.numeric(x),
    REGION1_W1 = factor(x),
    REGION2_W1 = factor(x),
    REGION3_W1 = factor(x),
    REL_EXPERIENC_W1 = factor(x),
    REL_IMPORTANT_W1 = factor(x),
    REL1_W1 = factor(x),
    REL2_W1 = factor(x),
    REL3_W1 = factor(x),
    REL4_W1 = factor(x),
    REL5_W1 = factor(x),
    REL6_W1 = factor(x),
    REL7_W1 = factor(x),
    REL8_W1 = factor(x),
    REL9_W1 = factor(x),
    SACRED_TEXTS_W1 = factor(x),
    SAT_LIVE_W1 = factor(x),
    SAT_RELATNSHP_W1 = as.numeric(x),
    SAY_IN_GOVT_W1 = factor(x),
    SELFID1_W1 = factor(x),
    SELFID2_W1 = factor(x),
    SHOW_LOVE_W1 = as.numeric(x),
    SUFFERING_W1 = factor(x),
    SVCS_12YRS_W1 = factor(x),
    SVCS_FATHER_W1 = factor(x),
    SVCS_MOTHER_W1 = factor(x),
    TEACHINGS_1_W1 = as.numeric(x),
    TEACHINGS_2_W1 = as.numeric(x),
    TEACHINGS_3_W1 = as.numeric(x),
    TEACHINGS_4_W1 = as.numeric(x),
    TEACHINGS_5_W1 = as.numeric(x),
    TEACHINGS_6_W1 = as.numeric(x),
    TEACHINGS_7_W1 = as.numeric(x),
    TEACHINGS_8_W1 = as.numeric(x),
    TEACHINGS_9_W1 = as.numeric(x),
    TEACHINGS_10_W1 = as.numeric(x),
    TEACHINGS_11_W1 = as.numeric(x),
    TEACHINGS_12_W1 = as.numeric(x),
    TEACHINGS_13_W1 = as.numeric(x),
    TEACHINGS_14_W1 = as.numeric(x),
    TEACHINGS_15_W1 = as.numeric(x),
    TELL_BELIEFS_W1 = factor(x),
    THREAT_LIFE_W1 = factor(x),
    TRAITS1_W1 = factor(x),
    TRAITS2_W1 = factor(x),
    TRAITS3_W1 = factor(x),
    TRAITS4_W1 = factor(x),
    TRAITS5_W1 = factor(x),
    TRAITS6_W1 = factor(x),
    TRAITS7_W1 = factor(x),
    TRAITS8_W1 = factor(x),
    TRAITS9_W1 = factor(x),
    TRAITS10_W1 = factor(x),
    TRUST_PEOPLE_W1 = factor(x),
    URBAN_RURAL_W1 = factor(x),
    VOLUNTEERED_W1 = factor(x),
    WB_FIVEYRS_W1 = as.numeric(x),
    WB_TODAY_W1 = as.numeric(x),
    WORRY_SAFETY_W1 = as.numeric(x),
    WORTHWHILE_W1 = as.numeric(x),
    ANNUAL_WEIGHT1_W1 = c(x),
    STRATA_W1 = c(x),
    PSU_W1 = c(x),
    FULL_PARTIAL_W1 = factor(x),
    COUNTRY_W2 = factor(x),
    WAVE_W2 = as.numeric(x),
    MODE_RECRUIT_W2 = factor(x),
    MODE_ANNUAL_W2 = factor(x),
    RECRUIT_TYPE_W2 = factor(x),
    DOI_RECRUIT_W2 = as.Date(x),
    DOI_ANNUAL_W2 = as.Date(x),
    ABUSED_W2 = factor(x),
    AFTER_DEATH_W2 = factor(x),
    AGE_W2 = as.numeric(x),
    APPROVE_GOVT_W2 = factor(x),
    ATTEND_SVCS_W2 = factor(x),
    BELIEVE_GOD_W2 = factor(x),
    BELONGING_W2 = as.numeric(x),
    BODILY_PAIN_W2 = factor(x),
    BORN_COUNTRY_W2 = factor(x),
    CAPABLE_W2 = factor(x),
    CIGARETTES_W2 = as.numeric(x),
    CLOSE_TO_W2 = factor(x),
    CNTRY_REL_BUD_W2 = as.numeric(x),
    CNTRY_REL_CHI_W2 = as.numeric(x),
    CNTRY_REL_CHR_W2 = as.numeric(x),
    CNTRY_REL_HIN_W2 = as.numeric(x),
    CNTRY_REL_ISL_W2 = as.numeric(x),
    CNTRY_REL_JUD_W2 = as.numeric(x),
    CNTRY_REL_SHI_W2 = as.numeric(x),
    COMFORT_REL_W2 = factor(x),
    CONNECTED_REL_W2 = factor(x),
    CONTENT_W2 = as.numeric(x),
    CONTROL_WORRY_W2 = factor(x),
    COVID_DEATH_W2 = factor(x),
    CRITICAL_W2 = factor(x),
    DAYS_EXERCISE_W2 = factor(x),
    DEPRESSED_W2 = factor(x),
    DISCRIMINATED_W2 = factor(x),
    DONATED_W2 = factor(x),
    DRINKS_W2 = as.numeric(x),
    EDUCATION_W2 = factor(x),
    EDUCATION_3_W2 = factor(x),
    EMPLOYMENT_W2 = factor(x),
    EXPECT_GOOD_W2 = as.numeric(x),
    EXPENSES_W2 = as.numeric(x),
    FATHER_LOVED_W2 = factor(x),
    FATHER_RELATN_W2 = factor(x),
    FEEL_ANXIOUS_W2 = factor(x),
    FORGIVE_W2 = factor(x),
    FREEDOM_W2 = as.numeric(x),
    GENDER_W2 = factor(x),
    GIVE_UP_W2 = as.numeric(x),
    GOD_PUNISH_W2 = factor(x),
    GRATEFUL_W2 = as.numeric(x),
    GROUP_NOT_REL_W2 = factor(x),
    HAPPY_W2 = as.numeric(x),
    HEALTH_GROWUP_W2 = factor(x),
    HEALTH_PROB_W2 = factor(x),
    HELP_STRANGER_W2 = factor(x),
    HOPE_FUTURE_W2 = as.numeric(x),
    INCOME_W2 = factor(x),
    INCOME_12YRS_W2 = factor(x),
    INCOME_DIFF_W2 = factor(x),
    INCOME_FEELINGS_W2 = factor(x),
    INTEREST_W2 = factor(x),
    LIFE_APPROACH_W2 = factor(x),
    LIFE_BALANCE_W2 = factor(x),
    LIFE_PURPOSE_W2 = as.numeric(x),
    LIFE_SAT_W2 = as.numeric(x),
    LONELY_W2 = as.numeric(x),
    LOVED_BY_GOD_W2 = factor(x),
    MARITAL_STATUS_W2 = factor(x),
    MENTAL_HEALTH_W2 = as.numeric(x),
    MOTHER_LOVED_W2 = factor(x),
    MOTHER_RELATN_W2 = factor(x),
    NUM_CHILDREN_W2 = as.numeric(x),
    NUM_HOUSEHOLD_W2 = as.numeric(x),
    OBEY_LAW_W2 = factor(x),
    OUTSIDER_W2 = factor(x),
    OWN_RENT_HOME_W2 = factor(x),
    PARENTS_12YRS_W2 = factor(x),
    PEACE_W2 = factor(x),
    PEOPLE_HELP_W2 = as.numeric(x),
    PHYSICAL_HLTH_W2 = as.numeric(x),
    POLITICAL_ID_W2 = as.numeric(x),
    PRAY_MEDITATE_W2 = factor(x),
    PROMOTE_GOOD_W2 = as.numeric(x),
    REGION1_W2 = factor(x),
    REGION2_W2 = factor(x),
    REGION3_W2 = factor(x),
    REL_EXPERIENC_W2 = factor(x),
    REL_IMPORTANT_W2 = factor(x),
    REL1_W2 = factor(x),
    REL2_W2 = factor(x),
    REL3_W2 = factor(x),
    REL4_W2 = factor(x),
    REL5_W2 = factor(x),
    REL6_W2 = factor(x),
    REL7_W2 = factor(x),
    REL8_W2 = factor(x),
    REL9_W2 = factor(x),
    SACRED_TEXTS_W2 = factor(x),
    SAT_LIVE_W2 = factor(x),
    SAT_RELATNSHP_W2 = as.numeric(x),
    SAY_IN_GOVT_W2 = factor(x),
    SELFID1_W2 = factor(x),
    SELFID2_W2 = factor(x),
    SHOW_LOVE_W2 = as.numeric(x),
    SUFFERING_W2 = factor(x),
    SVCS_12YRS_W2 = factor(x),
    SVCS_FATHER_W2 = factor(x),
    SVCS_MOTHER_W2 = factor(x),
    TEACHINGS_1_W2 = as.numeric(x),
    TEACHINGS_2_W2 = as.numeric(x),
    TEACHINGS_3_W2 = as.numeric(x),
    TEACHINGS_4_W2 = as.numeric(x),
    TEACHINGS_5_W2 = as.numeric(x),
    TEACHINGS_6_W2 = as.numeric(x),
    TEACHINGS_7_W2 = as.numeric(x),
    TEACHINGS_8_W2 = as.numeric(x),
    TEACHINGS_9_W2 = as.numeric(x),
    TEACHINGS_10_W2 = as.numeric(x),
    TEACHINGS_11_W2 = as.numeric(x),
    TEACHINGS_12_W2 = as.numeric(x),
    TEACHINGS_13_W2 = as.numeric(x),
    TEACHINGS_14_W2 = as.numeric(x),
    TEACHINGS_15_W2 = as.numeric(x),
    TELL_BELIEFS_W2 = factor(x),
    THREAT_LIFE_W2 = factor(x),
    TRAITS1_W2 = factor(x),
    TRAITS2_W2 = factor(x),
    TRAITS3_W2 = factor(x),
    TRAITS4_W2 = factor(x),
    TRAITS5_W2 = factor(x),
    TRAITS6_W2 = factor(x),
    TRAITS7_W2 = factor(x),
    TRAITS8_W2 = factor(x),
    TRAITS9_W2 = factor(x),
    TRAITS10_W2 = factor(x),
    TRUST_PEOPLE_W2 = factor(x),
    URBAN_RURAL_W2 = factor(x),
    VOLUNTEERED_W2 = factor(x),
    WB_FIVEYRS_W2 = as.numeric(x),
    WB_TODAY_W2 = as.numeric(x),
    WORRY_SAFETY_W2 = as.numeric(x),
    WORTHWHILE_W2 = as.numeric(x),
    ANNUAL_WEIGHT1_W2 = c(x),
    STRATA_W2 = c(x),
    PSU_W2 = c(x),
    FULL_PARTIAL_W2 = factor(x),
    AGE_GRP_W1 = factor(x),
    AGE_GRP_W2 = factor(x),
  )
}

#' Recode Variable Labels
#'
#' A relatively simple switch function to transform variables to from the numeric values
#' to the labels associated with those levels.
#'
#' @param x a vector
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a vector
#' @examples {
#'   # TO-DO
#' }
#' @export
recode_labels <- function(x, var) {
  switch(var,
    ID = x,
    COUNTRY_W1 = case_when(x == 1 ~ "1. Argentina", x == 2 ~ "2. Australia", x == 3 ~ "3.
Brazil", x == 4 ~ "4. Egypt", x == 5 ~ "5. Germany", x == 6 ~ "6. India", x
    == 7 ~ "7. Indonesia", x == 8 ~ "8. Israel", x == 9 ~ "9. Japan", x == 10 ~
      "10. Kenya", x == 11 ~ "11. Mexico", x == 12 ~ "12. Nigeria", x == 13 ~ "13.
Philippines", x == 14 ~ "14. Poland", x == 16 ~ "16. South Africa", x == 17 ~
      "17. Spain", x == 18 ~ "18. Tanzania", x == 19 ~ "19. Turkey", x == 20 ~ "20.
United Kingdom", x == 22 ~ "22. United States", x == 23 ~ "23. Sweden", x == 24
    ~ "24. Hong Kong", .default = "(Missing)"),
    WAVE_W1 = x,
    MODE_RECRUIT_W1 = case_when(x == 1 ~ "1. CAPI", x == 2 ~ "2. CATI", x == 3 ~ "3. CAWI",
      .default =
        "(Missing)"
    ),
    MODE_ANNUAL_W1 = case_when(x == 1 ~ "1. CAPI", x == 2 ~ "2. CATI", x == 3 ~ "3. CAWI",
      .default =
        "(Missing)"
    ),
    RECRUIT_TYPE_W1 = case_when(x == 1 ~ "1. Separate Recruitment and Annual Surveys", x == 2 ~ "2.
Combined Recruitment and Annual Surveys", .default = "(Missing)"),
    DOI_RECRUIT_W1 = x,
    DOI_ANNUAL_W1 = x,
    ABUSED_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    AFTER_DEATH_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 3 ~ "3. Unsure",
      .default =
        "(Missing)"
    ),
    AGE_W1 = x,
    APPROVE_GOVT_W1 = case_when(x == 1 ~ "1. Strongly approve", x == 2 ~ "2. Somewhat approve", x == 3
    ~ "3. Neither approve nor disapprove", x == 4 ~ "4. Somewhat disapprove", x == 5
    ~ "5. Strongly disapprove", .default = "(Missing)"),
    ATTEND_SVCS_W1 = case_when(x == 1 ~ "1. More than once a week", x == 2 ~ "2. Once a week", x ==
      3 ~ "3. One to three times a month", x == 4 ~ "4. A few times a year", x == 5 ~
      "5. Never", .default = "(Missing)"),
    BELIEVE_GOD_W1 = case_when(x == 1 ~ "1. One God", x == 2 ~ "2. More than one god", x == 3 ~
      "3. An impersonal spiritual force", x == 4 ~ "4. None of these", x == 5 ~ "5.
Unsure", .default = "(Missing)"),
    BELONGING_W1 = x,
    BODILY_PAIN_W1 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. None at all",
      .default = "(Missing)"
    ),
    BORN_COUNTRY_W1 = case_when(x == 1 ~ "1. Born in this country", x == 2 ~ "2. Born in another
country", .default = "(Missing)"),
    CAPABLE_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    CIGARETTES_W1 = x,
    CLOSE_TO_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    CNTRY_REL_BUD_W1 = x,
    CNTRY_REL_CHI_W1 = x,
    CNTRY_REL_CHR_W1 = x,
    CNTRY_REL_HIN_W1 = x,
    CNTRY_REL_ISL_W1 = x,
    CNTRY_REL_JUD_W1 = x,
    CNTRY_REL_SHI_W1 = x,
    COMFORT_REL_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    CONNECTED_REL_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    CONTENT_W1 = x,
    CONTROL_WORRY_W1 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    COVID_DEATH_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    CRITICAL_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    DAYS_EXERCISE_W1 = case_when(x == 0 ~ "0. 0 days", x == 1 ~ "1. 1 day", x == 2 ~ "2. 2 days", x
    == 3 ~ "3. 3 days", x == 4 ~ "4. 4 days", x == 5 ~ "5. 5 days", x == 6 ~ "6. 6
days", x == 7 ~ "7. 7 days/Every day", .default = "(Missing)"),
    DEPRESSED_W1 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    DISCRIMINATED_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    DONATED_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    DRINKS_W1 = x,
    EDUCATION_W1 = case_when(x == 100 ~ "100. Argentina: (No formal education)", x == 101 ~ "101.
Argentina: Incomplete primary school", x == 102 ~ "102. Argentina: Complete
primary school", x == 103 ~ "103. Argentina: Incomplete secondary school", x ==
      104 ~ "104. Argentina: Complete secondary school", x == 105 ~ "105. Argentina:
Incomplete tertiary school", x == 106 ~ "106. Argentina: Complete tertiary
school", x == 107 ~ "107. Argentina: Incomplete university", x == 108 ~ "108.
Argentina: Complete university", x == 109 ~ "109. Argentina: Post-graduate", x
    == 200 ~ "200. Australia: (No formal education)", x == 201 ~ "201. Australia:
Year 8 or below (or equivalent) (usually 14 years old)", x == 202 ~ "202.
Australia: Year 9 (or equivalent) (usually 15 years old)", x == 203 ~ "203.
Australia: Year 10 (School Certificate SC or equivalent) (usually 16 years
old)", x == 204 ~ "204. Australia: Year 11 (or equivalent) (usually 17 years
old)", x == 205 ~ "205. Australia: Year 12 (Higher School Certificate HSC or
equivalent) (usually 18 years old)", x == 206 ~ "206. Australia: Certificate
(less than 1 year in TAFE or equivalent)", x == 207 ~ "207. Australia: Advanced
Diploma or Diploma (1 year or 2 year Diploma from TAFE or equivalent)", x
    == 208 ~ "208. Australia: Bachelor Degree (University Graduate)", x == 209
    ~ "209. Australia: Graduate Diploma or Graduate Certificate", x == 210 ~
      "210. Australia: Postgraduate Degree (Masters or Ph.D.)", x == 300 ~ "300.
Brazil: (Illiterate/No formal education)", x == 301 ~ "301. Brazil: 1 to 5
years incomplete/Elementary incomplete", x == 302 ~ "302. Brazil: Elementary
complete/Secondary incomplete", x == 303 ~ "303. Brazil: Secondary complete/High
school incomplete", x == 304 ~ "304. Brazil: High school complete/Media
education complete", x == 305 ~ "305. Brazil: Superior studies incomplete", x
    == 306 ~ "306. Brazil: Superior studies complete", x == 400 ~ "400. Egypt: (No
formal education)", x == 401 ~ "401. Egypt: Did not complete primary", x ==
      402 ~ "402. Egypt: Completed primary", x == 403 ~ "403. Egypt: Did not complete
intermediate", x == 404 ~ "404. Egypt: Completed intermediate", x == 405 ~
      "405. Egypt: Did not complete secondary", x == 406 ~ "406. Egypt: Completed
secondary", x == 407 ~ "407. Egypt: College/Did not complete university", x
    == 408 ~ "408. Egypt: Completed university", x == 409 ~ "409. Egypt: Higher
education (Masters degree, Ph.D., etc.)", x == 500 ~ "500. Germany: (No formal
education/training)", x == 501 ~ "501. Germany: Incomplete primary school", x
    == 502 ~ "502. Germany: Primary school (ISCED 1)", x == 503 ~ "503. Germany:
Some secondary school, not completed", x == 504 ~ "504. Germany: General
secondary school (ISCED 2)", x == 505 ~ "505. Germany: Secondary modern
school/Intermediate certificate (ISCED 3)", x == 506 ~ "506. Germany: Vocational
school (ISCED 4)", x == 507 ~ "507. Germany: High school diploma/A-levels, age
11-18/19, grades 5-12/13 - qualifies for college/university", x == 508 ~ "508.
Germany: College/university, university of applied science", x == 601 ~ "601.
India: (Illiterate)", x == 602 ~ "602. India: Below SSC", x == 603 ~ "603.
India: SSC/HSC", x == 604 ~ "604. India: Some college but did not graduate", x
    == 605 ~ "605. India: Graduate/Post Graduate - General", x == 606 ~ "606. India:
Graduate/Post Graduate - Professional", x == 700 ~ "700. Indonesia: (No formal
education)", x == 701 ~ "701. Indonesia: Primary School", x == 702 ~ "702.
Indonesia: Junior High School", x == 703 ~ "703. Indonesia: Senior School", x
    == 704 ~ "704. Indonesia: Diploma", x == 705 ~ "705. Indonesia: University", x
    == 706 ~ "706. Indonesia: Did not attend/complete primary school", x == 800 ~
      "800. Israel: (No formal education)", x == 801 ~ "801. Israel: Up to 8 years",
    x == 802 ~ "802. Israel: 9-11 years", x == 803 ~ "803. Israel: 12 years without
matriculation", x == 804 ~ "804. Israel: 12 years with matriculation", x == 805
    ~ "805. Israel: 13-14 years (non-academic, like technician, practical engineer,
nurse)", x == 806 ~ "806. Israel: 15-16 years (first degree, such as BA, BSC)",
    x == 807 ~ "807. Israel: 17+ years (second degree, such as MA, MSc)", x == 808 ~
      "808. Israel: Ph.D.", x == 901 ~ "901. Japan: (No education)", x == 902 ~ "902.
Japan: Elementary or Junior High", x == 903 ~ "903. Japan: Middle or Senior
High", x == 904 ~ "904. Japan: Higher prof or Junior college", x == 905 ~ "905.
Japan: College or University", x == 906 ~ "906. Japan: Graduate course", x ==
      1000 ~ "1000. Kenya: (No formal education)", x == 1001 ~ "1001. Kenya: First
year of primary education", x == 1002 ~ "1002. Kenya: Second year of primary
education", x == 1003 ~ "1003. Kenya: Third year of primary education", x ==
      1004 ~ "1004. Kenya: Fourth year of primary education", x == 1005 ~ "1005.
Kenya: Fifth year of primary education", x == 1006 ~ "1006. Kenya: Sixth
year of primary education", x == 1007 ~ "1007. Kenya: Seventh year of primary
education", x == 1008 ~ "1008. Kenya: Eighth year of primary education", x ==
      1009 ~ "1009. Kenya: First year of secondary education", x == 1010 ~ "1010.
Kenya: Second year of secondary education", x == 1011 ~ "1011. Kenya: Third
year of secondary education", x == 1012 ~ "1012. Kenya: Fourth year of secondary
education", x == 1013 ~ "1013. Kenya: Fifth year of secondary education", x
    == 1014 ~ "1014. Kenya: Sixth year of secondary education", x == 1015 ~ "1015.
Kenya: First year of technical training (Polytechnic or Diploma colleges)",
    x == 1016 ~ "1016. Kenya: Second year of technical training (Polytechnic or
Diploma colleges)", x == 1017 ~ "1017. Kenya: Third year of technical training
(Polytechnic or Diploma colleges)", x == 1018 ~ "1018. Kenya: First year of
teachers training college", x == 1019 ~ "1019. Kenya: Second year of teachers
training college", x == 1020 ~ "1020. Kenya: Completed education at teachers
training college", x == 1021 ~ "1021. Kenya: Some university education", x
    == 1022 ~ "1022. Kenya: Completed university education", x == 1023 ~ "1023.
Kenya: Completed post-university education", x == 1100 ~ "1100. Mexico: (No
formal education)", x == 1101 ~ "1101. Mexico: Some primary", x == 1102 ~
      "1102. Mexico: Primary complete", x == 1103 ~ "1103. Mexico: Some secondary",
    x == 1104 ~ "1104. Mexico: Secondary complete", x == 1105 ~ "1105. Mexico: High
school/technical career complete", x == 1106 ~ "1106. Mexico: Some University",
    x == 1107 ~ "1107. Mexico: University complete", x == 1108 ~ "1108. Mexico:
Post-graduate", x == 1200 ~ "1200. Nigeria: (No formal education)", x ==
      1201 ~ "1201. Nigeria: First year of primary education", x == 1202 ~ "1202.
Nigeria: Second year of primary education", x == 1203 ~ "1203. Nigeria: Third
year of primary education", x == 1204 ~ "1204. Nigeria: Fourth year of primary
education", x == 1205 ~ "1205. Nigeria: Fifth year of primary education", x
    == 1206 ~ "1206. Nigeria: Sixth year of primary education", x == 1207 ~ "1207.
Nigeria: First year of secondary education", x == 1208 ~ "1208. Nigeria:
Second year of secondary education", x == 1209 ~ "1209. Nigeria: Third year
of secondary education", x == 1210 ~ "1210. Nigeria: Fourth year of secondary
education", x == 1211 ~ "1211. Nigeria: Fifth year of secondary education",
    x == 1212 ~ "1212. Nigeria: Sixth year of secondary education", x == 1213 ~
      "1213. Nigeria: First year of technical training", x == 1214 ~ "1214. Nigeria:
Second year of technical training", x == 1215 ~ "1215. Nigeria: Third year of
technical training", x == 1216 ~ "1216. Nigeria: First year of teacher training
college", x == 1217 ~ "1217. Nigeria: Second year of teacher training college",
    x == 1218 ~ "1218. Nigeria: Third year of teacher training college", x == 1219
    ~ "1219. Nigeria: Fourth year of teacher training college", x == 1220 ~ "1220.
Nigeria: Some education at a Technical College or the Institute of Teachers
Education", x == 1221 ~ "1221. Nigeria: Some tertiary education, including
Universities, Polytechnics, and Teacher Training College", x == 1222 ~ "1222.
Nigeria: Post University education", x == 1301 ~ "1301. Philippines: (None/Did
not undergo formal schooling)", x == 1302 ~ "1302. Philippines: Preschool", x
    == 1303 ~ "1303. Philippines: Elementary", x == 1304 ~ "1304. Philippines: High
school", x == 1305 ~ "1305. Philippines: Postsecondary", x == 1306 ~ "1306.
Philippines: College undergraduate", x == 1307 ~ "1307. Philippines: Academic
degree holder", x == 1308 ~ "1308. Philippines: Post-baccalaureate", x == 1400
    ~ "1400. Poland: (No formal education)", x == 1401 ~ "1401. Poland: Less than
4 classes elementary education", x == 1402 ~ "1402. Poland: At least 4 classes
elementary education", x == 1403 ~ "1403. Poland: Completed elementary school,
6 or 8 classes", x == 1404 ~ "1404. Poland: Gimnazium", x == 1405 ~ "1405.
Poland: Basic Vocational", x == 1406 ~ "1406. Poland: Secondary degree, Liceum
Technikum", x == 1407 ~ "1407. Poland: Post-secondary", x == 1408 ~ "1408.
Poland: High diploma (university) with BA, MA, Engineer, or other equivalent
title", x == 1409 ~ "1409. Poland: Doctoral degree or higher", x == 1600 ~
      "1600. South Africa: (No formal education)", x == 1601 ~ "1601. South Africa:
First year of primary education", x == 1602 ~ "1602. South Africa: Second
year of primary education", x == 1603 ~ "1603. South Africa: Third year of
primary education", x == 1604 ~ "1604. South Africa: Fourth year of primary
education", x == 1605 ~ "1605. South Africa: Fifth year of primary education",
    x == 1606 ~ "1606. South Africa: Sixth year of primary education", x == 1607
    ~ "1607. South Africa: Seventh year of primary education", x == 1608 ~ "1608.
South Africa: First year of secondary education (Grade 8)", x == 1609 ~ "1609.
South Africa: Second year of secondary education (Grade 9)", x == 1610 ~ "1610.
South Africa: Third year of secondary education (Grade 10)", x == 1611 ~ "1611.
South Africa: Fourth year of secondary education (Grade 11)", x == 1612 ~
      "1612. South Africa: Fifth year of secondary education (Grade 12)", x == 1613
    ~ "1613. South Africa: Some tertiary education (college, university)", x ==
      1614 ~ "1614. South Africa: Completed tertiary education (college, university
complete)", x == 1615 ~ "1615. South Africa: Post university education", x ==
      1700 ~ "1700. Spain: (No formal education)", x == 1701 ~ "1701. Spain: Primary
Incomplete (less than 5 years)", x == 1702 ~ "1702. Spain: Primary Complete,
First level of EGB", x == 1703 ~ "1703. Spain: Secondary School Incomplete", x
    == 1704 ~ "1704. Spain: Second level of EGB, Secondary School Graduate or ESO
complete (Certificate of success in EGB course)", x == 1705 ~ "1705. Spain:
Secondary School Certificate, FP1 (Vocational Training I)", x == 1706 ~ "1706.
Spain: Secondary School graduate LOGSE, COU, Pre University, FP2 (Vocational
Training II)", x == 1707 ~ "1707. Spain: First Stage of University Degree
(University Diploma Course or 3 complete years of University)", x == 1708 ~
      "1708. Spain: University or Engineering degree or High Level Technician", x
    == 1709 ~ "1709. Spain: Doctorate", x == 1800 ~ "1800. Tanzania: (No formal
education)", x == 1801 ~ "1801. Tanzania: First year of primary education",
    x == 1802 ~ "1802. Tanzania: Second year of primary education", x == 1803 ~
      "1803. Tanzania: Third year of primary education", x == 1804 ~ "1804. Tanzania:
Fourth year of primary education", x == 1805 ~ "1805. Tanzania: Fifth year
of primary education", x == 1806 ~ "1806. Tanzania: Sixth year of primary
education", x == 1807 ~ "1807. Tanzania: Seventh year of primary education",
    x == 1808 ~ "1808. Tanzania: First year of secondary education", x == 1809
    ~ "1809. Tanzania: Second year of secondary education", x == 1810 ~ "1810.
Tanzania: Third year of secondary education", x == 1811 ~ "1811. Tanzania:
Fourth year of secondary education", x == 1812 ~ "1812. Tanzania: Fifth year
of secondary education", x == 1813 ~ "1813. Tanzania: Sixth year of secondary
education", x == 1814 ~ "1814. Tanzania: First year of technical training",
    x == 1815 ~ "1815. Tanzania: Second year of technical training (Polytechnic
or Diploma colleges)", x == 1816 ~ "1816. Tanzania: Third year of technical
training (Polytechnic or Diploma colleges)", x == 1817 ~ "1817. Tanzania: First
year of teachers training college", x == 1818 ~ "1818. Tanzania: Second year
of teachers training college", x == 1819 ~ "1819. Tanzania: Completed education
at teachers training college", x == 1820 ~ "1820. Tanzania: Some university
education", x == 1821 ~ "1821. Tanzania: Completed university education", x ==
      1822 ~ "1822. Tanzania: Completed post university education", x == 1900 ~ "1900.
Turkey: (No formal education)", x == 1901 ~ "1901. Turkey: Literate without
any diploma / did not complete primary", x == 1902 ~ "1902. Turkey: Primary
school (5 years)", x == 1903 ~ "1903. Turkey: Primary education (8 years)", x
    == 1904 ~ "1904. Turkey: Junior high school / Vocational school at junior high
school level", x == 1905 ~ "1905. Turkey: High school / Vocational school at
high school level", x == 1906 ~ "1906. Turkey: Training Technical / Profession
school", x == 1907 ~ "1907. Turkey: Universities", x == 1908 ~ "1908. Turkey:
Masters degree", x == 1909 ~ "1909. Turkey: Ph.D.", x == 2000 ~ "2000. United
Kingdom: (No formal education)", x == 2001 ~ "2001. United Kingdom: Nursery
School", x == 2002 ~ "2002. United Kingdom: Infant/Junior School/Basic Adult
Literacy", x == 2003 ~ "2003. United Kingdom: Lower Secondary School (Age less
than 14)", x == 2004 ~ "2004. United Kingdom: Upper Secondary School (GCSE/SCE,
Youth training/NTMA, A level, Highers, NVQ/SVQ Level 3)", x == 2005 ~ "2005.
United Kingdom: Higher Education Access Courses", x == 2006 ~ "2006. United
Kingdom: Undergraduate Degree", x == 2007 ~ "2007. United Kingdom: Masters
Degree", x == 2008 ~ "2008. United Kingdom: HND/HNC/Nursing Degree, PG Diplomas,
NVQ/SVQ Levels 4/5", x == 2009 ~ "2009. United Kingdom: Doctorate", x == 2200
    ~ "2200. United States: No formal education", x == 2201 ~ "2201. United States:
Grade 8 or lower", x == 2202 ~ "2202. United States: Some high school (Grades 9
through 11)", x == 2203 ~ "2203. United States: High school graduate (Grade 12
with diploma or GED certificate)", x == 2204 ~ "2204. United States: Technical,
trade, vocational, or business school or program after high school", x == 2205
    ~ "2205. United States: Some college - college, university, or community college
- but no degree", x == 2206 ~ "2206. United States: Two-year associate degree
from a college, university, or community college", x == 2207 ~ "2207. United
States: Four-year bachelors degree from a college or university (e.g., BS,
BA, AB)", x == 2208 ~ "2208. United States: Some postgraduate or professional
schooling after graduating college, but no postgraduate degree (e.g.,", x ==
      2209 ~ "2209. United States: Postgraduate or professional degree, including
masters, doctorate, medical, or law degree (e.g., MA, MS", x == 2300 ~ "2300.
Sweden: No formal education", x == 2301 ~ "2301. Sweden: Incomplete primary
school", x == 2302 ~ "2302. Sweden: Primary education (ISCED 1)", x == 2303 ~
      "2303. Sweden: Lower secondary education (ISCED 2)", x == 2304 ~ "2304. Sweden:
Upper secondary education (ISCED 3)", x == 2305 ~ "2305. Sweden: Post-secondary
including pre-vocational or vocational education but not tertiary (ISCED 4)", x
    == 2306 ~ "2306. Sweden: Tertiary education – first level (ISCED 5)", x == 2307
    ~ "2307. Sweden: Tertiary education – advanced level (ISCED 6)", x == 2400 ~
      "2400. Hong Kong: No education", x == 2401 ~ "2401. Hong Kong: PSLE and below",
    x == 2402 ~ "2402. Hong Kong: Some junior/lower secondary", x == 2403 ~ "2403.
Hong Kong: Completed junior/lower secondary (GCE N Level (Form 3))", x == 2404
    ~ "2404. Hong Kong: Some senior/higher secondary", x == 2405 ~ "2405. Hong Kong:
Completed senior/higher secondary (GCE O Level (Form 5), GCE A Level (Form 7
in old structure), DSE Level (F", x == 2406 ~ "2406. Hong Kong: Post-secondary
Non-degree (e.g. certificate/diploma/associate degree)", x == 2407 ~ "2407. Hong
Kong: Some University Degree (Bachelors Degree)", x == 2408 ~ "2408. Hong Kong:
Completed University Degree (Bachelors degree)", x == 2409 ~ "2409. Hong Kong:
University Postgraduate Degree (e.g., MBA, Ph. D)",
    .default = "(Missing)"
    ),
    EDUCATION_3_W1 = case_when(x == 1 ~ "1. Up to 8", x == 2 ~ "2. 9-15", x ==
      3 ~ "3. 16+", .default = "(Missing)"),
    EMPLOYMENT_W1 = case_when(x == 1 ~ "1. Employed for an employer", x == 2 ~ "2. Self-employed",
      x == 3 ~ "3. Retired", x == 4 ~ "4. Student", x == 5 ~ "5. Homemaker",
      x == 6 ~ "6. Unemployed and looking for a job", x == 7 ~ "7. None of
these/Other",
      .default = "(Missing)"
    ),
    EXPECT_GOOD_W1 = x,
    EXPENSES_W1 = x,
    FATHER_LOVED_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    FATHER_RELATN_W1 = case_when(x == 1 ~ "1. Very good", x == 2 ~ "2. Somewhat good", x == 3
    ~ "3. Somewhat bad", x == 4 ~ "4. Very bad", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    FEEL_ANXIOUS_W1 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    FORGIVE_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    FREEDOM_W1 = x,
    GENDER_W1 = case_when(x == 1 ~ "1. Male", x == 2 ~ "2. Female", x == 3 ~ "3. Other", x == 4
    ~ "4. Prefer not to answer", .default = "(Missing)"),
    GIVE_UP_W1 = x,
    GOD_PUNISH_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    GRATEFUL_W1 = x,
    GROUP_NOT_REL_W1 = case_when(x == 1 ~ "1. More than once a week", x == 2 ~ "2. Once a week", x ==
      3 ~ "3. One to three times a month", x == 4 ~ "4. A few times a year", x == 5 ~
      "5. Never", .default = "(Missing)"),
    HAPPY_W1 = x,
    HEALTH_GROWUP_W1 = case_when(x == 1 ~ "1. Excellent", x == 2 ~ "2. Very good", x == 3 ~ "3. Good",
      x == 4 ~ "4. Fair", x == 5 ~ "5. Poor",
      .default = "(Missing)"
    ),
    HEALTH_PROB_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    HELP_STRANGER_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    HOPE_FUTURE_W1 = x,
    INCOME_W1 = case_when(x == 101 ~ "101. Argentina: 1,000 pesos or less", x == 102 ~ "102.
Argentina: 1,001 – 5,000 pesos", x == 103 ~ "103. Argentina: 5,001 – 10,000
pesos", x == 104 ~ "104. Argentina: 10,001 – 20,000 pesos", x == 105 ~ "105.
Argentina: 20,001 – 30,000 pesos", x == 106 ~ "106. Argentina: 30,001 – 40,000
pesos", x == 107 ~ "107. Argentina: 40,001 – 50,000 pesos", x == 108 ~ "108.
Argentina: 50,001 – 60,000 pesos", x == 109 ~ "109. Argentina: 60,001 – 70,000
pesos", x == 110 ~ "110. Argentina: 70,001 – 80,000 pesos", x == 111 ~ "111.
Argentina: 80,001 – 90,000 pesos", x == 112 ~ "112. Argentina: 90,001 – 100,000
pesos", x == 113 ~ "113. Argentina: 100,001 – 120,000 pesos", x == 114 ~ "114.
Argentina: 120,001 – 150,000 pesos", x == 115 ~ "115. Argentina: 150,001 –
200,000 pesos", x == 116 ~ "116. Argentina: More than 200,000 pesos", x == 201
    ~ "201. Australia: 20,000 dollars or less", x == 202 ~ "202. Australia: 20,001 –
30,000 dollars", x == 203 ~ "203. Australia: 30,001 – 40,000 dollars", x == 204
    ~ "204. Australia: 40,001 – 50,000 dollars", x == 205 ~ "205. Australia: 50,001
– 60,000 dollars", x == 206 ~ "206. Australia: 60,001 – 75,000 dollars", x ==
      207 ~ "207. Australia: 75,001 – 100,000 dollars", x == 208 ~ "208. Australia:
100,001 – 150,000 dollars", x == 209 ~ "209. Australia: 150,001 – 250,000
dollars", x == 210 ~ "210. Australia: More than 250,000 dollars", x == 301 ~
      "301. Brazil: 100 reals or less", x == 302 ~ "302. Brazil: 101 – 500 reals",
    x == 303 ~ "303. Brazil: 501 – 750 reals", x == 304 ~ "304. Brazil: 751 –
1,000 reals", x == 305 ~ "305. Brazil: 1,001 – 1,500 reals", x == 306 ~ "306.
Brazil: 1,501 – 2,000 reals", x == 307 ~ "307. Brazil: 2,001 – 2,500 reals", x
    == 308 ~ "308. Brazil: 2,501 – 3,000 reals", x == 309 ~ "309. Brazil: 3,001 –
4,000 reals", x == 310 ~ "310. Brazil: 4,001 – 5,000 reals", x == 311 ~ "311.
Brazil: 5,001 – 6,000 reals", x == 312 ~ "312. Brazil: 6,001 – 8,000 reals", x
    == 313 ~ "313. Brazil: 8,001 – 10,000 reals", x == 314 ~ "314. Brazil: 10,001
– 15,000 reals", x == 315 ~ "315. Brazil: More than 15,000 reals", x == 401
    ~ "401. Egypt: 200 EGP or less", x == 402 ~ "402. Egypt: 201 – 500 EGP", x
    == 403 ~ "403. Egypt: 501 – 1,000 EGP", x == 404 ~ "404. Egypt: 1,001 – 3,000
EGP", x == 405 ~ "405. Egypt: 3,001 – 5,000 EGP", x == 406 ~ "406. Egypt: 5,001
– 7,000 EGP", x == 407 ~ "407. Egypt: 7,001 – 10,000 EGP", x == 408 ~ "408.
Egypt: 10,001 – 20,000 EGP", x == 409 ~ "409. Egypt: More than 20,000 EGP",
    x == 501 ~ "501. Germany: 500 euros or less", x == 502 ~ "502. Germany: 501 -
1,500 euros", x == 503 ~ "503. Germany: 1,501 - 2,000 euros", x == 504 ~ "504.
Germany: 2,001 - 2,500 euros", x == 505 ~ "505. Germany: 2,501 - 3,000 euros",
    x == 506 ~ "506. Germany: 3,001 - 3,500 euros", x == 507 ~ "507. Germany: 3,501
- 4,000 euros", x == 508 ~ "508. Germany: 4,001 - 5,000 euros", x == 509 ~ "509.
Germany: 5,001 - 6,000 euros", x == 510 ~ "510. Germany: 6,001 - 7,500 euros",
    x == 511 ~ "511. Germany: 7,501 - 10,000 euros", x == 512 ~ "512. Germany: More
than 10,000 euros", x == 601 ~ "601. India: 1,000 rupees or less", x == 602
    ~ "602. India: 1,001 – 2,000 rupees", x == 603 ~ "603. India: 2,001 – 3,000
rupees", x == 604 ~ "604. India: 3,001 – 4,000 rupees", x == 605 ~ "605. India:
4,001 – 5,000 rupees", x == 606 ~ "606. India: 5,001 – 6,000 rupees", x == 607
    ~ "607. India: 6,001 – 7,500 rupees", x == 608 ~ "608. India: 7,501 – 10,000
rupees", x == 609 ~ "609. India: 10,001 – 15,000 rupees", x == 610 ~ "610.
India: 15,001 – 20,000 rupees", x == 611 ~ "611. India: 20,001 – 35,000 rupees",
    x == 612 ~ "612. India: 35,001 – 50,000 rupees", x == 613 ~ "613. India: More
than 50,000 rupees", x == 701 ~ "701. Indonesia: 150,000 rupiah or less", x ==
      702 ~ "702. Indonesia: 150,001 – 300,000 rupiah", x == 703 ~ "703. Indonesia:
300,001 – 600,000 rupiah", x == 704 ~ "704. Indonesia: 600,001 – 1,000,000
rupiah", x == 705 ~ "705. Indonesia: 1,000,001 – 1,500,000 rupiah", x == 706
    ~ "706. Indonesia: 1,500,001 – 2,000,000 rupiah", x == 707 ~ "707. Indonesia:
2,000,001 – 2,500,000 rupiah", x == 708 ~ "708. Indonesia: 2,500,001 – 4,000,000
rupiah", x == 709 ~ "709. Indonesia: 4,000,001 - 8,000,000 rupiah", x == 710
    ~ "710. Indonesia: 8,000,001 - 14,000,000 rupiah", x == 711 ~ "711. Indonesia:
More than 14,000,000 rupiah", x == 801 ~ "801. Israel: 1,000 Israeli shekels or
less", x == 802 ~ "802. Israel: 1,001 - 3,000 Israeli shekels", x == 803 ~ "803.
Israel: 3,001 - 5,000 Israeli shekels", x == 804 ~ "804. Israel: 5,001 - 6,000
Israeli shekels", x == 805 ~ "805. Israel: 6,001 - 7,000 Israeli shekels", x
    == 806 ~ "806. Israel: 7,001 - 8,000 Israeli shekels", x == 807 ~ "807. Israel:
8,001 - 9,000 Israeli shekels", x == 808 ~ "808. Israel: 9,001 - 10,000 Israeli
shekels", x == 809 ~ "809. Israel: 10,001 - 11,000 Israeli shekels", x == 810 ~
      "810. Israel: 11,001 - 12,000 Israeli shekels", x == 811 ~ "811. Israel: 12,001
– 15,000 Israeli shekels", x == 812 ~ "812. Israel: 15,001 – 16,000 Israeli
shekels", x == 813 ~ "813. Israel: 16,001 – 18,000 Israeli shekels", x == 814 ~
      "814. Israel: 18,001 – 20,000 Israeli shekels", x == 815 ~ "815. Israel: 20,001
- 40,000 Israeli shekels", x == 816 ~ "816. Israel: More than 40,000 Israeli
shekels", x == 901 ~ "901. Japan: 50,000 yen or less", x == 902 ~ "902. Japan:
50,001 – 100,000 yen", x == 903 ~ "903. Japan: 100,001 – 150,000 yen", x == 904
    ~ "904. Japan: 150,001 – 200,000 yen", x == 905 ~ "905. Japan: 200,001 – 250,000
yen", x == 906 ~ "906. Japan: 250,001 – 300,000 yen", x == 907 ~ "907. Japan:
300,001 – 400,000 yen", x == 908 ~ "908. Japan: 400,001 – 450,000 yen", x ==
      909 ~ "909. Japan: 450,001 – 500,000 yen", x == 910 ~ "910. Japan: 500,001 –
600,000 yen", x == 911 ~ "911. Japan: 600,001 – 700,000 yen", x == 912 ~ "912.
Japan: 700,001 – 800,000 yen", x == 913 ~ "913. Japan: 800,001 – 900,000 yen",
    x == 914 ~ "914. Japan: 900,001 – 1,000,000 yen", x == 915 ~ "915. Japan: More
than 1,000,000 yen", x == 1001 ~ "1001. Kenya: 500 Kshs/month or less", x ==
      1002 ~ "1002. Kenya: 551 – 1,000 Kshs/month", x == 1003 ~ "1003. Kenya: 1,001
– 2,000 Kshs/month", x == 1004 ~ "1004. Kenya: 2,001 – 3,000 Kshs/month", x ==
      1005 ~ "1005. Kenya: 3,001 – 4,000 Kshs/month", x == 1006 ~ "1006. Kenya: 4,001
– 5,000 Kshs/month", x == 1007 ~ "1007. Kenya: 5,001 – 6,000 Kshs/month", x ==
      1008 ~ "1008. Kenya: 6,001 – 8,000 Kshs/month", x == 1009 ~ "1009. Kenya: 8,001
– 10,000 Kshs/month", x == 1010 ~ "1010. Kenya: 10,001 – 12,000 Kshs/month",
    x == 1011 ~ "1011. Kenya: 12,001 – 18,000 Kshs/month", x == 1012 ~ "1012.
Kenya: 18,001 – 25,000 Kshs/month", x == 1013 ~ "1013. Kenya: 25,001 – 40,000
Kshs/month", x == 1014 ~ "1014. Kenya: 40,001 – 80,000 Kshs/month", x == 1015
    ~ "1015. Kenya: More than 80,000 Kshs/month", x == 1101 ~ "1101. Mexico: 550
Mexican pesos or less", x == 1102 ~ "1102. Mexico: 551 – 1.000 Mexican pesos",
    x == 1103 ~ "1103. Mexico: 1.001 – 1.500 Mexican pesos", x == 1104 ~ "1104.
Mexico: 1.501 – 2.000 Mexican pesos", x == 1105 ~ "1105. Mexico: 2.001 – 2.500
Mexican pesos", x == 1106 ~ "1106. Mexico: 2.501 – 3.000 Mexican pesos", x ==
      1107 ~ "1107. Mexico: 3.001 – 4.000 Mexican pesos", x == 1108 ~ "1108. Mexico:
4.001 – 5.000 Mexican pesos", x == 1109 ~ "1109. Mexico: 5.001 – 7.000 Mexican
pesos", x == 1110 ~ "1110. Mexico: 7.001 – 10.000 Mexican pesos", x == 1111 ~
      "1111. Mexico: 10.001 – 15.000 Mexican pesos", x == 1112 ~ "1112. Mexico: 15.001
– 20.000 Mexican pesos", x == 1113 ~ "1113. Mexico: 20.001 – 30.000 Mexican
pesos", x == 1114 ~ "1114. Mexico: 30.001 – 40.000 Mexican pesos", x == 1115 ~
      "1115. Mexico: 40.001 – 50.000 Mexican pesos", x == 1116 ~ "1116. Mexico: 50.001
– 70.000 Mexican pesos", x == 1117 ~ "1117. Mexico: More than 70.000 Mexican
pesos", x == 1201 ~ "1201. Nigeria: 3,700 naira/month or less", x == 1202 ~
      "1202. Nigeria: 3,701 – 6,000 naira/month", x == 1203 ~ "1203. Nigeria: 6,001
– 9,000 naira/month", x == 1204 ~ "1204. Nigeria: 9,001 – 13,000 naira/month",
    x == 1205 ~ "1205. Nigeria: 13,001 – 18,000 naira/month", x == 1206 ~ "1206.
Nigeria: 18,001 – 24,000 naira/month", x == 1207 ~ "1207. Nigeria: 24,001 –
30,000 naira/month", x == 1208 ~ "1208. Nigeria: 30,001 – 37,000 naira/month",
    x == 1209 ~ "1209. Nigeria: 37,001 – 47,000 naira/month", x == 1210 ~ "1210.
Nigeria: 47,001 – 57,000 naira/month", x == 1211 ~ "1211. Nigeria: 57,001 –
67,000 naira/month", x == 1212 ~ "1212. Nigeria: 67,001 – 77,000 naira/month",
    x == 1213 ~ "1213. Nigeria: 77,001 – 100,000 naira/month", x == 1214 ~ "1214.
Nigeria: 100,001 – 150,000 naira/month", x == 1215 ~ "1215. Nigeria: More than
150,000 naira/month", x == 1301 ~ "1301. Philippines: 700 pesos or less", x ==
      1302 ~ "1302. Philippines: 701 – 1,500 pesos", x == 1303 ~ "1303. Philippines:
1,501 – 3,000 pesos", x == 1304 ~ "1304. Philippines: 3,001 – 4,000 pesos", x ==
      1305 ~ "1305. Philippines: 4,001 – 6,000 pesos", x == 1306 ~ "1306. Philippines:
6,001 – 8,000 pesos", x == 1307 ~ "1307. Philippines: 8,001 – 10,000 pesos",
    x == 1308 ~ "1308. Philippines: 10,001 – 15,000 pesos", x == 1309 ~ "1309.
Philippines: 15,001 – 20,000 pesos", x == 1310 ~ "1310. Philippines: 20,001 –
25,000 pesos", x == 1311 ~ "1311. Philippines: 25,001 – 30,000 pesos", x == 1312
    ~ "1312. Philippines: 30,001 – 40,000 pesos", x == 1313 ~ "1313. Philippines:
40,001 – 50,000 pesos", x == 1314 ~ "1314. Philippines: 50,001 – 90,000 pesos",
    x == 1315 ~ "1315. Philippines: More than 90,000 pesos", x == 1401 ~ "1401.
Poland: 100 PLN or less", x == 1402 ~ "1402. Poland: 101 – 500 PLN", x == 1403 ~
      "1403. Poland: 501 – 1,000 PLN", x == 1404 ~ "1404. Poland: 1,001 – 1,500 PLN",
    x == 1405 ~ "1405. Poland: 1,501 – 2,000 PLN", x == 1406 ~ "1406. Poland: 2,001
– 2,500 PLN", x == 1407 ~ "1407. Poland: 2,501 – 3,000 PLN", x == 1408 ~ "1408.
Poland: 3,001 – 3,500 PLN", x == 1409 ~ "1409. Poland: 3,501 – 4,000 PLN", x
    == 1410 ~ "1410. Poland: 4,001 – 5,000 PLN", x == 1411 ~ "1411. Poland: 5,001
– 6,000 PLN", x == 1412 ~ "1412. Poland: 6,001 – 8,000 PLN", x == 1413 ~ "1413.
Poland: 8,001 – 10,000 PLN", x == 1414 ~ "1414. Poland: 10,001 – 15,000 PLN", x
    == 1415 ~ "1415. Poland: 15,001 – 20,000 PLN", x == 1416 ~ "1416. Poland: More
than 20,000 PLN", x == 1601 ~ "1601. South Africa: 200 rand/month or less", x
    == 1602 ~ "1602. South Africa: 201 – 350 rand/month", x == 1603 ~ "1603. South
Africa: 351 – 1,000 rand/month", x == 1604 ~ "1604. South Africa: 1,001 – 2,000
rand/month", x == 1605 ~ "1605. South Africa: 2,001 – 3,000 rand/month", x ==
      1606 ~ "1606. South Africa: 3,001 – 4,000 rand/month", x == 1607 ~ "1607. South
Africa: 4,001 – 5,000 rand/month", x == 1608 ~ "1608. South Africa: 5,001 –
6,000 rand/month", x == 1609 ~ "1609. South Africa: 6,001 – 8,000 rand/month",
    x == 1610 ~ "1610. South Africa: 8,001 – 10,000 rand/month", x == 1611 ~ "1611.
South Africa: 10,001 – 15,000 rand/month", x == 1612 ~ "1612. South Africa:
15,001 – 20,000 rand/month", x == 1613 ~ "1613. South Africa: 20,001 – 30,000
rand/month", x == 1614 ~ "1614. South Africa: More than 30,000 rand/month",
    x == 1701 ~ "1701. Spain: 250 euros or less", x == 1702 ~ "1702. Spain: 251 -
1,000 euros", x == 1703 ~ "1703. Spain: 1,001 - 1,250 euros", x == 1704 ~ "1704.
Spain: 1,251 – 1,500 euros", x == 1705 ~ "1705. Spain: 1,501 - 1,750 euros", x
    == 1706 ~ "1706. Spain: 1,751 – 2,000 euros", x == 1707 ~ "1707. Spain: 2,001
- 2,250 euros", x == 1708 ~ "1708. Spain: 2,251 – 2,500 euros", x == 1709 ~
      "1709. Spain: 2,501 - 2,750 euros", x == 1710 ~ "1710. Spain: 2,751 – 3,000
euros", x == 1711 ~ "1711. Spain: 3,001 - 3,500 euros", x == 1712 ~ "1712.
Spain: 3,501 - 4,000 euros", x == 1713 ~ "1713. Spain: 4,001 - 5,000 euros",
    x == 1714 ~ "1714. Spain: 5,001 – 6,000 euros", x == 1715 ~ "1715. Spain:
More than 6,000 euros", x == 1801 ~ "1801. Tanzania: 20,000 shillings/month
or less", x == 1802 ~ "1802. Tanzania: 20,001 – 50,000 shillings/month",
    x == 1803 ~ "1803. Tanzania: 50,001 – 100,000 shillings/month", x ==
      1804 ~ "1804. Tanzania: 100,001 – 150,000 shillings/month", x == 1805
    ~ "1805. Tanzania: 150,001 – 200,000 shillings/month", x == 1806 ~
      "1806. Tanzania: 200,001 – 250,000 shillings/month", x == 1807 ~ "1807.
Tanzania: 250,001 – 300,000 shillings/month", x == 1808 ~ "1808. Tanzania:
300,001 – 400,000 shillings/month", x == 1809 ~ "1809. Tanzania: 400,001
– 500,000 shillings/month", x == 1810 ~ "1810. Tanzania: 500,001 –
700,000 shillings/month", x == 1811 ~ "1811. Tanzania: More than 700,000
shillings/month", x == 1901 ~ "1901. Turkey: 100 TL or less", x == 1902 ~ "1902.
Turkey: 101 - 500 TL", x == 1903 ~ "1903. Turkey: 501 - 1,000 TL", x == 1904
    ~ "1904. Turkey: 1,001 - 1,500 TL", x == 1905 ~ "1905. Turkey: 1,501 - 2,000
TL", x == 1906 ~ "1906. Turkey: 2,001 - 2,500 TL", x == 1907 ~ "1907. Turkey:
2,501 – 3,000 TL", x == 1908 ~ "1908. Turkey: 3,001 - 3,500 TL", x == 1909 ~
      "1909. Turkey: 3,501 – 4,000 TL", x == 1910 ~ "1910. Turkey: 4,001 - 4,500 TL",
    x == 1911 ~ "1911. Turkey: 4,501 – 5,000 TL", x == 1912 ~ "1912. Turkey: 5,001
– 6,000 TL", x == 1913 ~ "1913. Turkey: 6,001 - 8,000 TL", x == 1914 ~ "1914.
Turkey: 8,001 - 10,000 TL", x == 1915 ~ "1915. Turkey: 10,001 - 12,000 TL", x ==
      1916 ~ "1916. Turkey: More than 12,000 TL", x == 2001 ~ "2001. United Kingdom:
500 pounds or less", x == 2002 ~ "2002. United Kingdom: 501 - 1,000 pounds",
    x == 2003 ~ "2003. United Kingdom: 1,001 - 1,500 pounds", x == 2004 ~ "2004.
United Kingdom: 1,501 - 2,000 pounds", x == 2005 ~ "2005. United Kingdom: 2,001
- 2,500 pounds", x == 2006 ~ "2006. United Kingdom: 2,501 - 3,000 pounds",
    x == 2007 ~ "2007. United Kingdom: 3,001 - 4,000 pounds", x == 2008 ~ "2008.
United Kingdom: 4,001 - 5,000 pounds", x == 2009 ~ "2009. United Kingdom: 5,001
- 6,000 pounds", x == 2010 ~ "2010. United Kingdom: 6,001 - 8,000 pounds", x
    == 2011 ~ "2011. United Kingdom: More than 8,000 pounds", x == 2201 ~ "2201.
United States: Less than $12,000", x == 2202 ~ "2202. United States: $12,000
to $23,999", x == 2203 ~ "2203. United States: $24,000 to $35,999", x == 2204
    ~ "2204. United States: $36,000 to $47,999", x == 2205 ~ "2205. United States:
$48,000 to $59,999", x == 2206 ~ "2206. United States: $60,000 to $89,999", x
    == 2207 ~ "2207. United States: $90,000 to $119,999", x == 2208 ~ "2208. United
States: $120,000 to $179,999", x == 2209 ~ "2209. United States: $180,000 to
$239,999", x == 2210 ~ "2210. United States: $240,000 and over", x == 2301 ~
      "2301. Sweden: 10,000 SEK or less", x == 2302 ~ "2302. Sweden: 10,001 - 15,000
SEK", x == 2303 ~ "2303. Sweden: 15,001 - 25,000 SEK", x == 2304 ~ "2304.
Sweden: 25,001 - 30,000 SEK", x == 2305 ~ "2305. Sweden: 30,001 - 35,000 SEK", x
    == 2306 ~ "2306. Sweden: 35,001 - 40,000 SEK", x == 2307 ~ "2307. Sweden: 40,001
- 50,000 SEK", x == 2308 ~ "2308. Sweden: 50,001 - 60,000 SEK", x == 2309 ~
      "2309. Sweden: 60,001 - 70,000 SEK", x == 2310 ~ "2310. Sweden: 70,001 - 80,000
SEK", x == 2311 ~ "2311. Sweden: 80,001 - 100,000 SEK", x == 2312 ~ "2312.
Sweden: More than 100,000 SEK", x == 2401 ~ "2401. Hong Kong: 5,000 dollars
or less", x == 2402 ~ "2402. Hong Kong: 5,001 – 10,000 dollars", x == 2403 ~
      "2403. Hong Kong: 10,001 – 15,000 dollars", x == 2404 ~ "2404. Hong Kong: 15,001
– 20,000 dollars", x == 2405 ~ "2405. Hong Kong: 20,001 – 30,000 dollars", x
    == 2406 ~ "2406. Hong Kong: 30,001 – 40,000 dollars", x == 2407 ~ "2407. Hong
Kong: 40,001 – 45,000 dollars", x == 2408 ~ "2408. Hong Kong: 45,001 – 50,000
dollars", x == 2409 ~ "2409. Hong Kong: 50,001 – 60,000 dollars", x == 2410 ~
      "2410. Hong Kong: 60,001 – 70,000 dollars", x == 2411 ~ "2411. Hong Kong: 70,001
– 80,000 dollars", x == 2412 ~ "2412. Hong Kong: 80,001 – 100,000 dollars", x ==
      2413 ~ "2413. Hong Kong: More than 100,000 dollars", x == 9900 ~ "9900. (None/No
household income)",
    .default = "(Missing)"
    ),
    INCOME_12YRS_W1 = case_when(x == 1 ~ "1. Lived comfortably", x == 2 ~ "2. Got by", x == 3 ~
      "3. Found it difficult", x == 4 ~ "4. Found it very difficult",
    .default =
      "(Missing)"
    ),
    INCOME_DIFF_W1 = case_when(x == 1 ~ "1. Strongly agree", x == 2 ~ "2. Somewhat agree", x == 3 ~
      "3. Neither agree nor disagree", x == 4 ~ "4. Somewhat disagree", x == 5 ~ "5.
Strongly disagree", .default = "(Missing)"),
    INCOME_FEELINGS_W1 = case_when(x == 1 ~ "1. Living comfortably on present income", x == 2 ~ "2.
Getting by on present income", x == 3 ~ "3. Finding it difficult on present
income", x == 4 ~ "4. Finding it very difficult on present income",
      .default =
        "(Missing)"
    ),
    INTEREST_W1 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    LIFE_APPROACH_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    LIFE_BALANCE_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    LIFE_PURPOSE_W1 = x,
    LIFE_SAT_W1 = x,
    LONELY_W1 = x,
    LOVED_BY_GOD_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    MARITAL_STATUS_W1 = case_when(x == 1 ~ "1. Single/Never been married", x == 2 ~ "2. Married", x ==
      3 ~ "3. Separated", x == 4 ~ "4. Divorced", x == 5 ~ "5. Widowed", x == 6 ~ "6.
Domestic partner", .default = "(Missing)"),
    MENTAL_HEALTH_W1 = x,
    MOTHER_LOVED_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    MOTHER_RELATN_W1 = case_when(x == 1 ~ "1. Very good", x == 2 ~ "2. Somewhat good", x == 3
    ~ "3. Somewhat bad", x == 4 ~ "4. Very bad", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    NUM_CHILDREN_W1 = x,
    NUM_HOUSEHOLD_W1 = x,
    OBEY_LAW_W1 = case_when(x == 1 ~ "1. Strongly agree", x == 2 ~ "2. Somewhat agree", x == 3 ~
      "3. Neither agree nor disagree", x == 4 ~ "4. Somewhat disagree", x == 5 ~ "5.
Strongly disagree", .default = "(Missing)"),
    OUTSIDER_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not apply)", .default = "(Missing)"),
    OWN_RENT_HOME_W1 = case_when(x == 1 ~ "1. Someone in this household OWNS this home", x == 2 ~
      "2. Someone in this household RENTS this home", x == 3 ~ "3. Both", x == 4 ~
      "4. Neither", x == 5 ~ "5. Rent", x == 6 ~ "6. Own", x == 7 ~ "7. Something
else", .default = "(Missing)"),
    PARENTS_12YRS_W1 = case_when(x == 1 ~ "1. Yes, married", x == 2 ~ "2. No, divorced", x == 3 ~ "3.
No, they were never married", x == 4 ~ "4. No, one or both of them had died", x
    == 5 ~ "5. Unsure", .default = "(Missing)"),
    PEACE_W1 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    PEOPLE_HELP_W1 = x,
    PHYSICAL_HLTH_W1 = x,
    POLITICAL_ID_W1 = x,
    PRAY_MEDITATE_W1 = case_when(x == 1 ~ "1. More than once a day", x == 2 ~ "2. About once a day", x
    == 3 ~ "3. Sometimes", x == 4 ~ "4. Never", .default = "(Missing)"),
    PROMOTE_GOOD_W1 = x,
    REGION1_W1 = case_when(x == 101 ~ "101. Argentina: Capital Federal", x == 102 ~ "102.
Argentina: Buenos Aires", x == 103 ~ "103. Argentina: Catamarca", x == 104
    ~ "104. Argentina: Chaco", x == 105 ~ "105. Argentina: Chubut", x == 106 ~
      "106. Argentina: Cordoba", x == 107 ~ "107. Argentina: Corrientes", x == 108
    ~ "108. Argentina: Entre Ríos", x == 109 ~ "109. Argentina: Formosa", x ==
      110 ~ "110. Argentina: Jujuy", x == 111 ~ "111. Argentina: La Pampa", x ==
      112 ~ "112. Argentina: La Rioja", x == 113 ~ "113. Argentina: Mendoza", x ==
      114 ~ "114. Argentina: Misiones", x == 115 ~ "115. Argentina: Neuquen", x ==
      116 ~ "116. Argentina: Río Negro", x == 117 ~ "117. Argentina: Salta", x ==
      118 ~ "118. Argentina: San Juan", x == 119 ~ "119. Argentina: San Luis", x ==
      120 ~ "120. Argentina: Santa Cruz", x == 121 ~ "121. Argentina: Santa Fe", x
    == 122 ~ "122. Argentina: Santiago del Estero", x == 123 ~ "123. Argentina:
Tierra del Fuego", x == 124 ~ "124. Argentina: Tucumán", x == 201 ~ "201.
Australia: Sydney", x == 202 ~ "202. Australia: New South Wales (NSW), excluding
Sydney", x == 203 ~ "203. Australia: Melbourne", x == 204 ~ "204. Australia:
Victoria (VIC), excluding Melbourne", x == 205 ~ "205. Australia: Brisbane",
    x == 206 ~ "206. Australia: Queensland (QLD), excluding Brisbane", x == 207
    ~ "207. Australia: Adelaide", x == 208 ~ "208. Australia: South Australia
(SA), excluding Adelaide", x == 209 ~ "209. Australia: Perth", x == 210 ~
      "210. Australia: Western Australia (WA), excluding Perth", x == 211 ~ "211.
Australia: Hobart", x == 212 ~ "212. Australia: Tasmania (TAS), excluding
Hobart", x == 213 ~ "213. Australia: Australian Capital Territory (ACT)", x ==
      214 ~ "214. Australia: Darwin", x == 215 ~ "215. Australia: Northern Territory
(NT), excluding Darwin", x == 311 ~ "311. Brazil: Roraima", x == 313 ~ "313.
Brazil: Amazonas", x == 315 ~ "315. Brazil: Pará", x == 317 ~ "317. Brazil:
Tocantins", x == 321 ~ "321. Brazil: Maranhão", x == 322 ~ "322. Brazil: Piauí",
    x == 323 ~ "323. Brazil: Ceará", x == 324 ~ "324. Brazil: Rio Grande do Norte",
    x == 325 ~ "325. Brazil: Paraíba", x == 326 ~ "326. Brazil: Pernambuco", x ==
      327 ~ "327. Brazil: Alagoas", x == 328 ~ "328. Brazil: Sergipe", x == 329 ~
      "329. Brazil: Bahia", x == 331 ~ "331. Brazil: Minas Gerais", x == 333 ~ "333.
Brazil: Rio de Janeiro", x == 335 ~ "335. Brazil: São Paulo", x == 341 ~ "341.
Brazil: Paraná", x == 342 ~ "342. Brazil: Santa Catarina", x == 343 ~ "343.
Brazil: Rio Grande do Sul", x == 350 ~ "350. Brazil: Mato Grosso do Sul", x ==
      351 ~ "351. Brazil: Mato Grosso", x == 352 ~ "352. Brazil: Goiás", x == 353 ~
      "353. Brazil: Distrito Federal", x == 354 ~ "354. Brazil: Espirito Santo", x
    == 355 ~ "355. Brazil: Amapá", x == 356 ~ "356. Brazil: Rondonia", x == 357 ~
      "357. Brazil: Acre", x == 401 ~ "401. Egypt: Dakhalia", x == 402 ~ "402. Egypt:
Faiyum", x == 403 ~ "403. Egypt: Sharqia", x == 404 ~ "404. Egypt: Aswan", x ==
      405 ~ "405. Egypt: Cairo", x == 406 ~ "406. Egypt: Ismailia", x == 407 ~ "407.
Egypt: Menia", x == 408 ~ "408. Egypt: Alexandria", x == 409 ~ "409. Egypt:
Gharbia", x == 410 ~ "410. Egypt: Sohag", x == 411 ~ "411. Egypt: Giza", x ==
      412 ~ "412. Egypt: Port Said", x == 413 ~ "413. Egypt: Suez", x == 414 ~ "414.
Egypt: Damietta", x == 415 ~ "415. Egypt: Qalyubia", x == 416 ~ "416. Egypt:
Kafr El-Sheikh", x == 417 ~ "417. Egypt: Monufia", x == 418 ~ "418. Egypt:
Beheira", x == 419 ~ "419. Egypt: Beni-Suef", x == 420 ~ "420. Egypt: Asyut",
    x == 421 ~ "421. Egypt: Qena", x == 422 ~ "422. Egypt: Luxor", x == 423 ~ "423.
Egypt: Matruh", x == 424 ~ "424. Egypt: Red Sea", x == 425 ~ "425. Egypt: New
Valley", x == 426 ~ "426. Egypt: North Sinai", x == 427 ~ "427. Egypt: South
Sinai", x == 501 ~ "501. Germany: Arnsberg", x == 502 ~ "502. Germany: Berlin",
    x == 503 ~ "503. Germany: Brandenburg", x == 504 ~ "504. Germany: Braunschweig",
    x == 505 ~ "505. Germany: Bremen", x == 506 ~ "506. Germany: Chemnitz", x
    == 507 ~ "507. Germany: Darmstadt", x == 508 ~ "508. Germany: Detmold", x ==
      509 ~ "509. Germany: Dresden", x == 510 ~ "510. Germany: Dusseldorf", x ==
      511 ~ "511. Germany: Freiburg", x == 512 ~ "512. Germany: Giessen", x == 513
    ~ "513. Germany: Hamburg", x == 514 ~ "514. Germany: Hannover", x == 515 ~
      "515. Germany: Karlsruhe", x == 516 ~ "516. Germany: Kassel", x == 517 ~ "517.
Germany: Koblenz", x == 518 ~ "518. Germany: Koln", x == 519 ~ "519. Germany:
Leipzig", x == 520 ~ "520. Germany: Luneburg", x == 521 ~ "521. Germany:
Mecklenburg-Vorpommern", x == 522 ~ "522. Germany: Mittelfranken", x == 523 ~
      "523. Germany: Munster", x == 524 ~ "524. Germany: Niederbayern", x == 525 ~
      "525. Germany: Oberbayern", x == 526 ~ "526. Germany: Oberfranken", x == 527 ~
      "527. Germany: Oberpfalz", x == 528 ~ "528. Germany: Rheinhessen-Pfalz", x ==
      529 ~ "529. Germany: Saarland", x == 530 ~ "530. Germany: Sachsen-Anhalt", x ==
      531 ~ "531. Germany: Schleswig-Holstein", x == 532 ~ "532. Germany: Schwaben",
    x == 533 ~ "533. Germany: Stuttgart", x == 534 ~ "534. Germany: Thuringen", x
    == 535 ~ "535. Germany: Trier", x == 536 ~ "536. Germany: Tubingen", x == 537
    ~ "537. Germany: Unterfranken", x == 538 ~ "538. Germany: Weser-Ems", x == 601
    ~ "601. India: Andhra Pradesh", x == 602 ~ "602. India: Arunachal Pradesh", x
    == 603 ~ "603. India: Assam", x == 604 ~ "604. India: Bihar", x == 605 ~ "605.
India: Chandigarh", x == 606 ~ "606. India: Chhattisgarh", x == 607 ~ "607.
India: Delhi", x == 608 ~ "608. India: Goa", x == 609 ~ "609. India: Gujarat",
    x == 610 ~ "610. India: Haryana", x == 611 ~ "611. India: Himachal Pradesh", x
    == 612 ~ "612. India: Jammu and Kashmir", x == 613 ~ "613. India: Jharkhand",
    x == 614 ~ "614. India: Karnataka", x == 615 ~ "615. India: Kerala", x == 616
    ~ "616. India: Madhya Pradesh", x == 617 ~ "617. India: Maharashtra", x == 618
    ~ "618. India: Manipur", x == 619 ~ "619. India: Meghalaya", x == 620 ~ "620.
India: Mizoram", x == 621 ~ "621. India: Nagaland", x == 622 ~ "622. India:
Odisha (Orissa)", x == 623 ~ "623. India: Pondicherry", x == 624 ~ "624. India:
Punjab", x == 625 ~ "625. India: Rajasthan", x == 626 ~ "626. India: Sikkim",
    x == 627 ~ "627. India: Tamil Nadu", x == 628 ~ "628. India: Tripura", x ==
      629 ~ "629. India: Uttar Pradesh", x == 630 ~ "630. India: Uttarakhand", x ==
      631 ~ "631. India: West Bengal", x == 632 ~ "632. India: Telangana", x == 701
    ~ "701. Indonesia: Nanggroe Aceh Darussalam", x == 702 ~ "702. Indonesia: North
Sumatra", x == 703 ~ "703. Indonesia: West Sumatra", x == 704 ~ "704. Indonesia:
Riau", x == 705 ~ "705. Indonesia: Jambi", x == 706 ~ "706. Indonesia: South
Sumatra", x == 707 ~ "707. Indonesia: Bengkulu", x == 708 ~ "708. Indonesia:
Lampung", x == 709 ~ "709. Indonesia: Bangka Belitung Island", x == 710 ~ "710.
Indonesia: Riau Island", x == 711 ~ "711. Indonesia: DKI Jakarta", x == 712 ~
      "712. Indonesia: West Java", x == 713 ~ "713. Indonesia: Central Java", x ==
      714 ~ "714. Indonesia: DI Yogjakarta", x == 715 ~ "715. Indonesia: East Java",
    x == 716 ~ "716. Indonesia: Banten", x == 717 ~ "717. Indonesia: Bali", x ==
      718 ~ "718. Indonesia: West Nusa Tenggara", x == 719 ~ "719. Indonesia: East
Nusa Tenggara", x == 720 ~ "720. Indonesia: West Kalimantan", x == 721 ~ "721.
Indonesia: Central Kalimantan", x == 722 ~ "722. Indonesia: South Kalimantan",
    x == 723 ~ "723. Indonesia: East Kalimantan", x == 724 ~ "724. Indonesia: North
Sulawesi", x == 725 ~ "725. Indonesia: Central Sulawesi", x == 726 ~ "726.
Indonesia: South Sulawesi", x == 727 ~ "727. Indonesia: Southeast Sulawesi",
    x == 728 ~ "728. Indonesia: Gorontalo", x == 729 ~ "729. Indonesia: West
Sulawesi", x == 730 ~ "730. Indonesia: Maluku", x == 731 ~ "731. Indonesia:
North Maluku", x == 732 ~ "732. Indonesia: Papua", x == 733 ~ "733. Indonesia:
West Papua", x == 734 ~ "734. Indonesia: North Kalimantan", x == 801 ~ "801.
Israel: Jerusalem District", x == 802 ~ "802. Israel: Northern District",
    x == 803 ~ "803. Israel: Haifa District", x == 804 ~ "804. Israel: Central
District", x == 805 ~ "805. Israel: Tel Aviv District", x == 806 ~ "806. Israel:
Southern District", x == 901 ~ "901. Japan: Hokkaido", x == 902 ~ "902. Japan:
Aomori", x == 903 ~ "903. Japan: Iwate", x == 904 ~ "904. Japan: Miyagi", x ==
      905 ~ "905. Japan: Akita", x == 906 ~ "906. Japan: Yamagata", x == 907 ~ "907.
Japan: Fukushima", x == 908 ~ "908. Japan: Ibaraki", x == 909 ~ "909. Japan:
Tochigi", x == 910 ~ "910. Japan: Gunnma", x == 911 ~ "911. Japan: Saitama", x
    == 912 ~ "912. Japan: Chiba", x == 913 ~ "913. Japan: Tokyo", x == 914 ~ "914.
Japan: Kanagawa", x == 915 ~ "915. Japan: Niigata", x == 916 ~ "916. Japan:
Toyama", x == 917 ~ "917. Japan: Ishikawa", x == 918 ~ "918. Japan: Fukui", x
    == 919 ~ "919. Japan: Yamanashi", x == 920 ~ "920. Japan: Nagano", x == 921 ~
      "921. Japan: Gifu", x == 922 ~ "922. Japan: Shizuoka", x == 923 ~ "923. Japan:
Aichi", x == 924 ~ "924. Japan: Mie", x == 925 ~ "925. Japan: Shiga", x == 926
    ~ "926. Japan: Kyoto", x == 927 ~ "927. Japan: Osaka", x == 928 ~ "928. Japan:
Hyogo", x == 929 ~ "929. Japan: Nara", x == 930 ~ "930. Japan: Wakayama", x
    == 931 ~ "931. Japan: Tottori", x == 932 ~ "932. Japan: Shimane", x == 933 ~
      "933. Japan: Okayama", x == 934 ~ "934. Japan: Hiroshima", x == 935 ~ "935.
Japan: Yamaguchi", x == 936 ~ "936. Japan: Tokushima", x == 937 ~ "937. Japan:
Kagawa", x == 938 ~ "938. Japan: Ehime", x == 939 ~ "939. Japan: Kochi", x ==
      940 ~ "940. Japan: Fukuoka", x == 941 ~ "941. Japan: Saga", x == 942 ~ "942.
Japan: Nagasaki", x == 943 ~ "943. Japan: Kumamoto", x == 944 ~ "944. Japan:
Oita", x == 945 ~ "945. Japan: Miyazaki", x == 946 ~ "946. Japan: Kagoshima",
    x == 947 ~ "947. Japan: Okinawa", x == 1001 ~ "1001. Kenya: Baringo", x == 1002
    ~ "1002. Kenya: Bomet", x == 1003 ~ "1003. Kenya: Bungoma", x == 1004 ~ "1004.
Kenya: Busia", x == 1005 ~ "1005. Kenya: Elgeyo Marakwet", x == 1006 ~ "1006.
Kenya: Embu", x == 1007 ~ "1007. Kenya: Garissa", x == 1008 ~ "1008. Kenya: Homa
Bay", x == 1009 ~ "1009. Kenya: Isiolo", x == 1010 ~ "1010. Kenya: Kajiado",
    x == 1011 ~ "1011. Kenya: Kakamega", x == 1012 ~ "1012. Kenya: Kericho", x ==
      1013 ~ "1013. Kenya: Kiambu", x == 1014 ~ "1014. Kenya: Kilifi", x == 1015 ~
      "1015. Kenya: Kirinyaga", x == 1016 ~ "1016. Kenya: Kisii", x == 1017 ~ "1017.
Kenya: Kisumu", x == 1018 ~ "1018. Kenya: Kitui", x == 1019 ~ "1019. Kenya:
Kwale", x == 1020 ~ "1020. Kenya: Laikipia", x == 1021 ~ "1021. Kenya: Lamu",
    x == 1022 ~ "1022. Kenya: Machakos", x == 1023 ~ "1023. Kenya: Makueni", x
    == 1024 ~ "1024. Kenya: Mandera", x == 1025 ~ "1025. Kenya: Marsabit", x ==
      1026 ~ "1026. Kenya: Meru", x == 1027 ~ "1027. Kenya: Migori", x == 1028 ~
      "1028. Kenya: Mombasa", x == 1029 ~ "1029. Kenya: Murang a", x == 1030 ~ "1030.
Kenya: Nairobi", x == 1031 ~ "1031. Kenya: Nakuru", x == 1032 ~ "1032. Kenya:
Nandi", x == 1033 ~ "1033. Kenya: Narok", x == 1034 ~ "1034. Kenya: Nyamira",
    x == 1035 ~ "1035. Kenya: Nyandarua", x == 1036 ~ "1036. Kenya: Nyeri", x ==
      1037 ~ "1037. Kenya: Samburu", x == 1038 ~ "1038. Kenya: Siaya", x == 1039 ~
      "1039. Kenya: Taita Taveta", x == 1040 ~ "1040. Kenya: Tana River", x == 1041
    ~ "1041. Kenya: Tharaka Nithi", x == 1042 ~ "1042. Kenya: Trans Nzoia", x ==
      1043 ~ "1043. Kenya: Turkana", x == 1044 ~ "1044. Kenya: Uasin Gishu", x ==
      1045 ~ "1045. Kenya: Vihiga", x == 1046 ~ "1046. Kenya: Wajir", x == 1047 ~
      "1047. Kenya: West Pokot", x == 1101 ~ "1101. Mexico: Aguascalientes", x == 1102
    ~ "1102. Mexico: Baja California", x == 1103 ~ "1103. Mexico: Baja California
Sur", x == 1104 ~ "1104. Mexico: Campeche", x == 1105 ~ "1105. Mexico: Chiapas",
    x == 1106 ~ "1106. Mexico: Chihuahua", x == 1107 ~ "1107. Mexico: Coahuila
de Zaragoza", x == 1108 ~ "1108. Mexico: Colima", x == 1109 ~ "1109. Mexico:
Distrito Federal/Ciudad de México", x == 1110 ~ "1110. Mexico: Durango", x ==
      1111 ~ "1111. Mexico: Guanajuato", x == 1112 ~ "1112. Mexico: Guerrero", x ==
      1113 ~ "1113. Mexico: Hidalgo", x == 1114 ~ "1114. Mexico: Jalisco", x == 1115
    ~ "1115. Mexico: México", x == 1116 ~ "1116. Mexico: Michoacán de Ocampo", x ==
      1117 ~ "1117. Mexico: Morelos", x == 1118 ~ "1118. Mexico: Nayarit", x == 1119
    ~ "1119. Mexico: Nuevo León", x == 1120 ~ "1120. Mexico: Oaxaca", x == 1121 ~
      "1121. Mexico: Puebla", x == 1122 ~ "1122. Mexico: Querétaro Arteaga", x == 1123
    ~ "1123. Mexico: Quintana Roo", x == 1124 ~ "1124. Mexico: San Luis Potosí",
    x == 1125 ~ "1125. Mexico: Sinaloa", x == 1126 ~ "1126. Mexico: Sonora", x ==
      1127 ~ "1127. Mexico: Tabasco", x == 1128 ~ "1128. Mexico: Tamaulipas", x ==
      1129 ~ "1129. Mexico: Tlaxcala", x == 1130 ~ "1130. Mexico: Veracruz", x ==
      1131 ~ "1131. Mexico: Yucatán", x == 1132 ~ "1132. Mexico: Zacatecas", x ==
      1201 ~ "1201. Nigeria: Abia", x == 1202 ~ "1202. Nigeria: Abuja", x == 1203
    ~ "1203. Nigeria: Adamawa", x == 1204 ~ "1204. Nigeria: Akwa Ibom", x == 1205
    ~ "1205. Nigeria: Ananmbra", x == 1206 ~ "1206. Nigeria: Bauchi", x == 1207
    ~ "1207. Nigeria: Bayelsa", x == 1208 ~ "1208. Nigeria: Benue", x == 1209 ~
      "1209. Nigeria: Borno", x == 1210 ~ "1210. Nigeria: Cross-River", x == 1211 ~
      "1211. Nigeria: Delta", x == 1212 ~ "1212. Nigeria: Eboyin", x == 1213 ~ "1213.
Nigeria: Edo", x == 1214 ~ "1214. Nigeria: Ekiti", x == 1215 ~ "1215. Nigeria:
Enugu", x == 1216 ~ "1216. Nigeria: Gombe", x == 1217 ~ "1217. Nigeria: Imo",
    x == 1218 ~ "1218. Nigeria: Jigawa", x == 1219 ~ "1219. Nigeria: Kaduna", x ==
      1220 ~ "1220. Nigeria: Kano", x == 1221 ~ "1221. Nigeria: Katsina", x == 1222
    ~ "1222. Nigeria: Kebbi", x == 1223 ~ "1223. Nigeria: Kogi", x == 1224 ~ "1224.
Nigeria: Kwara", x == 1225 ~ "1225. Nigeria: Lagos", x == 1226 ~ "1226. Nigeria:
Nassarawa", x == 1227 ~ "1227. Nigeria: Niger", x == 1228 ~ "1228. Nigeria:
Ogun", x == 1229 ~ "1229. Nigeria: Ondo", x == 1230 ~ "1230. Nigeria: Osun", x
    == 1231 ~ "1231. Nigeria: Oyo", x == 1232 ~ "1232. Nigeria: Plateau", x == 1233
    ~ "1233. Nigeria: Rivers", x == 1234 ~ "1234. Nigeria: Sokoto", x == 1235 ~
      "1235. Nigeria: Taraba", x == 1236 ~ "1236. Nigeria: Yobe", x == 1237 ~ "1237.
Nigeria: Zamfara", x == 1301 ~ "1301. Philippines: National Capital Region
(NCR)", x == 1302 ~ "1302. Philippines: Cordillera (CAR)", x == 1303 ~ "1303.
Philippines: Ilocos (Region I)", x == 1304 ~ "1304. Philippines: Cagayan Valley
(Region II)", x == 1305 ~ "1305. Philippines: Central Luzon (Region III)", x
    == 1306 ~ "1306. Philippines: Calabarzon (Region IV-A)", x == 1307 ~ "1307.
Philippines: Mimaropa (Region IV-B)", x == 1308 ~ "1308. Philippines: Bicol
(Region V)", x == 1309 ~ "1309. Philippines: Western Visayas (Region VI)", x
    == 1310 ~ "1310. Philippines: Central Visayas (Region VII)", x == 1311 ~ "1311.
Philippines: Eastern Visayas (Region VIII)", x == 1312 ~ "1312. Philippines:
Zamboanga Peninsula (Region IX)", x == 1313 ~ "1313. Philippines: Northern
Mindanao (Region X)", x == 1314 ~ "1314. Philippines: Davao (Region XI)", x
    == 1315 ~ "1315. Philippines: SOCCSKSARGEN (Region XII)", x == 1316 ~ "1316.
Philippines: Caraga (Region XIII)", x == 1317 ~ "1317. Philippines: Bangsamoro
Autonomous Region in Muslim Mindanao (BARMM)", x == 1401 ~ "1401. Poland: Lodz
Voivodeship", x == 1402 ~ "1402. Poland: Masovian Voivodeship", x == 1403 ~
      "1403. Poland: Lesser Poland Voivodeship", x == 1404 ~ "1404. Poland: Silesian
Voivodeship", x == 1405 ~ "1405. Poland: Lublin Voivodeship", x == 1406 ~ "1406.
Poland: Subcarpathian Voivodeship", x == 1407 ~ "1407. Poland: Swietokrzyskie
Voivodeship", x == 1408 ~ "1408. Poland: Podlaskie Voivodeship", x == 1409 ~
      "1409. Poland: Greater Poland Voivodeship", x == 1410 ~ "1410. Poland: West
Pomeranian Voivodeship", x == 1411 ~ "1411. Poland: Lubusz Voivodeship", x ==
      1412 ~ "1412. Poland: Lower Silesian Voivodeship", x == 1413 ~ "1413. Poland:
Opole Voivodeship", x == 1414 ~ "1414. Poland: Kuyavian-Pomeranian Voivodeship",
    x == 1415 ~ "1415. Poland: Warmian-Masurian Voivodeship", x == 1416 ~ "1416.
Poland: Pomeranian Voivodeship", x == 1601 ~ "1601. South Africa: Eastern
Cape", x == 1602 ~ "1602. South Africa: Free State", x == 1603 ~ "1603. South
Africa: Gauteng", x == 1604 ~ "1604. South Africa: KwaZulu Natal", x == 1605
    ~ "1605. South Africa: Limpopo", x == 1606 ~ "1606. South Africa: Mpumalanga",
    x == 1607 ~ "1607. South Africa: North West", x == 1608 ~ "1608. South Africa:
Northern Cape", x == 1609 ~ "1609. South Africa: Western Cape", x == 1701 ~
      "1701. Spain: Galicia", x == 1702 ~ "1702. Spain: Asturias", x == 1703 ~ "1703.
Spain: Cantabria", x == 1704 ~ "1704. Spain: Basque Community", x == 1705 ~
      "1705. Spain: Navarre", x == 1706 ~ "1706. Spain: La Rioja", x == 1707 ~ "1707.
Spain: Aragon", x == 1708 ~ "1708. Spain: Madrid", x == 1709 ~ "1709. Spain:
Castile-Leon", x == 1710 ~ "1710. Spain: Castile-La Mancha", x == 1711 ~ "1711.
Spain: Extremadura", x == 1712 ~ "1712. Spain: Catalonia", x == 1713 ~ "1713.
Spain: Valencian Community", x == 1714 ~ "1714. Spain: Balearic Islands", x ==
      1715 ~ "1715. Spain: Andalusia", x == 1716 ~ "1716. Spain: Region of Murcia",
    x == 1717 ~ "1717. Spain: Ceuta", x == 1718 ~ "1718. Spain: Melilla", x == 1719
    ~ "1719. Spain: Canary Islands", x == 1801 ~ "1801. Tanzania: Dar es Salaam", x
    == 1802 ~ "1802. Tanzania: Dodoma", x == 1803 ~ "1803. Tanzania: Tabora", x ==
      1804 ~ "1804. Tanzania: Singida", x == 1805 ~ "1805. Tanzania: Shinyanga", x ==
      1806 ~ "1806. Tanzania: Ruvuma", x == 1807 ~ "1807. Tanzania: Rukwa", x == 1808
    ~ "1808. Tanzania: Pwani", x == 1809 ~ "1809. Tanzania: South Unguja", x == 1810
    ~ "1810. Tanzania: North Unguja", x == 1811 ~ "1811. Tanzania: South Pemba", x
    == 1812 ~ "1812. Tanzania: North Pemba", x == 1813 ~ "1813. Tanzania: Mwanza",
    x == 1814 ~ "1814. Tanzania: Mtwara", x == 1815 ~ "1815. Tanzania: Morogoro",
    x == 1816 ~ "1816. Tanzania: Mbeya", x == 1817 ~ "1817. Tanzania: Mara", x ==
      1818 ~ "1818. Tanzania: Manyara", x == 1820 ~ "1820. Tanzania: Kilimanjaro",
    x == 1821 ~ "1821. Tanzania: Kigoma", x == 1822 ~ "1822. Tanzania: Kagera", x
    == 1823 ~ "1823. Tanzania: Iringa", x == 1824 ~ "1824. Tanzania: Urban West",
    x == 1825 ~ "1825. Tanzania: Arusha", x == 1826 ~ "1826. Tanzania: Tanga",
    x == 1827 ~ "1827. Tanzania: Simiyu", x == 1828 ~ "1828. Tanzania: Katavi",
    x == 1829 ~ "1829. Tanzania: Geita", x == 1830 ~ "1830. Tanzania: Njombe", x
    == 1831 ~ "1831. Tanzania: Lindi", x == 1901 ~ "1901. Turkey: Istanbul", x ==
      1902 ~ "1902. Turkey: Tekirdag", x == 1903 ~ "1903. Turkey: Balikesir", x ==
      1904 ~ "1904. Turkey: Izmir", x == 1905 ~ "1905. Turkey: Aydin", x == 1906 ~
      "1906. Turkey: Manisa", x == 1907 ~ "1907. Turkey: Bursa", x == 1908 ~ "1908.
Turkey: Kocaeli", x == 1909 ~ "1909. Turkey: Ankara", x == 1910 ~ "1910. Turkey:
Konya", x == 1911 ~ "1911. Turkey: Antalya", x == 1912 ~ "1912. Turkey: Adana",
    x == 1913 ~ "1913. Turkey: Hatay", x == 1914 ~ "1914. Turkey: Kirikkale", x
    == 1915 ~ "1915. Turkey: Kayseri", x == 1916 ~ "1916. Turkey: Zonguldak", x
    == 1917 ~ "1917. Turkey: Kastamonu", x == 1918 ~ "1918. Turkey: Samsun", x
    == 1919 ~ "1919. Turkey: Trabzon", x == 1920 ~ "1920. Turkey: Erzurum", x ==
      1921 ~ "1921. Turkey: Agri", x == 1922 ~ "1922. Turkey: Malatya", x == 1923 ~
      "1923. Turkey: Van", x == 1924 ~ "1924. Turkey: Gaziantep", x == 1925 ~ "1925.
Turkey: Sanliurfa", x == 1926 ~ "1926. Turkey: Mardin", x == 2001 ~ "2001.
United Kingdom: Tees Valley and Durham", x == 2002 ~ "2002. United Kingdom:
Northumberland and Tyne and Wear", x == 2003 ~ "2003. United Kingdom: Cumbria",
    x == 2004 ~ "2004. United Kingdom: Greater Manchester", x == 2005 ~ "2005.
United Kingdom: Lancashire", x == 2006 ~ "2006. United Kingdom: Cheshire",
    x == 2007 ~ "2007. United Kingdom: Merseyside", x == 2008 ~ "2008. United
Kingdom: East Yorkshire and Northern Lincolnshire", x == 2009 ~ "2009. United
Kingdom: North Yorkshire", x == 2010 ~ "2010. United Kingdom: South Yorkshire",
    x == 2011 ~ "2011. United Kingdom: West Yorkshire", x == 2012 ~ "2012. United
Kingdom: Derbyshire and Nottinghamshire", x == 2013 ~ "2013. United Kingdom:
Leicestershire, Rutland and Northamptonshire", x == 2014 ~ "2014. United
Kingdom: Lincolnshire", x == 2015 ~ "2015. United Kingdom: Herefordshire,
Worcestershire and Warwickshire", x == 2016 ~ "2016. United Kingdom: Shropshire
and Staffordshire", x == 2017 ~ "2017. United Kingdom: West Midlands", x ==
      2018 ~ "2018. United Kingdom: East Anglia", x == 2019 ~ "2019. United Kingdom:
Bedfordshire and Hertfordshire", x == 2020 ~ "2020. United Kingdom: Essex",
    x == 2021 ~ "2021. United Kingdom: Inner London — West", x == 2022 ~ "2022.
United Kingdom: Inner London — East", x == 2023 ~ "2023. United Kingdom: Outer
London — East and North East", x == 2024 ~ "2024. United Kingdom: Outer London —
South", x == 2025 ~ "2025. United Kingdom: Outer London — West and North West",
    x == 2026 ~ "2026. United Kingdom: Berkshire, Buckinghamshire and Oxfordshire",
    x == 2027 ~ "2027. United Kingdom: Surrey, East and West Sussex", x == 2028 ~
      "2028. United Kingdom: Hampshire and Isle of Wight", x == 2029 ~ "2029. United
Kingdom: Kent", x == 2030 ~ "2030. United Kingdom: Gloucestershire, Wiltshire
and Bristol/Bath area", x == 2031 ~ "2031. United Kingdom: Dorset and Somerset",
    x == 2032 ~ "2032. United Kingdom: Cornwall and Isles of Scilly", x == 2033 ~
      "2033. United Kingdom: Devon", x == 2034 ~ "2034. United Kingdom: West Wales
and The Valleys", x == 2035 ~ "2035. United Kingdom: East Wales", x == 2036
    ~ "2036. United Kingdom: North Eastern Scotland", x == 2037 ~ "2037. United
Kingdom: Highlands and Islands", x == 2038 ~ "2038. United Kingdom: Eastern
Scotland", x == 2039 ~ "2039. United Kingdom: West Central Scotland", x ==
      2040 ~ "2040. United Kingdom: Southern Scotland", x == 2041 ~ "2041. United
Kingdom: Northern Ireland", x == 2201 ~ "2201. United States: Alaska", x == 2202
    ~ "2202. United States: Alabama", x == 2203 ~ "2203. United States: Arkansas",
    x == 2204 ~ "2204. United States: Arizona", x == 2205 ~ "2205. United States:
California", x == 2206 ~ "2206. United States: Colorado", x == 2207 ~ "2207.
United States: Connecticut", x == 2208 ~ "2208. United States: Delaware", x
    == 2209 ~ "2209. United States: Florida", x == 2210 ~ "2210. United States:
Georgia", x == 2211 ~ "2211. United States: Hawaii", x == 2212 ~ "2212. United
States: Iowa", x == 2213 ~ "2213. United States: Idaho", x == 2214 ~ "2214.
United States: Illinois", x == 2215 ~ "2215. United States: Indiana", x == 2216
    ~ "2216. United States: Kansas", x == 2217 ~ "2217. United States: Kentucky",
    x == 2218 ~ "2218. United States: Louisiana", x == 2219 ~ "2219. United States:
Massachusetts", x == 2220 ~ "2220. United States: Maryland", x == 2221 ~ "2221.
United States: Maine", x == 2222 ~ "2222. United States: Michigan", x == 2223 ~
      "2223. United States: Minnesota", x == 2224 ~ "2224. United States: Missouri", x
    == 2225 ~ "2225. United States: Mississippi", x == 2226 ~ "2226. United States:
Montana", x == 2227 ~ "2227. United States: North Carolina", x == 2228 ~ "2228.
United States: North Dakota", x == 2229 ~ "2229. United States: Nebraska", x ==
      2230 ~ "2230. United States: New Hampshire", x == 2231 ~ "2231. United States:
New Jersey", x == 2232 ~ "2232. United States: New Mexico", x == 2233 ~ "2233.
United States: Nevada", x == 2234 ~ "2234. United States: New York", x == 2235
    ~ "2235. United States: Ohio", x == 2236 ~ "2236. United States: Oklahoma",
    x == 2237 ~ "2237. United States: Oregon", x == 2238 ~ "2238. United States:
Pennsylvania", x == 2239 ~ "2239. United States: Rhode Island", x == 2240 ~
      "2240. United States: South Carolina", x == 2241 ~ "2241. United States: South
Dakota", x == 2242 ~ "2242. United States: Tennessee", x == 2243 ~ "2243.
United States: Texas", x == 2244 ~ "2244. United States: Utah", x == 2245 ~
      "2245. United States: Virginia", x == 2246 ~ "2246. United States: Vermont", x
    == 2247 ~ "2247. United States: Washington", x == 2248 ~ "2248. United States:
Wisconsin", x == 2249 ~ "2249. United States: West Virginia", x == 2250 ~
      "2250. United States: Wyoming", x == 2251 ~ "2251. United States: Other", x
    == 2301 ~ "2301. Sweden: Blekinge", x == 2302 ~ "2302. Sweden: Dalarna", x ==
      2303 ~ "2303. Sweden: Gavleborg", x == 2304 ~ "2304. Sweden: Gotland", x ==
      2305 ~ "2305. Sweden: Halland", x == 2306 ~ "2306. Sweden: Jamtland", x == 2307
    ~ "2307. Sweden: Jonkoping", x == 2308 ~ "2308. Sweden: Kalmar", x == 2309 ~
      "2309. Sweden: Kronoberg", x == 2310 ~ "2310. Sweden: Norrbotten", x == 2311
    ~ "2311. Sweden: Orebro", x == 2312 ~ "2312. Sweden: Ostergotland", x == 2313
    ~ "2313. Sweden: Skane", x == 2314 ~ "2314. Sweden: Sodermanland", x == 2315
    ~ "2315. Sweden: Stockholm", x == 2316 ~ "2316. Sweden: Uppsala", x == 2317 ~
      "2317. Sweden: Varmland", x == 2318 ~ "2318. Sweden: Vasterbotten", x == 2319
    ~ "2319. Sweden: Vasternorrland", x == 2320 ~ "2320. Sweden: Vastmanland", x ==
      2321 ~ "2321. Sweden: Vastra Gotaland", x == 2401 ~ "2401. Hong Kong: Hong Kong
Island", x == 2402 ~ "2402. Hong Kong: Kowloon", x == 2403 ~ "2403. Hong Kong:
New Territories",
    .default = "(Missing)"
    ),
    REGION2_W1 = case_when(x == 101 ~ "101. Argentina: Metropolitana", x == 102 ~ "102.
Argentina: Pampeana", x == 103 ~ "103. Argentina: Cuyo", x == 104 ~ "104.
Argentina: N.O.A. (Noroeste Argentino)", x == 105 ~ "105. Argentina: N.E.A.
(Noreste Argentino)", x == 106 ~ "106. Argentina: Patagonia", x == 201 ~ "201.
Australia: Australian Capital Territory", x == 202 ~ "202. Australia: New
South Wales", x == 203 ~ "203. Australia: Northern Territory", x == 204 ~ "204.
Australia: Queensland", x == 205 ~ "205. Australia: South Australia", x == 206
    ~ "206. Australia: Tasmania", x == 207 ~ "207. Australia: Victoria", x == 208
    ~ "208. Australia: Western Australia", x == 301 ~ "301. Brazil: South (SU)", x
    == 302 ~ "302. Brazil: Southeast (SE)", x == 303 ~ "303. Brazil: Central-West
(CO)", x == 304 ~ "304. Brazil: North (NO)", x == 305 ~ "305. Brazil:
Northeast (NE)", x == 401 ~ "401. Egypt: Urban Governorates", x == 402 ~ "402.
Egypt: Northern Governorates (Nile Delta)", x == 403 ~ "403. Egypt: Southern
Governorates (Upper Egypt)", x == 404 ~ "404. Egypt: Frontier Governorates",
    x == 501 ~ "501. Germany: Schleswig-Holstein", x == 502 ~ "502. Germany:
Hamburg", x == 503 ~ "503. Germany: Niedersachsen", x == 504 ~ "504. Germany:
Bremen", x == 505 ~ "505. Germany: Nordrhein-Westfalen", x == 506 ~ "506.
Germany: Hessen", x == 507 ~ "507. Germany: Rheinland-Pfalz", x == 508 ~ "508.
Germany: Baden-Wurttemberg", x == 509 ~ "509. Germany: Bayern", x == 510 ~ "510.
Germany: Saarland", x == 511 ~ "511. Germany: Berlin", x == 512 ~ "512. Germany:
Brandenburg", x == 513 ~ "513. Germany: Mecklenburg-Vorpommern", x == 514 ~
      "514. Germany: Sachsen", x == 515 ~ "515. Germany: Sachsen-Anhalt", x == 516 ~
      "516. Germany: Thuringen", x == 601 ~ "601. India: Central", x == 602 ~ "602.
India: East", x == 603 ~ "603. India: West", x == 604 ~ "604. India: North", x
    == 605 ~ "605. India: South", x == 701 ~ "701. Indonesia: Sumatra", x == 702 ~
      "702. Indonesia: DKI Jakarta, Banten, West Java", x == 703 ~ "703. Indonesia:
Central Java and DI Yogjakarta", x == 704 ~ "704. Indonesia: East Java", x ==
      705 ~ "705. Indonesia: Kalimantan", x == 706 ~ "706. Indonesia: Sulawesi", x ==
      707 ~ "707. Indonesia: Bali, NTB, NTT", x == 708 ~ "708. Indonesia: Indonesia
Timur", x == 901 ~ "901. Japan: Hokkaido", x == 902 ~ "902. Japan: Tohoku", x ==
      903 ~ "903. Japan: Kanto", x == 904 ~ "904. Japan: Hokuriku", x == 905 ~ "905.
Japan: Koshinetsu", x == 906 ~ "906. Japan: Tokai", x == 907 ~ "907. Japan:
Chukyo", x == 908 ~ "908. Japan: Kansai", x == 909 ~ "909. Japan: Chugoku", x ==
      910 ~ "910. Japan: Shikoku", x == 911 ~ "911. Japan: Kyushu", x == 1001 ~ "1001.
Kenya: Nairobi Province", x == 1002 ~ "1002. Kenya: Central Province", x == 1003
    ~ "1003. Kenya: Eastern Province", x == 1004 ~ "1004. Kenya: Nyanza Province", x
    == 1005 ~ "1005. Kenya: Rift Valley Province", x == 1006 ~ "1006. Kenya: Western
Province", x == 1007 ~ "1007. Kenya: North Eastern Province", x == 1008 ~ "1008.
Kenya: Coast Province", x == 1101 ~ "1101. Mexico: Norte (North)", x == 1102
    ~ "1102. Mexico: Sur (South)", x == 1103 ~ "1103. Mexico: Centro (Central)",
    x == 1201 ~ "1201. Nigeria: North Central", x == 1202 ~ "1202. Nigeria: North
East", x == 1203 ~ "1203. Nigeria: North West", x == 1204 ~ "1204. Nigeria:
South East", x == 1205 ~ "1205. Nigeria: South South", x == 1206 ~ "1206.
Nigeria: South West", x == 1301 ~ "1301. Philippines: NCR", x == 1303 ~ "1303.
Philippines: Balance Luzon", x == 1304 ~ "1304. Philippines: Visayas", x == 1305
    ~ "1305. Philippines: Mindanao", x == 1401 ~ "1401. Poland: Central region", x
    == 1402 ~ "1402. Poland: South region", x == 1403 ~ "1403. Poland: East region",
    x == 1404 ~ "1404. Poland: Northwest region", x == 1405 ~ "1405. Poland:
Southwest region", x == 1406 ~ "1406. Poland: North region", x == 1407 ~ "1407.
Poland: Masovian", x == 1701 ~ "1701. Spain: North-West", x == 1702 ~ "1702.
Spain: North-East", x == 1703 ~ "1703. Spain: Community of Madrid", x == 1704
    ~ "1704. Spain: Centre", x == 1705 ~ "1705. Spain: East", x == 1706 ~ "1706.
Spain: South", x == 1707 ~ "1707. Spain: The Canaries", x == 1801 ~ "1801.
Tanzania: Central", x == 1802 ~ "1802. Tanzania: Coastal", x == 1803 ~ "1803.
Tanzania: Islands", x == 1804 ~ "1804. Tanzania: Lake", x == 1805 ~ "1805.
Tanzania: Mountain", x == 1806 ~ "1806. Tanzania: Northern", x == 1807 ~ "1807.
Tanzania: Southern", x == 1808 ~ "1808. Tanzania: Western", x == 1901 ~ "1901.
Turkey: Istanbul Region", x == 1902 ~ "1902. Turkey: West Marmara Region", x ==
      1903 ~ "1903. Turkey: Aegean Region", x == 1904 ~ "1904. Turkey: East Marmara
Region", x == 1905 ~ "1905. Turkey: West Anatolia Region", x == 1906 ~ "1906.
Turkey: Mediterranean Region", x == 1907 ~ "1907. Turkey: Central Anatolia
Region", x == 1908 ~ "1908. Turkey: West Black Sea Region", x == 1909 ~ "1909.
Turkey: East Black Sea Region", x == 1910 ~ "1910. Turkey: Northeast Anatolia
Region", x == 1911 ~ "1911. Turkey: Central East Anatolia Region", x == 1912 ~
      "1912. Turkey: Southeast Anatolia Region", x == 2001 ~ "2001. United Kingdom:
East Midlands", x == 2002 ~ "2002. United Kingdom: East of England", x == 2003
    ~ "2003. United Kingdom: London", x == 2004 ~ "2004. United Kingdom: North
East", x == 2005 ~ "2005. United Kingdom: North West", x == 2006 ~ "2006. United
Kingdom: Scotland", x == 2007 ~ "2007. United Kingdom: South East", x == 2008 ~
      "2008. United Kingdom: South West", x == 2009 ~ "2009. United Kingdom: Wales",
    x == 2010 ~ "2010. United Kingdom: West Midlands", x == 2011 ~ "2011. United
Kingdom: Yorkshire and The Humber", x == 2012 ~ "2012. United Kingdom: Northern
Ireland", x == 2201 ~ "2201. United States: New England Division", x == 2202
    ~ "2202. United States: Middle Atlantic Division", x == 2203 ~ "2203. United
States: East North Division", x == 2204 ~ "2204. United States: West North
Division", x == 2205 ~ "2205. United States: South Atlantic Division", x == 2206
    ~ "2206. United States: East South Division", x == 2207 ~ "2207. United States:
West South Division", x == 2208 ~ "2208. United States: Mountain Division", x
    == 2209 ~ "2209. United States: Pacific Division", x == 2301 ~ "2301. Sweden:
Stockholm", x == 2302 ~ "2302. Sweden: Ostra Mellansverige", x == 2303 ~ "2303.
Sweden: Sydsverige", x == 2304 ~ "2304. Sweden: Norra Mellansverige", x == 2305
    ~ "2305. Sweden: Mellersta Norrland", x == 2306 ~ "2306. Sweden: Ovre Norrland",
    x == 2307 ~ "2307. Sweden: Smaland med oarna", x == 2308 ~ "2308. Sweden:
Vastverige",
    .default = "(Missing)"
    ),
    REGION3_W1 = case_when(x == 1901 ~ "1901. Turkey: Aegean Region (Ege Bolgesi)", x == 1902 ~
      "1902. Turkey: Black Sea Region (Karadeniz Bolgesi)", x == 1903 ~ "1903. Turkey:
Central Anatolia Region (Ic Anadolu Bolgesi)", x == 1904 ~ "1904. Turkey:
Eastern Anatolia Region (Dogu Anadolu Bolgesi)", x == 1905 ~ "1905. Turkey:
Marmara Region (Marmara Bolgesi)", x == 1906 ~ "1906. Turkey: Mediterranean
Region (Akdeniz Bolgesi)", x == 1907 ~ "1907. Turkey: Southeastern Anatolia
Region (Guneydogu Anadolu Bolgesi)", x == 2201 ~ "2201. United States: Northeast
Region", x == 2202 ~ "2202. United States: Midwest Region", x == 2203 ~
      "2203. United States: South Region", x == 2204 ~ "2204. United States: West
Region", .default = "(Missing)"),
    REL_EXPERIENC_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    REL_IMPORTANT_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    REL1_W1 = case_when(x == 1 ~ "1. Christianity", x == 2 ~ "2. Islam", x == 3 ~ "3.
Hinduism", x == 4 ~ "4. Buddhism", x == 5 ~ "5. Judaism", x == 6 ~ "6.
Sikhism", x == 7 ~ "7. Baha’i", x == 8 ~ "8. Jainism", x == 9 ~ "9. Shinto",
      x == 10 ~ "10. Taoism", x == 11 ~ "11. Confucianism", x == 12 ~ "12. Primal,
Animist, or Folk religion", x == 13 ~ "13. Spiritism", x == 14 ~ "14. Umbanda,
Candomblé, and other African-derived religions", x == 15 ~ "15. Chinese
folk/traditional religion", x == 96 ~ "96. Some other religion", x == 97 ~ "97.
No religion/Atheist/Agnostic",
      .default = "(Missing)"
    ),
    REL2_W1 = case_when(x == 1 ~ "1. Christianity", x == 2 ~ "2. Islam", x == 3 ~ "3.
Hinduism", x == 4 ~ "4. Buddhism", x == 5 ~ "5. Judaism", x == 6 ~ "6.
Sikhism", x == 7 ~ "7. Baha’i", x == 8 ~ "8. Jainism", x == 9 ~ "9. Shinto",
      x == 10 ~ "10. Taoism", x == 11 ~ "11. Confucianism", x == 12 ~ "12. Primal,
Animist, or Folk religion", x == 13 ~ "13. Spiritism", x == 14 ~ "14. Umbanda,
Candomblé, and other African-derived religions", x == 15 ~ "15. Chinese
folk/traditional religion", x == 96 ~ "96. Some other religion", x == 97 ~ "97.
No religion/Atheist/Agnostic",
      .default = "(Missing)"
    ),
    REL3_W1 = case_when(x == 1 ~ "1. Catholic", x == 2 ~ "2. Orthodox", x == 3 ~ "3.
Anglican/Episcopal", x == 4 ~ "4. Presbyterian/Reformed, such as [insert
country specific examples]", x == 5 ~ "5. Lutheran", x == 6 ~ "6. Methodist",
      x == 7 ~ "7. Baptist, such as [insert country specific examples]", x == 8 ~
        "8. Pentecostal/Charismatic denominations, such as [insert country specific
examples]", x == 9 ~ "9. Independent Church, Holiness, or Evangelical, such as
[insert country specific examples]", x == 10 ~ "10. Church of Jesus Christ of
Latter-Day Saints/Other Mormon tradition", x == 11 ~ "11. Jehovah’s Witness",
      x == 12 ~ "12. Seventh Day Adventist", x == 13 ~ "13. Prophetic, Ethiopian,
or Zionist (AIC, African Initiated Church)", x == 96 ~ "96. Some other
denomination", x == 97 ~ "97. No denomination in particular (just Christian/just
Protestant)",
      .default = "(Missing)"
    ),
    REL4_W1 = case_when(x == 1 ~ "1. Sunni", x == 2 ~ "2. Shi’a", x == 3 ~ "3. Sufis", x ==
      4 ~ "4. Bohra", x == 5 ~ "5. Ahmadiyya", x == 6 ~ "6. Khojas", x == 7 ~ "7.
Quranists", x == 8 ~ "8. Alevi", x == 96 ~ "96. Some other sect", x == 97 ~ "97.
No sect in particular (just Muslim)", .default = "(Missing)"),
    REL5_W1 = case_when(x == 1 ~ "1. Vaishnavite", x == 2 ~ "2. Shaivite", x == 3 ~ "3.
Shakta", x == 4 ~ "4. Smarta", x == 96 ~ "96. Some other sect or denomination",
      x == 97 ~ "97. No sect or denomination in particular (just Hindu)",
      .default =
        "(Missing)"
    ),
    REL6_W1 = case_when(x == 1 ~ "1. Secular", x == 2 ~ "2. Reform", x == 3 ~ "3.
Conservative", x == 4 ~ "4. Orthodox", x == 5 ~ "5. Non-Orthodox", x == 6 ~
      "6. Hiloni", x == 7 ~ "7. Masorti lo dati", x == 8 ~ "8. Masorti dati", x ==
      9 ~ "9. Dati", x == 10 ~ "10. Haredi", x == 96 ~ "96. Some other denomination
or tradition (Please specify)", x == 97 ~ "97. No denomination or tradition in
particular (just Jewish)", .default = "(Missing)"),
    REL7_W1 = case_when(x == 1 ~ "1. Atheist – do not believe in any god", x == 2 ~ "2.
Agnostic – unsure whether a God or gods exist", x == 3 ~ "3. Neither",
      .default = "(Missing)"
    ),
    REL8_W1 = case_when(x == 1 ~ "1. Spiritual", x == 2 ~ "2. Religious", x == 3 ~ "3. Both",
      x == 4 ~ "4. Neither",
      .default = "(Missing)"
    ),
    REL9_W1 = case_when(x == 1 ~ "1. Jodo sect (Honen)", x == 2 ~ "2. Jodoshin sect
(Shinran)", x == 3 ~ "3. Rinzai sect (Eisai)", x == 4 ~ "4. Soto sect (Dogen)",
      x == 5 ~ "5. Ji sect (Ippen)", x == 6 ~ "6. Hokke sect (the Nichiren sect,
Nichiren)", x == 96 ~ "96. Some other sect", x == 97 ~ "97. No sect in
particular",
      .default = "(Missing)"
    ),
    SACRED_TEXTS_W1 = case_when(x == 1 ~ "1. More than once a day", x == 2 ~ "2. About once a day", x
    == 3 ~ "3. Sometimes", x == 4 ~ "4. Never", .default = "(Missing)"),
    SAT_LIVE_W1 = case_when(x == 1 ~ "1. Satisfied", x == 2 ~ "2. Dissatisfied", x == 3 ~ "3.
Unsure", .default = "(Missing)"),
    SAT_RELATNSHP_W1 = x,
    SAY_IN_GOVT_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3.
Unsure", .default = "(Missing)"),
    SELFID1_W1 = case_when(x == 101 ~ "101. Argentina: Asian", x == 102 ~ "102. Argentina:
Black", x == 103 ~ "103. Argentina: Indigenous", x == 104 ~ "104. Argentina:
Mestizo(a)", x == 105 ~ "105. Argentina: Mullato(a)", x == 106 ~ "106.
Argentina: White", x == 201 ~ "201. Australia: Aboriginal", x == 202
    ~ "202. Australia: Australian", x == 203 ~ "203. Australia: Australian
British/European", x == 204 ~ "204. Australia: Chinese", x == 205 ~ "205.
Australia: Indian", x == 206 ~ "206. Australia: Japanese", x == 207 ~ "207.
Australia: Malay", x == 208 ~ "208. Australia: Sinhalese", x == 209 ~ "209.
Australia: Spanish", x == 210 ~ "210. Australia: Sri Lankan Moor", x == 211 ~
      "211. Australia: Sri Lankan Tamil", x == 212 ~ "212. Australia: Vietnamese", x
    == 213 ~ "213. Australia: Taiwanese/Holo", x == 214 ~ "214. Australia: Russian",
    x == 215 ~ "215. Australia: Samoan", x == 216 ~ "216. Australia: New Zealander",
    x == 217 ~ "217. Australia: Other European", x == 301 ~ "301. Brazil: Branca",
    x == 302 ~ "302. Brazil: Preta", x == 303 ~ "303. Brazil: Parda", x == 304 ~
      "304. Brazil: Amarela", x == 305 ~ "305. Brazil: Indígena", x == 401 ~ "401.
Egypt: Arab", x == 402 ~ "402. Egypt: Turkish", x == 403 ~ "403. Egypt: Greek",
    x == 404 ~ "404. Egypt: Abazas", x == 405 ~ "405. Egypt: Bedouin Arab", x ==
      406 ~ "406. Egypt: Siwis", x == 407 ~ "407. Egypt: Nubian", x == 601 ~ "601.
India: General", x == 602 ~ "602. India: Other backward caste", x == 603 ~
      "603. India: Schedule caste", x == 604 ~ "604. India: Schedule tribe", x == 701
    ~ "701. Indonesia: Banjar/Melayu Banjar", x == 702 ~ "702. Indonesia: Betawi",
    x == 703 ~ "703. Indonesia: Bugis", x == 704 ~ "704. Indonesia: Jawa", x ==
      705 ~ "705. Indonesia: Madura", x == 706 ~ "706. Indonesia: Minangkabau", x ==
      707 ~ "707. Indonesia: Sunda/Parahyangan", x == 708 ~ "708. Indonesia: Bali",
    x == 709 ~ "709. Indonesia: Batak", x == 710 ~ "710. Indonesia: Makasar", x ==
      801 ~ "801. Israel: Jewish", x == 802 ~ "802. Israel: Arab", x == 901 ~ "901.
Japan: Chinese", x == 902 ~ "902. Japan: Japanese", x == 903 ~ "903. Japan:
Korean", x == 1001 ~ "1001. Kenya: Luhya", x == 1002 ~ "1002. Kenya: Luo", x
    == 1003 ~ "1003. Kenya: Kalenjin", x == 1004 ~ "1004. Kenya: Kamba", x == 1005
    ~ "1005. Kenya: Kikuyu", x == 1006 ~ "1006. Kenya: Kisii", x == 1007 ~ "1007.
Kenya: Maasai", x == 1008 ~ "1008. Kenya: Meru", x == 1009 ~ "1009. Kenya:
Kenyan Somali/Somali", x == 1010 ~ "1010. Kenya: Miji Kenda tribes", x == 1011
    ~ "1011. Kenya: Embu", x == 1101 ~ "1101. Mexico: White", x == 1102 ~ "1102.
Mexico: Mestizo", x == 1103 ~ "1103. Mexico: Indigenous", x == 1104 ~ "1104.
Mexico: Black", x == 1105 ~ "1105. Mexico: Mulatto", x == 1201 ~ "1201. Nigeria:
Hausa", x == 1202 ~ "1202. Nigeria: Yoruba", x == 1203 ~ "1203. Nigeria: Igbo
(Ibo)", x == 1204 ~ "1204. Nigeria: Edo", x == 1205 ~ "1205. Nigeria: Urhobo",
    x == 1206 ~ "1206. Nigeria: Fulani", x == 1207 ~ "1207. Nigeria: Kanuri",
    x == 1208 ~ "1208. Nigeria: Tiv", x == 1209 ~ "1209. Nigeria: Efik", x ==
      1210 ~ "1210. Nigeria: Ijaw", x == 1211 ~ "1211. Nigeria: Igala", x == 1212
    ~ "1212. Nigeria: Ibibio", x == 1213 ~ "1213. Nigeria: Idoma", x == 1301 ~
      "1301. Philippines: Tagalog", x == 1302 ~ "1302. Philippines: Cebuano", x ==
      1303 ~ "1303. Philippines: Ilocano/Ilokano", x == 1304 ~ "1304. Philippines:
Visayan/Bisaya", x == 1305 ~ "1305. Philippines: Ilonggo/Hiligaynon", x ==
      1306 ~ "1306. Philippines: Bicolano/Bikolano", x == 1307 ~ "1307. Philippines:
Waray", x == 1308 ~ "1308. Philippines: Tausug", x == 1309 ~ "1309. Philippines:
Maranao", x == 1310 ~ "1310. Philippines: Maguindanaoan", x == 1311 ~ "1311.
Philippines: Chinese-Filipino", x == 1312 ~ "1312. Philippines: Kapampangan",
    x == 1313 ~ "1313. Philippines: Pangasinense", x == 1314 ~ "1314. Philippines:
Zamboangueno", x == 1315 ~ "1315. Philippines: Malay", x == 1316 ~ "1316.
Philippines: Masbateno", x == 1317 ~ "1317. Philippines: Aeta", x == 1318 ~
      "1318. Philippines: Igorot", x == 1319 ~ "1319. Philippines: Mangyan", x ==
      1320 ~ "1320. Philippines: Badjao", x == 1401 ~ "1401. Poland: Polish", x ==
      1402 ~ "1402. Poland: German", x == 1403 ~ "1403. Poland: Belarussian", x ==
      1404 ~ "1404. Poland: Ukrainian", x == 1405 ~ "1405. Poland: Roma", x == 1406
    ~ "1406. Poland: Russian", x == 1407 ~ "1407. Poland: Ethnic Jewish", x ==
      1408 ~ "1408. Poland: Lemko", x == 1409 ~ "1409. Poland: Silesia", x == 1410 ~
      "1410. Poland: Kashubians", x == 1601 ~ "1601. South Africa: Black", x == 1602
    ~ "1602. South Africa: Asian/Indian", x == 1603 ~ "1603. South Africa: Colored",
    x == 1604 ~ "1604. South Africa: White", x == 1801 ~ "1801. Tanzania: African",
    x == 1802 ~ "1802. Tanzania: Indian", x == 1803 ~ "1803. Tanzania: Arab", x
    == 1901 ~ "1901. Turkey: Turkish", x == 1902 ~ "1902. Turkey: Kurdish/Zaza",
    x == 1903 ~ "1903. Turkey: Arab", x == 1904 ~ "1904. Turkey: Laz", x == 1905
    ~ "1905. Turkey: Circassian", x == 1906 ~ "1906. Turkey: Bosnian", x == 1907
    ~ "1907. Turkey: Armenian", x == 1908 ~ "1908. Turkey: Georgian", x == 1909 ~
      "1909. Turkey: Uyghur", x == 1910 ~ "1910. Turkey: Jewish", x == 1911 ~ "1911.
Turkey: Albanian", x == 1912 ~ "1912. Turkey: Greek", x == 1913 ~ "1913. Turkey:
Azeri", x == 2001 ~ "2001. United Kingdom: Asian", x == 2002 ~ "2002. United
Kingdom: Black", x == 2003 ~ "2003. United Kingdom: White", x == 2201 ~ "2201.
United States: White", x == 2202 ~ "2202. United States: Other", x == 2203 ~
      "2203. United States: Black", x == 2204 ~ "2204. United States: Asian", x ==
      2205 ~ "2205. United States: Hispanic", x == 2401 ~ "2401. Hong Kong: Chinese
(Cantonese)", x == 2402 ~ "2402. Hong Kong: Chinese (Chaoshan)", x == 2403 ~
      "2403. Hong Kong: Chinese (Fujianese)", x == 2404 ~ "2404. Hong Kong: Chinese
(Hakka)", x == 2405 ~ "2405. Hong Kong: Chinese (Shanghainese)", x == 2406 ~
      "2406. Hong Kong: Chinese (Other ethnicity)", x == 2407 ~ "2407. Hong Kong:
East Asian (Korean, Japanese)", x == 2408 ~ "2408. Hong Kong: Southeast Asian
(Filipino, Indonesian, Thailand)", x == 2409 ~ "2409. Hong Kong: South Asian
(Indian, Nepalese, Pakistani)", x == 2410 ~ "2410. Hong Kong: Taiwanese", x ==
      2411 ~ "2411. Hong Kong: White", x == 9995 ~ "9995. Prefer not to answer", x
    == 9996 ~ "9996. Other", x == 9997 ~ "9997. (No other response)",
    .default =
      "(Missing)"
    ),
    SELFID2_W1 = case_when(x == 101 ~ "101. Argentina: Asian", x == 102 ~ "102. Argentina:
Black", x == 103 ~ "103. Argentina: Indigenous", x == 104 ~ "104. Argentina:
Mestizo(a)", x == 105 ~ "105. Argentina: Mullato(a)", x == 106 ~ "106.
Argentina: White", x == 201 ~ "201. Australia: Aboriginal", x == 202
    ~ "202. Australia: Australian", x == 203 ~ "203. Australia: Australian
British/European", x == 204 ~ "204. Australia: Chinese", x == 205 ~ "205.
Australia: Indian", x == 206 ~ "206. Australia: Japanese", x == 207 ~ "207.
Australia: Malay", x == 208 ~ "208. Australia: Sinhalese", x == 209 ~ "209.
Australia: Spanish", x == 210 ~ "210. Australia: Sri Lankan Moor", x == 211 ~
      "211. Australia: Sri Lankan Tamil", x == 212 ~ "212. Australia: Vietnamese", x
    == 213 ~ "213. Australia: Taiwanese/Holo", x == 214 ~ "214. Australia: Russian",
    x == 215 ~ "215. Australia: Samoan", x == 216 ~ "216. Australia: New Zealander",
    x == 217 ~ "217. Australia: Other European", x == 301 ~ "301. Brazil: Branca",
    x == 302 ~ "302. Brazil: Preta", x == 303 ~ "303. Brazil: Parda", x == 304 ~
      "304. Brazil: Amarela", x == 305 ~ "305. Brazil: Indígena", x == 401 ~ "401.
Egypt: Arab", x == 402 ~ "402. Egypt: Turkish", x == 403 ~ "403. Egypt: Greek",
    x == 404 ~ "404. Egypt: Abazas", x == 405 ~ "405. Egypt: Bedouin Arab", x ==
      406 ~ "406. Egypt: Siwis", x == 407 ~ "407. Egypt: Nubian", x == 601 ~ "601.
India: General", x == 602 ~ "602. India: Other backward caste", x == 603 ~
      "603. India: Schedule caste", x == 604 ~ "604. India: Schedule tribe", x == 701
    ~ "701. Indonesia: Banjar/Melayu Banjar", x == 702 ~ "702. Indonesia: Betawi",
    x == 703 ~ "703. Indonesia: Bugis", x == 704 ~ "704. Indonesia: Jawa", x ==
      705 ~ "705. Indonesia: Madura", x == 706 ~ "706. Indonesia: Minangkabau", x ==
      707 ~ "707. Indonesia: Sunda/Parahyangan", x == 708 ~ "708. Indonesia: Bali",
    x == 709 ~ "709. Indonesia: Batak", x == 710 ~ "710. Indonesia: Makasar", x ==
      801 ~ "801. Israel: Jewish", x == 802 ~ "802. Israel: Arab", x == 901 ~ "901.
Japan: Chinese", x == 902 ~ "902. Japan: Japanese", x == 903 ~ "903. Japan:
Korean", x == 1001 ~ "1001. Kenya: Luhya", x == 1002 ~ "1002. Kenya: Luo", x
    == 1003 ~ "1003. Kenya: Kalenjin", x == 1004 ~ "1004. Kenya: Kamba", x == 1005
    ~ "1005. Kenya: Kikuyu", x == 1006 ~ "1006. Kenya: Kisii", x == 1007 ~ "1007.
Kenya: Maasai", x == 1008 ~ "1008. Kenya: Meru", x == 1009 ~ "1009. Kenya:
Kenyan Somali/Somali", x == 1010 ~ "1010. Kenya: Miji Kenda tribes", x == 1011
    ~ "1011. Kenya: Embu", x == 1101 ~ "1101. Mexico: White", x == 1102 ~ "1102.
Mexico: Mestizo", x == 1103 ~ "1103. Mexico: Indigenous", x == 1104 ~ "1104.
Mexico: Black", x == 1105 ~ "1105. Mexico: Mulatto", x == 1201 ~ "1201. Nigeria:
Hausa", x == 1202 ~ "1202. Nigeria: Yoruba", x == 1203 ~ "1203. Nigeria: Igbo
(Ibo)", x == 1204 ~ "1204. Nigeria: Edo", x == 1205 ~ "1205. Nigeria: Urhobo",
    x == 1206 ~ "1206. Nigeria: Fulani", x == 1207 ~ "1207. Nigeria: Kanuri",
    x == 1208 ~ "1208. Nigeria: Tiv", x == 1209 ~ "1209. Nigeria: Efik", x ==
      1210 ~ "1210. Nigeria: Ijaw", x == 1211 ~ "1211. Nigeria: Igala", x == 1212
    ~ "1212. Nigeria: Ibibio", x == 1213 ~ "1213. Nigeria: Idoma", x == 1301 ~
      "1301. Philippines: Tagalog", x == 1302 ~ "1302. Philippines: Cebuano", x ==
      1303 ~ "1303. Philippines: Ilocano/Ilokano", x == 1304 ~ "1304. Philippines:
Visayan/Bisaya", x == 1305 ~ "1305. Philippines: Ilonggo/Hiligaynon", x ==
      1306 ~ "1306. Philippines: Bicolano/Bikolano", x == 1307 ~ "1307. Philippines:
Waray", x == 1308 ~ "1308. Philippines: Tausug", x == 1309 ~ "1309. Philippines:
Maranao", x == 1310 ~ "1310. Philippines: Maguindanaoan", x == 1311 ~ "1311.
Philippines: Chinese-Filipino", x == 1312 ~ "1312. Philippines: Kapampangan",
    x == 1313 ~ "1313. Philippines: Pangasinense", x == 1314 ~ "1314. Philippines:
Zamboangueno", x == 1315 ~ "1315. Philippines: Malay", x == 1316 ~ "1316.
Philippines: Masbateno", x == 1317 ~ "1317. Philippines: Aeta", x == 1318 ~
      "1318. Philippines: Igorot", x == 1319 ~ "1319. Philippines: Mangyan", x ==
      1320 ~ "1320. Philippines: Badjao", x == 1401 ~ "1401. Poland: Polish", x ==
      1402 ~ "1402. Poland: German", x == 1403 ~ "1403. Poland: Belarussian", x ==
      1404 ~ "1404. Poland: Ukrainian", x == 1405 ~ "1405. Poland: Roma", x == 1406
    ~ "1406. Poland: Russian", x == 1407 ~ "1407. Poland: Ethnic Jewish", x ==
      1408 ~ "1408. Poland: Lemko", x == 1409 ~ "1409. Poland: Silesia", x == 1410 ~
      "1410. Poland: Kashubians", x == 1601 ~ "1601. South Africa: Black", x == 1602
    ~ "1602. South Africa: Asian/Indian", x == 1603 ~ "1603. South Africa: Colored",
    x == 1604 ~ "1604. South Africa: White", x == 1801 ~ "1801. Tanzania: African",
    x == 1802 ~ "1802. Tanzania: Indian", x == 1803 ~ "1803. Tanzania: Arab", x
    == 1901 ~ "1901. Turkey: Turkish", x == 1902 ~ "1902. Turkey: Kurdish/Zaza",
    x == 1903 ~ "1903. Turkey: Arab", x == 1904 ~ "1904. Turkey: Laz", x == 1905
    ~ "1905. Turkey: Circassian", x == 1906 ~ "1906. Turkey: Bosnian", x == 1907
    ~ "1907. Turkey: Armenian", x == 1908 ~ "1908. Turkey: Georgian", x == 1909 ~
      "1909. Turkey: Uyghur", x == 1910 ~ "1910. Turkey: Jewish", x == 1911 ~ "1911.
Turkey: Albanian", x == 1912 ~ "1912. Turkey: Greek", x == 1913 ~ "1913. Turkey:
Azeri", x == 2001 ~ "2001. United Kingdom: Asian", x == 2002 ~ "2002. United
Kingdom: Black", x == 2003 ~ "2003. United Kingdom: White", x == 2201 ~ "2201.
United States: White", x == 2202 ~ "2202. United States: Other", x == 2203 ~
      "2203. United States: Black", x == 2204 ~ "2204. United States: Asian", x ==
      2205 ~ "2205. United States: Hispanic", x == 2401 ~ "2401. Hong Kong: Chinese
(Cantonese)", x == 2402 ~ "2402. Hong Kong: Chinese (Chaoshan)", x == 2403 ~
      "2403. Hong Kong: Chinese (Fujianese)", x == 2404 ~ "2404. Hong Kong: Chinese
(Hakka)", x == 2405 ~ "2405. Hong Kong: Chinese (Shanghainese)", x == 2406 ~
      "2406. Hong Kong: Chinese (Other ethnicity)", x == 2407 ~ "2407. Hong Kong:
East Asian (Korean, Japanese)", x == 2408 ~ "2408. Hong Kong: Southeast Asian
(Filipino, Indonesian, Thailand)", x == 2409 ~ "2409. Hong Kong: South Asian
(Indian, Nepalese, Pakistani)", x == 2410 ~ "2410. Hong Kong: Taiwanese", x ==
      2411 ~ "2411. Hong Kong: White", x == 9995 ~ "9995. Prefer not to answer", x
    == 9996 ~ "9996. Other", x == 9997 ~ "9997. (No other response)",
    .default =
      "(Missing)"
    ),
    SHOW_LOVE_W1 = x,
    SUFFERING_W1 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    SVCS_12YRS_W1 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never",
      .default =
        "(Missing)"
    ),
    SVCS_FATHER_W1 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never", x == 97 ~
      "97. (Does not apply)", .default = "(Missing)"),
    SVCS_MOTHER_W1 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never", x == 97 ~
      "97. (Does not apply)", .default = "(Missing)"),
    TEACHINGS_1_W1 = x,
    TEACHINGS_2_W1 = x,
    TEACHINGS_3_W1 = x,
    TEACHINGS_4_W1 = x,
    TEACHINGS_5_W1 = x,
    TEACHINGS_6_W1 = x,
    TEACHINGS_7_W1 = x,
    TEACHINGS_8_W1 = x,
    TEACHINGS_9_W1 = x,
    TEACHINGS_10_W1 = x,
    TEACHINGS_11_W1 = x,
    TEACHINGS_12_W1 = x,
    TEACHINGS_13_W1 = x,
    TEACHINGS_14_W1 = x,
    TEACHINGS_15_W1 = x,
    TELL_BELIEFS_W1 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    THREAT_LIFE_W1 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    TRAITS1_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS2_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS3_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS4_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS5_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS6_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS7_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS8_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS9_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS10_W1 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRUST_PEOPLE_W1 = case_when(x == 1 ~ "1. All", x == 2 ~ "2. Most", x == 3 ~ "3. Some", x == 4 ~
      "4. Not very many", x == 5 ~ "5. None", .default = "(Missing)"),
    URBAN_RURAL_W1 = case_when(x == 1 ~ "1. A rural area or on a farm", x == 2 ~ "2. A small town
or village", x == 3 ~ "3. A large city", x == 4 ~ "4. A suburb of a large
city", .default = "(Missing)"),
    VOLUNTEERED_W1 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    WB_FIVEYRS_W1 = x,
    WB_TODAY_W1 = x,
    WORRY_SAFETY_W1 = x,
    WORTHWHILE_W1 = x,
    ANNUAL_WEIGHT1_W1 = x,
    STRATA_W1 = x,
    PSU_W1 = x,
    FULL_PARTIAL_W1 = x,
    COUNTRY_W2 = case_when(x == 1 ~ "1. Argentina", x == 2 ~ "2. Australia", x == 3 ~ "3.
Brazil", x == 4 ~ "4. Egypt", x == 5 ~ "5. Germany", x == 6 ~ "6. India", x
    == 7 ~ "7. Indonesia", x == 8 ~ "8. Israel", x == 9 ~ "9. Japan", x == 10 ~
      "10. Kenya", x == 11 ~ "11. Mexico", x == 12 ~ "12. Nigeria", x == 13 ~ "13.
Philippines", x == 14 ~ "14. Poland", x == 16 ~ "16. South Africa", x == 17 ~
      "17. Spain", x == 18 ~ "18. Tanzania", x == 19 ~ "19. Turkey", x == 20 ~ "20.
United Kingdom", x == 22 ~ "22. United States", x == 23 ~ "23. Sweden", x == 24
    ~ "24. Hong Kong", .default = "(Missing)"),
    WAVE_W2 = x,
    MODE_RECRUIT_W2 = case_when(x == 1 ~ "1. CAPI", x == 2 ~ "2. CATI", x == 3 ~ "3. CAWI",
      .default =
        "(Missing)"
    ),
    MODE_ANNUAL_W2 = case_when(x == 1 ~ "1. CAPI", x == 2 ~ "2. CATI", x == 3 ~ "3. CAWI",
      .default =
        "(Missing)"
    ),
    RECRUIT_TYPE_W2 = case_when(x == 1 ~ "1. Separate Recruitment and Annual Surveys", x == 2 ~ "2.
Combined Recruitment and Annual Surveys", .default = "(Missing)"),
    DOI_RECRUIT_W2 = x,
    DOI_ANNUAL_W2 = x,
    ABUSED_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    AFTER_DEATH_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 3 ~ "3. Unsure",
      .default =
        "(Missing)"
    ),
    AGE_W2 = x,
    APPROVE_GOVT_W2 = case_when(x == 1 ~ "1. Strongly approve", x == 2 ~ "2. Somewhat approve", x == 3
    ~ "3. Neither approve nor disapprove", x == 4 ~ "4. Somewhat disapprove", x == 5
    ~ "5. Strongly disapprove", .default = "(Missing)"),
    ATTEND_SVCS_W2 = case_when(x == 1 ~ "1. More than once a week", x == 2 ~ "2. Once a week", x ==
      3 ~ "3. One to three times a month", x == 4 ~ "4. A few times a year", x == 5 ~
      "5. Never", .default = "(Missing)"),
    BELIEVE_GOD_W2 = case_when(x == 1 ~ "1. One God", x == 2 ~ "2. More than one god", x == 3 ~
      "3. An impersonal spiritual force", x == 4 ~ "4. None of these", x == 5 ~ "5.
Unsure", .default = "(Missing)"),
    BELONGING_W2 = x,
    BODILY_PAIN_W2 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. None at all",
      .default = "(Missing)"
    ),
    BORN_COUNTRY_W2 = case_when(x == 1 ~ "1. Born in this country", x == 2 ~ "2. Born in another
country", .default = "(Missing)"),
    CAPABLE_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    CIGARETTES_W2 = x,
    CLOSE_TO_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    CNTRY_REL_BUD_W2 = x,
    CNTRY_REL_CHI_W2 = x,
    CNTRY_REL_CHR_W2 = x,
    CNTRY_REL_HIN_W2 = x,
    CNTRY_REL_ISL_W2 = x,
    CNTRY_REL_JUD_W2 = x,
    CNTRY_REL_SHI_W2 = x,
    COMFORT_REL_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    CONNECTED_REL_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    CONTENT_W2 = x,
    CONTROL_WORRY_W2 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    COVID_DEATH_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    CRITICAL_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    DAYS_EXERCISE_W2 = case_when(x == 0 ~ "0. 0 days", x == 1 ~ "1. 1 day", x == 2 ~ "2. 2 days", x
    == 3 ~ "3. 3 days", x == 4 ~ "4. 4 days", x == 5 ~ "5. 5 days", x == 6 ~ "6. 6
days", x == 7 ~ "7. 7 days/Every day", .default = "(Missing)"),
    DEPRESSED_W2 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    DISCRIMINATED_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    DONATED_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    DRINKS_W2 = x,
    EDUCATION_W2 = case_when(x == 100 ~ "100. Argentina: (No formal education)", x == 101 ~ "101.
Argentina: Incomplete primary school", x == 102 ~ "102. Argentina: Complete
primary school", x == 103 ~ "103. Argentina: Incomplete secondary school", x ==
      104 ~ "104. Argentina: Complete secondary school", x == 105 ~ "105. Argentina:
Incomplete tertiary school", x == 106 ~ "106. Argentina: Complete tertiary
school", x == 107 ~ "107. Argentina: Incomplete university", x == 108 ~ "108.
Argentina: Complete university", x == 109 ~ "109. Argentina: Post-graduate", x
    == 200 ~ "200. Australia: (No formal education)", x == 201 ~ "201. Australia:
Year 8 or below (or equivalent) (usually 14 years old)", x == 202 ~ "202.
Australia: Year 9 (or equivalent) (usually 15 years old)", x == 203 ~ "203.
Australia: Year 10 (School Certificate SC or equivalent) (usually 16 years
old)", x == 204 ~ "204. Australia: Year 11 (or equivalent) (usually 17 years
old)", x == 205 ~ "205. Australia: Year 12 (Higher School Certificate HSC or
equivalent) (usually 18 years old)", x == 206 ~ "206. Australia: Certificate
(less than 1 year in TAFE or equivalent)", x == 207 ~ "207. Australia: Advanced
Diploma or Diploma (1 year or 2 year Diploma from TAFE or equivalent)", x
    == 208 ~ "208. Australia: Bachelor Degree (University Graduate)", x == 209
    ~ "209. Australia: Graduate Diploma or Graduate Certificate", x == 210 ~
      "210. Australia: Postgraduate Degree (Masters or Ph.D.)", x == 300 ~ "300.
Brazil: (Illiterate/No formal education)", x == 301 ~ "301. Brazil: 1 to 5
years incomplete/Elementary incomplete", x == 302 ~ "302. Brazil: Elementary
complete/Secondary incomplete", x == 303 ~ "303. Brazil: Secondary complete/High
school incomplete", x == 304 ~ "304. Brazil: High school complete/Media
education complete", x == 305 ~ "305. Brazil: Superior studies incomplete", x
    == 306 ~ "306. Brazil: Superior studies complete", x == 400 ~ "400. Egypt: (No
formal education)", x == 401 ~ "401. Egypt: Did not complete primary", x ==
      402 ~ "402. Egypt: Completed primary", x == 403 ~ "403. Egypt: Did not complete
intermediate", x == 404 ~ "404. Egypt: Completed intermediate", x == 405 ~
      "405. Egypt: Did not complete secondary", x == 406 ~ "406. Egypt: Completed
secondary", x == 407 ~ "407. Egypt: College/Did not complete university", x
    == 408 ~ "408. Egypt: Completed university", x == 409 ~ "409. Egypt: Higher
education (Masters degree, Ph.D., etc.)", x == 500 ~ "500. Germany: (No formal
education/training)", x == 501 ~ "501. Germany: Incomplete primary school", x
    == 502 ~ "502. Germany: Primary school (ISCED 1)", x == 503 ~ "503. Germany:
Some secondary school, not completed", x == 504 ~ "504. Germany: General
secondary school (ISCED 2)", x == 505 ~ "505. Germany: Secondary modern
school/Intermediate certificate (ISCED 3)", x == 506 ~ "506. Germany: Vocational
school (ISCED 4)", x == 507 ~ "507. Germany: High school diploma/A-levels, age
11-18/19, grades 5-12/13 - qualifies for college/university", x == 508 ~ "508.
Germany: College/university, university of applied science", x == 601 ~ "601.
India: (Illiterate)", x == 602 ~ "602. India: Below SSC", x == 603 ~ "603.
India: SSC/HSC", x == 604 ~ "604. India: Some college but did not graduate", x
    == 605 ~ "605. India: Graduate/Post Graduate - General", x == 606 ~ "606. India:
Graduate/Post Graduate - Professional", x == 700 ~ "700. Indonesia: (No formal
education)", x == 701 ~ "701. Indonesia: Primary School", x == 702 ~ "702.
Indonesia: Junior High School", x == 703 ~ "703. Indonesia: Senior School", x
    == 704 ~ "704. Indonesia: Diploma", x == 705 ~ "705. Indonesia: University", x
    == 706 ~ "706. Indonesia: Did not attend/complete primary school", x == 800 ~
      "800. Israel: (No formal education)", x == 801 ~ "801. Israel: Up to 8 years",
    x == 802 ~ "802. Israel: 9-11 years", x == 803 ~ "803. Israel: 12 years without
matriculation", x == 804 ~ "804. Israel: 12 years with matriculation", x == 805
    ~ "805. Israel: 13-14 years (non-academic, like technician, practical engineer,
nurse)", x == 806 ~ "806. Israel: 15-16 years (first degree, such as BA, BSC)",
    x == 807 ~ "807. Israel: 17+ years (second degree, such as MA, MSc)", x == 808 ~
      "808. Israel: Ph.D.", x == 901 ~ "901. Japan: (No education)", x == 902 ~ "902.
Japan: Elementary or Junior High", x == 903 ~ "903. Japan: Middle or Senior
High", x == 904 ~ "904. Japan: Higher prof or Junior college", x == 905 ~ "905.
Japan: College or University", x == 906 ~ "906. Japan: Graduate course", x ==
      1000 ~ "1000. Kenya: (No formal education)", x == 1001 ~ "1001. Kenya: First
year of primary education", x == 1002 ~ "1002. Kenya: Second year of primary
education", x == 1003 ~ "1003. Kenya: Third year of primary education", x ==
      1004 ~ "1004. Kenya: Fourth year of primary education", x == 1005 ~ "1005.
Kenya: Fifth year of primary education", x == 1006 ~ "1006. Kenya: Sixth
year of primary education", x == 1007 ~ "1007. Kenya: Seventh year of primary
education", x == 1008 ~ "1008. Kenya: Eighth year of primary education", x ==
      1009 ~ "1009. Kenya: First year of secondary education", x == 1010 ~ "1010.
Kenya: Second year of secondary education", x == 1011 ~ "1011. Kenya: Third
year of secondary education", x == 1012 ~ "1012. Kenya: Fourth year of secondary
education", x == 1013 ~ "1013. Kenya: Fifth year of secondary education", x
    == 1014 ~ "1014. Kenya: Sixth year of secondary education", x == 1015 ~ "1015.
Kenya: First year of technical training (Polytechnic or Diploma colleges)",
    x == 1016 ~ "1016. Kenya: Second year of technical training (Polytechnic or
Diploma colleges)", x == 1017 ~ "1017. Kenya: Third year of technical training
(Polytechnic or Diploma colleges)", x == 1018 ~ "1018. Kenya: First year of
teachers training college", x == 1019 ~ "1019. Kenya: Second year of teachers
training college", x == 1020 ~ "1020. Kenya: Completed education at teachers
training college", x == 1021 ~ "1021. Kenya: Some university education", x
    == 1022 ~ "1022. Kenya: Completed university education", x == 1023 ~ "1023.
Kenya: Completed post-university education", x == 1100 ~ "1100. Mexico: (No
formal education)", x == 1101 ~ "1101. Mexico: Some primary", x == 1102 ~
      "1102. Mexico: Primary complete", x == 1103 ~ "1103. Mexico: Some secondary",
    x == 1104 ~ "1104. Mexico: Secondary complete", x == 1105 ~ "1105. Mexico: High
school/technical career complete", x == 1106 ~ "1106. Mexico: Some University",
    x == 1107 ~ "1107. Mexico: University complete", x == 1108 ~ "1108. Mexico:
Post-graduate", x == 1200 ~ "1200. Nigeria: (No formal education)", x ==
      1201 ~ "1201. Nigeria: First year of primary education", x == 1202 ~ "1202.
Nigeria: Second year of primary education", x == 1203 ~ "1203. Nigeria: Third
year of primary education", x == 1204 ~ "1204. Nigeria: Fourth year of primary
education", x == 1205 ~ "1205. Nigeria: Fifth year of primary education", x
    == 1206 ~ "1206. Nigeria: Sixth year of primary education", x == 1207 ~ "1207.
Nigeria: First year of secondary education", x == 1208 ~ "1208. Nigeria:
Second year of secondary education", x == 1209 ~ "1209. Nigeria: Third year
of secondary education", x == 1210 ~ "1210. Nigeria: Fourth year of secondary
education", x == 1211 ~ "1211. Nigeria: Fifth year of secondary education",
    x == 1212 ~ "1212. Nigeria: Sixth year of secondary education", x == 1213 ~
      "1213. Nigeria: First year of technical training", x == 1214 ~ "1214. Nigeria:
Second year of technical training", x == 1215 ~ "1215. Nigeria: Third year of
technical training", x == 1216 ~ "1216. Nigeria: First year of teacher training
college", x == 1217 ~ "1217. Nigeria: Second year of teacher training college",
    x == 1218 ~ "1218. Nigeria: Third year of teacher training college", x == 1219
    ~ "1219. Nigeria: Fourth year of teacher training college", x == 1220 ~ "1220.
Nigeria: Some education at a Technical College or the Institute of Teachers
Education", x == 1221 ~ "1221. Nigeria: Some tertiary education, including
Universities, Polytechnics, and Teacher Training College", x == 1222 ~ "1222.
Nigeria: Post University education", x == 1301 ~ "1301. Philippines: (None/Did
not undergo formal schooling)", x == 1302 ~ "1302. Philippines: Preschool", x
    == 1303 ~ "1303. Philippines: Elementary", x == 1304 ~ "1304. Philippines: High
school", x == 1305 ~ "1305. Philippines: Postsecondary", x == 1306 ~ "1306.
Philippines: College undergraduate", x == 1307 ~ "1307. Philippines: Academic
degree holder", x == 1308 ~ "1308. Philippines: Post-baccalaureate", x == 1400
    ~ "1400. Poland: (No formal education)", x == 1401 ~ "1401. Poland: Less than
4 classes elementary education", x == 1402 ~ "1402. Poland: At least 4 classes
elementary education", x == 1403 ~ "1403. Poland: Completed elementary school,
6 or 8 classes", x == 1404 ~ "1404. Poland: Gimnazium", x == 1405 ~ "1405.
Poland: Basic Vocational", x == 1406 ~ "1406. Poland: Secondary degree, Liceum
Technikum", x == 1407 ~ "1407. Poland: Post-secondary", x == 1408 ~ "1408.
Poland: High diploma (university) with BA, MA, Engineer, or other equivalent
title", x == 1409 ~ "1409. Poland: Doctoral degree or higher", x == 1600 ~
      "1600. South Africa: (No formal education)", x == 1601 ~ "1601. South Africa:
First year of primary education", x == 1602 ~ "1602. South Africa: Second
year of primary education", x == 1603 ~ "1603. South Africa: Third year of
primary education", x == 1604 ~ "1604. South Africa: Fourth year of primary
education", x == 1605 ~ "1605. South Africa: Fifth year of primary education",
    x == 1606 ~ "1606. South Africa: Sixth year of primary education", x == 1607
    ~ "1607. South Africa: Seventh year of primary education", x == 1608 ~ "1608.
South Africa: First year of secondary education (Grade 8)", x == 1609 ~ "1609.
South Africa: Second year of secondary education (Grade 9)", x == 1610 ~ "1610.
South Africa: Third year of secondary education (Grade 10)", x == 1611 ~ "1611.
South Africa: Fourth year of secondary education (Grade 11)", x == 1612 ~
      "1612. South Africa: Fifth year of secondary education (Grade 12)", x == 1613
    ~ "1613. South Africa: Some tertiary education (college, university)", x ==
      1614 ~ "1614. South Africa: Completed tertiary education (college, university
complete)", x == 1615 ~ "1615. South Africa: Post university education", x ==
      1700 ~ "1700. Spain: (No formal education)", x == 1701 ~ "1701. Spain: Primary
Incomplete (less than 5 years)", x == 1702 ~ "1702. Spain: Primary Complete,
First level of EGB", x == 1703 ~ "1703. Spain: Secondary School Incomplete", x
    == 1704 ~ "1704. Spain: Second level of EGB, Secondary School Graduate or ESO
complete (Certificate of success in EGB course)", x == 1705 ~ "1705. Spain:
Secondary School Certificate, FP1 (Vocational Training I)", x == 1706 ~ "1706.
Spain: Secondary School graduate LOGSE, COU, Pre University, FP2 (Vocational
Training II)", x == 1707 ~ "1707. Spain: First Stage of University Degree
(University Diploma Course or 3 complete years of University)", x == 1708 ~
      "1708. Spain: University or Engineering degree or High Level Technician", x
    == 1709 ~ "1709. Spain: Doctorate", x == 1800 ~ "1800. Tanzania: (No formal
education)", x == 1801 ~ "1801. Tanzania: First year of primary education",
    x == 1802 ~ "1802. Tanzania: Second year of primary education", x == 1803 ~
      "1803. Tanzania: Third year of primary education", x == 1804 ~ "1804. Tanzania:
Fourth year of primary education", x == 1805 ~ "1805. Tanzania: Fifth year
of primary education", x == 1806 ~ "1806. Tanzania: Sixth year of primary
education", x == 1807 ~ "1807. Tanzania: Seventh year of primary education",
    x == 1808 ~ "1808. Tanzania: First year of secondary education", x == 1809
    ~ "1809. Tanzania: Second year of secondary education", x == 1810 ~ "1810.
Tanzania: Third year of secondary education", x == 1811 ~ "1811. Tanzania:
Fourth year of secondary education", x == 1812 ~ "1812. Tanzania: Fifth year
of secondary education", x == 1813 ~ "1813. Tanzania: Sixth year of secondary
education", x == 1814 ~ "1814. Tanzania: First year of technical training",
    x == 1815 ~ "1815. Tanzania: Second year of technical training (Polytechnic
or Diploma colleges)", x == 1816 ~ "1816. Tanzania: Third year of technical
training (Polytechnic or Diploma colleges)", x == 1817 ~ "1817. Tanzania: First
year of teachers training college", x == 1818 ~ "1818. Tanzania: Second year
of teachers training college", x == 1819 ~ "1819. Tanzania: Completed education
at teachers training college", x == 1820 ~ "1820. Tanzania: Some university
education", x == 1821 ~ "1821. Tanzania: Completed university education", x ==
      1822 ~ "1822. Tanzania: Completed post university education", x == 1900 ~ "1900.
Turkey: (No formal education)", x == 1901 ~ "1901. Turkey: Literate without
any diploma / did not complete primary", x == 1902 ~ "1902. Turkey: Primary
school (5 years)", x == 1903 ~ "1903. Turkey: Primary education (8 years)", x
    == 1904 ~ "1904. Turkey: Junior high school / Vocational school at junior high
school level", x == 1905 ~ "1905. Turkey: High school / Vocational school at
high school level", x == 1906 ~ "1906. Turkey: Training Technical / Profession
school", x == 1907 ~ "1907. Turkey: Universities", x == 1908 ~ "1908. Turkey:
Masters degree", x == 1909 ~ "1909. Turkey: Ph.D.", x == 2000 ~ "2000. United
Kingdom: (No formal education)", x == 2001 ~ "2001. United Kingdom: Nursery
School", x == 2002 ~ "2002. United Kingdom: Infant/Junior School/Basic Adult
Literacy", x == 2003 ~ "2003. United Kingdom: Lower Secondary School (Age less
than 14)", x == 2004 ~ "2004. United Kingdom: Upper Secondary School (GCSE/SCE,
Youth training/NTMA, A level, Highers, NVQ/SVQ Level 3)", x == 2005 ~ "2005.
United Kingdom: Higher Education Access Courses", x == 2006 ~ "2006. United
Kingdom: Undergraduate Degree", x == 2007 ~ "2007. United Kingdom: Masters
Degree", x == 2008 ~ "2008. United Kingdom: HND/HNC/Nursing Degree, PG Diplomas,
NVQ/SVQ Levels 4/5", x == 2009 ~ "2009. United Kingdom: Doctorate", x == 2200
    ~ "2200. United States: No formal education", x == 2201 ~ "2201. United States:
Grade 8 or lower", x == 2202 ~ "2202. United States: Some high school (Grades 9
through 11)", x == 2203 ~ "2203. United States: High school graduate (Grade 12
with diploma or GED certificate)", x == 2204 ~ "2204. United States: Technical,
trade, vocational, or business school or program after high school", x == 2205
    ~ "2205. United States: Some college - college, university, or community college
- but no degree", x == 2206 ~ "2206. United States: Two-year associate degree
from a college, university, or community college", x == 2207 ~ "2207. United
States: Four-year bachelors degree from a college or university (e.g., BS,
BA, AB)", x == 2208 ~ "2208. United States: Some postgraduate or professional
schooling after graduating college, but no postgraduate degree (e.g.,", x ==
      2209 ~ "2209. United States: Postgraduate or professional degree, including
masters, doctorate, medical, or law degree (e.g., MA, MS", x == 2300 ~ "2300.
Sweden: No formal education", x == 2301 ~ "2301. Sweden: Incomplete primary
school", x == 2302 ~ "2302. Sweden: Primary education (ISCED 1)", x == 2303 ~
      "2303. Sweden: Lower secondary education (ISCED 2)", x == 2304 ~ "2304. Sweden:
Upper secondary education (ISCED 3)", x == 2305 ~ "2305. Sweden: Post-secondary
including pre-vocational or vocational education but not tertiary (ISCED 4)", x
    == 2306 ~ "2306. Sweden: Tertiary education – first level (ISCED 5)", x == 2307
    ~ "2307. Sweden: Tertiary education – advanced level (ISCED 6)", x == 2400 ~
      "2400. Hong Kong: No education", x == 2401 ~ "2401. Hong Kong: PSLE and below",
    x == 2402 ~ "2402. Hong Kong: Some junior/lower secondary", x == 2403 ~ "2403.
Hong Kong: Completed junior/lower secondary (GCE N Level (Form 3))", x == 2404
    ~ "2404. Hong Kong: Some senior/higher secondary", x == 2405 ~ "2405. Hong Kong:
Completed senior/higher secondary (GCE O Level (Form 5), GCE A Level (Form 7
in old structure), DSE Level (F", x == 2406 ~ "2406. Hong Kong: Post-secondary
Non-degree (e.g. certificate/diploma/associate degree)", x == 2407 ~ "2407. Hong
Kong: Some University Degree (Bachelors Degree)", x == 2408 ~ "2408. Hong Kong:
Completed University Degree (Bachelors degree)", x == 2409 ~ "2409. Hong Kong:
University Postgraduate Degree (e.g., MBA, Ph. D)",
    .default = "(Missing)"
    ),
    EDUCATION_3_W2 = case_when(x == 1 ~ "1. Up to 8", x == 2 ~ "2. 9-15", x ==
      3 ~ "3. 16+", .default = "(Missing)"),
    EMPLOYMENT_W2 = case_when(x == 1 ~ "1. Employed for an employer", x == 2 ~ "2. Self-employed",
      x == 3 ~ "3. Retired", x == 4 ~ "4. Student", x == 5 ~ "5. Homemaker",
      x == 6 ~ "6. Unemployed and looking for a job", x == 7 ~ "7. None of
these/Other",
      .default = "(Missing)"
    ),
    EXPECT_GOOD_W2 = x,
    EXPENSES_W2 = x,
    FATHER_LOVED_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    FATHER_RELATN_W2 = case_when(x == 1 ~ "1. Very good", x == 2 ~ "2. Somewhat good", x == 3
    ~ "3. Somewhat bad", x == 4 ~ "4. Very bad", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    FEEL_ANXIOUS_W2 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    FORGIVE_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    FREEDOM_W2 = x,
    GENDER_W2 = case_when(x == 1 ~ "1. Male", x == 2 ~ "2. Female", x == 3 ~ "3. Other", x == 4
    ~ "4. Prefer not to answer", .default = "(Missing)"),
    GIVE_UP_W2 = x,
    GOD_PUNISH_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    GRATEFUL_W2 = x,
    GROUP_NOT_REL_W2 = case_when(x == 1 ~ "1. More than once a week", x == 2 ~ "2. Once a week", x ==
      3 ~ "3. One to three times a month", x == 4 ~ "4. A few times a year", x == 5 ~
      "5. Never", .default = "(Missing)"),
    HAPPY_W2 = x,
    HEALTH_GROWUP_W2 = case_when(x == 1 ~ "1. Excellent", x == 2 ~ "2. Very good", x == 3 ~ "3. Good",
      x == 4 ~ "4. Fair", x == 5 ~ "5. Poor",
      .default = "(Missing)"
    ),
    HEALTH_PROB_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    HELP_STRANGER_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    HOPE_FUTURE_W2 = x,
    INCOME_W2 = case_when(x == 101 ~ "101. Argentina: 1,000 pesos or less", x == 102 ~ "102.
Argentina: 1,001 – 5,000 pesos", x == 103 ~ "103. Argentina: 5,001 – 10,000
pesos", x == 104 ~ "104. Argentina: 10,001 – 20,000 pesos", x == 105 ~ "105.
Argentina: 20,001 – 30,000 pesos", x == 106 ~ "106. Argentina: 30,001 – 40,000
pesos", x == 107 ~ "107. Argentina: 40,001 – 50,000 pesos", x == 108 ~ "108.
Argentina: 50,001 – 60,000 pesos", x == 109 ~ "109. Argentina: 60,001 – 70,000
pesos", x == 110 ~ "110. Argentina: 70,001 – 80,000 pesos", x == 111 ~ "111.
Argentina: 80,001 – 90,000 pesos", x == 112 ~ "112. Argentina: 90,001 – 100,000
pesos", x == 113 ~ "113. Argentina: 100,001 – 120,000 pesos", x == 114 ~ "114.
Argentina: 120,001 – 150,000 pesos", x == 115 ~ "115. Argentina: 150,001 –
200,000 pesos", x == 116 ~ "116. Argentina: More than 200,000 pesos", x == 201
    ~ "201. Australia: 20,000 dollars or less", x == 202 ~ "202. Australia: 20,001 –
30,000 dollars", x == 203 ~ "203. Australia: 30,001 – 40,000 dollars", x == 204
    ~ "204. Australia: 40,001 – 50,000 dollars", x == 205 ~ "205. Australia: 50,001
– 60,000 dollars", x == 206 ~ "206. Australia: 60,001 – 75,000 dollars", x ==
      207 ~ "207. Australia: 75,001 – 100,000 dollars", x == 208 ~ "208. Australia:
100,001 – 150,000 dollars", x == 209 ~ "209. Australia: 150,001 – 250,000
dollars", x == 210 ~ "210. Australia: More than 250,000 dollars", x == 301 ~
      "301. Brazil: 100 reals or less", x == 302 ~ "302. Brazil: 101 – 500 reals",
    x == 303 ~ "303. Brazil: 501 – 750 reals", x == 304 ~ "304. Brazil: 751 –
1,000 reals", x == 305 ~ "305. Brazil: 1,001 – 1,500 reals", x == 306 ~ "306.
Brazil: 1,501 – 2,000 reals", x == 307 ~ "307. Brazil: 2,001 – 2,500 reals", x
    == 308 ~ "308. Brazil: 2,501 – 3,000 reals", x == 309 ~ "309. Brazil: 3,001 –
4,000 reals", x == 310 ~ "310. Brazil: 4,001 – 5,000 reals", x == 311 ~ "311.
Brazil: 5,001 – 6,000 reals", x == 312 ~ "312. Brazil: 6,001 – 8,000 reals", x
    == 313 ~ "313. Brazil: 8,001 – 10,000 reals", x == 314 ~ "314. Brazil: 10,001
– 15,000 reals", x == 315 ~ "315. Brazil: More than 15,000 reals", x == 401
    ~ "401. Egypt: 200 EGP or less", x == 402 ~ "402. Egypt: 201 – 500 EGP", x
    == 403 ~ "403. Egypt: 501 – 1,000 EGP", x == 404 ~ "404. Egypt: 1,001 – 3,000
EGP", x == 405 ~ "405. Egypt: 3,001 – 5,000 EGP", x == 406 ~ "406. Egypt: 5,001
– 7,000 EGP", x == 407 ~ "407. Egypt: 7,001 – 10,000 EGP", x == 408 ~ "408.
Egypt: 10,001 – 20,000 EGP", x == 409 ~ "409. Egypt: More than 20,000 EGP",
    x == 501 ~ "501. Germany: 500 euros or less", x == 502 ~ "502. Germany: 501 -
1,500 euros", x == 503 ~ "503. Germany: 1,501 - 2,000 euros", x == 504 ~ "504.
Germany: 2,001 - 2,500 euros", x == 505 ~ "505. Germany: 2,501 - 3,000 euros",
    x == 506 ~ "506. Germany: 3,001 - 3,500 euros", x == 507 ~ "507. Germany: 3,501
- 4,000 euros", x == 508 ~ "508. Germany: 4,001 - 5,000 euros", x == 509 ~ "509.
Germany: 5,001 - 6,000 euros", x == 510 ~ "510. Germany: 6,001 - 7,500 euros",
    x == 511 ~ "511. Germany: 7,501 - 10,000 euros", x == 512 ~ "512. Germany: More
than 10,000 euros", x == 601 ~ "601. India: 1,000 rupees or less", x == 602
    ~ "602. India: 1,001 – 2,000 rupees", x == 603 ~ "603. India: 2,001 – 3,000
rupees", x == 604 ~ "604. India: 3,001 – 4,000 rupees", x == 605 ~ "605. India:
4,001 – 5,000 rupees", x == 606 ~ "606. India: 5,001 – 6,000 rupees", x == 607
    ~ "607. India: 6,001 – 7,500 rupees", x == 608 ~ "608. India: 7,501 – 10,000
rupees", x == 609 ~ "609. India: 10,001 – 15,000 rupees", x == 610 ~ "610.
India: 15,001 – 20,000 rupees", x == 611 ~ "611. India: 20,001 – 35,000 rupees",
    x == 612 ~ "612. India: 35,001 – 50,000 rupees", x == 613 ~ "613. India: More
than 50,000 rupees", x == 701 ~ "701. Indonesia: 150,000 rupiah or less", x ==
      702 ~ "702. Indonesia: 150,001 – 300,000 rupiah", x == 703 ~ "703. Indonesia:
300,001 – 600,000 rupiah", x == 704 ~ "704. Indonesia: 600,001 – 1,000,000
rupiah", x == 705 ~ "705. Indonesia: 1,000,001 – 1,500,000 rupiah", x == 706
    ~ "706. Indonesia: 1,500,001 – 2,000,000 rupiah", x == 707 ~ "707. Indonesia:
2,000,001 – 2,500,000 rupiah", x == 708 ~ "708. Indonesia: 2,500,001 – 4,000,000
rupiah", x == 709 ~ "709. Indonesia: 4,000,001 - 8,000,000 rupiah", x == 710
    ~ "710. Indonesia: 8,000,001 - 14,000,000 rupiah", x == 711 ~ "711. Indonesia:
More than 14,000,000 rupiah", x == 801 ~ "801. Israel: 1,000 Israeli shekels or
less", x == 802 ~ "802. Israel: 1,001 - 3,000 Israeli shekels", x == 803 ~ "803.
Israel: 3,001 - 5,000 Israeli shekels", x == 804 ~ "804. Israel: 5,001 - 6,000
Israeli shekels", x == 805 ~ "805. Israel: 6,001 - 7,000 Israeli shekels", x
    == 806 ~ "806. Israel: 7,001 - 8,000 Israeli shekels", x == 807 ~ "807. Israel:
8,001 - 9,000 Israeli shekels", x == 808 ~ "808. Israel: 9,001 - 10,000 Israeli
shekels", x == 809 ~ "809. Israel: 10,001 - 11,000 Israeli shekels", x == 810 ~
      "810. Israel: 11,001 - 12,000 Israeli shekels", x == 811 ~ "811. Israel: 12,001
– 15,000 Israeli shekels", x == 812 ~ "812. Israel: 15,001 – 16,000 Israeli
shekels", x == 813 ~ "813. Israel: 16,001 – 18,000 Israeli shekels", x == 814 ~
      "814. Israel: 18,001 – 20,000 Israeli shekels", x == 815 ~ "815. Israel: 20,001
- 40,000 Israeli shekels", x == 816 ~ "816. Israel: More than 40,000 Israeli
shekels", x == 901 ~ "901. Japan: 50,000 yen or less", x == 902 ~ "902. Japan:
50,001 – 100,000 yen", x == 903 ~ "903. Japan: 100,001 – 150,000 yen", x == 904
    ~ "904. Japan: 150,001 – 200,000 yen", x == 905 ~ "905. Japan: 200,001 – 250,000
yen", x == 906 ~ "906. Japan: 250,001 – 300,000 yen", x == 907 ~ "907. Japan:
300,001 – 400,000 yen", x == 908 ~ "908. Japan: 400,001 – 450,000 yen", x ==
      909 ~ "909. Japan: 450,001 – 500,000 yen", x == 910 ~ "910. Japan: 500,001 –
600,000 yen", x == 911 ~ "911. Japan: 600,001 – 700,000 yen", x == 912 ~ "912.
Japan: 700,001 – 800,000 yen", x == 913 ~ "913. Japan: 800,001 – 900,000 yen",
    x == 914 ~ "914. Japan: 900,001 – 1,000,000 yen", x == 915 ~ "915. Japan: More
than 1,000,000 yen", x == 1001 ~ "1001. Kenya: 500 Kshs/month or less", x ==
      1002 ~ "1002. Kenya: 551 – 1,000 Kshs/month", x == 1003 ~ "1003. Kenya: 1,001
– 2,000 Kshs/month", x == 1004 ~ "1004. Kenya: 2,001 – 3,000 Kshs/month", x ==
      1005 ~ "1005. Kenya: 3,001 – 4,000 Kshs/month", x == 1006 ~ "1006. Kenya: 4,001
– 5,000 Kshs/month", x == 1007 ~ "1007. Kenya: 5,001 – 6,000 Kshs/month", x ==
      1008 ~ "1008. Kenya: 6,001 – 8,000 Kshs/month", x == 1009 ~ "1009. Kenya: 8,001
– 10,000 Kshs/month", x == 1010 ~ "1010. Kenya: 10,001 – 12,000 Kshs/month",
    x == 1011 ~ "1011. Kenya: 12,001 – 18,000 Kshs/month", x == 1012 ~ "1012.
Kenya: 18,001 – 25,000 Kshs/month", x == 1013 ~ "1013. Kenya: 25,001 – 40,000
Kshs/month", x == 1014 ~ "1014. Kenya: 40,001 – 80,000 Kshs/month", x == 1015
    ~ "1015. Kenya: More than 80,000 Kshs/month", x == 1101 ~ "1101. Mexico: 550
Mexican pesos or less", x == 1102 ~ "1102. Mexico: 551 – 1.000 Mexican pesos",
    x == 1103 ~ "1103. Mexico: 1.001 – 1.500 Mexican pesos", x == 1104 ~ "1104.
Mexico: 1.501 – 2.000 Mexican pesos", x == 1105 ~ "1105. Mexico: 2.001 – 2.500
Mexican pesos", x == 1106 ~ "1106. Mexico: 2.501 – 3.000 Mexican pesos", x ==
      1107 ~ "1107. Mexico: 3.001 – 4.000 Mexican pesos", x == 1108 ~ "1108. Mexico:
4.001 – 5.000 Mexican pesos", x == 1109 ~ "1109. Mexico: 5.001 – 7.000 Mexican
pesos", x == 1110 ~ "1110. Mexico: 7.001 – 10.000 Mexican pesos", x == 1111 ~
      "1111. Mexico: 10.001 – 15.000 Mexican pesos", x == 1112 ~ "1112. Mexico: 15.001
– 20.000 Mexican pesos", x == 1113 ~ "1113. Mexico: 20.001 – 30.000 Mexican
pesos", x == 1114 ~ "1114. Mexico: 30.001 – 40.000 Mexican pesos", x == 1115 ~
      "1115. Mexico: 40.001 – 50.000 Mexican pesos", x == 1116 ~ "1116. Mexico: 50.001
– 70.000 Mexican pesos", x == 1117 ~ "1117. Mexico: More than 70.000 Mexican
pesos", x == 1201 ~ "1201. Nigeria: 3,700 naira/month or less", x == 1202 ~
      "1202. Nigeria: 3,701 – 6,000 naira/month", x == 1203 ~ "1203. Nigeria: 6,001
– 9,000 naira/month", x == 1204 ~ "1204. Nigeria: 9,001 – 13,000 naira/month",
    x == 1205 ~ "1205. Nigeria: 13,001 – 18,000 naira/month", x == 1206 ~ "1206.
Nigeria: 18,001 – 24,000 naira/month", x == 1207 ~ "1207. Nigeria: 24,001 –
30,000 naira/month", x == 1208 ~ "1208. Nigeria: 30,001 – 37,000 naira/month",
    x == 1209 ~ "1209. Nigeria: 37,001 – 47,000 naira/month", x == 1210 ~ "1210.
Nigeria: 47,001 – 57,000 naira/month", x == 1211 ~ "1211. Nigeria: 57,001 –
67,000 naira/month", x == 1212 ~ "1212. Nigeria: 67,001 – 77,000 naira/month",
    x == 1213 ~ "1213. Nigeria: 77,001 – 100,000 naira/month", x == 1214 ~ "1214.
Nigeria: 100,001 – 150,000 naira/month", x == 1215 ~ "1215. Nigeria: More than
150,000 naira/month", x == 1301 ~ "1301. Philippines: 700 pesos or less", x ==
      1302 ~ "1302. Philippines: 701 – 1,500 pesos", x == 1303 ~ "1303. Philippines:
1,501 – 3,000 pesos", x == 1304 ~ "1304. Philippines: 3,001 – 4,000 pesos", x ==
      1305 ~ "1305. Philippines: 4,001 – 6,000 pesos", x == 1306 ~ "1306. Philippines:
6,001 – 8,000 pesos", x == 1307 ~ "1307. Philippines: 8,001 – 10,000 pesos",
    x == 1308 ~ "1308. Philippines: 10,001 – 15,000 pesos", x == 1309 ~ "1309.
Philippines: 15,001 – 20,000 pesos", x == 1310 ~ "1310. Philippines: 20,001 –
25,000 pesos", x == 1311 ~ "1311. Philippines: 25,001 – 30,000 pesos", x == 1312
    ~ "1312. Philippines: 30,001 – 40,000 pesos", x == 1313 ~ "1313. Philippines:
40,001 – 50,000 pesos", x == 1314 ~ "1314. Philippines: 50,001 – 90,000 pesos",
    x == 1315 ~ "1315. Philippines: More than 90,000 pesos", x == 1401 ~ "1401.
Poland: 100 PLN or less", x == 1402 ~ "1402. Poland: 101 – 500 PLN", x == 1403 ~
      "1403. Poland: 501 – 1,000 PLN", x == 1404 ~ "1404. Poland: 1,001 – 1,500 PLN",
    x == 1405 ~ "1405. Poland: 1,501 – 2,000 PLN", x == 1406 ~ "1406. Poland: 2,001
– 2,500 PLN", x == 1407 ~ "1407. Poland: 2,501 – 3,000 PLN", x == 1408 ~ "1408.
Poland: 3,001 – 3,500 PLN", x == 1409 ~ "1409. Poland: 3,501 – 4,000 PLN", x
    == 1410 ~ "1410. Poland: 4,001 – 5,000 PLN", x == 1411 ~ "1411. Poland: 5,001
– 6,000 PLN", x == 1412 ~ "1412. Poland: 6,001 – 8,000 PLN", x == 1413 ~ "1413.
Poland: 8,001 – 10,000 PLN", x == 1414 ~ "1414. Poland: 10,001 – 15,000 PLN", x
    == 1415 ~ "1415. Poland: 15,001 – 20,000 PLN", x == 1416 ~ "1416. Poland: More
than 20,000 PLN", x == 1601 ~ "1601. South Africa: 200 rand/month or less", x
    == 1602 ~ "1602. South Africa: 201 – 350 rand/month", x == 1603 ~ "1603. South
Africa: 351 – 1,000 rand/month", x == 1604 ~ "1604. South Africa: 1,001 – 2,000
rand/month", x == 1605 ~ "1605. South Africa: 2,001 – 3,000 rand/month", x ==
      1606 ~ "1606. South Africa: 3,001 – 4,000 rand/month", x == 1607 ~ "1607. South
Africa: 4,001 – 5,000 rand/month", x == 1608 ~ "1608. South Africa: 5,001 –
6,000 rand/month", x == 1609 ~ "1609. South Africa: 6,001 – 8,000 rand/month",
    x == 1610 ~ "1610. South Africa: 8,001 – 10,000 rand/month", x == 1611 ~ "1611.
South Africa: 10,001 – 15,000 rand/month", x == 1612 ~ "1612. South Africa:
15,001 – 20,000 rand/month", x == 1613 ~ "1613. South Africa: 20,001 – 30,000
rand/month", x == 1614 ~ "1614. South Africa: More than 30,000 rand/month",
    x == 1701 ~ "1701. Spain: 250 euros or less", x == 1702 ~ "1702. Spain: 251 -
1,000 euros", x == 1703 ~ "1703. Spain: 1,001 - 1,250 euros", x == 1704 ~ "1704.
Spain: 1,251 – 1,500 euros", x == 1705 ~ "1705. Spain: 1,501 - 1,750 euros", x
    == 1706 ~ "1706. Spain: 1,751 – 2,000 euros", x == 1707 ~ "1707. Spain: 2,001
- 2,250 euros", x == 1708 ~ "1708. Spain: 2,251 – 2,500 euros", x == 1709 ~
      "1709. Spain: 2,501 - 2,750 euros", x == 1710 ~ "1710. Spain: 2,751 – 3,000
euros", x == 1711 ~ "1711. Spain: 3,001 - 3,500 euros", x == 1712 ~ "1712.
Spain: 3,501 - 4,000 euros", x == 1713 ~ "1713. Spain: 4,001 - 5,000 euros",
    x == 1714 ~ "1714. Spain: 5,001 – 6,000 euros", x == 1715 ~ "1715. Spain:
More than 6,000 euros", x == 1801 ~ "1801. Tanzania: 20,000 shillings/month
or less", x == 1802 ~ "1802. Tanzania: 20,001 – 50,000 shillings/month",
    x == 1803 ~ "1803. Tanzania: 50,001 – 100,000 shillings/month", x ==
      1804 ~ "1804. Tanzania: 100,001 – 150,000 shillings/month", x == 1805
    ~ "1805. Tanzania: 150,001 – 200,000 shillings/month", x == 1806 ~
      "1806. Tanzania: 200,001 – 250,000 shillings/month", x == 1807 ~ "1807.
Tanzania: 250,001 – 300,000 shillings/month", x == 1808 ~ "1808. Tanzania:
300,001 – 400,000 shillings/month", x == 1809 ~ "1809. Tanzania: 400,001
– 500,000 shillings/month", x == 1810 ~ "1810. Tanzania: 500,001 –
700,000 shillings/month", x == 1811 ~ "1811. Tanzania: More than 700,000
shillings/month", x == 1901 ~ "1901. Turkey: 100 TL or less", x == 1902 ~ "1902.
Turkey: 101 - 500 TL", x == 1903 ~ "1903. Turkey: 501 - 1,000 TL", x == 1904
    ~ "1904. Turkey: 1,001 - 1,500 TL", x == 1905 ~ "1905. Turkey: 1,501 - 2,000
TL", x == 1906 ~ "1906. Turkey: 2,001 - 2,500 TL", x == 1907 ~ "1907. Turkey:
2,501 – 3,000 TL", x == 1908 ~ "1908. Turkey: 3,001 - 3,500 TL", x == 1909 ~
      "1909. Turkey: 3,501 – 4,000 TL", x == 1910 ~ "1910. Turkey: 4,001 - 4,500 TL",
    x == 1911 ~ "1911. Turkey: 4,501 – 5,000 TL", x == 1912 ~ "1912. Turkey: 5,001
– 6,000 TL", x == 1913 ~ "1913. Turkey: 6,001 - 8,000 TL", x == 1914 ~ "1914.
Turkey: 8,001 - 10,000 TL", x == 1915 ~ "1915. Turkey: 10,001 - 12,000 TL", x ==
      1916 ~ "1916. Turkey: More than 12,000 TL", x == 2001 ~ "2001. United Kingdom:
500 pounds or less", x == 2002 ~ "2002. United Kingdom: 501 - 1,000 pounds",
    x == 2003 ~ "2003. United Kingdom: 1,001 - 1,500 pounds", x == 2004 ~ "2004.
United Kingdom: 1,501 - 2,000 pounds", x == 2005 ~ "2005. United Kingdom: 2,001
- 2,500 pounds", x == 2006 ~ "2006. United Kingdom: 2,501 - 3,000 pounds",
    x == 2007 ~ "2007. United Kingdom: 3,001 - 4,000 pounds", x == 2008 ~ "2008.
United Kingdom: 4,001 - 5,000 pounds", x == 2009 ~ "2009. United Kingdom: 5,001
- 6,000 pounds", x == 2010 ~ "2010. United Kingdom: 6,001 - 8,000 pounds", x
    == 2011 ~ "2011. United Kingdom: More than 8,000 pounds", x == 2201 ~ "2201.
United States: Less than $12,000", x == 2202 ~ "2202. United States: $12,000
to $23,999", x == 2203 ~ "2203. United States: $24,000 to $35,999", x == 2204
    ~ "2204. United States: $36,000 to $47,999", x == 2205 ~ "2205. United States:
$48,000 to $59,999", x == 2206 ~ "2206. United States: $60,000 to $89,999", x
    == 2207 ~ "2207. United States: $90,000 to $119,999", x == 2208 ~ "2208. United
States: $120,000 to $179,999", x == 2209 ~ "2209. United States: $180,000 to
$239,999", x == 2210 ~ "2210. United States: $240,000 and over", x == 2301 ~
      "2301. Sweden: 10,000 SEK or less", x == 2302 ~ "2302. Sweden: 10,001 - 15,000
SEK", x == 2303 ~ "2303. Sweden: 15,001 - 25,000 SEK", x == 2304 ~ "2304.
Sweden: 25,001 - 30,000 SEK", x == 2305 ~ "2305. Sweden: 30,001 - 35,000 SEK", x
    == 2306 ~ "2306. Sweden: 35,001 - 40,000 SEK", x == 2307 ~ "2307. Sweden: 40,001
- 50,000 SEK", x == 2308 ~ "2308. Sweden: 50,001 - 60,000 SEK", x == 2309 ~
      "2309. Sweden: 60,001 - 70,000 SEK", x == 2310 ~ "2310. Sweden: 70,001 - 80,000
SEK", x == 2311 ~ "2311. Sweden: 80,001 - 100,000 SEK", x == 2312 ~ "2312.
Sweden: More than 100,000 SEK", x == 2401 ~ "2401. Hong Kong: 5,000 dollars
or less", x == 2402 ~ "2402. Hong Kong: 5,001 – 10,000 dollars", x == 2403 ~
      "2403. Hong Kong: 10,001 – 15,000 dollars", x == 2404 ~ "2404. Hong Kong: 15,001
– 20,000 dollars", x == 2405 ~ "2405. Hong Kong: 20,001 – 30,000 dollars", x
    == 2406 ~ "2406. Hong Kong: 30,001 – 40,000 dollars", x == 2407 ~ "2407. Hong
Kong: 40,001 – 45,000 dollars", x == 2408 ~ "2408. Hong Kong: 45,001 – 50,000
dollars", x == 2409 ~ "2409. Hong Kong: 50,001 – 60,000 dollars", x == 2410 ~
      "2410. Hong Kong: 60,001 – 70,000 dollars", x == 2411 ~ "2411. Hong Kong: 70,001
– 80,000 dollars", x == 2412 ~ "2412. Hong Kong: 80,001 – 100,000 dollars", x ==
      2413 ~ "2413. Hong Kong: More than 100,000 dollars", x == 9900 ~ "9900. (None/No
household income)",
    .default = "(Missing)"
    ),
    INCOME_12YRS_W2 = case_when(x == 1 ~ "1. Lived comfortably", x == 2 ~ "2. Got by", x == 3 ~
      "3. Found it difficult", x == 4 ~ "4. Found it very difficult",
    .default =
      "(Missing)"
    ),
    INCOME_DIFF_W2 = case_when(x == 1 ~ "1. Strongly agree", x == 2 ~ "2. Somewhat agree", x == 3 ~
      "3. Neither agree nor disagree", x == 4 ~ "4. Somewhat disagree", x == 5 ~ "5.
Strongly disagree", .default = "(Missing)"),
    INCOME_FEELINGS_W2 = case_when(x == 1 ~ "1. Living comfortably on present income", x == 2 ~ "2.
Getting by on present income", x == 3 ~ "3. Finding it difficult on present
income", x == 4 ~ "4. Finding it very difficult on present income",
      .default =
        "(Missing)"
    ),
    INTEREST_W2 = case_when(x == 1 ~ "1. Nearly every day", x == 2 ~ "2. More than half the days",
      x == 3 ~ "3. Several days", x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    LIFE_APPROACH_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    LIFE_BALANCE_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    LIFE_PURPOSE_W2 = x,
    LIFE_SAT_W2 = x,
    LONELY_W2 = x,
    LOVED_BY_GOD_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    MARITAL_STATUS_W2 = case_when(x == 1 ~ "1. Single/Never been married", x == 2 ~ "2. Married", x ==
      3 ~ "3. Separated", x == 4 ~ "4. Divorced", x == 5 ~ "5. Widowed", x == 6 ~ "6.
Domestic partner", .default = "(Missing)"),
    MENTAL_HEALTH_W2 = x,
    MOTHER_LOVED_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    MOTHER_RELATN_W2 = case_when(x == 1 ~ "1. Very good", x == 2 ~ "2. Somewhat good", x == 3
    ~ "3. Somewhat bad", x == 4 ~ "4. Very bad", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    NUM_CHILDREN_W2 = x,
    NUM_HOUSEHOLD_W2 = x,
    OBEY_LAW_W2 = case_when(x == 1 ~ "1. Strongly agree", x == 2 ~ "2. Somewhat agree", x == 3 ~
      "3. Neither agree nor disagree", x == 4 ~ "4. Somewhat disagree", x == 5 ~ "5.
Strongly disagree", .default = "(Missing)"),
    OUTSIDER_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", x == 97 ~ "97. (Does not
apply)", .default = "(Missing)"),
    OWN_RENT_HOME_W2 = case_when(x == 1 ~ "1. Someone in this household OWNS this home", x == 2 ~
      "2. Someone in this household RENTS this home", x == 3 ~ "3. Both", x == 4 ~
      "4. Neither", x == 5 ~ "5. Rent", x == 6 ~ "6. Own", x == 7 ~ "7. Something
else", .default = "(Missing)"),
    PARENTS_12YRS_W2 = case_when(x == 1 ~ "1. Yes, married", x == 2 ~ "2. No, divorced", x == 3 ~ "3.
No, they were never married", x == 4 ~ "4. No, one or both of them had died", x
    == 5 ~ "5. Unsure", .default = "(Missing)"),
    PEACE_W2 = case_when(x == 1 ~ "1. Always", x == 2 ~ "2. Often", x == 3 ~ "3. Rarely", x ==
      4 ~ "4. Never", .default = "(Missing)"),
    PEOPLE_HELP_W2 = x,
    PHYSICAL_HLTH_W2 = x,
    POLITICAL_ID_W2 = x,
    PRAY_MEDITATE_W2 = case_when(x == 1 ~ "1. More than once a day", x == 2 ~ "2. About once a day", x
    == 3 ~ "3. Sometimes", x == 4 ~ "4. Never", .default = "(Missing)"),
    PROMOTE_GOOD_W2 = x,
    REGION1_W2 = case_when(x == 101 ~ "101. Argentina: Capital Federal", x == 102 ~ "102.
Argentina: Buenos Aires", x == 103 ~ "103. Argentina: Catamarca", x == 104
    ~ "104. Argentina: Chaco", x == 105 ~ "105. Argentina: Chubut", x == 106 ~
      "106. Argentina: Cordoba", x == 107 ~ "107. Argentina: Corrientes", x == 108
    ~ "108. Argentina: Entre Ríos", x == 109 ~ "109. Argentina: Formosa", x ==
      110 ~ "110. Argentina: Jujuy", x == 111 ~ "111. Argentina: La Pampa", x ==
      112 ~ "112. Argentina: La Rioja", x == 113 ~ "113. Argentina: Mendoza", x ==
      114 ~ "114. Argentina: Misiones", x == 115 ~ "115. Argentina: Neuquen", x ==
      116 ~ "116. Argentina: Río Negro", x == 117 ~ "117. Argentina: Salta", x ==
      118 ~ "118. Argentina: San Juan", x == 119 ~ "119. Argentina: San Luis", x ==
      120 ~ "120. Argentina: Santa Cruz", x == 121 ~ "121. Argentina: Santa Fe", x
    == 122 ~ "122. Argentina: Santiago del Estero", x == 123 ~ "123. Argentina:
Tierra del Fuego", x == 124 ~ "124. Argentina: Tucumán", x == 201 ~ "201.
Australia: Sydney", x == 202 ~ "202. Australia: New South Wales (NSW), excluding
Sydney", x == 203 ~ "203. Australia: Melbourne", x == 204 ~ "204. Australia:
Victoria (VIC), excluding Melbourne", x == 205 ~ "205. Australia: Brisbane",
    x == 206 ~ "206. Australia: Queensland (QLD), excluding Brisbane", x == 207
    ~ "207. Australia: Adelaide", x == 208 ~ "208. Australia: South Australia
(SA), excluding Adelaide", x == 209 ~ "209. Australia: Perth", x == 210 ~
      "210. Australia: Western Australia (WA), excluding Perth", x == 211 ~ "211.
Australia: Hobart", x == 212 ~ "212. Australia: Tasmania (TAS), excluding
Hobart", x == 213 ~ "213. Australia: Australian Capital Territory (ACT)", x ==
      214 ~ "214. Australia: Darwin", x == 215 ~ "215. Australia: Northern Territory
(NT), excluding Darwin", x == 311 ~ "311. Brazil: Roraima", x == 313 ~ "313.
Brazil: Amazonas", x == 315 ~ "315. Brazil: Pará", x == 317 ~ "317. Brazil:
Tocantins", x == 321 ~ "321. Brazil: Maranhão", x == 322 ~ "322. Brazil: Piauí",
    x == 323 ~ "323. Brazil: Ceará", x == 324 ~ "324. Brazil: Rio Grande do Norte",
    x == 325 ~ "325. Brazil: Paraíba", x == 326 ~ "326. Brazil: Pernambuco", x ==
      327 ~ "327. Brazil: Alagoas", x == 328 ~ "328. Brazil: Sergipe", x == 329 ~
      "329. Brazil: Bahia", x == 331 ~ "331. Brazil: Minas Gerais", x == 333 ~ "333.
Brazil: Rio de Janeiro", x == 335 ~ "335. Brazil: São Paulo", x == 341 ~ "341.
Brazil: Paraná", x == 342 ~ "342. Brazil: Santa Catarina", x == 343 ~ "343.
Brazil: Rio Grande do Sul", x == 350 ~ "350. Brazil: Mato Grosso do Sul", x ==
      351 ~ "351. Brazil: Mato Grosso", x == 352 ~ "352. Brazil: Goiás", x == 353 ~
      "353. Brazil: Distrito Federal", x == 354 ~ "354. Brazil: Espirito Santo", x
    == 355 ~ "355. Brazil: Amapá", x == 356 ~ "356. Brazil: Rondonia", x == 357 ~
      "357. Brazil: Acre", x == 401 ~ "401. Egypt: Dakhalia", x == 402 ~ "402. Egypt:
Faiyum", x == 403 ~ "403. Egypt: Sharqia", x == 404 ~ "404. Egypt: Aswan", x ==
      405 ~ "405. Egypt: Cairo", x == 406 ~ "406. Egypt: Ismailia", x == 407 ~ "407.
Egypt: Menia", x == 408 ~ "408. Egypt: Alexandria", x == 409 ~ "409. Egypt:
Gharbia", x == 410 ~ "410. Egypt: Sohag", x == 411 ~ "411. Egypt: Giza", x ==
      412 ~ "412. Egypt: Port Said", x == 413 ~ "413. Egypt: Suez", x == 414 ~ "414.
Egypt: Damietta", x == 415 ~ "415. Egypt: Qalyubia", x == 416 ~ "416. Egypt:
Kafr El-Sheikh", x == 417 ~ "417. Egypt: Monufia", x == 418 ~ "418. Egypt:
Beheira", x == 419 ~ "419. Egypt: Beni-Suef", x == 420 ~ "420. Egypt: Asyut",
    x == 421 ~ "421. Egypt: Qena", x == 422 ~ "422. Egypt: Luxor", x == 423 ~ "423.
Egypt: Matruh", x == 424 ~ "424. Egypt: Red Sea", x == 425 ~ "425. Egypt: New
Valley", x == 426 ~ "426. Egypt: North Sinai", x == 427 ~ "427. Egypt: South
Sinai", x == 501 ~ "501. Germany: Arnsberg", x == 502 ~ "502. Germany: Berlin",
    x == 503 ~ "503. Germany: Brandenburg", x == 504 ~ "504. Germany: Braunschweig",
    x == 505 ~ "505. Germany: Bremen", x == 506 ~ "506. Germany: Chemnitz", x
    == 507 ~ "507. Germany: Darmstadt", x == 508 ~ "508. Germany: Detmold", x ==
      509 ~ "509. Germany: Dresden", x == 510 ~ "510. Germany: Dusseldorf", x ==
      511 ~ "511. Germany: Freiburg", x == 512 ~ "512. Germany: Giessen", x == 513
    ~ "513. Germany: Hamburg", x == 514 ~ "514. Germany: Hannover", x == 515 ~
      "515. Germany: Karlsruhe", x == 516 ~ "516. Germany: Kassel", x == 517 ~ "517.
Germany: Koblenz", x == 518 ~ "518. Germany: Koln", x == 519 ~ "519. Germany:
Leipzig", x == 520 ~ "520. Germany: Luneburg", x == 521 ~ "521. Germany:
Mecklenburg-Vorpommern", x == 522 ~ "522. Germany: Mittelfranken", x == 523 ~
      "523. Germany: Munster", x == 524 ~ "524. Germany: Niederbayern", x == 525 ~
      "525. Germany: Oberbayern", x == 526 ~ "526. Germany: Oberfranken", x == 527 ~
      "527. Germany: Oberpfalz", x == 528 ~ "528. Germany: Rheinhessen-Pfalz", x ==
      529 ~ "529. Germany: Saarland", x == 530 ~ "530. Germany: Sachsen-Anhalt", x ==
      531 ~ "531. Germany: Schleswig-Holstein", x == 532 ~ "532. Germany: Schwaben",
    x == 533 ~ "533. Germany: Stuttgart", x == 534 ~ "534. Germany: Thuringen", x
    == 535 ~ "535. Germany: Trier", x == 536 ~ "536. Germany: Tubingen", x == 537
    ~ "537. Germany: Unterfranken", x == 538 ~ "538. Germany: Weser-Ems", x == 601
    ~ "601. India: Andhra Pradesh", x == 602 ~ "602. India: Arunachal Pradesh", x
    == 603 ~ "603. India: Assam", x == 604 ~ "604. India: Bihar", x == 605 ~ "605.
India: Chandigarh", x == 606 ~ "606. India: Chhattisgarh", x == 607 ~ "607.
India: Delhi", x == 608 ~ "608. India: Goa", x == 609 ~ "609. India: Gujarat",
    x == 610 ~ "610. India: Haryana", x == 611 ~ "611. India: Himachal Pradesh", x
    == 612 ~ "612. India: Jammu and Kashmir", x == 613 ~ "613. India: Jharkhand",
    x == 614 ~ "614. India: Karnataka", x == 615 ~ "615. India: Kerala", x == 616
    ~ "616. India: Madhya Pradesh", x == 617 ~ "617. India: Maharashtra", x == 618
    ~ "618. India: Manipur", x == 619 ~ "619. India: Meghalaya", x == 620 ~ "620.
India: Mizoram", x == 621 ~ "621. India: Nagaland", x == 622 ~ "622. India:
Odisha (Orissa)", x == 623 ~ "623. India: Pondicherry", x == 624 ~ "624. India:
Punjab", x == 625 ~ "625. India: Rajasthan", x == 626 ~ "626. India: Sikkim",
    x == 627 ~ "627. India: Tamil Nadu", x == 628 ~ "628. India: Tripura", x ==
      629 ~ "629. India: Uttar Pradesh", x == 630 ~ "630. India: Uttarakhand", x ==
      631 ~ "631. India: West Bengal", x == 632 ~ "632. India: Telangana", x == 701
    ~ "701. Indonesia: Nanggroe Aceh Darussalam", x == 702 ~ "702. Indonesia: North
Sumatra", x == 703 ~ "703. Indonesia: West Sumatra", x == 704 ~ "704. Indonesia:
Riau", x == 705 ~ "705. Indonesia: Jambi", x == 706 ~ "706. Indonesia: South
Sumatra", x == 707 ~ "707. Indonesia: Bengkulu", x == 708 ~ "708. Indonesia:
Lampung", x == 709 ~ "709. Indonesia: Bangka Belitung Island", x == 710 ~ "710.
Indonesia: Riau Island", x == 711 ~ "711. Indonesia: DKI Jakarta", x == 712 ~
      "712. Indonesia: West Java", x == 713 ~ "713. Indonesia: Central Java", x ==
      714 ~ "714. Indonesia: DI Yogjakarta", x == 715 ~ "715. Indonesia: East Java",
    x == 716 ~ "716. Indonesia: Banten", x == 717 ~ "717. Indonesia: Bali", x ==
      718 ~ "718. Indonesia: West Nusa Tenggara", x == 719 ~ "719. Indonesia: East
Nusa Tenggara", x == 720 ~ "720. Indonesia: West Kalimantan", x == 721 ~ "721.
Indonesia: Central Kalimantan", x == 722 ~ "722. Indonesia: South Kalimantan",
    x == 723 ~ "723. Indonesia: East Kalimantan", x == 724 ~ "724. Indonesia: North
Sulawesi", x == 725 ~ "725. Indonesia: Central Sulawesi", x == 726 ~ "726.
Indonesia: South Sulawesi", x == 727 ~ "727. Indonesia: Southeast Sulawesi",
    x == 728 ~ "728. Indonesia: Gorontalo", x == 729 ~ "729. Indonesia: West
Sulawesi", x == 730 ~ "730. Indonesia: Maluku", x == 731 ~ "731. Indonesia:
North Maluku", x == 732 ~ "732. Indonesia: Papua", x == 733 ~ "733. Indonesia:
West Papua", x == 734 ~ "734. Indonesia: North Kalimantan", x == 801 ~ "801.
Israel: Jerusalem District", x == 802 ~ "802. Israel: Northern District",
    x == 803 ~ "803. Israel: Haifa District", x == 804 ~ "804. Israel: Central
District", x == 805 ~ "805. Israel: Tel Aviv District", x == 806 ~ "806. Israel:
Southern District", x == 901 ~ "901. Japan: Hokkaido", x == 902 ~ "902. Japan:
Aomori", x == 903 ~ "903. Japan: Iwate", x == 904 ~ "904. Japan: Miyagi", x ==
      905 ~ "905. Japan: Akita", x == 906 ~ "906. Japan: Yamagata", x == 907 ~ "907.
Japan: Fukushima", x == 908 ~ "908. Japan: Ibaraki", x == 909 ~ "909. Japan:
Tochigi", x == 910 ~ "910. Japan: Gunnma", x == 911 ~ "911. Japan: Saitama", x
    == 912 ~ "912. Japan: Chiba", x == 913 ~ "913. Japan: Tokyo", x == 914 ~ "914.
Japan: Kanagawa", x == 915 ~ "915. Japan: Niigata", x == 916 ~ "916. Japan:
Toyama", x == 917 ~ "917. Japan: Ishikawa", x == 918 ~ "918. Japan: Fukui", x
    == 919 ~ "919. Japan: Yamanashi", x == 920 ~ "920. Japan: Nagano", x == 921 ~
      "921. Japan: Gifu", x == 922 ~ "922. Japan: Shizuoka", x == 923 ~ "923. Japan:
Aichi", x == 924 ~ "924. Japan: Mie", x == 925 ~ "925. Japan: Shiga", x == 926
    ~ "926. Japan: Kyoto", x == 927 ~ "927. Japan: Osaka", x == 928 ~ "928. Japan:
Hyogo", x == 929 ~ "929. Japan: Nara", x == 930 ~ "930. Japan: Wakayama", x
    == 931 ~ "931. Japan: Tottori", x == 932 ~ "932. Japan: Shimane", x == 933 ~
      "933. Japan: Okayama", x == 934 ~ "934. Japan: Hiroshima", x == 935 ~ "935.
Japan: Yamaguchi", x == 936 ~ "936. Japan: Tokushima", x == 937 ~ "937. Japan:
Kagawa", x == 938 ~ "938. Japan: Ehime", x == 939 ~ "939. Japan: Kochi", x ==
      940 ~ "940. Japan: Fukuoka", x == 941 ~ "941. Japan: Saga", x == 942 ~ "942.
Japan: Nagasaki", x == 943 ~ "943. Japan: Kumamoto", x == 944 ~ "944. Japan:
Oita", x == 945 ~ "945. Japan: Miyazaki", x == 946 ~ "946. Japan: Kagoshima",
    x == 947 ~ "947. Japan: Okinawa", x == 1001 ~ "1001. Kenya: Baringo", x == 1002
    ~ "1002. Kenya: Bomet", x == 1003 ~ "1003. Kenya: Bungoma", x == 1004 ~ "1004.
Kenya: Busia", x == 1005 ~ "1005. Kenya: Elgeyo Marakwet", x == 1006 ~ "1006.
Kenya: Embu", x == 1007 ~ "1007. Kenya: Garissa", x == 1008 ~ "1008. Kenya: Homa
Bay", x == 1009 ~ "1009. Kenya: Isiolo", x == 1010 ~ "1010. Kenya: Kajiado",
    x == 1011 ~ "1011. Kenya: Kakamega", x == 1012 ~ "1012. Kenya: Kericho", x ==
      1013 ~ "1013. Kenya: Kiambu", x == 1014 ~ "1014. Kenya: Kilifi", x == 1015 ~
      "1015. Kenya: Kirinyaga", x == 1016 ~ "1016. Kenya: Kisii", x == 1017 ~ "1017.
Kenya: Kisumu", x == 1018 ~ "1018. Kenya: Kitui", x == 1019 ~ "1019. Kenya:
Kwale", x == 1020 ~ "1020. Kenya: Laikipia", x == 1021 ~ "1021. Kenya: Lamu",
    x == 1022 ~ "1022. Kenya: Machakos", x == 1023 ~ "1023. Kenya: Makueni", x
    == 1024 ~ "1024. Kenya: Mandera", x == 1025 ~ "1025. Kenya: Marsabit", x ==
      1026 ~ "1026. Kenya: Meru", x == 1027 ~ "1027. Kenya: Migori", x == 1028 ~
      "1028. Kenya: Mombasa", x == 1029 ~ "1029. Kenya: Murang a", x == 1030 ~ "1030.
Kenya: Nairobi", x == 1031 ~ "1031. Kenya: Nakuru", x == 1032 ~ "1032. Kenya:
Nandi", x == 1033 ~ "1033. Kenya: Narok", x == 1034 ~ "1034. Kenya: Nyamira",
    x == 1035 ~ "1035. Kenya: Nyandarua", x == 1036 ~ "1036. Kenya: Nyeri", x ==
      1037 ~ "1037. Kenya: Samburu", x == 1038 ~ "1038. Kenya: Siaya", x == 1039 ~
      "1039. Kenya: Taita Taveta", x == 1040 ~ "1040. Kenya: Tana River", x == 1041
    ~ "1041. Kenya: Tharaka Nithi", x == 1042 ~ "1042. Kenya: Trans Nzoia", x ==
      1043 ~ "1043. Kenya: Turkana", x == 1044 ~ "1044. Kenya: Uasin Gishu", x ==
      1045 ~ "1045. Kenya: Vihiga", x == 1046 ~ "1046. Kenya: Wajir", x == 1047 ~
      "1047. Kenya: West Pokot", x == 1101 ~ "1101. Mexico: Aguascalientes", x == 1102
    ~ "1102. Mexico: Baja California", x == 1103 ~ "1103. Mexico: Baja California
Sur", x == 1104 ~ "1104. Mexico: Campeche", x == 1105 ~ "1105. Mexico: Chiapas",
    x == 1106 ~ "1106. Mexico: Chihuahua", x == 1107 ~ "1107. Mexico: Coahuila
de Zaragoza", x == 1108 ~ "1108. Mexico: Colima", x == 1109 ~ "1109. Mexico:
Distrito Federal/Ciudad de México", x == 1110 ~ "1110. Mexico: Durango", x ==
      1111 ~ "1111. Mexico: Guanajuato", x == 1112 ~ "1112. Mexico: Guerrero", x ==
      1113 ~ "1113. Mexico: Hidalgo", x == 1114 ~ "1114. Mexico: Jalisco", x == 1115
    ~ "1115. Mexico: México", x == 1116 ~ "1116. Mexico: Michoacán de Ocampo", x ==
      1117 ~ "1117. Mexico: Morelos", x == 1118 ~ "1118. Mexico: Nayarit", x == 1119
    ~ "1119. Mexico: Nuevo León", x == 1120 ~ "1120. Mexico: Oaxaca", x == 1121 ~
      "1121. Mexico: Puebla", x == 1122 ~ "1122. Mexico: Querétaro Arteaga", x == 1123
    ~ "1123. Mexico: Quintana Roo", x == 1124 ~ "1124. Mexico: San Luis Potosí",
    x == 1125 ~ "1125. Mexico: Sinaloa", x == 1126 ~ "1126. Mexico: Sonora", x ==
      1127 ~ "1127. Mexico: Tabasco", x == 1128 ~ "1128. Mexico: Tamaulipas", x ==
      1129 ~ "1129. Mexico: Tlaxcala", x == 1130 ~ "1130. Mexico: Veracruz", x ==
      1131 ~ "1131. Mexico: Yucatán", x == 1132 ~ "1132. Mexico: Zacatecas", x ==
      1201 ~ "1201. Nigeria: Abia", x == 1202 ~ "1202. Nigeria: Abuja", x == 1203
    ~ "1203. Nigeria: Adamawa", x == 1204 ~ "1204. Nigeria: Akwa Ibom", x == 1205
    ~ "1205. Nigeria: Ananmbra", x == 1206 ~ "1206. Nigeria: Bauchi", x == 1207
    ~ "1207. Nigeria: Bayelsa", x == 1208 ~ "1208. Nigeria: Benue", x == 1209 ~
      "1209. Nigeria: Borno", x == 1210 ~ "1210. Nigeria: Cross-River", x == 1211 ~
      "1211. Nigeria: Delta", x == 1212 ~ "1212. Nigeria: Eboyin", x == 1213 ~ "1213.
Nigeria: Edo", x == 1214 ~ "1214. Nigeria: Ekiti", x == 1215 ~ "1215. Nigeria:
Enugu", x == 1216 ~ "1216. Nigeria: Gombe", x == 1217 ~ "1217. Nigeria: Imo",
    x == 1218 ~ "1218. Nigeria: Jigawa", x == 1219 ~ "1219. Nigeria: Kaduna", x ==
      1220 ~ "1220. Nigeria: Kano", x == 1221 ~ "1221. Nigeria: Katsina", x == 1222
    ~ "1222. Nigeria: Kebbi", x == 1223 ~ "1223. Nigeria: Kogi", x == 1224 ~ "1224.
Nigeria: Kwara", x == 1225 ~ "1225. Nigeria: Lagos", x == 1226 ~ "1226. Nigeria:
Nassarawa", x == 1227 ~ "1227. Nigeria: Niger", x == 1228 ~ "1228. Nigeria:
Ogun", x == 1229 ~ "1229. Nigeria: Ondo", x == 1230 ~ "1230. Nigeria: Osun", x
    == 1231 ~ "1231. Nigeria: Oyo", x == 1232 ~ "1232. Nigeria: Plateau", x == 1233
    ~ "1233. Nigeria: Rivers", x == 1234 ~ "1234. Nigeria: Sokoto", x == 1235 ~
      "1235. Nigeria: Taraba", x == 1236 ~ "1236. Nigeria: Yobe", x == 1237 ~ "1237.
Nigeria: Zamfara", x == 1301 ~ "1301. Philippines: National Capital Region
(NCR)", x == 1302 ~ "1302. Philippines: Cordillera (CAR)", x == 1303 ~ "1303.
Philippines: Ilocos (Region I)", x == 1304 ~ "1304. Philippines: Cagayan Valley
(Region II)", x == 1305 ~ "1305. Philippines: Central Luzon (Region III)", x
    == 1306 ~ "1306. Philippines: Calabarzon (Region IV-A)", x == 1307 ~ "1307.
Philippines: Mimaropa (Region IV-B)", x == 1308 ~ "1308. Philippines: Bicol
(Region V)", x == 1309 ~ "1309. Philippines: Western Visayas (Region VI)", x
    == 1310 ~ "1310. Philippines: Central Visayas (Region VII)", x == 1311 ~ "1311.
Philippines: Eastern Visayas (Region VIII)", x == 1312 ~ "1312. Philippines:
Zamboanga Peninsula (Region IX)", x == 1313 ~ "1313. Philippines: Northern
Mindanao (Region X)", x == 1314 ~ "1314. Philippines: Davao (Region XI)", x
    == 1315 ~ "1315. Philippines: SOCCSKSARGEN (Region XII)", x == 1316 ~ "1316.
Philippines: Caraga (Region XIII)", x == 1317 ~ "1317. Philippines: Bangsamoro
Autonomous Region in Muslim Mindanao (BARMM)", x == 1401 ~ "1401. Poland: Lodz
Voivodeship", x == 1402 ~ "1402. Poland: Masovian Voivodeship", x == 1403 ~
      "1403. Poland: Lesser Poland Voivodeship", x == 1404 ~ "1404. Poland: Silesian
Voivodeship", x == 1405 ~ "1405. Poland: Lublin Voivodeship", x == 1406 ~ "1406.
Poland: Subcarpathian Voivodeship", x == 1407 ~ "1407. Poland: Swietokrzyskie
Voivodeship", x == 1408 ~ "1408. Poland: Podlaskie Voivodeship", x == 1409 ~
      "1409. Poland: Greater Poland Voivodeship", x == 1410 ~ "1410. Poland: West
Pomeranian Voivodeship", x == 1411 ~ "1411. Poland: Lubusz Voivodeship", x ==
      1412 ~ "1412. Poland: Lower Silesian Voivodeship", x == 1413 ~ "1413. Poland:
Opole Voivodeship", x == 1414 ~ "1414. Poland: Kuyavian-Pomeranian Voivodeship",
    x == 1415 ~ "1415. Poland: Warmian-Masurian Voivodeship", x == 1416 ~ "1416.
Poland: Pomeranian Voivodeship", x == 1601 ~ "1601. South Africa: Eastern
Cape", x == 1602 ~ "1602. South Africa: Free State", x == 1603 ~ "1603. South
Africa: Gauteng", x == 1604 ~ "1604. South Africa: KwaZulu Natal", x == 1605
    ~ "1605. South Africa: Limpopo", x == 1606 ~ "1606. South Africa: Mpumalanga",
    x == 1607 ~ "1607. South Africa: North West", x == 1608 ~ "1608. South Africa:
Northern Cape", x == 1609 ~ "1609. South Africa: Western Cape", x == 1701 ~
      "1701. Spain: Galicia", x == 1702 ~ "1702. Spain: Asturias", x == 1703 ~ "1703.
Spain: Cantabria", x == 1704 ~ "1704. Spain: Basque Community", x == 1705 ~
      "1705. Spain: Navarre", x == 1706 ~ "1706. Spain: La Rioja", x == 1707 ~ "1707.
Spain: Aragon", x == 1708 ~ "1708. Spain: Madrid", x == 1709 ~ "1709. Spain:
Castile-Leon", x == 1710 ~ "1710. Spain: Castile-La Mancha", x == 1711 ~ "1711.
Spain: Extremadura", x == 1712 ~ "1712. Spain: Catalonia", x == 1713 ~ "1713.
Spain: Valencian Community", x == 1714 ~ "1714. Spain: Balearic Islands", x ==
      1715 ~ "1715. Spain: Andalusia", x == 1716 ~ "1716. Spain: Region of Murcia",
    x == 1717 ~ "1717. Spain: Ceuta", x == 1718 ~ "1718. Spain: Melilla", x == 1719
    ~ "1719. Spain: Canary Islands", x == 1801 ~ "1801. Tanzania: Dar es Salaam", x
    == 1802 ~ "1802. Tanzania: Dodoma", x == 1803 ~ "1803. Tanzania: Tabora", x ==
      1804 ~ "1804. Tanzania: Singida", x == 1805 ~ "1805. Tanzania: Shinyanga", x ==
      1806 ~ "1806. Tanzania: Ruvuma", x == 1807 ~ "1807. Tanzania: Rukwa", x == 1808
    ~ "1808. Tanzania: Pwani", x == 1809 ~ "1809. Tanzania: South Unguja", x == 1810
    ~ "1810. Tanzania: North Unguja", x == 1811 ~ "1811. Tanzania: South Pemba", x
    == 1812 ~ "1812. Tanzania: North Pemba", x == 1813 ~ "1813. Tanzania: Mwanza",
    x == 1814 ~ "1814. Tanzania: Mtwara", x == 1815 ~ "1815. Tanzania: Morogoro",
    x == 1816 ~ "1816. Tanzania: Mbeya", x == 1817 ~ "1817. Tanzania: Mara", x ==
      1818 ~ "1818. Tanzania: Manyara", x == 1820 ~ "1820. Tanzania: Kilimanjaro",
    x == 1821 ~ "1821. Tanzania: Kigoma", x == 1822 ~ "1822. Tanzania: Kagera", x
    == 1823 ~ "1823. Tanzania: Iringa", x == 1824 ~ "1824. Tanzania: Urban West",
    x == 1825 ~ "1825. Tanzania: Arusha", x == 1826 ~ "1826. Tanzania: Tanga",
    x == 1827 ~ "1827. Tanzania: Simiyu", x == 1828 ~ "1828. Tanzania: Katavi",
    x == 1829 ~ "1829. Tanzania: Geita", x == 1830 ~ "1830. Tanzania: Njombe", x
    == 1831 ~ "1831. Tanzania: Lindi", x == 1901 ~ "1901. Turkey: Istanbul", x ==
      1902 ~ "1902. Turkey: Tekirdag", x == 1903 ~ "1903. Turkey: Balikesir", x ==
      1904 ~ "1904. Turkey: Izmir", x == 1905 ~ "1905. Turkey: Aydin", x == 1906 ~
      "1906. Turkey: Manisa", x == 1907 ~ "1907. Turkey: Bursa", x == 1908 ~ "1908.
Turkey: Kocaeli", x == 1909 ~ "1909. Turkey: Ankara", x == 1910 ~ "1910. Turkey:
Konya", x == 1911 ~ "1911. Turkey: Antalya", x == 1912 ~ "1912. Turkey: Adana",
    x == 1913 ~ "1913. Turkey: Hatay", x == 1914 ~ "1914. Turkey: Kirikkale", x
    == 1915 ~ "1915. Turkey: Kayseri", x == 1916 ~ "1916. Turkey: Zonguldak", x
    == 1917 ~ "1917. Turkey: Kastamonu", x == 1918 ~ "1918. Turkey: Samsun", x
    == 1919 ~ "1919. Turkey: Trabzon", x == 1920 ~ "1920. Turkey: Erzurum", x ==
      1921 ~ "1921. Turkey: Agri", x == 1922 ~ "1922. Turkey: Malatya", x == 1923 ~
      "1923. Turkey: Van", x == 1924 ~ "1924. Turkey: Gaziantep", x == 1925 ~ "1925.
Turkey: Sanliurfa", x == 1926 ~ "1926. Turkey: Mardin", x == 2001 ~ "2001.
United Kingdom: Tees Valley and Durham", x == 2002 ~ "2002. United Kingdom:
Northumberland and Tyne and Wear", x == 2003 ~ "2003. United Kingdom: Cumbria",
    x == 2004 ~ "2004. United Kingdom: Greater Manchester", x == 2005 ~ "2005.
United Kingdom: Lancashire", x == 2006 ~ "2006. United Kingdom: Cheshire",
    x == 2007 ~ "2007. United Kingdom: Merseyside", x == 2008 ~ "2008. United
Kingdom: East Yorkshire and Northern Lincolnshire", x == 2009 ~ "2009. United
Kingdom: North Yorkshire", x == 2010 ~ "2010. United Kingdom: South Yorkshire",
    x == 2011 ~ "2011. United Kingdom: West Yorkshire", x == 2012 ~ "2012. United
Kingdom: Derbyshire and Nottinghamshire", x == 2013 ~ "2013. United Kingdom:
Leicestershire, Rutland and Northamptonshire", x == 2014 ~ "2014. United
Kingdom: Lincolnshire", x == 2015 ~ "2015. United Kingdom: Herefordshire,
Worcestershire and Warwickshire", x == 2016 ~ "2016. United Kingdom: Shropshire
and Staffordshire", x == 2017 ~ "2017. United Kingdom: West Midlands", x ==
      2018 ~ "2018. United Kingdom: East Anglia", x == 2019 ~ "2019. United Kingdom:
Bedfordshire and Hertfordshire", x == 2020 ~ "2020. United Kingdom: Essex",
    x == 2021 ~ "2021. United Kingdom: Inner London — West", x == 2022 ~ "2022.
United Kingdom: Inner London — East", x == 2023 ~ "2023. United Kingdom: Outer
London — East and North East", x == 2024 ~ "2024. United Kingdom: Outer London —
South", x == 2025 ~ "2025. United Kingdom: Outer London — West and North West",
    x == 2026 ~ "2026. United Kingdom: Berkshire, Buckinghamshire and Oxfordshire",
    x == 2027 ~ "2027. United Kingdom: Surrey, East and West Sussex", x == 2028 ~
      "2028. United Kingdom: Hampshire and Isle of Wight", x == 2029 ~ "2029. United
Kingdom: Kent", x == 2030 ~ "2030. United Kingdom: Gloucestershire, Wiltshire
and Bristol/Bath area", x == 2031 ~ "2031. United Kingdom: Dorset and Somerset",
    x == 2032 ~ "2032. United Kingdom: Cornwall and Isles of Scilly", x == 2033 ~
      "2033. United Kingdom: Devon", x == 2034 ~ "2034. United Kingdom: West Wales
and The Valleys", x == 2035 ~ "2035. United Kingdom: East Wales", x == 2036
    ~ "2036. United Kingdom: North Eastern Scotland", x == 2037 ~ "2037. United
Kingdom: Highlands and Islands", x == 2038 ~ "2038. United Kingdom: Eastern
Scotland", x == 2039 ~ "2039. United Kingdom: West Central Scotland", x ==
      2040 ~ "2040. United Kingdom: Southern Scotland", x == 2041 ~ "2041. United
Kingdom: Northern Ireland", x == 2201 ~ "2201. United States: Alaska", x == 2202
    ~ "2202. United States: Alabama", x == 2203 ~ "2203. United States: Arkansas",
    x == 2204 ~ "2204. United States: Arizona", x == 2205 ~ "2205. United States:
California", x == 2206 ~ "2206. United States: Colorado", x == 2207 ~ "2207.
United States: Connecticut", x == 2208 ~ "2208. United States: Delaware", x
    == 2209 ~ "2209. United States: Florida", x == 2210 ~ "2210. United States:
Georgia", x == 2211 ~ "2211. United States: Hawaii", x == 2212 ~ "2212. United
States: Iowa", x == 2213 ~ "2213. United States: Idaho", x == 2214 ~ "2214.
United States: Illinois", x == 2215 ~ "2215. United States: Indiana", x == 2216
    ~ "2216. United States: Kansas", x == 2217 ~ "2217. United States: Kentucky",
    x == 2218 ~ "2218. United States: Louisiana", x == 2219 ~ "2219. United States:
Massachusetts", x == 2220 ~ "2220. United States: Maryland", x == 2221 ~ "2221.
United States: Maine", x == 2222 ~ "2222. United States: Michigan", x == 2223 ~
      "2223. United States: Minnesota", x == 2224 ~ "2224. United States: Missouri", x
    == 2225 ~ "2225. United States: Mississippi", x == 2226 ~ "2226. United States:
Montana", x == 2227 ~ "2227. United States: North Carolina", x == 2228 ~ "2228.
United States: North Dakota", x == 2229 ~ "2229. United States: Nebraska", x ==
      2230 ~ "2230. United States: New Hampshire", x == 2231 ~ "2231. United States:
New Jersey", x == 2232 ~ "2232. United States: New Mexico", x == 2233 ~ "2233.
United States: Nevada", x == 2234 ~ "2234. United States: New York", x == 2235
    ~ "2235. United States: Ohio", x == 2236 ~ "2236. United States: Oklahoma",
    x == 2237 ~ "2237. United States: Oregon", x == 2238 ~ "2238. United States:
Pennsylvania", x == 2239 ~ "2239. United States: Rhode Island", x == 2240 ~
      "2240. United States: South Carolina", x == 2241 ~ "2241. United States: South
Dakota", x == 2242 ~ "2242. United States: Tennessee", x == 2243 ~ "2243.
United States: Texas", x == 2244 ~ "2244. United States: Utah", x == 2245 ~
      "2245. United States: Virginia", x == 2246 ~ "2246. United States: Vermont", x
    == 2247 ~ "2247. United States: Washington", x == 2248 ~ "2248. United States:
Wisconsin", x == 2249 ~ "2249. United States: West Virginia", x == 2250 ~
      "2250. United States: Wyoming", x == 2251 ~ "2251. United States: Other", x
    == 2301 ~ "2301. Sweden: Blekinge", x == 2302 ~ "2302. Sweden: Dalarna", x ==
      2303 ~ "2303. Sweden: Gavleborg", x == 2304 ~ "2304. Sweden: Gotland", x ==
      2305 ~ "2305. Sweden: Halland", x == 2306 ~ "2306. Sweden: Jamtland", x == 2307
    ~ "2307. Sweden: Jonkoping", x == 2308 ~ "2308. Sweden: Kalmar", x == 2309 ~
      "2309. Sweden: Kronoberg", x == 2310 ~ "2310. Sweden: Norrbotten", x == 2311
    ~ "2311. Sweden: Orebro", x == 2312 ~ "2312. Sweden: Ostergotland", x == 2313
    ~ "2313. Sweden: Skane", x == 2314 ~ "2314. Sweden: Sodermanland", x == 2315
    ~ "2315. Sweden: Stockholm", x == 2316 ~ "2316. Sweden: Uppsala", x == 2317 ~
      "2317. Sweden: Varmland", x == 2318 ~ "2318. Sweden: Vasterbotten", x == 2319
    ~ "2319. Sweden: Vasternorrland", x == 2320 ~ "2320. Sweden: Vastmanland", x ==
      2321 ~ "2321. Sweden: Vastra Gotaland", x == 2401 ~ "2401. Hong Kong: Hong Kong
Island", x == 2402 ~ "2402. Hong Kong: Kowloon", x == 2403 ~ "2403. Hong Kong:
New Territories",
    .default = "(Missing)"
    ),
    REGION2_W2 = case_when(x == 101 ~ "101. Argentina: Metropolitana", x == 102 ~ "102.
Argentina: Pampeana", x == 103 ~ "103. Argentina: Cuyo", x == 104 ~ "104.
Argentina: N.O.A. (Noroeste Argentino)", x == 105 ~ "105. Argentina: N.E.A.
(Noreste Argentino)", x == 106 ~ "106. Argentina: Patagonia", x == 201 ~ "201.
Australia: Australian Capital Territory", x == 202 ~ "202. Australia: New
South Wales", x == 203 ~ "203. Australia: Northern Territory", x == 204 ~ "204.
Australia: Queensland", x == 205 ~ "205. Australia: South Australia", x == 206
    ~ "206. Australia: Tasmania", x == 207 ~ "207. Australia: Victoria", x == 208
    ~ "208. Australia: Western Australia", x == 301 ~ "301. Brazil: South (SU)", x
    == 302 ~ "302. Brazil: Southeast (SE)", x == 303 ~ "303. Brazil: Central-West
(CO)", x == 304 ~ "304. Brazil: North (NO)", x == 305 ~ "305. Brazil:
Northeast (NE)", x == 401 ~ "401. Egypt: Urban Governorates", x == 402 ~ "402.
Egypt: Northern Governorates (Nile Delta)", x == 403 ~ "403. Egypt: Southern
Governorates (Upper Egypt)", x == 404 ~ "404. Egypt: Frontier Governorates",
    x == 501 ~ "501. Germany: Schleswig-Holstein", x == 502 ~ "502. Germany:
Hamburg", x == 503 ~ "503. Germany: Niedersachsen", x == 504 ~ "504. Germany:
Bremen", x == 505 ~ "505. Germany: Nordrhein-Westfalen", x == 506 ~ "506.
Germany: Hessen", x == 507 ~ "507. Germany: Rheinland-Pfalz", x == 508 ~ "508.
Germany: Baden-Wurttemberg", x == 509 ~ "509. Germany: Bayern", x == 510 ~ "510.
Germany: Saarland", x == 511 ~ "511. Germany: Berlin", x == 512 ~ "512. Germany:
Brandenburg", x == 513 ~ "513. Germany: Mecklenburg-Vorpommern", x == 514 ~
      "514. Germany: Sachsen", x == 515 ~ "515. Germany: Sachsen-Anhalt", x == 516 ~
      "516. Germany: Thuringen", x == 601 ~ "601. India: Central", x == 602 ~ "602.
India: East", x == 603 ~ "603. India: West", x == 604 ~ "604. India: North", x
    == 605 ~ "605. India: South", x == 701 ~ "701. Indonesia: Sumatra", x == 702 ~
      "702. Indonesia: DKI Jakarta, Banten, West Java", x == 703 ~ "703. Indonesia:
Central Java and DI Yogjakarta", x == 704 ~ "704. Indonesia: East Java", x ==
      705 ~ "705. Indonesia: Kalimantan", x == 706 ~ "706. Indonesia: Sulawesi", x ==
      707 ~ "707. Indonesia: Bali, NTB, NTT", x == 708 ~ "708. Indonesia: Indonesia
Timur", x == 901 ~ "901. Japan: Hokkaido", x == 902 ~ "902. Japan: Tohoku", x ==
      903 ~ "903. Japan: Kanto", x == 904 ~ "904. Japan: Hokuriku", x == 905 ~ "905.
Japan: Koshinetsu", x == 906 ~ "906. Japan: Tokai", x == 907 ~ "907. Japan:
Chukyo", x == 908 ~ "908. Japan: Kansai", x == 909 ~ "909. Japan: Chugoku", x ==
      910 ~ "910. Japan: Shikoku", x == 911 ~ "911. Japan: Kyushu", x == 1001 ~ "1001.
Kenya: Nairobi Province", x == 1002 ~ "1002. Kenya: Central Province", x == 1003
    ~ "1003. Kenya: Eastern Province", x == 1004 ~ "1004. Kenya: Nyanza Province", x
    == 1005 ~ "1005. Kenya: Rift Valley Province", x == 1006 ~ "1006. Kenya: Western
Province", x == 1007 ~ "1007. Kenya: North Eastern Province", x == 1008 ~ "1008.
Kenya: Coast Province", x == 1101 ~ "1101. Mexico: Norte (North)", x == 1102
    ~ "1102. Mexico: Sur (South)", x == 1103 ~ "1103. Mexico: Centro (Central)",
    x == 1201 ~ "1201. Nigeria: North Central", x == 1202 ~ "1202. Nigeria: North
East", x == 1203 ~ "1203. Nigeria: North West", x == 1204 ~ "1204. Nigeria:
South East", x == 1205 ~ "1205. Nigeria: South South", x == 1206 ~ "1206.
Nigeria: South West", x == 1301 ~ "1301. Philippines: NCR", x == 1303 ~ "1303.
Philippines: Balance Luzon", x == 1304 ~ "1304. Philippines: Visayas", x == 1305
    ~ "1305. Philippines: Mindanao", x == 1401 ~ "1401. Poland: Central region", x
    == 1402 ~ "1402. Poland: South region", x == 1403 ~ "1403. Poland: East region",
    x == 1404 ~ "1404. Poland: Northwest region", x == 1405 ~ "1405. Poland:
Southwest region", x == 1406 ~ "1406. Poland: North region", x == 1407 ~ "1407.
Poland: Masovian", x == 1701 ~ "1701. Spain: North-West", x == 1702 ~ "1702.
Spain: North-East", x == 1703 ~ "1703. Spain: Community of Madrid", x == 1704
    ~ "1704. Spain: Centre", x == 1705 ~ "1705. Spain: East", x == 1706 ~ "1706.
Spain: South", x == 1707 ~ "1707. Spain: The Canaries", x == 1801 ~ "1801.
Tanzania: Central", x == 1802 ~ "1802. Tanzania: Coastal", x == 1803 ~ "1803.
Tanzania: Islands", x == 1804 ~ "1804. Tanzania: Lake", x == 1805 ~ "1805.
Tanzania: Mountain", x == 1806 ~ "1806. Tanzania: Northern", x == 1807 ~ "1807.
Tanzania: Southern", x == 1808 ~ "1808. Tanzania: Western", x == 1901 ~ "1901.
Turkey: Istanbul Region", x == 1902 ~ "1902. Turkey: West Marmara Region", x ==
      1903 ~ "1903. Turkey: Aegean Region", x == 1904 ~ "1904. Turkey: East Marmara
Region", x == 1905 ~ "1905. Turkey: West Anatolia Region", x == 1906 ~ "1906.
Turkey: Mediterranean Region", x == 1907 ~ "1907. Turkey: Central Anatolia
Region", x == 1908 ~ "1908. Turkey: West Black Sea Region", x == 1909 ~ "1909.
Turkey: East Black Sea Region", x == 1910 ~ "1910. Turkey: Northeast Anatolia
Region", x == 1911 ~ "1911. Turkey: Central East Anatolia Region", x == 1912 ~
      "1912. Turkey: Southeast Anatolia Region", x == 2001 ~ "2001. United Kingdom:
East Midlands", x == 2002 ~ "2002. United Kingdom: East of England", x == 2003
    ~ "2003. United Kingdom: London", x == 2004 ~ "2004. United Kingdom: North
East", x == 2005 ~ "2005. United Kingdom: North West", x == 2006 ~ "2006. United
Kingdom: Scotland", x == 2007 ~ "2007. United Kingdom: South East", x == 2008 ~
      "2008. United Kingdom: South West", x == 2009 ~ "2009. United Kingdom: Wales",
    x == 2010 ~ "2010. United Kingdom: West Midlands", x == 2011 ~ "2011. United
Kingdom: Yorkshire and The Humber", x == 2012 ~ "2012. United Kingdom: Northern
Ireland", x == 2201 ~ "2201. United States: New England Division", x == 2202
    ~ "2202. United States: Middle Atlantic Division", x == 2203 ~ "2203. United
States: East North Division", x == 2204 ~ "2204. United States: West North
Division", x == 2205 ~ "2205. United States: South Atlantic Division", x == 2206
    ~ "2206. United States: East South Division", x == 2207 ~ "2207. United States:
West South Division", x == 2208 ~ "2208. United States: Mountain Division", x
    == 2209 ~ "2209. United States: Pacific Division", x == 2301 ~ "2301. Sweden:
Stockholm", x == 2302 ~ "2302. Sweden: Ostra Mellansverige", x == 2303 ~ "2303.
Sweden: Sydsverige", x == 2304 ~ "2304. Sweden: Norra Mellansverige", x == 2305
    ~ "2305. Sweden: Mellersta Norrland", x == 2306 ~ "2306. Sweden: Ovre Norrland",
    x == 2307 ~ "2307. Sweden: Smaland med oarna", x == 2308 ~ "2308. Sweden:
Vastverige",
    .default = "(Missing)"
    ),
    REGION3_W2 = case_when(x == 1901 ~ "1901. Turkey: Aegean Region (Ege Bolgesi)", x == 1902 ~
      "1902. Turkey: Black Sea Region (Karadeniz Bolgesi)", x == 1903 ~ "1903. Turkey:
Central Anatolia Region (Ic Anadolu Bolgesi)", x == 1904 ~ "1904. Turkey:
Eastern Anatolia Region (Dogu Anadolu Bolgesi)", x == 1905 ~ "1905. Turkey:
Marmara Region (Marmara Bolgesi)", x == 1906 ~ "1906. Turkey: Mediterranean
Region (Akdeniz Bolgesi)", x == 1907 ~ "1907. Turkey: Southeastern Anatolia
Region (Guneydogu Anadolu Bolgesi)", x == 2201 ~ "2201. United States: Northeast
Region", x == 2202 ~ "2202. United States: Midwest Region", x == 2203 ~
      "2203. United States: South Region", x == 2204 ~ "2204. United States: West
Region", .default = "(Missing)"),
    REL_EXPERIENC_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    REL_IMPORTANT_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    REL1_W2 = case_when(x == 1 ~ "1. Christianity", x == 2 ~ "2. Islam", x == 3 ~ "3.
Hinduism", x == 4 ~ "4. Buddhism", x == 5 ~ "5. Judaism", x == 6 ~ "6.
Sikhism", x == 7 ~ "7. Baha’i", x == 8 ~ "8. Jainism", x == 9 ~ "9. Shinto",
      x == 10 ~ "10. Taoism", x == 11 ~ "11. Confucianism", x == 12 ~ "12. Primal,
Animist, or Folk religion", x == 13 ~ "13. Spiritism", x == 14 ~ "14. Umbanda,
Candomblé, and other African-derived religions", x == 15 ~ "15. Chinese
folk/traditional religion", x == 96 ~ "96. Some other religion", x == 97 ~ "97.
No religion/Atheist/Agnostic",
      .default = "(Missing)"
    ),
    REL2_W2 = case_when(x == 1 ~ "1. Christianity", x == 2 ~ "2. Islam", x == 3 ~ "3.
Hinduism", x == 4 ~ "4. Buddhism", x == 5 ~ "5. Judaism", x == 6 ~ "6.
Sikhism", x == 7 ~ "7. Baha’i", x == 8 ~ "8. Jainism", x == 9 ~ "9. Shinto",
      x == 10 ~ "10. Taoism", x == 11 ~ "11. Confucianism", x == 12 ~ "12. Primal,
Animist, or Folk religion", x == 13 ~ "13. Spiritism", x == 14 ~ "14. Umbanda,
Candomblé, and other African-derived religions", x == 15 ~ "15. Chinese
folk/traditional religion", x == 96 ~ "96. Some other religion", x == 97 ~ "97.
No religion/Atheist/Agnostic",
      .default = "(Missing)"
    ),
    REL3_W2 = case_when(x == 1 ~ "1. Catholic", x == 2 ~ "2. Orthodox", x == 3 ~ "3.
Anglican/Episcopal", x == 4 ~ "4. Presbyterian/Reformed, such as [insert
country specific examples]", x == 5 ~ "5. Lutheran", x == 6 ~ "6. Methodist",
      x == 7 ~ "7. Baptist, such as [insert country specific examples]", x == 8 ~
        "8. Pentecostal/Charismatic denominations, such as [insert country specific
examples]", x == 9 ~ "9. Independent Church, Holiness, or Evangelical, such as
[insert country specific examples]", x == 10 ~ "10. Church of Jesus Christ of
Latter-Day Saints/Other Mormon tradition", x == 11 ~ "11. Jehovah’s Witness",
      x == 12 ~ "12. Seventh Day Adventist", x == 13 ~ "13. Prophetic, Ethiopian,
or Zionist (AIC, African Initiated Church)", x == 96 ~ "96. Some other
denomination", x == 97 ~ "97. No denomination in particular (just Christian/just
Protestant)",
      .default = "(Missing)"
    ),
    REL4_W2 = case_when(x == 1 ~ "1. Sunni", x == 2 ~ "2. Shi’a", x == 3 ~ "3. Sufis", x ==
      4 ~ "4. Bohra", x == 5 ~ "5. Ahmadiyya", x == 6 ~ "6. Khojas", x == 7 ~ "7.
Quranists", x == 8 ~ "8. Alevi", x == 96 ~ "96. Some other sect", x == 97 ~ "97.
No sect in particular (just Muslim)", .default = "(Missing)"),
    REL5_W2 = case_when(x == 1 ~ "1. Vaishnavite", x == 2 ~ "2. Shaivite", x == 3 ~ "3.
Shakta", x == 4 ~ "4. Smarta", x == 96 ~ "96. Some other sect or denomination",
      x == 97 ~ "97. No sect or denomination in particular (just Hindu)",
      .default =
        "(Missing)"
    ),
    REL6_W2 = case_when(x == 1 ~ "1. Secular", x == 2 ~ "2. Reform", x == 3 ~ "3.
Conservative", x == 4 ~ "4. Orthodox", x == 5 ~ "5. Non-Orthodox", x == 6 ~
      "6. Hiloni", x == 7 ~ "7. Masorti lo dati", x == 8 ~ "8. Masorti dati", x ==
      9 ~ "9. Dati", x == 10 ~ "10. Haredi", x == 96 ~ "96. Some other denomination
or tradition (Please specify)", x == 97 ~ "97. No denomination or tradition in
particular (just Jewish)", .default = "(Missing)"),
    REL7_W2 = case_when(x == 1 ~ "1. Atheist – do not believe in any god", x == 2 ~ "2.
Agnostic – unsure whether a God or gods exist", x == 3 ~ "3. Neither",
      .default = "(Missing)"
    ),
    REL8_W2 = case_when(x == 1 ~ "1. Spiritual", x == 2 ~ "2. Religious", x == 3 ~ "3. Both",
      x == 4 ~ "4. Neither",
      .default = "(Missing)"
    ),
    REL9_W2 = case_when(x == 1 ~ "1. Jodo sect (Honen)", x == 2 ~ "2. Jodoshin sect
(Shinran)", x == 3 ~ "3. Rinzai sect (Eisai)", x == 4 ~ "4. Soto sect (Dogen)",
      x == 5 ~ "5. Ji sect (Ippen)", x == 6 ~ "6. Hokke sect (the Nichiren sect,
Nichiren)", x == 96 ~ "96. Some other sect", x == 97 ~ "97. No sect in
particular",
      .default = "(Missing)"
    ),
    SACRED_TEXTS_W2 = case_when(x == 1 ~ "1. More than once a day", x == 2 ~ "2. About once a day", x
    == 3 ~ "3. Sometimes", x == 4 ~ "4. Never", .default = "(Missing)"),
    SAT_LIVE_W2 = case_when(x == 1 ~ "1. Satisfied", x == 2 ~ "2. Dissatisfied", x == 3 ~ "3.
Unsure", .default = "(Missing)"),
    SAT_RELATNSHP_W2 = x,
    SAY_IN_GOVT_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3.
Unsure", .default = "(Missing)"),
    SELFID1_W2 = case_when(x == 101 ~ "101. Argentina: Asian", x == 102 ~ "102. Argentina:
Black", x == 103 ~ "103. Argentina: Indigenous", x == 104 ~ "104. Argentina:
Mestizo(a)", x == 105 ~ "105. Argentina: Mullato(a)", x == 106 ~ "106.
Argentina: White", x == 201 ~ "201. Australia: Aboriginal", x == 202
    ~ "202. Australia: Australian", x == 203 ~ "203. Australia: Australian
British/European", x == 204 ~ "204. Australia: Chinese", x == 205 ~ "205.
Australia: Indian", x == 206 ~ "206. Australia: Japanese", x == 207 ~ "207.
Australia: Malay", x == 208 ~ "208. Australia: Sinhalese", x == 209 ~ "209.
Australia: Spanish", x == 210 ~ "210. Australia: Sri Lankan Moor", x == 211 ~
      "211. Australia: Sri Lankan Tamil", x == 212 ~ "212. Australia: Vietnamese", x
    == 213 ~ "213. Australia: Taiwanese/Holo", x == 214 ~ "214. Australia: Russian",
    x == 215 ~ "215. Australia: Samoan", x == 216 ~ "216. Australia: New Zealander",
    x == 217 ~ "217. Australia: Other European", x == 301 ~ "301. Brazil: Branca",
    x == 302 ~ "302. Brazil: Preta", x == 303 ~ "303. Brazil: Parda", x == 304 ~
      "304. Brazil: Amarela", x == 305 ~ "305. Brazil: Indígena", x == 401 ~ "401.
Egypt: Arab", x == 402 ~ "402. Egypt: Turkish", x == 403 ~ "403. Egypt: Greek",
    x == 404 ~ "404. Egypt: Abazas", x == 405 ~ "405. Egypt: Bedouin Arab", x ==
      406 ~ "406. Egypt: Siwis", x == 407 ~ "407. Egypt: Nubian", x == 601 ~ "601.
India: General", x == 602 ~ "602. India: Other backward caste", x == 603 ~
      "603. India: Schedule caste", x == 604 ~ "604. India: Schedule tribe", x == 701
    ~ "701. Indonesia: Banjar/Melayu Banjar", x == 702 ~ "702. Indonesia: Betawi",
    x == 703 ~ "703. Indonesia: Bugis", x == 704 ~ "704. Indonesia: Jawa", x ==
      705 ~ "705. Indonesia: Madura", x == 706 ~ "706. Indonesia: Minangkabau", x ==
      707 ~ "707. Indonesia: Sunda/Parahyangan", x == 708 ~ "708. Indonesia: Bali",
    x == 709 ~ "709. Indonesia: Batak", x == 710 ~ "710. Indonesia: Makasar", x ==
      801 ~ "801. Israel: Jewish", x == 802 ~ "802. Israel: Arab", x == 901 ~ "901.
Japan: Chinese", x == 902 ~ "902. Japan: Japanese", x == 903 ~ "903. Japan:
Korean", x == 1001 ~ "1001. Kenya: Luhya", x == 1002 ~ "1002. Kenya: Luo", x
    == 1003 ~ "1003. Kenya: Kalenjin", x == 1004 ~ "1004. Kenya: Kamba", x == 1005
    ~ "1005. Kenya: Kikuyu", x == 1006 ~ "1006. Kenya: Kisii", x == 1007 ~ "1007.
Kenya: Maasai", x == 1008 ~ "1008. Kenya: Meru", x == 1009 ~ "1009. Kenya:
Kenyan Somali/Somali", x == 1010 ~ "1010. Kenya: Miji Kenda tribes", x == 1011
    ~ "1011. Kenya: Embu", x == 1101 ~ "1101. Mexico: White", x == 1102 ~ "1102.
Mexico: Mestizo", x == 1103 ~ "1103. Mexico: Indigenous", x == 1104 ~ "1104.
Mexico: Black", x == 1105 ~ "1105. Mexico: Mulatto", x == 1201 ~ "1201. Nigeria:
Hausa", x == 1202 ~ "1202. Nigeria: Yoruba", x == 1203 ~ "1203. Nigeria: Igbo
(Ibo)", x == 1204 ~ "1204. Nigeria: Edo", x == 1205 ~ "1205. Nigeria: Urhobo",
    x == 1206 ~ "1206. Nigeria: Fulani", x == 1207 ~ "1207. Nigeria: Kanuri",
    x == 1208 ~ "1208. Nigeria: Tiv", x == 1209 ~ "1209. Nigeria: Efik", x ==
      1210 ~ "1210. Nigeria: Ijaw", x == 1211 ~ "1211. Nigeria: Igala", x == 1212
    ~ "1212. Nigeria: Ibibio", x == 1213 ~ "1213. Nigeria: Idoma", x == 1301 ~
      "1301. Philippines: Tagalog", x == 1302 ~ "1302. Philippines: Cebuano", x ==
      1303 ~ "1303. Philippines: Ilocano/Ilokano", x == 1304 ~ "1304. Philippines:
Visayan/Bisaya", x == 1305 ~ "1305. Philippines: Ilonggo/Hiligaynon", x ==
      1306 ~ "1306. Philippines: Bicolano/Bikolano", x == 1307 ~ "1307. Philippines:
Waray", x == 1308 ~ "1308. Philippines: Tausug", x == 1309 ~ "1309. Philippines:
Maranao", x == 1310 ~ "1310. Philippines: Maguindanaoan", x == 1311 ~ "1311.
Philippines: Chinese-Filipino", x == 1312 ~ "1312. Philippines: Kapampangan",
    x == 1313 ~ "1313. Philippines: Pangasinense", x == 1314 ~ "1314. Philippines:
Zamboangueno", x == 1315 ~ "1315. Philippines: Malay", x == 1316 ~ "1316.
Philippines: Masbateno", x == 1317 ~ "1317. Philippines: Aeta", x == 1318 ~
      "1318. Philippines: Igorot", x == 1319 ~ "1319. Philippines: Mangyan", x ==
      1320 ~ "1320. Philippines: Badjao", x == 1401 ~ "1401. Poland: Polish", x ==
      1402 ~ "1402. Poland: German", x == 1403 ~ "1403. Poland: Belarussian", x ==
      1404 ~ "1404. Poland: Ukrainian", x == 1405 ~ "1405. Poland: Roma", x == 1406
    ~ "1406. Poland: Russian", x == 1407 ~ "1407. Poland: Ethnic Jewish", x ==
      1408 ~ "1408. Poland: Lemko", x == 1409 ~ "1409. Poland: Silesia", x == 1410 ~
      "1410. Poland: Kashubians", x == 1601 ~ "1601. South Africa: Black", x == 1602
    ~ "1602. South Africa: Asian/Indian", x == 1603 ~ "1603. South Africa: Colored",
    x == 1604 ~ "1604. South Africa: White", x == 1801 ~ "1801. Tanzania: African",
    x == 1802 ~ "1802. Tanzania: Indian", x == 1803 ~ "1803. Tanzania: Arab", x
    == 1901 ~ "1901. Turkey: Turkish", x == 1902 ~ "1902. Turkey: Kurdish/Zaza",
    x == 1903 ~ "1903. Turkey: Arab", x == 1904 ~ "1904. Turkey: Laz", x == 1905
    ~ "1905. Turkey: Circassian", x == 1906 ~ "1906. Turkey: Bosnian", x == 1907
    ~ "1907. Turkey: Armenian", x == 1908 ~ "1908. Turkey: Georgian", x == 1909 ~
      "1909. Turkey: Uyghur", x == 1910 ~ "1910. Turkey: Jewish", x == 1911 ~ "1911.
Turkey: Albanian", x == 1912 ~ "1912. Turkey: Greek", x == 1913 ~ "1913. Turkey:
Azeri", x == 2001 ~ "2001. United Kingdom: Asian", x == 2002 ~ "2002. United
Kingdom: Black", x == 2003 ~ "2003. United Kingdom: White", x == 2201 ~ "2201.
United States: White", x == 2202 ~ "2202. United States: Other", x == 2203 ~
      "2203. United States: Black", x == 2204 ~ "2204. United States: Asian", x ==
      2205 ~ "2205. United States: Hispanic", x == 2401 ~ "2401. Hong Kong: Chinese
(Cantonese)", x == 2402 ~ "2402. Hong Kong: Chinese (Chaoshan)", x == 2403 ~
      "2403. Hong Kong: Chinese (Fujianese)", x == 2404 ~ "2404. Hong Kong: Chinese
(Hakka)", x == 2405 ~ "2405. Hong Kong: Chinese (Shanghainese)", x == 2406 ~
      "2406. Hong Kong: Chinese (Other ethnicity)", x == 2407 ~ "2407. Hong Kong:
East Asian (Korean, Japanese)", x == 2408 ~ "2408. Hong Kong: Southeast Asian
(Filipino, Indonesian, Thailand)", x == 2409 ~ "2409. Hong Kong: South Asian
(Indian, Nepalese, Pakistani)", x == 2410 ~ "2410. Hong Kong: Taiwanese", x ==
      2411 ~ "2411. Hong Kong: White", x == 9995 ~ "9995. Prefer not to answer", x
    == 9996 ~ "9996. Other", x == 9997 ~ "9997. (No other response)",
    .default =
      "(Missing)"
    ),
    SELFID2_W2 = case_when(x == 101 ~ "101. Argentina: Asian", x == 102 ~ "102. Argentina:
Black", x == 103 ~ "103. Argentina: Indigenous", x == 104 ~ "104. Argentina:
Mestizo(a)", x == 105 ~ "105. Argentina: Mullato(a)", x == 106 ~ "106.
Argentina: White", x == 201 ~ "201. Australia: Aboriginal", x == 202
    ~ "202. Australia: Australian", x == 203 ~ "203. Australia: Australian
British/European", x == 204 ~ "204. Australia: Chinese", x == 205 ~ "205.
Australia: Indian", x == 206 ~ "206. Australia: Japanese", x == 207 ~ "207.
Australia: Malay", x == 208 ~ "208. Australia: Sinhalese", x == 209 ~ "209.
Australia: Spanish", x == 210 ~ "210. Australia: Sri Lankan Moor", x == 211 ~
      "211. Australia: Sri Lankan Tamil", x == 212 ~ "212. Australia: Vietnamese", x
    == 213 ~ "213. Australia: Taiwanese/Holo", x == 214 ~ "214. Australia: Russian",
    x == 215 ~ "215. Australia: Samoan", x == 216 ~ "216. Australia: New Zealander",
    x == 217 ~ "217. Australia: Other European", x == 301 ~ "301. Brazil: Branca",
    x == 302 ~ "302. Brazil: Preta", x == 303 ~ "303. Brazil: Parda", x == 304 ~
      "304. Brazil: Amarela", x == 305 ~ "305. Brazil: Indígena", x == 401 ~ "401.
Egypt: Arab", x == 402 ~ "402. Egypt: Turkish", x == 403 ~ "403. Egypt: Greek",
    x == 404 ~ "404. Egypt: Abazas", x == 405 ~ "405. Egypt: Bedouin Arab", x ==
      406 ~ "406. Egypt: Siwis", x == 407 ~ "407. Egypt: Nubian", x == 601 ~ "601.
India: General", x == 602 ~ "602. India: Other backward caste", x == 603 ~
      "603. India: Schedule caste", x == 604 ~ "604. India: Schedule tribe", x == 701
    ~ "701. Indonesia: Banjar/Melayu Banjar", x == 702 ~ "702. Indonesia: Betawi",
    x == 703 ~ "703. Indonesia: Bugis", x == 704 ~ "704. Indonesia: Jawa", x ==
      705 ~ "705. Indonesia: Madura", x == 706 ~ "706. Indonesia: Minangkabau", x ==
      707 ~ "707. Indonesia: Sunda/Parahyangan", x == 708 ~ "708. Indonesia: Bali",
    x == 709 ~ "709. Indonesia: Batak", x == 710 ~ "710. Indonesia: Makasar", x ==
      801 ~ "801. Israel: Jewish", x == 802 ~ "802. Israel: Arab", x == 901 ~ "901.
Japan: Chinese", x == 902 ~ "902. Japan: Japanese", x == 903 ~ "903. Japan:
Korean", x == 1001 ~ "1001. Kenya: Luhya", x == 1002 ~ "1002. Kenya: Luo", x
    == 1003 ~ "1003. Kenya: Kalenjin", x == 1004 ~ "1004. Kenya: Kamba", x == 1005
    ~ "1005. Kenya: Kikuyu", x == 1006 ~ "1006. Kenya: Kisii", x == 1007 ~ "1007.
Kenya: Maasai", x == 1008 ~ "1008. Kenya: Meru", x == 1009 ~ "1009. Kenya:
Kenyan Somali/Somali", x == 1010 ~ "1010. Kenya: Miji Kenda tribes", x == 1011
    ~ "1011. Kenya: Embu", x == 1101 ~ "1101. Mexico: White", x == 1102 ~ "1102.
Mexico: Mestizo", x == 1103 ~ "1103. Mexico: Indigenous", x == 1104 ~ "1104.
Mexico: Black", x == 1105 ~ "1105. Mexico: Mulatto", x == 1201 ~ "1201. Nigeria:
Hausa", x == 1202 ~ "1202. Nigeria: Yoruba", x == 1203 ~ "1203. Nigeria: Igbo
(Ibo)", x == 1204 ~ "1204. Nigeria: Edo", x == 1205 ~ "1205. Nigeria: Urhobo",
    x == 1206 ~ "1206. Nigeria: Fulani", x == 1207 ~ "1207. Nigeria: Kanuri",
    x == 1208 ~ "1208. Nigeria: Tiv", x == 1209 ~ "1209. Nigeria: Efik", x ==
      1210 ~ "1210. Nigeria: Ijaw", x == 1211 ~ "1211. Nigeria: Igala", x == 1212
    ~ "1212. Nigeria: Ibibio", x == 1213 ~ "1213. Nigeria: Idoma", x == 1301 ~
      "1301. Philippines: Tagalog", x == 1302 ~ "1302. Philippines: Cebuano", x ==
      1303 ~ "1303. Philippines: Ilocano/Ilokano", x == 1304 ~ "1304. Philippines:
Visayan/Bisaya", x == 1305 ~ "1305. Philippines: Ilonggo/Hiligaynon", x ==
      1306 ~ "1306. Philippines: Bicolano/Bikolano", x == 1307 ~ "1307. Philippines:
Waray", x == 1308 ~ "1308. Philippines: Tausug", x == 1309 ~ "1309. Philippines:
Maranao", x == 1310 ~ "1310. Philippines: Maguindanaoan", x == 1311 ~ "1311.
Philippines: Chinese-Filipino", x == 1312 ~ "1312. Philippines: Kapampangan",
    x == 1313 ~ "1313. Philippines: Pangasinense", x == 1314 ~ "1314. Philippines:
Zamboangueno", x == 1315 ~ "1315. Philippines: Malay", x == 1316 ~ "1316.
Philippines: Masbateno", x == 1317 ~ "1317. Philippines: Aeta", x == 1318 ~
      "1318. Philippines: Igorot", x == 1319 ~ "1319. Philippines: Mangyan", x ==
      1320 ~ "1320. Philippines: Badjao", x == 1401 ~ "1401. Poland: Polish", x ==
      1402 ~ "1402. Poland: German", x == 1403 ~ "1403. Poland: Belarussian", x ==
      1404 ~ "1404. Poland: Ukrainian", x == 1405 ~ "1405. Poland: Roma", x == 1406
    ~ "1406. Poland: Russian", x == 1407 ~ "1407. Poland: Ethnic Jewish", x ==
      1408 ~ "1408. Poland: Lemko", x == 1409 ~ "1409. Poland: Silesia", x == 1410 ~
      "1410. Poland: Kashubians", x == 1601 ~ "1601. South Africa: Black", x == 1602
    ~ "1602. South Africa: Asian/Indian", x == 1603 ~ "1603. South Africa: Colored",
    x == 1604 ~ "1604. South Africa: White", x == 1801 ~ "1801. Tanzania: African",
    x == 1802 ~ "1802. Tanzania: Indian", x == 1803 ~ "1803. Tanzania: Arab", x
    == 1901 ~ "1901. Turkey: Turkish", x == 1902 ~ "1902. Turkey: Kurdish/Zaza",
    x == 1903 ~ "1903. Turkey: Arab", x == 1904 ~ "1904. Turkey: Laz", x == 1905
    ~ "1905. Turkey: Circassian", x == 1906 ~ "1906. Turkey: Bosnian", x == 1907
    ~ "1907. Turkey: Armenian", x == 1908 ~ "1908. Turkey: Georgian", x == 1909 ~
      "1909. Turkey: Uyghur", x == 1910 ~ "1910. Turkey: Jewish", x == 1911 ~ "1911.
Turkey: Albanian", x == 1912 ~ "1912. Turkey: Greek", x == 1913 ~ "1913. Turkey:
Azeri", x == 2001 ~ "2001. United Kingdom: Asian", x == 2002 ~ "2002. United
Kingdom: Black", x == 2003 ~ "2003. United Kingdom: White", x == 2201 ~ "2201.
United States: White", x == 2202 ~ "2202. United States: Other", x == 2203 ~
      "2203. United States: Black", x == 2204 ~ "2204. United States: Asian", x ==
      2205 ~ "2205. United States: Hispanic", x == 2401 ~ "2401. Hong Kong: Chinese
(Cantonese)", x == 2402 ~ "2402. Hong Kong: Chinese (Chaoshan)", x == 2403 ~
      "2403. Hong Kong: Chinese (Fujianese)", x == 2404 ~ "2404. Hong Kong: Chinese
(Hakka)", x == 2405 ~ "2405. Hong Kong: Chinese (Shanghainese)", x == 2406 ~
      "2406. Hong Kong: Chinese (Other ethnicity)", x == 2407 ~ "2407. Hong Kong:
East Asian (Korean, Japanese)", x == 2408 ~ "2408. Hong Kong: Southeast Asian
(Filipino, Indonesian, Thailand)", x == 2409 ~ "2409. Hong Kong: South Asian
(Indian, Nepalese, Pakistani)", x == 2410 ~ "2410. Hong Kong: Taiwanese", x ==
      2411 ~ "2411. Hong Kong: White", x == 9995 ~ "9995. Prefer not to answer", x
    == 9996 ~ "9996. Other", x == 9997 ~ "9997. (No other response)",
    .default =
      "(Missing)"
    ),
    SHOW_LOVE_W2 = x,
    SUFFERING_W2 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    SVCS_12YRS_W2 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never",
      .default =
        "(Missing)"
    ),
    SVCS_FATHER_W2 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never", x == 97 ~
      "97. (Does not apply)", .default = "(Missing)"),
    SVCS_MOTHER_W2 = case_when(x == 1 ~ "1. At least once a week", x == 2 ~ "2. One to three times
a month", x == 3 ~ "3. Less than once a month", x == 4 ~ "4. Never", x == 97 ~
      "97. (Does not apply)", .default = "(Missing)"),
    TEACHINGS_1_W2 = x,
    TEACHINGS_2_W2 = x,
    TEACHINGS_3_W2 = x,
    TEACHINGS_4_W2 = x,
    TEACHINGS_5_W2 = x,
    TEACHINGS_6_W2 = x,
    TEACHINGS_7_W2 = x,
    TEACHINGS_8_W2 = x,
    TEACHINGS_9_W2 = x,
    TEACHINGS_10_W2 = x,
    TEACHINGS_11_W2 = x,
    TEACHINGS_12_W2 = x,
    TEACHINGS_13_W2 = x,
    TEACHINGS_14_W2 = x,
    TEACHINGS_15_W2 = x,
    TELL_BELIEFS_W2 = case_when(x == 1 ~ "1. Agree", x == 2 ~ "2. Disagree", x == 3 ~ "3. Not
relevant", x == 4 ~ "4. Unsure", .default = "(Missing)"),
    THREAT_LIFE_W2 = case_when(x == 1 ~ "1. A lot", x == 2 ~ "2. Some", x == 3 ~ "3. Not very much",
      x == 4 ~ "4. Not at all",
      .default = "(Missing)"
    ),
    TRAITS1_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS2_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS3_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS4_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS5_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS6_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS7_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS8_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS9_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRAITS10_W2 = case_when(x == 1 ~ "1. Agree strongly", x == 2 ~ "2. Agree moderately", x == 3
    ~ "3. Agree a little", x == 4 ~ "4. Neither agree nor disagree", x == 5 ~ "5.
Disagree a little", x == 6 ~ "6. Disagree moderately", x == 7 ~ "7. Disagree
strongly", .default = "(Missing)"),
    TRUST_PEOPLE_W2 = case_when(x == 1 ~ "1. All", x == 2 ~ "2. Most", x == 3 ~ "3. Some", x == 4 ~
      "4. Not very many", x == 5 ~ "5. None", .default = "(Missing)"),
    URBAN_RURAL_W2 = case_when(x == 1 ~ "1. A rural area or on a farm", x == 2 ~ "2. A small town
or village", x == 3 ~ "3. A large city", x == 4 ~ "4. A suburb of a large
city", .default = "(Missing)"),
    VOLUNTEERED_W2 = case_when(x == 1 ~ "1. Yes", x == 2 ~ "2. No", .default = "(Missing)"),
    WB_FIVEYRS_W2 = x,
    WB_TODAY_W2 = x,
    WORRY_SAFETY_W2 = x,
    WORTHWHILE_W2 = x,
    ANNUAL_WEIGHT1_W2 = x,
    STRATA_W2 = x,
    PSU_W2 = x,
    FULL_PARTIAL_W2 = x,
    AGE_GRP_W1 = case_when(
      x >= 18 & x < 25 ~ "18-24",
      x >= 25 & x < 30 ~ "25-29",
      x >= 30 & x < 40 ~ "30-39",
      x >= 40 & x < 50 ~ "40-49",
      x >= 50 & x < 60 ~ "50-59",
      x >= 60 & x < 70 ~ "60-69",
      x >= 70 & x < 80 ~ "70-79",
      x >= 80 & x < 100 ~ "80 or older",
      .default = "(Missing)"
    ),
    AGE_GRP_W2 = case_when(
      x >= 18 & x < 25 ~ "18-24",
      x >= 25 & x < 30 ~ "25-29",
      x >= 30 & x < 40 ~ "30-39",
      x >= 40 & x < 50 ~ "40-49",
      x >= 50 & x < 60 ~ "50-59",
      x >= 60 & x < 70 ~ "60-69",
      x >= 70 & x < 80 ~ "70-79",
      x >= 80 & x < 100 ~ "80 or older",
      .default = "(Missing)"
    ),
  )
}

#' Recode Variable Based on Levels
#'
#' A relatively simple switch function to transform variables by either reordering levels (reverse-
#' coding or otherwise).
#'
#' @param x a vector
#' @param var a character string (e.g., 'HAPPY_W1')
#' @returns a vector
#' @examples {
#'   # to-do
#' }
#' @export
reorder_levels <- function(x, var) {
  switch(var,
    ID = x,
    COUNTRY_W1 = fct_rev(x),
    WAVE_W1 = x,
    MODE_RECRUIT_W1 = fct_rev(x),
    MODE_ANNUAL_W1 = fct_rev(x),
    RECRUIT_TYPE_W1 = fct_rev(x),
    DOI_RECRUIT_W1 = x,
    DOI_ANNUAL_W1 = x,
    ABUSED_W1 = fct_rev(x),
    AFTER_DEATH_W1 = fct_rev(x),
    AGE_W1 = x,
    APPROVE_GOVT_W1 = fct_rev(x),
    ATTEND_SVCS_W1 = fct_rev(x),
    BELIEVE_GOD_W1 = fct_rev(x),
    BELONGING_W1 = x,
    BODILY_PAIN_W1 = fct_rev(x),
    BORN_COUNTRY_W1 = fct_rev(x),
    CAPABLE_W1 = fct_rev(x),
    CIGARETTES_W1 = x,
    CLOSE_TO_W1 = fct_rev(x),
    CNTRY_REL_BUD_W1 = x,
    CNTRY_REL_CHI_W1 = x,
    CNTRY_REL_CHR_W1 = x,
    CNTRY_REL_HIN_W1 = x,
    CNTRY_REL_ISL_W1 = x,
    CNTRY_REL_JUD_W1 = x,
    CNTRY_REL_SHI_W1 = x,
    COMFORT_REL_W1 = fct_rev(x),
    CONNECTED_REL_W1 = fct_rev(x),
    CONTENT_W1 = x,
    CONTROL_WORRY_W1 = fct_rev(x),
    COVID_DEATH_W1 = fct_rev(x),
    CRITICAL_W1 = fct_rev(x),
    DAYS_EXERCISE_W1 = fct_rev(x),
    DEPRESSED_W1 = fct_rev(x),
    DISCRIMINATED_W1 = fct_rev(x),
    DONATED_W1 = fct_rev(x),
    DRINKS_W1 = x,
    EDUCATION_W1 = x,
    EDUCATION_3_W1 = fct_rev(x),
    EMPLOYMENT_W1 = fct_rev(x),
    EXPECT_GOOD_W1 = x,
    EXPENSES_W1 = x,
    FATHER_LOVED_W1 = fct_rev(x),
    FATHER_RELATN_W1 = fct_rev(x),
    FEEL_ANXIOUS_W1 = fct_rev(x),
    FORGIVE_W1 = fct_rev(x),
    FREEDOM_W1 = x,
    GENDER_W1 = fct_rev(x),
    GIVE_UP_W1 = x,
    GOD_PUNISH_W1 = fct_rev(x),
    GRATEFUL_W1 = x,
    GROUP_NOT_REL_W1 = fct_rev(x),
    HAPPY_W1 = x,
    HEALTH_GROWUP_W1 = fct_rev(x),
    HEALTH_PROB_W1 = fct_rev(x),
    HELP_STRANGER_W1 = fct_rev(x),
    HOPE_FUTURE_W1 = x,
    INCOME_W1 = x,
    INCOME_12YRS_W1 = fct_rev(x),
    INCOME_DIFF_W1 = fct_rev(x),
    INCOME_FEELINGS_W1 = fct_rev(x),
    INTEREST_W1 = fct_rev(x),
    LIFE_APPROACH_W1 = fct_rev(x),
    LIFE_BALANCE_W1 = fct_rev(x),
    LIFE_PURPOSE_W1 = x,
    LIFE_SAT_W1 = x,
    LONELY_W1 = x,
    LOVED_BY_GOD_W1 = fct_rev(x),
    MARITAL_STATUS_W1 = fct_rev(x),
    MENTAL_HEALTH_W1 = x,
    MOTHER_LOVED_W1 = fct_rev(x),
    MOTHER_RELATN_W1 = fct_rev(x),
    NUM_CHILDREN_W1 = x,
    NUM_HOUSEHOLD_W1 = x,
    OBEY_LAW_W1 = fct_rev(x),
    OUTSIDER_W1 = fct_rev(x),
    OWN_RENT_HOME_W1 = fct_rev(x),
    PARENTS_12YRS_W1 = fct_rev(x),
    PEACE_W1 = fct_rev(x),
    PEOPLE_HELP_W1 = x,
    PHYSICAL_HLTH_W1 = x,
    POLITICAL_ID_W1 = x,
    PRAY_MEDITATE_W1 = fct_rev(x),
    PROMOTE_GOOD_W1 = x,
    REGION1_W1 = x,
    REGION2_W1 = x,
    REGION3_W1 = x,
    REL_EXPERIENC_W1 = fct_rev(x),
    REL_IMPORTANT_W1 = fct_rev(x),
    REL1_W1 = fct_rev(x),
    REL2_W1 = fct_rev(x),
    REL3_W1 = fct_rev(x),
    REL4_W1 = fct_rev(x),
    REL5_W1 = fct_rev(x),
    REL6_W1 = fct_rev(x),
    REL7_W1 = fct_rev(x),
    REL8_W1 = fct_rev(x),
    REL9_W1 = fct_rev(x),
    SACRED_TEXTS_W1 = fct_rev(x),
    SAT_LIVE_W1 = fct_rev(x),
    SAT_RELATNSHP_W1 = x,
    SAY_IN_GOVT_W1 = fct_rev(x),
    SELFID1_W1 = x,
    SELFID2_W1 = x,
    SHOW_LOVE_W1 = x,
    SUFFERING_W1 = fct_rev(x),
    SVCS_12YRS_W1 = fct_rev(x),
    SVCS_FATHER_W1 = fct_rev(x),
    SVCS_MOTHER_W1 = fct_rev(x),
    TEACHINGS_1_W1 = x,
    TEACHINGS_2_W1 = x,
    TEACHINGS_3_W1 = x,
    TEACHINGS_4_W1 = x,
    TEACHINGS_5_W1 = x,
    TEACHINGS_6_W1 = x,
    TEACHINGS_7_W1 = x,
    TEACHINGS_8_W1 = x,
    TEACHINGS_9_W1 = x,
    TEACHINGS_10_W1 = x,
    TEACHINGS_11_W1 = x,
    TEACHINGS_12_W1 = x,
    TEACHINGS_13_W1 = x,
    TEACHINGS_14_W1 = x,
    TEACHINGS_15_W1 = x,
    TELL_BELIEFS_W1 = fct_rev(x),
    THREAT_LIFE_W1 = fct_rev(x),
    TRAITS1_W1 = fct_rev(x),
    TRAITS2_W1 = fct_rev(x),
    TRAITS3_W1 = fct_rev(x),
    TRAITS4_W1 = fct_rev(x),
    TRAITS5_W1 = fct_rev(x),
    TRAITS6_W1 = fct_rev(x),
    TRAITS7_W1 = fct_rev(x),
    TRAITS8_W1 = fct_rev(x),
    TRAITS9_W1 = fct_rev(x),
    TRAITS10_W1 = fct_rev(x),
    TRUST_PEOPLE_W1 = fct_rev(x),
    URBAN_RURAL_W1 = fct_rev(x),
    VOLUNTEERED_W1 = fct_rev(x),
    WB_FIVEYRS_W1 = x,
    WB_TODAY_W1 = x,
    WORRY_SAFETY_W1 = x,
    WORTHWHILE_W1 = x,
    ANNUAL_WEIGHT1_W1 = x,
    STRATA_W1 = x,
    PSU_W1 = x,
    FULL_PARTIAL_W1 = x,
    COUNTRY_W2 = fct_rev(x),
    WAVE_W2 = x,
    MODE_RECRUIT_W2 = fct_rev(x),
    MODE_ANNUAL_W2 = fct_rev(x),
    RECRUIT_TYPE_W2 = fct_rev(x),
    DOI_RECRUIT_W2 = x,
    DOI_ANNUAL_W2 = x,
    ABUSED_W2 = fct_rev(x),
    AFTER_DEATH_W2 = fct_rev(x),
    AGE_W2 = x,
    APPROVE_GOVT_W2 = fct_rev(x),
    ATTEND_SVCS_W2 = fct_rev(x),
    BELIEVE_GOD_W2 = fct_rev(x),
    BELONGING_W2 = x,
    BODILY_PAIN_W2 = fct_rev(x),
    BORN_COUNTRY_W2 = fct_rev(x),
    CAPABLE_W2 = fct_rev(x),
    CIGARETTES_W2 = x,
    CLOSE_TO_W2 = fct_rev(x),
    CNTRY_REL_BUD_W2 = x,
    CNTRY_REL_CHI_W2 = x,
    CNTRY_REL_CHR_W2 = x,
    CNTRY_REL_HIN_W2 = x,
    CNTRY_REL_ISL_W2 = x,
    CNTRY_REL_JUD_W2 = x,
    CNTRY_REL_SHI_W2 = x,
    COMFORT_REL_W2 = fct_rev(x),
    CONNECTED_REL_W2 = fct_rev(x),
    CONTENT_W2 = x,
    CONTROL_WORRY_W2 = fct_rev(x),
    COVID_DEATH_W2 = fct_rev(x),
    CRITICAL_W2 = fct_rev(x),
    DAYS_EXERCISE_W2 = fct_rev(x),
    DEPRESSED_W2 = fct_rev(x),
    DISCRIMINATED_W2 = fct_rev(x),
    DONATED_W2 = fct_rev(x),
    DRINKS_W2 = x,
    EDUCATION_W2 = x,
    EDUCATION_3_W2 = fct_rev(x),
    EMPLOYMENT_W2 = fct_rev(x),
    EXPECT_GOOD_W2 = x,
    EXPENSES_W2 = x,
    FATHER_LOVED_W2 = fct_rev(x),
    FATHER_RELATN_W2 = fct_rev(x),
    FEEL_ANXIOUS_W2 = fct_rev(x),
    FORGIVE_W2 = fct_rev(x),
    FREEDOM_W2 = x,
    GENDER_W2 = fct_rev(x),
    GIVE_UP_W2 = x,
    GOD_PUNISH_W2 = fct_rev(x),
    GRATEFUL_W2 = x,
    GROUP_NOT_REL_W2 = fct_rev(x),
    HAPPY_W2 = x,
    HEALTH_GROWUP_W2 = fct_rev(x),
    HEALTH_PROB_W2 = fct_rev(x),
    HELP_STRANGER_W2 = fct_rev(x),
    HOPE_FUTURE_W2 = x,
    INCOME_W2 = x,
    INCOME_12YRS_W2 = fct_rev(x),
    INCOME_DIFF_W2 = fct_rev(x),
    INCOME_FEELINGS_W2 = fct_rev(x),
    INTEREST_W2 = fct_rev(x),
    LIFE_APPROACH_W2 = fct_rev(x),
    LIFE_BALANCE_W2 = fct_rev(x),
    LIFE_PURPOSE_W2 = x,
    LIFE_SAT_W2 = x,
    LONELY_W2 = x,
    LOVED_BY_GOD_W2 = fct_rev(x),
    MARITAL_STATUS_W2 = fct_rev(x),
    MENTAL_HEALTH_W2 = x,
    MOTHER_LOVED_W2 = fct_rev(x),
    MOTHER_RELATN_W2 = fct_rev(x),
    NUM_CHILDREN_W2 = x,
    NUM_HOUSEHOLD_W2 = x,
    OBEY_LAW_W2 = fct_rev(x),
    OUTSIDER_W2 = fct_rev(x),
    OWN_RENT_HOME_W2 = fct_rev(x),
    PARENTS_12YRS_W2 = fct_rev(x),
    PEACE_W2 = fct_rev(x),
    PEOPLE_HELP_W2 = x,
    PHYSICAL_HLTH_W2 = x,
    POLITICAL_ID_W2 = x,
    PRAY_MEDITATE_W2 = fct_rev(x),
    PROMOTE_GOOD_W2 = x,
    REGION1_W2 = x,
    REGION2_W2 = x,
    REGION3_W2 = x,
    REL_EXPERIENC_W2 = fct_rev(x),
    REL_IMPORTANT_W2 = fct_rev(x),
    REL1_W2 = fct_rev(x),
    REL2_W2 = fct_rev(x),
    REL3_W2 = fct_rev(x),
    REL4_W2 = fct_rev(x),
    REL5_W2 = fct_rev(x),
    REL6_W2 = fct_rev(x),
    REL7_W2 = fct_rev(x),
    REL8_W2 = fct_rev(x),
    REL9_W2 = fct_rev(x),
    SACRED_TEXTS_W2 = fct_rev(x),
    SAT_LIVE_W2 = fct_rev(x),
    SAT_RELATNSHP_W2 = x,
    SAY_IN_GOVT_W2 = fct_rev(x),
    SELFID1_W2 = x,
    SELFID2_W2 = x,
    SHOW_LOVE_W2 = x,
    SUFFERING_W2 = fct_rev(x),
    SVCS_12YRS_W2 = fct_rev(x),
    SVCS_FATHER_W2 = fct_rev(x),
    SVCS_MOTHER_W2 = fct_rev(x),
    TEACHINGS_1_W2 = x,
    TEACHINGS_2_W2 = x,
    TEACHINGS_3_W2 = x,
    TEACHINGS_4_W2 = x,
    TEACHINGS_5_W2 = x,
    TEACHINGS_6_W2 = x,
    TEACHINGS_7_W2 = x,
    TEACHINGS_8_W2 = x,
    TEACHINGS_9_W2 = x,
    TEACHINGS_10_W2 = x,
    TEACHINGS_11_W2 = x,
    TEACHINGS_12_W2 = x,
    TEACHINGS_13_W2 = x,
    TEACHINGS_14_W2 = x,
    TEACHINGS_15_W2 = x,
    TELL_BELIEFS_W2 = fct_rev(x),
    THREAT_LIFE_W2 = fct_rev(x),
    TRAITS1_W2 = fct_rev(x),
    TRAITS2_W2 = fct_rev(x),
    TRAITS3_W2 = fct_rev(x),
    TRAITS4_W2 = fct_rev(x),
    TRAITS5_W2 = fct_rev(x),
    TRAITS6_W2 = fct_rev(x),
    TRAITS7_W2 = fct_rev(x),
    TRAITS8_W2 = fct_rev(x),
    TRAITS9_W2 = fct_rev(x),
    TRAITS10_W2 = fct_rev(x),
    TRUST_PEOPLE_W2 = fct_rev(x),
    URBAN_RURAL_W2 = fct_rev(x),
    VOLUNTEERED_W2 = fct_rev(x),
    WB_FIVEYRS_W2 = x,
    WB_TODAY_W2 = x,
    WORRY_SAFETY_W2 = x,
    WORTHWHILE_W2 = x,
    ANNUAL_WEIGHT1_W2 = x,
    STRATA_W2 = x,
    PSU_W2 = x,
    FULL_PARTIAL_W2 = x,
    AGE_GRP_W1 = x,
    AGE_GRP_W2 = x,
  )
}

#' Recode Variable to Numeric
#'
#' A more complex switch function that is highly dependent on the variable and needs to be reviewed
#' for accuracy.
#'
#' @param x a vector
#' @param var a character string (e.g., 'HAPPY_W1')
#' @param is.sum (default FALSE) a logical determining whether to change how a variable is treated.
#' Currently only applies to certain composite variables.
#' @returns a vector
#' @examples {
#'   # to-do
#' }
#' @export
recode_to_numeric <- function(x, var, is.sum = FALSE) {
  switch(var,
    ID = as.numeric(x),
    COUNTRY_W1 = as.numeric(x),
    WAVE_W1 = as.numeric(x),
    MODE_RECRUIT_W1 = as.numeric(x),
    MODE_ANNUAL_W1 = as.numeric(x),
    RECRUIT_TYPE_W1 = as.numeric(x),
    DOI_RECRUIT_W1 = as.numeric(x),
    DOI_ANNUAL_W1 = as.numeric(x),
    ABUSED_W1 = as.numeric(x),
    AFTER_DEATH_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3) ~ 0),
    AGE_W1 = as.numeric(x),
    APPROVE_GOVT_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    ATTEND_SVCS_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    BELIEVE_GOD_W1 = case_when(x %in% c(1, 2, 3) ~ 1, x %in% c(4, 5) ~ 0),
    BELONGING_W1 = as.numeric(x),
    BODILY_PAIN_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    BORN_COUNTRY_W1 = as.numeric(x),
    CAPABLE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    CIGARETTES_W1 = as.numeric(x),
    CLOSE_TO_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    CNTRY_REL_BUD_W1 = as.numeric(x),
    CNTRY_REL_CHI_W1 = as.numeric(x),
    CNTRY_REL_CHR_W1 = as.numeric(x),
    CNTRY_REL_HIN_W1 = as.numeric(x),
    CNTRY_REL_ISL_W1 = as.numeric(x),
    CNTRY_REL_JUD_W1 = as.numeric(x),
    CNTRY_REL_SHI_W1 = as.numeric(x),
    COMFORT_REL_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    CONNECTED_REL_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    CONTENT_W1 = as.numeric(x),
    CONTROL_WORRY_W1 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    COVID_DEATH_W1 = as.numeric(x),
    CRITICAL_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    DAYS_EXERCISE_W1 = as.numeric(x),
    DEPRESSED_W1 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    DISCRIMINATED_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    DONATED_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    DRINKS_W1 = as.numeric(x),
    EDUCATION_W1 = as.numeric(x),
    EDUCATION_3_W1 = case_when(x %in% c(3) ~ 1, x %in% c(1, 2) ~ 0),
    EMPLOYMENT_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5, 6, 7) ~ 0),
    EXPECT_GOOD_W1 = as.numeric(x),
    EXPENSES_W1 = as.numeric(x),
    FATHER_LOVED_W1 = as.numeric(x),
    FATHER_RELATN_W1 = as.numeric(x),
    FEEL_ANXIOUS_W1 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    FORGIVE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    FREEDOM_W1 = as.numeric(x),
    GENDER_W1 = as.numeric(x),
    GIVE_UP_W1 = as.numeric(x),
    GOD_PUNISH_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    GRATEFUL_W1 = as.numeric(x),
    GROUP_NOT_REL_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    HAPPY_W1 = as.numeric(x),
    HEALTH_GROWUP_W1 = as.numeric(x),
    HEALTH_PROB_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    HELP_STRANGER_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    HOPE_FUTURE_W1 = as.numeric(x),
    INCOME_W1 = as.numeric(x),
    INCOME_12YRS_W1 = as.numeric(x),
    INCOME_DIFF_W1 = as.numeric(x),
    INCOME_FEELINGS_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    INTEREST_W1 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    LIFE_APPROACH_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2:4) ~ 0),
    LIFE_BALANCE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    LIFE_PURPOSE_W1 = as.numeric(x),
    LIFE_SAT_W1 = as.numeric(x),
    LONELY_W1 = as.numeric(x),
    LOVED_BY_GOD_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    MARITAL_STATUS_W1 = case_when(x %in% c(2) ~ 1, x %in% c(1, 3, 4, 5, 6) ~ 0),
    MENTAL_HEALTH_W1 = as.numeric(x),
    MOTHER_LOVED_W1 = as.numeric(x),
    MOTHER_RELATN_W1 = as.numeric(x),
    NUM_CHILDREN_W1 = as.numeric(x),
    NUM_HOUSEHOLD_W1 = as.numeric(x),
    OBEY_LAW_W1 = as.numeric(x),
    OUTSIDER_W1 = as.numeric(x),
    OWN_RENT_HOME_W1 = case_when(x %in% c(1, 3, 6) ~ 1, x %in% c(2, 4, 5, 7) ~ 0),
    PARENTS_12YRS_W1 = as.numeric(x),
    PEACE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    PEOPLE_HELP_W1 = as.numeric(x),
    PHYSICAL_HLTH_W1 = as.numeric(x),
    POLITICAL_ID_W1 = as.numeric(x),
    PRAY_MEDITATE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    PROMOTE_GOOD_W1 = as.numeric(x),
    REGION1_W1 = as.numeric(x),
    REGION2_W1 = as.numeric(x),
    REGION3_W1 = as.numeric(x),
    REL_EXPERIENC_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    REL_IMPORTANT_W1 = as.numeric(x),
    REL1_W1 = as.numeric(x),
    REL2_W1 = as.numeric(x),
    REL3_W1 = as.numeric(x),
    REL4_W1 = as.numeric(x),
    REL5_W1 = as.numeric(x),
    REL6_W1 = as.numeric(x),
    REL7_W1 = as.numeric(x),
    REL8_W1 = as.numeric(x),
    REL9_W1 = as.numeric(x),
    SACRED_TEXTS_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    SAT_LIVE_W1 = as.numeric(x),
    SAT_RELATNSHP_W1 = as.numeric(x),
    SAY_IN_GOVT_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3) ~ 0),
    SELFID1_W1 = as.numeric(x),
    SELFID2_W1 = as.numeric(x),
    SHOW_LOVE_W1 = as.numeric(x),
    SUFFERING_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    SVCS_12YRS_W1 = as.numeric(x),
    SVCS_FATHER_W1 = as.numeric(x),
    SVCS_MOTHER_W1 = as.numeric(x),
    TEACHINGS_1_W1 = as.numeric(x),
    TEACHINGS_2_W1 = as.numeric(x),
    TEACHINGS_3_W1 = as.numeric(x),
    TEACHINGS_4_W1 = as.numeric(x),
    TEACHINGS_5_W1 = as.numeric(x),
    TEACHINGS_6_W1 = as.numeric(x),
    TEACHINGS_7_W1 = as.numeric(x),
    TEACHINGS_8_W1 = as.numeric(x),
    TEACHINGS_9_W1 = as.numeric(x),
    TEACHINGS_10_W1 = as.numeric(x),
    TEACHINGS_11_W1 = as.numeric(x),
    TEACHINGS_12_W1 = as.numeric(x),
    TEACHINGS_13_W1 = as.numeric(x),
    TEACHINGS_14_W1 = as.numeric(x),
    TEACHINGS_15_W1 = as.numeric(x),
    TELL_BELIEFS_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    THREAT_LIFE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    TRAITS1_W1 = as.numeric(x),
    TRAITS2_W1 = as.numeric(x),
    TRAITS3_W1 = as.numeric(x),
    TRAITS4_W1 = as.numeric(x),
    TRAITS5_W1 = as.numeric(x),
    TRAITS6_W1 = as.numeric(x),
    TRAITS7_W1 = as.numeric(x),
    TRAITS8_W1 = as.numeric(x),
    TRAITS9_W1 = as.numeric(x),
    TRAITS10_W1 = as.numeric(x),
    TRUST_PEOPLE_W1 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    URBAN_RURAL_W1 = as.numeric(x),
    VOLUNTEERED_W1 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    WB_FIVEYRS_W1 = as.numeric(x),
    WB_TODAY_W1 = as.numeric(x),
    WORRY_SAFETY_W1 = as.numeric(x),
    WORTHWHILE_W1 = as.numeric(x),
    ANNUAL_WEIGHT1_W1 = as.numeric(x),
    STRATA_W1 = as.numeric(x),
    PSU_W1 = as.numeric(x),
    FULL_PARTIAL_W1 = as.numeric(x),
    COUNTRY_W2 = as.numeric(x),
    WAVE_W2 = as.numeric(x),
    MODE_RECRUIT_W2 = as.numeric(x),
    MODE_ANNUAL_W2 = as.numeric(x),
    RECRUIT_TYPE_W2 = as.numeric(x),
    DOI_RECRUIT_W2 = as.numeric(x),
    DOI_ANNUAL_W2 = as.numeric(x),
    ABUSED_W2 = as.numeric(x),
    AFTER_DEATH_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3) ~ 0),
    AGE_W2 = as.numeric(x),
    APPROVE_GOVT_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    ATTEND_SVCS_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    BELIEVE_GOD_W2 = case_when(x %in% c(1, 2, 3) ~ 1, x %in% c(4, 5) ~ 0),
    BELONGING_W2 = as.numeric(x),
    BODILY_PAIN_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    BORN_COUNTRY_W2 = as.numeric(x),
    CAPABLE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    CIGARETTES_W2 = as.numeric(x),
    CLOSE_TO_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    CNTRY_REL_BUD_W2 = as.numeric(x),
    CNTRY_REL_CHI_W2 = as.numeric(x),
    CNTRY_REL_CHR_W2 = as.numeric(x),
    CNTRY_REL_HIN_W2 = as.numeric(x),
    CNTRY_REL_ISL_W2 = as.numeric(x),
    CNTRY_REL_JUD_W2 = as.numeric(x),
    CNTRY_REL_SHI_W2 = as.numeric(x),
    COMFORT_REL_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    CONNECTED_REL_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    CONTENT_W2 = as.numeric(x),
    CONTROL_WORRY_W2 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    COVID_DEATH_W2 = as.numeric(x),
    CRITICAL_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    DAYS_EXERCISE_W2 = as.numeric(x),
    DEPRESSED_W2 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    DISCRIMINATED_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    DONATED_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    DRINKS_W2 = as.numeric(x),
    EDUCATION_W2 = as.numeric(x),
    EDUCATION_3_W2 = case_when(x %in% c(3) ~ 1, x %in% c(1, 2) ~ 0),
    EMPLOYMENT_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5, 6, 7) ~ 0),
    EXPECT_GOOD_W2 = as.numeric(x),
    EXPENSES_W2 = as.numeric(x),
    FATHER_LOVED_W2 = as.numeric(x),
    FATHER_RELATN_W2 = as.numeric(x),
    FEEL_ANXIOUS_W2 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    FORGIVE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    FREEDOM_W2 = as.numeric(x),
    GENDER_W2 = as.numeric(x),
    GIVE_UP_W2 = as.numeric(x),
    GOD_PUNISH_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    GRATEFUL_W2 = as.numeric(x),
    GROUP_NOT_REL_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    HAPPY_W2 = as.numeric(x),
    HEALTH_GROWUP_W2 = as.numeric(x),
    HEALTH_PROB_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    HELP_STRANGER_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    HOPE_FUTURE_W2 = as.numeric(x),
    INCOME_W2 = as.numeric(x),
    INCOME_12YRS_W2 = as.numeric(x),
    INCOME_DIFF_W2 = as.numeric(x),
    INCOME_FEELINGS_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    INTEREST_W2 = case_when(
      !is.sum & x %in% c(1, 2) ~ 1,
      !is.sum & x %in% c(3, 4) ~ 0,
      is.sum ~ as.numeric(x) - 1
    ),
    LIFE_APPROACH_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    LIFE_BALANCE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    LIFE_PURPOSE_W2 = as.numeric(x),
    LIFE_SAT_W2 = as.numeric(x),
    LONELY_W2 = as.numeric(x),
    LOVED_BY_GOD_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    MARITAL_STATUS_W2 = case_when(x %in% c(2) ~ 1, x %in% c(1, 3, 4, 5, 6) ~ 0),
    MENTAL_HEALTH_W2 = as.numeric(x),
    MOTHER_LOVED_W2 = as.numeric(x),
    MOTHER_RELATN_W2 = as.numeric(x),
    NUM_CHILDREN_W2 = as.numeric(x),
    NUM_HOUSEHOLD_W2 = as.numeric(x),
    OBEY_LAW_W2 = as.numeric(x),
    OUTSIDER_W2 = as.numeric(x),
    OWN_RENT_HOME_W2 = case_when(x %in% c(1, 3, 6) ~ 1, x %in% c(2, 4, 5, 7) ~ 0),
    PARENTS_12YRS_W2 = as.numeric(x),
    PEACE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    PEOPLE_HELP_W2 = as.numeric(x),
    PHYSICAL_HLTH_W2 = as.numeric(x),
    POLITICAL_ID_W2 = as.numeric(x),
    PRAY_MEDITATE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    PROMOTE_GOOD_W2 = as.numeric(x),
    REGION1_W2 = as.numeric(x),
    REGION2_W2 = as.numeric(x),
    REGION3_W2 = as.numeric(x),
    REL_EXPERIENC_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    REL_IMPORTANT_W2 = as.numeric(x),
    REL1_W2 = as.numeric(x),
    REL2_W2 = as.numeric(x),
    REL3_W2 = as.numeric(x),
    REL4_W2 = as.numeric(x),
    REL5_W2 = as.numeric(x),
    REL6_W2 = as.numeric(x),
    REL7_W2 = as.numeric(x),
    REL8_W2 = as.numeric(x),
    REL9_W2 = as.numeric(x),
    SACRED_TEXTS_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    SAT_LIVE_W2 = as.numeric(x),
    SAT_RELATNSHP_W2 = as.numeric(x),
    SAY_IN_GOVT_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3) ~ 0),
    SELFID1_W2 = as.numeric(x),
    SELFID2_W2 = as.numeric(x),
    SHOW_LOVE_W2 = as.numeric(x),
    SUFFERING_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    SVCS_12YRS_W2 = as.numeric(x),
    SVCS_FATHER_W2 = as.numeric(x),
    SVCS_MOTHER_W2 = as.numeric(x),
    TEACHINGS_1_W2 = as.numeric(x),
    TEACHINGS_2_W2 = as.numeric(x),
    TEACHINGS_3_W2 = as.numeric(x),
    TEACHINGS_4_W2 = as.numeric(x),
    TEACHINGS_5_W2 = as.numeric(x),
    TEACHINGS_6_W2 = as.numeric(x),
    TEACHINGS_7_W2 = as.numeric(x),
    TEACHINGS_8_W2 = as.numeric(x),
    TEACHINGS_9_W2 = as.numeric(x),
    TEACHINGS_10_W2 = as.numeric(x),
    TEACHINGS_11_W2 = as.numeric(x),
    TEACHINGS_12_W2 = as.numeric(x),
    TEACHINGS_13_W2 = as.numeric(x),
    TEACHINGS_14_W2 = as.numeric(x),
    TEACHINGS_15_W2 = as.numeric(x),
    TELL_BELIEFS_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2, 3, 4) ~ 0),
    THREAT_LIFE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4) ~ 0),
    TRAITS1_W2 = as.numeric(x),
    TRAITS2_W2 = as.numeric(x),
    TRAITS3_W2 = as.numeric(x),
    TRAITS4_W2 = as.numeric(x),
    TRAITS5_W2 = as.numeric(x),
    TRAITS6_W2 = as.numeric(x),
    TRAITS7_W2 = as.numeric(x),
    TRAITS8_W2 = as.numeric(x),
    TRAITS9_W2 = as.numeric(x),
    TRAITS10_W2 = as.numeric(x),
    TRUST_PEOPLE_W2 = case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 5) ~ 0),
    URBAN_RURAL_W2 = as.numeric(x),
    VOLUNTEERED_W2 = case_when(x %in% c(1) ~ 1, x %in% c(2) ~ 0),
    WB_FIVEYRS_W2 = as.numeric(x),
    WB_TODAY_W2 = as.numeric(x),
    WORRY_SAFETY_W2 = as.numeric(x),
    WORTHWHILE_W2 = as.numeric(x),
    ANNUAL_WEIGHT1_W2 = as.numeric(x),
    STRATA_W2 = as.numeric(x),
    PSU_W2 = as.numeric(x),
    FULL_PARTIAL_W2 = as.numeric(x),
    AGE_GRP_W1 = as.numeric(x),
    AGE_GRP_W2 = as.numeric(x),
    COMPOSITE_DEPRESSION_W1 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_ANXIETY_W1 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_DEP_ANX_COMBO_W1 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_EXTRAVERSION_W1 = as.numeric(x),
    COMPOSITE_OPENNESS_W1 = as.numeric(x),
    COMPOSITE_AGREEABLENESS_W1 = as.numeric(x),
    COMPOSITE_CONSCIENTIOUSNESS_W1 = as.numeric(x),
    COMPOSITE_NEUROTICISM_W1 = as.numeric(x),
    COMPOSITE_FLOURISHING_W1 = as.numeric(x),
    COMPOSITE_FLOURISHING_SECURE_W1 = as.numeric(x),
    COMPOSITE_HAPPI_LIFE_SAT_W1 = as.numeric(x),
    COMPOSITE_HEALTH_W1 = as.numeric(x),
    COMPOSITE_MEANING_PURPOSE_W1 = as.numeric(x),
    COMPOSITE_CHARACTER_W1 = as.numeric(x),
    COMPOSITE_SUBJECTIVE_SOC_CONN_W1 = as.numeric(x),
    COMPOSITE_FINL_MAT_WORRY_W1 = as.numeric(x),
    COMPOSITE_DEPRESSION_W2 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_ANXIETY_W2 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_DEP_ANX_COMBO_W2 = case_when(x >= 3 ~ 1, x < 3 ~ 0),
    COMPOSITE_EXTRAVERSION_W2 = as.numeric(x),
    COMPOSITE_OPENNESS_W2 = as.numeric(x),
    COMPOSITE_AGREEABLENESS_W2 = as.numeric(x),
    COMPOSITE_CONSCIENTIOUSNESS_W2 = as.numeric(x),
    COMPOSITE_NEUROTICISM_W2 = as.numeric(x),
    COMPOSITE_FLOURISHING_W2 = as.numeric(x),
    COMPOSITE_FLOURISHING_SECURE_W2 = as.numeric(x),
    COMPOSITE_HAPPI_LIFE_SAT_W2 = as.numeric(x),
    COMPOSITE_HEALTH_W2 = as.numeric(x),
    COMPOSITE_MEANING_PURPOSE_W2 = as.numeric(x),
    COMPOSITE_CHARACTER_W2 = as.numeric(x),
    COMPOSITE_SUBJECTIVE_SOC_CONN_W2 = as.numeric(x),
    COMPOSITE_FINL_MAT_WORRY_W2 = as.numeric(x),
  )
}
