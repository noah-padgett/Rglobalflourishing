# simple country-means

df.raw <- df.raw |>
  mutate(
    ANNUAL_WEIGHT_R3 = case_when(
      is.na(ANNUAL_WEIGHT_R3) ~ 0,
      .default = ANNUAL_WEIGHT_R3
    )
  )
df.svy <- svydesign(ids=~PSU, strata=~STRATA, weights=~ANNUAL_WEIGHT_R3, data=df.raw)

sum.tb <- df.svy |>
  tbl_svysummary(
    include = c(COMPOSITE_FLOURISHING_SECURE_Y1, COMPOSITE_FLOURISHING_SECURE_Y2, COMPOSITE_FLOURISHING_SECURE_Y3),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "(Missing)",
    missing_stat = "{N_miss} ({p_miss}%)"
  )

sum.tb


sum.tb.by <- df.svy |>
  tbl_svysummary(
    by = COUNTRY,
    include = c(COMPOSITE_FLOURISHING_SECURE_Y1, COMPOSITE_FLOURISHING_SECURE_Y2, COMPOSITE_FLOURISHING_SECURE_Y3),
    label = list(
      COMPOSITE_FLOURISHING_SECURE_Y1 ~ "Secure Flourishing Index (Wave 1)",
      COMPOSITE_FLOURISHING_SECURE_Y2 ~ "Secure Flourishing Index (Wave 2)",
      COMPOSITE_FLOURISHING_SECURE_Y3 ~ "Secure Flourishing Index (Wave 3)"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "(Missing)",
    missing_stat = "{N_miss} ({p_miss}%)"
  ) |>
  add_overall()
sum.tb.by

read_docx() |>
  body_add_flextable(as_flex_table(sum.tb.by))|>
  print(target = "wave3_prelim.docx")
