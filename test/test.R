load(here::here(data.dir, "gfs_w2_imputed_data_20imp.RData"))

#df.imp <- df.imp %>%
#  filter(COUNTRY == "Egypt")

#df.imp.long <- recode_imputed_data(
#  df.imp,
#  list.default = RECODE.DEFAULTS,
#  list.composites = LIST.COMPOSITES,
#  wgt = "ANNUAL_WEIGHT_R2"
#)

df.isr <- df.imp %>%
  filter(COUNTRY == "Israel") %>%


country_vec <- as.character(unique(df.raw$COUNTRY))

recode_imp_by_country(country_name = "Israel")

mem_res <- bench::mark(

  map(.x = country_vec,
      .f = ~recode_imp_by_country(country_name = .x))

)
