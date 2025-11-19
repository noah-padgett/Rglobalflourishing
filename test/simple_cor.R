# simple cor(.) complete-case
library(Rglobalflourishing) ## remotes::install_github("noah-padgett/Rglobalflourishing")
load_packages()
library(msm)
options(
  survey.lonely.psu = "certainty"
)

## Analysis - correlation -- complete-case analysis
data.dir <-  "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave1-data/"
dataset.name <- "gfs_all_countries_wave1.sav"
df_w1 <- haven::read_sav(here::here(data.dir, dataset.name))



# recode missing (-98,98,99)
df_w1 <- df_w1 %>%
  mutate(
    COUNTRY = case_when(
      COUNTRY == 1 ~ "Argentina",
      COUNTRY == 2 ~ "Australia",
      COUNTRY == 3 ~ "Brazil",
      COUNTRY == 4 ~ "Egypt",
      COUNTRY == 5 ~ "Germany",
      COUNTRY == 6 ~ "India",
      COUNTRY == 7 ~ "Indonesia",
      COUNTRY == 8 ~ "Israel",
      COUNTRY == 9 ~ "Japan",
      COUNTRY == 10 ~ "Kenya",
      COUNTRY == 11 ~ "Mexico",
      COUNTRY == 12 ~ "Nigeria",
      COUNTRY == 13 ~ "Philippines",
      COUNTRY == 14 ~ "Poland",
      COUNTRY == 16 ~ "South Africa",
      COUNTRY == 17 ~ "Spain",
      COUNTRY == 18 ~ "Tanzania",
      COUNTRY == 19 ~ "Turkey",
      COUNTRY == 20 ~ "United Kingdom",
      COUNTRY == 22 ~ "United States",
      COUNTRY == 23 ~ "Sweden",
      COUNTRY == 24 ~ "Hong Kong"
    ),
    # recode missing
    across(c(LIFE_SAT, HAPPY),\(x){
      case_when(
        x %in% c(-98,98,99) ~ NA, 
        .default = x
      )
    })
  )

## ============================================================================================== ##
## ============================================================================================== ##
## The following conducts the primary analyses by country


compute_cor_se <- function(x, y, mydesign, na.rm = FALSE){
  out <- data.frame(
    Variable_1 = x,
    Variable_2 = y, 
    est = NA,
    se = NA, 
    n = nrow(mydesign[['variables']])
  )
  ## check for missing
  if(!na.rm){
    if(anyNA(mydesign[['variables']][,c(x,y)])){
      warning("Missing data detected in 'x' or 'y' variable. Need to set 'na.rm=TRUE' to run correlation but analysis will omit all cases missing on either variable.")
    }
  }
  fit.var <- svyvar(reformulate(c(x,y)), design = mydesign, na.rm=na.rm)
  cor.est <- cov2cor(as.matrix(fit.var))[1,2]
  xcov <- attr(fit.var, which="var")
  xcov <- xcov[c(1,2,4), c(1,2,4)]
  cor.se <- msm::deltamethod(~ x2/(sqrt(x1)*sqrt(x3)), c(fit.var[1,1],fit.var[1,2],fit.var[2,2]), xcov, ses = TRUE)
  out$est[1] <- cor.est
  out$se[1] <- cor.se
  out
}

## design is done **BY COUNTRY**
svy.data <- svydesign(
  data = df_w1 %>% filter(COUNTRY == "United States"),
  id =  ~ PSU,
  strata =  ~ STRATA,
  weights = ~ ANNUAL_WEIGHT1
)
compute_cor_se(x="HAPPY", y="LIFE_SAT", mydesign = svy.data)
compute_cor_se("HAPPY", "LIFE_SAT", mydesign = svy.data, na.rm=TRUE)


## map around countries
all_cor_est <- map(unique(df_w1$COUNTRY), \(x){
  svy.data <- svydesign(
    data = df_w1 %>% filter(COUNTRY == x),
    id =  ~ PSU,
    strata =  ~ STRATA,
    weights = ~ ANNUAL_WEIGHT1
  )
  compute_cor_se("HAPPY", "LIFE_SAT", mydesign = svy.data, na.rm=TRUE) |>
    mutate(COUNTRY = x, .before = 1)
}) |>
  bind_rows()
