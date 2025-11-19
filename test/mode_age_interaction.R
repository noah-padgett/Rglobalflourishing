## I'm not sure what multical or weightit are doing without digging into themn more. We need to weight to the population counts not the sample counts.

library(Rglobalflourishing) ## remotes::install_github("noah-padgett/Rglobalflourishing")
load_packages()
options(
  survey.lonely.psu = "certainty"
)
## Analysis - correlation -- complete-case analysis
data.dir <-  "/Users/noahp/Documents/GitHub/global-flourishing-study/data/wave1-data/"
dataset.name <- "gfs_all_countries_wave1.sav"
df_w1 <- haven::read_sav(here::here(data.dir, dataset.name))

df_w1 <- df_w1 %>%
  mutate(
    COUNTRY2 = case_when(
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
    )
  )

df_w1_arg <- df_w1 |> filter(COUNTRY2 == "Argentina")

# create raking groups from raw data
df_w1_arg <- df_w1_arg |>
  mutate(
    GENDER = labelled::to_character(GENDER),
    GENDER = case_when(
      !(GENDER %in% c("Male", "Female")) ~ NA,
      .default = GENDER
    ),
    EMPLOYMENT_STATUS = labelled::to_character(EMPLOYMENT),
    EMPLOYMENT_STATUS = case_when(
      EMPLOYMENT_STATUS %in% c("Employed for an employer", "Self-employed") ~ "Employed",
      EMPLOYMENT_STATUS %in% c("Unemployed and looking for a job", "Student", "Retired", "Homemaker", "None of these/Other") ~ "Not Employed"
    ),
    EDU_CS = str_remove(labelled::to_character(EDUCATION), "Argentina:  "),
    EDU = case_when(
      EDU_CS %in% c("(No formal education)", "Incomplete primary school") ~ "No Education/Primary Incomplete",
      EDU_CS %in% c("Complete primary school") ~ "Primary Complete", 
      EDU_CS %in% c("Incomplete secondary school") ~ "Secondary Incomplete", 
      EDU_CS %in% c("Complete secondary school") ~ "Secondary Complete", 
      EDU_CS %in% c("Incomplete tertiary school", "Complete tertiary school") ~ "Some/Complete Tertiary (Technical)", 
      EDU_CS %in% c("Incomplete university", "Complete university", "Post-graduate") ~ "Some/Complete University",
      .default = EDU_CS
    ),
    EDUCATION_GROUP = EDU,
    REGION1 = str_remove(labelled::to_character(REGION1), "Argentina:  "),
    REGION2 = str_remove(labelled::to_character(REGION2), "Argentina:  "),
    REGION = case_when(
      REGION1 == "Buenos Aires" & REGION2 %in% c("Metropolitana", "Pampeana") ~ "Buenos Aires",
      REGION2 %in% c("Metropolitana", "Pampeana") ~ paste0(REGION2, " (excluding Buenos Aires)"),
      REGION2 == "N.O.A. (Noroeste Argentino)" ~ "N.O.A.(Noroeste Argentino)",
      .default = REGION2
    ),
    AGE_GRP = case_when(
      AGE >= 18 & AGE < 25 ~ "18-24",
      AGE >= 25 & AGE < 35 ~ "25-34",
      AGE >= 35 & AGE < 45 ~ "35-44",
      AGE >= 45 & AGE < 55 ~ "45-54",
      AGE >= 55 & AGE < 65 ~ "55-64",
      AGE >= 65 & AGE <= 100 ~ "65+"
    ),
    across(c(AGE_GRP, GENDER, REGION, EDUCATION_GROUP, EMPLOYMENT_STATUS), \(x){
      x[x %in% c("(Don't know)", "(Saw, skipped)")] <- NA
      x.tb = table(x)
      x.mode = which(x.tb == max(x.tb))
      y <- x
      y[is.na(y)] <- names(x.tb)[x.mode]
      y
    }),
    AGE_GENDER_REGION = paste0(REGION,", ", GENDER, ", ", AGE_GRP),
    
    ## flourishing index
    SFI = rowMeans(across(c(LIFE_SAT, HAPPY, PHYSICAL_HLTH, MENTAL_HEALTH, WORTHWHILE, LIFE_PURPOSE, PROMOTE_GOOD, GIVE_UP, CONTENT, SAT_RELATNSHP, EXPENSES, WORRY_SAFETY), \(x){
      y = x
      y[!(y %in% 0:10)] <- NA
      y
    }))
  )
table(df_w1_arg$AGE_GRP, useNA="always")
table(df_w1_arg$GENDER, useNA="always")
table(df_w1_arg$REGION, useNA="always")
table(df_w1_arg$EDUCATION_GROUP, useNA="always")
table(df_w1_arg$EMPLOYMENT_STATUS, useNA="always")

table(df_w1_arg$AGE_GENDER_REGION, useNA="always")

anyNA(df_w1_arg$SFI)
sum(is.na(df_w1_arg$SFI))/nrow(df_w1_arg) # proportion missing -- need to handle missing on the 2.7% of 
df_w1_arg <- df_w1_arg |>
  mutate(
    MISSING_SFI = ifelse(is.na(SFI), "Missing SFI", "Has SFI Estimate")
  )
table(df_w1_arg[,c("MODE_ANNUAL", "MISSING_SFI")])

## to have a 100% compelte-case analyses, now remove all cases with missing on SFI
df_w1_arg <- df_w1_arg |>
  filter(!is.na(SFI))

## raking:
df_targets_arg <- readxl::read_excel("targets-argentina.xlsx")
pop.age_gender_region <- df_targets_arg |> 
  filter(TARGET == "AGE_GENDER_REGION") |>
  select(GROUP, `Total Count`) |>
  rename(
    AGE_GENDER_REGION = "GROUP",
    Freq = "Total Count"
  )
pop.edu <- df_targets_arg |> 
  filter(TARGET == "EDUCATION_GROUP") |>
  select(GROUP, `Total Count`) |>
  rename(
    EDUCATION_GROUP = "GROUP",
    Freq = "Total Count"
  )

pop.employ <- df_targets_arg |> 
  filter(TARGET == "EMPLOYMENT_STATUS") |>
  select(GROUP, `Total Count`) |>
  rename(
    EMPLOYMENT_STATUS = "GROUP",
    Freq = "Total Count"
  )


## base design -- start from the baseline sampling weight developed by Gallup

svy_data_full_sample <- svydesign(id=~PSU, strata=~STRATA, weights=~ANNUAL_WEIGHT1, data=df_w1_arg)
svy_data_full_sample <- rake(
  svy_data_full_sample, 
  sample.margins = list(~AGE_GENDER_REGION,~EDUCATION_GROUP,~EMPLOYMENT_STATUS), 
  population.margins = list(pop.age_gender_region, pop.edu, pop.employ)
)
df_w1_arg$wgts <- weights(svy_data_full_sample)
df_w1_arg <- df_w1_arg |>
  mutate(
    wgts_scaled = n() * wgts / sum(wgts),
    wgts_scaled = case_when(
      wgts_scaled < quantile(wgts_scaled, 0.10) ~ quantile(wgts_scaled, 0.10),
      wgts_scaled > quantile(wgts_scaled, 0.95) ~ quantile(wgts_scaled, 0.95),
      .default = wgts_scaled
    ),
    wgts_scaled = n() * wgts_scaled / sum(wgts_scaled)
  )

## ISSUE IDENTIFIED WITH SURVEY PACKAGE -- raking fails when any cell of the design is missing when using the rake(.) function, needed to update by adding the "..." pass along argument to postStratify which does allow for some cells to be missing.
## Needed to update survey::rake function to allow "partial = TRUE"
rake2 <- function (design, sample.margins, population.margins, control = list(maxit = 10, epsilon = 1, verbose = FALSE), compress = NULL, ...) {
  if (!missing(control)) {
    control.defaults <- formals(rake)$control
    for (n in names(control.defaults)) if (!(n %in% names(control))) 
      control[[n]] <- control.defaults[[n]]
  }
  is.rep <- inherits(design, "svyrep.design")
  if (is.rep && is.null(compress)) 
    compress <- inherits(design$repweights, "repweights_compressed")
  if (is.rep) 
    design$degf <- NULL
  if (length(sample.margins) != length(population.margins)) 
    stop("sample.margins and population.margins do not match.")
  nmar <- length(sample.margins)
  if (control$epsilon < 1) 
    epsilon <- control$epsilon * sum(weights(design, "sampling"))
  else epsilon <- control$epsilon
  strata <- lapply(sample.margins, function(margin) if (inherits(margin, 
                                                                 "formula")) {
    mf <- model.frame(margin, data = design$variables, na.action = na.fail)
  })
  allterms <- unlist(lapply(sample.margins, all.vars))
  ff <- formula(paste("~", paste(allterms, collapse = "+"), 
                      sep = ""))
  oldtable <- svytable(ff, design)
  if (control$verbose) 
    print(oldtable)
  oldpoststrata <- design$postStrata
  iter <- 0
  converged <- FALSE
  while (iter < control$maxit) {
    design$postStrata <- NULL
    for (i in 1:nmar) {
      design <- postStratify(design, strata[[i]], population.margins[[i]], 
                             compress = FALSE,...)
    }
    newtable <- svytable(ff, design)
    if (control$verbose) 
      print(newtable)
    delta <- max(abs(oldtable - newtable))
    if (delta < epsilon) {
      converged <- TRUE
      break
    }
    oldtable <- newtable
    iter <- iter + 1
  }
  rakestrata <- design$postStrata
  if (!is.null(rakestrata)) {
    class(rakestrata) <- "raking"
    design$postStrata <- c(oldpoststrata, list(rakestrata))
  }
  design$call <- sys.call()
  if (is.rep && compress) 
    design$repweights <- compressWeights(design$repweights)
  if (is.rep) 
    design$degf <- degf(design)
  if (!converged) 
    warning("Raking did not converge after ", iter, " iterations.\n")
  return(design)
}

## Run post-stratification on margins (raking) by age-gender-region, education, and employment
svy_data_web <- svydesign(id=~PSU, strata=~STRATA, weights=~ANNUAL_WEIGHT1, data=df_w1_arg |> filter(MODE_ANNUAL == 3))
svy_data_web <- rake2(
  svy_data_web, 
  sample.margins = list(~AGE_GENDER_REGION,~EDUCATION_GROUP,~EMPLOYMENT_STATUS), 
  population.margins = list(pop.age_gender_region, pop.edu, pop.employ),
  control = list(maxit = 100)
)

svy_data_phone <- svydesign(id=~PSU, strata=~STRATA, weights=~ANNUAL_WEIGHT1, data=df_w1_arg |> filter(MODE_ANNUAL == 2))
## note -- will have warnings due to missing cells in the population
svy_data_phone <- rake2(
  svy_data_phone, 
  sample.margins = list(~AGE_GENDER_REGION,~EDUCATION_GROUP,~EMPLOYMENT_STATUS), 
  population.margins = list(pop.age_gender_region, pop.edu, pop.employ),
  control = list(maxit = 100), 
  partial = TRUE
)

## rescale and trim weights
df_phone <- df_w1_arg |> filter(MODE_ANNUAL == 2)
df_phone$wgts <- weights(svy_data_phone)
df_phone <- df_phone|>
  mutate(
    wgts_scaled = n() * wgts / sum(wgts),
    wgts_scaled = case_when(
      wgts_scaled < quantile(wgts_scaled, 0.10) ~ quantile(wgts_scaled, 0.10),
      wgts_scaled > quantile(wgts_scaled, 0.95) ~ quantile(wgts_scaled, 0.95),
      .default = wgts_scaled
    ),
    wgts_scaled = n() * wgts_scaled / sum(wgts_scaled)
  )

df_web <- df_w1_arg |> filter(MODE_ANNUAL == 3)
df_web$wgts <- weights(svy_data_web)
df_web <- df_web|>
  mutate(
    wgts_scaled = n() * wgts / sum(wgts),
    wgts_scaled = case_when(
      wgts_scaled < quantile(wgts_scaled, 0.10) ~ quantile(wgts_scaled, 0.10),
      wgts_scaled > quantile(wgts_scaled, 0.95) ~ quantile(wgts_scaled, 0.95),
      .default = wgts_scaled
    ),
    wgts_scaled = n() * wgts_scaled / sum(wgts_scaled)
  )

## make the survey design object with the updated trimmed post-stratified weights so that the standard errors are computed using the sample size and not the population size
svy_data_full_sample <- svydesign(id=~PSU, strata=~STRATA, weights=~wgts_scaled, data=df_w1_arg)
svy_data_phone <- svydesign(id=~PSU, strata=~STRATA, weights=~wgts_scaled, data=df_phone)
svy_data_web <- svydesign(id=~PSU, strata=~STRATA, weights=~wgts_scaled, data=df_web)


svymean(~SFI, design=svy_data_full_sample)
svymean(~SFI, design=svy_data_phone)
svymean(~SFI, design=svy_data_web)
svyby(~SFI, by=~MODE_ANNUAL, svymean, design=svy_data_full_sample)

svyby(~SFI, by=~AGE_GRP, svymean, design=svy_data_full_sample)
svyby(~SFI, by=~AGE_GRP, svymean, design=svy_data_phone)
svyby(~SFI, by=~AGE_GRP, svymean, design=svy_data_web)

## conclusion -- yes, there is likely a "mode" by age interaction going on
