
data.dir <- "test/ignore/data"
imp.files <- list.files(here(data.dir,"imp"))
imp.files <- imp.files[str_detect(imp.files, "imputed_data_obj")]
imp.files <- imp.files[str_detect(imp.files, paste0("_imp_"))]

check.imp <- lapply(imp.files, FUN=function(x){
  load(here::here(data.dir,"imp",x), ex <- new.env())
  df.tmp <- complete(ex$fit.imp, action = 1)
  var.w.miss <- df.tmp |>
    summarise(
      across(everything(), ~mean(is.na(.)))
    )
  keep.cols <- names(var.w.miss)[var.w.miss > 0]
  keep.cols  <- keep.cols[!(keep.cols %in% c('WAVE_Y2', 'WAVE_Y3', 'DOI_ANNUAL_Y2', 'DOI_ANNUAL_Y3', 'POLITICAL_ID_Y1', 'POLITICAL_ID_Y2', 'POLITICAL_ID_Y3', 'REGION2_Y2', 'REGION2_Y3', 'REGION3_Y1', 'REGION3_Y2', 'REGION3_Y3', 'ANNUAL_WEIGHT_C2', 'ANNUAL_WEIGHT_L2', 'ANNUAL_WEIGHT_C3', 'ANNUAL_WEIGHT_L3', 'FULL_PARTIAL_Y2', 'FULL_PARTIAL_Y3'))]
  var.w.miss |> select(all_of( keep.cols))
})
names(check.imp) <- imp.files
check.imp

# Australia -- major issue
# Hong Kong -- major issue
# Egypt - Believe God
# Kenya -- only a single case still has missing -> a case had mising age at y1
# Poland -- age missing
# South Africa -- single case -- age y1
# Tanzania -- age y1
# United Kingdom -- single case
#

c("Australia", "Hong Kong", "Egypt", "Kenya", "Poland", "South Africa", "Tanzania", "United Kingdom")



data.dir <- "test/ignore/data"
imp.files <- list.files(here(data.dir,"recoded"))
imp.files <- imp.files[str_detect(imp.files, "imputed_data_obj")]

check.imp <- lapply(imp.files, FUN=function(x){
  df.tmp <- readr::read_rds(here::here(data.dir,"recoded",x))
  var.w.miss <- df.tmp |>
    group_by(COUNTRY, .imp) |>
    summarise(
      across(everything(), ~mean(is.na(.)))
    )
  keep.cols <- names(var.w.miss)[var.w.miss > 0.0 & var.w.miss < 1.0]

  keep.cols  <- keep.cols[!(keep.cols %in% c('REGION2_Y1', 'SELFID1','SELFID2','COV_SELFID1','RACE','COV_RACE_PLURALITY', 'WAVE_Y2', 'WAVE_Y3', 'DOI_ANNUAL_Y2', 'DOI_ANNUAL_Y3', 'POLITICAL_ID_Y1', 'POLITICAL_ID_Y2', 'POLITICAL_ID_Y3', 'REGION2_Y2', 'REGION2_Y3', 'REGION3_Y1', 'REGION3_Y2', 'REGION3_Y3', 'ANNUAL_WEIGHT_C2', 'ANNUAL_WEIGHT_L2', 'ANNUAL_WEIGHT_C3', 'ANNUAL_WEIGHT_L3', 'FULL_PARTIAL_Y2', 'FULL_PARTIAL_Y3'))]
  var.w.miss |> select(COUNTRY, .imp, all_of( keep.cols))
}) |> bind_rows()
#names(check.imp) <- imp.files
check.imp
