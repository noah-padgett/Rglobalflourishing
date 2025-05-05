
tryCatch({
future_walk(OUTCOME.VEC0, \(x) {
  walk(FOCAL_PREDICTOR, \(y) {

    # 👇 Force load packages
    library(dplyr)

    # 👇 Enable traceback for this worker
    options(error = function() {
      cat("=== TRACEBACK ===\n")
      traceback(2)
      quit(save = "no", status = 1)  # kills the worker cleanly
    })

    # 👇 Wrap the function call in tryCatch for full control
    tryCatch({
      gfs_run_regression_single_outcome(
        your.outcome = x,
        your.pred = y,
        data.dir = data.dir,
        wgt = ANNUAL_WEIGHT_R2,
        psu = PSU,
        strata = STRATA,
        covariates = DEMO.CHILDHOOD.PRED,
        contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
        list.composites = LIST.COMPOSITES[[1]],
        standardize = TRUE,
        pc.cutoff = 7,
        pc.rule = "constant",
        res.dir = "results-primary",
        appnd.txt.to.filename = "_primary_wpc"
      )
    }, error = function(e) {
      message("❌ Error in worker: ", e$message)
      traceback(2)
      stop(e)
    })
  })
})
}, error = function(e) {
  tf <- tempfile("traceback_", fileext = ".txt")
  zz <- file(tf, open = "wt")
  sink(zz)
  sink(zz, type = "message")

  cat("❌ Error: ", e$message, "\n\n")
  traceback(2)

  sink(type = "message")
  sink()
  close(zz)

  message("Full traceback written to: ", tf)
  stop(e)
})







cl <- parallel::makeCluster(4)  # or however many cores you want
plan(cluster, workers = cl)

future_walk(
  OUTCOME.VEC0,
  \(x) {
    walk(FOCAL_PREDICTOR, \(y) {
      # FORCE load inside the worker
      load_packages()
      gfs_run_regression_single_outcome(
        your.outcome = x,
        your.pred = y,
        data.dir = data.dir,
        wgt = ANNUAL_WEIGHT_R2,
        psu = PSU,
        strata = STRATA,
        covariates = DEMO.CHILDHOOD.PRED,
        contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
        list.composites = LIST.COMPOSITES[[1]],
        standardize = TRUE,
        pc.cutoff = 7,
        pc.rule = "constant",
        res.dir = "results-primary",
        appnd.txt.to.filename = "_primary_wpc"
      )
    })
  }
)




















load(here::here(data.dir, "gfs_w2_imputed_data_20imp.RData"))

#df.imp <- df.imp %>%
#  filter(COUNTRY == "Egypt")

#df.imp.long <- recode_imputed_data(
#  df.imp,
#  list.default = RECODE.DEFAULTS,
#  list.composites = LIST.COMPOSITES,
#  wgt = "ANNUAL_WEIGHT_R2"
#)

df_arg <- readRDS(here(data.dir, "recoded_imputed_data_obj_Argentina_imp1.rds"))


inspect_globals_for_future <- function(fun) {
  if (!requireNamespace("globals", quietly = TRUE)) {
    stop("Please install the 'globals' package first.")
  }

  globals_info <- globals::globalsOf(fun, recursive = TRUE)
  global_names <- names(globals_info)

  cat("You likely need to include these in `.options = furrr_options(globals = ...)`:\n\n")
  cat(paste0("globals = list(\n  ",
             paste(paste0(global_names, " = ", global_names), collapse = ",\n  "),
             "\n)\n"))
  invisible(global_names)
}

inspect_globals_for_future(
  function(x) {
    walk(FOCAL_PREDICTOR, function(y) {
      gfs_run_regression_single_outcome(
        your.outcome = x,
        your.pred = y,
        data.dir = data.dir,
        wgt = ANNUAL_WEIGHT_R2,
        psu = PSU,
        strata = STRATA,
        covariates = DEMO.CHILDHOOD.PRED,
        contemporaneous.exposures = CONTEMPORANEOUS.EXPOSURES.VEC,
        list.composites = LIST.COMPOSITES[[1]],
        standardize = TRUE,
        pc.cutoff = 7,
        pc.rule = "constant",
        res.dir = "results-primary",
        appnd.txt.to.filename = "_primary_wpc"
      )
    })
  }
)


#df.isr <- df.imp %>%
#  filter(COUNTRY == "Israel") %>%

unique(df_turkey$PSU)


country_vec <- as.character(unique(df.raw$COUNTRY))

recode_imp_by_country(country_name = "Israel")

mem_res <- bench::mark(

  map(.x = country_vec,
      .f = ~recode_imp_by_country(country_name = .x))

)
