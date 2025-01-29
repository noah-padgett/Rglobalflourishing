#' Meta-Analysis Utility Functions
#'
#' A set of utility functions used in the post-processing of the meta-analysis results.
#'
#' @param fit the resulting object from fitting the metafor::rma(.) function.
#' @param x vector of calibrated effect sizes
#' @param q a numerical values (default = 0) that defines the threshold of meaningful effect size
#' @param above a logical indicator of whether to come the proportion above or below the threshold
#' @param method a character string defining how the proportion should be computed (default is "empirical" but can be "normal")
#' @param theta the population average effect size
#' @param tau the population standard deviation
#' @return depends on function...
#' @examples
#' # TO-DO
#' @export
compute_calibrated <- function(fit) {
  theta <- as.numeric(fit["b"])
  tau2 <- as.numeric(fit["tau2"])
  yi <- as.numeric(unlist(fit["yi"]))
  vi <- as.numeric(unlist(fit["vi"]))
  # a reviewer called these empirical bayes estimates:
  theta + (yi - theta) * sqrt(tau2 / (tau2 + vi))
}
#' @rdname compute_calibrated
#' @export
proportion_meaningful <- function(x, q = 0, above = TRUE, method = "empirical", theta = 0, tau = 1) {
  k <- length(x)
  out <- NA
  if (method == "empirical") {
    if (above) {
      out <- sum(x > q) / k
    } else {
      out <- sum(x < q) / k
    }
  }
  if (method == "normal") {
    # follows from Matheur and VanderWeele (2017)
    if (above) {
      out <- 1 - pnorm((q - theta) / tau)
    } else {
      out <- pnorm((q - theta) / tau)
    }
  }
  out
}
#' @rdname compute_calibrated
#' @export
get_meta_ci <- function(fit, type = "Q", .digits=2) {
  if (type == "Q") {
    tmp <- confint(fit, type = "PL")
    ci <- paste0(
      "(",
      .round(tmp[["random"]][2, 2], .digits), ",",
      .round(tmp[["random"]][2, 3], .digits), ")"
    )
  }
  if ( type == "rma") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci = paste0(
      "(",
      .round(tmp[1, "conf.low", drop = TRUE], .digits), ",",
      .round(tmp[1, "conf.high", drop = TRUE], .digits), ")"
    )
  }
  if ( type == "rma.rr") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci = paste0(
      "(",
      .round(exp(tmp[1, "conf.low", drop = TRUE]), .digits), ",",
      .round(exp(tmp[1, "conf.high", drop = TRUE]), .digits), ")"
    )
  }
  if (type == "FE") {
    tmp <- tidy(fit, conf.int = TRUE)
    ci <- paste0(
      "(",
      .round(tmp[1, "conf.low", drop = TRUE], .digits), ",",
      .round(tmp[1, "conf.high", drop = TRUE], .digits), ")"
    )
  }
  ci
}
#' @rdname compute_calibrated
#' @export
compute_global_pvalue <- function(x, p.min = 1e-16) {
  # Replace all p-values of exactly 0 with p.min
  x <- ifelse(x < p.min, p.min, x)
  x <- x[!is.na(x)]
  L <- length(x)
  unlist(p.hmp(x, L = L))
}
#' @rdname compute_calibrated
#' @export
add_pop_wgts <- function(df) {
  # add in population sizes
  poplist <- c(
    20329009,
    6097316,
    964761394,
    193828725,
    107139250,
    73216028,
    56059669,
    69392175,
    6191774,
    25639336,
    109534481,
    30917886,
    38728302,
    39144781,
    8296398,
    31482707,
    62703653,
    53129081,
    259759435,
    33085830,
    156216636,
    89518608
  )
  names(poplist) <-
    c(
      "Australia",
      "Hong Kong",
      "India",
      "Indonesia",
      "Japan",
      "Philippines",
      "Egypt",
      "Germany",
      "Israel",
      "Kenya",
      "Nigeria",
      "Poland",
      "South Africa",
      "Spain",
      "Sweden",
      "Tanzania",
      "Turkiye",
      "United Kingdom",
      "United States",
      "Argentina",
      "Brazil",
      "Mexico"
    )
  df$wi <- 1
  for (i in names(poplist)) {
    df$wi[df$Country == i] <- poplist[i] / sum(poplist)
  }
  df
}
#' @rdname compute_calibrated
#' @export
construct_meta_input_from_saved_results <- function(res.dir, outcomes, predictors) {
  tmp.list <- list()
  for (your.outcome in outcomes) {
    for (your.pred in predictors) {
      load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results.RData")))
      tmp.list[[paste0(your.outcome, "_", your.pred)]] <- metainput
    }
  }
  tmp.list
}

#' @rdname compute_calibrated
#' @export
get_country_specific_regression_results <- function(res.dir, outcomes, predictors) {
  tmp.list <- list()
  for (your.outcome in outcomes) {
    for (your.pred in predictors) {
      load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results.RData")))
      # create new columns in output to help with constructing tables
      output <- output  %>%
        dplyr::filter(Variable == "FOCAL_PREDICTOR")
      tmp.list[[paste0(your.outcome, "_", your.pred)]] <- output
    }
  }
  tmp.list
}
#' @rdname compute_calibrated
#' @export
get_country_specific_pca_summary <- function(res.dir, outcomes, predictors) {
  tmp.list <- list()
  for (your.outcome in outcomes) {
    for (your.pred in predictors) {
      load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results.RData")))
      tmp.list[[paste0(your.outcome, "_", your.pred)]] <- fit.pca.summary
    }
  }
  tmp.list
}
