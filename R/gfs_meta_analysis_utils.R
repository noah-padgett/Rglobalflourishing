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
get_meta_ci <- function(fit, type = "Q", ci.alpha = 0.05, .exp = FALSE, .digits=2) {
  if (type == "Q") {
    tmp <- confint(fit, type = "PL")
    tmp.lb <- tmp[["random"]][2, 2]
    tmp.ub <- tmp[["random"]][2, 3]
  }
  if ( type == "rma") {
    tmp <- tidy(fit, conf.int = TRUE)
    tmp.lb <- tmp[1, "estimate", drop = TRUE] - qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
    tmp.ub <- tmp[1, "estimate", drop = TRUE] + qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
  }
  if ( type == "rma.rr") {
    tmp <- tidy(fit, conf.int = TRUE)
    tmp.lb <- tmp[1, "estimate", drop = TRUE] - qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
    tmp.ub <- tmp[1, "estimate", drop = TRUE] + qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
  }
  if (type == "FE") {
    tmp <- tidy(fit, conf.int = TRUE)
    tmp.lb <- tmp[1, "estimate", drop = TRUE] - qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
    tmp.ub <- tmp[1, "estimate", drop = TRUE] + qnorm(1-ci.alpha/2)*tmp[1, "std.error", drop = TRUE]
  }
  ci = paste0(
    "(",
    .round(tmp.lb, .digits), ",",
    .round(tmp.ub, .digits), ")"
  )
  if(.exp){
    ci = paste0(
      "(",
      .round(exp(tmp.lb), .digits), ",",
      .round(exp(tmp.ub), .digits), ")"
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

