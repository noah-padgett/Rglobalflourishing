#' Append Principal Components to Data.Frame
#'
#' Adds the principal components of a set of variables to the data.frame.
#'
#' @param data a data.frame or survey.design object
#' @param var a character vector of variables names in data (default uses all variables in data)
#' @param std a logical (defaults FALSE) of whether to standardized all resulting PCA after conducting PCA.
#' @returns the data.frame or survey.design object with PCs added to data (or under variables)
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' Additional details about PCA forthcoming...
#'
append_pc_to_df <- function(data, var = NULL, std = FALSE) {
  # get class of data
  is.survey <- ifelse(any(class(data) %in% c(
    "survey.design2", "survey.design"
  )), TRUE, FALSE)

  if (is.null(var)) {
    var <- ifelse(is.survey,
      colnames(data$variables),
      colnames(data)
    )
  }

  if (is.survey) {
    fit.pca <- survey::svyprcomp(
      reformulate(var), # OR: as.formula(paste0(" ~ ", paste0( var, collapse = " + " ))),
      design = data,
      scale = TRUE,
      scores = TRUE,
      center = TRUE
    )
  } else {
    fit.pca <- stats::prcomp(
      reformulate(var), # OR: as.formula(paste0(" ~ ", paste0( var, collapse = " + " ))),
      data = data,
      scale. = TRUE,
      center = TRUE
    )
  }
  # get raw data for combining
  if (is.survey) {
    df.x <- data$variables
  } else {
    df.x <- data
  }
  # return PC
  Zpc <- fit.pca$x
  df.pc <- as.data.frame(Zpc)
  colnames(df.pc) <- paste0("PC_", 1:ncol(Zpc))
  if (std) {
    df.pc <- df.pc %>%
      mutate(across(everything(), \(x) scale(x)))
  }
  df <- cbind(df.x, df.pc)
  if (is.survey) {
    df <- survey::svydesign(
      data = df,
      ids = ~PSU,
      strata = ~STRATA,
      weights = ~WGT
    )
  }
  df
}

#' Get Eigenvalues
#'
#' Extacts the estimated eigenvalues for summary later.
#'
#' @param data a data.frame or survey.design object
#' @param var a character vector of variable names
#' @param cor a logical of whether to get eigenvalues based on correlation matrix instead of covariance matrix
#' @returns a vector of eigenvalues
#' @examples {
#'   # to-do
#' }
#' @export
#' @description
#' When data is a survey.design object, the eigenvalues are estimated based on the weighted
#' covariance matrix estimated with the stats package function cov.wt(.) using the design
#' probabilities.
#'
get_eigenvalues <- function(data, var, cor = FALSE) {
  # get class of data
  is.survey <- ifelse(any(class(data) %in% c(
    "survey.design2", "survey.design"
  )), TRUE, FALSE)
  if (is.survey) {
    wgt <- data$prob
    est.cov <- stats::cov.wt(x = data$variables[, var], wt = wgt)
    fit.cov <- est.cov$cov
  } else {
    fit.cov <- stats::cov(data[, var])
  }
  fit.cov.eigen <- eigen(fit.cov)
  if (cor) {
    fit.cov.eigen <- eigen(stats::cov2cor(fit.cov))
  }
  fit.cov.eigen$values
}
