% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gfs_pca.R
\name{append_pc_to_df}
\alias{append_pc_to_df}
\title{Append Principal Components to Data.Frame}
\usage{
append_pc_to_df(data, var = NULL, std = FALSE)
}
\arguments{
\item{data}{a data.frame or survey.design object}

\item{var}{a character vector of variables names in data (default uses all variables in data)}

\item{std}{a logical (defaults FALSE) of whether to standardized all resulting PCA after conducting PCA.}
}
\value{
the data.frame or survey.design object with PCs added to data (or under variables)
}
\description{
Additional details about PCA forthcoming...
}
\details{
Adds the principal components of a set of variables to the data.frame.
}
\examples{
{
  # TO-DO
}
}
