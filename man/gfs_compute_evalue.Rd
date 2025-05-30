% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gfs_evalues.R
\name{gfs_compute_evalue}
\alias{gfs_compute_evalue}
\title{Compute E-values for Estimates and Confidence Intervales}
\usage{
gfs_compute_evalue(
  est,
  se = NULL,
  sd = NULL,
  ci.low = NULL,
  ci.up = NULL,
  type = "linear",
  rare = 0,
  what = c("EE", "ECI")
)
}
\arguments{
\item{est}{effect estimate}

\item{se}{standard error of estimate}

\item{sd}{standard deviation of outcome}

\item{ci.low}{lower confidence limit}

\item{ci.up}{upper confidence limit}

\item{type}{how to compute evalue (linear or RR)}

\item{rare}{an option used in the evalues package}

\item{what}{define which values to extract (default is both using `c("EE", "ECI")`)}
}
\value{
a vector containing
* EE, E-value for estimate
* ECI, E-value for confidence interval
}
\description{
TO-DO
}
\details{
Conduct simple sensitivity analyses for OLS or RR estimates. This is a wrapper for
the evalues package.
}
\examples{

load_packages()
gfs_compute_evalue(est = 0.20, se=0.11, sd=1, type='linear')
gfs_compute_evalue(est = 0.20, ci.low=-0.02, ci.up=0.42, type='RR')

}
