% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_variable_value_switch_funtions.R
\name{get_outcome_scale}
\alias{get_outcome_scale}
\title{Get Outcome Response Scale}
\usage{
get_outcome_scale(var)
}
\arguments{
\item{var}{a character string (e.g., 'HAPPY_Y1')}
}
\value{
a character string (by default returns 'cont')
}
\description{
A relatively simple switch function to get the scale ('bin', 'cont')
}
\examples{
{
  get_outcome_scale("HAPPY_Y1")
}
}
