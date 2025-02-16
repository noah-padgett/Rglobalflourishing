% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_variable_value_switch_funtions.R
\name{get_outcome_better_name}
\alias{get_outcome_better_name}
\title{Get outcome name (better)}
\usage{
get_outcome_better_name(var, include.name = TRUE, include.wave = FALSE)
}
\arguments{
\item{var}{a character string (e.g., 'HAPPY_W1')}

\item{include.wave}{(logical) whether to include wave identifier (Wave 1) in returned string, not applicable when include.name is true}

\item{include.label}{(logical) whether to include variable name (var) in returned string}
}
\value{
a character string
}
\description{
A relatively simple switch function to get a better name for each variable
}
\examples{
{
  get_outcome_better_name("HAPPY_W1")
  get_outcome_better_name("HAPPY_W1", include.name = FALSE)
  get_outcome_better_name("HAPPY_W1", include.name = FALSE, include.wave = TRUE)
}
}
