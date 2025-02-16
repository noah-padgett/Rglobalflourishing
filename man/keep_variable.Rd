% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{keep_variable}
\alias{keep_variable}
\title{Create Keep Variable Vector}
\usage{
keep_variable(x, data)
}
\arguments{
\item{x}{(character vector) of a vector of variable names in data}

\item{data}{(data.frame)}
}
\value{
a logical vector of length equal to the length of 'x'
}
\description{
keep_variable(.), a wrapper around the function 'check_variance(.)' to create a logical
vector of length(x) of the results of the check_variance function. The result can then be used to
to subset the vector 'x', e.g., 'x[keep_variable(x, data)]', to only include predictors 'x' that
have any variance. The inverse selection, 'x[!(keep_variable(x, data))]', can be used to identify
which variables were excluded.
}
\details{
Creates a logical vector to keep or delete variables for use in a regression model.
}
\examples{
{
  df <- data.frame(A = c(1, 1, 0, 0), B = c("B", "B", "B", "B"))
  keep_variable(c("A", "B"), df)
}
}
