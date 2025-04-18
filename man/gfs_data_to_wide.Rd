% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gfs_data_to_wide.R
\name{gfs_data_to_wide}
\alias{gfs_data_to_wide}
\title{Transform Data to Wide Format}
\usage{
gfs_data_to_wide(
  data,
  var = "WAVE",
  ids = c("ID", "COUNTRY"),
  test = FALSE,
  ...
)
}
\arguments{
\item{data}{name of file to get data from}

\item{var}{variable name containing the wave/timepoint indicator (default = "WAVE")}

\item{ids}{variable name containing the ID variables separating individual cases}

\item{test}{(false)}

\item{...}{other arguments}
}
\value{
a wide dataset
}
\description{
TO-DO
}
\details{
Transforms data provided by COS to "wide" format for analysis in longitudinal structure/.
}
\examples{
{
  # TO-DO
}
}
