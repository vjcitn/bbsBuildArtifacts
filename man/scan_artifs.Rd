% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scan_artifs.R
\name{scan_artifs}
\alias{scan_artifs}
\title{retrieve and process information from a series of report.tgz collected over time}
\usage{
scan_artifs(version = "3.17", startd = "2023-06-21", endd = "2023-07-15")
}
\arguments{
\item{version}{character(1) defaults to "3.17"}

\item{startd}{character(1) we will begin the scan at this date (must answer as.Date)}

\item{endd}{character(1) we will end the scan at this date (must answer as.Date)}
}
\description{
retrieve and process information from a series of report.tgz collected over time
}
\note{
startd and endd are used as `date` in setup_artifacts, and it is assumed that
report.tgz from past dates have been cached using setup_artifacts on those dates
}
