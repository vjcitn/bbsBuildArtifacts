% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event_freqs.R
\name{event_freqs}
\alias{event_freqs}
\title{provide a concise table of events by host}
\usage{
event_freqs(af, simplify_columns = TRUE)
}
\arguments{
\item{af}{ArtifSet instance}

\item{simplify_columns}{logical(1) if FALSE, no simplification}
}
\description{
provide a concise table of events by host
}
\examples{
af = make_demo_ArtifSet()
event_freqs(af, simplify_columns=FALSE)  # a package can generate events in multiple phases, so counts are not of 'packages'
}
