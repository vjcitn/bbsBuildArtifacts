% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bbs_package_data.R
\name{make_BBS_package_data}
\alias{make_BBS_package_data}
\title{encapsulate BBS state for a package}
\usage{
make_BBS_package_data(afset, packagename)
}
\arguments{
\item{afset}{instance of ArtifSet}

\item{packagename}{character(1)}
}
\description{
encapsulate BBS state for a package
}
\examples{
af = make_demo_ArtifSet()
ho = slot(af, "hostnames")
ho
pd1 = make_BBS_package_data(af, "zinbwave")
hd = host_data_by_phase( pd1, ho[1], "buildsrc")
head(hd)
hd = host_data_by_phase( pd1, ho[1], "checksrc")
head(hd)
pd2 = make_BBS_package_data(af, "zlibbioc")
hd = host_data_by_phase( pd2, ho[1], "install")
tail(hd)
hd = host_data_by_phase( pd2, ho[1], "checksrc")
head(hd)
}
