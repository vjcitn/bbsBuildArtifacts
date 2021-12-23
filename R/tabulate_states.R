#' (memoised) retrieve data.frame from BUILD_STATUS_DB.txt
#' @param aset ArtifSet instance
#' @export
tabulate_states = function(aset) {
 stopifnot(inherits(aset, "ArtifSet"))
 dat = read.delim(grep("BUILD_STATUS_DB.txt", aset@extra_paths, value=TRUE)[1], sep="#", h=FALSE)
 names(dat) = c("package", "host", "ph.stat")
 tmp = strsplit(dat$ph.stat, ": ")
 dat$phase = vapply(tmp, function(x)x[1], character(1))
 dat$state = vapply(tmp, function(x)x[2], character(1))
 dat[, c("package", "host", "phase", "state")]
}

#' get info on packages with non-ok status
#' @return a list with one element per phase in which some package produced non-OK state
#' @param aset ArtifSet instance
#' @examples
#' z = make_demo_ArtifSet()
#' packages_with_events(z)
#' @export
packages_with_events = function(aset) {
 tab = tabulate_states(aset)
 ina = names(paths(aset))
 tab = tab[ which(tab$package %in% ina), ]
 tmp = tab[ which(tab$state != "OK"), ]
 split(tmp, tmp$phase)
}
