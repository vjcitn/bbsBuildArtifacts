tabulate_states = function(aset) {
 stopifnot(inherits(aset, "ArtifSet"))
 dat = read.delim(grep("BUILD_STATUS_DB.txt", aset@extra_paths, value=TRUE)[1], sep="#", h=FALSE)
 names(dat) = c("package", "host", "ph.stat")
 tmp = strsplit(dat$ph.stat, ": ")
 dat$phase = vapply(tmp, function(x)x[1], character(1))
 dat$state = vapply(tmp, function(x)x[2], character(1))
 dat[, c("package", "host", "phase", "state")]
}
