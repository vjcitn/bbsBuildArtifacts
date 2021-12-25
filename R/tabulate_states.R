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

#' filter packages by host, phase, state
#' @param af ArtifSet instance
#' @param host character(1) name of build host
#' @param phase character(1) one of 'install', 'buildsrc', 'checksrc', 'buildbin'
#' @param event_class character(1) one of 'ERROR', 'WARNINGS', 'TIMEOUT', 'skipped', 'wontinstall'
#' @return character vector or NULL
#' @examples
#' af = make_demo_ArtifSet()
#' packnames_with_events(af) # uses a large BUILD_STATUS_DB.txt in demo ArtifSet
#' @export
packnames_with_events = function(af, host="nebbiolo2",
   phase="checksrc", event_class="ERROR") {
  stopifnot(phase %in% c('install', 'buildsrc', 'checksrc', 'buildbin'))
  stopifnot(event_class %in% c('ERROR', 'WARNINGS', 'TIMEOUT', 'skipped','wontinstall'))
  tt = tabulate_states(af)
  if (event_class == "wontinstall") {
    phase = "install"
    event_class = "ERROR"
    }
  tt[ which(tt$host==host & tt$phase==phase & tt$state == event_class), ]$package
}

