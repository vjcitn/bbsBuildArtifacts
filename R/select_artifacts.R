#' collect the paths to a set of artifacts
#' @param hostpath character(1) e.g., path to a folder named 'tokay1', which holds the build artifacts
#' for this builder
#' @param packs_re character() a vector of regular expressions defining packages of interest
#' @note Could be improved to work on a comprehensive tarball.
#' @examples
#' setup_demo_artifacts()
#' eg = select_artifacts(paste(tempdir(), "tokay2", sep="/"),
#'     c("IRanges", "S4V*", "a4\\."))
#' eg
#' @export
select_artifacts = function(hostpath, packs_re) {
 arts = unlist(lapply(packs_re, function(x) dir(hostpath, pattern=x, full.names=TRUE)))
 pnames = unique(sapply(strsplit(basename(arts), "\\."), "[", 1))
 ans = list(artifacts = arts, re = packs_re, pnames=pnames, host=basename(hostpath))
 class(ans) = "artifact_paths"
 ans
}

#' simplify display of artifact set
#' @param x instance of artifact_paths
#' @param \dots not used
#' @export
print.artifact_paths = function(x, ...) {
 cat(sprintf("%d artifact paths for regexp vector\n", length(x$artifacts)))
 cat("  ", dQuote(x$re), "\n")
}

# helpers for package file size string

getpfsz = function(w) {paste(strsplit(w, " ")[[1]][1], getb(w))}
getb = function(x) gsub(".*>(.*)<.*", "\\1", x)


#' get process outcomes for artifacts
#' @param apathset instance of artifact_paths
#' @param process character(1) one of "buildbin", "buildsrc", "install", "check"
#' @param type character(1) one of "EllapsedTime", "Status", "PackageFileSize"
#' @examples
#' setup_demo_artifacts()
#' eg = select_artifacts(paste(tempdir(), "tokay2", sep="/"),
#'      c("IRanges", "S4V*", "a4\\."))
#' eg2 = select_artifacts(paste(tempdir(), "nebbiolo1", sep="/"),
#'      c("IRanges", "S4V*", "a4\\."))
#' get_process_outcomes(eg, process="buildsrc", type="Status")
#' get_process_outcomes(eg2, "buildsrc", type="EllapsedTime")
#' get_process_outcomes(eg2, "buildsrc", type="PackageFileSize")
#' @export
get_process_outcomes = function( apathset , process, type ) {
  stopifnot( inherits(apathset, "artifact_paths") )
  sel = grep(paste(process, "..*dcf$", sep=""), apathset$artifacts, value=TRUE)
  dat = lapply(sel, read.dcf)
  tmp = sapply(dat, function(x) c(x[,"Package"], x[,type]))
  vers = sapply(dat, function(x) c(x[,"Version"], x[,type]))
  if (type == "EllapsedTime") outco = as.numeric(gsub(" seconds", "", tmp[type,]))
  else if (type == "PackageFileSize") outco = unname(sapply(tmp[type,], getpfsz))
  else outco=tmp[type,]
  ans = data.frame(package=tmp[1,,drop=TRUE], version=vers[1,,drop=TRUE], host=apathset$host, process=process)
  ans[[type]] = outco
  ans
}

#' get events from an artifact set using rcmdcheck::parse_check
#' @param apathset instance of artifact_paths
#' @examples
#' setup_demo_artifacts()
#' eg = select_artifacts(paste(tempdir(), "tokay2", sep="/"),
#'      c("IRanges", "S4V*", "a4\\."))
#' get_events(eg)
#' @export
get_events = function( apathset ) {
  chkdirs = grep("Rcheck", apathset$artifacts, value=TRUE)
  tmp = lapply(chkdirs, dir, full.names=TRUE)
  logs = lapply(tmp, function(x) grep("00check.log", x, value=TRUE))
  ps = lapply(logs, rcmdcheck::parse_check)
  ans = lapply(ps, function(x) x[c("warnings", "notes", "errors")])
  names(ans) = apathset$pnames
  ans
}

#' .Rcheck artifacts are not reliably installed from inst, so we use a zip file
#' @importFrom utils unzip
#' @export
setup_demo_artifacts = function() {
 td = tempdir()
 curd = getwd()
 on.exit(setwd(curd))
 setwd(td)
 if (!file.exists("tokay2"))
   unzip(system.file("demo_artifacts/demo.zip", package="bbsBuildArtifacts"))
}
