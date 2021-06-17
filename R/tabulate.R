#' tabulate key metrics for a set of packages for a host
#' @param packre character() vector of regular expressions picking out
#' packages in a folder of build artifacts.  For example `"a4\\."` can be used 
#' as an element of `packre` to isolate package `a4`, while `"a4"` would
#' select both `a4` and `a4Core`.
#' @param hostpath character(1) path to folder, named for host where packages were built to generate artifacts,
#' in which all artifacts reside
#' @param procs character() processes to report, from among "install", "buildsrc", "buildbin" (only
#' relevant for hosts on which binary packages are built), "checksrc"
#' @note Early implementation just tabulates time and checksrc status.
#' @examples
#' packre = c("IRanges", "S4V", "a4")
#' host = "nebbiolo1"
#' setup_demo_artifacts()
#' hostpath = paste(tempdir(), host, sep="/")
#' chk = tabulate_bbs_metrics( packre, hostpath )
#' chk
#' @export
tabulate_bbs_metrics = function(packre, hostpath, procs = c("install", "buildsrc", "checksrc")) {
  aset = select_artifacts(hostpath, packre)
  timetabs = lapply(procs, function(x) get_process_outcomes(aset, x, type="EllapsedTime"))
  stattab = get_process_outcomes(aset, "checksrc", type="Status")
  ans = timetabs[[1]]
  if (length(timetabs)>1) {
   for (i in 2:length(timetabs))
     ans = merge(ans, timetabs[[i]])
   }
  merge(ans, stattab)
}
