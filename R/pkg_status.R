#' list packages presenting with status on BBS
#' @param af instance of ArtifSet
#' @param event character(1) one of "ERROR", "WARNINGS", "OK", "skipped", "TIMEOUT"
#' @param phase character(1) one of "install", "buildsrc", "checksrc", "buildbin"
#' @param host character(1) relevant hostname
#' @examples
#' af = make_demo_ArtifSet()
#' pkgs_with_event(af)
#' @export
pkgs_with_event = function(af, event="ERROR", phase="install", host="nebbiolo2") {
#
# FIXME -- use tabulate_states here, should be only approach to getting information from BUILD_STATUS_DB
#
  bdb = read.delim(grep("BUILD_STATUS_DB", slot(af, "extra_paths"), value=TRUE), sep="#",
         h=FALSE)
  ans = bdb |> dplyr::filter(V2 == host & V3 == paste0(phase, ": ", event))
  names(ans) = c("package", "host", "state")
  ans
}

#' list packages that have a new event comparing af2 (later) to af1 (earlier)
#' @param af1 instance of ArtifSet, earlier
#' @param af2 instance of ArtifSet, later
#' @param event character(1) one of "ERROR", "WARNINGS", "OK", "skipped", "TIMEOUT"
#' @param phase character(1) one of "install", "buildsrc", "checksrc", "buildbin"
#' @param host character(1) relevant hostname
#' @examples
#' af2 = make_demo_ArtifSet(url = demo_url2(), 
#'        demostring = "test_report_3.14_bioc_20220105", destbase = "test_report_0105")
#' af1 = make_demo_ArtifSet()
#' new_events(af1, af2)
#' @export
new_events = function(af1, af2, event="ERROR", phase="checksrc", host="nebbiolo2") {
# FIXME should check dates
  p2 = pkgs_with_event(af2, event=event, phase=phase, host=host)
  p1 = pkgs_with_event(af1, event=event, phase=phase, host=host)
  setdiff(p2$package, p1$package)
}

#' list packages that have resolved an event comparing af2 (later) to af1 (earlier)
#' @param af1 instance of ArtifSet, earlier
#' @param af2 instance of ArtifSet, later
#' @param event character(1) one of "ERROR", "WARNINGS", "OK", "skipped", "TIMEOUT"
#' @param phase character(1) one of "install", "buildsrc", "checksrc", "buildbin"
#' @param host character(1) relevant hostname
#' @examples
#' af2 = make_demo_ArtifSet(url = demo_url2(), 
#'        demostring = "test_report_3.14_bioc_20220105", destbase = "test_report_0105")
#' af1 = make_demo_ArtifSet()
#' becomes_OK(af1, af2)
#' @export
becomes_OK = function(af1, af2, event="ERROR", phase="checksrc", host="nebbiolo2") {
  p2 = pkgs_with_event(af2, event="OK", phase=phase, host=host)
  p1 = pkgs_with_event(af1, event=event, phase=phase, host=host)
  intersect(p2$package, p1$package)
}
