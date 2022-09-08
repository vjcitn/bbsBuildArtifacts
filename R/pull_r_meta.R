#' acquire information on build platform from BBS NodeInfo.html file
#' @importFrom rvest read_html html_nodes html_text
#' @param htmlfile character(1) path to NodeInfo.html for a given platform
#' @examples
#' demaf = make_demo_ArtifSet()
#' ho = slot(demaf, "hostnames")
#' ho2_info = grep(ho[2], slot(demaf, "extra_paths"), value=TRUE)
#' pull_R_meta(grep("NodeInfo", ho2_info, value=TRUE))
#' @export
pull_R_meta = function(htmlfile) {
 z = rvest::read_html(htmlfile)
 dat = rvest::html_nodes(z, "table")[[2]]
 tab = rvest::html_nodes(dat, "td")
 rstring = html_text(tab)[10]
 osstring = html_text(tab)[4]
 list(r_meta=rstring, os_meta=osstring)
}
