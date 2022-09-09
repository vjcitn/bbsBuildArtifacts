

#' enumerate packages that fail on only a subset of platforms
#' @param af ArtifSet instance
#' @param phase.in character(1) e.g., "checksrc", "buildbin", "install"
#' @param state character(1) e.g., "ERROR", "WARNINGS"
#' @examples
#' af = make_demo_ArtifSet()
#' idiosync_status(af)
#' @export
idiosync_status = function(af, phase.in="checksrc", state="ERROR") {
   ho = slot(af, "hostnames")
   linhost = ho["linux"]
   winhost = ho["windows"]
   machost = ho["macos"]
   dd = readLines(af@extra_paths[grep("BUILD_STATUS", af@extra_paths)])
   ddd = strsplit(dd, "#|:\ ")
   eee =do.call(rbind,ddd)
   dfcur = data.frame(eee) 
   names(dfcur) = c("pkgname", "host", "phase", "status")
   errdf = dfcur |> dplyr::filter(status==state & phase==phase.in)
   spl= with(errdf, split(pkgname,host))
   anycerr = unique(errdf$pkgname)
   wm = unique(c(spl[[winhost]], spl[[machost]]))
   lm = unique(c(spl[[linhost]], spl[[machost]]))
   lw = unique(c(spl[[linhost]], spl[[winhost]]))
   linux_only = setdiff(spl[[linhost]], wm)
   win_only = setdiff(spl[[winhost]], lm)
   mac_only = setdiff(spl[[machost]], lw)
   list(linux_only=linux_only, win_only=win_only, mac_only=mac_only)
}
