#' provide a concise table of events by host
#' @param af ArtifSet instance
#' @examples
#' af = make_demo_ArtifSet()
#' event_freqs(af)  # a package can generate events in multiple phases, so counts are not of 'packages'
#' @export
event_freqs = function(af) {
 ho = slot(af, "hostnames")
 hm = names(ho)
 names(hm) = ho
 hostmap = hm
 tt = tabulate_states(af)
 si = which(tt$state == "skipped")
 tt[si,]$state = paste(tt[si,]$phase, tt[si,]$state, sep=":")
 ttt =  table(tt$host, tt$state)
 ttt = ttt[names(hostmap),]   # in case of multiple related hosts like introducing palomino3
 simplify.tab = function (x, hostmap) 
 {
    nr = nrow(x)
    dat = matrix(as.integer(x), nrow = nr)
    ans = data.frame(dat)
    colnames(ans) = colnames(x)
    rownames(ans) = hostmap[rownames(x)]
    ans = cbind(host=hostmap[rownames(x)], ans)
    rownames(ans) = NULL
    ans
 }
 simplify.tab(ttt, hostmap=hostmap) |> dplyr::select(-c(OK, `buildbin:skipped`)) |>
    dplyr::mutate(skipchk=`checksrc:skipped`) |>
    dplyr::select(host, ERROR, WARNINGS, skipchk, TIMEOUT)
}
