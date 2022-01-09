#' produce a simple list with data on times
#' @param af ArtifSet instance
#' @param num number of largest times to report per phase, default to 10
#' @examples
#' af = make_demo_ArtifSet()
#' r = time_report(af)
#' r
#' summary(r)
#' @export
time_report = function (af, num = 10) 
{
    d = as.data.frame(af)
    et = d$elapsed_time
    bad = which(is.na(et))
    ntail = function(...) tail(..., n = num)
    ans = lapply(split(d[-bad, ], d$phase[-bad]), function(x) {
        ox = order(x$elapsed_time)
        n = x$pkgname[ox]
        h = x$host[ox]
        t = x$elapsed_time[ox]
        data.frame(pkg = ntail(n, num), host = ntail(h, num), 
            time_sec = ntail(t, num))
    })
    summs = lapply(split(d[-bad, ], d$phase[-bad]), function(x) {
       lapply(split(x, x$host), function(x) summary(x$elapsed_time))} )
    res = list(tails=ans, summaries=summs)
    class(res) = c("bbs_time_report", "list")
    res
}

#' print method for time report, focusing on large times
#' @param x instance of bbs_time_report
#' @param \dots not used
#' @export
print.bbs_time_report = function(x, ...) {
  print(x$tails)
}

#' print method for time report, focusing on large times
#' @param object instance of bbs_time_report
#' @param \dots not used
#' @export
summary.bbs_time_report = function(object, ...) {
  print(object$summaries)
}
