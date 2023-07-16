extract_counts = function(x) {
   ds = as.Date(names(x))  # will produce NA for version, startd, endd
   drop = which(is.na(ds)) 
   dat = x
   if (length(drop)>0) dat = x[-drop]
   hon = names(dat[[1]])
   byhost = vector("list", length(hon))
   names(byhost) = hon
   for (h in hon) {
     byhost[[h]] = list(inst = sapply(dat, function(x) x[[h]]$inst),
                        chk = sapply(dat, function(x) x[[h]]$chk))
     }
   list(byhost=byhost, dates=ds[-drop], hosts=hon)
}

melt_artifset_scan = function(x) {
   ec = extract_counts(x)
   counts = ec$byhost
   dates = ec$dates
   hdfs = vector("list", length(ec$hosts))
   for (i in ec$hosts) hdfs[[i]] = data.frame(host=i, date=dates,
      inst=ec$byhost[[i]]$inst, chk=ec$byhost[[i]]$chk)
   ans = do.call(rbind, hdfs)
   instdf = ans
   instdf = dplyr::mutate(instdf, num=inst)
   instdf$phase = "install"
   chkdf = ans
   chkdf = dplyr::mutate(instdf, num=chk)
   chkdf$phase = "checksrc"
   ans = rbind(instdf, chkdf)
   #ggplot(ans, aes(x=date, y=num, colour=host)) + geom_line(lwd=2) + facet_grid(phase~., scales="free")
   ans
}

#' plot counts of checksrc OK for a scan
#' @importFrom dplyr mutate
#' @import ggplot2
#' @param x instance of artifset_scan
#' @examples
#' data(scan_demo_3.17)
#' plot_scan_chk(scan_demo_3.17) + ggplot2::ylim(1590,1720)
#' @export
plot_scan_chk = function(x) {
   dat = melt_artifset_scan(x) |> dplyr::filter(phase=="checksrc")
   ggplot(dat, aes(x=date, y=num, colour=host)) + geom_line(lwd=2) +
    ylab("count of packages with checksrc=OK")
}

#' plot counts of install OK for a scan
#' @param x instance of artifset_scan
#' @examples
#' data(scan_demo_3.17)
#' plot_scan_inst(scan_demo_3.17) + ggplot2::ylim(2120,2250)
#' @export
plot_scan_inst = function(x) {
   dat = melt_artifset_scan(x) |> dplyr::filter(phase=="install")
   ggplot(dat, aes(x=date, y=num, colour=host)) + geom_line(lwd=2) +
    ylab("count of packages with install=OK")
}

