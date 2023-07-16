

#> dir("/tmp/RtmpDWSfEI/file4e5764f0d680b/report/", patt="txt", full=TRUE)
#[1] "/tmp/RtmpDWSfEI/file4e5764f0d680b/report//BUILD_STATUS_DB.txt"      
#[2] "/tmp/RtmpDWSfEI/file4e5764f0d680b/report//pkg_dep_graph.txt"        
#[3] "/tmp/RtmpDWSfEI/file4e5764f0d680b/report//PROPAGATION_STATUS_DB.txt"
#> readLines(.Last.value[1]) -> bdb
#> length(grep("nebbiolo1#install: OK", bdb))
#[1] 2211
#> 
#

allhosts = function(af) as.character(slot(af, "hostnames"))
insttag = function(x) sprintf("%s#install: OK", x)
chktag = function(x) sprintf("%s#checksrc: OK", x)
build_st_db = function(af) {
	pa1 = paths(af)[1]
	repdir = dirname(pa1)
	paste0(repdir, "/BUILD_STATUS_DB.txt")
}
n_inst_ok = function(af, ho) length(grep(insttag(ho), readLines(build_st_db(af))))
n_chk_ok = function(af, ho) length(grep(chktag(ho), readLines(build_st_db(af))))

unlink_artifset = function(af) { pa1 = paths(af)[1]; dd = dirname(pa1); unlink(dd, recursive=TRUE) }

#' retrieve and process information from a series of report.tgz collected over time
#' @param version character(1) defaults to "3.17"
#' @param startd character(1) we will begin the scan at this date (must answer as.Date)
#' @param endd character(1) we will end the scan at this date (must answer as.Date)
#' @note startd and endd are used as `date` in setup_artifacts, and it is assumed that
#' report.tgz from past dates have been cached using setup_artifacts on those dates
#' @export
scan_artifs = function(version="3.17", startd="2023-06-21", endd="2023-07-15") {
	ini = as.Date(startd)
	las = as.Date(endd)
	totry = seq(from=ini, to=las, by=1)
	sol = mclapply(totry, mylk(version))
        extr = list()
        extr$version = version
        extr$startd = startd
        extr$endd = endd
	names(sol) = as.character(totry)
        sol = c(sol, extr)
	class(sol) = c("artifset_scan", "list")
        sol
}

#' printer
#' @export
print.artifset_scan = function(x, ...) {
  cat(sprintf("bbsBuildArtifacts artifset_scan for version %s\n", x$version))
  cat(sprintf(" start %s, end %s\n", x$startd, x$endd))
}


#library(bbsBuildArtifacts)
#source("scan_artifs.R")
mylk = function(version) function (x) 
{
    cat("setup ...")
    chk = try(tmp <- setup_artifacts(version = version, date = x))
    if (inherits(chk, "try-error")) 
        return(NA)
    gather = function(x) {
        sol = list(inst = n_inst_ok(chk, x), chk = n_chk_ok(chk, 
            x))
#        names(sol[[1]]) = allhosts(chk)
#        names(sol[[2]]) = allhosts(chk)
        sol
    }
    ans = lapply(allhosts(chk), gather)
    names(ans) = allhosts(chk)
		 
    cat("unlinking\n")
    unlink_artifset(tmp)
    ans
}
