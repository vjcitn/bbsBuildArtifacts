###
### Enhancement get last git commit from info.dcf and add to data FIXME
###

#' ingest dcf files for a package
#' @param afpath an element of an artifact_folder_paths instance
#' @param host character(1) host used in BBS
#' @param summary_types character() defaults to `c("install", "buildsrc", "checksrc")`
#' @param read.dcf.silent logical(1) defaults to TRUE, otherwise bad DCF or build will emit error note
#' @examples
#' cururl = demo_url()
#' af = artifact_folder_paths(url=cururl, destbase="test_report")
#' pbh = package_by_host_data(af["affyPara"])
#' pbh
#' @export
package_by_host_data = function(afpath, host="nebbiolo2", 
      summary_types = c("install", "buildsrc", "checksrc"), read.dcf.silent = TRUE) {
   stopifnot(length(afpath)==1)
   stopifnot(dir.exists(afpath))
   if (!requireNamespace("rcmdcheck")) stop("install rcmdcheck to use this function") # too heavy to import?
   pas = sprintf(paste0(afpath, "/raw-results/", host, "//%s-summary.dcf"), summary_types)
   chk_out_pa = paste0(afpath, "/raw-results/", host, "//checksrc-out.txt")
   bld_out_pa = paste0(afpath, "/raw-results/", host, "//buildsrc-out.txt")
   inst_out_pa = paste0(afpath, "/raw-results/", host, "//install-out.txt")
   bldbin_out_pa = paste0(afpath, "/raw-results/", host, "//buildbin-out.txt")
#
# multiple dcfs
#
   dcfs = lapply(pas, function(x) safe.read.dcf(x, silent=read.dcf.silent))
   safe.parse.check = function(x) {
     if (!file.exists(x)) return(NA)
     ans = try(rcmdcheck::check_details(rcmdcheck::parse_check(x)))
     if (inherits(ans, "try-error")) return(NA)
     ans
   }
   names(dcfs) = summary_types
   attr(dcfs, "hostname") = host  # late discovery that host is not listed in DCF
   class(dcfs) = "artifact_build_dcfs"
#
# only one 
#
   parsed_chks = safe.parse.check( chk_out_pa ) #lapply(chk_out_pas, safe.parse.check)
   bld_txt = try(readLines(bld_out_pa), silent=TRUE) # lapply(bld_out_pas, function(x) try(readLines(x), silent=TRUE))
   if (inherits(bld_txt, "try-error")) bld_txt = "no buildsrc-out.txt"
   inst_txt = (function(x) {
                   if (!file.exists(x)) return("no install-out.txt")
                   try(readLines(x), silent=TRUE)
                   })(inst_out_pa)
   bldbin_txt = (function(x) {
                   if (!file.exists(x)) return("no buildbin-out.txt")
                   try(readLines(x), silent=TRUE)
                   })(bldbin_out_pa)
   ans = list(dcfs=dcfs, parsed_chks = parsed_chks, bld_txt=bld_txt, inst_txt=inst_txt,
     bldbin_txt=bldbin_txt, host=host,
     pkgname = basename(afpath))
   class(ans) = "pkg_by_host_data"
   ans
}

#' simplify presentation of pkg data
#' @param x instance of pkg_by_host_data
#' @param \dots not used
#' @export
print.pkg_by_host_data = function(x, ...) {
 cat(sprintf("pkg_by_host_data instance for %s on host %s\n", x$pkgname, x$host))
}

#' produce a data.frame from a collection of `package_by_host_data` outputs
#' @param x instance of `artifact_build_dcfs`
#' @export
simplify_artifact_build_dcfs = function(x) {
    stopifnot(inherits(x, "artifact_build_dcfs"))
    pkgname = x[[1]][, "Package"]
    pkgversion = x[[1]][, "Version"]
    curhost = attr(x, "hostname")
    ac = as.character
    getsec = function(x) as.numeric(gsub(" seconds", "", x))
    stats = do.call(rbind, lapply(x, function(z) data.frame(status = ac(z[, 
        "Status"]), elapsed_time = getsec(ac(z[, "EllapsedTime"])))))
    ans = data.frame(host = curhost, pkgname = ac(pkgname), pkgversion = ac(pkgversion), 
        stats, phase = rownames(stats))
    rownames(ans) = NULL
    ans
}



dummy.dcf = function() {   # in case a dcf file does not exist, we return this
structure(c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
"NA"), .Dim = c(1L, 10L), .Dimnames = list(NULL, c("Package",
"Version", "Command", "StartedAt", "EndedAt", "EllapsedTime",
"RetCode", "Status", "PackageFile", "PackageFileSize")))
}

safe.read.dcf = function(x, silent=TRUE) {
   tmp = suppressWarnings(try(read.dcf(x), silent=silent))
   if (!inherits(tmp, "try-error")) return(tmp)
   return(dummy.dcf())
   }

