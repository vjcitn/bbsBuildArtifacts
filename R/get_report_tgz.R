#
# this R file includes resources to process BBS artifacts
#


valid_types = function() c("bioc", "data-experiment", "workflows", "books", "bioc-longtests")

#
# the tarball includes some supporting information
#
non_package_pattern = function() "\\.png$|\\.html$|\\.txt$|Renviron.*|\\.dcf$|\\.css$|report\\.js"

build_report_tgz_url = function(version, type) {
  stopifnot(type %in% valid_types())
  sprintf("https://bioconductor.org/checkResults/%s/%s-LATEST/report.tgz", version, type)
}


#' A demonstration URL with trimmed report.tgz in a local file
#' @export
demo_url = function() paste0("file://", system.file("test_report_3.14_bioc_20211210/test_report.tgz",
    package="bbsBuildArtifacts"))

#' get reporting artifacts for a Bioconductor collection (e.g., software, experiment, workflow, ...)
#' @import BiocFileCache
#' @importFrom utils download.file untar
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param url defaults to NULL, if supplied, used to retrieve and cache tgz file 
#' @return A gzipped tarball is downloaded, copied to a cache, and the cache reference is returned.
#' @note Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.
#' @examples
#' cururl = paste0("file://", system.file("test_report_3.14_bioc_20211210/test_report.tgz", 
#'     package="bbsBuildArtifacts"))
#' id = get_report_tgz_cacheid(url=cururl)
#' BiocFileCache::bfcquery(BiocFileCache::BiocFileCache(), cururl)
#' @export
get_report_tgz_cacheid = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(),
     url=NULL) {
    if (is.null(url)) current_url = build_report_tgz_url(version, type)
    else current_url = url
    chk = bfcquery(cache, current_url)
    if (!(length(chk$rpath)==0)) return(chk$rid)
    tf = tempfile()
    download.file(current_url, tf)
    bfcadd(cache, rname=current_url, fpath=tf, action="move")
    chk = bfcquery(cache, current_url)
    chk$rid
}

clean_cache = function(version, type, cache=BiocFileCache::BiocFileCache()) {
    current_url = build_report_tgz_url(version, type)
    chk = bfcquery(cache, current_url)
    if (length(chk$rid)==0) {
        message(sprintf("%s not found in supplied cache, returning FALSE", current_url))
        return(FALSE)
        }
    bfcremove(cache, rids = chk$rid)
}

#' untar the artifact archive and return the path
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param destination character(1) path to folder to use, defaults to `tempdir()`
#' @param verbose logical(1) will indicate start and end of untar() process
#' @param url passed to `get_report_tgz_cacheid`
#' @param destbase character(1) name of folder that includes all artifacts, defaults to "report"
#' @return character(1) path to folder beneath which all artifacts are found
#' @note Typically some additional files exist in addition to the package-related folders
#' at the returned path.
#' `:::non_package_pattern()` can be used to find these.
#' @examples
#' cururl = demo_url()
#' td = tempdir()
#' path_to_untarred_artifact_folders(url=cururl, destination=td, destbase="test_report")
#' @export
path_to_untarred_artifact_folders = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(),
       destination=tempdir(), verbose=TRUE, url=NULL, destbase="/report") {
    rid = get_report_tgz_cacheid(version = version, type=type, cache=cache, url=url)
    path = cache[[rid]]
    if (verbose) message("starting untar...")
    untar(path, exdir=destination)
    if (verbose) message("done")
    paste0(destination, "/", destbase)
}

# length(grep(non_package_pattern(), dir(paste0(pp, "/report"), full=TRUE), invert=TRUE)
    
#' obtain all package artifact folder paths
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param url passed to `get_report_tgz_cacheid`
#' @param destbase passed to `path_to_untarred_artifact_folders`, defaults to "report"
#' @examples
#' cururl = demo_url()
#' artifact_folder_paths(url=cururl, destbase="test_report")
#' @export
artifact_folder_paths = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(),
   url=NULL, destbase="report") {
   pa = path_to_untarred_artifact_folders(version=version, type=type, cache=cache, url=url, destbase=destbase)
   allpa = dir(pa, full.names=TRUE)
   vals = grep( non_package_pattern(), allpa, invert=TRUE, value=TRUE)
   names(vals) = basename(vals)  # now use vals[pkgname] to get path
   class(vals) = "artifact_folder_paths"
   vals
}

#' show artifact paths nicely
#' @param x instance of `artifact_folder_paths`
#' @param \dots not used
#' @export
print.artifact_folder_paths = function(x, ...) {
   cat("artifact_folders_paths instance:\n")
   cat(sprintf("  There are %d folders.", length(x)), "\n")
   cat("  Subset via [pkgname] to get full path.\n")
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
#' af = artifact_folder_paths(url=cururl)
#' @export
package_by_host_data = function(afpath, host="nebbiolo2", 
      summary_types = c("install", "buildsrc", "checksrc"), read.dcf.silent = TRUE) {
   stopifnot(length(afpath)==1)
   stopifnot(dir.exists(afpath))
   if (!requireNamespace("rcmdcheck")) stop("install rcmdcheck to use this function") # too heavy to import?
   pas = sprintf(paste0(afpath, "/raw-results/", host, "//%s-summary.dcf"), summary_types)
   chk_out_pas = paste0(afpath, "/raw-results/", host, "//checksrc-out.txt")
   bld_out_pas = paste0(afpath, "/raw-results/", host, "//buildsrc-out.txt")
   bldbin_out_pas = paste0(afpath, "/raw-results/", host, "//buildbin-out.txt")
   dcfs = lapply(pas, function(x) safe.read.dcf(x, silent=read.dcf.silent))
   safe.parse.check = function(x) {
     if (!file.exists(x)) return(NA)
     ans = try(rcmdcheck::check_details(rcmdcheck::parse_check(x)))
     if (inherits(ans, "try-error")) return(NA)
     ans
   }
   parsed_chks = lapply(chk_out_pas, safe.parse.check)
   names(dcfs) = summary_types
   attr(dcfs, "hostname") = host  # late discovery that host is not listed in DCF
   class(dcfs) = "artifact_build_dcfs"
   bld_txt = lapply(bld_out_pas, function(x) try(readLines(x), silent=TRUE))
   bldbin_txt = lapply(bldbin_out_pas, function(x) try(readLines(x), silent=TRUE))
   ans = list(dcfs=dcfs, parsed_chks = parsed_chks, bld_txt=bld_txt, bldbin_txt=bldbin_txt, host=host,
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


#> dir(paste0(aa[1], "/raw-results/nebbiolo2/"), full=TRUE)
#[1] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//buildsrc-out.txt"    
#[2] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//buildsrc-summary.dcf"
#[3] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//checksrc-out.txt"    
#[4] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//checksrc-summary.dcf"
#[5] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//install-out.txt"     
#[6] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//install-summary.dcf" 

