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


#' get reporting artifacts for a Bioconductor collection (e.g., software, experiment, workflow, ...)
#' @import BiocFileCache
#' @importFrom utils download.file untar
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @return A gzipped tarball is downloaded, copied to a cache, and the cache reference is returned.
#' @note Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.
#' @export
get_report_tgz_cacheid = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache()) {
    current_url = build_report_tgz_url(version, type)
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
#' @return character(1) path to folder beneath which all artifacts are found
#' @note Typically some additional files exist in addition to the package-related folders
#' at the returned path.
#' `:::non_package_pattern()` can be used to find these.
#' @export
path_to_untarred_artifact_folders = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(),
       destination=tempdir(), verbose=TRUE) {
    rid = get_report_tgz_cacheid(version = version, type=type, cache=cache)
    path = cache[[rid]]
    if (verbose) message("starting untar...")
    untar(path, exdir=destination)
    if (verbose) message("done")
    paste0(destination, "/report")
}

# length(grep(non_package_pattern(), dir(paste0(pp, "/report"), full=TRUE), invert=TRUE)
    
#' obtain all package artifact folder paths
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @export
artifact_folder_paths = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache()) {
   pa = path_to_untarred_artifact_folders(version=version, type=type, cache=cache)
   allpa = dir(pa, full.names=TRUE)
   vals = grep( non_package_pattern(), allpa, invert=TRUE, value=TRUE)
   names(vals) = basename(vals)  # now use vals[pkgname] to get path
   class(vals) = "artifact_folder_paths"
   vals
}

print.artifact_folder_paths = function(x, ...) {
   cat("artifact_folders_paths instance:\n")
   cat(sprintf("  There are %d folders.", length(x)), "\n")
   cat(sprintf("  Use %s[pkgname] to get full path.", deparse(substitute(x))), "\n")
}

dummy.dcf = function() {   # in case a dcf file does not exist, we return this
structure(c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", 
"NA"), .Dim = c(1L, 10L), .Dimnames = list(NULL, c("Package", 
"Version", "Command", "StartedAt", "EndedAt", "EllapsedTime", 
"RetCode", "Status", "PackageFile", "PackageFileSize")))
}

safe.read.dcf = function(x) {
   tmp = try(read.dcf(x))
   if (!inherits(tmp, "try-error")) return(tmp)
   return(dummy.dcf())
   }

package_by_host_data = function(afpath, host="nebbiolo2", summary_types = c("install", "buildsrc", "checksrc")) {
   stopifnot(length(afpath)==1)
   stopifnot(dir.exists(afpath))
   pas = sprintf(paste0(afpath, "/raw-results/", host, "//%s-summary.dcf"), summary_types)
   dcfs = lapply(pas, function(x) safe.read.dcf(x))
   names(dcfs) = summary_types
   class(dcfs) = "artifact_build_dcfs"
   dcfs
}

simplify_artifact_build_dcfs = function(x, ...) {
   pkgname = x[[1]][, "Package"]
   pkgversion = x[[1]][, "Version"]
   ac = as.character
   getsec = function(x) as.numeric(gsub(" seconds", "", x))
   stats = do.call(rbind, lapply(x, function(z) data.frame(status=ac(z[, "Status"]), elapsed_time=getsec(ac(z[, "EllapsedTime"])))))
   ans = data.frame(pkgname=ac(pkgname), pkgversion=ac(pkgversion), stats=stats, phase = rownames(stats))
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

