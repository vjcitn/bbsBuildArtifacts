#warnings
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

#' A demonstration URL with trimmed report.tgz in a local file, more recent than that for `demo_url`
#' @export
demo_url2 = function() paste0("file://", system.file("test_report_3.14_bioc_20220105/test_report_0105.tgz",
    package="bbsBuildArtifacts"))


isFileURL = function(x) substr(x, 1, 7) == "file://"
is_online = function() !is.null(curl::nslookup("r-project.org", error = FALSE))

#' get reporting artifacts for a Bioconductor collection (e.g., software, experiment, workflow, ...)
#' @import BiocFileCache
#' @importFrom utils download.file untar
#' @param version character(1) defaults to "3.14"
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param url defaults to NULL, if supplied, used to retrieve or cache tgz file
#' @return A gzipped tarball is downloaded, informatively renamed, copied to a cache, and the cache reference is returned.
#' @note Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.  If url is NULL, the
#' report.tgz content will be cached with name formed with type, version, and date of command invocation.
#' @examples
#' cururl = paste0("file://", system.file("test_report_3.14_bioc_20211210/test_report.tgz", 
#'     package="bbsBuildArtifacts"))
#' id = get_report_tgz_cacheid(url=cururl)
#' BiocFileCache::bfcquery(BiocFileCache::BiocFileCache(), cururl)
#' @export
get_report_tgz_cacheid = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(), url=NULL) {
    if (is.null(url)) {
       current_url = build_report_tgz_url(version, type)
       informative_name = paste0(type, "_", version, "_", Sys.Date(), "_report.tgz")
       }
    else {
       current_url = url
       informative_name = current_url
       }
    chk = bfcquery(cache, informative_name)
    if (!(length(chk$rpath)==0)) return(chk$rid)
    tf = tempfile()
    if (is_online()) download.file(current_url, tf)
     else stop("you are offline and requested artifact tarball is not in cache.")
    if (!isFileURL(current_url)) {
      base = dirname(tf)
      leaf = basename(tf)
      full_inf_name = paste0(base, "/", informative_name)
      file.rename(tf, full_inf_name)
      }
    else full_inf_name = tf # if file:// just move content to cache
    bfcadd(cache, rname=informative_name, fpath=full_inf_name, action="move")
    chk = bfcquery(cache, informative_name)
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
#' Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.
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
#' @note Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.
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


#> dir(paste0(aa[1], "/raw-results/nebbiolo2/"), full=TRUE)
#[1] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//buildsrc-out.txt"    
#[2] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//buildsrc-summary.dcf"
#[3] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//checksrc-out.txt"    
#[4] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//checksrc-summary.dcf"
#[5] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//install-out.txt"     
#[6] "/var/folders/n4/p9th81md60s8nv12yv40sv8m0000gp/T//RtmpzkUheQ/report/a4/raw-results/nebbiolo2//install-summary.dcf" 

#' try with date
#' @export
get_report_tgz_cacheid2 = function(version = "3.14", type="bioc", cache=BiocFileCache::BiocFileCache(), url=NULL,
     date=Sys.Date()) {
    if (is.null(url)) {
       current_url = build_report_tgz_url(version, type)
       informative_name = paste0(type, "_", version, "_", date, "_report.tgz")
       }
    else {
       current_url = url
       informative_name = current_url
       }
    chk = bfcquery(cache, informative_name)
    if (!(length(chk$rpath)==0)) return(chk$rid)
    tf = tempfile()
    if (is_online()) download.file(current_url, tf)
     else stop("you are offline and requested artifact tarball is not in cache.")
    if (!isFileURL(current_url)) {
      base = dirname(tf)
      leaf = basename(tf)
      full_inf_name = paste0(base, "/", informative_name)
      file.rename(tf, full_inf_name)
      }
    else full_inf_name = tf # if file:// just move content to cache
    bfcadd(cache, rname=informative_name, fpath=full_inf_name, action="move")
    chk = bfcquery(cache, informative_name)
    chk$rid
}
