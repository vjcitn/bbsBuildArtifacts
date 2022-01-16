#' Artifact set S4 class definition
#' @export
setClass("ArtifSet", representation(type="character", version="character",
    pkg_paths="character", extra_paths="character", hostnames="character",
    tarball_date="character"))


#' display salient information about ArtifSet
#' @param object instance of ArtifSet
#' @export
setMethod("show", "ArtifSet", function(object) {
  cat("bbsBuildArtifacts ArtifSet instance.\n")
  cat(sprintf("  %d pkg paths for type %s, Bioconductor version %s.\n", length(slot(object, "pkg_paths")), 
    slot(object, "type"), slot(object, "version")))
  cat(sprintf("  %d extra file paths.\n", length(slot(object, "extra_paths"))))
  cat(sprintf("  tarball production date: %s\n", slot(object, "tarball_date")))
  platinf = Platform_info(object)
  cat(sprintf("R version: %s", platinf$R_info))
  cat("Platforms: \n  ")
  cat(platinf$OS_info, sep="\n  ")
  cat("Use paths(aset)[...] to retrieve selected paths.\n")
})

Platform_info = function(af) {
 nodeinfos = grep("NodeInfo", slot(af, "extra_paths"), value=TRUE)
 os_info = vapply( slot(af, "hostnames"), function(node) { 
   target = grep(node, nodeinfos, value=TRUE)
   pull_R_meta(target)$os_meta
   }, character(1))
 R_info = pull_R_meta( nodeinfos[1] )$r_meta
 list(OS_info = os_info, R_info = R_info )
}

#' helper
#' @param x instance of ArtifSet
#' @export
setGeneric("paths", function(x) standardGeneric("paths"))

#' helper subsetter
#' @param x instance of ArtifSet
#' @export
setMethod("paths", "ArtifSet", function(x) slot(x, "pkg_paths"))

#' vector of hostnames for build nodes
#' @param release character(1) defaults to '3.14'
#' @export
hostnames_by_release = function(release="3.14") {
 if (release=="3.14") return(c(linux="nebbiolo2", macos="machv2", windows="tokay2"))
 else if (release=="3.15") return(c(linux="nebbiolo1", macos="merida1", windows="riesling1"))
}

avail_hostnames = function(x) {
  top = grep("install.html$", dir(x@pkg_paths[1]), value=TRUE)
  gsub("-install.html", "", top)
}

#' Create ArtifSet instance
#' @param type character(1) defaults to 'bioc' which implies 'software'; see Note.
#' @param version character(1) defaults to "3.14"
#' @param date character(1) "yyyy-mm-dd", not obligatory but can be used to retrieve earlier cache entry
#' @param hostnames character() vector of host names for which build artifacts are available
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param destination character(1) path to folder to use for artifacts
#' @param verbose logical(1) if TRUE (default) will provide message about tar activity 
#' @param extracted defaults to NULL, if non-null a character(1) path to folder that holds `report` folder
#' @param url character(1) passed to `get_report_tgz_cacheid`
#' @param destbase character(1) name of folder holding all artifacts, defaults to 'report'
#' @note Use bbsBuildArtifacts:::valid_types() to see valid values for `type`.  The logic
#' of managing artifacts from multiple dates is very cumbersome and may be unreliable,
#' but the 'date of tarball production' will be reported for each ArtifSet.
#' @examples
#' cururl = demo_url()
#' z = setup_artifacts(url=cururl, destbase="test_report")
#' z
#' @export
setup_artifacts = function(type="bioc", version="3.14", date, hostnames=hostnames_by_release(version),
   cache=BiocFileCache::BiocFileCache(), destination=tempfile(),
   verbose=TRUE, extracted=NULL, url=NULL, destbase="report") {
   if (!is.null(extracted)) {
        destination = extracted
        }
   else {
       if (missing(date)) date = Sys.Date()
       tag = get_report_tgz_cacheid(version=version, type=type, cache=cache, url=url, date=date) ## NEW
       tarpath = cache[[tag]]
       if (verbose) message("starting untar...")
       chk = try(untar(tarpath, exdir=destination))
       if (inherits(chk, "try-error")) stop("could not untar artifact tgz")
       if (verbose) message("done.")
     }
     allfiles = dir(paste0(destination, "/", destbase), full.names=TRUE)
     all_pkg_folders = grep( non_package_pattern(), allfiles,
         invert=TRUE, value=TRUE)
     names(all_pkg_folders) = basename(all_pkg_folders)
     extra_files = grep( non_package_pattern(), allfiles,
         invert=FALSE, value=TRUE)
     bsdb = grep("BUILD_STATUS_DB", allfiles, value=TRUE)[1]
     dbinf = try(file.info(bsdb)[,"mtime"], silent=TRUE)
     if (nchar(dbinf)>0) dbdate = as.character(as.Date(dbinf))
       else dbdate = NA_character_
     ans = new("ArtifSet", type=type, version=version, pkg_paths=all_pkg_folders,
            extra_paths = extra_files, hostnames=hostnames, tarball_date=dbdate)
     chk = avail_hostnames(ans)
     if (!(all(ans@hostnames %in% hostnames))) message("check hostnames value")
     ans
}



#' basic artifact data collected to tibble
#' @importFrom BiocParallel bplapply
#' @param x instance of ArtifSet
#' @param row.names defaults to NULL
#' @param optional defaults to FALSE
#' @param \dots unused
#' @export
as.data.frame.ArtifSet = function (x, row.names = NULL, optional = FALSE, ...) {
  by_host = vector("list", length(x@hostnames))
  suppressWarnings({  # lots of try() here, we know some processes don't produce data
  for (i in seq_len(length(by_host))) {
    by_host[[i]] = do.call(rbind, bplapply(x@pkg_paths, function(z) simplify_artifact_build_dcfs(package_by_host_data(z,
               host=x@hostnames[i])$dcfs)))
    }
  })
ans = do.call(rbind, by_host)
rownames(ans) = NULL
ans
}

#' simplify production of the test ArtifSet
#' @param cache BiocFileCache instance to use for checking for available image and retrieving if possible
#' @param preclean logical(1) defaults to TRUE, in which case we search `cache` for demostring in cache and remove
#' @param demostring character(1) token to query BiocFileCache for in preclean step
#' @param url character(1) url for local tgz
#' @param destbase character(1) folder name under which all BBS content is held, defaults to `test_report`
#' @examples
#' make_demo_ArtifSet()
#' @export
make_demo_ArtifSet = function(cache=BiocFileCache::BiocFileCache(), preclean=TRUE, 
       demostring="test_report_3.14_bioc_20211210", url=demo_url(), destbase="test_report") {
  if (preclean) {
    ca = cache
    lk = BiocFileCache::bfcquery(ca, demostring)
    if (length(lk$rid)>0) bfcremove(ca, lk$rid)
    }
  setup_artifacts(url=url, destbase=destbase, cache=cache)
}
