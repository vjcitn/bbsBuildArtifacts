#' Artifact set S4 class definition
#' @export
setClass("ArtifSet", representation(type="character", version="character",
    pkg_paths="character", extra_paths="character", hostnames="character"))


#' display salient information about ArtifSet
#' @export
setMethod("show", "ArtifSet", function(object) {
  cat("bbsBuildArtifacts ArtifSet instance.\n")
  cat(sprintf("  %d pkg paths for type %s, version %s.\n", length(slot(object, "pkg_paths")), 
    slot(object, "type"), slot(object, "version")))
  cat(sprintf("  %d extra file paths.\n", length(slot(object, "extra_paths"))))
  cat("  use paths(aset)[...] to see selected paths.\n")
})

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
#' @param hostnames character() vector of host names for which build artifacts are available
#' @param cache instance of `BiocFileCache::BiocFileCache()`
#' @param destination character(1) path to folder to use for artifacts, defaults to `tempdir()`
#' @param verbose logical(1) if TRUE (default) will provide message about tar activity 
#' @param extracted defaults to NULL, if non-null a character(1) path to folder that holds `report` folder
#' @export
setup_artifacts = function(type="bioc", version="3.14", hostnames=hostnames_by_release(version),
   cache=BiocFileCache::BiocFileCache(), destination=tempdir(),
   verbose=TRUE, extracted=NULL) {
   if (!is.null(extracted)) destination = extracted
   else {
       tag = get_report_tgz_cacheid(version=version, type=type, cache=cache)
       tarpath = cache[[tag]]
       if (verbose) message("starting untar...")
       chk = try(untar(tarpath, exdir=destination))
       if (inherits(chk, "try-error")) stop("could not untar artifact tgz")
       if (verbose) message("done.")
     }
     allfiles = dir(paste0(destination, "/report"), full.names=TRUE)
     all_pkg_folders = grep( bbsBuildArtifacts:::non_package_pattern(), allfiles,
         invert=TRUE, value=TRUE)
     names(all_pkg_folders) = basename(all_pkg_folders)
     extra_files = grep( bbsBuildArtifacts:::non_package_pattern(), allfiles,
         invert=FALSE, value=TRUE)
     ans = new("ArtifSet", type=type, version=version, pkg_paths=all_pkg_folders,
            extra_paths = extra_files, hostnames=hostnames)
     chk = avail_hostnames(ans)
     if (!(all(ans@hostnames %in% hostnames))) message("check hostnames value")
     ans
}



#' basic artifact data collected to tibble
#' @param x instance of ArtifSet
#' @param row.names defaults to NULL
#' @param options defaults to FALSE
#' @param \dots unused
#' @export
as.data.frame.ArtifSet = function (x, row.names = NULL, optional = FALSE, ...) {
  by_host = vector("list", length(x@hostnames))
  suppressWarnings({  # lots of try() here, we know some processes don't produce data
  for (i in seq_len(length(by_host))) {
    by_host[[i]] = do.call(rbind, lapply(x@pkg_paths, function(z) simplify_artifact_build_dcfs(package_by_host_data(z,
               host=x@hostnames[i])$dcfs)))
    }
  })
ans = do.call(rbind, by_host)
rownames(ans) = NULL
ans
}
