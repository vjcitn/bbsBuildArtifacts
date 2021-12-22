# Because conversion to data.frame is a bit slow, let's do it once per session
     
#' zzz.R
#' @importFrom memoise memoise
     .onLoad <- function(libname, pkgname) {
      as.data.frame.ArtifSet <<- memoise::memoise(as.data.frame.ArtifSet)
      artifact_folder_paths <<- memoise::memoise(artifact_folder_paths)
     }

