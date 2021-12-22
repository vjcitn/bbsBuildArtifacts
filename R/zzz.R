# Because conversion to data.frame is a bit slow, let's do it once per session
     
     .onLoad <- function(libname, pkgname) {
      as.data.frame.ArtifSet <<- memoise::memoise(as.data.frame.ArtifSet)
#      artifact_folder_paths <<- memoise::memoise(artifact_folder_paths)
      tabulate_states <<- memoise::memoise(tabulate_states)
     }

