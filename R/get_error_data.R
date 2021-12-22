get_event_data = function(afset, phase, status="ERROR"){
  stopifnot(inherits(afset, "ArtifSet"))
  stopifnot(length(phase)==1L & phase %in% c("buildsrc", "install", "checksrc", "buildbin"))
  stopifnot(status %in% c("ERROR", "WARNINGS", "TIMEOUT", "OK"))
  tab = as.data.frame(afset) # memoised, first can be slow
  tab = tab[ which(tab$phase == phase & tab$status == status), ]
  tab
}
