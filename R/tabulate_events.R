#' scan a slice of an ArtifSet for events produced in R CMD check of source
#' @importFrom BiocParallel bpparam bplapply
#' @param af instance of ArtifSet
#' @param event_class character(1) "warnings" (default) or "errors"
#' @param host character(1) name of host on which build report was generated
#' @param BPPARAM instance of a BiocParallel BiocParallelParam subclass
#' @param which_to_use vector of elements in ArtifSet (can be package names or numeric indices), defaults to NULL
#' in which case all elements are examined for events
#' @examples
#' cururl = demo_url()
#' z = setup_artifacts(url=cururl, destbase="test_report")
#' collect_events(z, event_class="warnings", host="nebbiolo2")
#' collect_events(z, event_class="errors", host="nebbiolo2")
#' @export
collect_events = function(af, host, event_class="warnings", BPPARAM=BiocParallel::bpparam(), which_to_use=NULL) {
  stopifnot(event_class %in% c("warnings", "errors"))
  if (missing(host)) stop("must supply host")
  if (is.null(which_to_use)) pa = paths(af)
  else pa = paths(af)[which_to_use]
  out1 = BiocParallel::bplapply(pa, function(x) package_by_host_data(x, host=host))
  out2 = lapply(out1, function(x) try(x$parsed_chks[[event_class]], silent=TRUE))
  noparse = which(sapply(out2, inherits, "try-error"))
  if (length(noparse)>0) out = out2[-noparse]
  else out=out2
  clean = which(sapply(out, function(x) length(x)==0))
  noparse_paths = pa[noparse]
  cleanpaths = pa[clean]
  out = out[-union(noparse,clean)]
  ans = list(events=out, noparse_paths=noparse_paths, cleanpaths=cleanpaths)
  class(ans) = c("event_collection", class(ans))
  ans
}

#' short report of an event collection
#' @param x event_collection instance
#' @param \dots not used
#' @export
print.event_collection = function(x, ...) {
  cat("bbsBuildArtifacts event_collection instance\n")
  cat(sprintf("%d events, %d no-parse paths.\n", length(x$events), length(x$noparse)))
  invisible(NULL)
}

