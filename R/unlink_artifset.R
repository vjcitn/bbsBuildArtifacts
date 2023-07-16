#' remove all elements of an ArtifSet from disk
#' @param aset ArtifSet instance
#' @export
unlink_artifset = function(aset) {
	pp = slot(aset, "pkg_paths")[1]
	dr = dirname(pp)
	unlink(dr, recursive=TRUE)
}
