#' look at a cache for potential BBS build artifact tarballs
#' @param cache BiocFileCache instance, defaults to BiocFileCache::BiocFileCache()
#' @export
examine_cache = function(cache = BiocFileCache::BiocFileCache()) {
  BiocFileCache::bfcquery(cache, "report.tgz")
}
