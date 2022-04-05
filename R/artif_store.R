#' tabulate available artifacts in a cache
#' @importFrom BiocFileCache BiocFileCache bfcquery
#' @param ca cache reference
#' @param qstring character(1) search key
#' @export
available_artifs = function(ca = BiocFileCache::BiocFileCache(), qstring="report.tgz") {
 chk = BiocFileCache::bfcquery(ca, qstring)
 chki = grep(qstring, chk$rpath)
 chk = chk[chki,]
 created = chk$create_time
 vers = gsub("_20.*", "", gsub(".*bioc_", "", chk$rpath))
 data.frame(rid=chk$rid, created=created, version=vers)
}
