#' given a package name, version, and biocPkgList instance, obtain the email for the maintainer
#' @param pkg character(1)
#' @param version character(1)
#' @param bpl biocPkgList instance
#' @examples
#' bpl = BiocPkgTools::biocPkgList(version="3.17")
#' maint4pkg("parody", "3.17", bpl)
#' @export
maint4pkg = function(pkg, version, bpl) {
 stopifnot(is.character(pkg))
 pkgMeta <- bpl[bpl[["Package"]] %in% pkg, ]
 if (!nrow(pkgMeta)) stop("No pkg '", pkg, "' found on Bioconductor")
 mainInfo <- pkgMeta[["Maintainer"]][[1L]]
 unname(vapply(mainInfo, BiocPkgTools:::.emailCut, character(1L)))
}

