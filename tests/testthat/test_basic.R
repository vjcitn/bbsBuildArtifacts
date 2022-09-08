
library(bbsBuildArtifacts)
library(testthat)

test_that("make_demo_ArtifSet works", {
  af = make_demo_ArtifSet()
  pa = paths(af)
  expect_true(length(pa)==11)
  expect_true(length(pa["zlibbioc"])==1)
  expect_true(length(pa["zinbwave"])==1)
})

basic_url = "https://bioconductor.org/checkResults/3.15/bioc-LATEST/report.tgz"

test_that("basic URL generation works", {
  expect_true( 
   bbsBuildArtifacts:::build_report_tgz_url("3.15", "bioc") ==
    basic_url)
})

test_that("cacheid retrieval works", {
   skip_if_offline()
   id = get_report_tgz_cacheid()
   expect_true(substr(id, 1, 3) == "BFC")
   id2 = get_report_tgz_cacheid()
   expect_true(id == id2)
})

test_that("event_freqs works", {
  af = make_demo_ArtifSet()
  ef = event_freqs(af, simplify_columns=FALSE)
  expect_true( all.equal( colnames(ef), 
      c("OK", "WARNINGS")))
  expect_true( all.equal( dim(ef), c(3,2) ) )
})

test_that("ArtifSet coercion works", {
  af = make_demo_ArtifSet()
  adf = as.data.frame(af)
  expect_true( all.equal(
    colnames(adf), 
     c("host", "pkgname", "pkgversion", "status", "elapsed_time", "phase")))
  expect_true( all.equal( dim(adf), c( 132, 6 ) ) )
})
 

test_that("pull_R_meta runs", {
  af = make_demo_ArtifSet()
  pa = grep("NodeInfo", slot(af, "extra_paths"), value=TRUE)[1]
  rmet = pull_R_meta(pa)
  expect_true( all.equal(names(rmet), c("r_meta", "os_meta")) )
})

