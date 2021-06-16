
library(bbsBuildArtifacts)
library(testthat)

test_that("select regexps are resepected", {
 setup_demo_artifacts()
 chk1 = select_artifacts(paste(tempdir(), "tokay2", sep="/"), c("IRanges", "S4V*", "a4"))
 expect_true(length(chk1$artifacts)==36)
 chk2 = select_artifacts(paste(tempdir(), "tokay2", sep="/"), c("IRanges", "S4V*", "a4\\."))
 expect_true(length(chk2$artifacts)==27)
})
