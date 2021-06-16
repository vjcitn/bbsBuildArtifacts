
library(bbsBuildArtifacts)
library(testthat)

test_that("select regexps are resepected", {
 chk1 = select_artifacts(system.file("demo_artifacts/tokay2", package="bbsBuildArtifacts"), c("IRanges", "S4V*", "a4"))
 expect_true(length(chk1$artifacts)==36)
 chk2 = select_artifacts(system.file("demo_artifacts/tokay2", package="bbsBuildArtifacts"), c("IRanges", "S4V*", "a4\\."))
 expect_true(length(chk2$artifacts)==27)
})
