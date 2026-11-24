testthat::context("import_nl tests")
testthat::test_that("import_nl extracts the folder and returns the nl object", {

  proj <- file.path(tempfile("import_test"), "proj")
  dir.create(proj, recursive = TRUE)
  writeLines("<model/>", file.path(proj, "model.nlogox"))
  nl <- nl(nlversion = "7.0.4", nlpath = tempdir(),
           modelpath = file.path(proj, "model.nlogox"))

  tarfile <- tempfile(fileext = ".tar.gz")
  suppressMessages(export_nl(nl, tarfile = tarfile))

  ## Read the archive again:
  targetdir <- tempfile("import_target")
  imported <- import_nl(tarfile = tarfile, targetdir = targetdir, new_session = FALSE)

  testthat::expect_identical(imported, nl)
  testthat::expect_true(file.exists(file.path(targetdir, "proj", "model.nlogox")))
  testthat::expect_true(file.exists(file.path(targetdir, "proj", "nlobject.rds")))

  ## An archive without an nl object is rejected before anything is extracted:
  otherdir <- file.path(tempfile("import_other"), "other")
  dir.create(otherdir, recursive = TRUE)
  writeLines("x", file.path(otherdir, "file.txt"))
  othertar <- tempfile(fileext = ".tar.gz")
  wd <- setwd(dirname(otherdir))
  utils::tar(othertar, files = "other", compression = "gzip", tar = "internal")
  setwd(wd)
  emptydir <- tempfile("import_empty")
  testthat::expect_error(import_nl(othertar, targetdir = emptydir),
                         class = "nlrx_error_import_archive")
  testthat::expect_false(dir.exists(emptydir))
})
