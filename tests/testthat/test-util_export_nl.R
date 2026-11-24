testthat::context("export_nl tests")

## A project folder with a model, a subfolder and a decoy file next to it:
local_export_project <- function(root = tempfile("export_test")) {
  proj <- file.path(root, "proj")
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines("<model/>", file.path(proj, "model.nlogox"))
  writeLines("a,b", file.path(proj, "data", "input.csv"))
  writeLines("not part of the project", file.path(root, "decoy.txt"))
  list(root = root, proj = proj,
       nl = nl(nlversion = "7.0.4", nlpath = tempdir(),
               modelpath = file.path(proj, "model.nlogox")))
}

archive_content <- function(tarfile) {
  sort(sub("/$", "", utils::untar(tarfile, list = TRUE, tar = "internal")))
}

project_content <- c("proj/data", "proj/data/input.csv",
                     "proj/model.nlogox", "proj/nlobject.rds")

testthat::test_that("export_nl archives the model folder and the nl object", {

  p <- local_export_project()
  tarfile <- tempfile(fileext = ".tar.gz")

  testthat::expect_message(res <- export_nl(p$nl, tarfile = tarfile),
                           "Exported 3 files")
  testthat::expect_identical(res, file.path(normalizePath(dirname(tarfile), winslash = "/"),
                                            basename(tarfile)))
  testthat::expect_identical(archive_content(tarfile), project_content)

  ## The exported folder is not changed:
  testthat::expect_false(file.exists(file.path(p$proj, "nlobject.rds")))
})

testthat::test_that("export_nl resolves relative paths to the folder they name", {

  p <- local_export_project()
  wd <- setwd(p$proj)
  on.exit(setwd(wd), add = TRUE)

  tarfile <- tempfile(fileext = ".tar.gz")
  suppressMessages(export_nl(p$nl, path = ".", tarfile = tarfile))
  testthat::expect_identical(archive_content(tarfile), project_content)

  setwd(p$root)
  tarfile <- tempfile(fileext = ".tar.gz")
  suppressMessages(export_nl(p$nl, path = "proj", tarfile = tarfile))
  testthat::expect_identical(archive_content(tarfile), project_content)
})

testthat::test_that("export_nl does not mix in files of earlier exports", {

  p1 <- local_export_project()
  p2 <- local_export_project()
  writeLines("only in the first project", file.path(p1$proj, "extra.txt"))

  suppressMessages(export_nl(p1$nl, tarfile = tempfile(fileext = ".tar.gz")))
  tarfile <- tempfile(fileext = ".tar.gz")
  suppressMessages(export_nl(p2$nl, tarfile = tarfile))
  testthat::expect_identical(archive_content(tarfile), project_content)
})

testthat::test_that("export_nl refuses the home directory, the root and archives within the folder", {

  p <- local_export_project()

  testthat::expect_error(export_nl(p$nl, path = "~", tarfile = tempfile()),
                         regexp = "home directory",
                         class = "nlrx_error_export_path")
  testthat::expect_error(export_nl(p$nl, path = "/", tarfile = tempfile()),
                         regexp = "filesystem root",
                         class = "nlrx_error_export_path")
  testthat::expect_error(export_nl(p$nl, tarfile = file.path(p$proj, "export.tar.gz")),
                         class = "nlrx_error_export_path")
})
