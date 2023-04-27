
testthat::context("Download NetLogo")
testthat::test_that("supported_netlogo_versions", {

  versions <- supported_netlogo_versions()

  testthat::expect_vector(versions)
  testthat::expect_true(all(check_netlogo_version(versions)))

  mockery::stub(download_netlogo, "utils::download.file", TRUE)
  nlversion <- "5.3.1"
  nlpath <- tempdir()
  os <- NA

  testthat::expect_null(download_netlogo(to = nlpath,
                                         os = os,
                                         version = nlversion,
                                         extract = TRUE))

})
