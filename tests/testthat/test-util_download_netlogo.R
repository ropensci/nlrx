
testthat::context("Download NetLogo")
testthat::test_that("supported_netlogo_versions", {

  versions <- supported_netlogo_versions()

  testthat::expect_vector(versions)
  testthat::expect_true(all(check_netlogo_version(versions)))
  testthat::expect_false(check_netlogo_version("non_valid_version"))
  testthat::expect_error(check_netlogo_version("non_valid_version", throw_error = TRUE))

  mockery::stub(download_netlogo, "utils::download.file", NULL)
  mockery::stub(download_netlogo, "system", NULL)
  nlversion <- "5.3.1"
  nlpath <- tempdir()
  os <- NA

  testthat::expect_null(download_netlogo(to = nlpath,
                                         os = os,
                                         version = nlversion,
                                         extract = TRUE))

  ## corrupt version
  testthat::expect_error(download_netlogo(to = nlpath,
                                         os = os,
                                         version = "1",
                                         extract = TRUE))

  ## corrupt os
  testthat::expect_error(download_netlogo(to = nlpath,
                                          os = "unknown",
                                          version = nlversion,
                                          extract = TRUE))

  ## non-matching arch
  testthat::expect_error(download_netlogo(to = nlpath,
                                          os = "mac",
                                          version = "7.0.0",
                                          arch = "64",
                                          extract = TRUE))
  testthat::expect_error(download_netlogo(to = nlpath,
                                          os = "win",
                                          version = "6.4.0",
                                          arch = "fail",
                                          extract = TRUE))
  testthat::expect_error(download_netlogo(to = nlpath,
                                          os = "unix",
                                          version = "6.4.0",
                                          arch = "fail",
                                          extract = TRUE))


})
