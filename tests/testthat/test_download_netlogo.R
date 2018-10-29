
testthat::context("Download NetLogo")
testthat::test_that("Download NetLogo", {

  # Run these tests only on TRAVIS:
  testthat::skip_if(!identical(Sys.getenv("TRAVIS"), "true"))

  nlversion <- "6.0.3"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ## Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.3",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))

})
