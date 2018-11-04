
testthat::context("Download NetLogo")
testthat::test_that("Download NetLogo", {

  # Run these tests only on TRAVIS:
  testthat::skip_if(!identical(Sys.getenv("TRAVIS"), "true"))

  ## Version 6.0.0
  nlversion <- "6.0.0"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ### Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.0",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))
  ## Version 6.0.1
  nlversion <- "6.0.1"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ### Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.1",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))
  ## Version 6.0.2
  nlversion <- "6.0.2"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ### Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.2",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))
  ## Version 6.0.3
  nlversion <- "6.0.3"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ### Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.3",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))

  ## Version 6.0.4
  nlversion <- "6.0.4"
  nlpath <- tempdir()
  download_netlogo(to = nlpath,
                   version = nlversion,
                   extract = TRUE)

  ### Check that download worked and netlogo jar file exists:
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "NetLogo 6.0.4",
                                              "app",
                                              paste0("netlogo-",
                                                     nlversion,
                                                     ".jar"))))

})
