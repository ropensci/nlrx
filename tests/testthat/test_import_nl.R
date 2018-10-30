testthat::context("export_nl tests")
testthat::test_that("export_nl", {

  # Run these tests only on TRAVIS:
  testthat::skip_if(!identical(Sys.getenv("TRAVIS"), "true"))

  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- "/home/travis/netlogo/NetLogo 6.0.3"
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "app",
                                              "netlogo-6.0.3.jar")))

  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")

  nl <- nl(nlversion = "6.0.3",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 2,
                              evalticks = c(1,2),
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif"),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif")),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=1,
                                nseeds=1,
                                precision=3)

  ## Store the nl object and the model folder as zip file:
  path <- dirname(nl@modelpath)
  tarfile <- tempfile(fileext = ".tar.gz")
  export_nl(nl, path=path, tarfile = tarfile)

  testthat::expect_true(file.exists(tarfile))

  ## Now read the zip file again:
  targetdir <- tempdir()
  import_nl(tarfile = tarfile, targetdir = targetdir, new_session = FALSE)

  nlobjectfile <- file.path(targetdir, basename(path), "nlobject.rds")
  testthat::expect_true(file.exists(nlobjectfile))

})
