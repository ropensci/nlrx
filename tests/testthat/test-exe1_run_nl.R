testthat::context("Run nl tests")
testthat::test_that("Run nl", {

  # Run these tests only on Github actions:
  testthat::skip_if(!identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1",
                   ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1",
                          ifelse(nlrx:::util_get_os() == "mac","/Applications/NetLogo 6.1.1",
                                 "FAILED")))

  testthat::expect_true(file.exists(file.path(nlpath,
                                              "app",
                                              "netlogo-6.1.1.jar")))


  ## Now we check if we can run a simple simulation:
  ## Step1: Create a nl obejct:
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")
  nl <- nl(nlversion = "6.1.1",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  ## Step2: Add Experiment
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



  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1

  testthat::test_that ("Before run_nl_one", {
    testthat::expect_message(f(), "^Message: Before run_nl_one\\n")
  })


  testthat::context("Run one simulation with run_nl_one()")
  results <- run_nl_one(nl, seed, siminputrow)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), 2)

  testthat::test_that ("After run_nl_one", {
    testthat::expect_message(f(), "^Message: After run_nl_one\\n")
  })

  testthat::context("Run all simulations with run_nl_all()")
  results <- run_nl_all(nl)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), length(nl@experiment@evalticks))

  testthat::test_that ("After run_nl_all", {
    testthat::expect_message(f(), "^Message: After run_nl_all\\n")
  })

  testthat::context("Run all simulations with run_nl_all() and wrong split parameter")
  testthat::expect_error(run_nl_all(nl, split=4))

  ## Step3: Test tickmetrics = false
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "false",
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



  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1

  testthat::context("Run one simulation with run_nl_one()
                    and tickmetrics false")
  results <- run_nl_one(nl, seed, siminputrow)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), 1)

  ## Step4: Test NLtable == 0
  ## Step3: Test tickmetrics = false
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 15,
                              evalticks = c(15),
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' =
                                                 list(min=1, max=1,
                                                      step=1, qfun="qunif"),
                                               'initial-number-wolves' =
                                                 list(min=400, max=500,
                                                      step=10, qfun="qunif")),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 1,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=1,
                                nseeds=1,
                                precision=3)



  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1

  testthat::context("Run one simulation with run_nl_one()
                    and tickmetrics false")
  results <- run_nl_one(nl, seed, siminputrow)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), 1)

})
