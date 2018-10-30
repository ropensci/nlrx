testthat::context("Run nl tests")
testthat::test_that("Run nl", {

  # Run these tests only on TRAVIS:
  testthat::skip_if(!identical(Sys.getenv("TRAVIS"), "true"))


  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- "/home/travis/netlogo/NetLogo 6.0.3"
  testthat::expect_true(file.exists(file.path(nlpath,
                                              "app",
                                              "netlogo-6.0.3.jar")))


  ## Now we check if we can run a simple simulation:
  ## Step1: Create a nl obejct:
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")
  nl <- nl(nlversion = "6.0.3",
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

  testthat::context("Run one simulation with run_nl_one()")
  results <- run_nl_one(nl, seed, siminputrow, "all")
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), 2)

  testthat::context("Run all simulations with run_nl_all()")
  results <- run_nl_all(nl)
  testthat::expect_match(class(results)[1], "tbl_df")
  testthat::expect_equal(nrow(results), length(nl@experiment@evalticks))



  testthat::context("Run optimization with run_nl_dyn()")
  nl@simdesign <- simdesign_GenAlg(nl, popSize = 5, iters = 1,
                                   evalcrit = 1, elitism = NA,
                                   mutationChance = NA, nseeds = 1)

  results.dyn <- run_nl_dyn(nl, seed=getsim(nl, "simseeds")[1])
  testthat::expect_match(class(results.dyn)[1], "rbga")
  testthat::expect_equal(length(results.dyn), 12)


})
