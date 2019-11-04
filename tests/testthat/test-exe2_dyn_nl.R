testthat::context("Exe2: run_nl_dyn tests")
testthat::test_that("run_nl_dyn", {

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
  nl@experiment <- experiment(expname="wolf-sheep",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=1,
                              metrics=c("count sheep", "count wolves"),
                              variables = list("sheep-gain-from-food" = list(min=2, max=6, qfun="qunif")),
                              constants = list('initial-number-sheep' = 100,
                                               'initial-number-wolves' = 50,
                                               "wolf-gain-from-food" = 20,
                                               "grass-regrowth-time" = 30,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "model-version" = "\"sheep-wolves-grass\"",
                                               "show-energy?" = "false"))

  testthat::context("Run optimization with run_nl_dyn() GenSA")
  nl@simdesign <- simdesign_GenSA(nl,
                                  par=NULL,
                                  evalcrit=1,
                                  control=list(max.time = 1),
                                  nseeds=1)

  results.dyn <- run_nl_dyn(nl, seed=getsim(nl, "simseeds")[1])
  testthat::expect_match(class(results.dyn), "list")
  testthat::expect_equal(length(results.dyn), 4)

  testthat::context("Run optimization with run_nl_dyn() GenAlg")
  nl@simdesign <- simdesign_GenAlg(nl, popSize = 5, iters = 1,
                                   evalcrit = 1, elitism = NA,
                                   mutationChance = NA, nseeds = 1)

  results.dyn <- run_nl_dyn(nl, seed=getsim(nl, "simseeds")[1])
  testthat::expect_match(class(results.dyn), "rbga")
  testthat::expect_equal(length(results.dyn), 12)

  testthat::context("Run optimization with run_nl_dyn() abcmcmc")
  nl@simdesign <- simdesign_ABCmcmc_Marjoram(nl = nl,
                                             summary_stat_target = c(100, 80),
                                             n_rec = 10,
                                             n_cluster = 1,
                                             use_seed = FALSE,
                                             progress_bar = TRUE,
                                             n_calibration = 120,
                                             nseeds = 1)

  results.dyn <- run_nl_dyn(nl, seed=getsim(nl, "simseeds")[1])
  testthat::expect_match(class(results.dyn)[1], "tbl_df")
  testthat::expect_equal(length(results.dyn), 8)

})
