testthat::context("Exe3: run_nl_dyn abc tests")
testthat::test_that("run_nl_dyn abc", {

  # Run these tests only on Github actions:
  testthat::skip_if(!identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1",
                   ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1",
                          ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1",
                                 "FAILED")))

  testthat::expect_true(nlpath != "FAILED")
  testthat::expect_true(dir.exists(nlpath))

  jarpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                    ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                           ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                                  "FAILED")))


  testthat::expect_true(jarpath != "FAILED")
  testthat::expect_true(file.exists(jarpath))


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
  nl@experiment <- experiment(expname="wolf-sheep",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=1,
                              metrics=c("count sheep"),
                              variables = list("sheep-gain-from-food" = list(min=2, max=6, qfun="qunif")),
                              constants = list('initial-number-sheep' = 1,
                                               'initial-number-wolves' = 1,
                                               "wolf-gain-from-food" = 20,
                                               "grass-regrowth-time" = 30,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "model-version" = "\"sheep-wolves\"",
                                               "show-energy?" = "false"))

  testthat::context("Run optimization with run_nl_dyn() abcmcmc")
  nl@simdesign <- simdesign_ABCmcmc_Marjoram(nl = nl,
                                             summary_stat_target = c(1),
                                             n_rec = 2,
                                             n_cluster = 1,
                                             use_seed = FALSE,
                                             progress_bar = TRUE,
                                             n_calibration = 101, #102
                                             nseeds = 1)

  #start <- Sys.time()
  results.dyn <- run_nl_dyn(nl, seed=getsim(nl, "simseeds")[1])
  #end <- Sys.time()
  testthat::expect_match(class(results.dyn)[1], "tbl_df")
  testthat::expect_equal(length(results.dyn), 8)

})
