testthat::context("Exe3: run_nl_dyn abc tests")
testthat::test_that("run_nl_dyn abc", {

  ## The execution path needs a NetLogo installation (NETLOGO_HOME):
  nlpath <- skip_without_netlogo()
  ## Now we check if we can run a simple simulation:
  ## Step1: Create a nl obejct:
  modelpath <- file.path(nlpath, "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogox")
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname="wolf-sheep",
                              outpath=outpath,
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
                                               "model-version" = "sheep-wolves",
                                               "show-energy?" = FALSE))

  testthat::context("Run optimization with run_nl_dyn() abcmcmc. This takes a while and cannot be shortened, as ABCmcmc needs burn-in period to return valid results.")
  nl@simdesign <- simdesign_ABCmcmc_Marjoram(nl = nl,
                                             summary_stat_target = c(1),
                                             n_rec = 2,
                                             n_cluster = 1,
                                             use_seed = FALSE,
                                             progress_bar = TRUE,
                                             n_calibration = 101, #102
                                             nseeds = 1)

  #start <- Sys.time()
  results.dyn <- run_nl_dyn(nl)
  #end <- Sys.time()
  testthat::expect_match(class(results.dyn)[1], "tbl_df")
  testthat::expect_equal(nrow(results.dyn), 1)
  testthat::expect_match(class(results.dyn$result[[1]])[1], "tbl_df")
  testthat::expect_equal(length(results.dyn$result[[1]]), 8)

})
