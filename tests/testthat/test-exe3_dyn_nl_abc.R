testthat::context("Exe3: run_nl_dyn abc tests")

## Both tests below drive the same nlrx code path: util_run_nl_dyn_ABCmcmc(), its
## handover of the nl object to EasyABC through a tempfile and a global option,
## the callback that runs NetLogo per parameter set, and the reshaping of
## EasyABC's return value into the eight-column non-Wegmann tibble. They differ
## only in which EasyABC sampler drives it, so the nl object is built once.
abc_nl <- function(nlpath) {
  modelpath <- file.path(nlpath, "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogox")
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  nl@experiment <- experiment(expname="wolf-sheep",
                              outpath=tempdir(),
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
  nl
}

## What both variants must return. Checking the names matters because nlrx is
## what assigns them, from getexp(nl, "variables") and getexp(nl, "metrics");
## the class and length checks cannot see a mix-up there.
expect_abc_result <- function(results.dyn, n_rec) {
  testthat::expect_match(class(results.dyn)[1], "tbl_df")
  testthat::expect_equal(nrow(results.dyn), 1)

  result <- results.dyn$result[[1]]

  ## run_nl_dyn() catches a failing optimization per seed and stores the error in
  ## this column, so the call returns either way. A length-1 try-error here is
  ## the failure mode to guard against.
  testthat::expect_false(inherits(result, "try-error"))
  testthat::expect_match(class(result)[1], "tbl_df")
  testthat::expect_equal(length(result), 8)
  testthat::expect_named(result$param[[1]], "sheep-gain-from-food")
  testthat::expect_named(result$stats[[1]], "count sheep")
  testthat::expect_equal(nrow(result$param[[1]]), n_rec)
}

## Marjoram_original takes its tolerance and proposal range as arguments rather
## than calibrating them, so it reaches the same nlrx code as the calibrated
## sampler in seconds instead of minutes. This is the variant that runs
## everywhere; the calibrated one below is gated.
testthat::test_that("run_nl_dyn abc, uncalibrated sampler", {

  ## The execution path needs a NetLogo installation (NETLOGO_HOME):
  nlpath <- skip_without_netlogo()
  nl <- abc_nl(nlpath)

  ## dist_max and proposal_range are deliberately permissive. The test asserts
  ## the shape of what nlrx returns, not that the chain converges, and a sampler
  ## that rejects everything would not record n_rec parameter sets.
  nl@simdesign <- simdesign_ABCmcmc_Marjoram_original(nl = nl,
                                                      summary_stat_target = c(1),
                                                      n_rec = 2,
                                                      n_between_sampling = 1,
                                                      dist_max = 1e6,
                                                      proposal_range = 1,
                                                      use_seed = FALSE,
                                                      progress_bar = FALSE,
                                                      nseeds = 1)

  results.dyn <- run_nl_dyn(nl)
  expect_abc_result(results.dyn, n_rec = 2)

})

## The calibrated sampler additionally exercises do.call(EasyABC::ABC_mcmc, .)
## with Marjoram's own three arguments - n_calibration, tolerance_quantile and
## proposal_phi - which nothing else checks against EasyABC's signature, and it
## invokes the NetLogo callback about a hundred times rather than twice. Both are
## platform independent, so running it once per CI run is enough.
##
## n_calibration cannot be lowered to make it faster. Below roughly this many
## calibration simulations EasyABC's Marjoram sampler degenerates - measured at
## 10, 20 and 50, it produces NAs and then dies in its own rejection loop
## ("missing value where TRUE/FALSE needed").
testthat::test_that("run_nl_dyn abc, calibrated sampler", {

  skip_if_slow_tests_disabled()

  ## The execution path needs a NetLogo installation (NETLOGO_HOME):
  nlpath <- skip_without_netlogo()
  nl <- abc_nl(nlpath)

  nl@simdesign <- simdesign_ABCmcmc_Marjoram(nl = nl,
                                             summary_stat_target = c(1),
                                             n_rec = 2,
                                             use_seed = FALSE,
                                             progress_bar = TRUE,
                                             n_calibration = 101, #102
                                             nseeds = 1)

  results.dyn <- run_nl_dyn(nl)
  expect_abc_result(results.dyn, n_rec = 2)

})
