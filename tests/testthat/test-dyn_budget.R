testthat::context("Evaluation budget and progress of dynamic simdesigns")

testthat::test_that("run_nl_dyn() reports the evaluation budget of each method", {

  nl <- nl_lhs

  ## GenSA without a stop criterion warns:
  nl@simdesign <- suppressMessages(simdesign_GenSA(nl, nseeds = 1))
  testthat::expect_warning(total <- util_report_dyn_budget(nl, nseeds = 1, nreplicates = 1),
                           regexp = "no stop criterion",
                           class = "nlrx_warning_dyn_budget")
  testthat::expect_true(is.na(total))

  ## Any stop criterion is reported instead; only max.call gives a total:
  nl@simdesign <- suppressMessages(simdesign_GenSA(nl, nseeds = 2, control = list(maxit = 20)))
  testthat::expect_message(total <- util_report_dyn_budget(nl, nseeds = 2, nreplicates = 1),
                           regexp = "20 iterations")
  testthat::expect_true(is.na(total))
  nl@simdesign <- suppressMessages(simdesign_GenSA(nl, nseeds = 1, control = list(max.call = 50)))
  testthat::expect_message(total <- util_report_dyn_budget(nl, nseeds = 1, nreplicates = 3),
                           regexp = "50 evaluations")
  testthat::expect_identical(total, 50)

  ## GenAlg: popSize x iters
  nl@simdesign <- suppressMessages(simdesign_GenAlg(nl, popSize = 20, iters = 10, nseeds = 1))
  testthat::expect_message(total <- util_report_dyn_budget(nl, nseeds = 1, nreplicates = 1),
                           regexp = "up to 200 evaluations")
  testthat::expect_equal(total, 200)

  ## ABC: calibration plus MCMC steps (no calibration for Marjoram_original)
  nl@simdesign <- suppressMessages(simdesign_ABCmcmc_Wegmann(
    nl, summary_stat_target = c(100, 80), n_rec = 50, n_between_sampling = 2,
    n_calibration = 200, tolerance_quantile = 0.05, nseeds = 1))
  testthat::expect_message(total <- util_report_dyn_budget(nl, nseeds = 1, nreplicates = 1),
                           regexp = "about 300 evaluations")
  testthat::expect_equal(total, 300)
  nl@simdesign <- suppressMessages(simdesign_ABCmcmc_Marjoram_original(
    nl, summary_stat_target = c(100, 80), n_rec = 50, n_between_sampling = 2, nseeds = 1))
  testthat::expect_message(total <- util_report_dyn_budget(nl, nseeds = 1, nreplicates = 1),
                           regexp = "about 100 evaluations")
  testthat::expect_equal(total, 100)
})

testthat::test_that("ABC simdesigns refuse a calibration that keeps fewer than two runs", {

  nl <- nl_lhs
  testthat::expect_error(
    suppressMessages(simdesign_ABCmcmc_Wegmann(nl, summary_stat_target = c(100, 80), n_rec = 10,
                                               n_calibration = 20, nseeds = 1)),
    regexp = "keeps only 1 of 20",
    class = "nlrx_error_abc_calibration"
  )
  testthat::expect_error(
    suppressMessages(simdesign_ABCmcmc_Marjoram(nl, summary_stat_target = c(100, 80), n_rec = 10,
                                                n_calibration = 50, tolerance_quantile = 0.01, nseeds = 1)),
    class = "nlrx_error_abc_calibration"
  )
  testthat::expect_silent(util_eval_abc_calibration(n_calibration = 200, tolerance_quantile = 0.01))
})

testthat::test_that("evaluations hold back the run messages and advance the progress", {

  nl <- nl_lhs
  testthat::local_mocked_bindings(run_nl_one = function(...) {
    message("Model running via: experiment.xml")
    tibble::tibble(x = 1)
  })

  .nlrx_dyn$progress <- cli::cli_progress_bar(total = 10, .auto_close = FALSE)
  .nlrx_dyn$evaluations <- 0
  .nlrx_dyn$total <- 10
  on.exit({ cli::cli_progress_done(id = .nlrx_dyn$progress); .nlrx_dyn$progress <- NULL }, add = TRUE)

  testthat::expect_silent(util_run_nl_dyn_evaluation(nl, seed = 1))
  testthat::expect_silent(util_run_nl_dyn_evaluation(nl, seed = 1))
  testthat::expect_identical(.nlrx_dyn$evaluations, 2)

  ## A failing run shows its messages together with the error:
  testthat::local_mocked_bindings(run_nl_one = function(...) {
    message("Model running via: experiment.xml")
    stop("NetLogo produced an error")
  })
  testthat::expect_message(
    testthat::expect_error(util_run_nl_dyn_evaluation(nl, seed = 1), "NetLogo produced an error"),
    "experiment.xml"
  )
})
