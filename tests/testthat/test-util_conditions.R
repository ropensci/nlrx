testthat::context("Advice-style conditions")
testthat::test_that("util_abort and util_warn signal classed conditions without a call", {

  err <- tryCatch(util_abort(c("Problem {1 + 1}.", "i" = "Context.", ">" = "Fix."),
                             class = "nlrx_error_test"),
                  error = identity)
  testthat::expect_s3_class(err, c("nlrx_error_test", "nlrx_error", "error"))
  testthat::expect_null(conditionCall(err))
  testthat::expect_match(conditionMessage(err), "Problem 2.")
  testthat::expect_match(conditionMessage(err), "Context.")
  testthat::expect_match(conditionMessage(err), "Fix.")

  wrn <- tryCatch(util_warn("Careful.", class = "nlrx_warning_test"),
                  warning = identity)
  testthat::expect_s3_class(wrn, c("nlrx_warning_test", "nlrx_warning", "warning"))
  testthat::expect_null(conditionCall(wrn))
})

testthat::test_that("NetLogo 6 model files are rejected with advice", {

  ## Empty and NetLogo 7 model paths pass:
  testthat::expect_true(util_check_model_format(character()))
  testthat::expect_true(util_check_model_format(c("a.nlogox", "b.nls")))

  testthat::expect_error(util_check_model_format(c("a.nlogox", "dir/Model.nlogo")),
                         regexp = "Model\\.nlogo.*transition",
                         class = "nlrx_error_model_format")

  ## nl() rejects the model directly:
  testthat::expect_error(nl(nlversion = "7.0.4", nlpath = tempdir(),
                            modelpath = "Model.nlogo"),
                         class = "nlrx_error_model_format")

  ## A modelpath changed after nl() is caught before parsing or running:
  nlogo6 <- tempfile(fileext = ".nlogo")
  writeLines("to setup end", nlogo6)
  nl <- nl(nlversion = "7.0.4", nlpath = tempdir())
  nl@modelpath <- nlogo6
  testthat::expect_error(report_model_parameters(nl),
                         class = "nlrx_error_model_format")
  testthat::expect_error(run_nl_block(nl, block_df = data.frame(),
                                      block_number = 1, threads = 1),
                         class = "nlrx_error_model_format")

  ## nldoc() gives the same advice:
  testthat::expect_error(nldoc(modelfiles = nlogo6, outpath = tempdir()),
                         class = "nlrx_error_model_format")
})
