testthat::context("idfinal")
testthat::test_that("idfinal runs at the end of each run, not once per block", {

  nlpath <- skip_without_netlogo()

  ## idfinal appends one line per execution to a file:
  logfile <- tempfile(fileext = ".txt")
  nl <- nl(nlversion = "7.0.4", nlpath = nlpath,
           modelpath = wolf_sheep_model(nlpath), jvmmem = 1024)
  nl@experiment <- experiment(expname = "idfinal", outpath = tempdir(), tickmetrics = "false",
                              idsetup = "setup", idgo = "go", runtime = 2,
                              idfinal = paste0("file-open \"", gsub("\\\\", "/", logfile),
                                               "\" file-print ticks file-close"),
                              metrics = "count sheep",
                              variables = list("initial-number-sheep" = list(min = 50, max = 150, qfun = "qunif")),
                              constants = list("model-version" = "sheep-wolves-grass"))
  nl@simdesign <- simdesign_lhs(nl, samples = 3, nseeds = 1, precision = 0)

  results <- suppressMessages(run_nl_all(nl, block_size = 10))

  ## Three runs in one block, so three executions after the last tick:
  testthat::expect_equal(nrow(results), 3)
  testthat::expect_identical(readLines(logfile), rep("2", 3))
})
