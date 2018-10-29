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
  modelpath <- file.path(nlpath, "app", "models", "Wolf Sheep Predation.nlogo")
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


  testthat::context("Create xml files:")
  xmlfile <- file.path(nl@experiment@outpath, "nlrx_test.xml")
  seed <- nl@simdesign@simseeds[1]
  siminputrow <- 1
  util_create_sim_XML(nl, seed, siminputrow, xmlfile)
  testthat::expect_true(file.exists(xmlfile))
  xmlfileread <- XML::xmlParse(file = xmlfile)
  xmlfileread <- XML::xmlToList(xmlfileread)

  testthat::expect_equal(xmlfileread$experiment$setup, "setup")
  testthat::expect_equal(xmlfileread$experiment$go, "go")
  testthat::expect_equal(xmlfileread$experiment$timeLimit[["steps"]], "2")
  testthat::expect_equal(xmlfileread$experiment$metric, "count sheep")
  testthat::expect_true(is.character(
    xmlfileread$experiment$enumeratedValueSet$value[["value"]]))
  testthat::expect_equal(
    xmlfileread$experiment$enumeratedValueSet$.attrs[["variable"]],
    "initial-number-sheep")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["name"]], "nlrx_test")
  testthat::expect_equal(xmlfileread$experiment$.attrs[["repetitions"]], "1")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["runMetricsEveryStep"]], "true")


  testthat::context("Create batch files:")
  batchfile <- util_read_write_batch(nl)
  testthat::expect_true(file.exists(batchfile))

  testthat::context("Call NetLogo batch file:")
  outfile <- file.path(nl@experiment@outpath, "nlrx_output_1.csv")
  util_call_nl(nl, xmlfile = xmlfile, batchfile = batchfile, outfile = outfile)
  expect_true(file.exists(outfile))

  # testthat::context("Gather results:")
  # results <- util_gather_results(nl, outfile, seed, siminputrow)
  # testthat::expect_match(class(results)[1], "tbl_df")
  # testthat::expect_equal(nrow(results), 1)
  #
  # testthat::context("Run one simulation with run_nl_one()")
  # results <- run_nl_one(nl, seed, siminputrow, "all")
  # testthat::expect_match(class(results)[1], "tbl_df")
  # testthat::expect_equal(nrow(results), 1)
  #
  # testthat::context("Run all simulations with run_nl_all()")
  # results <- run_nl_all(nl, "all")
  # testthat::expect_match(class(results)[1], "tbl_df")
  # testthat::expect_equal(nrow(results), length(nl@experiment@evalticks))
  #
  # testthat::context("Cleanup:")
  # util_cleanup(nl, outpath, "all")
  # util_cleanup(nl, dirname(xmlfile), pattern = "all")
  # util_cleanup(nl, dirname(batchpath), pattern = ".bat")
  # util_cleanup(nl, dirname(batchpath), pattern = ".sh")
  # testthat::expect_false(file.exists(xmlfile))
  # testthat::expect_false(file.exists(batchfile))
  # testthat::expect_false(file.exists(outfile))


})
