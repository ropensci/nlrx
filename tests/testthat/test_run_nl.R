testthat::context("Run nl tests")
testthat::test_that("Run nl", {


  # Load a nl object from sample data:
  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "6.0.2",
           nlpath = "/home/NetLogo_6.0.2/",
           modelpath = "/home/Wolf Sheep Predation.nlogo",
           jvmmem = 1000)

  ## Step2: Add Experiment

  nl@experiment <- experiment(expname = "nlrx_simple",
                              outpath = "/home/nlrxout",
                              repetition = 1,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 50,
                              evalticks = 50,
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
                                samples=100,
                                nseeds=1,
                                precision=3)

  testthat::context("Create xml files:")
  xmlfile <-
    tempfile(pattern = "nlrx_test", fileext = ".xml")
  seed <- 1234
  siminputrow <- 1
  util_create_sim_XML(nl, seed, siminputrow, xmlfile)

  xmlfileread <- XML::xmlParse(file = xmlfile)
  xmlfileread <- XML::xmlToList(xmlfileread)

  testthat::expect_equal(xmlfileread$experiment$setup, "setup")
  testthat::expect_equal(xmlfileread$experiment$go, "go")
  testthat::expect_equal(xmlfileread$experiment$timeLimit[["steps"]], "50")
  testthat::expect_equal(xmlfileread$experiment$metric, "count sheep")
  testthat::expect_true(is.character(
    xmlfileread$experiment$enumeratedValueSet$value[["value"]]))
  testthat::expect_equal(
    xmlfileread$experiment$enumeratedValueSet$.attrs[["variable"]],
    "initial-number-sheep")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["name"]], "nlrx_simple")
  testthat::expect_equal(xmlfileread$experiment$.attrs[["repetitions"]], "1")
  testthat::expect_equal(
    xmlfileread$experiment$.attrs[["runMetricsEveryStep"]], "false")


  testthat::context("Create batch files:")





})
