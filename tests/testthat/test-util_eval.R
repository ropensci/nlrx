testthat::context("util_eval")
testthat::test_that("util_eval", {

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
  ## Step1: Create a nl object:
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")

   nl <- nl(nlversion = "6.0.3",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)


  ## Without proper experiment, this should throw an error:
  testthat::expect_error(util_eval_variables(nl))
  testthat::expect_error(util_eval_constants(nl))
  testthat::expect_error(util_eval_experiment(nl))

  outpath <- tempdir()

  ## Add an experiment with incomplete variables:
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
                              variables = list('number-sheep' =
                                                 list(min=50,
                                                      values=c(1, 2, 3)),
                                               'number-wolves' =
                                                 list(qfun="qunif")),
                              constants = list("versionzzzz" =
                                                 "\"sheep-wolves-grass\""))

  ## Without proper constants, this should throw an error:
  testthat::expect_error(eval_variables_constants(nl))

  ## Add an experiment with incomplete variables:
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
                              variables = list('number-sheep' =
                                                 list(min=50,
                                                      values=c(1, 2, 3)),
                                               'number-wolves' =
                                                 list(qfun="qunif")),
                              constants = list("version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))


  testthat::expect_error(util_eval_variables_distinct(nl))
  testthat::expect_error(util_eval_variables_ff(nl))
  testthat::expect_error(util_eval_variables_sa(nl))
  testthat::expect_error(util_eval_variables_op(nl))
  testthat::expect_error(eval_variables_constants(nl))

  ## Add an experiment with different number of values and distinct
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "fail",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 2,
                              evalticks = c(1,2),
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' =
                                                 list(min=50,
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(qfun="qunif",
                                                      values=c(2,3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  testthat::expect_error(util_eval_variables_distinct(nl))
  testthat::expect_error(util_eval_experiment(nl))

  ## Add an experiment with whitespace in expname:
  nl@experiment <- experiment(expname = "nlrx test",
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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))
  testthat::expect_error(util_eval_experiment(nl))


  ## Add an experiment with same parameter in variables and constants:
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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "initial-number-sheep" = 10,
                                               "show-energy?" = "false"))
  testthat::expect_error(util_eval_experiment(nl))
  testthat::expect_error(eval_variables_constants(nl))

  ## Add non existing constant parameter:
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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false",
                                               "this-param-does-not-exist" = 0))
  testthat::expect_error(eval_variables_constants(nl))

  ## Add non existing variable parameter:
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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               "this-param-does-not-exist" =
                                                 list(min=0, max=1, step=1, qfun="qunif",
                                                      value=c(1, 2, 3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))
  testthat::expect_error(eval_variables_constants(nl))

  ## Leave some parameters non defined:
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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3))),
                              constants = list())
  testthat::expect_warning(eval_variables_constants(nl))

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
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3)),
                                               'initial-number-wolves' =
                                                 list(min=50, max=150,
                                                      step=10, qfun="qunif",
                                                      values=c(1, 2, 3))),
                              constants = list("model-version" =
                                                 "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))


  ## Check variable definitions:
  testthat::expect_true(is.null(util_eval_variables(nl)))
  testthat::expect_true(is.null(util_eval_constants(nl)))
  testthat::expect_true(is.null(util_eval_experiment(nl)))
  testthat::expect_message(eval_variables_constants(nl), "All defined variables and constants are valid!")


  ## Check specific combinations for simdesigns:
  testthat::expect_true(is.null(util_eval_variables_distinct(nl)))
  testthat::expect_true(is.null(util_eval_variables_ff(nl)))
  testthat::expect_true(is.null(util_eval_variables_sa(nl)))
  testthat::expect_true(is.null(util_eval_variables_op(nl)))


  ## Create a simdesign:
  testthat::expect_error(util_eval_simdesign(nl))

  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=1,
                                nseeds=1,
                                precision=3)

  testthat::expect_true(is.null(util_eval_simdesign(nl)))


  ## Test eval_simoutput() with empty simoutput
  nl <- nl()
  testthat::expect_error(eval_simoutput(nl))

  ## Test eval_simoutput() with results
  nl <- nl_lhs
  testthat::expect_equal(nrow(eval_simoutput(nl)), 0)

  ## Test eval_simoutput() with results and removed data
  nl <- nl_lhs
  setsim(nl, "simoutput") <- nl@simdesign@simoutput[-1,]
  testthat::expect_equal(nrow(eval_simoutput(nl)), 1)

  ## Test eval_simoutput() with gensa data
  nl <- nl_gensa
  testthat::expect_error(eval_simoutput(nl))


})
