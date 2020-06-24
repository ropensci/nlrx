testthat::context("Get nl spatial")
testthat::test_that("Get nl spatial", {

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
                              metrics.turtles = list("turtles" = c("who", "breed", "pxcor", "pycor", "xcor", "ycor")),
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
                              variables = list('initial-number-sheep' = list(min=50, max=150, step=10, qfun="qunif"),
                                               'initial-number-wolves' = list(min=50, max=150, step=10, qfun="qunif")),
                              constants = list("model-version" = "\"sheep-wolves-grass\"",
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

  results <- run_nl_all(nl)

  testthat::context("Simoutput attached")
  testthat::expect_error(
    unnest_simoutput(nl))
  testthat::expect_error(
    nl_to_raster(nl))
  testthat::expect_error(
    nl_to_points(nl))

  ## Attach results to nl:
  setsim(nl, "simoutput") <- results

  testthat::context("Unnest simoutput:")
  results.spatial <- unnest_simoutput(nl)

  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$agent), "character")
  testthat::expect_match(class(results.spatial$breed), "character")

  testthat::context("nl_to_raster")
  results.spatial <- nl_to_raster(nl)
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$spatial.raster[[1]])[1], "RasterLayer")

  testthat::context("nl_to_points pxcor")
  results.spatial <- nl_to_points(nl, coords = "px")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$spatial.turtles[[1]])[1], "sf")

  testthat::context("nl_to_points xcor")
  results.spatial <- nl_to_points(nl, coords = "x")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$spatial.turtles[[1]])[1], "sf")

  ## Check functioning without explicitly measuring breeds:
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
                              metrics.turtles = list("turtles" = c("who", "pxcor", "pycor",
                                                                   "xcor", "ycor")),
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
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

  results <- run_nl_all(nl)
  setsim(nl, "simoutput") <- results
  testthat::context("nl_to_points pxcor")
  results.spatial <- nl_to_points(nl, coords = "px")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$spatial.turtles[[1]])[1], "sf")

  ## Check error message if coordinates are missing:
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
                              metrics.turtles = list("turtles" = c("who", "breed")),
                              metrics.patches = c("pcolor"),
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

  results <- run_nl_all(nl)
  setsim(nl, "simoutput") <- results
  testthat::expect_error(nl_to_points(nl))
  testthat::expect_error(nl_to_raster(nl))


  ## Check unnest without turtle metrics:
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
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
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

  results <- run_nl_all(nl)
  setsim(nl, "simoutput") <- results
  results.spatial <- unnest_simoutput(nl)
  testthat::expect_match(class(results.spatial)[1], "tbl_df")


  ## nl_to_graph function
  testthat::context("nl_to_graph")
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Networks", "Giant Component.nlogo")
  nl <- nl(nlversion = "6.0.3",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)


  nl@experiment <- experiment(expname="networks",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=50,
                              metrics.turtles = list("turtles" = c("who", "color")),
                              metrics.links = list("links" = c("[who] of end1", "[who] of end2")),
                              constants = list("num-nodes" = 80,
                                               "layout?" = "true"))

  nl@simdesign <- simdesign_simple(nl, 1)

  testthat::context("Simoutput attached")
  testthat::expect_error(
    nl_to_graph(nl))

  nl@simdesign@simoutput <- run_nl_all(nl)
  results.spatial <- nl_to_graph(nl)

  testthat::context("nl_to_graph")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$spatial.links[[1]])[1], "igraph")

  ## Check missing who numbers:
  nl@experiment <- experiment(expname="networks",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=50,
                              metrics.turtles = list("turtles" = c("who", "color")),
                              metrics.links = list("links" = c("[who] of end1")),
                              constants = list("num-nodes" = 80,
                                               "layout?" = "true"))

  nl@simdesign <- simdesign_simple(nl, 1)
  nl@simdesign@simoutput <- run_nl_all(nl)
  testthat::context("Missing [who] of end 2")
  testthat::expect_error(nl_to_graph(nl))

  nl@experiment <- experiment(expname="networks",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=50,
                              metrics.turtles = list("turtles" = c("who", "color")),
                              metrics.links = list("links" = c("[who] of end2")),
                              constants = list("num-nodes" = 80,
                                               "layout?" = "true"))

  nl@simdesign <- simdesign_simple(nl, 1)
  nl@simdesign@simoutput <- run_nl_all(nl)
  testthat::context("Missing [who] of end 1")
  testthat::expect_error(nl_to_graph(nl))

  nl@experiment <- experiment(expname="networks",
                              outpath=outpath,
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=50,
                              metrics.turtles = list("turtles" = c("color")),
                              metrics.links = list("links" = c("[who] of end1", "[who] of end2")),
                              constants = list("num-nodes" = 80,
                                               "layout?" = "true"))

  nl@simdesign <- simdesign_simple(nl, 1)
  nl@simdesign@simoutput <- run_nl_all(nl)
  testthat::context("Missing [who] of end 1")
  testthat::expect_error(nl_to_graph(nl))

})

