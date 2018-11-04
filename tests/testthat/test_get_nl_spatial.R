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
                              metrics.turtles = c("who", "breed",
                                                  "pxcor", "pycor",
                                                  "xcor", "ycor"),
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

  ## Attach results to nl:
  setsim(nl, "simoutput") <- results

  testthat::context("Get spatial data with turtles px: raster/sf")
  results.spatial <- get_nl_spatial(nl, turtles = TRUE, patches=TRUE,
                                    turtle_coords = "px", format = "spatial")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$patches[[1]])[1], "RasterLayer")
  testthat::expect_match(class(results.spatial$turtles[[1]])[1], "sf")

  testthat::context("Get spatial data with turtles px: tibble")
  results.spatial <- get_nl_spatial(nl, turtles = TRUE, patches=TRUE,
                                    turtle_coords = "px", format = "tibble")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_false(is.null(results.spatial$patches_x))
  testthat::expect_false(is.null(results.spatial$patches_y))
  testthat::expect_false(is.null(results.spatial$turtles_x))
  testthat::expect_false(is.null(results.spatial$turtles_y))

  testthat::context("Get spatial data with turtles x: raster/sf")
  results.spatial <- get_nl_spatial(nl, turtles = TRUE, patches=TRUE,
                                    turtle_coords = "x", format = "spatial")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$patches[[1]])[1], "RasterLayer")
  testthat::expect_match(class(results.spatial$turtles[[1]])[1], "sf")

  testthat::context("Get spatial data with turtles x: tibble")
  results.spatial <- get_nl_spatial(nl, turtles = TRUE, patches=TRUE,
                                    turtle_coords = "x", format = "tibble")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_false(is.null(results.spatial$patches_x))
  testthat::expect_false(is.null(results.spatial$patches_y))
  testthat::expect_false(is.null(results.spatial$turtles_x))
  testthat::expect_false(is.null(results.spatial$turtles_y))

  testthat::context("Get spatial data without turtles: raster")
  results.spatial <- get_nl_spatial(nl, turtles = FALSE, patches=TRUE,
                                    format = "spatial")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_match(class(results.spatial$patches[[1]])[1], "RasterLayer")
  testthat::expect_false(exists("results.spatial$turtles"))

  testthat::context("Get spatial data without turtles: tibble")

  results.spatial <- get_nl_spatial(nl, turtles = FALSE, patches=TRUE,
                                    format = "tibble")
  testthat::expect_match(class(results.spatial)[1], "tbl_df")
  testthat::expect_false(is.null(results.spatial$patches_x))
  testthat::expect_false(is.null(results.spatial$patches_y))
  testthat::expect_false(exists("results.spatial$turtles_x"))
  testthat::expect_false(exists("results.spatial$turtles_y"))

  })
