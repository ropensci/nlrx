testthat::context("Class setter and getter")
testthat::test_that("Class setter and getter", {


  nl <- nl(nlversion = "6.0.3",
           nlpath = "test",
           modelpath = "test",
           jvmmem = 1024)

  ## Check setnl and getnl:
  setnl(nl, "nlversion") <- "6.0.4"

  testthat::expect_match(getnl(nl, "nlversion"), "6.0.4")

  ## Add Experiment
  nl@experiment <- experiment(expname = "nlrx_test",
                              outpath = "out",
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 2,
                              evalticks = 1:2,
                              metrics = c("count sheep","count wolves"),
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
                              metrics.turtles = list("turtles" =
                                                     c("who", "breed", "pxcor",
                                                       "pycor", "xcor", "ycor")),
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

  ## Check setexp and getexp:
  setexp(nl, "expname") <- "newexpname"

  testthat::expect_match(getexp(nl, "expname"), "newexpname")

  ## Add simdesign
  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=10,
                                nseeds=1,
                                precision=3)

  ## Check setsim and getsim:
  setsim(nl, "precision") <- 1

  testthat::expect_equal(getsim(nl, "precision"), 1)

})
