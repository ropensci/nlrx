testthat::context("Simdesign Helper")
testthat::test_that("Simdesign objects are created properly", {

  nl <- nl(
    nlversion = "6.0.3",
    nlpath = "/home/travis/netlogo/NetLogo 6.0.3",
    modelpath = "/home/travis/netlogo/NetLogo 6.0.3/app/models/Wolf Sheep Predation.nlogo",
    jvmmem = 1024
  )

  nl@experiment <- experiment(
    expname = "nlrxtest",
    outpath = "/home/out",
    repetition = 1,
    tickmetrics = "true",
    idsetup = "setup",
    idgo = "go",
    idfinal = NA_character_,
    runtime = 10,
    evalticks = seq(1, 10),
    metrics = c("count sheep", "count wolves"),
    variables = list(
      "initial-number-sheep" = list(min = 50, max = 150, step = 10,
                                    qfun = "qunif"),
      "initial-number-wolves" = list(min = 50, max = 150, step = 10,
                                     qfun = "qunif")
    ),
    constants = list(
      "model-version" = "\"sheep-wolves-grass\"",
      "grass-regrowth-time" = 30,
      "sheep-gain-from-food" = 4,
      "wolf-gain-from-food" = 20,
      "sheep-reproduce" = 4,
      "wolf-reproduce" = 5,
      "show-energy?" = "false"
    )
  )

  # Skill all following tests on CRAN and travis
  testthat::context("Class construction: simdesign")
  # Testing validity of simdesign simple:
  nl@simdesign <- simdesign_simple(
    nl = nl,
    nseeds = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "simple")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 1)
  testthat::expect_equal(length(getsim(nl, "simobject")), 0)
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)


  # Testing validity of simdesign ff:
  nl@simdesign <- simdesign_ff(
    nl = nl,
    nseeds = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "ff")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 121)
  testthat::expect_equal(length(getsim(nl, "simobject")), 0)
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)

  # Testing validity of simdesign lhs:
  nl@simdesign <- simdesign_lhs(
    nl = nl,
    samples = 10,
    nseeds = 3,
    precision = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "lhs")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 10)
  testthat::expect_equal(length(getsim(nl, "simobject")), 0)
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)


  # Testing validity of simdesign sobol:
  nl@simdesign <- simdesign_sobol(
    nl = nl,
    samples = 5,
    sobolorder = 2,
    sobolnboot = 2,
    sobolconf = 0.95,
    nseeds = 3,
    precision = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "sobol")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 20)
  testthat::expect_equal(length(getsim(nl, "simobject")), 1)
  testthat::expect_match(class(getsim(nl, "simobject")[[1]]), "sobol")
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)


  # Testing validity of simdesign sobol2007
  nl@simdesign <- simdesign_sobol2007(
    nl = nl,
    samples = 5,
    sobolnboot = 2,
    sobolconf = 0.95,
    nseeds = 3,
    precision = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "sobol2007")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 20)
  testthat::expect_equal(length(getsim(nl, "simobject")), 1)
  testthat::expect_match(class(getsim(nl, "simobject")[[1]]), "sobol2007")
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)

  # Testing validity of simdesign soboljansen:
  nl@simdesign <- simdesign_soboljansen(
    nl = nl,
    samples = 5,
    sobolnboot = 2,
    sobolconf = 0.95,
    nseeds = 3,
    precision = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "soboljansen")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 20)
  testthat::expect_equal(length(getsim(nl, "simobject")), 1)
  testthat::expect_match(class(getsim(nl, "simobject")[[1]]), "soboljansen")
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)


  # Testing validity of simdesign morris:
  nl@simdesign <- simdesign_morris(
    nl = nl,
    morristype = "oat",
    morrislevels = 4,
    morrisr = 1,
    morrisgridjump = 2,
    nseeds = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "morris")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 3)
  testthat::expect_equal(length(getsim(nl, "simobject")), 1)
  testthat::expect_match(class(getsim(nl, "simobject")[[1]]), "morris")
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)

  # Testing validity of simdesign eFast:
  nl@simdesign <- simdesign_eFast(
    nl = nl,
    samples = 70,
    nseeds = 3
  )

  testthat::expect_match(getsim(nl, "simmethod"), "eFast")
  testthat::expect_equal(nrow(getsim(nl, "siminput")), 140)
  testthat::expect_equal(length(getsim(nl, "simobject")), 1)
  testthat::expect_match(class(getsim(nl, "simobject")[[1]]), "fast99")
  testthat::expect_equal(length(getsim(nl, "simseeds")), 3)
  testthat::expect_equal(nrow(getsim(nl, "simoutput")), 0)
})
