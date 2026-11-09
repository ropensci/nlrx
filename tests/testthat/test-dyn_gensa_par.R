testthat::context("GenSA start values")

testthat::test_that("simdesign_GenSA validates the optional start values", {

  nl <- nl_lhs  # two variables, both bounded 50-150

  ## A valid start point is stored as given:
  nl@simdesign <- simdesign_GenSA(nl = nl, par = c(60, 140), evalcrit = 1, nseeds = 1)
  testthat::expect_identical(getsim(nl, "simobject")$par, c(60, 140))

  ## NULL is the documented default: GenSA draws its own start point.
  nl_null <- nl_lhs
  nl_null@simdesign <- simdesign_GenSA(nl = nl_null, par = NULL, evalcrit = 1, nseeds = 1)
  testthat::expect_null(getsim(nl_null, "simobject")$par)

  ## One value per variable, in the order of the variables:
  testthat::expect_error(simdesign_GenSA(nl = nl_lhs, par = c(60), evalcrit = 1),
                         regexp = "one start value per variable")
  testthat::expect_error(simdesign_GenSA(nl = nl_lhs, par = c("60", "140"), evalcrit = 1),
                         regexp = "numeric vector")

  ## Start values have to lie within the boundaries that are searched:
  testthat::expect_error(simdesign_GenSA(nl = nl_lhs, par = c(60, 500), evalcrit = 1),
                         regexp = "initial-number-wolves")
})

testthat::test_that("the start values reach GenSA", {

  nl <- nl_lhs
  nl@simdesign <- simdesign_GenSA(nl = nl, par = c(60, 140), evalcrit = 1, nseeds = 1)

  ## Mock GenSA so that the optimization is not actually run: the point of this
  ## test is that `par` is handed over instead of the hardcoded NULL it used to be.
  testthat::local_mocked_bindings(
    GenSA = function(par, ...) list(par = par),
    .package = "GenSA"
  )

  result <- util_run_nl_dyn_GenSA(nl = nl, seed = getsim(nl, "simseeds")[1])

  testthat::expect_identical(result$par, c(60, 140))
})
