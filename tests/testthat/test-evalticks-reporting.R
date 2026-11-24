testthat::context("Evalticks without results")

testthat::test_that("util_unreported_evalticks finds requested ticks without results", {

  ## The regression this guards: nlrx 0.4.x wrote
  ##   evalticks[!which(evalticks %in% steps)]
  ## which coerces which()'s indices to logical and therefore never selected
  ## anything, so the intended message could not fire.
  testthat::expect_identical(util_unreported_evalticks(c(5, 20), c(5)), 20)
  testthat::expect_identical(util_unreported_evalticks(c(5, 10), c(5, 10)), numeric(0))
  testthat::expect_identical(util_unreported_evalticks(c(5, 10), numeric(0)), c(5, 10))

  ## Duplicated and unordered result ticks do not matter:
  testthat::expect_identical(util_unreported_evalticks(c(5, 10, 15), c(10, 5, 10, 5)), 15)

  ## evalticks = NA means "every tick", so there is nothing to report:
  testthat::expect_length(util_unreported_evalticks(NA_integer_, c(1, 2)), 0)
  testthat::expect_length(util_unreported_evalticks(integer(0), c(1, 2)), 0)
})
