testthat::context("Util stuff")
testthat::test_that("Util stuff", {


  testthat::context("Test util_generate_seeds()")

  n <- 50
  seeds1 <- util_generate_seeds(n)
  seeds2 <- util_generate_seeds(n)

  testthat::expect_equal(length(seeds1), n)
  testthat::expect_equal(length(seeds2), n)
  testthat::expect_false(isTRUE(all.equal(seeds1, seeds2)))



  testthat::context("Test util_create_lhs()")

  input <- list("initial-number-sheep" = list(min = 50, max = 150, step = 10,
                                              qfun = "qunif"),
                "initial-number-wolves" = list(min = 50, max = 150, step = 10,
                                               qfun = "qunif"))
  samples <- 100
  precision <- 3

  lhs <- util_create_lhs(input, samples, precision)

  testthat::expect_equal(names(input), names(lhs))
  testthat::expect_equal(nrow(lhs), samples)
  testthat::expect_equal(ncol(lhs), length(input))
  testthat::expect_true(max(sapply(lhs[[1]], FUN=function(x)
    nchar(strsplit(as.character(x), "\\.")[[1]][2])), na.rm=TRUE) <=
                         precision)
  testthat::expect_true(max(sapply(lhs[[2]], FUN=function(x)
    nchar(strsplit(as.character(x), "\\.")[[1]][2])), na.rm=TRUE) <=
    precision)


  testthat::context("Test util_get_os()")
  os <- util_get_os()

  testthat::expect_match(class(os), "character")


})
