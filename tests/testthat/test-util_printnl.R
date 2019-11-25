testthat::context("Util print")
testthat::test_that("Util print", {

  testthat::context("Print nl object")
  testthat::expect_output(print(nl_lhs))
})
