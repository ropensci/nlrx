testthat::test_that("deprecated args trigger warnings", {
  testthat::expect_warning(
    util_check_deprecated_args(list(split = 5), "split"),
    "deprecated"
  )
})

testthat::test_that("unsupported args trigger errors", {
  testthat::expect_error(
    util_check_deprecated_args(list(unknown = TRUE), "split"),
    "Unsupported"
  )
})

testthat::test_that("empty input of `dots` passes silently", {
  testthat::expect_silent(
    util_check_deprecated_args(list(), "split")
  )
})
