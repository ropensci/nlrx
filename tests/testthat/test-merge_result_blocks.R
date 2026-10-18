testthat::test_that("merges multiple blocks correctly", {
  # Create mock result blocks
  block1 <- data.frame(step = 1:5, value = 1:5, siminputrow = 1)
  block2 <- data.frame(step = 1:5, value = 6:10, siminputrow = 2)

  nl <- nl_lhs # nl might be removed in the future, as its only used in merge_result_blocks to restore the column names which logolink automatically renames
  results <- merge_result_blocks(nl, list(block1, block2))

  testthat::expect_equal(nrow(results), 10)
  testthat::expect_true("siminputrow" %in% colnames(results))
})

testthat::test_that("single block passes through", {
  block1 <- data.frame(step = 1:5, value = 1:5)

  nl <- nl_lhs
  results <- merge_result_blocks(nl, list(block1))

  testthat::expect_equal(nrow(results), 5)
})
