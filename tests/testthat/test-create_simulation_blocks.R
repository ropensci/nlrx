testthat::test_that("creates correct number of blocks", {
  nl <- nl_lhs  # Already has 100 siminput rows

  # Test block_size = 10 → 10 blocks
  blocks <- create_simulation_blocks(nl, block_size = 10)
  testthat::expect_length(blocks, 10)
  testthat::expect_equal(nrow(blocks[[1]]), 10)
})

testthat::test_that("handles last block with remainder", {
  nl <- nl_lhs

  # Limit to 95 rows
  nl@simdesign@siminput <- nl@simdesign@siminput[1:95, ]

  blocks <- create_simulation_blocks(nl, block_size = 10)
  testthat::expect_length(blocks, 10) # 9 full + 1 partial
  testthat::expect_equal(nrow(blocks[[10]]), 5) # Last block has 5
})

testthat::test_that("block_size > nrow creates single block", {
  nl <- nl_lhs

  # Limit to 10 rows
  nl@simdesign@siminput <- nl@simdesign@siminput[1:10, ]

  blocks <- create_simulation_blocks(nl, block_size = 100)
  testthat::expect_length(blocks, 1)
  testthat::expect_equal(nrow(blocks[[1]]), 10)
})
