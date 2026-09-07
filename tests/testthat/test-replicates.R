testthat::context("Replicated model runs")

testthat::test_that("replicate seeds are derived deterministically from the design seed", {

  seeds_a <- nlrx:::util_generate_replicate_seeds(42, 3)
  seeds_b <- nlrx:::util_generate_replicate_seeds(42, 3)

  testthat::expect_length(seeds_a, 3)
  testthat::expect_identical(seeds_a, seeds_b)
  testthat::expect_equal(length(unique(seeds_a)), 3)

  ## A different design seed gives different replicate seeds:
  testthat::expect_false(isTRUE(all.equal(seeds_a,
                                          nlrx:::util_generate_replicate_seeds(43, 3))))

  ## The seeds of a smaller nreplicates are a prefix of those of a larger one,
  ## so that raising nreplicates extends the set instead of replacing it:
  testthat::expect_identical(seeds_a, nlrx:::util_generate_replicate_seeds(42, 5)[1:3])
})

testthat::test_that("deriving replicate seeds does not disturb the RNG stream", {

  ## The dynamic designs (genalg, EasyABC) draw from the global RNG stream,
  ## so deriving replicate seeds must leave it untouched:
  set.seed(1)
  state_before <- .Random.seed
  invisible(nlrx:::util_generate_replicate_seeds(42, 5))
  testthat::expect_identical(state_before, .Random.seed)

  ## The stream continues where it was, independent of the derivation:
  set.seed(1)
  expected <- runif(3)
  set.seed(1)
  invisible(nlrx:::util_generate_replicate_seeds(42, 5))
  testthat::expect_identical(expected, runif(3))
})

testthat::test_that("a single replicate or a missing seed is passed through unchanged", {

  testthat::expect_identical(nlrx:::util_generate_replicate_seeds(42, 1), 42)
  testthat::expect_identical(nlrx:::util_generate_replicate_seeds(NA, 3), NA)
})

testthat::test_that("replicated results are reduced over ticks and then over replicates", {

  ## Two replicates of unequal length: replicate 1 reports three ticks of 0,
  ## replicate 2 a single tick of 12. Both replicates count the same,
  ## so the result is mean(0, 12) = 6 and not the mean over all rows (3):
  results <- tibble::tibble(`random-seed` = c(1, 1, 1, 2),
                            step = c(1, 2, 3, 1),
                            "count sheep" = c(0, 0, 0, 12))

  reduced <- nlrx:::util_reduce_replicates(results, "count sheep")

  testthat::expect_equal(nrow(reduced), 1)
  testthat::expect_equal(reduced[["count sheep"]], 6)
  testthat::expect_false(reduced[["count sheep"]] == mean(results[["count sheep"]]))
})

testthat::test_that("reduction of several metrics keeps names and order", {

  results <- tibble::tibble(`random-seed` = c(1, 1, 2, 2),
                            "count sheep" = c(10, 20, 30, 40),
                            "count wolves" = c(1, 1, 3, 3))

  reduced <- nlrx:::util_reduce_replicates(results, c("count sheep", "count wolves"))

  testthat::expect_identical(names(reduced), c("count sheep", "count wolves"))
  testthat::expect_equal(reduced[["count sheep"]], 25)
  testthat::expect_equal(reduced[["count wolves"]], 2)
})

testthat::test_that("results without replicate seeds are reduced over ticks only", {

  results <- tibble::tibble(step = c(1, 2), "count sheep" = c(1, 3))
  reduced <- nlrx:::util_reduce_replicates(results, "count sheep")

  testthat::expect_equal(reduced[["count sheep"]], 2)
})

testthat::test_that("run_nl_dyn rejects invalid nreplicates", {

  nl <- nl_gensa

  testthat::expect_error(run_nl_dyn(nl, seed = 1, nreplicates = 0), "positive integer")
  testthat::expect_error(run_nl_dyn(nl, seed = 1, nreplicates = 2.5), "positive integer")
  testthat::expect_error(run_nl_dyn(nl, seed = 1, nreplicates = c(2, 3)), "positive integer")
  testthat::expect_error(run_nl_dyn(nl, seed = 1, nreplicates = NA), "positive integer")
})

testthat::test_that("the removed repetition argument is handled", {

  ## repetition = 1 had no effect and is only deprecated, so that existing
  ## scripts and published examples keep working:
  testthat::expect_warning(experiment(expname = "test", repetition = 1), "deprecated")

  ## repetition > 1 changed the results and is therefore an error:
  testthat::expect_error(experiment(expname = "test", repetition = 3), "no longer supported")

  ## The slot is gone:
  testthat::expect_false("repetition" %in% methods::slotNames("experiment"))

  ## Unrelated unknown arguments still error:
  testthat::expect_error(experiment(expname = "test", nonsense = 1), "Unsupported")
})
