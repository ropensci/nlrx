testthat::context("Issue follow-ups")

testthat::test_that("util_collapse_commands turns slot vectors into one code block", {

  ## The regression this guards: run_nl_block() used `if (!is.na(slot))`, which
  ## errors with "the condition has length > 1" as soon as a slot holds a
  ## vector - although idsetup/idgo/idfinal are documented as accepting one.
  testthat::expect_identical(util_collapse_commands("setup"), "setup")
  testthat::expect_identical(
    util_collapse_commands(c("setup", "ask turtles [ set shape \"wolf\" ]")),
    "setup\nask turtles [ set shape \"wolf\" ]"
  )

  ## Undefined slots become NULL, which is what logolink expects:
  testthat::expect_null(util_collapse_commands(NA_character_))
  testthat::expect_null(util_collapse_commands(character(0)))

  ## NA entries within a vector are dropped rather than written out:
  testthat::expect_identical(util_collapse_commands(c("setup", NA)), "setup")
})

testthat::test_that("experiment() warns about evalticks beyond the runtime", {

  make <- function(evalticks, runtime) {
    experiment(expname = "ticks", outpath = tempdir(), idsetup = "setup",
               idgo = "go", runtime = runtime, evalticks = evalticks,
               metrics = "count turtles")
  }

  ## Issue #111: a tick the run cannot reach yields no results. Note that a
  ## tick *equal* to the runtime is measured, so it must not warn - which is
  ## why the check is "> runtime" and not ">= runtime" as the issue suggested.
  testthat::expect_warning(make(c(5, 20), 10), regexp = "beyond the runtime")
  testthat::expect_silent(make(c(5, 10), 10))
  testthat::expect_silent(make(NA_integer_, 10))

  ## Without a runtime the reachable ticks are unknown, so no warning:
  testthat::expect_silent(make(c(5, 20), 0))
})

testthat::test_that("util_harmonize_block_types keeps results when blocks disagree", {

  ## Issues #110 and #90: one block reports a metric as a number, another as a
  ## character (a NetLogo error message), and dplyr::bind_rows() then threw
  ## away the results of every completed run.
  blocks <- list(
    tibble::tibble(run_number = 1, metric = 1.5),
    tibble::tibble(run_number = 2, metric = "Error: division by zero")
  )

  testthat::expect_error(dplyr::bind_rows(blocks))

  testthat::expect_warning(harmonized <- util_harmonize_block_types(blocks),
                           regexp = "different types for: metric")
  bound <- dplyr::bind_rows(harmonized)

  testthat::expect_identical(nrow(bound), 2L)
  testthat::expect_type(bound$metric, "character")
  testthat::expect_identical(bound$metric, c("1.5", "Error: division by zero"))

  ## Matching types are left alone, and so are all-NA columns, which carry no
  ## type information:
  consistent <- list(tibble::tibble(metric = 1), tibble::tibble(metric = 2))
  testthat::expect_silent(unchanged <- util_harmonize_block_types(consistent))
  testthat::expect_type(dplyr::bind_rows(unchanged)$metric, "double")

  with_na <- list(tibble::tibble(metric = NA), tibble::tibble(metric = 2))
  testthat::expect_silent(util_harmonize_block_types(with_na))

  ## Agent metrics are list columns and must not be touched:
  nested <- list(
    tibble::tibble(metrics.turtles = list(tibble::tibble(who = 1))),
    tibble::tibble(metrics.turtles = list(tibble::tibble(who = 2)))
  )
  testthat::expect_silent(util_harmonize_block_types(nested))
})

testthat::test_that("simdesign_morris passes scale through to sensitivity::morris", {

  ## Ported from the morris_no_scale branch: scale was hardcoded to TRUE, so
  ## unscaled elementary effects were not obtainable.
  nl <- nl_lhs

  for (scale in c(TRUE, FALSE)) {
    nl@simdesign <- simdesign_morris(nl = nl, morristype = "oat",
                                     morrislevels = 4, morrisr = 2,
                                     morrisgridjump = 2, nseeds = 1,
                                     scale = scale)
    testthat::expect_identical(getsim(nl, "simobject")[[1]]$scale, scale)
  }

  ## The default stays TRUE, as before:
  nl@simdesign <- simdesign_morris(nl = nl, morristype = "oat",
                                   morrislevels = 4, morrisr = 2,
                                   morrisgridjump = 2, nseeds = 1)
  testthat::expect_true(getsim(nl, "simobject")[[1]]$scale)
})
