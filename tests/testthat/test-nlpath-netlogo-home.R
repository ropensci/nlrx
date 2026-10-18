testthat::context("nlpath and NETLOGO_HOME")

testthat::test_that("nl() defaults nlpath to NETLOGO_HOME", {

  withr_home <- Sys.getenv("NETLOGO_HOME")
  on.exit(
    if (nzchar(withr_home)) Sys.setenv(NETLOGO_HOME = withr_home) else Sys.unsetenv("NETLOGO_HOME"),
    add = TRUE
  )

  Sys.setenv(NETLOGO_HOME = tempdir())
  testthat::expect_identical(getnl(nl(modelpath = "m.nlogox"), "nlpath"),
                             path.expand(tempdir()))

  ## An explicit path still wins over the environment variable:
  explicit <- file.path(tempdir(), "explicit")
  testthat::expect_identical(getnl(nl(nlpath = explicit, modelpath = "m.nlogox"), "nlpath"),
                             path.expand(explicit))

  ## Without the variable, nlpath stays empty and NETLOGO_HOME is resolved at run time:
  Sys.unsetenv("NETLOGO_HOME")
  testthat::expect_false(nzchar(getnl(nl(modelpath = "m.nlogox"), "nlpath")))
})

testthat::test_that("run_nl_block() reports a missing NetLogo installation", {

  withr_home <- Sys.getenv("NETLOGO_HOME")
  on.exit(
    if (nzchar(withr_home)) Sys.setenv(NETLOGO_HOME = withr_home) else Sys.unsetenv("NETLOGO_HOME"),
    add = TRUE
  )

  nl <- nl_lhs
  block_df <- getsim(nl, "siminput")[1, , drop = FALSE]
  block_df$siminputrow <- 1

  ## Neither nlpath nor NETLOGO_HOME: a clear error instead of an obscure
  ## failure inside logolink.
  Sys.unsetenv("NETLOGO_HOME")
  nl@nlpath <- ""
  testthat::expect_error(
    run_nl_block(nl = nl, block_df = block_df, block_number = 1, threads = 1),
    regexp = "No NetLogo installation defined"
  )

  ## A stale nlpath with no fallback is an error that names the path.
  nl@nlpath <- file.path(tempdir(), "does-not-exist")
  testthat::expect_error(
    run_nl_block(nl = nl, block_df = block_df, block_number = 1, threads = 1),
    regexp = "does not exist on this system"
  )

  ## With NETLOGO_HOME set, a stale nlpath (e.g. from an imported nl object)
  ## warns and falls back rather than refusing to run. The run itself fails
  ## afterwards because the fallback directory holds no NetLogo, which is not
  ## what this test asserts - only the fallback warning is.
  Sys.setenv(NETLOGO_HOME = tempdir())
  testthat::expect_warning(
    try(run_nl_block(nl = nl, block_df = block_df, block_number = 1, threads = 1),
        silent = TRUE),
    regexp = "Falling back to NETLOGO_HOME"
  )

  ## The environment is left as it was found.
  testthat::expect_identical(Sys.getenv("NETLOGO_HOME"), tempdir())
})
