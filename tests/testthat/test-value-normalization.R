testthat::context("Value normalization for logolink")

testthat::test_that("util_normalize_nl_value repairs the pre-0.5.0 value conventions", {

  ## Escaped string quotes are stripped, exactly one layer:
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value("\"sheep-wolves-grass\""), "sheep-wolves-grass")
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value("sheep-wolves-grass"), "sheep-wolves-grass")
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value("\"\"double\"\""), "\"double\"")
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value(c("\"a\"", "b")), c("a", "b"))

  ## Boolean strings become logicals, in any capitalisation:
  testthat::expect_identical(nlrx:::util_normalize_nl_value("false"), FALSE)
  testthat::expect_identical(nlrx:::util_normalize_nl_value("TRUE"), TRUE)
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value(c("true", "false")), c(TRUE, FALSE))

  ## A quoted boolean is converted as well: NetLogo parses an unquoted `true`
  ## as a boolean, so the string "true" was never expressible as a value.
  ## Converting it keeps the result the same no matter how often the function
  ## is applied along the pipeline.
  testthat::expect_identical(nlrx:::util_normalize_nl_value("\"true\""), TRUE)
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value(nlrx:::util_normalize_nl_value("\"false\"")), FALSE)

  ## Non-character values are passed through unchanged:
  testthat::expect_identical(nlrx:::util_normalize_nl_value(30), 30)
  testthat::expect_identical(nlrx:::util_normalize_nl_value(TRUE), TRUE)
  testthat::expect_identical(
    nlrx:::util_normalize_nl_value(list(first = 1, step = 1, last = 3)),
    list(first = 1, step = 1, last = 3))
  testthat::expect_identical(nlrx:::util_normalize_nl_value(character(0)), character(0))
  testthat::expect_identical(nlrx:::util_normalize_nl_value(NA_character_), NA_character_)

  ## Idempotent, so it can be applied at several points of the pipeline:
  once <- nlrx:::util_normalize_nl_value("\"default\"")
  testthat::expect_identical(nlrx:::util_normalize_nl_value(once), once)
})

testthat::test_that("experiment() stores normalized constants and variables", {

  exp <- experiment(
    expname = "quoting",
    outpath = tempdir(),
    idsetup = "setup",
    idgo = "go",
    runtime = 10,
    metrics = c("count sheep"),
    variables = list("model-version" = list(
      values = c("\"sheep-wolves-grass\"", "\"sheep-wolves\""))),
    constants = list("model-version" = "\"sheep-wolves-grass\"",
                     "show-energy?" = "false",
                     "grass-regrowth-time" = 30))

  testthat::expect_identical(exp@constants[["model-version"]], "sheep-wolves-grass")
  testthat::expect_identical(exp@constants[["show-energy?"]], FALSE)
  testthat::expect_identical(exp@constants[["grass-regrowth-time"]], 30)
  testthat::expect_identical(exp@variables[["model-version"]]$values,
                             c("sheep-wolves-grass", "sheep-wolves"))
})

testthat::test_that("normalized values are written as single-quoted BehaviorSpace values", {

  ## Regression test for the pre-0.5.0 escaped-quote convention, which reached
  ## NetLogo as ""sheep-wolves-grass"" and aborted the run. Needs no NetLogo:
  ## logolink writes the XML without executing it.
  exp <- experiment(
    expname = "quoting",
    outpath = tempdir(),
    idsetup = "setup",
    idgo = "go",
    runtime = 10,
    metrics = c("count sheep"),
    constants = list("model-version" = "\"sheep-wolves-grass\"",
                     "show-energy?" = "false",
                     "grass-regrowth-time" = 30))

  xml_path <- suppressWarnings(logolink::create_experiment(
    name = "quoting",
    time_limit = 10,
    setup = "setup",
    go = "go",
    metrics = "count sheep",
    constants = lapply(exp@constants, nlrx:::util_normalize_nl_value)))

  xml <- xml2::read_xml(xml_path)
  value_of <- function(variable) {
    xml2::xml_attr(
      xml2::xml_find_all(
        xml,
        paste0(".//enumeratedValueSet[@variable='", variable, "']/value")),
      "value")
  }

  testthat::expect_identical(value_of("model-version"), "\"sheep-wolves-grass\"")
  testthat::expect_identical(value_of("show-energy?"), "false")
  testthat::expect_identical(value_of("grass-regrowth-time"), "30")
})

testthat::test_that("check_value_format only reports what nlrx cannot repair", {

  ## Repaired conventions are not reported any more:
  testthat::expect_length(nlrx:::check_value_format("\"default\"", "pSTRING"), 0)
  testthat::expect_length(nlrx:::check_value_format("false", "pLOGIC"), 0)

  ## NetLogo list literals cannot survive the handover to logolink:
  testthat::expect_length(nlrx:::check_value_format("[1 2 3]", "pLIST"), 1)
  testthat::expect_match(nlrx:::check_value_format("[1 2 3]", "pLIST"), "pLIST")
})
