testthat::context("Util stuff")
testthat::test_that("Util stuff", {


  # Run these tests only on Github actions:
  testthat::skip_if(!identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  ## Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  ## Check that netLogo installation worked:
  nlpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1",
                   ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1",
                          ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1",
                                 "FAILED")))

  testthat::expect_true(nlpath != "FAILED")
  testthat::expect_true(dir.exists(nlpath))

  jarpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                    ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                           ifelse(nlrx:::util_get_os() == "mac","/Applications/netlogo/NetLogo 6.1.1/app/netlogo-6.1.1.jar",
                                  "FAILED")))

  testthat::expect_true(jarpath != "FAILED")
  testthat::expect_true(file.exists(jarpath))

  ## Now we check if we can run a simple simulation:
  ## Step1: Create a nl obejct:
  modelpath <- file.path(nlpath, "app", "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogo")

  nl <- nl(nlversion = "6.1.1",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)


  params <- report_model_parameters(nl)

  testthat::expect_match(class(params), "list")
  testthat::expect_equal(length(params), 9)

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
  testthat::expect_true(max(vapply(lhs[[1]], FUN=function(x)
    nchar(strsplit(as.character(x), "\\.")[[1]][2]), numeric(1)), na.rm=TRUE) <=
                         precision)
  testthat::expect_true(max(vapply(lhs[[2]], FUN=function(x)
    nchar(strsplit(as.character(x), "\\.")[[1]][2]), numeric(1)), na.rm=TRUE) <=
    precision)


  testthat::context("Test util_get_os()")
  os <- util_get_os()

  testthat::expect_match(class(os), "character")


})
