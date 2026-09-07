testthat::context("Exe: run_nl_block tests")

testthat::test_that("run_nl_block", {

  # Run these tests only on Github actions:
  testthat::skip_if(!identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  # Check that JAVA is installed:
  testthat::expect_true(system('java -version') == 0)

  # Check that NetLogo installation worked:
  nlpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 7.0.4",
                   ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 7.0.4",
                          ifelse(nlrx:::util_get_os() == "mac", "/Applications/netlogo/NetLogo 7.0.4",
                                 "FAILED")))

  testthat::expect_true(nlpath != "FAILED")
  testthat::expect_true(dir.exists(nlpath))

  jarpath <- ifelse(nlrx:::util_get_os() == "win", "C:/Program Files/NetLogo 7.0.4/app/netlogo-7.0.4.jar",
                    ifelse(nlrx:::util_get_os() == "unix", "/home/runner/work/netlogo/NetLogo 7.0.4/app/netlogo-7.0.4.jar",
                           ifelse(nlrx:::util_get_os() == "mac", "/Applications/netlogo/NetLogo 7.0.4/app/netlogo-7.0.4.jar",
                                  "FAILED")))

  testthat::expect_true(jarpath != "FAILED")
  testthat::expect_true(file.exists(jarpath))


  # Check if simple simulation can be ran:
  # Step1: Create a nl object:
  modelpath <- file.path(nlpath, "models", "Sample Models",
                         "Biology", "Wolf Sheep Predation.nlogox")
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  outpath <- tempdir()

  # Step2: Add Experiment
  nl@experiment <- experiment(expname = "wolf-sheep",
                              outpath = outpath,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 1,
                              metrics = c("count sheep", "count wolves"),
                              variables = list("sheep-gain-from-food" = list(min = 2, max = 6, qfun = "qunif")),
                              constants = list('initial-number-sheep' = 100,
                                               'initial-number-wolves' = 50,
                                               "wolf-gain-from-food" = 20,
                                               "grass-regrowth-time" = 30,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "model-version" = "sheep-wolves-grass",
                                               "show-energy?" = FALSE))

  testthat::context("Run block execution with run_nl_block()")

  # Step3: Add simdesign (LHS for simple block testing)
  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=10,
                                nseeds=1,
                                precision=3)

  # Step4: Create blocks using create_simulation_blocks()
  blocks <- create_simulation_blocks(nl, block_size = 5) # ensures correct formatting with random-seed column
  block_df <- blocks[[1]] # Get first block (5 simulations)

  # Step5: Run block execution
  results <- run_nl_block(nl = nl,
                          block_df = block_df,
                          block_number = 1,
                          threads = 1)

  # Step6: Check results
  testthat::expect_s3_class(results, "tbl_df")
  testthat::expect_gt(nrow(results), 0)
  testthat::expect_true("step" %in% colnames(results))
  testthat::expect_true("siminputrow" %in% colnames(results))
})
