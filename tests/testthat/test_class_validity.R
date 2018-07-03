testthat::context("Class validity")
testthat::test_that("class objects are valid classes", {

  testthat::expect_match(class(nl())[1], "nl")
  testthat::expect_match(class(experiment())[1], "experiment")
  testthat::expect_match(class(simdesign())[1], "simdesign")

})
