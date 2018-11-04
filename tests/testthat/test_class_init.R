testthat::context("Class initialization")
testthat::test_that("class objects are initialized", {

 .initClasses()
 class(nl)
 nl <- methods::new("nl")
 # exp <- methods::new("experiment")
 # sd <- methods::new("simdesign")
 #
 testthat::expect_match(class(nl)[[1]], "nl")
 # testthat::expect_match(class(exp)[[1]], "experiment")
 # testthat::expect_match(class(sd)[[1]], "simdesign")

 testthat::expect_match(class(nl())[1], "nl")
 # testthat::expect_match(class(experiment())[1], "experiment")
 # testthat::expect_match(class(simdesign())[1], "simdesign")

})
