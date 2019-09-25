testthat::context("Analyze nl with sample data")
testthat::test_that("Analysis of sample data", {
  myfuns <- list(mean=mean, sd=stats::sd, min=min, max=max)
  testthat::context("Load sample data ff")
  data("nl_ff", package = "nlrx")

  testthat::expect_match(class(nl_ff)[1], "nl")

  testthat::context("Analyze sample data ff")
  ff <- analyze_nl(nl_ff, funs = myfuns)

  testthat::expect_match(class(ff)[1], "tbl_df")
  testthat::expect_equal(nrow(ff), 121, .01)
  testthat::expect_equal(ncol(ff), 19, .01)
  testthat::expect_equal(mean(ff$`count sheep_mean`), 83.1, .01)
  testthat::expect_equal(mean(ff$`count wolves_mean`), 112, .01)



  testthat::context("Load sample data lhs")
  data("nl_lhs", package = "nlrx")

  testthat::expect_match(class(nl_lhs)[1], "nl")

  testthat::context("Analyze sample data lhs")
  lhs <- analyze_nl(nl_lhs, funs = myfuns)

  testthat::expect_match(class(lhs)[1], "tbl_df")
  testthat::expect_equal(nrow(lhs), 100, .01)
  testthat::expect_equal(ncol(lhs), 19, .01)
  testthat::expect_equal(mean(lhs$`count sheep_mean`), 81.2, .01)
  testthat::expect_equal(mean(lhs$`count wolves_mean`), 110, .01)



  testthat::context("Load sample data sobol")
  data("nl_sobol", package = "nlrx")

  testthat::expect_match(class(nl_sobol)[1], "nl")

  testthat::context("Analyze sample data sobol")
  sobol <- analyze_nl(nl_sobol, funs = myfuns)

  testthat::expect_match(class(sobol)[1], "tbl_df")
  testthat::expect_equal(nrow(sobol), 18, .01)
  testthat::expect_equal(ncol(sobol), 8, .01)
  testthat::expect_equal(mean(sobol$original), 0.3385253, .01)
  testthat::expect_equal(mean(sobol$bias), 0.006594322, .01)


  testthat::context("Load sample data sobol2007")
  data("nl_sobol2007", package = "nlrx")

  testthat::expect_match(class(nl_sobol2007)[1], "nl")

  testthat::context("Analyze sample data sobol2007")
  sobol2007 <- analyze_nl(nl_sobol2007, funs = myfuns)

  testthat::expect_match(class(sobol2007)[1], "tbl_df")
  testthat::expect_equal(nrow(sobol2007), 24, .01)
  testthat::expect_equal(ncol(sobol2007), 9, .01)
  testthat::expect_equal(mean(sobol2007$original), 0.513, .01)
  testthat::expect_equal(mean(sobol2007$bias), 0.003789834, .01)


  testthat::context("Load sample data soboljansen")
  data("nl_soboljansen", package = "nlrx")

  testthat::expect_match(class(nl_soboljansen)[1], "nl")

  testthat::context("Analyze sample data soboljansen")
  soboljansen <- analyze_nl(nl_soboljansen, funs = myfuns)

  testthat::expect_match(class(soboljansen)[1], "tbl_df")
  testthat::expect_equal(nrow(soboljansen), 24, .01)
  testthat::expect_equal(ncol(soboljansen), 9, .01)
  testthat::expect_equal(mean(soboljansen$original), 0.509, .01)
  testthat::expect_equal(mean(soboljansen$bias), 0.001826447, .01)


  testthat::context("Load sample data morris")
  data("nl_morris", package = "nlrx")

  testthat::expect_match(class(nl_morris)[1], "nl")

  testthat::context("Analyze sample data morris")
  morris <- analyze_nl(nl_morris, funs = myfuns)

  testthat::expect_match(class(morris)[1], "tbl_df")
  testthat::expect_equal(nrow(morris), 36, .01)
  testthat::expect_equal(ncol(morris), 5, .01)
  testthat::expect_equal(mean(morris$value), 37.9, .01)



  testthat::context("Load sample data eFast")
  data("nl_eFast", package = "nlrx")

  testthat::expect_match(class(nl_eFast)[1], "nl")

  testthat::context("Analyze sample data eFast")
  eFast <- analyze_nl(nl_eFast, funs = myfuns)

  testthat::expect_match(class(eFast)[1], "tbl_df")
  testthat::expect_equal(nrow(eFast), 24, .01)
  testthat::expect_equal(ncol(eFast), 5, .01)
  testthat::expect_equal(mean(eFast$value), 393, .01)
})
