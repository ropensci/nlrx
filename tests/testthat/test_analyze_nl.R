testthat::context("Analyze nl with sample data")
testthat::test_that("Analysis of sample data", {


  testthat::context("Load sample data ff")
  data("nl_ff", package="nlrx")

  testthat::expect_match(class(nl_ff)[1], "nl")

  testthat::context("Analyze sample data ff")
  ff <- analyze_nl(nl_ff)

  testthat::expect_match(class(ff)[1], "tbl_df")
  testthat::expect_equal(nrow(ff), 121, .01)
  testthat::expect_equal(ncol(ff), 11, .01)
  testthat::expect_equal(mean(ff$`count sheep_mean`), 197.1322, .01)
  testthat::expect_equal(mean(ff$`count wolves_mean`), 62.34711, .01)



  testthat::context("Load sample data lhs")
  data("nl_lhs", package="nlrx")

  testthat::expect_match(class(nl_lhs)[1], "nl")

  testthat::context("Analyze sample data lhs")
  lhs <- analyze_nl(nl_lhs)

  testthat::expect_match(class(lhs)[1], "tbl_df")
  testthat::expect_equal(nrow(lhs), 100, .01)
  testthat::expect_equal(ncol(lhs), 18, .01)
  testthat::expect_equal(mean(lhs$`count sheep_mean`), 129.48, .01)
  testthat::expect_equal(mean(lhs$`count wolves_mean`), 31.91, .01)



  testthat::context("Load sample data sobol")
  data("nl_sobol", package="nlrx")

  testthat::expect_match(class(nl_sobol)[1], "nl")

  testthat::context("Analyze sample data sobol")
  sobol <- analyze_nl(nl_sobol)

  testthat::expect_match(class(sobol)[1], "tbl_df")
  testthat::expect_equal(nrow(sobol), 6, .01)
  testthat::expect_equal(ncol(sobol), 8, .01)
  testthat::expect_equal(mean(sobol$original), 0.3385253, .01)
  testthat::expect_equal(mean(sobol$bias), 0.006594322, .01)


  testthat::context("Load sample data sobol2007")
  data("nl_sobol2007", package="nlrx")

  testthat::expect_match(class(nl_sobol2007)[1], "nl")

  testthat::context("Analyze sample data sobol2007")
  sobol2007 <- analyze_nl(nl_sobol2007)

  testthat::expect_match(class(sobol2007)[1], "tbl_df")
  testthat::expect_equal(nrow(sobol2007), 8, .01)
  testthat::expect_equal(ncol(sobol2007), 9, .01)
  testthat::expect_equal(mean(sobol2007$original), 0.5373813, .01)
  testthat::expect_equal(mean(sobol2007$bias), 0.00490168, .01)


  testthat::context("Load sample data soboljansen")
  data("nl_soboljansen", package="nlrx")

  testthat::expect_match(class(nl_soboljansen)[1], "nl")

  testthat::context("Analyze sample data soboljansen")
  soboljansen <- analyze_nl(nl_soboljansen)

  testthat::expect_match(class(soboljansen)[1], "tbl_df")
  testthat::expect_equal(nrow(soboljansen), 8, .01)
  testthat::expect_equal(ncol(soboljansen), 9, .01)
  testthat::expect_equal(mean(soboljansen$original), 0.4977627, .01)
  testthat::expect_equal(mean(soboljansen$bias), 0.00232037, .01)


  testthat::context("Load sample data morris")
  data("nl_morris", package="nlrx")

  testthat::expect_match(class(nl_morris)[1], "nl")

  testthat::context("Analyze sample data morris")
  morris <- analyze_nl(nl_morris)

  testthat::expect_match(class(morris)[1], "tbl_df")
  testthat::expect_equal(nrow(morris), 12, .01)
  testthat::expect_equal(ncol(morris), 5, .01)
  testthat::expect_equal(mean(morris$value), 23.61037, .01)



  testthat::context("Load sample data eFast")
  data("nl_eFast", package="nlrx")

  testthat::expect_match(class(nl_eFast)[1], "nl")

  testthat::context("Analyze sample data eFast")
  eFast <- analyze_nl(nl_eFast)

  testthat::expect_match(class(eFast)[1], "tbl_df")
  testthat::expect_equal(nrow(eFast), 8, .01)
  testthat::expect_equal(ncol(eFast), 5, .01)
  testthat::expect_equal(mean(eFast$value), 258.6271, .01)

})
