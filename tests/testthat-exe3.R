Sys.unsetenv("R_TESTS")

library(testthat)
library(nlrx)

test_check("nlrx", filter = "exe3")
