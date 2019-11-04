Sys.unsetenv("R_TESTS")

library(testthat)
library(nlrx)

#test_check("nlrx")
test_check("nlrx", filter = "^[a-m]")
test_check("nlrx", filter = "^[n-z]")
