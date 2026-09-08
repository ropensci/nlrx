
testthat::context("Download NetLogo")
testthat::test_that("supported_netlogo_versions", {

  versions <- supported_netlogo_versions()

  testthat::expect_vector(versions)
  testthat::expect_true(all(check_netlogo_version(versions)))
  testthat::expect_false(check_netlogo_version("non_valid_version"))
  testthat::expect_error(check_netlogo_version("non_valid_version", throw_error = TRUE))

  # Mock out the actual download and the tar extraction so the test stays
  # offline. `os` is pinned rather than left at NA so the extract branch (which
  # only triggers on unix) is exercised on every platform.
  testthat::local_mocked_bindings(download.file = function(...) NULL,
                                  .package = "utils")
  testthat::local_mocked_bindings(system = function(...) NULL,
                                  .package = "base")
  nlversion <- "7.0.0"
  nlpath <- tempdir()

  testthat::expect_null(download_netlogo(to = nlpath,
                                         os = "unix",
                                         version = nlversion,
                                         extract = TRUE))

})

testthat::test_that("util_netlogo_download_url builds GitHub release URLs", {

  # Pin the exact string shape for one version ...
  testthat::expect_equal(
    util_netlogo_download_url("7.0.4", "unix"),
    "https://github.com/NetLogo/NetLogo/releases/download/v7.0.4/NetLogo-7.0.4-64.tgz"
  )
  testthat::expect_equal(
    util_netlogo_download_url("7.0.4", "win"),
    "https://github.com/NetLogo/NetLogo/releases/download/v7.0.4/NetLogo-7.0.4-64.msi"
  )

  # ... then check every supported version constructs the expected pattern
  # (offline, so CRAN runs it too).
  for (v in supported_netlogo_versions()) {
    base <- paste0("https://github.com/NetLogo/NetLogo/releases/download/v", v, "/")
    testthat::expect_equal(util_netlogo_download_url(v, "unix"),
                           paste0(base, "NetLogo-", v, "-64.tgz"))
    testthat::expect_equal(util_netlogo_download_url(v, "win"),
                           paste0(base, "NetLogo-", v, "-64.msi"))
    testthat::expect_equal(util_netlogo_download_url(v, "mac"),
                           paste0(base, "NetLogo-", v, "-x86_64.dmg"))
  }

  testthat::expect_error(util_netlogo_download_url("7.0.4", "Unknown OS"))

})

testthat::test_that("NetLogo download URLs are reachable (headers only, no download)", {

  # Needs internet; never run on CRAN.
  testthat::skip_on_cran()
  testthat::skip_if_offline("github.com")
  testthat::skip_if_not(isTRUE(capabilities("libcurl")), "libcurl not available")

  for (v in supported_netlogo_versions()) {
    for (os in c("win", "mac", "unix")) {
      nl_url <- util_netlogo_download_url(v, os)
      # curlGetHeaders performs a HEAD-like request: it retrieves the response
      # headers (following redirects) without downloading the file body.
      status <- attr(curlGetHeaders(nl_url, redirect = TRUE), "status")
      testthat::expect_equal(
        status, 200L,
        info = paste0("Unreachable NetLogo download URL (", os, "): ", nl_url)
      )
    }
  }

})
