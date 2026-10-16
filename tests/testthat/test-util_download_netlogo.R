
testthat::context("Download NetLogo")
testthat::test_that("supported_netlogo_versions", {

  versions <- supported_netlogo_versions()

  testthat::expect_vector(versions)
  testthat::expect_true(all(check_netlogo_version(versions)))
  testthat::expect_false(check_netlogo_version("non_valid_version"))
  testthat::expect_error(check_netlogo_version("non_valid_version", throw_error = TRUE))

  ## Tested versions pass silently:
  testthat::expect_silent(check_netlogo_version("7.0.4", throw_error = TRUE))

  ## An untested NetLogo 7 release only warns - execution goes through logolink,
  ## so it is likely to work - and is still reported as untested:
  testthat::expect_warning(untested <- check_netlogo_version("7.9.9", throw_error = TRUE))
  testthat::expect_false(untested)

  ## NetLogo < 7 is not untested but unsupported, so it stays an error - and the
  ## error points at the last nlrx version that supports NetLogo 6.x:
  testthat::expect_error(check_netlogo_version("6.4.0", throw_error = TRUE),
                         regexp = "install_github")

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
    ## macOS ships one image per architecture, so both are requestable and the
    ## default follows the running machine:
    testthat::expect_equal(util_netlogo_download_url(v, "mac", arch = "x86_64"),
                           paste0(base, "NetLogo-", v, "-x86_64.dmg"))
    testthat::expect_equal(util_netlogo_download_url(v, "mac", arch = "aarch64"),
                           paste0(base, "NetLogo-", v, "-aarch64.dmg"))
    testthat::expect_equal(util_netlogo_download_url(v, "mac"),
                           paste0(base, "NetLogo-", v, "-", util_get_mac_arch(), ".dmg"))
  }

  testthat::expect_error(util_netlogo_download_url("7.0.4", "Unknown OS"))

  ## The detected architecture is one of the two NetLogo publishes:
  testthat::expect_true(util_get_mac_arch() %in% c("aarch64", "x86_64"))

})

testthat::test_that("NetLogo download URLs are reachable (headers only, no download)", {

  # Needs internet; never run on CRAN.
  testthat::skip_on_cran()
  testthat::skip_if_not(isTRUE(capabilities("libcurl")), "libcurl not available")
  # skip_if_offline() errors rather than skips when the curl package is missing,
  # so guard it. curl is only needed for that connectivity probe; the test body
  # below uses base curlGetHeaders(), which needs libcurl but not the package.
  testthat::skip_if_not_installed("curl")
  testthat::skip_if_offline("github.com")

  for (v in supported_netlogo_versions()) {
    ## Both macOS images are published, so both are probed:
    nl_urls <- c(util_netlogo_download_url(v, "win"),
                 util_netlogo_download_url(v, "unix"),
                 util_netlogo_download_url(v, "mac", arch = "x86_64"),
                 util_netlogo_download_url(v, "mac", arch = "aarch64"))

    for (nl_url in nl_urls) {
      # curlGetHeaders performs a HEAD-like request: it retrieves the response
      # headers (following redirects) without downloading the file body.
      status <- attr(curlGetHeaders(nl_url, redirect = TRUE), "status")
      testthat::expect_equal(
        status, 200L,
        info = paste0("Unreachable NetLogo download URL: ", nl_url)
      )
    }
  }

})
