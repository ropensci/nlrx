
testthat::context("Download NetLogo")
testthat::test_that("supported_netlogo_versions", {

  versions <- supported_netlogo_versions()

  testthat::expect_vector(versions)
  testthat::expect_true(all(check_netlogo_version(versions)))
  testthat::expect_false(check_netlogo_version("non_valid_version"))
  testthat::expect_error(check_netlogo_version("non_valid_version", throw_error = TRUE),
                         class = "nlrx_error_netlogo_version_invalid")

  ## Tested versions pass silently:
  testthat::expect_silent(check_netlogo_version("7.0.4", throw_error = TRUE))

  ## An untested NetLogo 7 release only warns - execution goes through logolink,
  ## so it is likely to work - and is still reported as untested:
  testthat::expect_warning(untested <- check_netlogo_version("7.9.9", throw_error = TRUE),
                           class = "nlrx_warning_netlogo_version_untested")
  testthat::expect_false(untested)

  ## The hint how to silence the warning only makes sense within nl():
  untested_msg <- function(...) {
    tryCatch(util_report_netlogo_version("7.9.9", ...),
             warning = conditionMessage)
  }
  testthat::expect_no_match(untested_msg(), "check_version")
  testthat::expect_match(untested_msg(in_nl = TRUE), "check_version")
  testthat::expect_warning(nl(nlversion = "7.9.9", nlpath = tempdir()),
                           regexp = "check_version",
                           class = "nlrx_warning_netlogo_version_untested")
  testthat::expect_silent(nl(nlversion = "7.9.9", nlpath = tempdir(),
                             check_version = FALSE))

  ## NetLogo < 7 is not untested but unsupported, so it stays an error - and the
  ## error points at the last nlrx version that supports NetLogo 6.x:
  testthat::expect_error(check_netlogo_version("6.4.0", throw_error = TRUE),
                         regexp = "install_github",
                         class = "nlrx_error_netlogo_version_unsupported")
  testthat::expect_error(nl(nlversion = "6.4.0", nlpath = tempdir()),
                         class = "nlrx_error_netlogo_version_unsupported")

  # Mock out the actual download and the tar extraction so the test stays
  # offline. `os` is pinned rather than left at NA so the extract branch (which
  # only triggers on unix) is exercised on every platform.
  download_args <- NULL
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    download_args <<- c(list(url = url, destfile = destfile, timeout = getOption("timeout")), list(...))
    0L
  }, .package = "utils")
  testthat::local_mocked_bindings(untar = function(...) 0L, .package = "utils")
  nlversion <- "7.0.0"
  nlpath <- tempdir()
  timeout <- getOption("timeout")

  res <- download_netlogo(to = nlpath, os = "unix", version = nlversion, extract = TRUE)
  testthat::expect_identical(res, file.path(nlpath, "NetLogo-7.0.0-64.tgz"))
  ## Binary transfer, with a timeout that suits the archive size, restored afterwards:
  testthat::expect_identical(download_args$mode, "wb")
  testthat::expect_gte(download_args$timeout, 3600)
  testthat::expect_identical(getOption("timeout"), timeout)

})

testthat::test_that("download_netlogo explains failed downloads", {

  nlpath <- tempfile("download_test")
  dir.create(nlpath)
  partial <- file.path(nlpath, "NetLogo-7.9.9-64.tgz")

  ## A release that does not exist (download.file warns, then fails):
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("", destfile)
    warning("downloaded length 0 != reported length 9")
    warning("cannot open URL '", url, "': HTTP status was '404 Not Found'")
    stop("cannot open URL '", url, "'")
  }, .package = "utils")
  testthat::expect_error(
    suppressWarnings(download_netlogo(to = nlpath, os = "unix", version = "7.9.9")),
    regexp = "not available for download",
    class = "nlrx_error_download_netlogo"
  )
  testthat::expect_false(file.exists(partial))

  ## Any other failure, e.g. a timeout:
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    warning("Timeout of 3600 seconds was reached")
    stop("download from '", url, "' failed")
  }, .package = "utils")
  testthat::expect_error(
    download_netlogo(to = nlpath, os = "unix", version = "7.0.4"),
    regexp = "internet connection",
    class = "nlrx_error_download_netlogo"
  )
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
