#' Report tested NetLogo versions
#'
#' @description Auxiliary function to report the NetLogo versions nlrx is tested with
#' @return a vector of NetLogo versions that nlrx is tested with (parameter `version`)
#' @details
#' These are the versions used in the automated tests and for \link[nlrx]{download_netlogo}.
#' Newer NetLogo 7 releases are not rejected: because execution is delegated to 'logolink',
#' they are likely to work and only emit a warning (see \link[nlrx]{check_netlogo_version}).
#'
#' @examples
#' \dontrun{
#' supported_netlogo_versions()
#' }
#'
#' @aliases supported_netlogo_versions
#' @rdname supported_netlogo_versions
#'
#' @export
supported_netlogo_versions <- function() {

  supported_versions <- c(
    "7.0.0",
    "7.0.1",
    "7.0.2",
    "7.0.3",
    "7.0.4"
  )
  return(supported_versions)
}

#' Check NetLogo versions
#'
#' @description Auxiliary function to check a NetLogo version number against the tested versions
#' @param version Character string naming which NetLogo Version to check
#' @param throw_error TRUE/FALSE, if TRUE untested versions are reported (see details)
#' @return logical, TRUE if the version is one of \link[nlrx]{supported_netlogo_versions}
#' @details
#' With \code{throw_error = TRUE} a version that is not among the tested ones is reported
#' according to how likely it is to work:
#' \itemize{
#'   \item NetLogo \strong{7.0.0 or newer}: a \strong{warning}. Simulations are executed by
#'   'logolink', so untested releases are likely to run; set \code{check_version = FALSE} in
#'   \link[nlrx]{nl} to silence it.
#'   \item NetLogo \strong{below 7.0.0}: an \strong{error}. Support for these versions was
#'   removed in nlrx 0.5.0, so simulations cannot run at all. The error points to nlrx 0.4.6,
#'   the last version supporting NetLogo 5.3.1 to 6.4.0, which can be installed from GitHub
#'   with \code{remotes::install_github("ropensci/nlrx@v0.4.6")}.
#'   \item A string that is not a version number: an \strong{error}.
#' }
#'
#' @examples
#' \dontrun{
#' check_netlogo_version("7.0.0")
#' check_netlogo_version("1.0")
#' check_netlogo_version("1.0", TRUE)
#' }
#'
#' @aliases check_netlogo_version
#' @rdname check_netlogo_version
#'
#' @export
check_netlogo_version <- function(version, throw_error=FALSE) {

  ## Check version support
  supported <- version %in% supported_netlogo_versions()

  ## Report untested versions if needed:
  if (isTRUE(throw_error) && !isTRUE(supported)) {
    util_report_netlogo_version(version)
  }

  return(supported)
}

#' Report a NetLogo version that nlrx is not tested with
#'
#' @description Signals the advice-style error or warning for a NetLogo version
#' that is not among \link[nlrx]{supported_netlogo_versions} (see
#' \link[nlrx]{check_netlogo_version} for the rules).
#'
#' @param version Character string naming the NetLogo version
#' @param in_nl TRUE if called from \code{nl()}, which adds the hint how to
#' silence the warning for untested NetLogo 7 versions
#' @aliases util_report_netlogo_version
#' @rdname util_report_netlogo_version
#' @keywords internal
util_report_netlogo_version <- function(version, in_nl = FALSE) {

  tested <- supported_netlogo_versions()

  ## A string that is not a version number cannot be acted on at all:
  parsed <- tryCatch(numeric_version(version), error = function(e) NULL)

  if (is.null(parsed)) {
    util_abort(c(
      "{.val {version}} is not a valid NetLogo version number.",
      "i" = "nlrx is tested with NetLogo {tested}."
    ), class = "nlrx_error_netlogo_version_invalid")
  }

  # For NetLogo 7+: untested, but execution is delegated to logolink, so it is
  # likely to work. Warn and continue instead of refusing.
  if (parsed >= numeric_version("7.0.0")) {
    util_warn(c(
      "NetLogo {version} is not among the versions nlrx is tested with.",
      "i" = "nlrx is tested with NetLogo {tested}. NetLogo 7 runs through logolink, so simulations are likely to work anyway.",
      if (isTRUE(in_nl)) c(">" = "Set {.code check_version = FALSE} in {.fn nl} to silence this warning.")
    ), class = "nlrx_warning_netlogo_version_untested")
    return(invisible(FALSE))
  }

  # For NetLogo below 7: support was removed, simulations cannot run.
  util_abort(c(
    "nlrx no longer supports NetLogo {version}.",
    "i" = "Support for NetLogo below 7.0.0 was removed in nlrx 0.5.0; nlrx is tested with NetLogo {tested}.",
    ">" = "Upgrade to NetLogo 7, see {.url https://docs.netlogo.org/7.0.4/transition#changes-for-netlogo-700}.",
    ">" = "Or keep NetLogo 6 with nlrx 0.4.6, the last version for NetLogo 5.3.1 to 6.4.0 (no longer updated): {.code remotes::install_github(\"ropensci/nlrx@v0.4.6\")}"
  ), class = "nlrx_error_netlogo_version_unsupported")
}


#' Download NetLogo
#'
#' @description Auxiliary function to download NetLogo
#'
#' @param to      Path to folder where the downloaded file is saved.
#' @param version Character string naming which NetLogo Version to download (see Details)
#' @param os operation system ("win", "mac", "unix") decides which version of netlogo (msi, dmg, tgz) is downloaded.
#' If set to NA (default) os will be detected automatically (`util_get_os()`)
#' @param extract TRUE/FALSE, if TRUE downloaded archive is extracted to subfolder of `to` (only unix)
#' @param arch macOS architecture of the disk image, "aarch64" (Apple Silicon) or "x86_64" (Intel).
#' If NA (default) the architecture of the running machine is detected (`util_get_mac_arch()`).
#' Ignored for Windows and Linux, which have one download each.
#' @details
#'
#' Retrieve the versions nlrx is tested with (parameter `version`):
#' supported_netlogo_versions()
#'
#' Newer NetLogo 7 releases can be downloaded as well: the download URL is derived from the
#' version number, so an untested version only emits a warning
#' (\link[nlrx]{check_netlogo_version}) and fails with an error pointing to the NetLogo
#' releases page if no such release exists.
#'
#' The download (about 400 MB) may take longer than R's default \code{timeout} of 60
#' seconds, so the timeout is raised to at least one hour while it runs.
#'
#' @return The path of the downloaded file, invisibly.
#'
#' @examples
#' \dontrun{
#' dlpath <- tempdir()  # adjust path to your needs
#' try(download_netlogo(dlpath, "7.0.4"))
#' }
#'
#' @aliases download_netlogo
#' @rdname download_netlogo
#'
#' @export
download_netlogo <- function(to, version, os = NA, extract = FALSE, arch = NA) {

  ## Check version support
  check_netlogo_version(version, throw_error = TRUE)
  ## Detect OS if not provided
  if (is.na(os)) {
    os <- util_get_os()
  }
  ## Construct the download URL (see util_netlogo_download_url)
  nl_dl <- util_netlogo_download_url(version, os, arch)
  to_file <- file.path(to, basename(nl_dl))

  ## Download. The archives are large, so R's default timeout of 60 seconds is
  ## raised for the download. mode = "wb": on Windows a text transfer would
  ## corrupt the installer (only some extensions are detected as binary).
  timeout <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(timeout), add = TRUE)

  ## download.file() reports a failed or incomplete download by warnings (and
  ## an error for a failed one), so any of them means the file is unusable:
  problems <- character()
  tryCatch(
    withCallingHandlers(
      utils::download.file(nl_dl, to_file, mode = "wb"),
      warning = function(w) {
        problems <<- c(problems, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) problems <<- c(problems, conditionMessage(e))
  )

  if (length(problems) > 0) {
    unlink(to_file)
    reason <- problems[length(problems)]

    if (any(grepl("404", problems))) {
      util_abort(c(
        "NetLogo {version} is not available for download.",
        "i" = "There is no file at {.url {nl_dl}} (HTTP 404).",
        ">" = "Check the available versions at {.url https://github.com/NetLogo/NetLogo/releases}."
      ), class = "nlrx_error_download_netlogo")
    }

    util_abort(c(
      "Downloading NetLogo {version} failed.",
      "i" = "{reason}",
      ">" = "Check your internet connection and try again, or download NetLogo from {.url https://github.com/NetLogo/NetLogo/releases} yourself."
    ), class = "nlrx_error_download_netlogo")
  }

  ## Extract the archive if os==unix and extract == TRUE:
  if (os == "unix" & extract == TRUE) {
    utils::untar(to_file, exdir = to)
  }

  invisible(to_file)
}

# Construct the NetLogo download URL for a given version and OS.
# NetLogo 7+ releases are hosted on GitHub under a 'v'-prefixed tag, e.g.
# https://github.com/NetLogo/NetLogo/releases/download/v7.0.4/NetLogo-7.0.4-64.tgz
# macOS ships separate x86_64 and aarch64 disk images; we default to x86_64,
# which also runs on Apple Silicon via Rosetta.
util_netlogo_download_url <- function(version, os, arch = NA) {

  ## NetLogo ships one disk image per macOS architecture:
  if (identical(os, "mac") && is.na(arch)) {
    arch <- util_get_mac_arch()
  }

  nl_file <- switch(
    os,
    "win"  = paste0("NetLogo-", version, "-64.msi"),
    "mac"  = paste0("NetLogo-", version, "-", arch, ".dmg"),
    "unix" = paste0("NetLogo-", version, "-64.tgz"),
    stop("Unknown OS. OS not supported by NetLogo", call. = FALSE)
  )
  paste0("https://github.com/NetLogo/NetLogo/releases/download/v",
         version, "/", nl_file)
}


#' Report the macOS architecture
#'
#' @description Reports which macOS architecture a NetLogo disk image should be downloaded for
#'
#' @return "aarch64" on Apple Silicon, "x86_64" otherwise
#' @details
#' Under Rosetta, R reports \code{x86_64} even on Apple Silicon, so the hardware
#' is asked directly via \code{sysctl hw.optional.arm64} in that case. Pass
#' \code{arch} to \link[nlrx]{download_netlogo} to override the result, for
#' example to fetch an image for a different machine.
#' @aliases util_get_mac_arch
#' @rdname util_get_mac_arch
#' @keywords internal
util_get_mac_arch <- function() {

  machine <- unname(Sys.info()["machine"])

  if (isTRUE(grepl("^(arm|aarch)", machine, ignore.case = TRUE))) {
    return("aarch64")
  }

  ## Rosetta reports an Intel machine on Apple Silicon hardware:
  arm_hardware <- suppressWarnings(tryCatch(
    system2("sysctl", c("-n", "hw.optional.arm64"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  ))

  if (length(arm_hardware) == 1 && identical(trimws(arm_hardware), "1")) {
    return("aarch64")
  }

  return("x86_64")
}
