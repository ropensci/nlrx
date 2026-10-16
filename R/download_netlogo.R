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

    ## A string that is not a version number cannot be acted on at all:
    parsed <- tryCatch(numeric_version(version), error = function(e) NULL)

    if (is.null(parsed)) {
      stop(
        paste(
          "NetLogo version", version, "is not a valid version number.",
          "Tested versions are:",
          paste(supported_netlogo_versions(), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # For NetLogo 7+: untested, but execution is delegated to logolink, so it is
    # likely to work. Warn and continue instead of refusing.
    if (parsed >= numeric_version("7.0.0")) {
      msg <- paste(
        "NetLogo version", version,
        "is not among the versions nlrx was tested with.",
        "NetLogo 7+ is executed through logolink, so simulations are likely to run anyway.",
        "To silence this warning, set check_version = FALSE in nl().",
        "Tested versions are:",
        paste(supported_netlogo_versions(), collapse = ", ")
      )
      warning(msg, call. = FALSE)
      # For NetLogo below 7: support was removed, simulations cannot run.
    } else {
      msg <- paste0(
        "NetLogo version ", version, " is no longer supported by nlrx.\n",
        "Support for NetLogo versions below 7.0.0 was removed in nlrx 0.5.0.\n",
        "Tested versions are: ",
        paste(supported_netlogo_versions(), collapse = ", "), ".\n\n",
        "Either upgrade to NetLogo 7.0.0 or newer, or keep using your NetLogo ",
        "installation with nlrx 0.4.6, the last version that supports NetLogo 5.3.1 to 6.4.0:\n",
        "  remotes::install_github(\"ropensci/nlrx@v0.4.6\")\n",
        "Note that nlrx 0.4.6 no longer receives updates."
      )
      stop(msg, call. = FALSE)
    }
  }

  return(supported)
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
#' @details
#'
#' Retrieve the versions nlrx is tested with (parameter `version`):
#' supported_netlogo_versions()
#'
#' Newer NetLogo 7 releases can be downloaded as well: the download URL is derived from the
#' version number, so an untested version only emits a warning
#' (\link[nlrx]{check_netlogo_version}) and fails with a download error if no such release
#' exists.
#'
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
download_netlogo <- function(to, version, os = NA, extract = FALSE) {

  ## Check version support
  check_netlogo_version(version, throw_error = TRUE)
  ## Detect OS if not provided
  if (is.na(os)) {
    os <- util_get_os()
  }
  ## Construct the download URL (see util_netlogo_download_url)
  nl_dl <- util_netlogo_download_url(version, os)
  to_file <- file.path(to, basename(nl_dl))

  ## Download
  utils::download.file(nl_dl, to_file)

  ## Extract the archive if os==unix and extract == TRUE:
  if (os == "unix" & extract == TRUE) {
    system(paste0("tar xvzf ", to_file, " --directory ", to))
  }
}

# Construct the NetLogo download URL for a given version and OS.
# NetLogo 7+ releases are hosted on GitHub under a 'v'-prefixed tag, e.g.
# https://github.com/NetLogo/NetLogo/releases/download/v7.0.4/NetLogo-7.0.4-64.tgz
# macOS ships separate x86_64 and aarch64 disk images; we default to x86_64,
# which also runs on Apple Silicon via Rosetta.
util_netlogo_download_url <- function(version, os) {
  nl_file <- switch(
    os,
    "win"  = paste0("NetLogo-", version, "-64.msi"),
    "mac"  = paste0("NetLogo-", version, "-x86_64.dmg"),
    "unix" = paste0("NetLogo-", version, "-64.tgz"),
    stop("Unknown OS. OS not supported by NetLogo", call. = FALSE)
  )
  paste0("https://github.com/NetLogo/NetLogo/releases/download/v",
         version, "/", nl_file)
}
