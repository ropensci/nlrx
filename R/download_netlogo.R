#' Report supported NetLogo versions
#'
#' @description Auxiliary function to report supported NetLogo version numbers
#' @return  a vector of supported Versions for Download and Usage (parameter `version`):
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

#' Check supported NetLogo versions
#'
#' @description Auxiliary function to check supported NetLogo version numbers
#' @param version Character string naming which NetLogo Version to check
#' @param throw_error TRUE/FALSE, if TRUE an error is thrown if the version is not supported
#' @return logical, if netlogo versions is supported by nlrx
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

  ## Throw error if needed:
  if (isTRUE(throw_error)) {
    if (!isTRUE(supported)) {

      # For NetLogo 7+
      if (numeric_version(version) >= numeric_version("7.0.0")) {
        msg <- paste(
          "NetLogo version", version,
          "is not officially tested with nlrx.",
          "NetLogo 7+ support is handled through logolink and execution may therefore still succeed.",
          "To bypass this check, set check_version = FALSE in nl().",
          "Officially tested versions are:",
          paste(supported_netlogo_versions(), collapse = ", ")
        )
        stop(msg, call. = FALSE)
        # For NetLogo below 7 (deprecated support)
      } else {
        msg <- paste(
          "NetLogo version",
          version,
          "is no longer supported by nlrx.",
          "Support for NetLogo versions below 7.0.0 has been deprecated.",
          "Please upgrade to NetLogo 7.0.0 or newer.",
          "To bypass this check (not recommended), set check_version = FALSE in nl().",
          "Officially supported versions are:",
          paste(supported_netlogo_versions(), collapse = ", ")
        )
        stop(msg, call. = FALSE)
      }
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
#' Retrieve supported Versions for Download and Usage (parameter `version`):
#' supported_netlogo_versions()
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
