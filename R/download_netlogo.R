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
    "5.3.1",
    "6.0.0",
    "6.0.1",
    "6.0.2",
    "6.0.3",
    "6.0.4",
    "6.1.0",
    "6.1.1",
    "6.2.0",
    "6.2.1",
    "6.2.2",
    "6.3.0",
    "6.4.0",
    "7.0.0",
    "7.0.1",
    "7.0.2"
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
#' check_netlogo_version("6.2.0")
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
      msg <- paste(
        "Netlogo version",
        version,
        "is not supported by nlrx.",
        "Use one of the supported versions: ",
        paste(supported_netlogo_versions(), collapse=", ")
      )
      stop(msg)
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
#' @param arch Character string defining the system architecture.
#'   Valid values depend on the operating system:
#'   * **Windows / Unix**:
#'     Must be `"32"` or `"64"`.
#'     If omitted, defaults to `"64"`.
#'   * **macOS**:
#'     - For NetLogo versions **≤ 6.4.0**, no architecture is used in the filename.
#'       In this case, `arch` must be `NULL` (the default).
#'     - For NetLogo versions **≥ 7.0.0**, the architecture is **mandatory** and must be
#'       either `"intel"` (resulting in a `-x86_64.dmg` installer) or
#'       `"silicon"` (resulting in a `-aarch64.dmg` installer).
#'   An error is raised if an invalid architecture is provided for the selected OS or version.
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
#' try(download_netlogo(dlpath, "6.0.3"))
#' }
#'
#' @aliases download_netlogo
#' @rdname download_netlogo
#'
#' @export
download_netlogo <- function(to, version, os = NA, arch = NULL, extract = FALSE) {

  ## Check version support
  check_netlogo_version(version, throw_error = TRUE)

  ## Parse version for comparisons
  version_numeric <- numeric_version(version)

  ## Construct base url
  nl_url <- paste0("https://ccl.northwestern.edu/netlogo/", version, "/")

  ## Determine OS
  if (is.na(os)) {
    os <- util_get_os()
  }

  ## -----------------------------
  ## OS-SPECIFIC FILENAME LOGIC
  ## -----------------------------

  if (os == "win") {

    ## Windows logic
    if (is.null(arch)) arch <- "64"
    if (!arch %in% c("32", "64"))
      stop("For Windows, `arch` must be '32' or '64'.")

    nl_file <- paste0("NetLogo-", version, "-", arch, ".msi")

  } else if (os == "mac") {

    ## macOS logic based on version
    if (version_numeric <= numeric_version("6.4.0")) {
      nl_file <- paste0("NetLogo-", version, ".dmg")
    } else if (version_numeric >= numeric_version("7.0.0")) {
      if (is.null(arch))
        stop("For NetLogo ≥ 7.0.0 on mac, `arch` must be 'intel' or 'silicon'.")
      if (arch == "intel") {
        nl_file <- paste0("NetLogo-", version, "-x86_64.dmg")
      } else if (arch == "silicon") {
        nl_file <- paste0("NetLogo-", version, "-aarch64.dmg")
      } else {
        stop("For macOS (NetLogo ≥ 7.0.0), `arch` must be 'intel' or 'silicon'.")
      }
    } else {
      stop("Unsupported Mac version logic encountered.")
    }
  } else if (os == "unix") {
    if (is.null(arch)) arch <- "64"
    if (!arch %in% c("32", "64"))
      stop("For Unix, `arch` must be '32' or '64'.")
    nl_file <- paste0("NetLogo-", version, "-", arch, ".tgz")

  } else if (os == "Unknown OS") {

    stop("Unknown OS. OS not supported by NetLogo.")

  } else {
    stop("Invalid `os` value provided.")
  }
  ## DOWNLOAD
  nl_dl <- paste0(nl_url, nl_file)
  to_file <- file.path(to, nl_file)
  utils::download.file(nl_dl, to_file)

  ## Extract for unix if requested:
  if (os == "unix" && extract) {
    system(paste0("tar xvzf ", shQuote(to_file), " --directory ", shQuote(to)))
  }
}

