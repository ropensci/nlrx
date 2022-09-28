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
    "6.2.2"
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
    msg <- paste(
      "Netlogo version",
      version,
      "is not supported by nlrx.",
      "Use one of the supported versions: ",
      paste(supported_netlogo_versions(), collapse=", ")
    )
    stop(msg)
  }

  return(supported)
}


#' Download NetLogo
#'
#' @description Auxiliary function to download NetLogo
#'
#' @param to      Path to folder where the downloaded file is saved.
#' @param version Character string naming which NetLogo Version to download (see Details)
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
download_netlogo <- function(to, version, extract = FALSE) {

  ## Check version support
  check_netlogo_version(version, throw_error = TRUE)
  ## Construct base url
  nl_url <- paste0("https://ccl.northwestern.edu/netlogo/", version, "/")
  ## Get filename depending on os
  switch(util_get_os(),
         # nocov start
         "win" = {
           nl_file <- paste0("NetLogo-", version, "-64.msi")
         },
         "mac" = {
           nl_file <- paste0("NetLogo-", version, ".dmg")
         },
         # nocov end
         "unix" = {
           nl_file <- paste0("NetLogo-", version, "-64.tgz")
         },
         # nocov start
         "Unknown OS" = {
           stop("Unknown OS. OS not supported by NetLogo")
         }
         # nocov end
  )
  ## Download
  nl_dl <- paste0(nl_url, nl_file)
  to_file <- paste0(to, nl_file)
  utils::download.file(nl_dl, to_file)

  ## Extract the archive if os=unix:
  if (util_get_os() == "unix") {
    system(paste0("tar xvzf ", to_file, " --directory ", to))
  }
}
