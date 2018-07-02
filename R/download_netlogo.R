#' Download NetLogo
#'
#' @description Auxiliary function to download NetLogo
#'
#' @param to      Character string with the name where the downloaded file is saved.
#' @param version Character string naming which NetLogo Version to download (see Details)
 #'
#' @details
#'
#' Supported Versions for Download and Usage (parameter `version`):
#'
#' * "6.0.4" = NetLogo Version 6.0.4
#' * "6.0.3" = NetLogo Version 6.0.3
#' * "6.0.2" = NetLogo Version 6.0.2
#' * "6.0.1" = NetLogo Version 6.0.1
#' * "6.0" = NetLogo Version 6.0
#' * "5.3.1" = NetLogo Version 5.3.1
#'
#'
#'
#' @examples
#' \dontrun{
#' download_netlogo("/home/user/experiment/" , "6.0.3")
#' }
#'
#' @aliases download_netlogo
#' @rdname download_netlogo
#'
#' @export


download_netlogo <- function(to, version) {

  switch(version,
         "6.0.4" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.4/"},
         "6.0.3" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.3/"},
         "6.0.2" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.2/"},
         "6.0.1" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.1/"},
         "6.0" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.0/"},
         "5.3.1" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/5.3.1/"})

  switch(util_get_os(),
         "win" = {nl_dl <- paste0(nl_vers, "NetLogo-", version, "-64.msi")},
         "mac" = {nl_dl <- paste0(nl_vers, "NetLogo-", version, ".dmg")},
         "unix" = {nl_dl <- paste0(nl_vers, "NetLogo-", version, "-64.tgz")},
         "Unknown OS" = {stop("Unknown OS. OS not supported by NetLogo")})

  utils::download.file(nl_dl, to)

}

