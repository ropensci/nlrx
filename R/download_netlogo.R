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
#' * "604" = NetLogo Version 6.0.4
#' * "603" = NetLogo Version 6.0.3
#' * "602" = NetLogo Version 6.0.2
#' * "601" = NetLogo Version 6.0.1
#' * "600" = NetLogo Version 6.0.0
#' * "531" = NetLogo Version 5.3.1
#'
#'
#'
#' @examples
#' \dontrun{
#' download_netlogo("/home/user/experiment/" , "603")
#' }
#'
#' @aliases download_netlogo
#' @rdname download_netlogo
#'
#' @export


download_netlogo <- function(to, version) {

  switch(version,
  "604" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.4/"},
  "603" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.3/"},
  "602" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.2/"},
  "601" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0.1/"},
  "600" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/6.0/"},
  "531" = {nl_vers <- "https://ccl.northwestern.edu/netlogo/5.3.1/"})

  download.file(nl_vers, to)

}



