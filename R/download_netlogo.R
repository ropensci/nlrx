#' Download NetLogo
#'
#' @description Auxiliary function to download NetLogo
#'
#' @param to      Path to folder where the downloaded file is saved.
#' @param version Character string naming which NetLogo Version to download (see Details)
#' @param extract TRUE/FALSE, if TRUE downloaded archive is extracted to subfolder of `to` (only unix)
#' @details
#'
#' Supported Versions for Download and Usage (parameter `version`):
#' * "6.1.0" = NetLogo Version 6.1.0
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
#' \donttest{
#' dlpath <- tempdir()  # adjust path to your needs
#' download_netlogo(dlpath, "6.0.3")
#' }
#'
#' @aliases download_netlogo
#' @rdname download_netlogo
#'
#' @export


download_netlogo <- function(to, version, extract = FALSE) {
  switch(version,
    "6.1.0" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.1.0/"
    },
    "6.0.4" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.0.4/"
    },
    "6.0.3" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.0.3/"
    },
    "6.0.2" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.0.2/"
    },
    "6.0.1" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.0.1/"
    },
    "6.0" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/6.0.0/"
    },
    "5.3.1" = {
      nl_url <- "https://ccl.northwestern.edu/netlogo/5.3.1/"
    }
  )

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

  nl_dl <- paste0(nl_url, nl_file)
  to_file <- paste0(to, nl_file)
  utils::download.file(nl_dl, to_file)

  ## Extract the archive if os=unix:
  if (util_get_os() == "unix") {
    system(paste0("tar xvzf ", to_file, " --directory ", to))
  }
}
