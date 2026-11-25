#' Import NetLogo Experiment
#'
#' @description Import NetLogo Experiment from export_nl
#'
#' @param tarfile Path to tarfile that contains files to run NetLogo experiment
#' @param targetdir Path to folder where the experiments gets extracted
#' @param new_session If TRUE, opens a new RStudio Session with an Rproj
#'
#' @return The imported nl object, invisibly.
#'
#' @details
#'
#' Imports NetLogo experiments that were saved with `export_nl`. The archived
#' folder is extracted into `targetdir` and the nl object stored with it is
#' returned.
#' If the folder comes with an .Rproj file (which is recommended because
#' relative paths enhance the reputability of your analysis),
#' `import_nl` can open this project in a new RStudio session (`new_session = TRUE`).
#'
#' Note that the paths stored in the nl object (`nlpath`, `modelpath`, `outpath`)
#' still point to the locations on the machine the experiment was exported from.
#'
#' @examples
#' \dontrun{
#'
#' infile <- "/home/user/test.tar.gz"
#' targetdirectory <- "/home/user/test"
#' nl <- import_nl(infile, targetdirectory)
#' }
#' @aliases import_nl
#' @rdname import_nl
#'
#' @export

import_nl <- function(tarfile, targetdir, new_session = FALSE) {

  ## Check that the archive holds one folder with an nl object, as written by
  ## export_nl(), before anything is extracted:
  files <- utils::untar(tarfile, list = TRUE, tar = "internal")
  folder <- unique(sub("/.*$", "", sub("^\\./", "", files)))
  folder <- folder[nzchar(folder) & folder != "."]

  if (length(folder) != 1 ||
      !any(sub("^\\./", "", files) == file.path(folder, "nlobject.rds"))) {
    util_abort(c(
      "{.file {tarfile}} is not an archive created by {.fn export_nl}.",
      "i" = "It has to contain one folder with an {.file nlobject.rds} file."
    ), class = "nlrx_error_import_archive")
  }

  ## Extract the folder:
  utils::untar(tarfile, exdir = targetdir, tar = "internal")
  folderpath <- file.path(targetdir, folder)

  ## Read the nl object:
  nl <- readRDS(file.path(folderpath, "nlobject.rds"))

  # nocov start
  rproj <- list.files(folderpath, pattern = "\\.Rproj$", full.names = TRUE)
  if (length(rproj) == 1 && isTRUE(new_session)) {
    rstudioapi::openProject(rproj)
  }
  # nocov end

  invisible(nl)
}
