#' Import NetLogo Experiment
#'
#' @description Import NetLogo Experiment from export_nl
#'
#' @param tarfile Path to tarfile that contains files to run NetLogo experiment
#' @param targetdir Path to folder where the experiments gets extracted
#' @param new_session If TRUE, opens a new RStudio Session with an Rproj
#'
#' @return The status value returned by the external command, invisibly.
#'
#' @details
#'
#' Imports NetLogo experiments that were saved with `export_nl`.
#' If the folder comes with an .Rproj file (which is recommended because
#' relative paths enhance the reproducability of your analysis),
#' `import_nl` opens this project and loads the nl object in your R environment.
#'
#' @examples
#' \dontrun{
#'
#' infile <- "/home/user/test.zip"
#' outfile <- "/home/user/test"
#' import_nl(infile, outfile)
#' }
#' @aliases import_nl
#' @rdname import_nl
#'
#' @export

import_nl <- function(tarfile, targetdir, new_session = FALSE) {
  #utils::unzip(folder, exdir = outfile, junkpaths = TRUE)

  system(paste0("tar -zxvf \"", tarfile, "\" -C \"", targetdir, "\""))

  # nocov start
  if (length(list.files(tarfile, pattern = "Rproj")) == 1 &&
    isTRUE(new_session)) {
    rstudioapi::openProject(list.files(tarfile,
      pattern = "Rproj",
      full.names = TRUE
    ))
  }
  # nocov end
}
