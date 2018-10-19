#' Import NetLogo Experiment
#'
#' @description Import NetLogo Experiment from export_nl
#'
#' @param nl nl object
#' @param folder Path to folder that contains files to run NetLogo experiment
#' @param outfile Path to folder where the experiments gets extracted
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
#'
#' }
#' @aliases import_nl
#' @rdname import_nl
#'
#' @export

import_nl <- function(folder, outfile, ...){
  unzip(folder, exdir = outfile, junkpaths = TRUE)

  if(length(list.files(outfile, pattern = "Rproj")) == 1){
    rstudioapi::openProject(list.files(outfile, pattern = "Rproj", full.names	= TRUE))
    readRDS("nlobject.rds")
  }

}
