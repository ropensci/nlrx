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
#' xxx
#'
#' @examples
#' \dontrun{
#'
#' infile <- "/home/marco/Desktop/test.zip"
#' outfile <- "/home/marco/Desktop/test"
#' import_nl(infile, outfile)
#'
#' }
#' @aliases import_nl
#' @rdname import_nl
#'
#' @export

import_nl <- function(folder, outfile, ...){
  unzip(folder, exdir = outfile, junkpaths = TRUE)
}
