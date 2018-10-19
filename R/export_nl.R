#' Export NetLogo Experiment
#'
#' @description Export NetLogo Experiment as zip file
#'
#' @param nl nl object
#' @param folder Path to folder that contains files to run NetLogo experiment
#' @param outfile Path to folder where the experiments gets stored as zip file
#'
#' @return The status value returned by the external command, invisibly.
#'
#' @details
#'
#' ccc
#'
#' @examples
#' \dontrun{
#'
#' folder <- "/home/marco/Documents/NetLogo 6.0.4/app/models/Code Examples/Extensions Examples/gis"
#' outfile <- "/home/marco/Desktop/test.zip"
#' export_nl(nl, folder=folder, outfile=outfile)
#'
#' }
#' @aliases export_nl
#' @rdname export_nl
#'
#' @export


export_nl <- function(nl,
                      folder=dirname(getnl(nl, "modelpath")),
                      outfile)
{

  folderpath <- dirname(folder)
  foldername <- basename(folder)

  mywd <- getwd()

  ## Create zip:
  setwd(folderpath)
  zip(zipfile=outfile, files=foldername)

  ## Add nl object:
  ## Create a rds file from the nl object
  nltempdir <- tempdir()
  nltempfile <- paste0(nltempdir, "/nlobject.rds")
  saveRDS(nl, nltempfile)
  setwd(nltempdir)
  zip(zipfile=outfile, files="nlobject.rds", flags="-j")

  ## Reset wd:
  setwd(mywd)

}
