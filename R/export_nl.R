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
#' Exports your folder that contains data and scripts for NetLogo + nlrx analyses
#' as a zip file. Furthermore, `export_nl` takes your nl object and saves it in the
#' zipped folder. This enables other person to run the same experiment as you.
#'
#' @examples
#' \dontrun{
#'
#' folder <- "/home/user/test"
#' outfile <- "/home/user/test.zip"
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

  mywd <- getwd()

  ## Create zip:
  setwd(folder)
  utils::zip(zipfile=outfile, files = list.files(), extras = "-r")

  ## Add nl object:
  ## Create a rds file from the nl object
  nltempdir <- tempdir()
  nltempfile <- paste0(nltempdir, "/nlobject.rds")
  saveRDS(nl, nltempfile)
  setwd(nltempdir)
  utils::zip(zipfile=outfile, files="nlobject.rds", flags="-j")

  ## Reset wd:
  setwd(mywd)

}
