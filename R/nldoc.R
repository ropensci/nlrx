#' Create NetLogo documentation
#'
#' @description Create NetLogo documentation
#'
#' @param modelfiles vector of filepaths to model files
#' @param outpath Path to folder where rendered documentation should be created
#' @param gui TRUE/FALSE, if TRUE a table with GUI elements from the model will be included in the documentation
#' @param bs TRUE/FALSE, if TRUE a table with behavior space experiments will be included in the documentation
#' @param infotab TRUE/FALSE, if TRUE infotab section will be included in the documentation
#' @param output_format either "html", "pdf" or "docx"
#' @param number_sections TRUE/FALSE, if TRUE sections in the documentation will be numbered
#' @param theme markdown theme, supported themes are "journal", "cerulean", "flatly", "readable", "spacelab", "united", "cosmo"
#' @param date date that is printed in the documentation header
#' @param toc TRUE/FALSE, if TRUE the documentation contains a table of contents - only for html and pdf output format
#'
#' @details
#'
#' nldoc reads model code from the provided model files.
#' The code is then split into several groups (code, gui, behavior space).
#' The procedures then finds noxygen commands within the NetLogo model code.
#' For a complete list of noxygen commands type ?nldoc
#' These commands are translated into a markdown documentation file.
#' If needed, tables of gui elements and behavior space experiments are added to the markdown file.
#' Finally, the document is rendered in the specified format.
#'
#' @examples
#' \dontrun{
#'
#' # List model files (.nls subfiles are also supported)
#' modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogo",
#'                 "https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nls")
#'
#' # Define output directory:
#' outdir <- tempdir()  # adjust path to your needs
#'
#' # Create documentation:
#' nldoc(modelfiles = modelfiles,
#'       infotab=TRUE,
#'       gui=TRUE,
#'       bs=TRUE,
#'       outpath = outdir,
#'       output_format = "html",
#'       number_sections = TRUE,
#'       theme = "cosmo",
#'       date = date(),
#'       toc = TRUE)
#' }
#'
#' @aliases nldoc
#' @rdname nldoc
#'
#' @export

nldoc <- function(modelfiles,
                  outpath,
                  infotab = TRUE,
                  gui = TRUE,
                  bs = TRUE,
                  output_format = "html",
                  number_sections = TRUE,
                  theme = "journal",
                  date = as.Date(Sys.time()),
                  toc = TRUE)
{
  ## Read code from provided netlogo files:
  nlogocode <- nldoc_read_nlogo(modelfiles)
  ## Parse code to noxygencode:
  noxygen <- nldoc_parse_modelcode(nlogocode)

  ## Read gui elements from nlogo file:
  if (isTRUE(gui))
  {
    noxygen_gui <- nldoc_table_gui(modelfiles)
  } else
  {
    noxygen_gui <- NA
  }


  ## Read behaviorspace experiments:
  if (isTRUE(bs))
  {
    noxygen_bs <- nldoc_table_bs(modelfiles)
  } else
  {
    noxygen_bs <- NA
  }

  ## Read infotab
  if (isTRUE(infotab))
  {
    noxygen_it <- nlogocode$infotabcode
  } else
  {
    noxygen_it <- NA
  }


  ## Write noxygen to nldoc
  nldoc_write_nldoc(noxygen = noxygen,
                   noxygen_it = noxygen_it,
                   noxygen_gui = noxygen_gui,
                   noxygen_bs = noxygen_bs,
                   outpath = outpath,
                   output_format = output_format,
                   number_sections = number_sections,
                   theme = theme,
                   date = date,
                   toc = toc)

}
