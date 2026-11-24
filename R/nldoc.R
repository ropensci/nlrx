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
#' The noxygen commands are listed in the nldoc article: \url{https://docs.ropensci.org/nlrx/articles/nlrx_nldoc.html}.
#' Without a \code{@model} header, the name of the model file is used as title.
#' These commands are translated into a markdown documentation file.
#' If needed, tables of gui elements and behavior space experiments are added to the markdown file.
#' Finally, the document is rendered in the specified format.
#'
#' @examples
#' \dontrun{
#'
#' # List model files (.nls subfiles are also supported)
#' modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogox",
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
  ## Read code from provided netlogox files:
  nlogocode <- nldoc_read_nlogox(modelfiles)
  ## Parse code to noxygencode:
  noxygen <- nldoc_parse_modelcode(nlogocode)

  ## Name of the main model file, the title if the code has no @model header:
  mainfile <- modelfiles[tolower(tools::file_ext(modelfiles)) == "nlogox"][1]
  modelname <- if (is.na(mainfile)) "Untitled Model" else tools::file_path_sans_ext(basename(mainfile))

  ## Without noxygen annotations, only the model file sections are documented:
  if (nrow(noxygen$header) == 0 && nrow(noxygen$global) == 0 && nrow(noxygen$proc) == 0)
  {
    cli::cli_alert_info(paste(
      "No noxygen annotations found in the model code, so the documentation only",
      "covers the GUI, Info tab and BehaviorSpace sections. The annotation tags are listed in",
      "{.url https://docs.ropensci.org/nlrx/articles/nlrx_nldoc.html}."
    ))
  }

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
                   toc = toc,
                   modelname = modelname)

}
