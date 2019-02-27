#' Read NetLogo behavior space experiments from files
#'
#' @description Read NetLogo behavior space experiments from files
#'
#' @param modelfiles vector of filepaths to model files
#'
#' @return list containing NetLogo behavior space experiments
#'
#' @details
#'
#' The procedure reads text from the provided model files and reports a list of behavior space experiments.
#'
#' @aliases nldoc_table_bs
#' @rdname nldoc_table_bs
#' @keywords internal
nldoc_table_bs <- function(modelfiles)
{
  ## Extract information of behavior space experiments
  ## Filter file names for main model file:
  modelpath <- modelfiles[grepl(pattern=".nlogo", modelfiles)]

  ## Open the model as string
  modelcode <- readLines(modelpath)


  ## Find start and end position:
  start <- grep(pattern="<experiments>", modelcode, fixed=TRUE)
  end <- grep(pattern="</experiments>", modelcode, fixed=TRUE)
  modelcode <- modelcode[start:end]

  ## Convert to xml
  experiments <- XML::xmlParse(modelcode)

  ## Convert to data frame:
  experiments.list <- XML::xmlToList(experiments)

  return(experiments.list)
}
