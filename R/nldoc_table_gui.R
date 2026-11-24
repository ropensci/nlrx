#' Read NetLogo GUI elements from files
#'
#' @description Read NetLogo GUI elements from files
#'
#' @param modelfiles vector of filepaths to model files
#'
#' @return list containing NetLogo GUI elements, or NA if the model has none
#'
#' @details
#'
#' The procedure reads the parameter widgets (sliders, switches, input boxes and
#' choosers) of the main model file (.nlogox) and reports a list of GUI elements.
#'
#' @aliases nldoc_table_gui
#' @rdname nldoc_table_gui
#' @keywords internal
nldoc_table_gui <- function(modelfiles) {

  ## Filter file names for main model file:
  modelpath <- modelfiles[tolower(tools::file_ext(modelfiles)) == "nlogox"][1]

  ## Read the widgets (the same parser as report_model_parameters()):
  modelparam <- util_read_nlogox_widgets(modelpath)

  if (length(modelparam) == 0) {
    return(NA)
  }

  return(modelparam)
}
