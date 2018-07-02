#' Initialize a new nl object
#'
#' @description Initialize a new nl object
#'
#' @param nlversion A character string defining the NetLogo version that is used
#' @param nlpath Path to the NetLogo main directory matching the defined version
#' @param modelpath Path to the NetLogo model file (*.nlogo) that is used for simulations
#' @param jvmmem Java virtual machine memory capacity in megabytes
#' @param experiment Holds a experiment S4 class object
#'
#' @details
#'
#' nl objects are the main class objects used in the nlrx package.
#' These objects store all information that is needed to run NetLogo simulations.
#' nl objects are initialized with basic information on Netlogo and the model.
#' To run simulation experiments, a experiment needs to be added to the nl object.
#' Each experiment also needs to have a specified simulation design in order to run simulations.
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' new_nl(nlversion = "6.0.3",
#' nlpath = "C:/Program Files/NetLogo 6.0.3/",
#' modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#' }
#'
#' @aliases new_nl
#' @rdname new_nl
#'
#' @export

new_nl <- function(nlversion, nlpath, modelpath, jvmmem) {

  nl <- methods::new("nl",
                     nlversion = nlversion,
                     nlpath = nlpath,
                     modelpath = modelpath,
                     jvmmem = jvmmem)

  return(nl)

}
