#' Initialize a new nl object
#'
#' @description Initialize a new nl object
#'
#' @param nlversion Character string naming which NetLogo Version is used.
#' @param nlpath Character string pointing to the Netlogo main directory, matching the defined version.
#' @param modelpath Character string pointing to the Netlogo model that should be used for analyses.
#' @param jvmmem Number defining the amount of memory in megabytes that is allocated to each jvm process.
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
#' new_nl(new_nl(nlversion = "6.0.3",
#' nlpath = "C:/Program Files/NetLogo 6.0.3/",
#' modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024))
#' }
#'
#' @aliases new_nl
#' @rdname new_nl
#'
#' @export

new_nl <- function(nlversion, nlpath, modelpath, jvmmem) {

  nl <- new("nl",
            nlversion = nlversion,
            nlpath = nlpath,
            modelpath = modelpath,
            jvmmem = jvmmem)

  return(nl)

}
