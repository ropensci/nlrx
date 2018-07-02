#' Evaluate variables list of an experiment object
#'
#' @description Evaluate variables list of an experiment object
#'
#' @param nl nl object
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_eval_variables
#' @rdname util_eval_variables

util_eval_variables <- function(nl) {

  if(length(variables(nl)) == 0){
    stop("Error: Experiment Variable list is empty. You need to define a variable list with at least one element!")
  }
}

#' Evaluate constants list of an experiment object
#'
#' @description Evaluate constants list of an experiment object
#'
#' @param nl nl object
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_eval_constants
#' @rdname util_eval_constants

util_eval_constants <- function(nl) {

  if(length(constants(nl)) == 0){
    stop("Error: Experiment constants list is empty. You need to define a constants list with at least one element!")
  }
}

#' Evaluate experiment object
#'
#' @description Evaluate experiment object
#'
#' @param nl nl object
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_eval_experiment
#' @rdname util_eval_experiment

util_eval_experiment <- function(nl) {

  notvalid <- c()

  if(is.na(expname(nl))) {
    notvalid <- c(notvalid, "expname")
  }
  if(is.na(outpath(nl))) {
    notvalid <- c(notvalid, "outpath")
  }
  if(is.na(runtime(nl))) {
    notvalid <- c(notvalid, "runtime")
  }
  if(is.na(metrics(nl))) {
    notvalid <- c(notvalid, "metrics")
  }
  if(purrr::is_empty(variables(nl)) & purrr::is_empty(constants(nl))) {
    notvalid <- c(notvalid, "variables or constants")
  }

  stop(paste0("Error: To add a sim design to a nl object you need to define a proper experiment first. The following elements are missing without default: ", paste(notvalid, collapse=" ; ")))

}

#' Evaluate simdesign object
#'
#' @description Evaluate simdesign object
#'
#' @param nl nl object
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_eval_simdesign
#' @rdname util_eval_simdesign

util_eval_simdesign <- function(nl) {

  notvalid <- c()

  if(is.na(simmethod(nl))) {
    notvalid <- c(notvalid, "simmethod")
  }
  if(purrr::is_empty(siminput(nl))) {
    notvalid <- c(notvalid, "siminput")
  }
  if(is.na(simseeds(nl))) {
    notvalid <- c(notvalid, "simseeds")
  }

  stop(paste0("Error: To run a simulation you have to add a sim design to a nl object with a properly defined experiment. Please first initialize a nl object, then add a proper experiment and finally add a simdesign by using one of the provided simdesign functions. The following elements are missing without default: ", paste(notvalid, collapse=" ; ")))

}
