#' Evaluate variables list of an experiment object
#'
#' @description Evaluate variables list of an experiment object
#' @param nl nl object
#' @aliases util_eval_variables
#' @rdname util_eval_variables

util_eval_variables <- function(nl) {

  # Check if there are any variables defined
  if(length(getexp(nl, "variables")) == 0){
    stop("Error: Experiment Variable list is empty.
         You need to define a variable list with at least one element!")
  }
}

#' Evaluate constants list of an experiment object
#'
#' @description Evaluate constants list of an experiment object
#' @param nl nl object
#' @aliases util_eval_constants
#' @rdname util_eval_constants

util_eval_constants <- function(nl) {

  if(length(getexp(nl, "constants")) == 0){
    stop("Error: Experiment constants list is empty.
         You need to define a constants list with at least one element!")
  }
}

#' Evaluate experiment object
#'
#' @description Evaluate experiment object
#' @param nl nl object
#' @aliases util_eval_experiment
#' @rdname util_eval_experiment

util_eval_experiment <- function(nl) {

  notvalid <- c()

  if(is.na(getexp(nl, "expname"))) {
    notvalid <- c(notvalid, "expname")
  }
  if(is.na(getexp(nl, "outpath"))) {
    notvalid <- c(notvalid, "outpath")
  }
  if(is.na(getexp(nl, "runtime"))) {
    notvalid <- c(notvalid, "runtime")
  }
  if(anyNA(getexp(nl, "metrics"))) {
    notvalid <- c(notvalid, "metrics")
  }
  if(purrr::is_empty(getexp(nl, "variables")) & purrr::is_empty(getexp(nl, "constants"))) {
    notvalid <- c(notvalid, "variables or constants")
  }

  if (length(notvalid) > 0) {
    stop(paste0("Error: To add a sim design to a nl object you need to define a proper experiment first.
                The following elements are missing without default: ",
                paste(notvalid, collapse=" ; ")))
  }
}

#' Evaluate simdesign object
#'
#' @description Evaluate simdesign object
#' @param nl nl object
#' @aliases util_eval_simdesign
#' @rdname util_eval_simdesign

util_eval_simdesign <- function(nl) {

  notvalid <- c()

  if(is.na(getsim(nl, "simmethod"))) {
    notvalid <- c(notvalid, "simmethod")
  }
  if(purrr::is_empty(getsim(nl, "siminput"))) {
    notvalid <- c(notvalid, "siminput")
  }
  if(anyNA(getsim(nl, "simseeds"))) {
    notvalid <- c(notvalid, "simseeds")
  }

  if (length(notvalid) > 0) {
    stop(paste0("Error: To run a simulation you have to add a simdesign to a nl object with a properly defined experiment.
                Please first initialize a nl object, then add a proper experiment,
                and finally add a simdesign by using one of the provided simdesign functions.
                The following elements are missing without default: ",
                paste(notvalid, collapse=" ; ")))
  }
}

#' Evaluate variable validity
#'
#' @description Evaluate variables and constants defined in experiment
#' @param nl nl object
#' @details
#' This function checks if the variables and constants that are defined in the experiment are valid.
#' It loads the model code of the NetLogo model and checks if these variables and constants really exist.
#' In case of nonvalid entries, the function throws an error message, indicating which variables and constants are not valid.
#' Please note, that this function might fail if the supported modelpath does not point to an existing nlogo file.
#' This might for example happen, if the modelpath is set up for a remote cluster execution.
#'
#'#' @examples
#' \dontrun{
#' eval_variables_constants(nl)
#' }
#'
#' @aliases eval_variables_constants
#' @rdname eval_variables_constants
#' @export
eval_variables_constants <- function(nl) {

  variables_validity <-  unlist(lapply(names(getexp(nl, "variables")),
                                      function(x) {x %in% names(load_model_parameters(nl))}))

  constants_validity <-  unlist(lapply(names(getexp(nl, "constants")),
                                      function(x) {x %in% names(load_model_parameters(nl))}))

  nonvalid_variables <- names(which(variables_validity==FALSE))
  nonvalid_constants <- names(which(constants_validity==FALSE))

  if (length(nonvalid_variables) > 0 | length(nonvalid_constants) > 0) {

    stop(paste0("Error: Defined variables were not found in NetLogo model: ",
                nonvalid_variables,
                " and defined constants were not found in NetLogo model: ",
                nonvalid_constants,
                ". Check load_model_parameters() function to show valid parameters."))
  }

  message("All defined variables and constants are valid!")
}
