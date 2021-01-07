
#' Evaluate variable validity
#'
#' @description Evaluate variables and constants defined in experiment
#' @param nl nl object
#' @details
#' This function checks if the variables and constants that are defined in the
#' experiment are valid.
#' It loads the model code of the NetLogo model and checks if these variables
#' and constants really exist.
#' In case of nonvalid entries, the function throws an error message, indicating
#'  which variables and constants are not valid.
#' Please note, that this function might fail if the supported modelpath does
#' not point to an existing nlogo file.
#' This might for example happen, if the modelpath is set up for a remote
#' cluster execution.
#'
#' @examples
#' \dontrun{
#' nl <- nl_lhs
#' eval_variables_constants(nl)
#' }
#'
#' @aliases eval_variables_constants
#' @rdname eval_variables_constants
#' @export
eval_variables_constants <- function(nl) {
  variables_validity <- unlist(lapply(
    names(getexp(nl, "variables")),
    function(x) {
      x %in% names(report_model_parameters(nl))
    }
  ))

  constants_validity <- unlist(lapply(
    names(getexp(nl, "constants")),
    function(x) {
      x %in% names(report_model_parameters(nl))
    }
  ))

  nonvalid_variables <-
    names(getexp(nl, "variables")[which(variables_validity == FALSE)])
  nonvalid_constants <-
    names(getexp(nl, "constants")[which(constants_validity == FALSE)])

  if (length(nonvalid_variables) > 0) {

    stop(paste0("Defined variables were not found in NetLogo model:\n",
                paste(nonvalid_variables, collapse = "\n"),
                "\nCheck report_model_parameters() function to show valid parameters."),
         call. = FALSE)
  }

  if (length(nonvalid_constants) > 0) {

    stop(paste0("Defined constants were not found in NetLogo model:\n",
                paste(nonvalid_constants, collapse = "\n"),
                "\nCheck report_model_parameters() function to show valid parameters."),
         call. = FALSE)
  }

  # Check if NetLogo model has parameters that are neither defined in constants
  # or variables and print a warning that they will be setup
  # with the default value from the NetLogo gui
  netlogo_variables_defined_in_exp <-
    names(report_model_parameters(nl)) %in% c(names(getexp(nl, "variables")),
                                              names(getexp(nl, "constants")))
  netlogo_variables_not_defined_in_exp <-
    names(report_model_parameters(nl))[which(netlogo_variables_defined_in_exp
                                             == FALSE)]
  if (length(netlogo_variables_not_defined_in_exp) > 0) {
    warning(paste0(
      "Parameters of the NetLogo model are neither defined in constants or variables slot of the experiment:\n",
      paste(netlogo_variables_not_defined_in_exp, collapse = "\n"),
      "\nWhen running this experiment, these NetLogo parameters will be setup with their current default value from the NetLogo Interface."
    ), call. = FALSE)
  }

  # Check if NetLogo parameters have been defined in variables AND constants:
  # Check if a NetLogo parameter has been defined in variables AND constants:
  if (any(names(getexp(nl, "variables")) %in% names(getexp(nl, "constants")))) {
    stop(paste0(
      "Same netlogo parameter present in variables AND constants:\n",
      paste(names(getexp(nl, "variables"))[names(getexp(nl, "variables")) %in%
                                             names(getexp(nl, "constants"))],
            collapse = "\n")), call. = FALSE)
  }

  # If no error message occurred print a message:
  message("All defined variables and constants are valid!")
}

