
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


  if (getnl(nl, "nlversion") >= "7.0.0") {
    check_constants_and_variables_logolink(nl)
  }

  # If no error message occurred print a message:
  message("All defined variables and constants are valid!")
}



#' Backend function for eval_variables_constants (NetLogo 7+)
#'
#' @description Checks constants and variable formatting of the experiment as required by Logolink
#'
#' @param nl nl object
#' @return Character vector of detected issues
#' @details
#' Checks constants and variable formatting of the experiment, as NetLogo 7+ (or at least Logolink)
#' has different formatting specifications than before.
#' @keywords internal

check_constants_and_variables_logolink <- function(nl) {
  issues <- character()

  # Check CONSTANTS
  constants <- nl@experiment@constants
  if (length(constants) > 0) {
    for (index in names(constants)) {
      current_content <- constants[[index]]

      if (is.list(current_content) && !is.null(names(current_content))) next

      issues <- c(issues, check_value_format(current_content, index))
    }
  }

  # Check VARIABLES
  variables <- nl@experiment@variables
  if (length(variables) > 0) {
    for (index in names(variables)) {
      current_content <- variables[[index]]

      if (is.list(current_content)) {
        for (subnm in names(current_content)) {
          issues <- c(
            issues,
            check_value_format(current_content[[subnm]], paste0(index, "$", subnm))
          )
        }
      } else {
        issues <- c(issues, check_value_format(current_content, index))
      }
    }
  }

  # Show issues
  if (length(issues) > 0) {
    warning(
      paste(
        c(
          "Potential NetLogo 7+ formatting issues detected:",
          paste0("- ", issues),
          "Strings should be plain character values, booleans should be TRUE/FALSE."
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  return(issues)
}



#' Helper function for \code{check_constants_and_variables_logolink()}
#'
#' @description Checks variables and constants for patterns problematic in NetLogo 7+
#'
#' @param x value to check
#' @param name optional name of the checked entry
#' @return Character vector of detected issues
#' @keywords internal

check_value_format <- function(x, name = NULL) {
  issues <- character() # Collect issue messages

  # Check for boolean-strings that NetLogo could expect as boolean
  if (is.character(x)) {
    if (length(x) == 1 && x %in% c("true", "false")) {
      issues <- c(
        issues,
        paste0(
          if (!is.null(name)) paste0("'", name, "': ") else "",
          "Boolean is given as character ('", x, "'). Confirm that this is intended."
        )
      )
    }

    # Check for manually quoted strings "\"example\"", which should be formatted with simple quotations from NetLogo 7 onward.
    if (length(x) == 1 && grepl('^".*"$', x)) {
      issues <- c(
        issues,
        paste0(
          if (!is.null(name)) paste0("'", name, "': ") else "",
          "String was manually quoted (", x, "). Netlogo 7+ expects plain character values."
        )
      )
    }
  }

  return(issues)
}


