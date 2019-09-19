#' Evaluate if variables list of an experiment object is empty
#'
#' @description Evaluate if variables list of an experiment object is empty
#' @param nl nl object
#' @details
#' util_eval_variables checks if the variables list of an experiment within a nl object is empty.
#' It reports an error message if no variables were defined.
#' @aliases util_eval_variables
#' @rdname util_eval_variables
#' @keywords internal
util_eval_variables <- function(nl) {

  # Check if there are any variables defined
  if (length(getexp(nl, "variables")) == 0) {
    stop("Error: Experiment Variable list is empty.
         You need to define a variable list with at least one element!",
      call. = FALSE
    )
  }
}



#' Evaluate variables list of an experiment object for distinct simdesign
#'
#' @description Evaluate variables list of an experiment object for distinct simdesign
#' @param nl nl object
#' @details
#' util_eval_variables_distinct checks if the variables list of an experiment within a nl object has enough information to create a \link[nlrx]{simdesign_distinct}.
#' It reports an error message if at least one variable does not have a vector of distinct values or if there is a mismatch in length of these values vectors.
#' @aliases util_eval_variables_distinct
#' @rdname util_eval_variables_distinct
#' @keywords internal
util_eval_variables_distinct <- function(nl) {

  vars <- getexp(nl, "variables")
  vars.values.missing <- lapply(vars, function(x) is.null(x$values))
  vars.length.mismatch <- var(unlist(lapply(
    vars,
    function(x) length(x$values)
  ))) != 0

  vars.missing <- data.frame(cbind(
    values.missing = unlist(vars.values.missing),
    length = unlist(vars.length.mismatch)
  ))


  vars.missing$variable <- rownames(vars.missing)
  values.missing <- vars.missing %>% dplyr::filter(values.missing == TRUE)

  # Check if there are missing values
  if (nrow(values.missing) > 0) {
    stop(paste0(
      "Error: Variable definition incomplete for variables: ",
      values.missing$variable, ".
          To setup a distinct simulation design you need to provide for each
          variable a vector of distinct values (e.g. list(values=c(1,2,3,4)))."
    ), call. = FALSE)
  }

  length.mismatch <- vars.missing %>% dplyr::filter(length == TRUE)

  # Check if there are any variables defined
  if (nrow(length.mismatch) > 0) {
    stop(paste0("Error: Mismatch in vector length of variable values.
          The length of provided values vectors of experiment variables is not
                equal across all variables."), call. = FALSE)
  }
}






#' Evaluate variables list of an experiment object for full-factorial simdesign
#'
#' @description Evaluate variables list of an experiment object for full-factorial simdesign
#' @param nl nl object
#' @details
#' util_eval_variables_ff checks if the variables list of an experiment within a nl object has enough information to create a \link[nlrx]{simdesign_ff}.
#' It reports an error message if at least one variable does not have a defined sequence (min, max, step) or a vector of distinct values.
#' @aliases util_eval_variables_ff
#' @rdname util_eval_variables_ff
#' @keywords internal
util_eval_variables_ff <- function(nl) {

  # The function checks if there is enough variable information to create a ff
  # simdesign
  # The ff design needs either a set of min, max and step or distinct values

  vars <- getexp(nl, "variables")
  vars.values.missing <- lapply(vars, function(x) is.null(x$values))
  vars.dist.missing <- lapply(vars, function(x) is.null(x$min) |
                                is.null(x$max) | is.null(x$step))
  vars.missing <- data.frame(cbind(values.missing = unlist(vars.values.missing),
                                   dist.missing = unlist(vars.dist.missing)))
  vars.missing$variable <- rownames(vars.missing)
  vars.missing <- vars.missing %>% dplyr::filter(values.missing == TRUE &
                                                   dist.missing == TRUE)

  # Check if there are any variables defined
  if (nrow(vars.missing) > 0) {
    stop(paste0("Error: Variable definition incomplete for variables: ",
    vars.missing$variable, ". To setup a full factorial simulation design you
    need to provide for each variable either a vector of distinct values
    (e.g. list(values=c(1,2,3,4))) or a sequence with min, max and step
    (e.g. list(min=1, max=4, step=1))."), call. = FALSE)
  }
}


#' Evaluate variables list of an experiment object for sensitivity analysis simdesigns
#'
#' @description Evaluate variables list of an experiment object for sensitivity analysis simdesigns
#' @param nl nl object
#' @details
#' util_eval_variables_sa checks if the variables list of an experiment within a nl object has enough information to create a sensitivity analysis simdesign.
#' It reports an error message if at least one variable does not have a defined distribution (min, max, qfun).
#' @aliases util_eval_variables_sa
#' @rdname util_eval_variables_sa
#' @keywords internal
util_eval_variables_sa <- function(nl) {

  # The function checks if there is enough variable information to create a
  # sensitivity analysis simdesign
  # The sa designs need a set of min, max and qfun

  vars <- getexp(nl, "variables")
  vars.dist.missing <- lapply(vars, function(x) is.null(x$min) |
      is.null(x$max) |
      is.null(x$qfun))
  vars.missing <- data.frame(dist.missing = unlist(vars.dist.missing))
  vars.missing$variable <- rownames(vars.missing)
  vars.missing <- vars.missing %>% dplyr::filter(dist.missing == TRUE)

  # Check if there are any variables defined
  if (nrow(vars.missing) > 0) {
    stop(paste0(
      "Error: Variable definition incomplete for variables: ",
      vars.missing$variable, ". To setup a sensitivity analysis simulation
      design you need to provide for each variable a distribution with min, max
      and qfun (e.g. list(min=1, max=4, qfun=\"qunif\"))."
    ), call. = FALSE)
  }
}



#' Evaluate variables list of an experiment object for optimization simdesigns
#'
#' @description Evaluate variables list of an experiment object for optimization simdesigns
#' @param nl nl object
#' @details
#' util_eval_variables_op checks if the variables list of an experiment within a nl object has enough information to create an optimization simdesign.
#' It reports an error message if at least one variable does not have a defined range (min, max).
#' @aliases util_eval_variables_op
#' @rdname util_eval_variables_op
#' @keywords internal
util_eval_variables_op <- function(nl) {

  # The function checks if there is enough variable information to create an
  # optimization simdesign
  # The optimization designs need a set of min and max

  vars <- getexp(nl, "variables")
  vars.dist.missing <- lapply(vars, function(x) is.null(x$min) | is.null(x$max))
  vars.missing <- data.frame(dist.missing = unlist(vars.dist.missing))
  vars.missing$variable <- rownames(vars.missing)
  vars.missing <- vars.missing %>% dplyr::filter(dist.missing == TRUE)

  # Check if there are any variables defined
  if (nrow(vars.missing) > 0) {
    stop(paste0(
      "Error: Variable definition incomplete for variables: ",
      vars.missing$variable, ". To setup an optimization simulation design you
    need to provide for each variable a distribution with min and max
    (e.g. list(min=1, max=4))."
    ), call. = FALSE)
  }
}




#' Evaluate if constants list of an experiment object is empty
#'
#' @description Evaluate if constants list of an experiment object is empty
#' @param nl nl object
#' util_eval_constants checks if the constants list of an experiment within a nl object is empty.
#' It reports an error message if no constants were defined. This evaluation is only done for \link[nlrx]{simdesign_simple}.
#' @aliases util_eval_constants
#' @rdname util_eval_constants
#' @keywords internal
util_eval_constants <- function(nl) {
  if (length(getexp(nl, "constants")) == 0) {
    stop("Error: Experiment constants list is empty.
         You need to define a constants list with at least one element!",
      call. = FALSE
    )
  }
}

#' Evaluate all slots of an experiment object
#'
#' @description Evaluate all slots of an experiment object
#' @param nl nl object
#' @details
#' util_eval_experiment checks if the information stored within the experiment slots are valid.
#' @aliases util_eval_experiment
#' @rdname util_eval_experiment
#' @keywords internal
util_eval_experiment <- function(nl) {
  notvalid <- c()

  if (is.na(getexp(nl, "expname"))) {
    notvalid <- c(notvalid, "expname")
  }
  if (!getexp(nl, "tickmetrics") %in% c("true", "false")) {
    notvalid <- c(notvalid, "tickmetrics must be either \"true\" or \"false\"")
  }
  if (is.na(getexp(nl, "outpath"))) {
    notvalid <- c(notvalid, "outpath")
  }
  if (anyNA(getexp(nl, "metrics"))) {
    notvalid <- c(notvalid, "metrics")
  }

  if (length(notvalid) > 0) {
    stop(paste0("To add a sim design to a nl object you need to
    define a proper experiment first. The following elements are missing without
    default: ", paste(notvalid, collapse = " ; ")), call. = FALSE)
  }

  # Check if a NetLogo parameter has been defined in variables AND constants:
  if (any(names(getexp(nl, "variables")) %in% names(getexp(nl, "constants")))) {
    stop(paste0(
      "Same netlogo parameter present in variables AND constants: ",
      paste(names(getexp(nl, "variables"))[names(getexp(nl, "variables")) %in%
                                             names(getexp(nl, "constants"))],
            collapse = " ; ")), call. = FALSE)
  }
}

#' Evaluate all slots of a simdesign object
#'
#' @description Evaluate all slots of a simdesign object
#' @param nl nl object
#' @details
#' util_eval_simdesign checks if the information stored within the simdesign slots are valid.
#' @aliases util_eval_simdesign
#' @rdname util_eval_simdesign
#' @keywords internal
util_eval_simdesign <- function(nl) {
  notvalid <- c()

  if (is.na(getsim(nl, "simmethod"))) {
    notvalid <- c(notvalid, "simmethod")
  }
  if (purrr::is_empty(getsim(nl, "siminput"))) {
    notvalid <- c(notvalid, "siminput")
  }
  if (anyNA(getsim(nl, "simseeds"))) {
    notvalid <- c(notvalid, "simseeds")
  }

  if (length(notvalid) > 0) {
    stop(paste0("Error: To run a simulation you have to add a simdesign to a nl
    object with a properly defined experiment. Please first initialize a nl
    object, then add a proper experiment, and finally add a simdesign by using
    one of the provided simdesign functions. The following elements are missing
    without default: ", paste(notvalid, collapse = " ; ")), call. = FALSE)
  }
}

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
    stop(paste0(
      "Defined variables were not found in NetLogo model: ",
      nonvalid_variables,
      ". Check report_model_parameters() function to show valid parameters."
    ), call. = FALSE)
  }

  if (length(nonvalid_constants) > 0) {
    stop(paste0(
      "Defined constants were not found in NetLogo model: ",
      nonvalid_constants,
      ". Check report_model_parameters() function to show valid parameters."
    ), call. = FALSE)
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
      "One ore more parameters of the NetLogo model are neither defined in constants or variables slot of the experiment: ",
      netlogo_variables_not_defined_in_exp,
      ". When running this experiment, these NetLogo parameters will be setup with their current default value from the NetLogo Interface."
    ), call. = FALSE)
  }

  # Check if NetLogo parameters have been defined in variables AND constants:
  # Check if a NetLogo parameter has been defined in variables AND constants:
  if (any(names(getexp(nl, "variables")) %in% names(getexp(nl, "constants")))) {
    stop(paste0(
      "Same netlogo parameter present in variables AND constants: ",
      paste(names(getexp(nl, "variables"))[names(getexp(nl, "variables")) %in%
                                             names(getexp(nl, "constants"))],
            collapse = " ; ")), call. = FALSE)
  }

  # If no error message occurred print a message:
  message("All defined variables and constants are valid!")
}


.util_check_siminput_tibble <- function(nl, new_simdesign) {

  ## Check if there are any NAs in the variables columns:

  if (isTRUE(anyNA(new_simdesign@siminput[names(nl@experiment@variables)]))) {
    warning("The generated siminput tibble of the simdesign contains NA values. You may need to increase the number of samples!",
            call. = FALSE)
  }


}
