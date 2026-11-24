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
#' The procedure reads the behavior space experiments of the main model file
#' (.nlogox) and reports one list per experiment: its \code{name}, the
#' \code{definitions} (procedures, time limit, metrics, ...) as a named character
#' vector, the \code{parameters} (constants) and the parameter sets of its
#' \code{subexperiments} as tibbles with the columns name, type and values.
#'
#' @aliases nldoc_table_bs
#' @rdname nldoc_table_bs
#' @keywords internal
nldoc_table_bs <- function(modelfiles)
{
  ## Filter file names for main model file:
  modelpath <- modelfiles[tolower(tools::file_ext(modelfiles)) == "nlogox"][1]

  ## Find all experiments:
  model.xml <- xml2::read_xml(modelpath)
  experiments <- xml2::xml_find_all(model.xml, ".//experiments/experiment")

  lapply(experiments, function(experiment) {

    ## Text of child elements, several ones (metrics) joined:
    text_of <- function(path) {
      paste(xml2::xml_text(xml2::xml_find_all(experiment, path)), collapse = "; ")
    }
    attr_of <- function(name) {
      value <- xml2::xml_attr(experiment, name)
      if (is.na(value)) "" else value
    }

    list(
      name = attr_of("name"),
      definitions = c(
        "Pre experiment" = text_of("./preExperiment"),
        "Setup" = text_of("./setup"),
        "Go" = text_of("./go"),
        "Post run" = text_of("./postRun"),
        "Post experiment" = text_of("./postExperiment"),
        "Time limit" = attr_of("timeLimit"),
        "Exit condition" = text_of("./exitCondition"),
        "Metrics" = text_of("./metrics/metric"),
        "Measure each tick" = attr_of("runMetricsEveryStep"),
        "Measure condition" = text_of("./runMetricsCondition"),
        "Repetitions" = attr_of("repetitions")
      ),
      parameters = nldoc_bs_valuesets(xml2::xml_find_first(experiment, "./constants")),
      subexperiments = lapply(xml2::xml_find_all(experiment, "./subExperiments/subExperiment"),
                              nldoc_bs_valuesets)
    )
  })
}

#' Read the value sets of a behavior space experiment
#'
#' @description Read the enumerated and stepped value sets of a behavior space
#' experiment (constants) or sub-experiment
#'
#' @param node xml node holding the value sets
#'
#' @return tibble with the columns name, type and values
#'
#' @aliases nldoc_bs_valuesets
#' @rdname nldoc_bs_valuesets
#' @keywords internal
nldoc_bs_valuesets <- function(node)
{
  parameters <- tibble::tibble(name = character(), type = character(), values = character())

  if (inherits(node, "xml_missing")) {
    return(parameters)
  }

  for (valueset in xml2::xml_children(node))
  {
    name <- xml2::xml_attr(valueset, "variable")

    ## Enumerated value set:
    if (xml2::xml_name(valueset) == "enumeratedValueSet")
    {
      values <- xml2::xml_attr(xml2::xml_find_all(valueset, "./value"), "value")
      parameters <- rbind(parameters, tibble::tibble(
        name = name,
        type = ifelse(length(values) > 1, "enumerated", "constant"),
        values = paste(values, collapse = ", ")
      ))
    }

    ## Stepped value set:
    if (xml2::xml_name(valueset) == "steppedValueSet")
    {
      parameters <- rbind(parameters, tibble::tibble(
        name = name,
        type = "sequence",
        values = paste0("from: ", xml2::xml_attr(valueset, "first"),
                        "; to: ", xml2::xml_attr(valueset, "last"),
                        "; by: ", xml2::xml_attr(valueset, "step"))
      ))
    }
  }

  return(parameters)
}
