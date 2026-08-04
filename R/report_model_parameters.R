#' Report globals from a NetLogo model that is defined within a nl object
#'
#' @description Report globals from a NetLogo model that is defined within a nl
#'  object
#'
#' @param nl nl object with a defined modelpath that points to a NetLogo model (.nlogox)
#'
#' @details
#'
#' The function reads the NetLogo model file that is defined within the nl object
#'  and reports all global parameters that are defined as widget elements on
#'  the GUI of the NetLogo model.
#' Only globals that are found by this function are valid globals that can be
#'  entered into the variables or constants vector of an experiment object.
#'
#'
#' @examples
#' \dontrun{
#' nl <- nl_lhs
#' report_model_parameters(nl)
#' }
#'
#' @aliases report_model_parameters
#' @rdname report_model_parameters
#'
#' @export

report_model_parameters <- function(nl) {

  ## Check if model exists:
  if(!file.exists(nl@modelpath)){
    stop("nl@modelpath does not exist on local file system. Cannot report model parameters!")
  }

  file_type <- tools::file_ext(nl@modelpath) # detect file type (.nlogox required)

  # Catch file format issues
  if (file_type == "nlogo") {
    stop(
      "The .nlogo format is no longer supported. ",
      "Please convert your model to .nlogox format (NetLogo 7+). ",
      "NetLogo can automatically upgrade models via the interface."
    )
  } else if (file_type == "nlogox") {

    # Parse .nlogox
    model.code <- xml2::read_xml(getnl(nl, "modelpath")) # Open model as XML
    widgets <- xml2::xml_find_first(model.code, ".//widgets") # Find widgets block

    if (inherits(widgets, "xml_missing")) {
      stop("No <widgets> block found in .nlogox model file")
    }

    widget_nodes <- xml2::xml_children(widgets) # Get all widgets

    # Loop over widgets
    modelparam <- list()
    for (node in widget_nodes) {

      widget_type <- xml2::xml_name(node)

      # SLIDER
      if (widget_type == "slider") {
        name <- xml2::xml_attr(node, "variable")

        entry <- list(
          type = "SLIDER",
          value = as.numeric(xml2::xml_attr(node, "default")),
          min = as.numeric(xml2::xml_attr(node, "min")),
          max = as.numeric(xml2::xml_attr(node, "max")),
          incr = as.numeric(xml2::xml_attr(node, "step"))
        )

        modelparam[[name]] <- entry
      }

      # SWITCH
      if (widget_type == "switch") {
        name <- xml2::xml_attr(node, "variable")

        entry <- list(
          type = "SWITCH",
          value = tolower(xml2::xml_attr(node, "on")) == "true"
        )

        modelparam[[name]] <- entry
      }

      # INPUTBOX
      if (widget_type == "inputBox") {
        name <- xml2::xml_attr(node, "variable")

        entry <- list(
          type = "INPUTBOX",
          value = xml2::xml_text(node),
          entrytype = xml2::xml_attr(node, "type")
        )

        modelparam[[name]] <- entry
      }

      # CHOOSER
      if (widget_type == "chooser") {
        name <- xml2::xml_attr(node, "variable")

        choice_nodes <- xml2::xml_find_all(node, "./choice")
        # Different .xml syntax for text content vs attributes:
        choice_value <- function(x) {
          if (!is.na(xml2::xml_attr(x, "value"))) {
            xml2::xml_attr(x, "value")
          } else {
            xml2::xml_text(x)
          }
        }
        validvalues <- vapply(choice_nodes, choice_value, character(1))

        select_id <- as.numeric(xml2::xml_attr(node, "current")) + 1
        selectedvalue <- validvalues[select_id]

        entry <- list(
          type = "CHOOSER",
          value = selectedvalue,
          validvalues = validvalues
        )

        modelparam[[name]] <- entry
      }
    }

    return(modelparam)

  # If the fiel type was not supported
  } else {
    stop("Extension: ", file_type, " is not supported. Please use .nlogox (NetLogo 7+)")
  }
}

