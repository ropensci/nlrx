#' Report globals from a NetLogo model that is defined within a nl object
#'
#' @description Report globals from a NetLogo model that is defined within a nl
#'  object
#'
#' @param nl nl object with a defined modelpath that points to a NetLogo model
#'  (.nlogo or .nlogox)
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

  file_type <- tools::file_ext(nl@modelpath) # detect file type (.nlogo for NetLogo <7.0 or .nlogox for NetLogo >=7.0)

  if (file_type == "nlogo") {
    if (getnl(nl, "nlversion") >= "7.0.0") {warning("You are using Netlogo > 7, which expects .nlogox and not .nlogo")}
    modelparam <- nlogo_parser(nl)
    return(modelparam)
  } else if (file_type == "nlogox") {
    if (getnl(nl, "nlversion") < "7.0.0") {warning("You are using Netlogo < 7, which uses .nlogo and not .nlogox. The model may still run.")}
    modelparam <- nlogox_parser(nl)
    return(modelparam)
  } else {
    stop("Extension: ", file_type, " is not supported (ensure .nlogo or .nlogox is used)")
  }
}


#' Backend function for report_model_parameters (NetLogo 7+)
#' @description Report globals from a .nlogox file that is defined in the nl object.
#' @param nl see \code{report_model_parameters()}
#' @details
#' reads .nlogox files for \code{report_model_parameters()}

nlogox_parser <- function(nl) {

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
}





#' Backend function for report_model_parameters (NetLogo <7)
#' @description Report globals from a .nlogo file that is defined in the nl object.
#' @param nl see \code{report_model_parameters()}
#' @details
#' reads .nlogo files for \code{report_model_parameters()}

nlogo_parser <- function(nl) {
  ## Open the model as string
  model.code <- readLines(getnl(nl, "modelpath"))

  ## Find the line in the NetLogoCode where the interface definiton starts
  ## (separator: @#$#@#$#@)
  model.code.s1 <- grep("@#$#@#$#@", model.code, fixed = TRUE)[1]

  ## Remove model code before first separator:
  if (is.na(model.code.s1) == FALSE) {
    model.code <- model.code[(model.code.s1 + 1):length(model.code)]
  }
  ## Find second separator where interface definiton ends:
  model.code.s2 <- grep("@#$#@#$#@", model.code, fixed = TRUE)[1]

  ## Remove model code following second separator:
  if (is.na(model.code.s1) == FALSE) {
    model.code <- model.code[1:(model.code.s2 - 1)]
  }

  ## Extract the parameters and their values line by line:
  modelparam <- list()

  for (i in seq_len(length(model.code)))
  {
    ## Read current line from model code
    l <- model.code[i]

    ## Check if l is a definition element and of what kind:
    if (l %in% c("SLIDER", "SWITCH", "INPUTBOX", "CHOOSER")) {
      if (l == "SLIDER") {
        name <- as.character(model.code[i + 5])

        entry <- list(
          type = l,
          value = as.numeric(as.character(model.code[i + 9])),
          min = as.numeric(as.character(model.code[i + 7])),
          max = as.numeric(as.character(model.code[i + 8])),
          incr = as.numeric(as.character(model.code[i + 10]))
        )
      }
      if (l == "SWITCH") {
        name <- as.character(model.code[i + 5])

        entry <- list(
          type = l,
          value = ifelse(as.numeric(as.character(model.code[i + 8])) == 1,
                         TRUE, FALSE)
        )
      }
      # nocov start
      if (l == "INPUTBOX") {
        name <- as.character(model.code[i + 5])

        entry <- list(
          type = l,
          value = model.code[i + 6],
          entrytype = model.code[i + 9]
        )
      }
      # nocov end
      if (l == "CHOOSER") {
        name <- as.character(model.code[i + 5])

        validvalues <- scan(text = (model.code[i + 7]), what = "", quiet = TRUE)
        select_id <- (as.numeric(as.character(model.code[i + 8])) + 1)
        selectedvalue <- validvalues[select_id]

        entry <- list(
          type = l,
          value = selectedvalue,
          validvalues = validvalues
        )
      }

      ## Store in data.frame:
      modelparam[[name]] <- entry
    }
  }

  return(modelparam)
}

