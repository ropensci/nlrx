#' Read NetLogo model code from files
#'
#' @description Read NetLogo model code from files
#'
#' @param modelfiles vector of filepaths to model files
#'
#' @return vector of strings containing NetLogo model code
#'
#' @details
#'
#' The procedure reads text from the provided model files and reports the code as a vector of strings.
#'
#' @aliases nldoc_read_nlogox
#' @rdname nldoc_read_nlogox
#' @keywords internal

nldoc_read_nlogox <- function(modelfiles) {
  modelcode <- ""
  guicode <- NA
  infotabcode <- NA
  nlversion <- NA
  bscode <- NA

  for (i in 1:length(modelfiles)) {
    # Check if file exists
    if (!file.exists(modelfiles[i])) {
      stop("File does not exist: ", modelfiles[i])
    }

    # Check file extension
    file_type <- tools::file_ext(modelfiles[i])

    # Handle .nls files (plain text, code only)
    if (file_type == "nls") {
      # Read all lines as model code
      modelcode.i <- readLines(modelfiles[i], warn = FALSE)

      # .nls files don't contain GUI, info, or BehaviorSpace
      guicode.i <- ";; .nls files do not contain GUI elements"
      infotabcode.i <- ";; .nls files do not contain info tab content"
      nlversion.i <- "Unknown"
      bscode.i <- ";; .nls files do not contain BehaviorSpace experiments"

    } else if (file_type == "nlogox") {
      # Parse .nlogox XML
      model.xml <- xml2::read_xml(modelfiles[i])

      # Extract <code> section (model procedures)
      code_node <- xml2::xml_find_first(model.xml, ".//code")
      if (inherits(code_node, "xml_missing")) {
        modelcode.i <- ";; No procedures found in this model"
      } else {
        modelcode.i <- xml2::xml_text(code_node)
      }

      # Extract <widgets> section (GUI elements)
      widgets_node <- xml2::xml_find_first(model.xml, ".//widgets")
      if (inherits(widgets_node, "xml_missing")) {
        guicode.i <- ";; The model does not employ GUI elements"
      } else {
        guicode.i <- xml2::xml_text(widgets_node)
      }

      # Extract <info> section (info tab)
      info_node <- xml2::xml_find_first(model.xml, ".//info")
      if (inherits(info_node, "xml_missing")) {
        infotabcode.i <- ";; No info tab content found"
      } else {
        infotabcode.i <- xml2::xml_text(info_node)
      }

      # Extract NetLogo version (from attribute or tag)
      version_node <- xml2::xml_find_first(model.xml, ".//netLogoVersion")
      if (inherits(version_node, "xml_missing")) {
        # Try version attribute on <model> tag
        nlversion.i <- xml2::xml_attr(model.xml, "version")
        if (is.na(nlversion.i)) {
          nlversion.i <- "Unknown"
        }
      } else {
        nlversion.i <- xml2::xml_text(version_node)
      }

      # Extract <experiments> section (BehaviorSpace)
      experiments_node <- xml2::xml_find_first(model.xml, ".//experiments")
      if (inherits(experiments_node, "xml_missing")) {
        bscode.i <- ";; No BehaviorSpace experiments defined"
      } else {
        bscode.i <- xml2::xml_text(experiments_node)
      }

    } else {
      stop(
        "The '", file_type, "' format is not supported. ",
        "Please use .nlogox (NetLogo 7+) or .nls (procedure files)."
      )
    }

    # Bind together in modelcode vector (for multiple files)
    modelcode <- c(modelcode, "\n", modelcode.i)

    # For multiple files, only first file's metadata is kept
    if (i == 1) {
      guicode <- guicode.i
      infotabcode <- infotabcode.i
      nlversion <- nlversion.i
      bscode <- bscode.i
    }
  }

  nlogoxcode <- list(
    modelcode = split_to_lines(modelcode),
    guicode = split_to_lines(guicode),
    infotabcode = split_to_lines(infotabcode),
    nlversion = nlversion,
    bscode = split_to_lines(bscode)
  )

  return(nlogoxcode)
}



# Helper: Split strings into line-by-line vectors
split_to_lines <- function(x) {
  if (length(x) == 0 || is.na(x[1])) {
    return(character(0))
  }
  # Remove empty strings
  x <- x[nchar(x) > 0]
  # Split any elements containing newlines
  if (any(grepl("\n", x))) {
    x <- unlist(strsplit(x, "\r?\n"))
  }
  # Remove empty strings from split
  x <- x[x != ""]
  return(x)
}
