#' Write NetLogo documentation
#'
#' @description Write NetLogo documentation
#'
#' @param noxygen list with parsed and processed noxygen tags from NetLogo model code
#' @param modelname title of the documentation if the model code has no @model header
#' @param noxygen_it list with parsed and processed infotab strings
#' @param noxygen_gui list with parsed and processed noxygen tags from NetLogo GUI elements
#' @param noxygen_bs list with parsed and processed noxygen tags from NetLogo behavior space experiments
#' @param outpath Path to folder where rendered documentation should be created
#' @param output_format either "html", "pdf" or "docx"
#' @param number_sections TRUE/FALSE, if TRUE sections in the documentation will be numbered
#' @param theme markdown theme, supported themes are "journal", "cerulean", "flatly", "readable", "spacelab", "united", "cosmo"
#' @param date date that is printed in the documentation header
#' @param toc TRUE/FALSE, if TRUE the documentation contains a table of contents - only for html and pdf output format
#'
#'
#' @return list containing NetLogo GUI elements
#'
#' @details
#'
#' The procedure uses parsed and processed noxygen tags to create and render a markdown documentation in the specified format.
#'
#' @aliases nldoc_write_nldoc
#' @rdname nldoc_write_nldoc
#' @keywords internal

nldoc_write_nldoc <- function(noxygen, noxygen_it, noxygen_gui, noxygen_bs, outpath, output_format, number_sections, theme, date, toc,
                              modelname = "Untitled Model")
{

  # Get all the values from the UI elements:
  mdfile <- file.path(outpath, "nldoc.md")

  # Change the value from the format field, to a pandoc compatible format description:
  if (output_format == "html")
  {
    output_format <- "html_document"
  }
  if (output_format == "pdf")
  {
    output_format <- "pdf_document"
  }
  if (output_format == "docx")
  {
    output_format <- "word_document"
  }

  # Title and author from the @model header (the first one, if there are
  # several), with fallback values for models without a header:
  model <- if (nrow(noxygen$header) > 0) noxygen$header$model[1] else NA_character_
  author <- if (nrow(noxygen$header) > 0) noxygen$header$author[1] else NA_character_

  if (is.na(model) || model == "") {
    model <- modelname
  }
  if (is.na(author) || author == "") {
    author <- "Unknown"
  }

  # write preambel for the markdown file (define format, style, ...)
  # Common lines:
  preambel <- c(
    "---",
    paste0("title: \"", model, "\""),
    paste0("author: \"", author, "\""),
    paste0("date: \"", date, "\""),
    "output:",
    paste0("  ", output_format, ":")
  )

  # Format-specific options:
  if (output_format == "html_document") {
    preambel <- c(preambel,
                  paste0("    toc: ", toc),
                  paste0("    number_sections: ", number_sections),
                  paste0("    theme: ", theme))
  } else if (output_format == "pdf_document") {
    preambel <- c(preambel,
                  paste0("    toc: ", toc),
                  paste0("    number_sections: ", number_sections))
  }

  # Close preamble:
  preambel <- c(preambel, "---", "")


  # Initialize string
  noxygencode <- ""

  # Process the information from global (if any are documented)
  if (nrow(noxygen$global) > 0)
  {
    noxygencode <- c(noxygencode, "# Global")
  }

  for (i in seq_len(nrow(noxygen$global)))
  {
    noxygen.global.i <- noxygen$global[i,]
    noxygencode.i <- paste0("## ", noxygen.global.i$global)
    noxygencode.i <- c(noxygencode.i, "#### Description {-}")
    noxygencode.i <- c(noxygencode.i, noxygen.global.i$description)


    ## Add code if provided:
    if (!is.na(noxygen.global.i$code))
    {
      noxygencode.i <- c(noxygencode.i, "```")
      noxygencode.i <- c(noxygencode.i, paste0(noxygen.global.i$code[[1]]))
      noxygencode.i <- c(noxygencode.i, "```")
    }

    noxygencode <- c(noxygencode, noxygencode.i)
  }

  # Process the information from procedures (if any are documented)
  if (nrow(noxygen$proc) > 0)
  {
    noxygencode <- c(noxygencode, "# Procedures")
  }

  for (i in seq_len(nrow(noxygen$proc)))
  {
    noxygen.proc.i <- noxygen$proc[i,]
    noxygencode.i <- paste0("## ", noxygen.proc.i$procedure)

    if (length(noxygen.proc.i$param[[1]]) > 0)
    {
      noxygencode.i <- c(noxygencode.i, "#### Parameters {-}")
      noxygencode.i <- c(noxygencode.i, paste(noxygen.proc.i$param[[1]], collapse="\r\n")) #collapse="</br>"
      noxygencode.i <- c(noxygencode.i, "\r\n")
    }
    if (length(noxygen.proc.i$return[[1]]) > 0)
    {
      noxygencode.i <- c(noxygencode.i, "#### Return {-}")
      noxygencode.i <- c(noxygencode.i, paste(noxygen.proc.i$return[[1]], collapse="\r\n")) #collapse="</br>"
      noxygencode.i <- c(noxygencode.i, "\r\n")
    }

    noxygencode.i <- c(noxygencode.i, "#### Description {-}")
    noxygencode.i <- c(noxygencode.i, noxygen.proc.i$description)

    ## Add code if provided:
    if (!is.na(noxygen.proc.i$code))
    {
      noxygencode.i <- c(noxygencode.i, "```")
      noxygencode.i <- c(noxygencode.i, paste0(noxygen.proc.i$code[[1]]))
      noxygencode.i <- c(noxygencode.i, "```")
    }
    noxygencode.i <- c(noxygencode.i, "\r\n")

    noxygencode <- c(noxygencode, noxygencode.i)
  }

  ## Add gui elements if wanted:
  if (length(noxygen_gui) > 0 && !is.na(noxygen_gui[1]))
  {
    noxygencode <- c(noxygencode, "# GUI elements")
    # Table header:
    noxygencode <- c(noxygencode, "| Name | Type | Value | Properties |")
    noxygencode <- c(noxygencode, "| ---- | ---- | ----- | ---------- |")


    for(i in 1:length(noxygen_gui))
    {
      noxygen_gui.i <- noxygen_gui[[i]]
      name.i <- names(noxygen_gui)[i]
      type.i <- noxygen_gui.i$type
      value.i <- noxygen_gui.i$value

      # Remove type and vlaue from list:
      noxygen_gui.i$type <- NULL
      noxygen_gui.i$value <- NULL
      # Properties with several values (chooser options) are listed comma-separated:
      prop.i <- vapply(noxygen_gui.i, paste, character(1), collapse = ", ")
      prop.i <- paste(names(noxygen_gui.i), prop.i, sep=": ", collapse="; ")

      noxygencode <- c(noxygencode, paste("| ", nldoc_md_cell(name.i), " | ", type.i, " | ",
                                          nldoc_md_cell(value.i), " | ", nldoc_md_cell(prop.i), " |"))
    }
  }

  if (length(noxygen_it) > 0 && !is.na(noxygen_it[1]))
  {
    noxygencode <- c(noxygencode, "# Info Tab")


    for(i in 1:length(noxygen_it))
    {
      noxygencode <- c(noxygencode, noxygen_it[i])
    }
  }


  ## Add bs elements if wanted:
  if (length(noxygen_bs) > 0 && !is.na(noxygen_bs[1]))
  {
    noxygencode <- c(noxygencode, "# Behavior Space Experiments")

    ## The function generates tables for each experiment:
    for (experiment in noxygen_bs)
    {
      noxygencode.i <- paste0("## ", experiment$name)

      # Definitions (only those the experiment sets):
      definitions <- experiment$definitions[nzchar(experiment$definitions)]
      noxygencode.i <- c(noxygencode.i, "#### Definitions {-}")
      noxygencode.i <- c(noxygencode.i, "| Definition | Value |")
      noxygencode.i <- c(noxygencode.i, "| ---------- | ----- |")
      noxygencode.i <- c(noxygencode.i, paste("|", names(definitions), "|", nldoc_md_cell(definitions), "|"))

      # Parameter tables of the experiment and its sub-experiments:
      tables <- c(list("Parameters" = experiment$parameters),
                  stats::setNames(experiment$subexperiments,
                                  sprintf("Sub-experiment %d", seq_along(experiment$subexperiments))))

      for (j in seq_along(tables))
      {
        parameters.j <- tables[[j]]

        if (nrow(parameters.j) > 0)
        {
          noxygencode.i <- c(noxygencode.i, paste0("#### ", names(tables)[j], " {-}"))
          noxygencode.i <- c(noxygencode.i, "| Parameter | Type | Values |")
          noxygencode.i <- c(noxygencode.i, "| --------- | ---- | ------ |")
          noxygencode.i <- c(noxygencode.i, paste("|", nldoc_md_cell(parameters.j$name), "|", parameters.j$type, "|",
                                                  nldoc_md_cell(parameters.j$values), "|"))
        }
      }

      # Attach current experiment to noxygencode:
      noxygencode.i <- c(noxygencode.i, "\r\n")
      noxygencode <- c(noxygencode, noxygencode.i)
    }
  }

  # Add preambel:
  noxygencode <- c(preambel, noxygencode)


  # Store the final string vector as a markdown (.md) file
  fileConn<-file(mdfile)
  writeLines(noxygencode, fileConn)
  close(fileConn)

  # Render the file
  if(rmarkdown::pandoc_available())
  {
    output <- rmarkdown::render(mdfile, quiet = TRUE)
    cli::cli_alert_success("Documentation written to {.file {output}}.")
  } else
  {
    warning("MarkDown file (*.md) created but could not be compiled due to missing pandoc installation.")
  }


  # Open the file:
  #outfile <- file.path(outpath, paste0("nldoc.", output_format))

}


#' Escape text for a markdown table cell
#'
#' @description Escapes pipes and removes line breaks, which would end the cell
#' or the row of a markdown table
#'
#' @param x character vector
#' @return escaped character vector
#' @aliases nldoc_md_cell
#' @rdname nldoc_md_cell
#' @keywords internal
nldoc_md_cell <- function(x)
{
  x <- gsub("[\r\n]+", " ", x)
  gsub("|", "\\|", x, fixed = TRUE)
}
