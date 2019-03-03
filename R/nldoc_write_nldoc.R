#' Write NetLogo documentation
#'
#' @description Write NetLogo documentation
#'
#' @param noxygen list with parsed and processed noxygen tags from NetLogo model code
#' @param noxygen_it list with parsed and processed infotab strings
#' @param noxygen_gui list with parsed and processed noxygen tags from NetLogo GUI elements
#' @param noxygen_bs list with parsed and processed noxygen tags from NetLogo behavior space experiments
#' @param outpath Path to folder where rendered documentation should be created
#' @param output_format either "html", "pdf" or "docx"
#' @param number_ections TRUE/FALSE, if TRUE sections in the documentation will be numbered
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

nldoc_write_nldoc <- function(noxygen, noxygen_it, noxygen_gui, noxygen_bs, outpath, output_format, number_sections, theme, date, toc)
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

  # write preambel for the markdown file (define format, style, ...)
  preambel <- paste("---\r\n",
                    "title: \"", noxygen$header$model, "\"\r\n",
                    "author: \"", noxygen$header$author, "\"\r\n",
                    "date: \"", date, "\"\r\n",
                    "output: \r\n",
                    "  ", output_format, ":\r\n",
                    "    toc: ", toc, "\r\n", sep = '')

  # if PDF is chosen, add section numbering definition to the preambel
  if (output_format == "pdf")
  {
    preambel <- paste(preambel,
                      "    number_sections: ", number_sections, "\r\n", sep = '')
  }

  # if HTML is chosen add section numbering and theme definition to the preambel
  if (output_format == "html_document")
  {
    preambel <- paste(preambel,
                      "    number_sections: ", number_sections, "\r\n",
                      "    theme: ", theme, "\r\n", sep = '')
  }

  # Add end of preambel!
  preambel <- paste(preambel,
                    "---", sep='')


  # Initialize string
  noxygencode <- ""

  # Process the information from global
  noxygencode <- c(noxygencode, "# Global")

  for (i in 1:nrow(noxygen$global))
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

  # Process the information from procedures
  noxygencode <- c(noxygencode, "# Procedures")

  for (i in 1:nrow(noxygen$proc))
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
  if (!is.na(noxygen_gui[1]))
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
      prop.i <- paste(names(noxygen_gui.i), noxygen_gui.i, sep=": ", collapse="; ")

      noxygencode <- c(noxygencode, paste("| ", name.i, " | ", type.i, " | ", value.i, " | ", prop.i, " |"))
    }
  }

  if (!is.na(noxygen_it[1]))
  {
    noxygencode <- c(noxygencode, "# Info Tab")


    for(i in 1:length(noxygen_it))
    {
      noxygencode <- c(noxygencode, noxygen_it[i])
    }
  }


  ## Add bs elements if wanted:
  if (!is.na(noxygen_bs[1]))
  {
    noxygencode <- c(noxygencode, "# Behavior Space Experiments")

    ## The function generates a table for each experiment:

    for(i in 1:length(noxygen_bs))
    {
      noxygen.bs.i <- noxygen_bs[[i]]
      noxygencode.i <- paste0("## ", noxygen.bs.i$.attrs[["name"]])

      # Main Table:
      noxygencode.i <- c(noxygencode.i, "#### Definitions {-}")
      noxygencode.i <- c(noxygencode.i, "| Setup | Go | Final | Time limit | Metrics | Measure each tick | repetitions | exit condition |")
      noxygencode.i <- c(noxygencode.i, "| --- | --- | --- | --- | --- | --- | --- | --- |")

      setup.i <- paste(noxygen.bs.i[names(noxygen.bs.i) == "setup"], collapse="; ")
      go.i <- paste(noxygen.bs.i[names(noxygen.bs.i) == "go"], collapse="; ")
      final.i <- paste(noxygen.bs.i[names(noxygen.bs.i) == "final"], collapse="; ")
      time.i <- noxygen.bs.i$timeLimit[["steps"]]
      metrics.i <- paste(noxygen.bs.i[names(noxygen.bs.i) == "metric"], collapse="; ")
      measure.i <- noxygen.bs.i$.attrs[["runMetricsEveryStep"]]
      rep.i <- noxygen.bs.i$.attrs[["repetitions"]]
      exit.i <- noxygen.bs.i[["exitCondition"]]

      noxygencode.i <- c(noxygencode.i, paste("|", setup.i, "|", go.i, "|", final.i, "|", time.i, "|", metrics.i, "|", measure.i, "|", rep.i, "|", exit.i, "|"))


      ## EnumeratedValueset if present:
      parameters.i <- tibble::tibble()

      if (length(noxygen.bs.i[names(noxygen.bs.i) == "enumeratedValueSet"]) > 0)
      {
        for (j in 1:length(noxygen.bs.i[names(noxygen.bs.i) == "enumeratedValueSet"]))
        {
          noxygen.bs.i.value <- noxygen.bs.i[names(noxygen.bs.i) == "enumeratedValueSet"][[j]]
          values.ij <- NULL
          for(k in 1:length(noxygen.bs.i.value[names(noxygen.bs.i.value) == "value"]))
          {
            values.ij <- c(values.ij, noxygen.bs.i.value[[k]])
          }
          name.ij <- noxygen.bs.i.value$.attrs[[1]]
          type.ij <- ifelse(length(values.ij) > 1, "enumerated", "constant")
          values.ij <- paste(values.ij, collapse=", ")

          parameters.ij <- tibble::tibble(name=name.ij, type=type.ij, values=values.ij)
          parameters.i <- rbind(parameters.i, parameters.ij)

          #noxygencode.i <- c(noxygencode.i, paste(noxygen.bs.i.value$.attrs[[1]], ": ", values, "\n"))
        }
      }

      ## Stepped value set if present:
      if (length(noxygen.bs.i[names(noxygen.bs.i) == "steppedValueSet"]) > 0)
      {
        for (j in 1:length(noxygen.bs.i[names(noxygen.bs.i) == "steppedValueSet"]))
        {
          noxygen.bs.i.value <- noxygen.bs.i[names(noxygen.bs.i) == "steppedValueSet"][[j]]
          name.ij <- noxygen.bs.i.value[1]
          type.ij <- "sequence"
          values.ij <- paste0("from: ", noxygen.bs.i.value[2], "; to: ", noxygen.bs.i.value[4], "; by: ", noxygen.bs.i.value[3])

          parameters.ij <- tibble::tibble(name=name.ij, type=type.ij, values=values.ij)
          parameters.i <- rbind(parameters.i, parameters.ij)

          #noxygencode.i <- c(noxygencode.i, paste(noxygen.bs.i.value[1], ": ", paste(noxygen.bs.i.value[2:4], collapse=", "), "\n"))
        }
      }

      # If we have at least one parameter, write parameter table:
      if (nrow(parameters.i > 0))
      {
        # Parameter Table:
        noxygencode.i <- c(noxygencode.i, "#### Parameters {-}")
        noxygencode.i <- c(noxygencode.i, "| Parameter | Type | Values |")
        noxygencode.i <- c(noxygencode.i, "| --------- | ---- | ------ |")

        for (j in 1:nrow(parameters.i))
        {
          parameters.ij <- parameters.i[j,]
          noxygencode.i <- c(noxygencode.i, paste("|", parameters.ij$name, "|", parameters.ij$type, "|", parameters.ij$values, "|"))
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
  rmarkdown::render(mdfile)

  # Open the file:
  outfile <- file.path(outpath, paste0("nldoc.", output_format))
  # Print success
  print("File successfully compiled!")
}
