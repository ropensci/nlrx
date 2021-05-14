#' Parse model code
#'
#' @description Parse model code
#'
#' @param nlogocode vector of netlogo code strings
#'
#' @return tibble with structured netlogo code
#'
#' @details
#'
#' The procedure searches for noxygen commands within the NetLogo code.
#' This information is used to structure the netlogo code strings in a tibble.
#' Additionally, tibbles with gui and behavior space information are created.
#'
#' @aliases nldoc_parse_modelcode
#' @rdname nldoc_parse_modelcode
#' @keywords internal
nldoc_parse_modelcode <- function(nlogocode)
{
  ## We now first go through the model code to find all procedures
  modelcode <- nlogocode$modelcode

  ## Extract all noxygen information:
  noxygenlines <- grep(pattern=";`@", modelcode)

  ## Identify blocks of noxygen code:
  noxygenblocks <- split(noxygenlines, c(0, cumsum(abs(diff(noxygenlines)) > 1)))

  # prepare tibbles for output:
  inf.header <- tibble::tibble()
  inf.global <- tibble::tibble()
  inf.proc <- tibble::tibble()

  ## Now loop trough the noxygenblocks:
  for (i in 1:length(noxygenblocks))
  {
    noxygenblock.i <- noxygenblocks[[i]]
    modelcode.i <- modelcode[noxygenblock.i]

    # Find out if iths @model, @global, or @procedure:
    if (length(grep(pattern="@model", modelcode.i)) > 0)
    {
      model <- gsub(".*@model ", "", modelcode.i[grep(pattern=";`@model", gsub(" ", "", modelcode.i, fixed = TRUE))])
      author <- paste(gsub(".*@author ", "", modelcode.i[grep(pattern=";`@author", gsub(" ", "", modelcode.i, fixed = TRUE))]), collapse = ", ")

      inf.header.i <- tibble::tibble(model = model,
                                     author = author)

      inf.header <- rbind(inf.header, inf.header.i)
    }

    if (length(grep(pattern="@global", modelcode.i)) > 0)
    {
      global <- gsub(".*@global ", "", modelcode.i[grep(pattern=";`@global", gsub(" ", "", modelcode.i, fixed = TRUE))])
      description <- paste(gsub(".*@details ", "", modelcode.i[grep(pattern=";`@details", gsub(" ", "", modelcode.i, fixed = TRUE))]), collapse="</br>")

      inf.global.i <- tibble::tibble(global = global,
                                     description = description)

      code <- gsub(".*@code ", "", modelcode.i[grep(pattern=";`@code", gsub(" ", "", modelcode.i, fixed = TRUE))])


      # If code =TRUE, add the code information:
      if(length(code) > 0)
      {
        if (toupper(code) == "TRUE")
        {
          start <- noxygenblock.i[grep(pattern=";`@code", gsub(" ", "", modelcode.i, fixed = TRUE))] + 1

          ## Find the position of the next global, the next "TO" or the next "procedure"  tag
          end <- (min(c(subset(grep(pattern=";`@global", gsub(" ", "", modelcode, fixed = TRUE)),
                            grep(pattern=";`@global", gsub(" ", "", modelcode, fixed = TRUE)) > start)[1],
                     subset(grep(pattern=";`@procedure", gsub(" ", "", modelcode, fixed = TRUE)),
                            grep(pattern=";`@procedure", gsub(" ", "", modelcode, fixed = TRUE)) > start)[1],
                     subset(grep(pattern="^TO", toupper(modelcode)),
                            grep(pattern="^TO", toupper(modelcode)) > start)[1]), na.rm = TRUE) - 1)


          codelines <- ""
          for (j in start:end)
          {
            codelines <- c(codelines, modelcode[j])
          }
        } else {
          codelines <- NA
        }
      } else {
        codelines <- NA
      }
      inf.global.i$code <- list(codelines)
      inf.global <- rbind(inf.global, inf.global.i)

    }

    if (length(grep(pattern="@procedure", modelcode.i)) > 0)
    {
      procedure <- gsub(".*@procedure ", "", modelcode.i[grep(pattern=";`@procedure", gsub(" ", "", modelcode.i, fixed = TRUE))])
      param <- list(gsub(".*@param ", "", modelcode.i[grep(pattern=";`@param", gsub(" ", "", modelcode.i, fixed = TRUE))]))
      return <- list(gsub(".*@return ", "", modelcode.i[grep(pattern=";`@return", gsub(" ", "", modelcode.i, fixed = TRUE))]))
      description <- paste(gsub(".*@details ", "", modelcode.i[grep(pattern=";`@details", gsub(" ", "", modelcode.i, fixed = TRUE))]), collapse="</br>")


      inf.proc.i <- tibble::tibble(procedure = procedure,
                                   param = param,
                                   return = return,
                                   description = description)

      code <- gsub(".*@code ", "", modelcode.i[grep(pattern=";`@code", gsub(" ", "", modelcode.i, fixed = TRUE))])

      # If code =TRUE, add the code information:
      if (length(code) > 0)
      {
        if(toupper(code) == "TRUE")
        {
          start <- noxygenblock.i[grep(pattern=";`@code", gsub(" ", "", modelcode.i, fixed = TRUE))] + 1
          end <- subset(grep(pattern="END", toupper(modelcode)), grep(pattern="END", toupper(modelcode)) > start)[1]
          codelines <- ""
          for (j in start:end)
          {
            codelines <- c(codelines, modelcode[j])
          }
        } else {
          codelines <- NA
        }
      } else {
        codelines <- NA
      }
      inf.proc.i$code <- list(codelines)
      inf.proc <- rbind(inf.proc, inf.proc.i)

    }
  }

  noxygen <- list(header = inf.header, global = inf.global, proc = inf.proc)

  return(noxygen)


}
