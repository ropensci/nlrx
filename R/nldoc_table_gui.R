#' Read NetLogo GUI elements from files
#'
#' @description Read NetLogo GUI elements from files
#'
#' @param modelfiles vector of filepaths to model files
#'
#' @return list containing NetLogo GUI elements
#'
#' @details
#'
#' The procedure reads text from the provided model files and reports a list of GUI elements.
#'
#' @aliases nldoc_table_gui
#' @rdname nldoc_table_gui
#' @keywords internal
nldoc_table_gui <- function(modelfiles) {

  ## Filter file names for main model file:
  modelpath <- modelfiles[grepl(pattern=".nlogo", modelfiles)]

  ## Open the model as string
  model.code <- readLines(modelpath)

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
