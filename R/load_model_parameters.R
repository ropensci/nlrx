#' Report globals from a NetLogo model that is defined within a nl object
#'
#' @description Report globals from a NetLogo model that is defined within a nl object
#'
#' @param nl nl object with a defined modelpath that points to a NetLogo model (*.nlogo)
#'
#' @details
#'
#' The function reads the NetLogo model file that is defined within the nl object and reports all global parameters that are defined as widget elements on the GUI of the NetLogo model.
#' Only globals that are found by this function are valid globals that can be entered into the variables or constants vector of an experiment object.
#'
#'
#' @examples
#' \dontrun{
#' load_model_parameters(nl)
#' }
#'
#' @aliases load_model_parameters
#' @rdname load_model_parameters
#'
#' @export

load_model_parameters <- function(nl) {

  ## Open the model as string
  model.code <- readLines(modelpath(nl))

  ## Find the line in the NetLogoCode where the interface definiton starts (separator: @#$#@#$#@)
  model.code.s1 <- grep("@#$#@#$#@", model.code, fixed=TRUE)[1]

  ## Remove model code before first separator:
  if (is.na(model.code.s1) == FALSE)
  {
    model.code <- model.code[(model.code.s1 + 1):length(model.code)]
  }
  ## Find second separator where interface definiton ends:
  model.code.s2 <- grep("@#$#@#$#@", model.code, fixed=TRUE)[1]

  ## Remove model code following second separator:
  if (is.na(model.code.s1) == FALSE)
  {
    model.code <- model.code[1:(model.code.s2 - 1)]
  }

  ## Extract the parameters and their values line by line:
  modelparam <- list()

  for (i in 1:length(model.code))
  {
    ## Read current line from model code
    l <- model.code[i]

    ## Check if l is a definition element and of what kind:
    if(l %in% c("SLIDER", "SWITCH", "INPUTBOX"))
    {
      if (l == "SLIDER")
      {
        name <- as.character(model.code[i + 5])
        value <- as.numeric(as.character(model.code[i + 9]))
      }
      if (l == "SWITCH")
      {
        name <- as.character(model.code[i + 5])
        value <- ifelse(as.numeric(as.character(model.code[i + 7])) == 1, TRUE, FALSE)
      }
      if (l == "INPUTBOX")
      {
        name <- as.character(model.code[i + 5])
        value <- model.code[i + 6]
      }

      ## Store in data.frame:
      modelparam[[name]] <- value
    }
  }

  return(modelparam)

}
