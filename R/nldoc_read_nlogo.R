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
#' @aliases nldoc_read_nlogo
#' @rdname nldoc_read_nlogo
#' @keywords internal
nldoc_read_nlogo <- function(modelfiles)
{
  # Its possible to select multiple files at once.
  # We have to remove GUI information from the files and bind them together:
  modelcode <- ""
  guicode <- NA

  for (i in 1:length(modelfiles))
  {
    # Open the netlogo file and store it in a string vector
    completecode.i <- readLines(modelfiles[i], warn=FALSE)

    # Find the line in the NetLogoCode where the interface definiton starts (separator: @#$#@#$#@)
    separator <- grep("@#$#@#$#@", completecode.i, fixed=TRUE)

    # Remove all interface definitions from the string vector
    if (length(separator) > 0)
    {
      # 0: sep[1] = modelcode
      modelcode.i <- completecode.i[1:(separator[1] - 1)]
      # 1st sep - 2nd sep = guicode
      guicode <- completecode.i[(separator[1] + 1):(separator[2] - 1)]
      # 2nd sep to 3rd sep = infotab:
      infotabcode <- completecode.i[(separator[2] + 1):(separator[3] - 1)]
      # 3rd sep to 4th sep = shapes
      # 4th sep to 5th sep = netlogo version
      nlversion <- completecode.i[(separator[4] + 1):(separator[5] - 1)]
      # 7th sep to 8th sep = bscode
      bscode <- completecode.i[(separator[7] + 1):(separator[8] - 1)]
    } else {
      # If there are no sepearators in the file, we have a nls file and just take everything as modelcode:
      modelcode.i <- completecode.i
    }

    # Bind together in modelcode vector:
    modelcode <- c(modelcode, "\n", modelcode.i)
  }

  nlogocode <- list(modelcode=modelcode,
                    guicode=guicode,
                    infotabcode=infotabcode,
                    nlversion=nlversion,
                    bscode=bscode)

  return(nlogocode)
}
