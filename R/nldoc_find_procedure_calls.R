#' Determine procedure calls
#'
#' @description Determine procedure calls
#'
#' @param nlogocode vector of netlogo code strings
#'
#' @return tibble with procedure names and procedure calls
#'
#' @details
#'
#' The procedure searches netlogo code for procedure definitions and calls.
#' The information is stored within a tibble that can be further processed.
#'
#' @aliases nldoc_find_procedure_calls
#' @rdname nldoc_find_procedure_calls
#' @keywords internal

nldoc_find_procedure_calls <- function(nlogocode)
{
  ## Extract modelcode:
  modelcode <- nlogocode$modelcode
  ## Remove all comment lines:
  modelcode <- modelcode[grep(pattern='^;', modelcode, invert=TRUE)]

  ## Remove all comments within lines:
  modelcode <- sub('\\;.*', '', modelcode)

  ## Function headings:
  procs <- tibble::tibble(procname=sub(" .*", "", sub("^\\S+\\s+", "", modelcode[grep(pattern='^TO ', toupper(modelcode))])),
                          start=grep(pattern='^TO ', toupper(modelcode)))



  nw <- tibble::tibble()

  ## Find appearances of procedures:
  for (i in procs$procname)
  {
    i.app <- grep(pattern=paste0("\\b", i, "\\b"), modelcode, ignore.case = TRUE)

    # Now check appearances and determinate the context
    for (j in i.app)
    {
      ## Only proceed if we dont have the heading of a procedure:
      if (!(j %in% procs$start))
      {
        ## Find the context:
        context <- procs %>% dplyr::filter(start < j) %>% dplyr::filter(dplyr::row_number()==n())

        ## Store result:
        nw.i <- tibble::tibble(from=context$procname, to=i)
        nw <- rbind(nw, nw.i)
      }
    }
  }

  return(nw)
}
