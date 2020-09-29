#' Write attached NetLogo simulation output to file
#'
#' @description Write attached NetLogo simulation output to file
#'
#' @param nl nl object
#' @param outpath optional path to directory where output is written
#'
#' Write NetLogo simulation output to a csv file in the directory outpath of the nl object
#' Output has to be attached to the simdesign first with simoutput(nl) <- results
#' The outpath argument can be optionally used to write output to a different directory than the defined outpath of the nl object.
#'
#' @examples
#'
#' # Load nl object including output data from testdata
#' nl <- nl_lhs
#'
#' # Write output to outpath directory
#' write_simoutput(nl, outpath=tempdir())
#'
#' @aliases write_simoutput
#' @rdname write_simoutput
#'
#' @export

write_simoutput <- function(nl, outpath=NA) {

  # Set the outpath:
  if (is.na(outpath)) {
    outpath <- getexp(nl, "outpath")
  }
  # Check if directory exists:
  if (!isTRUE(dir.exists(outpath))) {
    stop(paste0(
      "Error: Output directory ", outpath, " does not exist!"
    ), call. = FALSE)
  }

  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    warning("Simoutput tibble is empty.
In order to write_simoutput, output results have to be
attached to the simdesign of the nl object first:
setsim(nl, \"simoutput\") <- results")
  }

  # Create filename
  outfilename <- file.path(
    outpath,
    paste0(getexp(nl, "expname"),
           "_",
           getsim(nl, "simmethod"),
           ".csv"))

  # Write output
  readr::write_csv(x = getsim(nl, "simoutput"), path = outfilename)
}

