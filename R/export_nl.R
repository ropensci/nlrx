#' Export NetLogo Experiment
#'
#' @description Export NetLogo Experiment as zip file
#'
#' @param nl nl object
#' @param path Path to folder that contains files to run NetLogo experiment
#' @param tarfile Path to folder where the experiments gets stored as zip file
#'
#' @return The status value returned by the external command, invisibly.
#'
#' @details
#'
#' Exports your folder that contains data and scripts for NetLogo + nlrx analyses
#' as a zip file. Furthermore, `export_nl` takes your nl object and saves it in the
#' zipped folder. This enables other person to run the same experiment as you.
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from testdata:
#' nl <- nl_lhs
#' path <- getwd() # adjust path to your needs, path should point to a directory with model data
#' outfile <- tempfile(fileext = ".zip") # adjust file path to your needs
#' export_nl(nl, path = path, tarfile = outfile)
#' }
#' @aliases export_nl
#' @rdname export_nl
#'
#' @export

export_nl <- function(nl,
                      path = dirname(getnl(nl, "modelpath")),
                      tarfile) {

  ## Set pathes:
  folder <- basename(path)
  path <- dirname(path)

  ## Create first tar file in tempdir:
  tempdir <- tempdir()
  tempfile <- file.path(tempdir, "modelfiles.tar.gz")

  ## Create the tar file:
  system(paste0("tar -czf \"", tempfile, "\" -C \"", path, "\" ", folder))

  ## Unpack the tarfile:
  system(paste0("tar -zxvf \"", tempfile, "\" -C \"", tempdir, "\""))

  ## Create a rds file from the provided nl object:
  tempfile_nl <- file.path(tempdir, folder, "nlobject.rds")
  saveRDS(nl, tempfile_nl)

  ## Pack it up again:
  system(paste0("tar -czf \"", tarfile, "\" -C \"", tempdir, "\" ", folder))

}
