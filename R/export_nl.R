#' Export NetLogo Experiment
#'
#' @description Export NetLogo Experiment as tar.gz file
#'
#' @param nl nl object
#' @param path Path to folder that contains files to run NetLogo experiment
#' @param tarfile Path to the tar.gz file the experiment gets stored in
#'
#' @return The path of the created tar.gz file, invisibly.
#'
#' @details
#'
#' Exports your folder that contains data and scripts for NetLogo + nlrx analyses
#' as a tar.gz file. Furthermore, `export_nl` takes your nl object and saves it as
#' `nlobject.rds` in the archived folder. This enables other person to run the same
#' experiment as you. The folder itself is not changed.
#'
#' By default, the folder that contains the NetLogo model is exported. Everything
#' in that folder ends up in the archive, so keep the model in a project folder of
#' its own. The home directory and the filesystem root are refused, and the
#' tar.gz file must not be stored within the exported folder.
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from testdata:
#' nl <- nl_lhs
#' path <- getwd() # adjust path to your needs, path should point to a directory with model data
#' outfile <- tempfile(fileext = ".tar.gz") # adjust file path to your needs
#' export_nl(nl, path = path, tarfile = outfile)
#' }
#' @aliases export_nl
#' @rdname export_nl
#'
#' @export

export_nl <- function(nl,
                      path = dirname(getnl(nl, "modelpath")),
                      tarfile) {

  ## Set pathes (absolute, so that "." or a relative path resolves to the
  ## folder it names):
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  tarfile <- file.path(normalizePath(dirname(tarfile), winslash = "/", mustWork = TRUE),
                       basename(tarfile))
  folder <- basename(path)

  ## Refuse folders that are almost certainly not a project folder:
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  if (path == home || dirname(path) == path) {
    util_abort(c(
      "Refusing to export {.file {path}}.",
      "i" = "{.fn export_nl} archives the whole folder, and this is your {if (path == home) 'home directory' else 'filesystem root'}.",
      ">" = "Keep the model, data and scripts in a project folder of their own and pass it via {.arg path}."
    ), class = "nlrx_error_export_path")
  }

  ## The archive would include itself:
  if (startsWith(paste0(tarfile, "/"), paste0(path, "/"))) {
    util_abort(c(
      "The {.arg tarfile} {.file {tarfile}} lies within the exported folder {.file {path}}.",
      ">" = "Store the archive outside of the exported folder."
    ), class = "nlrx_error_export_path")
  }

  ## Assemble the archive in a staging folder of its own, so that the exported
  ## folder stays untouched and nothing from other exports gets mixed in:
  staging <- tempfile("export_nl")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  file.copy(path, staging, recursive = TRUE, copy.date = TRUE)

  ## Create a rds file from the provided nl object:
  saveRDS(nl, file.path(staging, folder, "nlobject.rds"))

  ## Pack it up:
  wd <- setwd(staging)
  on.exit(setwd(wd), add = TRUE, after = FALSE)
  utils::tar(tarfile, files = folder, compression = "gzip", tar = "internal")

  ## Report what was exported, so that an unexpectedly large export shows:
  files <- list.files(file.path(staging, folder), recursive = TRUE, all.files = TRUE)
  size <- format(structure(sum(file.size(file.path(staging, folder, files))),
                           class = "object_size"), units = "auto")
  cli::cli_alert_success("Exported {length(files)} file{?s} ({size}) from {.file {path}} to {.file {tarfile}}.")

  invisible(tarfile)
}
