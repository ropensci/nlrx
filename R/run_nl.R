
#' Execute NetLogo simulation
#'
#' @description Execute NetLogo simulation from a nl object with a defined experiment and simdesign
#'
#' @param nl nl object
#' @param seed a random seed for the NetLogo simulation
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @param cleanup indicate which filetypes should be deleted
#' @return tibble with simulation output results
#' @details
#'
#' run_nl executes one simulation of the specified NetLogo model within the provided nl object.
#' The random seed is set within the NetLogo model to control stochasticity.
#' The run id defines which row of the input data tibble within the simdesign object of the provided nl object is executed.
#' Cleanup can either be ".xml" to delete all temporarily created xml files; ".csv" to delete all temporarily created csv files or "all" to delete all temporarily created files.
#'
#' This function can be used to run single simulations of a NetLogo model.
#' It can also be used within a loop environment that loops over the input tibble and the simseeds of the attached simdesign to run a full simulation of all input combinations and seeds.
#' We suggest to use the furrr package function future_map_dfr to loop over simulations.
#' This approach enables singlecore and multicore simulations on local machines and remote HPC clusters.
#'
#' @examples
#' \dontrun{
#'
#' # Run one simulation:
#' results <- run_nl(nl=nl,
#' seed=simseeds(nl)[1],
#' run=1,
#' cleanup="all")
#'
#' # Run all simulations on local machine in parallel:
#'
#' library(furrr)
#' plan(multisession)
#' results %<-% furrr::future_map_dfr(getsim(nl, "simseeds"), function(seed){
#'   furrr::future_map_dfr(seq_len(nrow(getsim(nl, "siminput"))), function(siminputrow) {
#'       run_nl(nl = nl,
#'              seed = seed,
#'              siminputrow = siminputrow,
#'              cleanup = "all")
#'    })
#'  })
#' }
#' @aliases run_nl
#' @rdname run_nl
#'
#' @export

run_nl <- function(nl, seed, siminputrow, cleanup="all") {

  util_eval_simdesign(nl)

  ## Write XML File:
  xmlfile <- tempfile(pattern=paste0("nlrx", seed, "_", siminputrow),
                      fileext=".xml")

  util_create_sim_XML(nl, seed, siminputrow, xmlfile)

  ## Execute:
  outfile <- tempfile(pattern=paste0("nlrx", seed, "_", siminputrow),
                      fileext=".csv")

  batchpath <- util_read_write_batch(nl)
  util_call_nl(nl, xmlfile, outfile, batchpath)

  ## Read results
  nl_results <- util_gather_results(nl, outfile)
  ## Update runnumber:
  nl_results$siminputrow <- siminputrow

  ## Delete temporary files:
  if(cleanup == "xml" | cleanup == "all") {
    util_cleanup(nl, pattern=".xml")
  }
  if(cleanup == "csv" | cleanup == "all") {
    util_cleanup(nl, pattern=".csv")
  }

  return(nl_results)
}

