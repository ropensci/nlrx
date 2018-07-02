

run_nl <- function(nl, seed, run, cleanup=NULL) {

  eval_simdesign(nl)

  ## Write XML File:
  xmlfile <- tempfile(pattern=paste0("nlrx", seed, "_", run), fileext=".xml")
  util_create_sim_XML(nl, seed, run, xmlfile)

  ## Execute:
  outfile <- tempfile(pattern=paste0("nlrx", seed, "_", run), fileext=".csv")
  batchpath <- util_read_write_batch(nl)
  util_call_nl(nl, xmlfile, outfile, batchpath)

  ## Read results
  nl_results <- util_gather_results(nl, outfile)
  ## Update runnumber:
  nl_results$`[run number]` <- run

  ## Delete temporary files:
  if(cleanup == "xml" | cleanup == "all") {
    util_cleanup(nl, pattern=".xml")
  }
  if(cleanup == "csv" | cleanup == "all") {
    util_cleanup(nl, pattern=".csv")
  }

  return(nl_results)
}

