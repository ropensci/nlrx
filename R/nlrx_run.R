
# Write an xml file for one simulation run
nlrx_create_XML <- function(nl, run, xmlfile) {

  library(XML)

  simdata_run <- nl@experiment@simdesign@simdata[run,]

  ### Create XML object:
  nlXML = newXMLDoc()
  experiments = newXMLNode("experiments", doc=nlXML)
  experiment = newXMLNode("experiment", attrs=c(name=nl@experiment@sim.name,
                                                repetitions=nl@experiment@n.rep,
                                                runMetricsEveryStep=nl@experiment@metrics.tick), parent=experiments)

  ## Add Setup and go:
  addChildren(experiment, newXMLNode("setup", nl@experiment@setup.name, parent=experiment))
  addChildren(experiment, newXMLNode("go", nl@experiment@go.name, parent=experiment))
  addChildren(experiment, newXMLNode("timeLimit", attrs=c(steps=nl@experiment@max.ticks), parent=experiment))

  ## Add metrics:
  metrics <- nl@experiment@metrics
  for (i in metrics) {
    addChildren(experiment, newXMLNode("metric", i, parent=experiment))
  }

  ## Add parameters and values:
  for (i in 1:length(simdata_run)) {
    addChildren(experiment, newXMLNode("enumeratedValueSet", attrs=c(variable=names(simdata_run[i])), newXMLNode("value", attrs=c(value=simdata_run[[i]]))))
  }

  ## Use NetLogo specific prefix:
  prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE experiments SYSTEM \"behaviorspace.dtd\">"

  # SAVE XML TO FILE
  cat(saveXML(nlXML, prefix=prefix), file=xmlfile)

}


# Call one simulation via commandline
nlrx_call_nl <- function(nl, xmlfile, outfile, seed) {

  NLcall <- paste0("\"", nl@nl.path, "\"", " --model ", "\"", nl@model.path, "\"", " --setup-file ", "\"", xmlfile, "\"", " --experiment ", nl@experiment@sim.name, " --table ", "\"", outfile, "\"", " --threads ", 1)
  system(NLcall, wait=TRUE)

}


nlrx_cleanup <- function(nl, pattern) {

  file.remove(dir(path=nl@experiment@out.path, pattern=pattern, full.names=TRUE))

}


nlrx_gather_results <- function(nl, outfile) {

  library(dplyr)

  NLtable <- read.csv(outfile, skip=6)

  ## Throw away all ticks that are not within the eval interval
  NLtable <- NLtable %>% filter(X.step. %in% nl@experiment@eval.ticks)

  return(NLtable)
}



nlrx_run_one <- function(nl, seed, run, cleanup=NULL) {

  ## Write XML File:
  xmlfile <- paste0(nl@experiment@out.path, "nlrx", seed, "_", run, ".xml")
  nlrx_create_XML(nl, run, xmlfile)

  ## Execute:
  outfile <- paste0(nl@experiment@out.path, "nlrx", seed, "_", run, ".csv")
  nlrx_call_nl(nl, xmlfile, outfile, seed)

  ## Read results
  nl_results <- nlrx_gather_results(nl, outfile)



  ## Delete temporary files:
  if(cleanup == "xml" | cleanup == "all") {
    nlrx_cleanup(nl, pattern=".xml")
  }
  if(cleanup == "csv" | cleanup == "all") {
    nlrx_cleanup(nl, pattern=".csv")
  }


  return(nl_results)
}

