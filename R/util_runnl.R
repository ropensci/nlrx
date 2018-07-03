#' Create a temporary behavior space xml file to setup NetLogo via commandline
#'
#' @description Create a temporary behavior space xml file to setup NetLogo via commandline
#'
#' @param nl nl object
#' @param seed random-seed for NetLogo simulation
#' @param siminputrow row id of the simulation input tibble of the simdesign within the provided nl object
#' @param xmlfile filepath where the xml file is stored
#' @aliases util_create_sim_XML
#' @rdname util_create_sim_XML

util_create_sim_XML <- function(nl, seed, siminputrow, xmlfile) {

  simdata_run <- getsim(nl, "siminput")[siminputrow,]

  ### Create XML object:
  nlXML = XML::newXMLDoc()
  experiments = XML::newXMLNode("experiments", doc=nlXML)
  experiment = XML::newXMLNode("experiment", attrs=c(name=getexp(nl, "expname"),
                                                     repetitions=getexp(nl, "repetition"),
                                                     runMetricsEveryStep=getexp(nl, "tickmetrics")),
                               parent=experiments)

  ## Add Setup, go
  idsetup <- paste(getexp(nl, "idsetup"), sep="\n", collapse="\n")
  idgo <- paste(getexp(nl, "idgo"), sep="\n", collapse="\n")
  XML::addChildren(experiment, XML::newXMLNode("setup", idsetup, parent=experiment))
  XML::addChildren(experiment, XML::newXMLNode("go", idgo, parent=experiment))

  ## Add final commands if provided:
  if (!is.na(getexp(nl, "idfinal"))) {
    idfinal <- paste(getexp(nl, "idfinal"), sep="\n", collapse="\n")
    XML::addChildren(experiment, XML::newXMLNode("final", idfinal, parent=experiment))
  }

  ## Add timeLimit:
  runtime <- getexp(nl, "runtime")
  XML::addChildren(experiment, XML::newXMLNode("timeLimit", attrs=c(steps=runtime), parent=experiment))

  ## Add metrics:
  metrics <- getexp(nl, "metrics")
  for (i in metrics) {
    XML::addChildren(experiment, XML::newXMLNode("metric", i, parent=experiment))
  }

  ## Add parameters and values:
  for (i in 1:length(simdata_run)) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet", attrs=c(variable=names(simdata_run[i])), XML::newXMLNode("value", attrs=c(value=simdata_run[[i]]))))
  }
  ## If repetition > 1 we use a ranodm seed, otherwise the provided seed:
  if (getexp(nl, "repetition") == 1) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet", attrs=c(variable="random-seed"), XML::newXMLNode("value", attrs=c(value=seed))))
  }

  ## Use NetLogo specific prefix:
  prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE experiments SYSTEM \"behaviorspace.dtd\">"

  # SAVE XML TO FILE
  cat(XML::saveXML(nlXML, prefix=prefix), file=xmlfile)

}

#' Setup and execute NetLogo via commandline
#'
#' @description Setup and execute NetLogo via commandline
#'
#' @param nl nl object
#' @param xmlfile file location of the experiment xml file
#' @param outfile file location for output results
#' @param batchfile file location of system specific batch file to call NetLogo via commandline
#' @aliases util_call_nl
#' @rdname util_call_nl

util_call_nl <- function(nl, xmlfile, outfile, batchfile) {

  NLcall <- paste0("\"", batchfile, "\"", " --model ", "\"", getnl(nl, "modelpath"), "\"", " --setup-file ", "\"", xmlfile, "\"", " --experiment ", getexp(nl, "expname"), " --table ", "\"", outfile, "\"", " --threads ", 1)
  system(NLcall, wait=TRUE)

}

#' Delete temporary files
#'
#' @description Delete temporary files
#'
#' @param nl nl object
#' @param pattern defines file types to be deleted (".xml", ".csv" or "all")
#' @aliases util_cleanup
#' @rdname util_cleanup

util_cleanup <- function(nl, pattern) {

  file.remove(dir(path=getexp(nl, "outpath"), pattern=pattern, full.names=TRUE))

}

#' Load output file from simulations
#'
#' @description Load output file from simulations
#'
#' @param nl nl object
#' @param outfile  file location of output results
#' @aliases util_gather_results
#' @rdname util_gather_results

util_gather_results <- function(nl, outfile) {

  NLtable <- readr::read_csv(outfile, skip=6)

  ## Throw away all ticks that are not within the eval interval
  NLtable <- NLtable %>% dplyr::filter(`[step]` %in% getexp(nl, "evalticks"))

  return(NLtable)
}

#' Write a modified batchfile that executes NetLogo
#'
#' @description Write a modified batchfile that executes NetLogo
#'
#' @param nl nl object
#' @aliases util_read_write_batch
#' @rdname util_read_write_batch

util_read_write_batch <- function(nl) {

  os <- util_get_os()
  batchpath_temp <- NULL

  if (os == "win") {
    # Prepare pathes:
    batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.bat")

    # Extensions Folder:
    extensionspath <- paste0(getnl(nl, "nlpath"), "app/extensions/")
    jarpath <- paste0(getnl(nl, "nlpath"), "app/netlogo-", getnl(nl, "nlversion"), ".jar")

    # jvmoptions string:
    jvmoptsline <- paste0("SET \"JVM_OPTS=-Xmx", getnl(nl, "jvmmem"), "m -XX:+UseParallelGC -Dfile.encoding=UTF-8 -Dnetlogo.extensions.dir=^\"", extensionspath, "^\"\"")
    jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")

    # Read batchfile (on windows use nlpath\netlogo-headless.bat, on linux and mac nlpath\netlogo-headless.sh)
    batch <- readr::read_lines(batchpath)

    # Get position index of jvmopts and jarpath line
    pos_jvmopts <- which(grepl("SET \"JVM_OPTS=-Xmx", batch))
    pos_jarpath <- which(grepl("SET \"ABSOLUTE_CLASSPATH=", batch))

    # Replace lines in batch with updated versions
    batch[pos_jvmopts] <- jvmoptsline
    batch[pos_jarpath] <- jarpathline

    # Create new batchfile:
    batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".bat")
    readr::write_lines(batch, path=batchpath_temp)

  }
  if (os == "unix") {

    # Prepare pathes:
    batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.sh")

    # jvmoptions string:
    basedirline <- paste0("BASE_DIR=\"$( cd \"$( ", getnl(nl, "nlpath"), " \"${BASH_SOURCE[0]}\" )\" && pwd )\"")
    jvmoptsline <- paste0("JVM_OPTS=(-Xmx", getnl(nl, "jvmmem"), "m -XX:+UseParallelGC -Dfile.encoding=UTF-8)")

    # Read batchfile (on windows use nlpath\netlogo-headless.bat, on linux and mac nlpath\netlogo-headless.sh)
    batch <- readr::read_lines(batchpath)

    # Get position index of jvmopts and jarpath line
    pos_basedir <- which(grepl("BASE_DIR=\"", batch))
    pos_jvmopts <- which(grepl("JVM_OPTS=", batch))

    # Replace lines in batch with updated versions
    batch[pos_basedir] <- basedirline
    batch[pos_jvmopts] <- jvmoptsline

    # Create new batchfile:
    batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".bat")
    readr::write_lines(batch, path=batchpath_temp)

  }


  return(batchpath_temp)


}
