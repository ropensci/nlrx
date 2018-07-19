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
#' @keywords internal
util_create_sim_XML <- function(nl, seed, siminputrow, xmlfile) {

  simdata_run <- getsim(nl, "siminput")[siminputrow,]

  ### Create XML object:
  nlXML <- XML::newXMLDoc()
  experiments <- XML::newXMLNode("experiments", doc=nlXML)
  experiment <- XML::newXMLNode("experiment", attrs=c(name=getexp(nl, "expname"),
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
  for (i in seq_along(simdata_run)) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet", attrs=c(variable=names(simdata_run[i])), XML::newXMLNode("value", attrs=c(value=simdata_run[[i]]))))
  }
  ## If repetition > 1 we use a ranodm seed, otherwise the provided seed:
  if (getexp(nl, "repetition") == 1) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet", attrs=c(variable="random-seed"), XML::newXMLNode("value", attrs=c(value=seed))))
  }

  ## Use NetLogo specific prefix:
  prefix <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE experiments SYSTEM \"behaviorspace.dtd\">"

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
#' @keywords internal
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
#' @keywords internal
util_cleanup <- function(nl, pattern) {

  file.remove(dir(path=getexp(nl, "outpath"), pattern=pattern, full.names=TRUE))

}

#' Load output file from simulations
#'
#' @description Load output file from simulations
#'
#' @param nl nl object
#' @param outfile  file location of output results
#' @param seed model random-seed
#' @param siminputrow current row of siminput tibble
#' @aliases util_gather_results
#' @rdname util_gather_results
#' @keywords internal
util_gather_results <- function(nl, outfile, seed, siminputrow) {

  NLtable <- readr::read_csv(outfile, skip=6, col_types = readr::cols())
  NLtable$siminputrow <- siminputrow

  # Check if tickmetrics is true, if not, we only keep the last reported line:
  if (getexp(nl, "tickmetrics") == "false") {

    # Report line with max step:
    NLtable <- NLtable %>% dplyr::filter(`[step]` == max(`[step]`))


  } else {

    # We filter all evalticks lines from the table
    NLtable <- NLtable %>% dplyr::filter(`[step]` %in% getexp(nl, "evalticks"))

    # We then chek if there are ticks, that have reported no results:
    noeval <- getexp(nl, "evalticks")[!which(getexp(nl, "evalticks") %in% NLtable$`[step]`)]

    if (length(noeval) > 0)
    {
      message(paste0("No model results reported for siminputrow ", siminputrow, " on ticks ", noeval))
    }


  }

  # Finally check if the tibble is still empty:
  if (nrow(NLtable) == 0) {

    # Create an na line:
    NArow <- tibble::tibble(`[run number]` = NA)
    NArow <- cbind(NArow, getsim(nl, "siminput")[siminputrow,])
    NArow <- cbind(NArow, tibble::tibble(`random-seed` = seed))
    NArow <- cbind(NArow, tibble::tibble(`[step]` = NA))

    NAmetrics <- t(tibble::tibble(rep(NA, length(getexp(nl, "metrics")))))
    colnames(NAmetrics) <- getexp(nl, "metrics")
    rownames(NAmetrics) <- NULL

    NArow <- cbind(NArow, NAmetrics)
    NArow$siminputrow <- siminputrow

    NLtable <- NArow
  }


  return(NLtable)
}

#' Write a modified batchfile that executes NetLogo
#'
#' @description Write a modified batchfile that executes NetLogo
#'
#' @param nl nl object
#' @aliases util_read_write_batch
#' @rdname util_read_write_batch
#' @keywords internal
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

    ## Create path variables:
    batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.sh")
    batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".sh")

    # Copy original file to temppath file
    system(paste0("cp \"", batchpath, "\" \"", batchpath_temp, "\""), wait=TRUE)

    # Define edited lines for shell script:
    basedirline <- paste0("BASE_DIR=\"", getnl(nl, "nlpath"), "\"")
    jvmoptsline <- paste0("JVM_OPTS=(-Xmx", getnl(nl, "jvmmem"), "m -Dfile.encoding=UTF-8)")

    ## Edit lines in place:
    system(paste0("sed -i -r 's!^BASE_DIR=.*!", basedirline, "!'", " \"", batchpath_temp, "\""))
    system(paste0("sed -i -r 's!^JVM_OPTS=.*!", jvmoptsline, "!'", " \"", batchpath_temp, "\""))

    # # Prepare pathes:
    # batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.sh")
    #
    #
    # # Read batchfile (on windows use nlpath\netlogo-headless.bat, on linux and mac nlpath\netlogo-headless.sh)
    # batch <- readr::read_lines(batchpath)
    #
    # # Get position index of jvmopts and jarpath line
    # pos_basedir <- which(grepl("BASE_DIR=\"", batch))
    # pos_jvmopts <- which(grepl("JVM_OPTS=", batch))
    #
    # # Replace lines in batch with updated versions
    # batch[pos_basedir] <- basedirline
    # batch[pos_jvmopts] <- jvmoptsline
    #
    # # Create new batchfile:
    # batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".sh")
    # readr::write_lines(batch, path=batchpath_temp)

  }


  return(batchpath_temp)


}

