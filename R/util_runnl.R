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

  ### Get the current values from the siminput matrix:
  simdata_run <- getsim(nl, "siminput")[siminputrow,]

  ### Attach a runnum variable if needed:
  if (!is.na(getexp(nl, "idrunnum"))) {
    runnum <- tibble(siminputrow)
    names(runnum) <- getexp(nl, "idrunnum")
    simdata_run <- cbind(simdata_run, runnum)
  }

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
  nl_majorversion <- substring(getnl(nl, "nlversion"), 0, 1)

  if (os == "win") {

    # Block 1 of the batch file:
    block1 <- c("@echo off",
                "setlocal ENABLEDELAYEDEXPANSION",
                "set BASE_DIR=%~dp0",
                "if defined JAVA_HOME (",
                "  set \"JAVA=%JAVA_HOME%\\bin\\java.exe\"",
                ") ELSE (",
                "  ECHO JAVA_HOME not defined, using java on PATH.",
                "  ECHO If you encounter errors, set JAVA_HOME or update your PATH to include java.exe.",
                "  set \"JAVA=java.exe\"",
                ")"
                )

    # JVM_OPTS line:
    extensionspath <- paste0(getnl(nl, "nlpath"), "app/extensions/")
    jvmoptsline <- paste0("SET \"JVM_OPTS=-Xmx", getnl(nl, "jvmmem"), "m -XX:+UseParallelGC -Dfile.encoding=UTF-8 -Dnetlogo.extensions.dir=^\"", extensionspath, "^\"\"")

    # Block 2 of the batch file:
    block2 <- c("set ARGS=",
                "FOR %%a IN (%*) DO (",
                "  SET \"ARG=%%a\"",
                "  IF \"!ARG!\" == \"--3D\" (",
                "    SET \"JVM_OPTS=!JVM_OPTS! -Dorg.nlogo.is3d=true\"",
                "  ) ELSE (",
                "    IF \"!ARG:~0,2!\" == \"-D\" (",
                "      SET \"JVM_OPTS=!JVM_OPTS! !ARG!\"",
                "	  ) ELSE (",
                "      SET \"ARGS=!ARGS! !ARG!\"",
                "	  )",
                "  )",
                ")")

    # Classpath line:
    if (nl_majorversion == 6) {
      jarpath <- paste0(getnl(nl, "nlpath"), "app/netlogo-", getnl(nl, "nlversion"), ".jar")
    }
    if (nl_majorversion == 5) {
      jarpath <- paste0(getnl(nl, "nlpath"), "app/NetLogo.jar")
    }

    jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")


    # Block 3 of the batch file:
    block3 <- c("\"%JAVA%\" %JVM_OPTS% -classpath \"%ABSOLUTE_CLASSPATH%\" org.nlogo.headless.Main %ARGS%")

    # Put all blocks together:
    allblocks <- c(block1, jvmoptsline, block2, jarpathline, block3)


    ## Write batch file:
    batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".bat")
    writeLines(allblocks, batchpath_temp)


    ##### OLD STUFF:
#
#     # Prepare pathes:
#     batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.bat")
#
#     # Extensions Folder:
#     extensionspath <- paste0(getnl(nl, "nlpath"), "app/extensions/")
#     jarpath <- paste0(getnl(nl, "nlpath"), "app/netlogo-", getnl(nl, "nlversion"), ".jar")
#
#     # jvmoptions string:
#     jvmoptsline <- paste0("SET \"JVM_OPTS=-Xmx", getnl(nl, "jvmmem"), "m -XX:+UseParallelGC -Dfile.encoding=UTF-8 -Dnetlogo.extensions.dir=^\"", extensionspath, "^\"\"")
#     jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")
#
#     # Read batchfile (on windows use nlpath\netlogo-headless.bat, on linux and mac nlpath\netlogo-headless.sh)
#     batch <- readr::read_lines(batchpath)
#
#     # Get position index of jvmopts and jarpath line
#     pos_jvmopts <- which(grepl("SET \"JVM_OPTS=-Xmx", batch))
#     pos_jarpath <- which(grepl("SET \"ABSOLUTE_CLASSPATH=", batch))
#
#     # Replace lines in batch with updated versions
#     batch[pos_jvmopts] <- jvmoptsline
#     batch[pos_jarpath] <- jarpathline
#
#     # Create new batchfile:
#     batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".bat")
#     readr::write_lines(batch, path=batchpath_temp)

  }
  if (os == "unix") {


    # Block1 of netlogo-headless.sh:
    block1 <- c("#!/bin/bash")

    # Basedirline:
    basedirline <- paste0("BASE_DIR=\"", getnl(nl, "nlpath"), "\"")

    # Block2 of netlogo-headless.sh:
    block2 <- c("if [[ ${JAVA_HOME+1} ]]; then",
                "  JAVA=\"${JAVA_HOME}/bin/java\"",
                "else",
                "  echo \"JAVA_HOME undefined, using java from path. For control over exact java version, set JAVA_HOME\"",
                "  JAVA=\"java\"",
                "fi;")

    # jvmoptsline:
    jvmoptsline <- paste0("JVM_OPTS=(-Xmx", getnl(nl, "jvmmem"), "m -XX:+UseParallelGC -Dfile.encoding=UTF-8)")

    # Block3 of netlogo-headless.sh:
    block3 <- c("OPTS_INDEX=2",
                "ARGS=()",
                "INDEX=0",
                "for arg in \"$@\"; do",
                "  if [[ \"$arg\" == \"--3D\" ]]; then",
                "    JVM_OPTS[OPTS_INDEX++]=\"-Dorg.nlogo.is3d=true\"",
                "  elif [[ \"$arg\" == -D* ]]; then",
                "    JVM_OPTS[OPTS_INDEX++]=\"$arg\"",
                "  else",
                "    ARGS[INDEX++]=\"$arg\"",
                "  fi",
                "done",
                "RAW_CLASSPATH=\"app/args4j-2.0.12.jar:app/asm-all-5.0.4.jar:app/asm-all-5.0.4.jar:app/autolink-0.6.0.jar:app/behaviorsearch.jar:app/commons-codec-1.10.jar:app/commons-logging-1.1.1.jar:app/config-1.3.1.jar:app/flexmark-0.20.0.jar:app/flexmark-ext-autolink-0.20.0.jar:app/flexmark-ext-escaped-character-0.20.0.jar:app/flexmark-ext-typographic-0.20.0.jar:app/flexmark-formatter-0.20.0.jar:app/flexmark-util-0.20.0.jar:app/gluegen-rt-2.3.2.jar:app/httpclient-4.2.jar:app/httpcore-4.2.jar:app/httpmime-4.2.jar:app/jcommon-1.0.16.jar:app/jfreechart-1.0.13.jar:app/jhotdraw-6.0b1.jar:app/jmf-2.1.1e.jar:app/jogl-all-2.3.2.jar:app/json-simple-1.1.1.jar:app/log4j-1.2.16.jar:app/macro-compat_2.12-1.1.1.jar:app/macro-compat_2.12-1.1.1.jar:app/netlogo-6.0.3.jar:app/parboiled_2.12-2.1.3.jar:app/parboiled_2.12-2.1.3.jar:app/picocontainer-2.13.6.jar:app/picocontainer-2.13.6.jar:app/rsyntaxtextarea-2.6.0.jar:app/scala-library-2.12.0.jar:app/scala-library.jar:app/scala-parser-combinators_2.12-1.0.4.jar:app/scala-parser-combinators_2.12-1.0.5.jar:app/shapeless_2.12-2.3.2.jar:app/shapeless_2.12-2.3.2.jar\"",
                "CLASSPATH=\'\'",
                "for jar in `echo $RAW_CLASSPATH | sed 's/:/ /g'`; do",
                "  CLASSPATH=\"$CLASSPATH:$BASE_DIR/$jar\"",
                "done",
                "CLASSPATH=`echo $CLASSPATH | sed 's/://'`",
                "$JAVA \"${JVM_OPTS[@]}\" -Dnetlogo.extensions.dir=\"${BASE_DIR}/app/extensions\" -classpath \"$CLASSPATH\" org.nlogo.headless.Main \"${ARGS[@]}\""
                )

    # Put all blocks together:
    allblocks <- c(block1, basedirline, block2, jvmoptsline, block3)


    ## Write batch file:
    batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".sh")
    writeLines(allblocks, batchpath_temp)

    ## Make sh executable on linux:
    system(paste0("chmod +x ", batchpath_temp), wait = TRUE)


#
#     ## OLD STUFF:
#
#
#     ## Create path variables:
#     batchpath <- paste0(getnl(nl, "nlpath"), "netlogo-headless.sh")
#     batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".sh")
#
#     # Copy original file to temppath file
#     system(paste0("cp \"", batchpath, "\" \"", batchpath_temp, "\""), wait=TRUE)
#
#     # Define edited lines for shell script:
#     basedirline <- paste0("BASE_DIR=\"", getnl(nl, "nlpath"), "\"")
#     jvmoptsline <- paste0("JVM_OPTS=(-Xmx", getnl(nl, "jvmmem"), "m -Dfile.encoding=UTF-8)")
#
#     ## Edit lines in place:
#     system(paste0("sed -i -r 's!^BASE_DIR=.*!", basedirline, "!'", " \"", batchpath_temp, "\""))
#     system(paste0("sed -i -r 's!^JVM_OPTS=.*!", jvmoptsline, "!'", " \"", batchpath_temp, "\""))


  }


  return(batchpath_temp)


}

