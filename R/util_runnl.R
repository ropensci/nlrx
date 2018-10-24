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
    runnum <- tibble(paste0("\"", getexp(nl, "expname"), "_", seed, "_", siminputrow, "\""))
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
    XML::addChildren(experiment, XML::newXMLNode("final",
                                                 idfinal, parent = experiment))
  }

  ## Add timeLimit:
  runtime <- getexp(nl, "runtime")
  XML::addChildren(experiment, XML::newXMLNode("timeLimit",
                                               attrs = c(steps = runtime),
                                               parent = experiment))

  ## Add stop condition if provided:
  if (!is.na(getexp(nl, "stopcond"))) {
    stopcond <- paste(getexp(nl, "stopcond"), sep = "\n", collapse = "\n")
    XML::addChildren(experiment, XML::newXMLNode("exitCondition",
                                                 stopcond,
                                                 parent = experiment))
  }

  ## Add metrics:
  metrics <- getexp(nl, "metrics")

  # Add turtle metrics if defined
  if (all(!is.na(getexp(nl, "metrics.turtles")))) {
    turtles.reporter <- paste0("runresult (word \"[(list ",
                               paste(getexp(nl, "metrics.turtles"),
                                     collapse = " "), ")] of turtles\")")
    metrics <- c(metrics, turtles.reporter)
  }

  # add patch metrics if defined
  if (all(!is.na(getexp(nl, "metrics.patches")))) {
    patches.reporter <- paste0("runresult (word \"[(list ",
                               paste(getexp(nl, "metrics.patches"),
                                     collapse = " "), ")] of patches\")")
    metrics <- c(metrics, patches.reporter)
  }

  # add link metrics if defined
  if (all(!is.na(getexp(nl, "metrics.links")))) {
    links.reporter <- paste0("runresult (word \"[(list ",
                             paste(getexp(nl, "metrics.links"),
                                   collapse = " "), ")] of links\")")
    metrics <- c(metrics, links.reporter)
  }


  for (i in metrics) {
    XML::addChildren(experiment, XML::newXMLNode("metric",
                                                 i,
                                                 parent=experiment))
  }

  ## Add parameters and values:
  for (i in seq_along(simdata_run)) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet",
                                                 attrs = c(variable =
                                                             names(
                                                               simdata_run[i])),
                                                 XML::newXMLNode("value",
                                                          attrs =
                                                          c(value =
                                                            simdata_run[[i]]))))
  }
  ## If repetition > 1 we use a ranodm seed, otherwise the provided seed:
  if (getexp(nl, "repetition") == 1) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet",
                                                 attrs = c(variable =
                                                             "random-seed"),
                                                 XML::newXMLNode("value",
                                                        attrs=c(value=seed))))
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

  if (all(!is.na(getexp(nl, "metrics.turtles")))) {
    ## Rename column and clean turtle metrics
    NLtable <- NLtable %>% dplyr::rename(metrics.turtles = paste0("runresult (word \"[(list ", paste(getexp(nl, "metrics.turtles"), collapse=" "), ")] of turtles\")"))
    NLtable[, grepl(c("metrics.turtles"), names(NLtable))] <- list(.util_clean_metrics_turtles(NLtable, nl))
  }

  if (all(!is.na(getexp(nl, "metrics.patches")))) {
    ## Rename column and clean patch metrics
    NLtable <- NLtable %>% dplyr::rename(metrics.patches = paste0("runresult (word \"[(list ",  paste(getexp(nl, "metrics.patches"), collapse=" "), ")] of patches\")"))
    NLtable[, grepl(c("metrics.patches"), names(NLtable))] <- list(.util_clean_metrics_patches(NLtable, nl))
  }

  if (all(!is.na(getexp(nl, "metrics.links")))) {
    ## Rename column and clean link metrics
    NLtable <- NLtable %>% dplyr::rename(metrics.links = paste0("runresult (word \"[(list ",  paste(getexp(nl, "metrics.links"), collapse=" "), ")] of links\")"))
    NLtable[, grepl(c("metrics.links"), names(NLtable))] <- list(.util_clean_metrics_links(NLtable, nl))
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
    if (getnl(nl, "nlversion") == "5.3.1") {
      ## NetLogo 5.3.1 does not contain a premade batchfile in the installation directory
      ## Thus, we have to write the batchfile manually

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
      jarpath <- paste0(getnl(nl, "nlpath"), "app/NetLogo.jar")
      jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")

      # Block 3 of the batch file:
      block3 <- c("\"%JAVA%\" %JVM_OPTS% -classpath \"%ABSOLUTE_CLASSPATH%\" org.nlogo.headless.Main %ARGS%")

      # Put all blocks together:
      allblocks <- c(block1, jvmoptsline, block2, jarpathline, block3)

      ## Write batch file:
      batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".bat")
      writeLines(allblocks, batchpath_temp)


    } else {
      ## For all other NetLogo versions we can just copy the headless bat from the installation folder:
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
  }
  if (os == "unix") {
    if (getnl(nl, "nlversion") == "5.3.1") {
      ## NetLogo 5.3.1 does not contain a premade shfile in the installation directory
      ## Thus, we have to write the shfile manually

      # Block1 of netlogo-headless.sh:
      block1 <- c("#!/bin/sh")

      # Basedirline:
      basedirline <- paste0("cd \"", getnl(nl, "nlpath"), "app/\"")

      # jvmoptsline:
      jvmoptsline <- paste0("java -Xmx", getnl(nl, "jvmmem"), "m -Dfile.encoding=UTF-8 -classpath NetLogo.jar org.nlogo.headless.Main \"$@\"")

      # Put all blocks together:
      allblocks <- c(block1, basedirline, jvmoptsline)

      ## Write batch file:
      batchpath_temp <- tempfile(pattern="netlogo-headless", fileext=".sh")
      writeLines(allblocks, batchpath_temp)

      ## Make sh executable on linux:
      system(paste0("chmod +x ", batchpath_temp), wait = TRUE)

    } else {
      ## For all other NetLogo versions we can just copy and modify the headless sh from the installation folder:
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

    }
  }


  return(batchpath_temp)

}

# .util_clean_metrics_turtles <- function(NLtable, nl){
#
#   turtles_string <- NLtable[, grepl(c("metrics.turtles"), names(NLtable))]
#
#   if (!any(getexp(nl, "metrics.turtles") == "breed")) {
#
#
#     turtle_owns <- purrr::map(seq_len(nrow(turtles_string)), function(x){
#
#       turtles_doublequote <- regmatches(turtles_string[x,][[1]],gregexpr('(?<=\\").*?(?=\\")', turtles_string[x,][[1]], perl=TRUE))
#       regmatches(turtles_string[x,][[1]],gregexpr('(?<=\\").*?(?=\\")', turtles_string[x,][[1]], perl=TRUE)) <- c(sapply(turtles_doublequote, FUN = function(x) gsub("\\s", "_", x)))
#
#       turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 2)
#       turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 1, max(nchar(turtles_string[x,][[1]])) - 1)
#
#       turtles_string[x,][[1]]  <- gsub("(?<=\\{).*?(?=\\})", "\\1", turtles_string[x,][[1]], perl=TRUE)
#       turtles_string[x,][[1]]  <- gsub("\\{\\}", "\\1", turtles_string[x,][[1]], perl=TRUE)
#       turtles_string[x,][[1]]  <- regmatches(turtles_string[x,][[1]], gregexpr("(?<=\\[).*?(?=\\])",turtles_string[x,][[1]], perl=TRUE))
#
#       turtle_owns <- suppressWarnings(purrr::map_dfr(seq_along(turtles_string[x,][[1]][[1]]), function(turtle_index){
#
#         # split turtle owns into unique elements of a vector
#         turtle_owns <- strsplit(turtles_string[x,][[1]][[1]][turtle_index], " ")[[1]]
#         as.data.frame(matrix(turtle_owns, nrow = 1))
#
#       }))
#       names(turtle_owns) <- getexp(nl, "metrics.turtles")
#
#       return(turtle_owns)
#     })
#   } else {
#
#     turtle_owns <- purrr::map(seq_len(nrow(turtles_string)), function(x){
#
#     #regmatches(turtles_string[x,][[1]],gregexpr("(?<=\\{)*?(?=\\})", turtles_string[x,][[1]], perl=TRUE))
#     #gsub("(?<=\\{)\\s(?=\\})", "_", turtles_string[x,][[1]], perl = TRUE)
#     #regmatches(turtles_string[x,][[1]],gregexpr("(?<=\\{).*?(?=\\})", turtles_string[x,][[1]], perl=TRUE)) <- c(sapply(turtles_breed, FUN = function(x) gsub("\\s", "_", x)))
#
#
#     #turtles_doublequote <- regmatches(turtles_string[x,][[1]],gregexpr('(?<=\\").*?(?=\\")', turtles_string[x,][[1]], perl=TRUE))
#     #regmatches(turtles_string[x,][[1]],gregexpr('(?<=\\").*?(?=\\")', turtles_string[x,][[1]], perl=TRUE)) <- c(sapply(turtles_doublequote, FUN = function(x) gsub("\\s", "_", x)))
#
#
#
#     turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 2)
#     turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 1, max(nchar(turtles_string[x,][[1]])) - 1)
#
#     turtles_string[x,][[1]]  <- gsub("breed", "", turtles_string[x,][[1]], perl=TRUE)
#     turtles_string[x,][[1]]  <- gsub("\\{", "\\1", turtles_string[x,][[1]], perl=TRUE)
#     turtles_string[x,][[1]]  <- gsub("\\}", "\\1", turtles_string[x,][[1]], perl=TRUE)
#     turtles_string[x,][[1]]  <- gsub("^ *|(?<= ) | *$", "", turtles_string[x,][[1]], perl = TRUE)
#     turtles_string[x,][[1]]  <-  regmatches(turtles_string[x,][[1]], gregexpr("(?<=\\[).*?(?=\\])",turtles_string[x,][[1]], perl=TRUE))
#
#     turtle_owns <- suppressWarnings(purrr::map_dfr(seq_along(turtles_string[x,][[1]][[1]]), function(turtle_index){
#
#       # split turtle owns into unique elements of a vector
#       turtle_owns <- strsplit(turtles_string[x,][[1]][[1]][turtle_index], " ")[[1]]
#       #turtle_owns <- as.numeric(turtle_owns)
#       as.data.frame(matrix(turtle_owns, nrow = 1))
#
#     }))
#
#     names(turtle_owns) <- getexp(nl, "metrics.turtles")
#
#     return(turtle_owns)
#     })
#   }
#   turtle_owns
# }

## Clean patch metrics
.util_clean_metrics_patches <- function(NLtable, nl) {

  patches_string <- NLtable[, grepl(c("metrics.patches"), names(NLtable))]

  patches_owns <- purrr::map(seq_len(nrow(patches_string)), function(x){

    patches_breed <-  regmatches(patches_string[x,][[1]],gregexpr("(?<=\\{).*?(?=\\})", patches_string[x,][[1]], perl=TRUE))

    patches_string[x,][[1]] <- substring(patches_string[x,][[1]], 2)
    patches_string[x,][[1]] <- substring(patches_string[x,][[1]], 1, max(nchar(patches_string[x,][[1]])) - 1)

    patches_string[x,][[1]]  <- gsub("(?<=\\{).*?(?=\\})", "\\1", patches_string[x,][[1]], perl=TRUE)
    patches_string[x,][[1]]  <- gsub("\\{\\}", "\\1", patches_string[x,][[1]], perl=TRUE)
    patches_string[x,][[1]]  <-  regmatches(patches_string[x,][[1]], gregexpr("(?<=\\[).*?(?=\\])",patches_string[x,][[1]], perl=TRUE))

    patches_owns <- suppressWarnings(purrr::map_dfr(seq_along(patches_string[x,][[1]][[1]]), function(patches_index){

      # split patches owns into unique elements of a vector
      patches_owns <- toupper(strsplit(patches_string[x,][[1]][[1]][patches_index], " ")[[1]])
      patches_owns <- as.data.frame(matrix(patches_owns, nrow = 1, byrow = T), stringsAsFactors=FALSE)
      patches_owns <- utils::type.convert(patches_owns)

      #patches_owns <- as.data.frame(matrix(patches_owns, nrow = 1), stringsAsFactors=FALSE)
      #patches_owns <- as.data.frame(lapply(as.list(patches_owns), type.convert, as.is=TRUE), stringsAsFactors=FALSE)
    }))
      names(patches_owns) <- getexp(nl, "metrics.patches")

      return(patches_owns)
    })
  patches_owns
}


.util_clean_metrics_links <- function(NLtable, nl){

  ## Extract current string:
  links_string <- NLtable[, grepl(c("metrics.links"), names(NLtable))]

  links_owns <- purrr::map(seq_len(nrow(links_string)), function(x){

    ## Remove the outer [] brackets
    links_string[x,][[1]] <- substring(links_string[x,][[1]], 2)
    links_string[x,][[1]] <- substring(links_string[x,][[1]], 1, max(nchar(links_string[x,][[1]])) - 1)
    links_string[x,][[1]]  <- gsub("breed", "", links_string[x,][[1]], perl=TRUE)
    #links_string[x,][[1]]  <- gsub("\\{", "(", links_string[x,][[1]], perl=TRUE)
    #links_string[x,][[1]]  <- gsub("\\}", ")", links_string[x,][[1]], perl=TRUE)
    links_string[x,][[1]]  <- gsub("^ *|(?<= ) | *$", "", links_string[x,][[1]], perl = TRUE)
    links_string[x,][[1]]  <-  regmatches(links_string[x,][[1]], gregexpr("(?<=\\[).*?(?=\\])",links_string[x,][[1]], perl=TRUE))

    links_owns <- suppressWarnings(purrr::map_dfr(seq_along(links_string[x,][[1]][[1]]), function(links_index){

      # split turtle owns into unique elements of a vector
      links_owns <- strsplit(links_string[x,][[1]][[1]][links_index], "(\\{(?:[^{}]++|(?1))*\\})(*SKIP)(*F)| ", perl=TRUE)[[1]]
      # Remove braces
      links_owns <- gsub("\\{", "", links_owns, perl=TRUE)
      links_owns <- gsub("\\}", "", links_owns, perl=TRUE)
      # Convert to data frame
      links_owns <- as.data.frame(matrix(links_owns, nrow = 1))

    }))

    ## If no data has been reported, create an empty data frame with NA:
    if (purrr::is_empty(links_owns)) {
      links_owns <- data.frame(t(rep(NA, length(getexp(nl, "metrics.links")))))
    }

    names(links_owns) <- getexp(nl, "metrics.links")
    return(links_owns)
  })

  links_owns
}

## Clean turtle metrics
.util_clean_metrics_turtles <- function(NLtable, nl){

  ## Extract current string:
  turtles_string <- NLtable[, grepl(c("metrics.turtles"), names(NLtable))]

  turtles_owns <- purrr::map(seq_len(nrow(turtles_string)), function(x){

    ## Remove the outer [] brackets
    turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 2)
    turtles_string[x,][[1]] <- substring(turtles_string[x,][[1]], 1, max(nchar(turtles_string[x,][[1]])) - 1)
    turtles_string[x,][[1]]  <- gsub("breed", "", turtles_string[x,][[1]], perl=TRUE)
   # turtles_string[x,][[1]]  <- gsub("\\{", "(", turtles_string[x,][[1]], perl=TRUE)
  #  turtles_string[x,][[1]]  <- gsub("\\}", ")", turtles_string[x,][[1]], perl=TRUE)
    turtles_string[x,][[1]]  <- gsub("^ *|(?<= ) | *$", "", turtles_string[x,][[1]], perl = TRUE)
    turtles_string[x,][[1]]  <-  regmatches(turtles_string[x,][[1]], gregexpr("(?<=\\[).*?(?=\\])",turtles_string[x,][[1]], perl=TRUE))

    turtles_owns <- suppressWarnings(purrr::map_dfr(seq_along(turtles_string[x,][[1]][[1]]), function(turtles_index){

      # split turtle owns into unique elements of a vector
      turtles_owns <- strsplit(turtles_string[x,][[1]][[1]][turtles_index], "(\\{(?:[^{}]++|(?1))*\\})(*SKIP)(*F)| ", perl=TRUE)[[1]]
      # Remove braces
      turtles_owns <- gsub("\\{", "", turtles_owns, perl=TRUE)
      turtles_owns <- gsub("\\}", "", turtles_owns, perl=TRUE)
      # Convert to data frame
      as.data.frame(matrix(turtles_owns, nrow = 1))

    }))

    ## If no data has been reported, create an empty data frame with NA:
    if (purrr::is_empty(turtles_owns)) {
      turtles_owns <- data.frame(t(rep(NA, length(getexp(nl, "metrics.turtles")))))
    }

    names(turtles_owns) <- getexp(nl, "metrics.turtles")
    return(turtles_owns)
  })

  turtles_owns
}
