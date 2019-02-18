#' Create a temporary behavior space xml file to setup NetLogo via command line
#'
#' @description Create a temporary behavior space xml file to setup NetLogo via
#' command line
#'
#' @param nl nl object
#' @param seed random-seed for NetLogo simulation
#' @param siminputrow row id of the simulation input tibble of the simdesign
#'  within the provided nl object
#' @param xmlfile filepath where the xml file is stored
#' @aliases util_create_sim_XML
#' @rdname util_create_sim_XML
#' @keywords internal
util_create_sim_XML <- function(nl, seed, siminputrow, xmlfile) {

  ### Get the current values from the siminput matrix:
  simdata_run <- getsim(nl, "siminput")[siminputrow, ]

  ### Attach a runnum variable if needed:
  if (!is.na(getexp(nl, "idrunnum"))) {
    runnum <- tibble::tibble(paste0("\"", getexp(nl, "expname"), "_", seed, "_",
                            siminputrow, "\""))
    names(runnum) <- getexp(nl, "idrunnum")
    simdata_run <- cbind(simdata_run, runnum)
  }

  ### Create XML object:
  nlXML <- XML::newXMLDoc()
  experiments <- XML::newXMLNode("experiments", doc = nlXML)
  experiment <- XML::newXMLNode("experiment",
    attrs = c(
      name = getexp(nl, "expname"),
      repetitions = getexp(nl, "repetition"),
      runMetricsEveryStep = getexp(nl, "tickmetrics")
    ),
    parent = experiments
  )

  ## Add Setup, go
  idsetup <- paste(getexp(nl, "idsetup"), sep = "\n", collapse = "\n")
  idgo <- paste(getexp(nl, "idgo"), sep = "\n", collapse = "\n")
  XML::addChildren(experiment, XML::newXMLNode("setup", idsetup,
                                               parent = experiment))
  XML::addChildren(experiment, XML::newXMLNode("go", idgo, parent = experiment))

  ## Add final commands if provided:
  if (!is.na(getexp(nl, "idfinal"))) {
    idfinal <- paste(getexp(nl, "idfinal"), sep = "\n", collapse = "\n")
    XML::addChildren(experiment, XML::newXMLNode("final",
      idfinal,
      parent = experiment
    ))
  }

  ## Add timeLimit:
  runtime <- getexp(nl, "runtime")
  ## If runtime = NA_integer_ (infinite) change to 0 as required by BehaviorSpace
  if (is.na(runtime)) {
    runtime <- 0
  }
  XML::addChildren(experiment, XML::newXMLNode("timeLimit",
    attrs = c(steps = runtime),
    parent = experiment
  ))

  ## Add stop condition if provided:
  if (!is.na(getexp(nl, "stopcond"))) {
    stopcond <- paste(getexp(nl, "stopcond"), sep = "\n", collapse = "\n")
    XML::addChildren(experiment, XML::newXMLNode("exitCondition",
      stopcond,
      parent = experiment
    ))
  }

  ## Add metrics:
  metrics <- getexp(nl, "metrics")

  # Add turtle metrics if defined
  if (all(!is.na(getexp(nl, "metrics.turtles")))) {
    # Loop trough breed sublists:
    turtles.reporter <- purrr::map_chr(seq_along(nl@experiment@metrics.turtles), function(x) {
      x.breed <- names(nl@experiment@metrics.turtles)[[x]]
      x.metrics <- c("breed", nl@experiment@metrics.turtles[[x]])
      turtles.reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(x.metrics, collapse = paste0("\",\"")), ")] of ", x.breed, ")")
      return(turtles.reporter)
    })
    metrics <- c(metrics, turtles.reporter)
  }

  # add patch metrics if defined
  if (all(!is.na(getexp(nl, "metrics.patches")))) {
    patches.reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(getexp(nl, "metrics.patches"), collapse = paste0("\",\"")), ")] of patches)")
    metrics <- c(metrics, patches.reporter)
  }

  # add link metrics if defined
  # nocov start
  if (all(!is.na(getexp(nl, "metrics.links")))) {
    links.reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(getexp(nl, "metrics.links"), collapse = paste0("\",\"")), ")] of links)")
    metrics <- c(metrics, links.reporter)
  }
  # nocov end

  for (i in metrics) {
    XML::addChildren(experiment, XML::newXMLNode("metric",
      i,
      parent = experiment
    ))
  }

  ## Add parameters and values:
  for (i in seq_along(simdata_run)) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet",
      attrs = c(
        variable =
          names(
            simdata_run[i]
          )
      ),
      XML::newXMLNode("value",
        attrs =
          c(
            value =
              simdata_run[[i]]
          )
      )
    ))
  }
  ## If repetition > 1 we use a ranodm seed, otherwise the provided seed:
  if (getexp(nl, "repetition") == 1) {
    XML::addChildren(experiment, XML::newXMLNode("enumeratedValueSet",
      attrs = c(
        variable =
          "random-seed"
      ),
      XML::newXMLNode("value",
        attrs = c(value = seed)
      )
    ))
  }

  ## Use NetLogo specific prefix:
  prefix <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE experiments
  SYSTEM \"behaviorspace.dtd\">"

  # SAVE XML TO FILE
  cat(XML::saveXML(nlXML, prefix = prefix), file = xmlfile)
}

#' Setup and execute NetLogo via command line
#'
#' @description Setup and execute NetLogo via command line
#'
#' @param nl nl object
#' @param xmlfile file location of the experiment xml file
#' @param outfile file location for output results
#' @param batchfile file location of system specific batch file to call NetLogo
#' via command line
#' @aliases util_call_nl
#' @rdname util_call_nl
#' @keywords internal
util_call_nl <- function(nl, xmlfile, outfile, batchfile) {
  os <- util_get_os()
  if (os %in% c("win", "unix"))
  {
    NLcall <- paste0("\"", batchfile, "\"", " --model ", "\"",
                     getnl(nl, "modelpath"), "\"", " --setup-file ", "\"",
                     xmlfile, "\"", " --experiment ", getexp(nl, "expname"),
                     " --table ", "\"", outfile, "\"", " --threads ", 1)
  }
  if (os == "mac")
  {
    NLcall <- paste0("sh \"", batchfile, "\"", " --model ", "\"",
                     getnl(nl, "modelpath"), "\"", " --setup-file ", "\"",
                     xmlfile, "\"", " --experiment ", getexp(nl, "expname"),
                     " --table ", "\"", outfile, "\"", " --threads ", 1)
  }
  system(NLcall, wait = TRUE)
}

#' Delete temporary files
#'
#' @description Delete temporary files
#'
#' @param nl nl object
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results.
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results.
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results.
#' @param cleanup.files vector with paths to temporary created files (csv, xml, bat)
#' @aliases util_cleanup
#' @rdname util_cleanup
#' @keywords internal
util_cleanup <- function(nl,
                         cleanup.csv = TRUE,
                         cleanup.xml = TRUE,
                         cleanup.bat = TRUE,
                         cleanup.files) {
  if (isTRUE(cleanup.csv)) {
    file.remove(cleanup.files$csv[[1]])
  }
  if (isTRUE(cleanup.xml)) {
    file.remove(cleanup.files$xml[[1]])
  }
  if (isTRUE(cleanup.bat)) {
    file.remove(cleanup.files$bat[[1]])
  }
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

  # Check if csv file exists:
  if (!file.exists(outfile))
  {
    stop(paste0("Temporary output file ", outfile, "not found. On unix systems this can happen if the default system temp folder is used.
                Try reassigning the default temp folder for this R session (unixtools package)."))
  }

  NLtable <- readr::read_csv(outfile, skip = 6, col_types = readr::cols())

  ## Check if results is empty:
  if (purrr::is_empty(NLtable)) {
    stop("Output file is empty - simulation aborted due to a runtime error!
         Make sure that parameter value definitions of the experiment are valid and the model code is running properly!")
  }

  ## if we have results, add siminputrow
  NLtable$siminputrow <- siminputrow

  # Check if tickmetrics is true, if not, we only keep the last reported line:
  if (getexp(nl, "tickmetrics") == "false") {

    # Report line with max step:
    NLtable <- NLtable %>% dplyr::filter(`[step]` == max(`[step]`))
  } else {

    # We filter all evalticks lines from the table
    if (all(!is.na(getexp(nl, "evalticks"))))
    {
      NLtable <- NLtable %>% dplyr::filter(`[step]` %in% getexp(nl, "evalticks"))
      # We then chek if there are ticks, that have reported no results:
      noeval <- getexp(nl, "evalticks")[!which(getexp(nl, "evalticks") %in%
                                                 NLtable$`[step]`)]

      if (length(noeval) > 0) {
        message(paste0("No model results reported for siminputrow ",
                       siminputrow, " on ticks ", noeval))
      }
    }
  }

  # Finally check if the tibble is still empty:
  if (nrow(NLtable) == 0) {

    # Create an na line:
    NArow <- tibble::tibble(`[run number]` = NA)
    NArow <- cbind(NArow, getsim(nl, "siminput")[siminputrow, ])
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

    for(x in seq_along(nl@experiment@metrics.turtles)) {
      x.breed <- names(nl@experiment@metrics.turtles)[[x]]
      x.metrics <- c("breed", nl@experiment@metrics.turtles[[x]])
      col.name <- paste0("metrics.", x.breed)
      turtles.reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(x.metrics, collapse = paste0("\",\"")), ")] of ", x.breed, ")")
      names(NLtable)[names(NLtable) == turtles.reporter] <- col.name
      NLtable[, grepl(col.name, names(NLtable))] <-
        list(.util_clean_metrics_turtles(NLtable, nl, col.name, x.metrics))
      return(turtles.reporter)
    }

  }

  if (all(!is.na(getexp(nl, "metrics.patches")))) {
    ## Rename column and clean patch metrics
    NLtable <- NLtable %>% dplyr::rename(metrics.patches =
                                           paste0("but-first but-last (word [remove \" \" (word ", paste(getexp(nl, "metrics.patches"), collapse = paste0("\",\"")), ")] of patches)"))
    NLtable$metrics.patches <-
      .util_clean_metrics_patches(NLtable, nl)
  }

  # nocov start
  if (all(!is.na(getexp(nl, "metrics.links")))) {

    for(x in seq_along(nl@experiment@metrics.links)) {
      x.breed <- names(nl@experiment@metrics.links)[[x]]
      x.metrics <- c("breed", nl@experiment@metrics.links[[x]])
      col.name <- paste0("metrics.", x.breed)
      links.reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(x.metrics, collapse = paste0("\",\"")), ")] of ", x.breed, ")")
      names(NLtable)[names(NLtable) == links.reporter] <- col.name
      NLtable[, grepl(col.name, names(NLtable))] <-
        list(.util_clean_metrics_links(NLtable, nl, col.name, x.metrics))
      return(links.reporter)
    }

    ## Rename column and clean link metrics
    NLtable <- NLtable %>% dplyr::rename(metrics.links =
                                           paste0("but-first but-last (word [remove \" \" (word ", paste(getexp(nl, "metrics.links"), collapse = paste0("\",\"")), ")] of links)"))
    NLtable[, grepl(c("metrics.links"), names(NLtable))] <-
      list(.util_clean_metrics_links(NLtable, nl))
  }
  # nocov end

  return(NLtable)
}



## Clean patch metrics
.util_clean_metrics_patches <- function(NLtable, nl) {

  patches_string <- NLtable[, grepl(c("metrics.patches"), names(NLtable))]
  patches_string <- stringr::str_split(patches_string$metrics.patches, " ")
  patches_string <- purrr::map(patches_string, function(x) {
    patches_owns <- tibble::as.tibble(x = x)
    patches_owns <- tidyr::separate(patches_owns, value,
                                    getexp(nl, "metrics.patches"), sep=",")
    patches_owns <- dplyr::mutate_all(patches_owns, function(x) {
      suppressWarnings(ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x))))
    })
    return(patches_owns)
  })
  return(patches_string)
}


.util_clean_metrics_turtles <- function(NLtable, nl, col.name, metrics) {

  turtles_string <- NLtable[, grepl(col.name, names(NLtable))]
  turtles_string <- stringr::str_split(dplyr::pull(turtles_string, col.name), " ")
  turtles_string <- purrr::map(turtles_string, function(x) {
    turtles_owns <- tibble::as.tibble(x = x)
    turtles_owns <- tidyr::separate(turtles_owns,
                                    value,
                                    metrics,
                                    sep=",")
    turtles_owns <- dplyr::mutate_all(turtles_owns, function(x) {
      suppressWarnings(ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x))))
    })
    turtles_owns$agent <- "turtles"
    return(turtles_owns)
  })
  return(turtles_string)
}

# nocov start
.util_clean_metrics_links <- function(NLtable, nl) {

  links_string <- NLtable[, grepl(c("metrics.links"), names(NLtable))]
  links_string <- stringr::str_split(links_string$metrics.links, " ")
  links_string <- purrr::map(links_string, function(x) {
    links_owns <- tibble::as.tibble(x = x)
    links_owns <- tidyr::separate(links_owns, value,
                                  getexp(nl, "metrics.links"), sep=",")
    links_owns <- dplyr::mutate_all(links_owns, function(x) {
      suppressWarnings(ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x))))
    })
    return(links_owns)
  })
  return(links_string)
}
# nocov end

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

  # nocov start
  if (os == "win") {
    if (getnl(nl, "nlversion") == "5.3.1") {
      ## NetLogo 5.3.1 does not contain a premade batchfile in the
      ## installation directory
      ## Thus, we have to write the batchfile manually

      # Block 1 of the batch file:
      block1 <- c(
        "@echo off",
        "setlocal ENABLEDELAYEDEXPANSION",
        "set BASE_DIR=%~dp0",
        "if defined JAVA_HOME (",
        "  set \"JAVA=%JAVA_HOME%\\bin\\java.exe\"",
        ") ELSE (",
        "  ECHO JAVA_HOME not defined, using java on PATH.",
        "  ECHO If you encounter errors, set JAVA_HOME or update your PATH to
        include java.exe.",
        "  set \"JAVA=java.exe\"",
        ")"
      )

      # JVM_OPTS line:
      extensionspath <- file.path(getnl(nl, "nlpath"), "app/extensions")
      jvmoptsline <- paste0("SET \"JVM_OPTS=-Xmx", getnl(nl, "jvmmem"),
                            "m -XX:+UseParallelGC -Dfile.encoding=UTF-8 ",
                            "-Dnetlogo.extensions.dir=^\"", extensionspath,
                            "^\"\"")

      # Block 2 of the batch file:
      block2 <- c(
        "set ARGS=",
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
        ")"
      )

      # Classpath line:
      jarpath <- file.path(getnl(nl, "nlpath"), "app/NetLogo.jar")
      jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")

      # Block 3 of the batch file:
      block3 <- c("\"%JAVA%\" %JVM_OPTS% -classpath \"%ABSOLUTE_CLASSPATH%\"
                  org.nlogo.headless.Main %ARGS%")

      # Put all blocks together:
      allblocks <- c(block1, jvmoptsline, block2, jarpathline, block3)

      ## Write batch file:
      batchpath_temp <- tempfile(pattern = "netlogo-headless", fileext = ".bat")
      writeLines(allblocks, batchpath_temp)

    } else {
      ## For all other NetLogo versions we can just copy the headless bat from
      ## the installation folder:
      # Prepare pathes:
      batchpath <- file.path(getnl(nl, "nlpath"), "netlogo-headless.bat")

      # Extensions Folder:
      extensionspath <- file.path(getnl(nl, "nlpath"), "app/extensions")
      jarpath <- file.path(getnl(nl, "nlpath"), paste0("app/netlogo-",
                        getnl(nl, "nlversion"), ".jar"))

      # jvmoptions string:
      jvmoptsline <- paste0("SET \"JVM_OPTS=-Xmx",
                            getnl(nl, "jvmmem"),
                            "m -XX:+UseParallelGC -Dfile.encoding=UTF-8 ",
                            "-Dnetlogo.extensions.dir=^\"",
                            extensionspath, "^\"\"")
      jarpathline <- paste0("SET \"ABSOLUTE_CLASSPATH=", jarpath, "\"")

      # Read batchfile (on windows use nlpath\netlogo-headless.bat, on linux and
      # mac nlpath\netlogo-headless.sh)
      batch <- readr::read_lines(batchpath)

      # Get position index of jvmopts and jarpath line
      pos_jvmopts <- which(grepl("SET \"JVM_OPTS=-Xmx", batch))
      pos_jarpath <- which(grepl("SET \"ABSOLUTE_CLASSPATH=", batch))

      # Replace lines in batch with updated versions
      batch[pos_jvmopts] <- jvmoptsline
      batch[pos_jarpath] <- jarpathline

      # Create new batchfile:
      batchpath_temp <- tempfile(pattern = "netlogo-headless", fileext = ".bat")
      readr::write_lines(batch, path = batchpath_temp)
    }
  }
  # nocov end
  if (os %in% c("unix", "mac")) {
    if (getnl(nl, "nlversion") == "5.3.1") {
      ## NetLogo 5.3.1 does not contain a premade shfile in the installation
      ## directory
      ## Thus, we have to write the shfile manually

      # Block1 of netlogo-headless.sh:
      block1 <- c("#!/bin/sh")

      # Basedirline:
      basedirline <- paste0("cd \"", getnl(nl, "nlpath"), "app/\"")

      # jvmoptsline:
      jvmoptsline <- paste0("java -Xmx",
                            getnl(nl, "jvmmem"),
                            "m -Dfile.encoding=UTF-8 -classpath NetLogo.jar ",
                            "org.nlogo.headless.Main \"$@\"")

      # Put all blocks together:
      allblocks <- c(block1, basedirline, jvmoptsline)

      ## Write batch file:
      batchpath_temp <- tempfile(pattern = "netlogo-headless", fileext = ".sh")
      writeLines(allblocks, batchpath_temp)

      ## Make sh executable on linux:
      system(paste0("chmod +x ", batchpath_temp), wait = TRUE)
    } else {
      ## For all other NetLogo versions we can just copy and modify the headless
      ## sh from the installation folder:
      ## Create path variables:
      batchpath <- file.path(getnl(nl, "nlpath"), "netlogo-headless.sh")
      batchpath_temp <- tempfile(pattern = "netlogo-headless", fileext = ".sh")

      # Copy original file to temppath file
      system(paste0("cp \"", batchpath, "\" \"", batchpath_temp, "\""),
             wait = TRUE)

      # Define edited lines for shell script:
      basedirline <- paste0("BASE_DIR=\"", getnl(nl, "nlpath"), "\"")
      jvmoptsline <- paste0("JVM_OPTS=(-Xmx",
                            getnl(nl, "jvmmem"),
                            "m -Dfile.encoding=UTF-8)")

      ## Edit lines in place:
      system(paste0("sed -i -r 's!^BASE_DIR=.*!",
                    basedirline, "!'", " \"",
                    batchpath_temp, "\""))
      system(paste0("sed -i -r 's!^JVM_OPTS=.*!",
                    jvmoptsline, "!'", " \"",
                    batchpath_temp, "\""))
    }
  }


  return(batchpath_temp)
}
