#' Analyze NetLogo simulation output
#'
#' @description Analyze NetLogo simulation output
#'
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @return analysis summary tibble
#'
#' @details
#' The analyze_nl function runs basic analyses on NetLogo simulation output.
#' In order to execute this function, simulation output needs to be attached to the simdesign first with \code{setsim(nl, "output") <- results}.
#'
#' analyze_nl calls different post-processing analysis functions, depending on the specified method in the simdesign object of the nl object.
#'
#'
#' \strong{The following simdesign are currently supported:}
#'
#' \link[nlrx]{simdesign_ff}
#'
#' Calls \link[nlrx]{analyze_ff}.
#' The function calculates aggregated output metrics by dropping random seeds and aggregating values with the provided functions.
#'
#' \link[nlrx]{simdesign_lhs}
#'
#' Calls \link[nlrx]{analyze_lhs}.
#' The function calculates aggregated output metrics by dropping random seeds and aggregating values with the provided functions.
#'
#' \link[nlrx]{simdesign_sobol}
#'
#' Calls \link[nlrx]{analyze_sobol}.
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' \link[nlrx]{simdesign_sobol2007}
#'
#' Calls \link[nlrx]{analyze_sobol2007}.
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' \link[nlrx]{simdesign_soboljansen}
#'
#' Calls \link[nlrx]{analyze_soboljansen}.
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' \link[nlrx]{simdesign_morris}
#'
#' Calls \link[nlrx]{analyze_morris}.
#' The function calculates morris sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' \link[nlrx]{simdesign_eFast}
#'
#' Calls \link[nlrx]{analyze_eFast}.
#' The function calculates eFast sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#'
#'
#'
#' \strong{For the following simdesign no postprocessing analysis function has been implemented yet:}
#'
#' \link[nlrx]{simdesign_simple},
#' \link[nlrx]{simdesign_distinct},
#' \link[nlrx]{simdesign_GenSA},
#' \link[nlrx]{simdesign_GenAlg}
#'
#' @examples
#'
#' # Load nl object including output data from testdata
#' nl <- nl_sobol
#'
#' # Define aggregation measurements:
#' myfuns <- list(mean=mean, sd=sd, min=min, max=max)
#'
#' # Calculate sensitivity indices:
#' analyze_nl(nl, funs = myfuns)
#'
#' @aliases analyze_nl
#' @rdname analyze_nl
#'
#' @export

analyze_nl <- function(nl, metrics = getexp(nl, "metrics"),
                       funs = list(mean = mean)) {

  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    stop("In order to run analyze_nl, output results have to be attached to the
         simdesign of the nl object first: setsim(nl, \"simoutput\")
         <- results")
  }


  method <- getsim(nl, "simmethod")

  if (method == "simple") {
    out <- analyze_simple(nl, metrics, funs)
  } else if (method == "ff") {
    out <- analyze_ff(nl, metrics, funs)
  } else if (method == "lhs") {
    out <- analyze_lhs(nl, metrics, funs)
  } else if (method == "morris") {
    out <- analyze_morris(nl, metrics, funs)
  } else if (method == "sobol") {
    out <- analyze_sobol(nl, metrics, funs)
  } else if (method == "sobol2007") {
    out <- analyze_sobol2007(nl, metrics, funs)
  } else if (method == "soboljansen") {
    out <- analyze_soboljansen(nl, metrics, funs)
  } else if (method == "eFast") {
    out <- analyze_eFast(nl, metrics, funs)
  } else {
    stop(paste0("No applicable analysis method for simmethod: ", method))
  }

  return(out)
}


#' Analyze NetLogo simulation output of simdesign simple
#'
#' @description Analyze NetLogo simulation output of simdesign simple
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The simdesign_simple analysis functions is not yet supported and will only print a warning message.
#'
#' @aliases analyze_simple
#' @rdname analyze_simple
#' @keywords internal
analyze_simple <- function(nl, metrics, funs) {
  warning("simdesign_simple analysis function is not yet supported!")
}


#' Analyze NetLogo simulation output of simdesign full-factorial
#'
#' @description Analyze NetLogo simulation output of simdesign full-factorial
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates aggregated output metrics by dropping random seeds and aggregating values with the provided functions.
#'
#' @aliases analyze_ff
#' @rdname analyze_ff
#' @keywords internal
analyze_ff <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  ## For ff we compute mean and sd values of each run/tick combination:
  ffagg <- getsim(nl, "simoutput") %>%
    dplyr::group_by_at(dplyr::vars(
      "siminputrow", "[step]",
      names(getsim(nl, "siminput"))
    )) %>%
    dplyr::summarise_at(metrics, funs) %>%
    dplyr::ungroup()

  ffagg <- tibble::as_tibble(ffagg)

  return(ffagg)
}



#' Analyze NetLogo simulation output of simdesign latin-hypercube
#'
#' @description Analyze NetLogo simulation output of simdesign latin-hypercube
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates aggregated output metrics by dropping random seeds and aggregating values with the provided functions.
#'
#' @aliases analyze_lhs
#' @rdname analyze_lhs
#' @keywords internal
analyze_lhs <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  ## For lhs we compute mean and sd values of each run/tick combination:
  lhsagg <- getsim(nl, "simoutput") %>%
    dplyr::group_by_at(dplyr::vars(
      "siminputrow", "[step]",
      names(getsim(nl, "siminput"))
    )) %>%
    dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
    dplyr::ungroup()

  lhsagg <- tibble::as_tibble(lhsagg)

  return(lhsagg)
}



#' Analyze NetLogo simulation output of simdesign sobol
#'
#' @description Analyze NetLogo simulation output of simdesign sobol
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' @aliases analyze_sobol
#' @rdname analyze_sobol
#' @keywords internal
analyze_sobol <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for (i in getsim(nl, "simseeds")) {

    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>%
      dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~ !all(is.na(.)))

    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))

    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(so, simoutput.i[j, ])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$parameter <- rownames(soS)
      soS$metric <- metrics[j]
      soS$seed <- i

      sensindex <- rbind(sensindex, soS)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as_tibble(sensindex)

  return(sensindex)
}



#' Analyze NetLogo simulation output of simdesign sobol2007
#'
#' @description Analyze NetLogo simulation output of simdesign sobol2007
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' @aliases analyze_sobol2007
#' @rdname analyze_sobol2007
#' @keywords internal
analyze_sobol2007 <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for (i in getsim(nl, "simseeds")) {

    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>%
      dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~ !all(is.na(.)))

    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(so, simoutput.i[j, ])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- metrics[j]
      soS$seed <- i
      soT <- so[["T"]]
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- metrics[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as_tibble(sensindex)

  return(sensindex)
}



#' Analyze NetLogo simulation output of simdesign soboljansen
#'
#' @description Analyze NetLogo simulation output of simdesign soboljansen
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates sobol sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' @aliases analyze_soboljansen
#' @rdname analyze_soboljansen
#' @keywords internal
analyze_soboljansen <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for (i in getsim(nl, "simseeds")) {

    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>%
      dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~ !all(is.na(.)))

    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(so, simoutput.i[j, ])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- metrics[j]
      soS$seed <- i
      soT <- so[["T"]]
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- metrics[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as_tibble(sensindex)

  return(sensindex)
}




#' Analyze NetLogo simulation output of simdesign morris
#'
#' @description Analyze NetLogo simulation output of simdesign morris
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates morris sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' @aliases analyze_morris
#' @rdname analyze_morris
#' @keywords internal
analyze_morris <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  sensindex <- NULL
  na.discovered <- FALSE
  mo <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for (i in getsim(nl, "simseeds")) {

    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>%
      dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~ !all(is.na(.)))

    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(mo, simoutput.i[j, ])

      if (anyNA(mo$ee))
      {
        na.discovered <- TRUE
      }


      mustar <- tibble::tibble(
        metric = metrics[j],
        parameter = colnames(mo$ee),
        index = "mustar",
        value = apply(mo$ee, 2, function(x) mean(abs(x), na.rm=TRUE)),
        seed = i
      )
      mu <- tibble::tibble(
        metric = metrics[j],
        parameter = colnames(mo$ee),
        index = "mu",
        value = apply(mo$ee, 2, function(x) mean(x, na.rm=TRUE)),
        seed = i
      )
      sigma <- tibble::tibble(
        metric = metrics[j],
        parameter = colnames(mo$ee),
        index = "sigma",
        value = apply(mo$ee, 2, function(x) stats::sd(x, na.rm=TRUE)),
        seed = i
      )

      sensindex <- rbind(sensindex, mustar, mu, sigma)
    }
  }

  # Print warning if NAs were discovered:
  if (isTRUE(na.discovered))
  {
    warning("NAs were discovered in the simulation output data during morris index calculation!")
  }

  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as_tibble(sensindex)

  return(sensindex)
}



#' Analyze NetLogo simulation output of simdesign eFast
#'
#' @description Analyze NetLogo simulation output of simdesign eFast
#' @param nl nl object
#' @param metrics vector of strings defining metric columns for evaluation. Defaults to metrics of the experiment within the nl object
#' @param funs list with the summary metrics for the sensitivity results
#' @details
#'
#' The function calculates eFast sensitivity indices from the output results using the \link[sensitivity]{sensitivity} package.
#'
#' @aliases analyze_eFast
#' @rdname analyze_eFast
#' @keywords internal
analyze_eFast <- function(nl, metrics, funs) {

  eval_simoutput(nl)

  sensindex <- NULL
  f99 <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for (i in getsim(nl, "simseeds")) {

    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>%
      dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~ !all(is.na(.)))

    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(f99, simoutput.i[j, ])

      D1 <- tibble::tibble(
        value = f99$D1,
        index = "first-order",
        parameter = names(getexp(nl, "variables")),
        metric = metrics[j],
        seed = i
      )
      Dt <- tibble::tibble(
        value = f99$Dt,
        index = "total",
        parameter = names(getexp(nl, "variables")),
        metric = metrics[j],
        seed = i
      )

      sensindex <- rbind(sensindex, D1, Dt)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as_tibble(sensindex)

  return(sensindex)
}
