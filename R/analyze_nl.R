
#' Write NetLogo simulation output to file
#'
#' @description Write NetLogo simulation output to file
#'
#' @param nl nl object
#'
#' Write NetLogo simulation output to a csv file in the direcotry outpath of the nl object
#' Output has to be attached to the simdesign first with simoutput(nl) <- results
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
#' # Attach output to simdesign:
#' setsim(nl, "simoutput") <- results
#'
#' # Write output to outpath directory
#' write_simoutput(nl)
#'
#' }
#' @aliases write_simoutput
#' @rdname write_simoutput
#'
#' @export

write_simoutput <- function(nl) {

  outfilename <- paste0(getexp(nl, "outpath"), getexp(nl, "expname"), "_", getsim(nl, "simmethod"), ".csv")
  readr::write_csv(x = getsim(nl, "simoutput"), path = outfilename)

}


#' Analyze NetLogo simulation output
#'
#' @description Analyze NetLogo simulation output
#'
#' @param nl nl object
#'
#' Runs basic analyses on NetLogo simulation output
#' Output has to be attached to the simdesign first with simoutput(nl) <- results
#'
#' The functions calls post-processing analysis functions, depending on the specified method in the simdesign object of the nl object.
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
#' # Attach output to simdesign:
#' simoutput(nl) <- results
#'
#' # Perform analysis:
#' analyze_nl(nl)
#'
#' }
#' @aliases analyze_nl
#' @rdname analyze_nl
#'
#' @export

analyze_nl <- function(nl) {

  method <- getsim(nl, "simmethod")

  if (method == "simple") {
    out <- analyze_simple(nl)
  }
  if (method == "ff") {
    out <- analyze_ff(nl)
  }
  if (method == "lhs") {
    out <- analyze_lhs(nl)
  }
  if (method == "morris") {
    out <- analyze_morris(nl)
  }
  if (method == "sobol") {
    out <- analyze_sobol(nl)
  }
  if (method == "sobol2007") {
    out <- analyze_sobol2007(nl)
  }
  if (method == "soboljansen") {
    out <- analyze_soboljansen(nl)
  }
  if (method == "eFast") {
    out <- analyze_eFast(nl)
  }


  return(out)
}


#' Analyze NetLogo simulation output of simdesign simple
#'
#' @description Analyze NetLogo simulation output of simdesign simple
#' @param nl nl object
#' @aliases analyze_simple
#' @rdname analyze_simple

analyze_simple <- function(nl) {

}


#' Analyze NetLogo simulation output of simdesign full-factorial
#'
#' @description Analyze NetLogo simulation output of simdesign full-factorial
#' @param nl nl object
#' @aliases analyze_ff
#' @rdname analyze_ff

analyze_ff <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  ffagg <- getsim(nl, "simoutput") %>% dplyr::group_by_at(vars("[step]", names(getsim(nl, "siminput")))) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean, stats::sd, min, max))
  return(ffagg)

}



#' Analyze NetLogo simulation output of simdesign latin-hypercube
#'
#' @description Analyze NetLogo simulation output of simdesign latin-hypercube
#' @param nl nl object
#' @aliases analyze_lhs
#' @rdname analyze_lhs

analyze_lhs <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  lhsagg <- getsim(nl, "simoutput") %>% dplyr::group_by_at(vars("[step]", names(getsim(nl, "siminput")))) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean, stats::sd, min, max))
  return(lhsagg)
}



#' Analyze NetLogo simulation output of simdesign sobol
#'
#' @description Analyze NetLogo simulation output of simdesign sobol
#' @param nl nl object
#' @aliases analyze_sobol
#' @rdname analyze_sobol

analyze_sobol <- function(nl) {

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean))  %>% dplyr::select(getexp(nl, "metrics"))
    simoutput.i <- t(as.matrix(simoutput.i))

    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
           sensitivity::tell(so, simoutput.i[j,])
           soS <- so$S
           soS[soS < 0] <- 0
           soS[soS > 1] <- 1
           soS$parameter <- rownames(soS)
           soS$metric <- getexp(nl, "metrics")[j]
           soS$seed <- i

           sensindex <- rbind(sensindex, soS)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



#' Analyze NetLogo simulation output of simdesign sobol2007
#'
#' @description Analyze NetLogo simulation output of simdesign sobol2007
#' @param nl nl object
#' @aliases analyze_sobol2007
#' @rdname analyze_sobol2007

analyze_sobol2007 <- function(nl) {

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean))  %>% dplyr::select(getexp(nl, "metrics"))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(so, simoutput.i[j,])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- getexp(nl, "metrics")[j]
      soS$seed <- i
      soT <- so$T
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- getexp(nl, "metrics")[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



#' Analyze NetLogo simulation output of simdesign soboljansen
#'
#' @description Analyze NetLogo simulation output of simdesign soboljansen
#' @param nl nl object
#' @aliases analyze_soboljansen
#' @rdname analyze_soboljansen

analyze_soboljansen <- function(nl) {

  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean))  %>% dplyr::select(getexp(nl, "metrics"))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(so, simoutput.i[j,])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- getexp(nl, "metrics")[j]
      soS$seed <- i
      soT <- so$T
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- getexp(nl, "metrics")[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}




#' Analyze NetLogo simulation output of simdesign morris
#'
#' @description Analyze NetLogo simulation output of simdesign morris
#' @param nl nl object
#' @aliases analyze_morris
#' @rdname analyze_morris


analyze_morris <- function(nl) {

  sensindex <- NULL
  mo <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>% dplyr::filter(`random-seed` == i) %>% group_by(`[run number]`) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean))  %>% dplyr::select(getexp(nl, "metrics"))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(mo, simoutput.i[j,])

      mustar <- tibble::tibble(metric=metrics(nl)[j],
                               parameter=colnames(mo$ee),
                               index="mustar",
                               value=apply(mo$ee, 2, function(x) mean(abs(x))),
                               seed=i)
      mu <- tibble::tibble(metric=metrics(nl)[j],
                           parameter=colnames(mo$ee),
                           index="mu",
                           value=apply(mo$ee, 2, mean),
                           seed=i)
      sigma <- tibble::tibble(metric=metrics(nl)[j],
                              parameter=colnames(mo$ee),
                              index="sigma",
                              value=apply(mo$ee, 2, stats::sd),
                              seed=i)

      sensindex <- rbind(sensindex, mustar, mu, sigma)
    }
  }

  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



#' Analyze NetLogo simulation output of simdesign eFast
#'
#' @description Analyze NetLogo simulation output of simdesign eFast
#' @param nl nl object
#' @aliases analyze_eFast
#' @rdname analyze_eFast

analyze_eFast <- function(nl) {

  sensindex <- NULL
  f99 <- getsim(nl, "simobject")[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(getexp(nl, "metrics"), funs(mean))  %>% dplyr::select(getexp(nl, "metrics"))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(f99, simoutput.i[j,])

      D1 <- tibble::tibble(value = f99$D1,
                           index="first-order",
                           parameter=names(getexp(nl, "variables")),
                           metric=getexp(nl, "metrics")[j],
                           seed=i)
      Dt <- tibble::tibble(value = f99$Dt,
                           index="total",
                           parameter=names(getexp(nl, "variables")),
                           metric=getexp(nl, "metrics")[j],
                           seed=i)

      sensindex <- rbind(sensindex, D1, Dt)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}
