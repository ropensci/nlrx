# Postprocessing

write_simoutput <- function(nl) {

  outfilename <- paste0(outpath(nl), expname(nl), "_", simmethod(nl), ".csv")
  readr::write_csv(x = simoutput(nl), path = outfilename)

}


analyze_nl <- function(nl) {

  method <- simmethod(nl)

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


analyze_simple <- function(nl) {

}

analyze_ff <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  ffagg <- simoutput(nl) %>% dplyr::group_by_at(vars("[step]", names(siminput(nl)))) %>% dplyr::summarise_at(metrics(nl), funs(mean, sd, min, max))
  return(ffagg)

}


analyze_lhs <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  lhsagg <- simoutput(nl) %>% dplyr::group_by_at(vars("[step]", names(siminput(nl)))) %>% dplyr::summarise_at(metrics(nl), funs(mean, sd, min, max))
  return(lhsagg)
}



analyze_sobol <- function(nl) {

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(metrics(nl), funs(mean))  %>% dplyr::select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))

    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
           sensitivity::tell(so, simoutput.i[j,])
           soS <- so$S
           soS[soS < 0] <- 0
           soS[soS > 1] <- 1
           soS$parameter <- rownames(soS)
           soS$metric <- metrics(nl)[j]
           soS$seed <- i

           sensindex <- rbind(sensindex, soS)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



analyze_sobol2007 <- function(nl) {

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(metrics(nl), funs(mean))  %>% dplyr::select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(so, simoutput.i[j,])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- metrics(nl)[j]
      soS$seed <- i
      soT <- so$T
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- metrics(nl)[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



analyze_soboljansen <- function(nl) {

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(metrics(nl), funs(mean))  %>% dplyr::select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(so, simoutput.i[j,])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- metrics(nl)[j]
      soS$seed <- i
      soT <- so$T
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- metrics(nl)[j]
      soT$seed <- i

      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}





analyze_morris <- function(nl) {

  sensindex <- NULL
  mo <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% dplyr::filter(`random-seed` == i) %>% group_by(`[run number]`) %>% dplyr::summarise_at(metrics(nl), funs(mean))  %>% dplyr::select(metrics(nl))
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
                              value=apply(mo$ee, 2, sd),
                              seed=i)

      sensindex <- rbind(sensindex, mustar, mu, sigma)
    }
  }

  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}


analyze_eFast <- function(nl) {

  sensindex <- NULL
  f99 <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% dplyr::filter(`random-seed` == i) %>% dplyr::group_by(`[run number]`) %>% dplyr::summarise_at(metrics(nl), funs(mean))  %>% dplyr::select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      sensitivity::tell(f99, simoutput.i[j,])

      D1 <- tibble::tibble(value = f99$D1,
                           index="first-order",
                           parameter=names(variables(nl)),
                           metric=metrics(nl)[j],
                           seed=i)
      Dt <- tibble::tibble(value = f99$Dt,
                           index="total",
                           parameter=names(variables(nl)),
                           metric=metrics(nl)[j],
                           seed=i)

      sensindex <- rbind(sensindex, D1, Dt)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}
