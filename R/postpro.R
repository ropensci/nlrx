# Postprocessing

write_simoutput <- function(nl) {

  library(readr)

  outfilename <- paste0(outpath(nl), expname(nl), "_", simmethod(nl), ".csv")
  write_csv(x = simoutput(nl), path = outfilename)

}


postpro <- function(nl) {

  method <- simmethod(nl)

  if (method == "simple") {
    out <- postpro_simple(nl)
  }
  if (method == "ff") {
    out <- postpro_ff(nl)
  }
  if (method == "lhs") {
    out <- postpro_lhs(nl)
  }
  if (method == "morris") {
    out <- postpro_morris(nl)
  }
  if (method == "sobol") {
    out <- postpro_sobol(nl)
  }
  if (method == "sobol2007") {
    out <- postpro_sobol2007(nl)
  }
  if (method == "soboljansen") {
    out <- postpro_soboljansen(nl)
  }
  if (method == "eFast") {
    out <- postpro_eFast(nl)
  }


  return(out)
}


postpro_simple <- function(nl) {

}

potspro_ff <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  library(dplyr)
  ffagg <- simoutput(nl) %>% group_by_at(vars("[step]", names(siminput(nl)))) %>% summarise_at(metrics(nl), funs(mean, sd, min, max))
  return(ffagg)

}


postpro_lhs <- function(nl) {

  ## For lhs we compute mean and sd values of each run/tick combination:
  library(dplyr)
  lhsagg <- simoutput(nl) %>% group_by_at(vars("[step]", names(siminput(nl)))) %>% summarise_at(metrics(nl), funs(mean, sd, min, max))
  return(lhsagg)
}



postpro_sobol <- function(nl) {

  library(sensitivity)
  library(dplyr)

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% filter(`random-seed` == i) %>% group_by(`[run number]`) %>% summarise_at(metrics(nl), funs(mean))  %>% select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
           tell(so, simoutput.i[j,])
           ss <- so$S
           ss[ss < 0] <- 0
           ss[ss > 1] <- 1
           ss$parameter <- rownames(ss)
           ss$metric <- metrics(nl)[j]
           ss$seed <- i

           sensindex <- rbind(sensindex, ss)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



postpro_sobol2007 <- function(nl) {

  library(sensitivity)
  library(dplyr)

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% filter(`random-seed` == i) %>% group_by(`[run number]`) %>% summarise_at(metrics(nl), funs(mean))  %>% select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      tell(so, simoutput.i[j,])
      ss <- so$S
      ss[ss < 0] <- 0
      ss[ss > 1] <- 1
      ss$index <- "first-order"
      ss$parameter <- rownames(ss)
      ss$metric <- metrics(nl)[j]
      ss$seed <- i
      tt <- so$T
      tt[tt < 0] <- 0
      tt[tt > 1] <- 1
      tt$index <- "total"
      tt$parameter <- rownames(ss)
      tt$metric <- metrics(nl)[j]
      tt$seed <- i

      sensindex <- rbind(sensindex, ss, tt)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}



postpro_soboljansen <- function(nl) {

  library(sensitivity)
  library(dplyr)

  sensindex <- NULL
  so <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% filter(`random-seed` == i) %>% group_by(`[run number]`) %>% summarise_at(metrics(nl), funs(mean))  %>% select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      tell(so, simoutput.i[j,])
      ss <- so$S
      ss[ss < 0] <- 0
      ss[ss > 1] <- 1
      ss$index <- "first-order"
      ss$parameter <- rownames(ss)
      ss$metric <- metrics(nl)[j]
      ss$seed <- i
      tt <- so$T
      tt[tt < 0] <- 0
      tt[tt > 1] <- 1
      tt$index <- "total"
      tt$parameter <- rownames(ss)
      tt$metric <- metrics(nl)[j]
      tt$seed <- i

      sensindex <- rbind(sensindex, ss, tt)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL

  return(sensindex)

}





postpro_morris <- function(nl) {

  library(sensitivity)
  library(dplyr)

  sensindex <- NULL
  mo <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% filter(`random-seed` == i) %>% group_by(`[run number]`) %>% summarise_at(metrics(nl), funs(mean))  %>% select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      tell(mo, simoutput.i[j,])

      mustar <- data.frame(metric=metrics(nl)[j],
                           parameter=colnames(mo$ee),
                           index="mustar",
                           value=apply(mo$ee, 2, function(x) mean(abs(x))),
                           seed=i)
      mu <- data.frame(metric=metrics(nl)[j],
                       parameter=colnames(mo$ee),
                       index="mu",
                       value=apply(mo$ee, 2, mean),
                       seed=i)
      sigma <- data.frame(metric=metrics(nl)[j],
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


postpro_eFast <- function(nl) {

  library(sensitivity)
  library(dplyr)

  sensindex <- NULL
  f99 <- simobject(nl)[[1]]

  # Calculate sensitivity indices separately for each random seed:
  for(i in simseeds(nl)) {

    # Select current seed runs, aggregate across steps and select only output columns:
    simoutput.i <- simoutput(nl) %>% filter(`random-seed` == i) %>% group_by(`[run number]`) %>% summarise_at(metrics(nl), funs(mean))  %>% select(metrics(nl))
    simoutput.i <- t(as.matrix(simoutput.i))


    # Loop over metric columns and calculate sensitivity indices:
    for (j in (1:length(simoutput.i[,1]))) {
      tell(f99, simoutput.i[j,])

      D1 <- tibble(value = f99$D1,
                   index="first-order",
                   parameter=names(variables(nl)),
                   metric=metrics(nl)[j],
                   seed=i)
      Dt <- tibble(value = f99$Dt,
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
