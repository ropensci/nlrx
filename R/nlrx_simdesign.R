

# Create a latin hypercube from input data
nlrx_create_lhs <- function(input.values, sample.count, precision) {

  library(lhs)
  library(tibble)

  # create a random sample of input factor sets (Latin Hypercube Sampling)
  lhs.design <- randomLHS(sample.count, length(input.values))
  # transform the standardized random values to the real input value range
  # and apply the desired random distribution
  lhs.design <- lapply(seq(1,length(input.values)), function(i) {
    match.fun(input.values[[i]]$quantile.function)(lhs.design[,i], input.values[[i]]$min, input.values[[i]]$max)
  })
  names(lhs.design) <- names(input.values)
  lhs.final <- as.tibble(lhs.design)
  ## Precision:
  lhs.final <- round(lhs.final, digits = precision)

  return(lhs.final)
}

nlrx_generate_seeds <- function(sim.seedrep) {

  seeds <- ceiling(runif(sim.seedrep, 0, 10000))
  return(seeds)
}



# Create and attach a lhs simulation Design
nlrx_add_simdesign_lhs <- function(nl, sim.seedrep, sim.sample, sim.digits) {

  input.values <- nl@experiment@param.change
  fixed.values <- nl@experiment@param.const

  lhs <- nlrx_create_lhs(input.values = input.values,
                         sample.count = sim.sample,
                         precision = sim.digits)

  lhs <- as.tibble(cbind(lhs, fixed.values, stringsAsFactors=FALSE))
  seeds <- nlrx_generate_seeds(sim.seedrep)

  # Cretae new simdesign and add to nl
  nl@experiment@simdesign <- new("simdesign",
                                 method="lhs",
                                 simdata=lhs,
                                 simseeds=seeds)

  return(nl)

}


nlrx_add_simdesign_sobol <- function(nl, sim.seedrep, sim.sample, sim.order, sim.nboot, sim.digits) {

  library(sensitivity)
  library(tibble)

  input.values <- nl@experiment@param.change
  fixed.values <- nl@experiment@param.const

  input.sets.1 <- nlrx_create_lhs(input.values=input.values,
                                 sample.count=sim.sample,
                                 precision=sim.digits)
  input.sets.2 <- nlrx_create_lhs(input.values=input.values,
                                 sample.count=sim.sample,
                                 precision=sim.digits)

  # create instance of sobol class
  so <- sobol(model = NULL, X1 = input.sets.1, X2 = input.sets.2, order=sim.order, nboot = sim.nboot)
  soX <- as.tibble(cbind(so$X, fixed.values, stringsAsFactors=FALSE))
  seeds <- nlrx_generate_seeds(sim.seedrep)

  # Cretae new simdesign and add to nl
  nl@experiment@simdesign <- new("simdesign",
                                 method="sobol",
                                 simdata=soX,
                                 simseeds=seeds,
                                 simobject=list(so))

  return(nl)
}

nlrx_add_simdesign_morris <- function(nl, sim.seedrep, sim.type, sim.levels, sim.rep, sim.grid.jump) {

  library(sensitivity)
  library(tibble)

  morris.design <- list(type = sim.type, levels = sim.levels, grid.jump = sim.grid.jump)

  input.values <- nl@experiment@param.change
  fixed.values <- nl@experiment@param.const

  # get the min and max values of the input factor ranges
  mins <- sapply(seq(1,length(input.values)), function(i) {
    input.values[[i]]$min})
  maxs <- sapply(seq(1,length(input.values)), function(i) {
    input.values[[i]]$max})

  # create input sets
  mo <- morris(model = NULL, factors = names(input.values), r = sim.rep, design = morris.design,
               binf = mins, bsup = maxs, scale=TRUE)

  moX <- as.tibble(cbind(as.tibble(mo$X), fixed.values, stringsAsFactors=FALSE))
  seeds <- nlrx_generate_seeds(sim.seedrep)

  # Cretae new simdesign and add to nl
  nl@experiment@simdesign <- new("simdesign",
                                 method="morris",
                                 simdata=moX,
                                 simseeds=seeds,
                                 simobject=list(mo))

  return(nl)
}


nlrx_show_simdata <- function(nl) {

  simdata <- nl@experiment@simdesign@simdata
  return(simdata)
}

