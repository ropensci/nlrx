

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

  lhs <- nlrx_create_lhs(input.values = input.values,
                         sample.count = sim.sample,
                         precision = sim.digits)

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

  input.sets.1 <- nlrx_create_lhs(input.values=input.values,
                                 sample.count=sim.sample,
                                 precision=sim.digits)
  input.sets.2 <- nlrx_create_lhs(input.values=input.values,
                                 sample.count=sim.sample,
                                 precision=sim.digits)

  # create instance of sobol class
  so <- sobol(model = NULL, X1 = input.sets.1, X2 = input.sets.2, order=sim.order, nboot = sim.nboot)

  seeds <- nlrx_generate_seeds(sim.seedrep)

  # Cretae new simdesign and add to nl
  nl@experiment@simdesign <- new("simdesign",
                                 method="sobol",
                                 simdata=so$X,
                                 simseeds=seeds,
                                 simobject=list(so))

  return(nl)
}

nlrx_show_simdata <- function(nl) {

  simdata <- nl@experiment@simdesign@simdata
  return(simdata)
}

