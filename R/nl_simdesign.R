

eval_simdesign <- function(nl) {

  library(purrr)
  # A valid experiment needs at least
  notvalid <- c()

  if(is.na(simmethod(nl))) {
    notvalid <- c(notvalid, "simmethod")
  }
  if(is_empty(siminput(nl))) {
    notvalid <- c(notvalid, "siminput")
  }
  if(is.na(simseeds(nl))) {
    notvalid <- c(notvalid, "simseeds")
  }


  stop(paste0("Error: To run a simulation you have to add a sim design to a nl object with a properly defined experiment. Please first initialize a nl object, then add a proper experiment and finally add a simdesign by using one of the provided simdesign functions. The following elements are missing without default: ", paste(notvalid, collapse=" ; ")))

}


simdesign_simple <- function(nl, nseeds) {

  eval_experiment(nl)
  eval_constants(nl)
  message("Creating simple simulation design")
  # This doesnt use variables but only constants to create a simdesign:
  simple <- as.tibble(constants(nl))
  seeds <- util_generate_seeds(nseeds = nseeds)
  simsimple <- new("simdesign",
                   simmethod="simple",
                   siminput=simple,
                   simseeds=seeds)

  return(simsimple)

}


simdesign_ff <- function(nl, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating full facotrial simulation design")
  library(plyr)
  # Add a full factorial simulatin design:
  # Generate vectors from variables data:
  ff <- llply(variables(nl), function(i) {
    seq(i$min, i$max, i$step)
  })

  ff <- as.tibble(expand.grid(ff))
  seeds <- util_generate_seeds(nseeds = nseeds)

  simff <- new("simdesign",
               simmethod="ff",
               siminput=ff,
               simseeds=seeds)

  return(simff)
}



# Create and attach a lhs simulation Design
simdesign_lhs <- function(nl, samples, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating latin hypercube simulation design")

  lhs <- util_create_lhs(input = variables(nl),
                         samples = samples,
                         precision = precision)

  lhs <- as.tibble(cbind(lhs, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds = nseeds)

  # Add simdesign to nl
  simlhs <- new("simdesign",
                simmethod="lhs",
                siminput=lhs,
                simseeds=seeds)

  return(simlhs)
}

simdesign_sobol <- function(nl, samples, sobolorder, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating sobol simulation design")

  library(sensitivity)
  library(tibble)

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sobol(model = NULL, X1 = lhs_1, X2 = lhs_2, order=sobolorder, nboot = sobolnboot, conf=sobolconf)
  soX <- as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsobol <- new("simdesign",
                simmethod="sobol",
                siminput=soX,
                simobject=list(so),
                simseeds=seeds)

  return(simsobol)
}

simdesign_sobol2007 <- function(nl, samples, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating sobol2007 simulation design")

  library(sensitivity)
  library(tibble)

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sobol2007(model = NULL, X1 = lhs_1, X2 = lhs_2, nboot = sobolnboot, conf=sobolconf)
  soX <- as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsobol2007 <- new("simdesign",
                simmethod="sobol2007",
                siminput=soX,
                simobject=list(so),
                simseeds=seeds)

  return(simsobol2007)
}

simdesign_soboljansen <- function(nl, samples, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating soboljansen simulation design")

  library(sensitivity)
  library(tibble)

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- soboljansen(model = NULL, X1 = lhs_1, X2 = lhs_2, nboot = sobolnboot, conf=sobolconf)
  soX <- as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsoboljansen <- new("simdesign",
                simmethod="soboljansen",
                siminput=soX,
                simobject=list(so),
                simseeds=seeds)

  return(simsoboljansen)
}


simdesign_morris <- function(nl, morristype, morrislevels, morrisr, morrisgridjump, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating morris simulation design")

  library(sensitivity)
  library(tibble)

  morrisdesign <- list(type = morristype, levels = morrislevels, grid.jump = morrisgridjump)

  # get the min and max values of the input factor ranges
  mins <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$min})
  maxs <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$max})

  # create input sets
  mo <- morris(model = NULL, factors = names(variables(nl)), r = morrisr, design = morrisdesign,
               binf = mins, bsup = maxs, scale=TRUE)

  moX <- as.tibble(cbind(as.tibble(mo$X), constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  simmorris <- new("simdesign",
                simmethod="morris",
                siminput=moX,
                simobject=list(mo),
                simseeds=seeds)

  return(simmorris)

}



simdesign_eFast <- function(nl, samples, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating eFast simulation design")

  library(sensitivity)
  library(tibble)

  # get names of quantile functions fpr the input factors
  q.functions <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$qfun})

  # generate a list of arguments for the quantile functions
  q.args <- lapply(variables(nl), function(i) {
    i$qfun <- NULL
    i$step <- NULL; return(i)})

  # create instance of fast99 class
  f99 <- fast99(model = NULL, factors = names(variables(nl)), n = samples, q = q.functions, q.arg = q.args)

  f99X <- as.tibble(cbind(as.tibble(f99$X), constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  simefast <- new("simdesign",
                simmethod="eFast",
                siminput=f99X,
                simobject=list(f99),
                simseeds=seeds)

  return(simefast)

}

