#' Add a simple simdesign to a nl object
#'
#' @description Add a simple simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param nseeds number of seeds for this simulation design
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The simple simdesign only uses model parameters that are defined in the constants field of the experiment object within the nl object.
#' Thus, the resulting input tibble of the simdesign has only one run with constant parameterisations.
#' This can be useful to run one simulation with a specific parameterset.
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_simple(nl = nl, nseeds = 3)
#' }
#'
#' @aliases simdesign_simple
#' @rdname simdesign_simple
#'
#' @export

simdesign_simple <- function(nl, nseeds) {

  eval_experiment(nl)
  eval_constants(nl)
  message("Creating simple simulation design")
  # This doesnt use variables but only constants to create a simdesign:
  simple <- tibble::as.tibble(constants(nl))
  seeds <- util_generate_seeds(nseeds = nseeds)
  simsimple <- methods::new("simdesign",
                            simmethod="simple",
                            siminput=simple,
                            simseeds=seeds)

  return(simsimple)

}

#' Add a full-factorial simdesign to a nl object
#'
#' @description Add a full-factorial simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param nseeds number of seeds for this simulation design
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The full-factorial simdesign uses parameter ranges that are defined in the variables field of the experiment object within the nl object.
#' First, a vector is created for each parameter, based on the min, max and step value.
#' Then, a full-factorial matrix of all parameter combinations is created as input tibble for the simdesign.
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_ff(nl = nl, nseeds = 3)
#' }
#'
#' @aliases simdesign_ff
#' @rdname simdesign_ff
#'
#' @export

simdesign_ff <- function(nl, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating full facotrial simulation design")

  # Add a full factorial simulatin design:
  # Generate vectors from variables data:
  ff <- plyr::llply(variables(nl), function(i) {
    seq(i$min, i$max, i$step)
  })

  ff <- tibble::as.tibble(expand.grid(ff))
  seeds <- util_generate_seeds(nseeds = nseeds)

  simff <- methods::new("simdesign",
                        simmethod="ff",
                        siminput=ff,
                        simseeds=seeds)

  return(simff)
}

#' Add a latin-hypercube simdesign to a nl object
#'
#' @description Add a latin-hypercube simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the latin hypercube
#' @param nseeds number of seeds for this simulation design
#' @param precision number of digits for the decimal fraction of parameter values
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The latin hypercube simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_lhs(nl=nl,
#' samples=10,
#' nseeds=3,
#' precision=3)
#' }
#'
#' @aliases simdesign_lhs
#' @rdname simdesign_lhs
#'
#' @export

simdesign_lhs <- function(nl, samples, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating latin hypercube simulation design")

  lhs <- util_create_lhs(input = variables(nl),
                         samples = samples,
                         precision = precision)

  lhs <- tibble::as.tibble(cbind(lhs, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds = nseeds)

  # Add simdesign to nl
  simlhs <- methods::new("simdesign",
                         simmethod="lhs",
                         siminput=lhs,
                         simseeds=seeds)

  return(simlhs)
}

#' Add a sobol simdesign to a nl object
#'
#' @description Add a sobol simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the sobol sensitivity analysis
#' @param sobolorder order of interactions of the sobol sensitivity analysis
#' @param sobolnboot number of bootstrap replicates of the sobol sensitivity analysis
#' @param sobolconf the confidence level for bootstrap confidence intervals
#' @param nseeds number of seeds for this simulation design
#' @param precision number of digits for the decimal fraction of parameter values
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The sobol simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' It uses the sensitivity package to set up a sobol sensitivity analysis, including a simobject of class sobol and a input tibble for simulations.
#' For details on method specific sensitivity analysis function parameters see ?sobol
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_sobol(nl=nl,
#' samples=100,
#' sobolorder=2,
#' sobolnboot=10,
#' sobolconf=0.95,
#' nseeds=2,
#' precision=3)
#' }
#'
#' @aliases simdesign_sobol
#' @rdname simdesign_sobol
#'
#' @export

simdesign_sobol <- function(nl, samples, sobolorder, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating sobol simulation design")

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::sobol(model = NULL, X1 = lhs_1, X2 = lhs_2, order=sobolorder, nboot = sobolnboot, conf=sobolconf)
  soX <- tibble::as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsobol <- methods::new("simdesign",
                           simmethod="sobol",
                           siminput=soX,
                           simobject=list(so),
                           simseeds=seeds)

  return(simsobol)
}

#' Add a sobol2007 simdesign to a nl object
#'
#' @description Add a sobol2007 simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the sobol sensitivity analysis
#' @param sobolnboot number of bootstrap replicates of the sobol sensitivity analysis
#' @param sobolconf the confidence level for bootstrap confidence intervals
#' @param nseeds number of seeds for this simulation design
#' @param precision number of digits for the decimal fraction of parameter values
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The sobol2007 simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' It uses the sensitivity package to set up a sobol2007 sensitivity analysis, including a simobject of class sobol and a input tibble for simulations.
#' For details on method specific sensitivity analysis function parameters see ?sobol2007
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_sobol2007(nl=nl,
#' samples=100,
#' sobolnboot=10,
#' sobolconf=0.95,
#' nseeds=2,
#' precision=3)
#' }
#'
#' @aliases simdesign_sobol2007
#' @rdname simdesign_sobol2007
#'
#' @export

simdesign_sobol2007 <- function(nl, samples, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating sobol2007 simulation design")

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::sobol2007(model = NULL, X1 = lhs_1, X2 = lhs_2, nboot = sobolnboot, conf=sobolconf)
  soX <- tibble::as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsobol2007 <- methods::new("simdesign",
                               simmethod="sobol2007",
                               siminput=soX,
                               simobject=list(so),
                               simseeds=seeds)

  return(simsobol2007)
}


#' Add a soboljansen simdesign to a nl object
#'
#' @description Add a soboljansen simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the sobol sensitivity analysis
#' @param sobolnboot number of bootstrap replicates of the sobol sensitivity analysis
#' @param sobolconf the confidence level for bootstrap confidence intervals
#' @param nseeds number of seeds for this simulation design
#' @param precision number of digits for the decimal fraction of parameter values
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The soboljansen simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' It uses the sensitivity package to set up a soboljansen sensitivity analysis, including a simobject of class sobol and a input tibble for simulations.
#' For details on method specific sensitivity analysis function parameters see ?soboljansen
#' Finally, the function reports a simdesign object.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_soboljansen(nl=nl,
#' samples=100,
#' sobolnboot=10,
#' sobolconf=0.95,
#' nseeds=2,
#' precision=3)
#' }
#'
#' @aliases simdesign_sobol2007
#' @rdname simdesign_sobol2007
#'
#' @export

simdesign_soboljansen <- function(nl, samples, sobolnboot, sobolconf, nseeds, precision) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating soboljansen simulation design")

  lhs_1 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = variables(nl),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::soboljansen(model = NULL, X1 = lhs_1, X2 = lhs_2, nboot = sobolnboot, conf=sobolconf)
  soX <- tibble::as.tibble(cbind(so$X, constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  simsoboljansen <- methods::new("simdesign",
                                 simmethod="soboljansen",
                                 siminput=soX,
                                 simobject=list(so),
                                 simseeds=seeds)

  return(simsoboljansen)
}


#' Add a morris elementary effects simdesign to a nl object
#'
#' @description Add a morris elementary effects simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param morristype morris design type
#' @param morrislevels number of parameter levels
#' @param morrisr morris r value
#' @param morrisgridjump morris grid jump value
#' @param nseeds number of seeds for this simulation design
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The morris simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' It uses the sensitivity package to set up a morris elementary effects sensitivity analysis, including a simobject of class morris and a input tibble for simulations.
#' For details on method specific sensitivity analysis function parameters see ?morris
#' Finally, the function reports a simdesign object.
#'
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_morris(nl=nl,
#'                                   morristype="oat",
#'                                   morrislevels=4,
#'                                   morrisr=3,
#'                                   morrisgridjump=2,
#'                                   nseeds=2)
#' }
#'
#' @aliases simdesign_morris
#' @rdname simdesign_morris
#'
#' @export

simdesign_morris <- function(nl, morristype, morrislevels, morrisr, morrisgridjump, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating morris simulation design")

  morrisdesign <- list(type = morristype, levels = morrislevels, grid.jump = morrisgridjump)

  # get the min and max values of the input factor ranges
  mins <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$min})
  maxs <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$max})

  # create input sets
  mo <- sensitivity::morris(model = NULL, factors = names(variables(nl)), r = morrisr, design = morrisdesign,
               binf = mins, bsup = maxs, scale=TRUE)

  moX <- tibble::as.tibble(cbind(as.tibble(mo$X), constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  simmorris <- methods::new("simdesign",
                            simmethod="morris",
                            siminput=moX,
                            simobject=list(mo),
                            simseeds=seeds)

  return(simmorris)

}


#' Add an eFast simdesign to a nl object
#'
#' @description Add an eFast simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the eFast sensitivity analysis
#' @param nseeds number of seeds for this simulation design
#'
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The eFast simdesign uses parameter ranges and q-functions that are defined in the variables field of the experiment object within the nl object.
#' It uses the sensitivity package to set up a fast99 elementary effects sensitivity analysis, including a simobject of class fast99 and a input tibble for simulations.
#' For details on method specific sensitivity analysis function parameters see ?fast99
#' Finally, the function reports a simdesign object.
#'
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' simdesign(nl) <- simdesign_eFast(nl=nl,
#'                                  samples=70,
#'                                  nseeds=1)
#' }
#'
#' @aliases simdesign_eFast
#' @rdname simdesign_eFast
#'
#' @export

simdesign_eFast <- function(nl, samples, nseeds) {

  eval_experiment(nl)
  eval_variables(nl)
  message("Creating eFast simulation design")

  # get names of quantile functions fpr the input factors
  q.functions <- sapply(seq(1,length(variables(nl))), function(i) {
    variables(nl)[[i]]$qfun})

  # generate a list of arguments for the quantile functions
  q.args <- lapply(variables(nl), function(i) {
    i$qfun <- NULL
    i$step <- NULL; return(i)})

  # create instance of fast99 class
  f99 <- sensitivity::fast99(model = NULL, factors = names(variables(nl)), n = samples, q = q.functions, q.arg = q.args)

  f99X <- tibble::as.tibble(cbind(as.tibble(f99$X), constants(nl), stringsAsFactors=FALSE))
  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  simefast <- methods::new("simdesign",
                           simmethod="eFast",
                           siminput=f99X,
                           simobject=list(f99),
                           simseeds=seeds)

  return(simefast)

}

