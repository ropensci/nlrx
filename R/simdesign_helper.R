
#' Add a simple simdesign to a nl object
#'
#' @description Add a simple simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param nseeds number of seeds for this simulation design
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_simple(nl = nl, nseeds = 3)
#' }
#'
#' @aliases simdesign_simple
#' @rdname simdesign_simple
#'
#' @export

simdesign_simple <- function(nl, nseeds) {

  util_eval_experiment(nl)
  util_eval_constants(nl)
  message("Creating simple simulation design")
  # This doesnt use variables but only constants to create a simdesign:
  simple <- tibble::as.tibble(getexp(nl, "constants"))
  seeds <- util_generate_seeds(nseeds = nseeds)

  new_simdesign <- simdesign(simmethod="simple",
                             siminput=simple,
                             simseeds=seeds)

  return(new_simdesign)

}

#' Add a full-factorial simdesign to a nl object
#'
#' @description Add a full-factorial simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param nseeds number of seeds for this simulation design
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_ff(nl = nl, nseeds = 3)
#' }
#'
#' @aliases simdesign_ff
#' @rdname simdesign_ff
#'
#' @export

simdesign_ff <- function(nl, nseeds) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating full facotrial simulation design")

  # Add a full factorial simulatin design:
  # Generate vectors from variables data:
  ff <- plyr::llply(getexp(nl, "variables"), function(i) {
    seq(i$min, i$max, i$step)
  })

  ff <- tibble::as.tibble(expand.grid(ff))
  seeds <- util_generate_seeds(nseeds = nseeds)

  new_simdesign <- simdesign(simmethod="ff",
                             siminput=ff,
                             simseeds=seeds)

  return(new_simdesign)
}

#' Add a latin-hypercube simdesign to a nl object
#'
#' @description Add a latin-hypercube simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the latin hypercube
#' @param nseeds number of seeds for this simulation design
#' @param precision number of digits for the decimal fraction of parameter values
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_lhs(nl=nl,
#' samples=100,
#' nseeds=3,
#' precision=3)
#' }
#'
#' @aliases simdesign_lhs
#' @rdname simdesign_lhs
#'
#' @export

simdesign_lhs <- function(nl, samples, nseeds, precision) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating latin hypercube simulation design")

  lhs <- util_create_lhs(input = getexp(nl, "variables"),
                         samples = samples,
                         precision = precision)

  lhs <- tibble::as.tibble(cbind(lhs,
                                 getexp(nl, "constants"),
                                 stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds = nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="lhs",
                             siminput=lhs,
                             simseeds=seeds)

  return(new_simdesign)
}

#' Add a sobol simdesign to a nl object
#'
#' @description Add a sobol simdesign to a nl object
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_sobol(nl=nl,
#' samples=1000,
#' sobolorder=2,
#' sobolnboot=100,
#' sobolconf=0.95,
#' nseeds=3,
#' precision=3)
#' }
#'
#' @aliases simdesign_sobol
#' @rdname simdesign_sobol
#'
#' @export

simdesign_sobol <- function(nl,
                            samples,
                            sobolorder,
                            sobolnboot,
                            sobolconf,
                            nseeds,
                            precision) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating sobol simulation design")

  lhs_1 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::sobol(model = NULL,
                           X1 = lhs_1,
                           X2 = lhs_2,
                           order=sobolorder,
                           nboot = sobolnboot,
                           conf=sobolconf)

  soX <- tibble::as.tibble(cbind(so$X,
                                 getexp(nl, "constants"),
                                 stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="sobol",
                             siminput=soX,
                             simobject=list(so),
                             simseeds=seeds)

  return(new_simdesign)
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
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_sobol2007(nl=nl,
#' samples=1000,
#' sobolnboot=100,
#' sobolconf=0.95,
#' nseeds=3,
#' precision=3)
#' }
#'
#' @aliases simdesign_sobol2007
#' @rdname simdesign_sobol2007
#'
#' @export

simdesign_sobol2007 <- function(nl,
                                samples,
                                sobolnboot,
                                sobolconf,
                                nseeds,
                                precision) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating sobol2007 simulation design")

  lhs_1 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::sobol2007(model = NULL,
                               X1 = lhs_1,
                               X2 = lhs_2,
                               nboot = sobolnboot,
                               conf=sobolconf)

  soX <- tibble::as.tibble(cbind(so$X,
                                 getexp(nl, "constants"),
                                 stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="sobol2007",
                             siminput=soX,
                             simobject=list(so),
                             simseeds=seeds)

  return(new_simdesign)
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
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_soboljansen(nl=nl,
#' samples=1000,
#' sobolnboot=100,
#' sobolconf=0.95,
#' nseeds=3,
#' precision=3)
#' }
#'
#' @aliases simdesign_soboljansen
#' @rdname simdesign_soboljansen
#'
#' @export

simdesign_soboljansen <- function(nl,
                                  samples,
                                  sobolnboot,
                                  sobolconf,
                                  nseeds,
                                  precision) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating soboljansen simulation design")

  lhs_1 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)
  lhs_2 <- util_create_lhs(input = getexp(nl, "variables"),
                           samples = samples,
                           precision = precision)

  # create instance of sobol class
  so <- sensitivity::soboljansen(model = NULL,
                                 X1 = lhs_1,
                                 X2 = lhs_2,
                                 nboot = sobolnboot,
                                 conf=sobolconf)

  soX <- tibble::as.tibble(cbind(so$X,
                                 getexp(nl, "constants"),
                                 stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds=nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="soboljansen",
                             siminput=soX,
                             simobject=list(so),
                             simseeds=seeds)

  return(new_simdesign)
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
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_morris(nl=nl,
#'                                   morristype="oat",
#'                                   morrislevels=4,
#'                                   morrisr=20,
#'                                   morrisgridjump=2,
#'                                   nseeds=3)
#' }
#'
#' @aliases simdesign_morris
#' @rdname simdesign_morris
#'
#' @export

simdesign_morris <- function(nl,
                             morristype,
                             morrislevels,
                             morrisr,
                             morrisgridjump,
                             nseeds) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating morris simulation design")

  morrisdesign <- list(type = morristype,
                       levels = morrislevels,
                       grid.jump = morrisgridjump)

  # get the min and max values of the input factor ranges
  mins <- unlist(lapply(getexp(nl, "variables"), "[", "min"))
  maxs <- unlist(lapply(getexp(nl, "variables"), "[", "max"))

  # create input sets
  mo <- sensitivity::morris(model = NULL,
                            factors = names(getexp(nl, "variables")),
                            r = morrisr,
                            design = morrisdesign,
                            binf = mins,
                            bsup = maxs,
                            scale=TRUE)

  moX <- tibble::as.tibble(cbind(as.tibble(mo$X),
                                 getexp(nl, "constants"),
                                 stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="morris",
                             siminput=moX,
                             simobject=list(mo),
                             simseeds=seeds)

  return(new_simdesign)

}


#' Add an eFast simdesign to a nl object
#'
#' @description Add an eFast simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param samples number of samples for the eFast sensitivity analysis
#' @param nseeds number of seeds for this simulation design
#' @return simdesign S4 class object
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
#' nl@@simdesign <- simdesign_eFast(nl=nl,
#'                                  samples=100,
#'                                  nseeds=1)
#' }
#'
#' @aliases simdesign_eFast
#' @rdname simdesign_eFast
#'
#' @export

simdesign_eFast <- function(nl,
                            samples,
                            nseeds) {

  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating eFast simulation design")

  # get names of quantile functions fpr the input factors
  q.functions <- unlist(lapply(getexp(nl, "variables"), "[", "qfun"))

  # generate a list of arguments for the quantile functions
  q.args <- lapply(getexp(nl, "variables"), function(i) {
    i$qfun <- NULL
    i$step <- NULL; return(i)})

  # create instance of fast99 class
  f99 <- sensitivity::fast99(model = NULL,
                             factors = names(getexp(nl, "variables")),
                             n = samples,
                             q = q.functions,
                             q.arg = q.args)

  f99X <- tibble::as.tibble(cbind(as.tibble(f99$X),
                                  getexp(nl, "constants"),
                                  stringsAsFactors=FALSE))

  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="eFast",
                             siminput=f99X,
                             simobject=list(f99),
                             simseeds=seeds)

  return(new_simdesign)

}




#' Add a Simulated Annealing simdesign to a nl object
#'
#' @description Add a Simulated Annealing simdesign to a nl object
#'
#' @param nl nl object with a defined experiment
#' @param par optional vector of start values for each parameter defined in variables of experiment
#' @param evalcrit position of evaluation criterion within defined NetLogo metrics of nl experiment
#' @param control list with further arguments passed to the GenSA function (see ?GenSA for details)
#' @param nseeds number of seeds for this simulation design
#' @return simdesign S4 class object
#' @details
#'
#' This function creates a simdesign S4 class which can be added to a nl object by using the setter function simdesign(nl).
#' The GenSA simdesign generates a simulated Annealing experiment within the defined min and max parameter boundaries
#' that are defined in the variables field of the experiment object within the nl object.
#' The evalcrit reporter defines the evaluation criterion for the simulated annealing procedure.
#' The reporter is defined within the experiment metrics vector.
#' For the simulated annealing function we only refer to the position of the reporter that we want to use for evaluation.
#' The function uses the GenSA package to set up a Simulated Annealing function.
#' For details on the GenSA function parameters see ?GenSA
#' Finally, the function reports a simdesign object.
#'
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl@@simdesign <- simdesign_GenSA(nl=nl,
#'                                  par=NULL,
#'                                  evalcrit=1,
#'                                  control=list(max.time = 600),
#'                                  nseeds=1)
#' }
#'
#' @aliases simdesign_GenSA
#' @rdname simdesign_GenSA
#'
#' @export

simdesign_GenSA <- function(nl,
                            par = NULL,
                            evalcrit = 1,
                            control = list(),
                            nseeds = 1) {

  # Evaluate experiment and variables:
  util_eval_experiment(nl)
  util_eval_variables(nl)
  message("Creating GenSA simulation design")

  # Parameters we need for simulated annealing:
  lower <- unlist(lapply(getexp(nl, "variables"), "[", "min"))
  upper <- unlist(lapply(getexp(nl, "variables"), "[", "max"))

  # Get evaulation criterion reporter from metrics vector:
  evalcrit_reporter <- getexp(nl, "metrics")[evalcrit]
  # Check if the reporter exists:
  if (is.na(evalcrit_reporter)) {
    stop(paste0("Error: No valid reporter at defined evalcrit position: ", evalcrit))
  }

  # Create a gsa object:
  gsa <- list(par = par,
              upper = upper,
              lower = lower,
              evalcrit = evalcrit_reporter,
              control = control)

  # generate random seeds
  seeds <- util_generate_seeds(nseeds)

  # Add simdesign to nl
  new_simdesign <- simdesign(simmethod="GenSA",
                             siminput=tibble::tibble(),
                             simobject=gsa,
                             simseeds=seeds)

  return(new_simdesign)

}

