


nlrx_nl_init <- function(nl.path, model.path) {
  
  setClass("experiment", slots=list(sim.name="character",
                                    out.path="character",
                                    n.rep="numeric",
                                    metrics.tick="logical",
                                    setup.name="character",
                                    go.name="character",
                                    max.tick="numeric",
                                    metrics="character",
                                    parameters="list"))
  
 
  # Initialize a class: NL object
  setClass("nl", slots=list(nl.path="character",
                            model.path="character",
                            experiment="experiment"))
  
  
  nl <- new("nl",
            nl.path=nl.path,
            model.path=model.path)
 
  return(nl)
  
}


nlrx_nl_addExperiment <- function(nl, sim.name, out.path, n.rep, metrics.tick, setup.name, go.name, max.tick, metrics, parameters  ) {
  
 
  experiment <- new("experiment", 
                    sim.name=sim.name,
                    out.path=out.path,
                    n.rep=n.rep,
                    metrics.tick=metrics.tick,
                    setup.name=setup.name,
                    go.name=go.name,
                    max.tick=max.tick,
                    metrics=metrics,
                    parameters=parameters)
  
  nl$experiment <- experiment
  
  return(nl)
}











#' Write NetLogo BehaviorSpace Experiment File as xml
#'
#' Creates an xml file in NetLogo BehaviorSpace format, that can be used to start NetLogo simulations via command line.
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
nlrx_writeXML <- function(filename, experiment.name, n.repetitions, metrics.each.tick, setup.name, go.name, time.limit, metrics, parameters) {
  
  library(XML)   
  
  ### Create XML object:
  nlXML = newXMLDoc()
  experiments = newXMLNode("experiments", doc=nlXML)
  experiment = newXMLNode("experiment", attrs=c(name=experiment.name, repetitions=n.repetitions, runMetricsEveryStep=metrics.each.tick), parent=experiments)
  
  ## Add Setup and go:
  addChildren(experiment, newXMLNode("setup", setup.name, parent=experiment))
  addChildren(experiment, newXMLNode("go", go.name, parent=experiment))
  addChildren(experiment, newXMLNode("timeLimit", attrs=c(steps=time.limit), parent=experiment))
  
  ## Add metrics:
  for (i in metrics) {
    addChildren(experiment, newXMLNode("metric", i, parent=experiment))
  }
  
  ## Add parameters and values:
  for (i in 1:length(parameters)) {
    addChildren(experiment, newXMLNode("enumeratedValueSet", attrs=c(variable=names(parameters[i])), newXMLNode("value", attrs=c(value=parameters[[i]]))))
  }
  
  ## Use NetLogo specific prefix:
  prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE experiments SYSTEM \"behaviorspace.dtd\">"
  
  # SAVE XML TO FILE
  cat(saveXML(nlXML, prefix=prefix), file=filename)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function NLRX_callNL --> opens NetLogo and executes specified experiment

NLRX_callNL <- function(NLDir, modelDir, outDir, xmlDir, experiment.name, nThreads) {
  
  NLcall <- paste0("\"", NLDir, "\"", " --model ", "\"", modelDir, "\"", " --setup-file ", "\"", xmlDir, "\"", " --experiment ", experiment.name, " --table ", "\"", outDir, "\"", " --threads ", 1)
  system(NLcall, wait=TRUE)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function NLRX_gatherResults --> read output table from experiment

NLRX_gatherResults <- function(outDir, experiment.name) {
  
  NLtable <- read.csv(outDir, skip=6)
  return(NLtable)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function NLRX_createLHS --> creates a LHS sample

NLRX_createLHS <- function(input.values, sample.count, precision) {
 
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function NLRX_createSobol  --> create a sobol object

NLRX_createSobol <- function(input.values, sample.count, precision, sobol.order, sobol.nboot) {
  
  library(sensitivity)
  
  input.sets.1 <- NLRX_createLHS(input.values=input.values,
                                 sample.count=sample.count,
                                 precision=digit.precision)
  input.sets.2 <- NLRX_createLHS(input.values=input.values, 
                                 sample.count=sample.count,
                                 precision=digit.precision)
  
  # create instance of sobol class
  so <- sobol(model = NULL, X1 = input.sets.1, X2 = input.sets.2, order=sobol.order, nboot = sobol.nboot)
  
  return(so)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NLRX_cleanUp --> deletes all files in the selected folders

NLRX_cleanUp <- function(files) {
  
  lapply(files, function(x) unlink(x))
  
}
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function NLRX_runMulti  --> runs multiple NetLogo simulations

NLRX_runMulti <- function(seed, x, parameters, xmlDir, n.repetitions, metrics.each.tick, setup.name, go.name, time.limit, metrics, NLDir, modelDir, outDir) {
  
  ## Write XML File:
  xmlDir <- paste0(xmlDir, "NLRX", seed, "_", x, ".xml")
  outDir <- paste0(outDir, "NLRX", seed, "_", x, ".csv")
  experiment.name <- "NLRX"
  
  NLRX_writeXML(filename = xmlDir,
                experiment.name = experiment.name,
                n.repetitions = n.repetitions,
                metrics.each.tick = metrics.each.tick,
                setup.name = setup.name,
                go.name = go.name,
                time.limit = time.limit,
                metrics = metrics,
                parameters = parameters)
  
  ## Execute:
  NLRX_callNL(NLDir = NLDir,
              modelDir = modelDir,
              outDir = outDir,
              xmlDir = xmlDir,
              experiment.name = experiment.name,
              nThreads = nThreads)
  
  ## Read results
  NLresults <- NLRX_gatherResults(outDir = outDir, 
                                  experiment.name = experiment.name)
  
  
  ## Delete temporary files:
  NLRX_cleanUp(files=list(xmlDir, outDir))
  
  return(NLresults)
}

