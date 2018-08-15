#' Construct a new nl object
#'
#' @description Construct a new nl object
#'
#' @param nlversion A character string defining the NetLogo version that is used
#' @param nlpath Path to the NetLogo main directory matching the defined version
#' @param modelpath Path to the NetLogo model file (*.nlogo) that is used for simulations
#' @param jvmmem Java virtual machine memory capacity in megabytes
#' @param experiment Holds a experiment S4 class object
#' @param simdesign Holds a simdesign S4 class object
#' @param ... ...
#' @return nl S4 class object
#' @details
#'
#' nl objects are the main class objects used in the nlrx package.
#' These objects store all information that is needed to run NetLogo simulations.
#' nl objects are initialized with basic information on Netlogo and the model.
#' To run simulation experiments, a experiment needs to be added to the nl object.
#' Each experiment also needs to have a specified simulation design in order to run simulations.
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "C:/Program Files/NetLogo 6.0.3/",
#' modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/
#' Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#' }
#'
#' @import methods
#' @name nl
#' @rdname nl
#' @export
nl <- function(nlversion = "6.0.2",
               nlpath = character(),
               modelpath = character(),
               jvmmem = 1024,
               experiment=new("experiment"),
               simdesign=new("simdesign"),
               ...) {

  methods::new("nl",
               nlversion = nlversion,
               nlpath = nlpath,
               modelpath = modelpath,
               jvmmem = jvmmem,
               experiment=new("experiment"),
               simdesign=new("simdesign"),
               ...)
}



#' Construct a new experiment object
#'
#' @description Construct a new experiment object
#'
#' @param expname A character string defining the name of the experiment
#' @param outpath Path to a directory where experiment output will be stored
#' @param repetition A number which gives the number of repetitions for each row of the simulation design input tibble
#' @param tickmetrics Character string "true" runs defined metrics on each simulation tick. "false" runs metrics only after simulation is finished
#' @param idsetup character string or vector of character strings, defining the name of the NetLogo setup procedure
#' @param idgo character string or vector of character strings, defining the name of the NetLogo go procedure
#' @param idfinal character string or vector of character strings, defining the name of NetLogo procedures that should be run after the last tick
#' @param idrunnum character string, defining the name of a NetLogo global that should be used to parse the current siminputrow during model executions which can then be used for self-written output.
#' @param runtime number of model ticks that should be run for each simulation
#' @param evalticks vector of tick numbers defining when measurements are taken
#' @param metrics vector of strings defining valid NetLogo reporters that are taken as output measurements (e.g. c("count turtles", "count patches"))
#' @param variables a nested list of variables that are changed within a simulation design. The name of each sublist item has to be a valid global of the defined NetLogo model. Depending on the desired simdesign each list item consist of a vector of values, a min value, a max value, a step value and a qfun (e.g. list("paramA" = list(values=c(0, 0.5, 1), min=0, max=1, step=0.1, qfun="qunif")))
#' @param constants a list of constants that are kept constant within a simulation design. The name of each list item has to be a valid global of the defined NetLogo model (e.g. list("pNUM" = 12, "pLOGIC"="TRUE", "pSTRING"="\"default\""))
#' @param ... ...
#' @return experiment S4 class object
#' @details
#'
#' nl objects are the main class objects used in the nlrx package.
#' These objects store all information that is needed to run NetLogo simulations.
#' nl objects are initialized with basic information on Netlogo and the model (more Details, see class definition of S4 class nl).
#' To run simulation experiments, a experiment needs to be added to the nl object.
#' Each experiment also needs to have a specified simulation design in order to run simulations.
#'
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl@@experiment <- experiment(expname="wolf-sheep",
#' outpath="C:/out/",
#' repetition=1,
#' tickmetrics="true",
#' idsetup="setup",
#' idgo="go",
#' idfinal=NA_character_,
#' idrunnum=NA_character_,
#' runtime=50,
#' evalticks=seq(40,50),
#' metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
#' variables = list('initial-number-sheep' = list(min=50, max=150, step=10, qfun="qunif"),
#'                  'initial-number-wolves' = list(min=50, max=150, step=10, qfun="qunif")),
#' constants = list("model-version" = "\"sheep-wolves-grass\"",
#'                  "grass-regrowth-time" = 30,
#'                  "sheep-gain-from-food" = 4,
#'                  "wolf-gain-from-food" = 20,
#'                  "sheep-reproduce" = 4,
#'                  "wolf-reproduce" = 5,
#'                  "show-energy?" = "false"))
#'
#' }
#'
#' @import methods
#' @name experiment
#' @rdname experiment
#' @export
experiment <- function(expname = "defaultexp",
                       outpath = NA_character_,
                       repetition = 1,
                       tickmetrics = "true",
                       idsetup = "setup",
                       idgo = "go",
                       idfinal = NA_character_,
                       idrunnum = NA_character_,
                       runtime = 1,
                       evalticks = seq(1,runtime,1),
                       metrics = c("count turtles"),
                       variables = list(),
                       constants = list(),
                       ...) {

  methods::new("experiment",
               expname=expname,
               outpath=outpath,
               repetition=repetition,
               tickmetrics=tickmetrics,
               idsetup=idsetup,
               idgo=idgo,
               idfinal=idfinal,
               idrunnum=idrunnum,
               runtime=runtime,
               evalticks=evalticks,
               metrics=metrics,
               variables=variables,
               constants=constants,
               ...)


}




#' Construct a new simdesign object
#'
#' @description Construct a new experiment object
#'
#' @param simmethod character string defining the method of the simulation design
#' @param siminput tibble providing input parameterisations for the NetLogo model (cols=parameter, rows=runs)
#' @param simobject used for some methods to store additional information (sobol, morris, eFast)
#' @param simseeds a vector or model random seeds
#' @param simoutput tibble contatining model results
#' @param ... ...
#' @return simdesign S4 class object
#' @details
#'
#' The simulation design class holds information on the input parameter design of model simulations.
#' It also stores information that is needed to run method specific analysis functions.
#' The simseeds can be used to run all model simulations that are defined within the siminput tibble several times with changing random-seeds.
#' While it is possible to add simdesign directly with this function, we suggest to use our predefined simdesign functions.
#' nlrx provides a bunch of different simulation designs, such as full-factorial, latin-hypercube, sobol, morris and eFast.
#' A simulation design is attached to a nl object by using on of these simdesign functions (see examples).
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#'
#' nl@simdesign <- simdesign_simple(nl = nl,
#'                                  nseeds = 3)
#'
#' nl@simdesign <- simdesign_ff(nl = nl,
#'                              nseeds = 3)
#'
#'
#' nl@simdesign <- simdesign_lhs(nl=nl,
#'                               samples=100,
#'                               nseeds=3,
#'                               precision=3)
#'
#' nl@simdesign <- simdesign_sobol(nl=nl,
#'                                 samples=200,
#'                                 sobolorder=2,
#'                                 sobolnboot=20,
#'                                 sobolconf=0.95,
#'                                 nseeds=3,
#'                                 precision=3)
#'
#' nl@simdesign <- simdesign_sobol2007(nl=nl,
#'                                     samples=200,
#'                                     sobolnboot=20,
#'                                     sobolconf=0.95,
#'                                     nseeds=3,
#'                                     precision=3)
#'
#' nl@simdesign <- simdesign_soboljansen(nl=nl,
#'                                       samples=200,
#'                                       sobolnboot=20,
#'                                       sobolconf=0.95,
#'                                       nseeds=3,
#'                                       precision=3)
#'
#' nl@simdesign <- simdesign_morris(nl=nl,
#'                                  morristype="oat",
#'                                  morrislevels=4,
#'                                  morrisr=100,
#'                                  morrisgridjump=2,
#'                                  nseeds=3)
#'
#' nl@simdesign <- simdesign_eFast(nl=nl,
#'                                 samples=100,
#'                                 nseeds=3)
#'
#' }
#'
#' @import methods tibble
#' @name simdesign
#' @rdname simdesign
#' @export
simdesign <- function(simmethod = character(),
                      siminput = tibble(),
                      simobject = list(),
                      simseeds = NA_integer_,
                      simoutput = tibble(),
                      ...) {

  methods::new("simdesign",
               simmethod=simmethod,
               siminput=siminput,
               simobject=simobject,
               simseeds=simseeds,
               simoutput=simoutput)

}

