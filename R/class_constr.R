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
#'
#' After setting up the nl object, an experiment needs to be attached \link[nlrx]{experiment}.
#' The experiment class stores all information related to the NetLogo simulation experiment, such as runtime,
#' variables, constants, measurements, and more.
#'
#' After attaching an experiment, different simdesign helper functions can be used to attach a simdesign to the nl object \link[nlrx]{simdesign}.
#' The simdesign helper functions use the variable definitions from the experiment within the nl object to generate a parameter tibble for simulations.
#'
#'
#'
#' @examples
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' # Windows default NetLogo installation path (adjust to your needs!):
#' netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
#' modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
#' outpath <- file.path("C:/out")
#' # Unix default NetLogo installation path (adjust to your needs!):
#' netlogopath <- file.path("/home/NetLogo 6.0.3")
#' modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
#' outpath <- file.path("/home/out")
#'
#' nl <- nl(nlversion = "6.0.3",
#'          nlpath = netlogopath,
#'          modelpath = modelpath,
#'          jvmmem = 1024)
#'
#' @name nl
#' @rdname nl
#' @export
nl <- function(nlversion = "6.0.2",
               nlpath = character(),
               modelpath = character(),
               jvmmem = 1024,
               experiment = methods::new("experiment"),
               simdesign = methods::new("simdesign"),
               ...) {

  methods::new("nl",
               nlversion = nlversion,
               nlpath = nlpath,
               modelpath = modelpath,
               jvmmem = jvmmem,
               experiment = methods::new("experiment"),
               simdesign = methods::new("simdesign"),
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
#' @param stopcond a NetLogo reporter that reports TRUE/FALSE. If it reports TRUE the current simulation run is stopped (e.g. "not any? turtles")
#' @param metrics vector of strings defining valid NetLogo reporters that are taken as output measurements (e.g. c("count turtles", "count patches"))
#' @param metrics.turtles vector of strings defining valid turtles-own variables that are taken as output measurements (e.g. c("who", "pxcor", "pycor", "color"))
#' @param metrics.patches vector of strings defining valid patches-own variables that are taken as output measurements (e.g. c("pxcor", "pycor", "pcolor"))
#' @param metrics.links vector of strings defining valid links-own variables that are taken as output measurements (e.g. c("end1", "end2"))
#' @param variables a nested list of variables that are changed within a simulation design. The name of each sublist item has to be a valid global of the defined NetLogo model. Depending on the desired simdesign each list item consist of a vector of values, a min value, a max value, a step value and a qfun (e.g. list("paramA" = list(values=c(0, 0.5, 1), min=0, max=1, step=0.1, qfun="qunif")))
#' @param constants a list of constants that are kept constant within a simulation design. The name of each list item has to be a valid global of the defined NetLogo model (e.g. list("pNUM" = 12, "pLOGIC"="TRUE", "pSTRING"="\"default\""))
#' @param ... ...
#' @return experiment S4 class object
#' @details
#'
#' The experiment class stores all information related to the NetLogo simulation experiment.
#' The class holds all information that is typically entered into NetLogo Behavior Space experiments.
#' When setting up an experiment, it is usually attached to an already defined \link[nlrx]{nl} object (see examples).
#' After attaching an experiment, different simdesign helper functions can be used to attach a simdesign to the nl object \link[nlrx]{simdesign}.
#' The simdesign helper functions use the variable definitions from the experiment within the nl object to generate a parameter tibble for simulations.
#'
#' \strong{The following class slots are obligatory to run an experiment:}
#'
#' \emph{repetition}
#'
#' In cases, where the random seed is controlled by nlrx simdesigns, repitition should be set to one as random seeds would not differ between simulations.
#' In cases, where the random seed is set within the NetLogo model, repitition can be increased to repeat the same parameterisation with different random seeds.
#'
#' \emph{tickmetrics}
#'
#' If "true", the defined output reporters are collected on each simulation tick that is defined in evalticks. If "false" measurements are taken only on the last tick.
#'
#' \emph{idsetup, idgo}
#'
#' These two class slots accept strings, or vectors of strings, defining NetLogo model procedures that should be executed for model setup (idestup) and model execution (idgo).
#'
#' \emph{runtime}
#'
#' Defines the maximum number of simulation ticks that are executed.
#'
#'
#'
#' \strong{Depending on the simdesign, the following slots may be obligatory:}
#'
#' \emph{metrics}
#'
#' A vector of valid netlogo reporters that defines which measurements are taken.
#' The optimization simdesigns need at least one defined metrics reporter for fitness calculation of the optimization algorithm.
#'
#' \emph{constants, variables}
#'
#' These slots accept lists with NetLogo parameters that should be varied within a simdesign (variables) or should be kept constant (constants) for each simulation.
#' Any NetLogo parameter that is not entered in at least one of these two lists will be set up as constant with the default value from the NetLogo interface.
#' It is not possible to enter a NetLogo parameter in both lists (a warning message will appear when a simdesign is attached to such an experiment).
#' All simdesigns except \link[nlrx]{simdesign_simple} need defined variables for setting up a parameter matrix.
#' Variables can be defined as distinct values, value distributions or range with increment.
#' The information that is needed, depends on the chosen simdesign (details on variable definition requiements can be found in the helpfiles of each simdesign helper function).
#'
#' \strong{All remaining slots are optional:}
#'
#' \emph{expname}
#'
#' A character string defining the name of the experiment, useful for documentation purposes.
#'
#' \emph{outpath}
#'
#' A valid path to an existing directory. The directory is used by the \link[nlrx]{write_simoutput} function to store attached simulation results to disk in csv format.
#'
#' \emph{idfinal}
#'
#' A character string or vector of strings defining NetLogo procedures that are executed at the end of each simulation (e.g. cleanup or self-written output procedures).
#'
#' \emph{idrunnum}
#'
#' This slot can be used to transfer the current nlrx experiment name, random seed and runnumber (siminputrow) to NetLogo.
#' To use this functionality, a string input field widget needs to be created on the GUI of your NetLogo model.
#' The name of this widget can be entered into the "idrunnum" field of the experiment.
#' During simulations, the value of this widget is automatically updated with a generated string that contains the current nlrx experiment name, random seed and siminputrow ("expname_seed_siminputrow").
#' For self-written output In NetLogo, we suggest to include this global variable which allows referencing the self-written output files to the collected output of the nlrx simulations in R.
#'
#' \emph{evalticks}
#'
#' A vector of integers, defining the ticks for which the defined metrics will be measured.
#'
#' \emph{stopcond}
#'
#' The stopcond slot can be used to define a stop condition by providing a string with valid NetLogo code that reports either true or false.
#' Each simulation will be stopped automatically, once the reporter reports true.
#'
#' \emph{metrics.turtles, metrics.patches}
#'
#' These two slots can be used to enter turtles-own (metrics.turtles) and patches-own (metrics.patches) variables of the NetLogo model.
#' These agent variables are measured in addition to the defined metrics.
#' After attaching the simulation results to the nl object, the measured output from these agent variables needs to be postprocessed by \link[nlrx]{get_nl_spatial}.
#' Please note that NetLogo models may contain a huge number of patches and turtles and output measurements of agent variables on each tick may need a lot of ressources.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl@@experiment <- experiment(expname="wolf-sheep",
#'                              outpath="C:/out/",
#'                              repetition=1,
#'                              tickmetrics="true",
#'                              idsetup="setup",
#'                              idgo="go",
#'                              idfinal=NA_character_,
#'                              idrunnum=NA_character_,
#'                              runtime=50,
#'                              evalticks=seq(40,50),
#'                              stopcond="not any? turtles",
#'                              metrics=c("count sheep",
#'                                        "count wolves",
#'                                      "count patches with [pcolor = green]"),
#'                              metrics.turtles=c("who",
#'                                                "pxcor",
#'                                                "pycor",
#'                                                "color"),
#'                              metrics.patches=c("pxcor", "pycor", "pcolor"),
#'                              metrics.links=c("end1","end2"),
#'                              variables = list('initial-number-sheep' =
#'                              list(min=50, max=150, step=10, qfun="qunif"),
#'                                               'initial-number-wolves' =
#'                              list(min=50, max=150, step=10, qfun="qunif")),
#'                              constants = list("model-version" =
#'                                               "\"sheep-wolves-grass\"",
#'                                               "grass-regrowth-time" = 30,
#'                                               "sheep-gain-from-food" = 4,
#'                                               "wolf-gain-from-food" = 20,
#'                                               "sheep-reproduce" = 4,
#'                                               "wolf-reproduce" = 5,
#'                                               "show-energy?" = "false"))
#'
#' }
#'
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
                       stopcond= NA_character_,
                       metrics = c("count turtles"),
                       metrics.turtles = NA_character_,
                       metrics.patches = NA_character_,
                       metrics.links = NA_character_,
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
               stopcond=stopcond,
               metrics=metrics,
               metrics.turtles=metrics.turtles,
               metrics.patches=metrics.patches,
               metrics.links=metrics.links,
               variables=variables,
               constants=constants,
               ...)


}




#' Construct a new simdesign object
#'
#' @description Construct a new simdesign object
#'
#' @param simmethod character string defining the method of the simulation design
#' @param siminput tibble providing input parameterisations for the NetLogo model (cols=parameter, rows=runs)
#' @param simobject used for some methods to store additional information (sobol, morris, eFast)
#' @param simseeds a vector or model random seeds
#' @param simoutput tibble containing model results
#' @param ... ...
#' @return simdesign S4 class object
#' @details
#'
#' The simulation design class holds information on the input parameter design of model simulations.
#' It also stores information that is needed to run method specific analysis functions.
#' The simseeds can be used to run all model simulations that are defined within the siminput tibble several times with changing random-seeds.
#' While it is possible to add simdesign directly with this function, we suggest to use our simdesign_helper functions.
#' A simulation design can be attached to a nl object by using one of these simdesign_helper functions on an already defined \link[nlrx]{nl}
#' object with a valid \link[nlrx]{experiment}.
#' All simdesign helpers use the defined constants and variables of the experiment to create the siminput tibble.
#' NetLogo parameters that are not defined in constants or variables will be set with their default value from the NetLogo interface.
#'
#' Currently, following simdesign_helper functions are provided:
#'
#' \link[nlrx]{simdesign_simple}
#'
#' The simple simdesign only uses defined constants and reports a parameter matrix with only one parameterization.
#' To setup a simple simdesign, no variables have to be defined.
#'
#' \link[nlrx]{simdesign_distinct}
#'
#' The distinct simdesign can be used to run distinct parameter combinations.
#' To setup a distinct simdesign, vectors of values need to be defined for each variable.
#' These vectors must have the same number of elements across all variables.
#' The first simulation run consist of all 1st elements of these variable vectors; the second run uses all 2nd values, and so on.
#'
#' \link[nlrx]{simdesign_ff}
#'
#' The full factorial simdesign creates a full-factorial parameter matrix with all possible combinations of parameter values.
#' To setup a full-factorial simdesign, vectors of values need to be defined for each variable.
#' Alternatively, a sequence can be defined by setting min, max and step.
#' However, if both (values and min, max, step) are defined, the values vector is prioritized.
#'
#' \link[nlrx]{simdesign_lhs}
#'
#' The latin hypercube simdesign creates a Latin Hypercube sampling parameter matrix.
#' The method can be used to generate a near-random sample of parameter values from the defined parameter distributions.
#' More Details on Latin Hypercube Sampling can be found in [McKay 1979](https://www.jstor.org/stable/1268522?origin=crossref&seq=1#metadata_info_tab_contents/).
#' nlrx uses the [lhs](https://CRAN.R-project.org/package=lhs/index.html) package to generate the Latin Hypercube parameter matrix.
#' To setup a latin hypercube sampling simdesign, variable distributions need to be defined (min, max, qfun).
#'
#' Sensitivity Analyses: \link[nlrx]{simdesign_sobol}, \link[nlrx]{simdesign_sobol2007}, \link[nlrx]{simdesign_soboljansen}, \link[nlrx]{simdesign_morris}, \link[nlrx]{simdesign_eFast}
#'
#' Sensitivity analyses are useful to estimate the importance of model parameters and to scan the parameter space in an efficient way.
#' nlrx uses the [sensitivity](https://CRAN.R-project.org/package=sensitivity/index.html) package to setup sensitivity analysis parameter matrices.
#' All supported sensitivity analysis simdesigns can be used to calculate sensitivity indices for each parameter-output combination.
#' These indices can be calculated by using the \link[nlrx]{analyze_nl} function after attaching the simulation results to the nl object.
#' To setup sensitivity analysis simdesigns, variable distributions (min, max, qfun) need to be defined.
#'
#' Optimization techniques: \link[nlrx]{simdesign_GenSA}, \link[nlrx]{simdesign_GenAlg}
#'
#' Optimization techniques are a powerful tool to search the parameter space for specific solutions.
#' Both approaches try to minimize a specified model output reporter by systematically (genetic algorithm, utilizing the [genalg](https://CRAN.R-project.org/package=genalg/index.html) package) or randomly (simulated annealing, utilizing the [genSA](https://CRAN.R-project.org/package=GenSA/index.html) package) changing the model parameters within the allowed ranges.
#' To setup optimization simdesigns, variable ranges (min, max) need to be defined.
#' Optimization simdesigns can only be executed using the \link[nlrx]{run_nl_dyn} function instead of \link[nlrx]{run_nl_all} or \link[nlrx]{run_nl_one}.
#'
#'
#' @examples
#' \dontrun{
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#'
#' nl@simdesign <- simdesign_simple(nl = nl,
#'                                  nseeds = 3)
#'
#' nl@simdesign <- simdesign_distinct(nl=nl,
#'                                    nseeds=3)
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
#' nl@simdesign <- simdesign_GenAlg(nl=nl,
#'                                  popSize = 200,
#'                                  iters = 100,
#'                                  evalcrit = 1,
#'                                  elitism = NA,
#'                                  mutationChance = NA,
#'                                  nseeds = 1)
#'
#' nl@simdesign <- simdesign_GenSA(nl=nl,
#'                                 par=NULL,
#'                                 evalcrit=1,
#'                                 control=list(max.time = 600),
#'                                 nseeds=1)
#'
#' }
#'
#' @name simdesign
#' @rdname simdesign
#' @export
simdesign <- function(simmethod = character(),
                      siminput = tibble::tibble(),
                      simobject = list(),
                      simseeds = NA_integer_,
                      simoutput = tibble::tibble(),
                      ...) {

  methods::new("simdesign",
               simmethod=simmethod,
               siminput=siminput,
               simobject=simobject,
               simseeds=simseeds,
               simoutput=simoutput)

}

