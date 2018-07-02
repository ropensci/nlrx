#' S4 class simdesign holds information on simulation input and output data for a defined nl class object with a valid experiment
#'
#' @slot simmethod A character string for method identification
#' @slot siminput Input parameter tibble for model simulations
#' @slot simobject List of model objects that are created for specific simulation designs and are needed for output analysis (e.g. sobol, morris, eFast)
#' @slot simseeds A vector of model seeds that are used to repeatedly run simulations using the siminput data
#' @slot simoutput Output simulation results can be attached to this slot as tibble
#' @export
methods::setClass("simdesign",

         slots=list(
           simmethod="character",
           siminput="tbl",
           simobject="list",
           simseeds="numeric",
           simoutput="tbl"
           ),

         prototype=list(
           simmethod=NA_character_,
           siminput=tibble(),
           simobject=list(),
           simseeds=NA_integer_,
           simoutput=tibble()
           ),

         where = topenv()
         )

#' S4 class experiment holds information on NetLogo experiment specifications for a defined nl class object
#'
#' @slot expname A character string defining the name of the experiment
#' @slot outpath Path to a directory where experiment output will be stored
#' @slot repetition A number which gives the number of repetitions for each row of the simulation design input tibble
#' @slot tickmetrics Character string "true" runs defined metrics on each simulation tick. "false" runs metrics only after simulation is finished
#' @slot idsetup character string or vector of character strings, defining the name of the NetLogo setup procedure
#' @slot idgo character string or vector of character strings, defining the name of the NetLogo go procedure
#' @slot idfinal character string or vector of character strings, defining the name of NetLogo procedures that should be run after the last tick
#' @slot runtime number of model ticks that should be run for each simulation
#' @slot evalticks vector of tick numbers defining when measurements are taken
#' @slot metrics vector of strings defining valid NetLogo reporters that are taken as output measurements (e.g. c("count turtles", "count patches"))
#' @slot variables a nested list of variables that are changed within a simulation design. The name of each sublist item has to be a valid global of the defined NetLogo model. Each list item consist of a min value, a max value, a step value and a qfun (e.g. list("paramA" = list(min=0, max=1, step=0.1, qfun="qunif")))
#' @slot constants a list of constants that are kept constant within a simulation design. The name of each list item has to be a valid global of the defined NetLogo model (e.g. list("pNUM" = 12, "pLOGIC"="TRUE", "pSTRING"="\"default\""))
#' @slot simdesign Holds a simdesign S4 class object
#' @export
methods::setClass("experiment",

         slots=list(
           expname="character",
           outpath="character",
           repetition="numeric",
           tickmetrics="character",
           idsetup="character",
           idgo="character",
           idfinal="character",
           runtime="numeric",
           evalticks="numeric",
           metrics="character",
           variables="list",
           constants="list",
           simdesign="simdesign"
           ),

         prototype=list(
           expname=NA_character_,
           outpath=NA_character_,
           repetition=1,
           tickmetrics="true",
           idsetup="setup",
           idgo="go",
           idfinal=NA_character_,
           runtime=NA_integer_,
           evalticks=NA_integer_,
           metrics=NA_character_,
           variables=list(),
           constants=list(),
           simdesign=new("simdesign")
           ),

         where = topenv()
         )

#' S4 class nl holds information on NetLogo version and directories and jvm specifications.
#'
#' @slot nlversion A character string defining the NetLogo version that is used
#' @slot nlpath Path to the NetLogo main directory matching the defined version
#' @slot modelpath Path to the NetLogo model file (*.nlogo) that is used for simulations
#' @slot jvmmem Java virtual machine memory capacity in megabytes
#' @slot experiment Holds a experiment S4 class object
#' @export
methods::setClass("nl",
         slots=list(
           nlversion="character",
           nlpath="character",
           modelpath="character",
           jvmmem="numeric",
           experiment="experiment"
           ),

         prototype=list(
           nlversion = NA_character_,
           nlpath = NA_character_,
           modelpath = NA_character_,
           jvmmem = 1024,
           experiment = new("experiment")
           ),

         where = topenv()
         )

