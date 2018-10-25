.initClasses <- function() {

  #' @title S4 class \code{"simdesign"}
  #' @description empty
  #' @slot simmethod A character string for method identification
  #' @slot siminput Input parameter tibble for model simulations
  #' @slot simobject Store method object (e.g. sobol, morris, eFast)
  #' @slot simseeds A vector of model seeds
  #' @slot simoutput variable to attach simulation result tibbles
  #' @import methods tibble
  #' @name simdesign-class
  #' @rdname simdesign-class
  #' @export
  simdesign <- methods::setClass("simdesign",
    slots = list(
      simmethod = "character",
      siminput = "tbl",
      simobject = "list",
      simseeds = "numeric",
      simoutput = "tbl"
    ),

    prototype = list(
      simmethod = NA_character_,
      siminput = tibble::tibble(),
      simobject = list(),
      simseeds = NA_integer_,
      simoutput = tibble::tibble()
    )
  )

  #' @title S4 class \code{"experiment"}
  #' @description empty
  #' @slot expname A character string defining the name of the experiment
  #' @slot outpath Path to a directory where experiment output will be stored
  #' @slot repetition The number of repetitions for each row of the simulation
  #' design input tibble
  #' @slot tickmetrics "true" runs defined metrics on each simulation tick.
  #' "false" runs metrics only after simulation is finished
  #' @slot idsetup character string or vector of character strings, defining
  #' the name of the NetLogo setup procedure
  #' @slot idgo character string or vector of character strings, defining the
  #'  name of the NetLogo go procedure
  #' @slot idfinal character string or vector of character strings, defining
  #' the name of NetLogo procedures that should be run after the last tick
  #' @slot idrunnum character string, defining the name of a NetLogo global
  #' that should be used to parse the current siminputrow during model
  #' executions which can then be used for self-written output.
  #' @slot runtime number of model ticks that should be run for each simulation
  #' @slot evalticks number or vector of tick numbers defining when measurements
  #'  are taken
  #' @slot stopcond a NetLogo reporter that reports TRUE/FALSE. If it reports
  #' TRUE the current simulation run is stopped (e.g. "not any? turtles")
  #' @slot metrics vector of strings defining valid NetLogo reporters that are
  #' taken as output measurements (e.g. c("count turtles", "count patches"))
  #' @slot metrics.turtles vector of strings defining valid turtles-own
  #' variables that are taken as output measurements (e.g. c("who", "pxcor",
  #'  "pycor", "color"))
  #' @slot metrics.patches vector of strings defining valid patches-own
  #' variables that are taken as output measurements (e.g. c("pxcor",
  #' "pycor", "pcolor"))
  #' @slot metrics.links vector of strings defining valid links-own variables
  #' that are taken as output measurements (e.g. c("end1", "end2"))
  #' @slot variables a nested list of variables that are changed within a
  #' simulation design. The name of each sublist item has to be a valid global
  #' of the defined NetLogo model. Each list item consist of a min value, a max
  #' value, a step value and a qfun (e.g. list("paramA" = list(min=0, max=1,
  #' step=0.1, qfun="qunif")))
  #' @slot constants a list of constants that are kept constant within a
  #' simulation design. The name of each list item has to be a valid global of
  #' the defined NetLogo model (e.g. list("pNUM" = 12, "pLOGIC"="TRUE",
  #' "pSTRING"="\"default\""))
  #' @slot simdesign Holds a simdesign S4 class object
  #' @import methods
  #' @name experiment-class
  #' @rdname experiment-class
  #' @export
  experiment <- methods::setClass("experiment",
    slots = list(
      expname = "character",
      outpath = "character",
      repetition = "numeric",
      tickmetrics = "character",
      idsetup = "character",
      idgo = "character",
      idfinal = "character",
      idrunnum = "character",
      runtime = "numeric",
      evalticks = "numeric",
      stopcond = "character",
      metrics = "character",
      metrics.turtles = "character",
      metrics.patches = "character",
      metrics.links = "character",
      variables = "list",
      constants = "list"
    ),

    prototype = list(
      expname = NA_character_,
      outpath = NA_character_,
      repetition = 1,
      tickmetrics = "true",
      idsetup = "setup",
      idgo = "go",
      idfinal = NA_character_,
      idrunnum = NA_character_,
      runtime = NA_integer_,
      evalticks = NA_integer_,
      stopcond = NA_character_,
      metrics = NA_character_,
      metrics.turtles = NA_character_,
      metrics.patches = NA_character_,
      metrics.links = NA_character_,
      variables = list(),
      constants = list()
    )
  )

  #' @title S4 class \code{"nl"}
  #' @description empty
  #' @slot nlversion A character string defining the NetLogo version that is
  #' used
  #' @slot nlpath Path to the NetLogo main directory matching the defined
  #' version
  #' @slot modelpath Path to the NetLogo model file (*.nlogo) that is used
  #' for simulations
  #' @slot jvmmem Java virtual machine memory capacity in megabytes
  #' @slot experiment Holds a experiment S4 class object
  #' @import methods
  #' @name nl-class
  #' @rdname nl-class
  #' @export
  nl <- methods::setClass("nl",
    slots = list(
      nlversion = "character",
      nlpath = "character",
      modelpath = "character",
      jvmmem = "numeric",
      experiment = "experiment",
      simdesign = "simdesign"
    ),

    prototype = list(
      nlversion = NA_character_,
      nlpath = NA_character_,
      modelpath = NA_character_,
      jvmmem = 1024,
      experiment = new("experiment"),
      simdesign = new("simdesign")
    )
  )
}
