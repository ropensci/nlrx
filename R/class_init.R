

#' @title initClasses
#' @description empty
#' @name init-classes
#' @rdname init-classes
#' @keywords internal
.initClasses <- function() {

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
      experiment = methods::new("experiment"),
      simdesign = methods::new("simdesign")
    )
  )
}
