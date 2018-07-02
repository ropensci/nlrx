#' Setter function to set a variable of a nl object
#'
#' @description Setter function to set a variable of a nl object
#'
#' @param nl nl object
#' @param var valid nl variable string
#' @param value valid value for the specified variable
#' @details
#' @examples
#' \dontrun{
#' set_nl(nl, "nlversion") <- "6.0.3"
#' }
#' @aliases "set_nl<-"
#' @rdname "set_nl<-"
#'
#' @export

"set_nl<-" <- function(nl, var, value) {
  attr(nl, var) <- value
  nl
}

#' Getter function to get a variable of a nl object
#'
#' @description Getter function to get a variable of a nl object
#'
#' @param nl nl object
#' @param var valid nl variable string
#' @details
#' @examples
#' \dontrun{
#' get_nl(nl, "nlversion")
#' }
#' @aliases get_nl
#' @rdname get_nl
#'
#' @export

get_nl <- function(nl, var)
{
  return(attr(nl, var))
}


#' Setter function to add an experiment object to a nl object
#'
#' @description Setter function to add an experiment object to a nl object
#'
#' @param nl nl object
#' @param value experiment object
#' @details
#'
#' This function is handy to add a experiment object to an already existing nl object.
#'
#' @examples
#' \dontrun{
#' experiment(nl) <- new_experiment(expname = "nlxrtest",
#'                                  outpath = "C:/out/",
#'                                  repetition = 1,
#'                                  tickmetrics = "true",
#'                                  idsetup = "setup",
#'                                  idgo = "go",
#'                                  idfinal = NA_character_,
#'                                  runtime = 10,
#'                                  evalticks = seq(8,10),
#'                                  metrics = c("count sheep","count wolves"),
#'                                  variables = list('initial-number-sheep' = list(min=50, max=150, step=10, qfun="qunif"),
#'                                                   'initial-number-wolves' = list(min=50, max=150, step=10, qfun="qunif")),
#'                                  constants = list("model-version" = "\"sheep-wolves-grass\"",
#'                                                   "grass-regrowth-time" = 30,
#'                                                   "sheep-gain-from-food" = 4,
#'                                                   "wolf-gain-from-food" = 20,
#'                                                   "sheep-reproduce" = 4,
#'                                                   "wolf-reproduce" = 5,
#'                                                   "show-energy?" = "false")
#'                                  )
#'
#' }
#' @aliases "experiment<-"
#' @rdname "experiment<-"
#'
#' @export

"experiment<-" <- function(nl, value){
  attr(nl, "experiment") <- value
  nl
}

#' Getter function to report an experiment object of a nl object
#'
#' @description Getter function to report an experiment object of a nl object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' experiment(nl)
#' }
#' @aliases experiment
#' @rdname experiment
#'
#' @export

experiment <- function(nl)
{
  return(nl@experiment)
}



#' Getter function to report the nlversion variable of a nl object
#'
#' @description Getter function to report the nlversion variable of a nl object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' nlversion(nl)
#' }
#' @aliases nlversion
#' @rdname nlversion
#'
#' @export

nlversion <- function(nl)
{
  return(nl@nlversion)
}



#' Getter function to report the nlpath variable of a nl object
#'
#' @description Getter function to report the nlpath variable of a nl object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' nlpath(nl)
#' }
#' @aliases nlpath
#' @rdname nlpath
#'
#' @export

nlpath <- function(nl)
{
  return(nl@nlpath)
}



#' Getter function to report the modelpath variable of a nl object
#'
#' @description Getter function to report the modelpath variable of a nl object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' modelpath(nl)
#' }
#' @aliases modelpath
#' @rdname modelpath
#'
#' @export

modelpath <- function(nl)
{
  return(nl@modelpath)
}
