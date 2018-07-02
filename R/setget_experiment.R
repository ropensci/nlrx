#' Setter function to set a variable of an experiment object
#'
#' @description Setter function to set a variable of an experiment object
#'
#' @param nl nl object
#' @param var valid experiment variable string
#' @param value valid value for the specified variable
#' @details
#' @examples
#' \dontrun{
#' set_exp(nl, "expname") <- "experimentName"
#' }
#' @aliases "set_exp<-"
#' @rdname "set_exp<-"
#'
#' @export

"set_exp<-" <- function(nl, var, value) {
  attr(nl@experiment, var) <- value
  nl
}

#' Getter function to get a variable of an experiment object
#'
#' @description Getter function to get a variable of an experiment object
#'
#' @param nl nl object
#' @param var valid experiment variable string
#' @details
#' @examples
#' \dontrun{
#' get_exp(nl, "expname")
#' }
#' @aliases get_exp
#' @rdname get_exp
#'
#' @export

get_exp <- function(nl, var)
{
  return(attr(nl@experiment, var))
}


#' Getter function to report the outpath of an experiment object
#'
#' @description Getter function to report the outpath of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' outpath(nl)
#' }
#' @aliases outpath
#' @rdname outpath
#'
#' @export

outpath <- function(nl)
{
  return(nl@experiment@outpath)
}


#' Getter function to report the repetition variable of an experiment object
#'
#' @description Getter function to report the repetition variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' repetition(nl)
#' }
#' @aliases repetition
#' @rdname repetition
#'
#' @export

repetition <- function(nl)
{
  return(nl@experiment@repetition)
}

#' Getter function to report the tickmetrics variable of an experiment object
#'
#' @description Getter function to report the tickmetrics variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' tickmetrics(nl)
#' }
#' @aliases tickmetrics
#' @rdname tickmetrics
#'
#' @export

tickmetrics <- function(nl)
{
  return(nl@experiment@tickmetrics)
}


#' Getter function to report the idsetup variable of an experiment object
#'
#' @description Getter function to report the idsetup variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' idsetup(nl)
#' }
#' @aliases idsetup
#' @rdname idsetup
#'
#' @export

idsetup <- function(nl)
{
  return(nl@experiment@idsetup)
}


#' Getter function to report the idgo variable of an experiment object
#'
#' @description Getter function to report the idgo variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' idgo(nl)
#' }
#' @aliases idgo
#' @rdname idgo
#'
#' @export

idgo <- function(nl)
{
  return(nl@experiment@idgo)
}


#' Getter function to report the idfinal variable of an experiment object
#'
#' @description Getter function to report the idfinal variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' idfinal(nl)
#' }
#' @aliases idfinal
#' @rdname idfinal
#'
#' @export

idfinal <- function(nl)
{
  return(nl@experiment@idfinal)
}


#' Getter function to report the runtime variable of an experiment object
#'
#' @description Getter function to report the runtime variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' runtime(nl)
#' }
#' @aliases runtime
#' @rdname runtime
#'
#' @export

runtime <- function(nl)
{
  return(nl@experiment@runtime)
}



#' Getter function to report the evalticks variable of an experiment object
#'
#' @description Getter function to report the evalticks variable of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' evalticks(nl)
#' }
#' @aliases evalticks
#' @rdname evalticks
#'
#' @export

evalticks <- function(nl)
{
  return(nl@experiment@evalticks)
}



#' Getter function to report the metrics vector of an experiment object
#'
#' @description Getter function to report the metrics vector of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' metrics(nl)
#' }
#' @aliases metrics
#' @rdname metrics
#'
#' @export

metrics <- function(nl)
{
  return(nl@experiment@metrics)
}


#' Getter function to report the variables list of an experiment object
#'
#' @description Getter function to report the variables list of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' variables(nl)
#' }
#' @aliases variables
#' @rdname variables
#'
#' @export

variables <- function(nl)
{
  return(nl@experiment@variables)
}


#' Getter function to report the constants list of an experiment object
#'
#' @description Getter function to report the constants list of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' constants(nl)
#' }
#' @aliases constants
#' @rdname constants
#'
#' @export

constants <- function(nl)
{
  return(nl@experiment@constants)
}

#' Setter function to add a simdesign object to an experiment object
#'
#' @description Setter function to add a simdesign object to an experiment object
#'
#' @param nl nl object
#' @param value simdesign object
#' @details
#'
#' This function is handy to add a simdesign object to an already existing experiment.
#' This package provides a bunch of simdesign creation functions which report a simdesign object.
#' With this setter function, these simdesign objects can be directly attached to an nl object.
#'
#' @examples
#' \dontrun{
#' simdesign(nl) <- simdesign_simple(nl=nl, nseeds=3)
#' }
#' @aliases "simdesign<-"
#' @rdname "simdesign<-"
#'
#' @export

"simdesign<-" <- function(nl, value){
  attr(nl@experiment, "simdesign") <- value
  nl
}

#' Getter function to report a simdesign object of an experiment object
#'
#' @description Getter function to report a simdesign object of an experiment object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' simdesign(nl)
#' }
#' @aliases simdesign
#' @rdname simdesign
#'
#' @export

simdesign <- function(nl)
{
  return(nl@experiment@simdesign)
}
