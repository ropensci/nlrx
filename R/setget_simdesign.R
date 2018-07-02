#' Setter function to set a variable of a simdesign object
#'
#' @description Setter function to set a variable of a simdesign object
#'
#' @param nl nl object
#' @param var valid simdesign variable string
#' @param value valid value for the specified variable
#' @details
#' @examples
#' \dontrun{
#' set_sim(nl, "simseeds") <- c(123, 456, 789)
#' }
#' @aliases "set_sim<-"
#' @rdname "set_sim<-"
#'
#' @export

"set_sim<-" <- function(nl, var, value) {
  attr(nl@experiment@simdesign, var) <- value
  nl
}

#' Getter function to get a variable of a simdesign object
#'
#' @description Getter function to get a variable of a simdesign object
#'
#' @param nl nl object
#' @param var valid simdesign variable string
#' @details
#' @examples
#' \dontrun{
#' get_sim(nl, "simseeds")
#' }
#' @aliases get_sim
#' @rdname get_sim
#'
#' @export

get_sim <- function(nl, var)
{
  return(attr(nl@experiment@simdesign, var))
}



#' Getter function to report the simmethod variable of a simdesign object
#'
#' @description Getter function to report the simmethod variable of a simdesign object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' simmethod(nl)
#' }
#' @aliases simmethod
#' @rdname simmethod
#'
#' @export

simmethod <- function(nl)
{
  return(nl@experiment@simdesign@simmethod)
}



#' Getter function to report the siminput tibble of a simdesign object
#'
#' @description Getter function to report the siminput tibble of a simdesign object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' siminput(nl)
#' }
#' @aliases siminput
#' @rdname siminput
#'
#' @export

siminput <- function(nl)
{
  return(nl@experiment@simdesign@siminput)
}


#' Getter function to report the simobject variable of a simdesign object
#'
#' @description Getter function to report the simobject variable of a simdesign object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' simobject(nl)
#' }
#' @aliases simobject
#' @rdname simobject
#'
#' @export

simobject <- function(nl)
{
  return(nl@experiment@simdesign@simobject)
}


#' Getter function to report the simseeds variable of a simdesign object
#'
#' @description Getter function to report the simseeds variable of a simdesign object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' simseeds(nl)
#' }
#' @aliases simseeds
#' @rdname simseeds
#'
#' @export

simseeds <- function(nl)
{
  return(nl@experiment@simdesign@simseeds)
}



#' Setter function to add an output tibble to a simdesign object
#'
#' @description Setter function to add an output tibble to a simdesign object
#'
#' @param nl nl object
#' @param value output tibble
#' @details
#'
#' This function is handy to add a output tibble form NetLogo simulations to a simdesign object within a nl object.
#' Afterwards, this output table can be accessed by the analyze_nl function to calculate further output.
#'
#' @examples
#' \dontrun{
#' simoutput(nl) <- results
#' }
#' @aliases "simoutput<-"
#' @rdname "simoutput<-"
#'
#' @export

"simoutput<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "simoutput") <- value
  nl
}

#' Getter function to report an output tibble of a simdesign object
#'
#' @description Getter function to report an output tibble of a simdesign object
#'
#' @param nl nl object
#' @details
#'
#' @examples
#' \dontrun{
#' simoutput(nl)
#' }
#' @aliases simoutput
#' @rdname simoutput
#'
#' @export

simoutput <- function(nl)
{
  return(nl@experiment@simdesign@simoutput)
}
