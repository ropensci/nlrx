#' Setter function to set a variable of a nl object
#'
#' @description Setter function to set a variable of a nl object
#' @param nl nl object
#' @param var valid nl variable string
#' @param value valid value for the specified variable
#' @examples
#' \dontrun{
#' setnl(nl, "nlversion") <- "6.0.3"
#' }
#' @aliases setnl
#' @rdname setnl
#'
#' @export

"setnl<-" <- function(nl, var, value) {
  attr(nl, var) <- value
  nl
}

#' Getter function to get a variable of a nl object
#'
#' @description Getter function to get a variable of a nl object
#' @param nl nl object
#' @param var valid nl variable string
#' @examples
#' \dontrun{
#' getnl(nl, "nlversion")
#' }
#' @aliases getnl
#' @rdname getnl
#'
#' @export

getnl <- function(nl, var)
{
  return(attr(nl, var))
}

#' Setter function to set a variable of an experiment object
#'
#' @description Setter function to set a variable of an experiment object
#'
#' @param nl nl object
#' @param var valid experiment variable string
#' @param value valid value for the specified variable
#' @examples
#' \dontrun{
#' setexp(nl, "expname") <- "experimentName"
#' }
#' @aliases setexp
#' @rdname setexp
#'
#' @export

"setexp<-" <- function(nl, var, value) {
  attr(nl@experiment, var) <- value
  nl
}

#' Getter function to get a variable of an experiment object
#'
#' @description Getter function to get a variable of an experiment object
#' @param nl nl object
#' @param var valid experiment variable string
#' @examples
#' \dontrun{
#' getexp(nl, "expname")
#' }
#' @aliases getexp
#' @rdname getexp
#'
#' @export

getexp <- function(nl, var)
{
  return(attr(nl@experiment, var))
}


#' Setter function to set a variable of a simdesign object
#'
#' @description Setter function to set a variable of a simdesign object
#' @param nl nl object
#' @param var valid simdesign variable string
#' @param value valid value for the specified variable
#' @examples
#' \dontrun{
#' setsim(nl, "simseeds") <- c(123, 456, 789)
#' }
#' @aliases setsim
#' @rdname setsim
#'
#' @export

"setsim<-" <- function(nl, var, value) {
  attr(nl@simdesign, var) <- value
  nl
}

#' Getter function to get a variable of a simdesign object
#'
#' @description Getter function to get a variable of a simdesign object
#' @param nl nl object
#' @param var valid simdesign variable string
#' @examples
#' \dontrun{
#' getsim(nl, "simseeds")
#' }
#' @aliases getsim
#' @rdname getsim
#'
#' @export

getsim <- function(nl, var)
{
  return(attr(nl@simdesign, var))
}
