#' Setter function to set a variable of a nl object
#'
#' @description Setter function to set a variable of a nl object
#' @param nl nl object
#' @param var valid nl variable string
#' @param value valid value for the specified variable
#' @examples
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # set NetLogo version
#' setnl(nl, "nlversion") <- "6.0.3"
#'
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
#'
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # get NetLogo version
#' getnl(nl, "nlversion")
#'
#' @aliases getnl
#' @rdname getnl
#'
#' @export

getnl <- function(nl, var) {
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
#'
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # Set experiment name
#' setexp(nl, "expname") <- "experimentName"
#'
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
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # Set experiment name
#' setexp(nl, "expname") <- "experimentName"
#'
#' # Get experiment name
#' getexp(nl, "experiment")
#'
#' @aliases getexp
#' @rdname getexp
#'
#' @export

getexp <- function(nl, var) {
  return(attr(nl@experiment, var))
}


#' Setter function to set a variable of a simdesign object
#'
#' @description Setter function to set a variable of a simdesign object
#' @param nl nl object
#' @param var valid simdesign variable string
#' @param value valid value for the specified variable
#' @examples
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # Set simulation seeds
#' setsim(nl, "simseeds") <- c(123, 456, 789)
#'
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
#' # Example for Wolf Sheep Predation model from NetLogo models library:
#' nl <- nl(nlversion = "6.0.3",
#' nlpath = "/home/user/NetLogo 6.0.3/",
#' modelpath = "/home/user/NetLogo 6.0.3/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
#' jvmmem = 1024)
#'
#' # Set simulation seeds
#' setsim(nl, "simseeds") <- c(123, 456, 789)
#'
#' # Set simulation seeds
#' getsim(nl, "simseeds")
#'
#' @aliases getsim
#' @rdname getsim
#'
#' @export

getsim <- function(nl, var) {
  return(attr(nl@simdesign, var))
}
