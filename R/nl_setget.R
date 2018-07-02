# Setter and getter for nl object variables

"nlversion<-" <- function(nl, value){
  attr(nl, "nlversion") <- value
  nl
}

nlversion <- function(nl)
{
  return(nl@nlversion)
}


"nlpath<-" <- function(nl, value){
  attr(nl, "nlpath") <- value
  nl
}

nlpath <- function(nl)
{
  return(nl@nlpath)
}


"modelpath<-" <- function(nl, value){
  attr(nl, "modelpath") <- value
  nl
}

modelpath <- function(nl)
{
  return(nl@modelpath)
}


"jvmmem<-" <- function(nl, value){
  attr(nl, "jvmmem") <- value
  nl
}

jvmmem <- function(nl)
{
  return(nl@jvmmem)
}

"experiment<-" <- function(nl, value){
  attr(nl, "experiment") <- value
  nl
}

experiment <- function(nl)
{
  return(nl@experiment)
}
