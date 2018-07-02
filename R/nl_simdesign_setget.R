# Setter and getter for nl experiment simdesign object variables


"simmethod<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "simmethod") <- value
  nl
}

simmethod <- function(nl)
{
  return(nl@experiment@simdesign@simmethod)
}

"siminput<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "siminput") <- value
  nl
}

siminput <- function(nl)
{
  return(nl@experiment@simdesign@siminput)
}

"simobject<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "simobject") <- value
  nl
}

simobject <- function(nl)
{
  return(nl@experiment@simdesign@simobject)
}

"simseeds<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "simseeds") <- value
  nl
}

simseeds <- function(nl)
{
  return(nl@experiment@simdesign@simseeds)
}

"simoutput<-" <- function(nl, value){
  attr(nl@experiment@simdesign, "simoutput") <- value
  nl
}

simoutput <- function(nl)
{
  return(nl@experiment@simdesign@simoutput)
}
