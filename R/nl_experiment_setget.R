# Setter and getter for nl experiment object variables

"expname<-" <- function(nl, value){
  attr(nl@experiment, "expname") <- value
  nl
}

expname <- function(nl)
{
  return(nl@experiment@expname)
}

"outpath<-" <- function(nl, value){
  attr(nl@experiment, "outpath") <- value
  nl
}

outpath <- function(nl)
{
  return(nl@experiment@outpath)
}

"repetition<-" <- function(nl, value){
  attr(nl@experiment, "repetition") <- value
  nl
}

repetition <- function(nl)
{
  return(nl@experiment@repetition)
}

"tickmetrics<-" <- function(nl, value){
  attr(nl@experiment, "tickmetrics") <- value
  nl
}

tickmetrics <- function(nl)
{
  return(nl@experiment@tickmetrics)
}


"idsetup<-" <- function(nl, value){
  attr(nl@experiment, "idsetup") <- value
  nl
}

idsetup <- function(nl)
{
  return(nl@experiment@idsetup)
}

"idgo<-" <- function(nl, value){
  attr(nl@experiment, "idgo") <- value
  nl
}

idgo <- function(nl)
{
  return(nl@experiment@idgo)
}

"idfinal<-" <- function(nl, value){
  attr(nl@experiment, "idfinal") <- value
  nl
}

idfinal <- function(nl)
{
  return(nl@experiment@idfinal)
}

"runtime<-" <- function(nl, value){
  attr(nl@experiment, "runtime") <- value
  nl
}

runtime <- function(nl)
{
  return(nl@experiment@runtime)
}


"evalticks<-" <- function(nl, value){
  attr(nl@experiment, "evalticks") <- value
  nl
}

evalticks <- function(nl)
{
  return(nl@experiment@evalticks)
}

"metrics<-" <- function(nl, value){
  attr(nl@experiment, "metrics") <- value
  nl
}

metrics <- function(nl)
{
  return(nl@experiment@metrics)
}

"variables<-" <- function(nl, value){
  attr(nl@experiment, "variables") <- value
  nl
}

variables <- function(nl)
{
  return(nl@experiment@variables)
}

"constants<-" <- function(nl, value){
  attr(nl@experiment, "constants") <- value
  nl
}

constants <- function(nl)
{
  return(nl@experiment@constants)
}

"simdesign<-" <- function(nl, value){
  attr(nl@experiment, "simdesign") <- value
  nl
}

simdesign <- function(nl)
{
  return(nl@experiment@simdesign)
}
