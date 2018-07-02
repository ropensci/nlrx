# Initialize class "simdesign"
#' @export
methods::setClass("simdesign",

         slots=list(
           simmethod="character",
           siminput="tbl",
           simobject="list",
           simseeds="numeric",
           simoutput="tbl"
           ),

         prototype=list(
           simmethod=NA_character_,
           siminput=tibble(),
           simobject=list(),
           simseeds=NA_integer_,
           simoutput=tibble()
           ),

         where = topenv()
         )

# Initialize class "experiment"
#' @export
methods::setClass("experiment",

         slots=list(
           expname="character",
           outpath="character",
           repetition="numeric",
           tickmetrics="character",
           idsetup="character",
           idgo="character",
           idfinal="character",
           runtime="numeric",
           evalticks="numeric",
           metrics="character",
           variables="list",
           constants="list",
           simdesign="simdesign"
           ),

         prototype=list(
           expname=NA_character_,
           outpath=NA_character_,
           repetition=1,
           tickmetrics="true",
           idsetup="setup",
           idgo="go",
           idfinal=NA_character_,
           runtime=NA_integer_,
           evalticks=NA_integer_,
           metrics=NA_character_,
           variables=list(),
           constants=list(),
           simdesign=new("simdesign")
           ),

         where = topenv()
         )

# Initialize class "nl"
#' @export
methods::setClass("nl",
         slots=list(
           nlversion="character",
           nlpath="character",
           modelpath="character",
           jvmmem="numeric",
           experiment="experiment"
           ),

         prototype=list(
           nlversion = NA_character_,
           nlpath = NA_character_,
           modelpath = NA_character_,
           jvmmem = 1024,
           experiment = new("experiment")
           ),

         where = topenv()
         )

