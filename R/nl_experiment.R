
new_experiment <- function(expname = "defaultexp",
                           outpath,
                           repetition = 1,
                           tickmetrics = "true",
                           idsetup = "setup",
                           idgo = "go",
                           idfinal = NA_character_,
                           runtime = 1,
                           evalticks = seq(1,runtime,1),
                           metrics = c("count turtles"),
                           variables = list(),
                           constants = list()) {

  newexp <- new("experiment",
                expname=expname,
                outpath=outpath,
                repetition=repetition,
                tickmetrics=tickmetrics,
                idsetup=idsetup,
                idgo=idgo,
                idfinal=idfinal,
                runtime=runtime,
                evalticks=evalticks,
                metrics=metrics,
                variables=variables,
                constants=constants)

  return(newexp)

}


eval_variables <- function(nl) {

  if(length(variables(nl)) == 0){
    stop("Error: Experiment Variable list is empty. You need to define a variable list with at least one element!")
  }
}

eval_constants <- function(nl) {

  if(length(constants(nl)) == 0){
    stop("Error: Experiment constants list is empty. You need to define a constants list with at least one element!")
  }
}

eval_experiment <- function(nl) {

  # A valid experiment needs at least
  notvalid <- c()

  if(is.na(expname(nl))) {
    notvalid <- c(notvalid, "expname")
  }
  if(is.na(outpath(nl))) {
    notvalid <- c(notvalid, "outpath")
  }
  if(is.na(runtime(nl))) {
    notvalid <- c(notvalid, "runtime")
  }
  if(is.na(metrics(nl))) {
    notvalid <- c(notvalid, "metrics")
  }
  if(purrr::is_empty(variables(nl)) & purrr::is_empty(constants(nl))) {
    notvalid <- c(notvalid, "variables or constants")
  }

  stop(paste0("Error: To add a sim design to a nl object you need to define a proper experiment first. The following elements are missing without default: ", paste(notvalid, collapse=" ; ")))

}
