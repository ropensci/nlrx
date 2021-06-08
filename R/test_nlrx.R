#' Test if nlrx runs on the local system
#'
#' @description Runs a short test if nlrx runs on the local system
#' @param nlpath Provide a path to a netlogo folder
#' @param nlversion Matching version string of the provided NetLogo folder (e.g. "6.1.1")
#' @details
#' Runs a short test if nlrx runs on the local system. Reports TRUE if successful!
#'
#' @examples
#' \dontrun{
#' test_nlrx(nlpath="/Users/xyz/netlogo/NetLogo 6.1.1", nlversion="6.1.1")
#' }
#'
#' @aliases test_nlrx
#' @rdname test_nlrx
#'
#' @export
test_nlrx <- function(nlpath, nlversion){
  testresult = FALSE
  message("Checking path...")
  if(!dir.exists(nlpath)) {
    stop("Provided NetLogo path does not exist!")
  }

  modelpath <- file.path(nlpath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")

  if(!file.exists(modelpath)){
    stop("Provided nlpath deviates from expected NetLogo standard folder structure. Test aborted!")
  }

  message("Setting up nl object...")
  # Setup nl object
  nl <- nl(nlversion = nlversion,
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1024)

  # Attach experiment
  nl@experiment <- experiment(expname="wolf-sheep",
                              outpath="",
                              repetition=1,
                              tickmetrics="true",
                              idsetup="setup",
                              idgo="go",
                              runtime=1,
                              metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                              variables = list('initial-number-sheep' = list(min=50, max=150, qfun="qunif"),
                                               'initial-number-wolves' = list(min=50, max=150, qfun="qunif")),
                              constants = list("model-version" = "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  # Attach simdesign
  nl@simdesign <- simdesign_simple(nl=nl, nseeds=1)

  message("Execute testrun...")
  # Run all simulations (loop over all siminputrows and simseeds)
  results <- run_nl_all(nl)
  message("success!")
  testresult = TRUE
  return(testresult)
}
