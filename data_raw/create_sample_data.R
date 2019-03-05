create_sample_data <- function() {

  ## Set random seed
  set.seed(593472)

  ## Set nl path, model path and output path
  nlpath <- "C:/Program Files/NetLogo 6.0.4/"
  modelpath <- "C:/Program Files/NetLogo 6.0.4/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo"
  outpath <- "C:/out"

  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_simple(nl = nl,
                                   nseeds = 1)
  # Store data
  nl_simple <- run_sample_data(nl)
  devtools::use_data(nl_simple, compress = "gzip", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_ff(nl = nl,
                               nseeds = 1)
  # Store data
  nl_ff <- run_sample_data(nl)
  devtools::use_data(nl_ff, compress = "gzip", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=100,
                                nseeds=1,
                                precision=3)
  # Store data
  nl_lhs <- run_sample_data(nl)
  devtools::use_data(nl_lhs, compress = "gzip", overwrite = TRUE)

  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_sobol(nl=nl,
                                  samples=1000,
                                  sobolorder=2,
                                  sobolnboot=100,
                                  sobolconf=0.95,
                                  nseeds=1,
                                  precision=3)
  # Store data
  nl_sobol <- run_sample_data(nl)
  devtools::use_data(nl_sobol, compress = "xz", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_sobol2007(nl=nl,
                                      samples=1000,
                                      sobolnboot=100,
                                      sobolconf=0.95,
                                      nseeds=1,
                                      precision=3)
  # Store data
  nl_sobol2007 <- run_sample_data(nl)
  devtools::use_data(nl_sobol2007, compress = "xz", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_soboljansen(nl=nl,
                                        samples=1000,
                                        sobolnboot=100,
                                        sobolconf=0.95,
                                        nseeds=1,
                                        precision=3)
  # Store data
  nl_soboljansen <- run_sample_data(nl)
  devtools::use_data(nl_soboljansen, compress = "xz", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_morris(nl=nl,
                                   morristype="oat",
                                   morrislevels=4,
                                   morrisr=26,
                                   morrisgridjump=2,
                                   nseeds=1)
  # Store data
  nl_morris <- run_sample_data(nl)
  devtools::use_data(nl_morris, compress = "gzip", overwrite = TRUE)


  # Create nl
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_eFast(nl=nl,
                                  samples=100,
                                  nseeds=1)
  # Store data
  nl_eFast <- run_sample_data(nl)
  devtools::use_data(nl_eFast, compress = "gzip", overwrite = TRUE)

  ## Spatial testdata:
  nl <- setup_sample_data_nl_spatial(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_simple(nl, nseeds=1)

  # Store data:
  nl_spatial <- run_sample_data(nl)
  devtools::use_data(nl_spatial, compress = "gzip", overwrite = TRUE)

  ## Nl distinct:
  nl <- setup_sample_data_nl_distinct(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_distinct(nl, nseeds=1)

  # Store data:
  nl_distinct <- run_sample_data(nl)
  devtools::use_data(nl_distinct, compress = "gzip", overwrite = TRUE)


}

setup_sample_data_nl <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "6.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 10,
                              evalticks = 10,
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' = list(min=50, max=150, step=10, qfun="qunif"),
                                               'initial-number-wolves' = list(min=50, max=150, step=10, qfun="qunif")),
                              constants = list("model-version" = "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  return(nl)

}

setup_sample_data_nl_spatial <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "6.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 10,
                              metrics = c("count sheep","count wolves"),
                              metrics.turtles = list("turtles" = c("who", "pxcor", "pycor")),
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
                              constants = list("model-version" = "\"sheep-wolves-grass\"",
                                               'initial-number-sheep' = 100,
                                               'initial-number-wolves' = 50,
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  return(nl)

}

setup_sample_data_nl_distinct <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "6.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 10,
                              evalticks = 10,
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' = list(values=c(10, 20, 30)),
                                               'initial-number-wolves' = list(values=c(10, 20, 30))),
                              constants = list("model-version" = "\"sheep-wolves-grass\"",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = "false"))

  return(nl)

}


run_sample_data <- function (nl) {

  library(future)
  plan(multisession)

  results <- run_nl_all(nl = nl)

  ## Attach output to nl
  setsim(nl, "simoutput") <- results

  return(nl)
}


