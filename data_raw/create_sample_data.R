## Creates the sample data in data/ (see R/sample_data.R).
##
## Run from the package root with nlrx loaded (pkgload::load_all()):
##   source("data_raw/create_sample_data.R")
##   create_sample_data(nlpath = "/path/to/NetLogo 7.0.4", outpath = tempdir())
##
## The simulations run on the local NetLogo installation; the paths stored
## in the saved nl objects are replaced by the placeholders below, so no
## local paths end up in the package.
create_sample_data <- function(nlpath,
                               modelpath = file.path(nlpath, "models/Sample Models/Biology/Wolf Sheep Predation.nlogox"),
                               outpath = tempdir(),
                               data_dir = "data",
                               threads = 1) {

  ## Set random seed
  set.seed(593472)

  ## Run all simdesigns with the requested number of NetLogo threads
  run_sample_data <- function(nl) run_sample_data_all(nl, threads = threads)

  ## Save one sample data set, with placeholder paths
  use_data <- function(nl, compress) {
    name <- deparse(substitute(nl))
    nl@nlpath <- "C:/Program Files/NetLogo 7.0.4/"
    nl@modelpath <- "C:/Program Files/NetLogo 7.0.4/models/Sample Models/Biology/Wolf Sheep Predation.nlogox"
    nl@experiment@outpath <- "C:/out"
    assign(name, nl)
    save(list = name, file = file.path(data_dir, paste0(name, ".rda")),
         compress = compress, version = 2)
  }

  # Create testdata nl_simple
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_simple(nl = nl,
                                   nseeds = 1)
  nl_simple <- run_sample_data(nl)
  use_data(nl_simple, compress = "gzip")


  # Create testdata nl_ff
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_ff(nl = nl,
                               nseeds = 1)
  nl_ff <- run_sample_data(nl)
  use_data(nl_ff, compress = "gzip")


  # Create testdata nl_lhs
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=100,
                                nseeds=1,
                                precision=3)
  nl_lhs <- run_sample_data(nl)
  use_data(nl_lhs, compress = "gzip")

  # Create testdata nl_sobol
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_sobol(nl=nl,
                                  samples=1000,
                                  sobolorder=2,
                                  sobolnboot=100,
                                  sobolconf=0.95,
                                  nseeds=1,
                                  precision=3)
  nl_sobol <- run_sample_data(nl)
  use_data(nl_sobol, compress = "xz")


  # Create testdata nl_sobol2007
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_sobol2007(nl=nl,
                                      samples=1000,
                                      sobolnboot=100,
                                      sobolconf=0.95,
                                      nseeds=1,
                                      precision=3)
  nl_sobol2007 <- run_sample_data(nl)
  use_data(nl_sobol2007, compress = "xz")


  # Create testdata nl_soboljansen
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_soboljansen(nl=nl,
                                        samples=1000,
                                        sobolnboot=100,
                                        sobolconf=0.95,
                                        nseeds=1,
                                        precision=3)
  nl_soboljansen <- run_sample_data(nl)
  use_data(nl_soboljansen, compress = "xz")


  # Create testdata nl_morris
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_morris(nl=nl,
                                   morristype="oat",
                                   morrislevels=4,
                                   morrisr=26,
                                   morrisgridjump=2,
                                   nseeds=1)
  nl_morris <- run_sample_data(nl)
  use_data(nl_morris, compress = "gzip")


  # Create testdata nl_eFast
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_eFast(nl=nl,
                                  samples=100,
                                  nseeds=1)
  nl_eFast <- run_sample_data(nl)
  use_data(nl_eFast, compress = "gzip")

  ## Create testdata nl_spatial
  nl <- setup_sample_data_nl_spatial(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_simple(nl, nseeds=1)
  nl_spatial <- run_sample_data(nl)
  use_data(nl_spatial, compress = "gzip")

  ## Create testdata nl_distinct
  nl <- setup_sample_data_nl_distinct(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_distinct(nl, nseeds=1)
  nl_distinct <- run_sample_data(nl)
  use_data(nl_distinct, compress = "gzip")

  ## Create testdata nl_gensa
  nl <- setup_sample_data_nl(nlpath, modelpath, outpath)
  nl@simdesign <- simdesign_GenSA(nl=nl,
                                  par=NULL,
                                  evalcrit=1,
                                  control=list(max.call = 20),
                                  nseeds=1)
  nl_gensa <- run_sample_data_dyn(nl)
  use_data(nl_gensa, compress = "gzip")

}

setup_sample_data_nl <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 10,
                              evalticks = 10,
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' = list(min=50, max=150, step=10, qfun="qunif"),
                                               'initial-number-wolves' = list(min=50, max=150, step=10, qfun="qunif")),
                              constants = list("model-version" = "sheep-wolves-grass",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = FALSE))

  return(nl)

}

setup_sample_data_nl_spatial <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              tickmetrics = "true",
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 10,
                              metrics = c("count sheep","count wolves"),
                              metrics.turtles = list("turtles" = c("who", "pxcor", "pycor")),
                              metrics.patches = c("pxcor", "pycor", "pcolor"),
                              constants = list("model-version" = "sheep-wolves-grass",
                                               'initial-number-sheep' = 100,
                                               'initial-number-wolves' = 50,
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = FALSE))

  return(nl)

}

setup_sample_data_nl_distinct <- function(nlpath, modelpath, outpath) {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "7.0.4",
           nlpath = nlpath,
           modelpath = modelpath,
           jvmmem = 1000)

  ## Step2: Add Experiment
  nl@experiment <- experiment(expname = "nlrx",
                              outpath = outpath,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              idfinal = NA_character_,
                              runtime = 10,
                              evalticks = 10,
                              metrics = c("count sheep","count wolves"),
                              variables = list('initial-number-sheep' = list(values=c(10, 20, 30)),
                                               'initial-number-wolves' = list(values=c(10, 20, 30))),
                              constants = list("model-version" = "sheep-wolves-grass",
                                               "grass-regrowth-time" = 30,
                                               "sheep-gain-from-food" = 4,
                                               "wolf-gain-from-food" = 20,
                                               "sheep-reproduce" = 4,
                                               "wolf-reproduce" = 5,
                                               "show-energy?" = FALSE))

  return(nl)

}


run_sample_data_all <- function(nl, threads = 1) {

  results <- run_nl_all(nl = nl, threads = threads)

  ## Attach output to nl
  setsim(nl, "simoutput") <- results

  return(nl)
}

run_sample_data_dyn <- function(nl) {

  ## One optimization per simdesign seed (a tibble with seed and result)
  results <- run_nl_dyn(nl = nl)

  ## Attach output to nl
  setsim(nl, "simoutput") <- results

  return(nl)
}
