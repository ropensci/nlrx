create_sample_data <- function() {

  set.seed(593472)
  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_simple(nl = nl,
                                   nseeds = 1)
  # Store data
  nl_simple <- run_sample_data(nl)
  devtools::use_data(nl_simple, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_ff(nl = nl,
                               nseeds = 1)
  # Store data
  nl_ff <- run_sample_data(nl)
  devtools::use_data(nl_ff, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_lhs(nl=nl,
                                samples=100,
                                nseeds=1,
                                precision=3)
  # Store data
  nl_lhs <- run_sample_data(nl)
  devtools::use_data(nl_lhs, compress = "gzip")

  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_sobol(nl=nl,
                                  samples=1000,
                                  sobolorder=2,
                                  sobolnboot=100,
                                  sobolconf=0.95,
                                  nseeds=1,
                                  precision=3)
  # Store data
  nl_sobol <- run_sample_data(nl)
  devtools::use_data(nl_sobol, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_sobol2007(nl=nl,
                                      samples=1000,
                                      sobolnboot=100,
                                      sobolconf=0.95,
                                      nseeds=1,
                                      precision=3)
  # Store data
  nl_sobol2007 <- run_sample_data(nl)
  devtools::use_data(nl_sobol2007, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_soboljansen(nl=nl,
                                        samples=1000,
                                        sobolnboot=100,
                                        sobolconf=0.95,
                                        nseeds=1,
                                        precision=3)
  # Store data
  nl_soboljansen <- run_sample_data(nl)
  devtools::use_data(nl_soboljansen, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_morris(nl=nl,
                                   morristype="oat",
                                   morrislevels=4,
                                   morrisr=26,
                                   morrisgridjump=2,
                                   nseeds=1)
  # Store data
  nl_morris <- run_sample_data(nl)
  devtools::use_data(nl_morris, compress = "gzip")


  # Create nl
  nl <- setup_sample_data_nl()
  nl@simdesign <- simdesign_eFast(nl=nl,
                                  samples=100,
                                  nseeds=1)
  # Store data
  nl_eFast <- run_sample_data(nl)
  devtools::use_data(nl_eFast, compress = "gzip")
}

setup_sample_data_nl <- function() {

  ## Step1: Create a nl obejct:
  nl <- nl(nlversion = "6.0.2",
           nlpath = "/home/uni08/jsaleck/NetLogo_6.0.2/",
           modelpath = "/home/uni08/jsaleck/NetLogo_6.0.2/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo",
           jvmmem = 1000)

  ## Step2: Add Experiment

  nl@experiment <- experiment(expname = "nlrx_simple",
                              outpath = "/home/uni08/jsaleck/nlrxout",
                              repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                              tickmetrics = "false",
                              idsetup = "setup",   # you can define multiple setup procedures with c()
                              idgo = "go",         # you can define multiple go procedures with c()
                              idfinal = NA_character_,  # you can define one or more final commands here
                              runtime = 50,
                              evalticks = 50,
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

run_sample_data <- function (nl) {

  library(future)
  library(furrr)
  library(future.batchtools)
  library(debugme)
  Sys.setenv(DEBUGME='batchtools')
  library(batchtools)


  options(future.makeNodePSOCK.rshopts = c("-i", "C:/Users/Jan/ownCloud/Coding/GWDG_HPC_Cluster/puttyprivate.ppk"))
  login <- tweak(remote, workers="gwdu101.gwdg.de", user="jsaleck")

  ## Define plan for future environment:
  bsub <- tweak(batchtools_lsf, template = 'lsf.tmpl',
                # workers = "export LSF_ENVDIR=/opt/lsf/conf",
                resources = list(job.name = 'poplar',
                                 log.file = 'poplar.log',
                                 queue = 'mpi',
                                 walltime = '01:00',   #12:00
                                 coremem = 2000,   # mb RAM per core
                                 processes = 12))   #8


  plan(list(login,
            bsub,
            multisession))

  results %<-% furrr::future_map_dfr(getsim(nl, "simseeds"), function(seed){
    furrr::future_map_dfr(seq_len(nrow(getsim(nl, "siminput"))), function(siminputrow) {

      run_nl(nl = nl,
             seed = seed,
             siminputrow = siminputrow,
             cleanup = "all")
    })
  })

  ## Attach output to nl
  setsim(nl, "simoutput") <- results

  return(nl)
}
