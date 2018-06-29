
## Todo:
#
# Write a function that downloads NL and sets the pathes
# Write the batch file with java parameters
# Extend xml to includ emultiple setup/go and final commands
# Postprocessing functions
# - Sensitivity indices


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl.path <- "C:/Program Files/NetLogo 6.0.2/netlogo-headless.bat"
model.path <- "C:/Program Files/NetLogo 6.0.2/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo"

nl <- nlrx_get_nl(nl.path, model.path)

## Step2: Add an experiment
sim.name <- "nlrxtest"
out.path <- "C:/Users/Jan/ownCloud/Rdev/nlrx_output/"
n.rep <- 1
metrics.tick <- "true"
setup.name <- "setup"
go.name <- "go"
max.ticks <- 10
eval.ticks <- seq(8,10)
metrics <- c("ticks","count sheep","count wolves")
param.change <- list('initial-number-sheep' = list(quantile.function="qunif", min=50, max=150),
                     'initial-number-wolves' = list(quantile.function="qunif", min=50, max=150))

param.const <- list("model-version" = "\"sheep-wolves-grass\"",
                     "grass-regrowth-time" = 30,
                     "sheep-gain-from-food" = 4,
                     "wolf-gain-from-food" = 20,
                     "sheep-reproduce" = 4,
                     "wolf-reproduce" = 5,
                     "show-energy?" = "false")

nl <- nlrx_add_experiment(nl = nl,
                          sim.name = sim.name,
                          out.path = out.path,
                          n.rep = n.rep,
                          metrics.tick = metrics.tick,
                          setup.name = setup.name,
                          go.name = go.name,
                          max.ticks = max.ticks,
                          eval.ticks = eval.ticks,
                          metrics = metrics,
                          param.change = param.change,
                          param.const = param.const
                          )

## Step3: Add a simDesign
sim.seedrep <- 3 # ALL
sim.sample <- 10 # sobol and lhs
sim.order <- 2 # sobol
sim.nboot <- 10 # sobol
sim.digits <- 3 # sobol and lhs
sim.type <- "oat"  # morris
sim.levels <- 4  # morris
sim.rep <- 4 # morris
sim.grid.jump <- 2 # morris


# Create a lhs simdesign
nl <- nlrx_add_simdesign_lhs(nl = nl,
                             sim.seedrep = sim.seedrep,
                             sim.sample = sim.sample,
                             sim.digits = sim.digits)

# Create a sobol simdesign
nl <- nlrx_add_simdesign_sobol(nl = nl,
                               sim.seedrep = sim.seedrep,
                               sim.sample = sim.sample,
                               sim.order = sim.order,
                               sim.nboot = sim.nboot,
                               sim.digits = sim.digits)


nl <- nlrx_add_simdesign_morris(nl = nl,
                                sim.seedrep = sim.seedrep,
                                sim.type = sim.type,
                                sim.levels = sim.levels,
                                sim.rep = sim.rep,
                                sim.grid.jump = sim.grid.jump)





## Inspect simdata:
simdata <- nlrx_show_simdata(nl)


## Run one:

result <- nlrx_run_one(nl=nl,
             seed=nl@experiment@simdesign@simseeds[1],
             run=1,
             cleanup="all")


## Run in parallel:


library(future)
library(furrr)

plan(sequential)

result %<-% furrr::future_map_dfr(nl@experiment@simdesign@simseeds, function(seed){

  ## Execute simulation function
  furrr::future_map_dfr(seq_len(nrow(nl@experiment@simdesign@simdata)), function(run) {

    nlrx_run_one(nl = nl,
                 seed = seed,
                 run = run,
                 cleanup = "all")

  },  .id = "run")
}, .id = "seed")



