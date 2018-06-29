
# initialize classes
nlrx_init_classes <- function() {

  library(tibble)

  # Initialize class "simdesign"
  setClass("simdesign",

           slots=list(
             method="character",
             simdata="tbl",
             simobject="list",
             simseeds="numeric"
           ),

           prototype=list(
             method=NA_character_,
             simdata=tibble(),
             simobject=list(),
             simseeds=NA_integer_
           ))


  # Initialize class "experiment"
  setClass("experiment",

           slots=list(
             sim.name="character",
             out.path="character",
             n.rep="numeric",
             metrics.tick="character",
             setup.name="character",
             go.name="character",
             max.ticks="numeric",
             eval.ticks="numeric",
             metrics="character",
             param.change="list",
             param.const="list",
             simdesign="simdesign"),

           prototype=list(
             sim.name=NA_character_,
             out.path=NA_character_,
             n.rep=1,
             metrics.tick="true",
             setup.name="setup",
             go.name="go",
             max.ticks=NA_integer_,
             eval.ticks=NA_integer_,
             metrics=NA_character_,
             param.change=list(),
             param.const=list(),
             simdesign=new("simdesign"))
  )

  # Initialize class "nl"
  setClass("nl",
           slots=list(nl.path="character",
                      model.path="character",
                      experiment="experiment"),

           prototype=list(
             nl.path = NA_character_,
             model.path = NA_character_,
             experiment = new("experiment"))
  )
}

# create and return a nl object
nlrx_get_nl <- function(nl.path, model.path) {

  nlrx_init_classes()

  nl <- new("nl",
            nl.path=nl.path,
            model.path=model.path)

  return(nl)

}


# create and attach an experiment to a nl object
nlrx_add_experiment <- function(nl, sim.name, out.path, n.rep, metrics.tick, setup.name, go.name, max.ticks, eval.ticks, metrics, param.change, param.const) {

  experiment <- new("experiment",
                    sim.name=sim.name,
                    out.path=out.path,
                    n.rep=n.rep,
                    metrics.tick=metrics.tick,
                    setup.name=setup.name,
                    go.name=go.name,
                    max.ticks=max.ticks,
                    eval.ticks=eval.ticks,
                    metrics=metrics,
                    param.change=param.change,
                    param.const=param.const)

  nl@experiment <- experiment

  return(nl)
}
