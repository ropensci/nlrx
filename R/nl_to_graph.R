
#' Generate igraph objects from measured turtles and links metrics
#'
#' @description Generate igraph objects from measured turtles and links metrics
#'
#' @param nl nl object
#'
#' Generate igraph objects from measured turtles and links metrics.
#' Output has to be attached to the simdesign first with simoutput(nl) <- results
#' I graph objects are created automatically for each combination of random-seed, siminputrow and step.
#' An additional column with igraph objects is attached to the original output results tibble of the nl object.
#' In order to generate igraph objects some metrics are mandatory:
#' The metrics.turtles slot of the experiment must contain "who" numbers (see example experiment).
#' Additional turtle metrics will be stored as properties of the igraph vertices.
#' The metrics.links slot of the experiment must contain "who" numbers of link end1 and end2 (see example experiment).
#' Additional link metrics will be stored as properties of the igraph edges.
#'
#' @examples
#' \dontrun{
#' ## Example running the Giant Component model from the NetLogo models library:
#' library(nlrx)
#' library(igraph)
#' # Windows default NetLogo installation path (adjust to your needs!):
#' netlogopath <- file.path("C:/Program Files/NetLogo 6.0.4")
#' modelpath <- file.path(netlogopath, "app/models/Sample Models/Networks/Giant Component.nlogo")
#' outpath <- file.path("C:/out")
#' # Unix default NetLogo installation path (adjust to your needs!):
#' netlogopath <- file.path("/home/NetLogo 6.0.4")
#' modelpath <- file.path(netlogopath, "app/models/Sample Models/Networks/Giant Component.nlogo")
#' outpath <- file.path("/home/out")
#'
#' nl <- nl(nlversion = "6.0.4",
#'          nlpath = netlogopath,
#'          modelpath = modelpath,
#'          jvmmem = 1024)
#'
#' nl@experiment <- experiment(expname="networks",
#'                             outpath=outpath,
#'                             repetition=1,
#'                             tickmetrics="false",
#'                             idsetup="setup",
#'                             idgo="go",
#'                             runtime=50,
#'                             metrics.turtles = list("turtles" = c("who", "color")),
#'                             metrics.links = list("links" = c("[who] of end1", "[who] of end2")),
#'                             constants = list("num-nodes" = 80,
#'                                              "layout?" = "true"))
#'
#' nl@simdesign <- simdesign_simple(nl, 1)
#' nl@simdesign@simoutput <- run_nl_all(nl)
#' nl.graph <- nl_to_graph(nl)
#'
#' ## Extract graph of tick 1:
#' nl.graph.i <- nl.graph$spatial.links[[1]]
#' ## Set vertex colors by measured color variable:
#' vcols <- c("7" = "grey", "15" = "red")
#' V(nl.graph.i)$color <- vcols[as.character(V(nl.graph.i)$color)]
#' ## Set edge colors by measured link breed:
#' ecols <- c("links" = "black")
#' E(nl.graph.i)$color <- ecols[E(nl.graph.i)$breed]
#'
#' ## Plot:
#' plot.igraph(nl.graph.i, vertex.size=8, vertex.label=NA, edge.arrow.size=0.2)
#'
#' }
#' @aliases nl_to_graph
#' @rdname nl_to_graph
#'
#' @export

nl_to_graph <- function(nl) {

  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    stop("Simoutput tibble is empty.
            In order to generate igraph from turtles and links metrics,
            output results have to be attached to the simdesign of the nl
            object first: setsim(nl, \"simoutput\") <- results")
  }

  ## Unnest simulation results:
  nl.unnest <- unnest_simoutput(nl)

  ## Prepare links data:
  nl.links <- nl.unnest %>% dplyr::filter(agent == "links")
  ## Check if [who] of end1 and [who] of end2 exist:
  if (!"[who] of end1" %in% colnames(nl.links)) {
      stop("igraph needs who numbers of link end1.
           Please add '[who] of end1' to metrics.links")
  }
  if (!"[who] of end2" %in% colnames(nl.links)) {
    stop("igraph needs who numbers of link end1.
         Please add '[who] of end1' to metrics.links")
  }
  ## Select link variables
  selectvars <- c(unique(unlist(getexp(nl, "metrics.links"))),
                  "agent", "breed", "siminputrow", "[step]", "random-seed")
  nl.links <- nl.links %>% dplyr::select(!!!selectvars) %>%
    dplyr::select("[who] of end1", "[who] of end2", dplyr::everything())

  ## Prepare turtle data:
  nl.turtles <- nl.unnest %>% dplyr::filter(agent == "turtles")
  ## Check if [who] exist:
  if (!"who" %in% colnames(nl.turtles)) {
    stop("igraph needs who numbers of turtles.
         Please add '[who]' to metrics.turtles")
  }
  ## Select turtle variables
  selectvars <- c(unique(unlist(getexp(nl, "metrics.turtles"))),
                  "agent", "breed", "siminputrow", "[step]", "random-seed")
  nl.turtles <- nl.turtles %>% dplyr::select(!!!selectvars) %>%
    dplyr::select("who", dplyr::everything())

  ### Combine both:

 nl.graph <- purrr::map_dfr(unique(nl.links$`random-seed`), function(s) {
    purrr::map_dfr(unique(nl.links$siminputrow), function(r) {
      purrr::map_dfr(unique(nl.links$`[step]`), function(t) {

        nl.links.rst <- nl.links %>% dplyr::filter(`random-seed` == s & siminputrow == r & `[step]` == t)
        nl.turtles.rst <- nl.turtles %>% dplyr::filter(`random-seed` == s & siminputrow == r & `[step]` == t)

        nl.graph <- tibble::tibble(`random-seed` = s,
                                   siminputrow = r,
                                   `[step]` = t,
                                   spatial.links = list(igraph::graph_from_data_frame(nl.links.rst, vertices = nl.turtles.rst)))
      })
    })
  })


  nl.graph <- getsim(nl, "simoutput") %>% dplyr::left_join(nl.graph, by = c("random-seed", "[step]", "siminputrow"))

  return(nl.graph)
}
