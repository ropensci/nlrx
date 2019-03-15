#' Create NetLogo procedure network
#'
#' @description Create NetLogo procedure network
#'
#' @param modelfiles vector of filepaths to model files
#'
#' @return network of model procedures (igraph)
#'
#' @details
#'
#' Reads model code from the provided model files.
#' The procedure identifies NetLogo proecdures and searches for procedure calls within the code.
#' From this data, an igraph network is created and returned.
#' This network can be used to plot the model procedure network and identify model components.
#'
#' @examples
#' \donttest{
#'
#' # List model files (.nls subfiles are also supported)
#' modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogo",
#'                 "https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nls")
#'
#' # Determine the function network:
#' nw <- nldoc_network(modelfiles)
#'
#' # Determine communities within the network and plot using Igraph package:
#' library(igraph)
#' com <- walktrap.community(nw)
#' V(nw)$community <- com$membership
#' rain <- rainbow(14, alpha=.5)
#' V(nw)$color <- rain[V(nw)$community]
#'
#' plot(nw,
#'      edge.arrow.size=.2,
#'      vertex.label.color="black",
#'      vertex.label.dist=1,
#'      vertex.size=5,
#'      edge.curved=0,
#'      vertex.label.cex=.5,
#'      layout=layout_with_fr(nw, niter = 2000))
#'
#' # Interactive plot using igraph::tkplot
#' tkplot(nw, layout=layout_with_fr(nw, niter = 2000))
#' }
#' @aliases nldoc_network
#' @rdname nldoc_network
#'
#' @export

nldoc_network <- function(modelfiles)
{
  ## Create a network from procedures:

  ## Read code from provided netlogo files:
  nlogocode <- nldoc_read_nlogo(modelfiles)

  ## Now find these names in model code:
  nw <- nldoc_find_procedure_calls(nlogocode)

  ## Create and plot network:

  nw.ig <- igraph::graph_from_data_frame(nw)
  nw.ig <- igraph::simplify(nw.ig, remove.multiple = T, remove.loops = T)

  return(nw.ig)
}



