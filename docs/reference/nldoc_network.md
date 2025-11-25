# Create NetLogo procedure network

Create NetLogo procedure network

## Usage

``` r
nldoc_network(modelfiles)
```

## Arguments

- modelfiles:

  vector of filepaths to model files

## Value

network of model procedures (igraph)

## Details

Reads model code from the provided model files. The procedure identifies
NetLogo procedures and searches for procedure calls within the code.
From this data, an igraph network is created and returned. This network
can be used to plot the model procedure network and identify model
components.

## Examples

``` r
if (FALSE) { # \dontrun{

# List model files (.nls subfiles are also supported)
modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogo",
                "https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nls")

# Determine the function network:
nw <- nldoc_network(modelfiles)

# Determine communities within the network and plot using Igraph package:
library(igraph)
com <- walktrap.community(nw)
V(nw)$community <- com$membership
rain <- rainbow(14, alpha=.5)
V(nw)$color <- rain[V(nw)$community]

plot(nw,
     edge.arrow.size=.2,
     vertex.label.color="black",
     vertex.label.dist=1,
     vertex.size=5,
     edge.curved=0,
     vertex.label.cex=.5,
     layout=layout_with_fr(nw, niter = 2000))

# Interactive plot using igraph::tkplot
tkplot(nw, layout=layout_with_fr(nw, niter = 2000))
} # }
```
