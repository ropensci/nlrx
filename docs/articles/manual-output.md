# Capturing NetLogo output manually

## Capturing output manually

While nlrx provides the `metrics`, `metrics.turtles`, `metrics.patches`
and `metrics.links` slots of the `exerpiment` class to capture global
and agent related output from NetLogo models, this might not be
sufficient under certain circumstances. For example, the `metrics` slot
cannot track and collect nested list output or similar complex data
structures such as NetLogo arrays or matrices. Another example would be
spatial output such as shapefiles or grids, generated with the GIS
extension. Additionally, you might already have implemented complex
routines in your model which write model output to disk.

The main question for this vignette to answer is: How can you link such
self-written output to your nlrx experiment. Here we show you one basic
example how this works. In this example we write ascii raster files
using the GIS extension in NetLogo. However, the same workflow can of
course be applied to other types of output, for example text files
written with `file-type` primitives or the csv extension.

#### Step 1: Creating an ID widget on your model interface

In order to link self-written output to our nlrx simulations within the
R session, we need to transfer the current nlrx siminputrow and seed to
our NetLogo model. That way, when executing
[`run_nl_all()`](https://docs.ropensci.org/nlrx/reference/run_nl_all.md),
within each simulation the NetLogo model exactly “knows” which parameter
row (siminputrow) is currently simulated and the corresponding random
seed. To transfer this information we just need to create a string input
widget on our model interface. In the example below, we created such a
widget at the bottom of the Wolf Sheep model called `nlrx_id`.

![](idrunnum_gui.PNG)

#### Step 2: Define idrunnum of the experiment

The idrunnum field of the experiment is a built-in feature that allows
to transfer the current experiment name, siminputrow and random seed to
a defined NetLogo gui parameter widget. In our case we want to use our
newly created `nlrx_id` string input field, so we just set
`idrunnum = nlrx_id`.

``` r
# Attach experiment
nl@experiment <- experiment(expname="wolf-sheep",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            idrunnum="nlrx_id",
                            runtime=50,
                            evalticks=seq(40,50),
                            metrics=c("count sheep"),
                            variables = list('initial-number-sheep' = list(min=50, max=150, qfun="qunif"),
                                             'initial-number-wolves' = list(min=50, max=150, qfun="qunif")),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))
```

#### Step 3: Use nlrx_id within NetLogo model to tag self-written output

We can now use the `nlrx_id` field within the NetLogo model to tag our
self-written output. One way to do this is to tag the filenames of
self-written output. Here is an example where raster grids are created
and written at the end of each model run:

``` netlogo
to write_final_landscape
  let filename (word nlrx_id "_" but-first (word (1000 + ticks)) ".asc")
  if (file-exists? filename) [file-delete filename]
  gis:store-dataset final_landscape filename  
end
```

Here, we first create our filename by using the string from the
`nlrx_id` field (which contains the current experiment name, siminputrow
and random seed divided by an underscore) and add the current tick
(containing leading zeros by adding 1000 to the tick count and removing
the first number afterwards).

Of course you could also use different approaches to utilize the
information within the `nlrx_id` field for tagging your output.

#### Step 4: Linking self-written output to nlrx collected output

Finally, we want to read in our self-written raster files, calculate
some landscape metrics and add those to the results table that we
received from the
[`run_nl_all()`](https://docs.ropensci.org/nlrx/reference/run_nl_all.md)
function.

We basically have two types of output now: The tibble from our model
executions, and a folder with self-written output (here defined as a
subfolder of our modelpath).

``` r
## Output from nlrx simulations:
results <- run_nl_all(nl)
## Self-writte output directory:
ascdir <- file.path(dirname(nl@modelpath), "output")
```

We can now loop over the files in that folder, read the content and use
the `strplit` function on the filename to identify the experiment name,
the siminputrow, the random seed and the tick count. As an example
application, we then calculate some landscapemetrics using the
`landscapemetrics` package. Finally we put everything into a results
tibble:

``` r
results.lsm <- purrr::map_dfr(list.files(ascdir, pattern = "asc", full.names = TRUE), function(x) {
  x.split <- strsplit(x, "_")[[1]]
  x.tick <- as.numeric(strsplit(x.split[[length(x.split)]], "\\.")[[1]][[1]])
  x.siminputrow <- as.numeric(x.split[[length(x.split) - 1]][[1]])
  x.seed <- as.numeric(x.split[[length(x.split) - 2]][[1]])
  x.raster <- raster(x)
  
  ## netlogo asc files use NaN as default nodata value in the asc file header
  ## this leads to problems when reading the raster because it sets ´zeros to NA
  ## here we set NAs back to zertos manually:
  x.raster <- reclassify(x.raster, cbind(NA, 0))
  
  ## Calculate landscape metrics:
  metrics <- c("lsm_l_ed", "lsm_l_shdi", "lsm_l_lsi", "lsm_l_lpi", "lsm_l_area_mn")
  x.metrics <- landscapemetrics::calculate_lsm(x.raster, what=metrics) %>% 
    dplyr::select(metric, value) %>% 
    tidyr::pivot_wider(names_from=metric, values_from = value)
  
  x.final <- tibble::tibble(siminputrow = x.siminputrow,
                            `[step]` = x.tick,
                            `random-seed` = x.seed)
  
  x.final <- cbind(x.final, x.metrics)
  return(x.final)
})
```

Now we have the same identifier columns in the nlrx reported output and
the self-written output tibble which allows us to join both tibbles
together:

``` r
## Combine results with lsm and store:
results <- results %>% left_join(results.lsm, by = c("siminputrow", "random-seed", "[step]"))

## Attach output to nl object:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "my_final_nl_object.rds"))
```
