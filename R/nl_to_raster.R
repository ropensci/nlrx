#' Get spatial data from metrics.patches output
#'
#' @description Turn patch metrics from NetLogo in spatial data objects
#'
#' @param nl nl object
#'
#' @return tibble with spatial data objects
#' @details
#' Converts measured metrics.patches into spatial raster objects.
#' In order to so, a patch coordinates needs to be measured (pxcor/pycor).
#' For each additional patch metric, a raster will be created using the
#' measured coordinates as x and y and the additional metric as z dimension.
#' In case of multiple measured metrics, a ratser stack with one raster
#' for each metric will be reported.
#'
#' In order to use this function, simulation results need to be attached to
#' the nl object first.
#'
#' @examples
#'
#' # Attach simulation results:
#' setsim(nl, "simoutput") <- results
#' # Convert patch metrics to spatial raster objects:
#' results.raster <- nl_to_raster(nl)
#'
#'
#' @aliases nl_to_raster
#' @rdname nl_to_raster
#'
#' @export

nl_to_raster <- function(nl){

  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    stop("Simoutput tibble is empty.
            In order to generate raster objects from patch metrics,
            output results have to be attached to the simdesign of the nl
            object first: setsim(nl, \"simoutput\") <- results")
  }

  patch.metrics <- c(nl@experiment@metrics.patches,"siminputrow", "[step]", "random-seed")
  patch.owns    <- nl@experiment@metrics.patches[!(nl@experiment@metrics.patches %in% c("pxcor", "pycor"))]
  nl_output <- unnest_simoutput(nl)

  patches <- nl_output %>% dplyr::filter(agent == "patches")

  checkcoord <- sum(c("pxcor", "pycor") %in% names(patches))

  if(nrow(patches) == 0 | checkcoord == 0) stop("You need to measure turtle coordinates to coerce model output into spatial points.")

  patches_dat <- patches %>%
    dplyr::select(patch.metrics) %>%
    split(.,  patches[,c("siminputrow", "[step]", "random-seed")]) %>%
    purrr::map(., function(x){
      raster::rasterFromXYZ(x[, c("pxcor",
                                            "pycor",
                                            patch.owns)])
    })

  patches_ret <- getsim(nl, "simoutput")
  patches_ret$spatial.raster <- patches_dat

  return(patches_ret)

}
