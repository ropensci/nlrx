#' Get spatial data from metrics.turtles and metrics.patches output
#'
#' @description Turn results from NetLogo in spatial data objects
#'
#' @param nl nl object
#' @param coords nl object
#'
#' @return tibble with spatial data objects
#' @details
#' Unnests output from run_nl into long format.
#'
#' @examples
#'
#' @aliases nl_to_raster
#' @rdname nl_to_raster
#'
#' @export

nl_to_raster <- function(nl){

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
