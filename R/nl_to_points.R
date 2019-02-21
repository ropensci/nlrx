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
#' @aliases nl_to_points
#' @rdname nl_to_points
#'
#' @export

nl_to_points <- function(nl, coords){

  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    stop("Simoutput tibble is empty.
            In order to generate spatial points from turtles metrics,
            output results have to be attached to the simdesign of the nl
            object first: setsim(nl, \"simoutput\") <- results")
  }


  turtles.metrics <- c()

  for (x in seq_along(nl@experiment@metrics.turtles)) {
    x.metrics <- nl@experiment@metrics.turtles[[x]]
    if (!"breed" %in% x.metrics) {
      x.metrics <- c("breed", x.metrics)
    }
    turtles.metrics <- c(turtles.metrics, x.metrics)
  }

  spatial.metrics <- c(unique(turtles.metrics),"siminputrow", "[step]", "random-seed")

  nl_output <- unnest_simoutput(nl)

  turtles <- nl_output %>% dplyr::filter(agent == "turtles")

  checkcoord1 <- sum(c("xcor", "ycor") %in% names(turtles))
  checkcoord2 <- sum(c("pxcor", "pycor") %in% names(turtles))

  if(nrow(turtles) == 0 & checkcoord1 == 0 ||checkcoord2 == 0) stop("You need to measure turtle coordinates to coerce model output into spatial points.")

  turtles_dat <- turtles %>%
    dplyr::select(spatial.metrics)

  if (coords == "px") {
    coord_ind <- grepl(c("\\bpxcor\\b|\\bpycor\\b"), names(turtles_dat))
  }

  if (coords == "x") {
    coord_ind <- grepl(c("\\bxcor\\b|\\bycor\\b"), names(turtles_dat))
  }

  turtles_dat <- sf::st_as_sf(turtles_dat, coords = which(coord_ind == TRUE))
  turtles_dat <- split(turtles_dat,  turtles[,c("siminputrow", "[step]", "random-seed")])


  turtles_ret <- getsim(nl, "simoutput")
  turtles_ret$spatial.turtles <- turtles_dat

  return(turtles_ret)
}
