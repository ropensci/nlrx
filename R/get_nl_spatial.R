#' Get spatial data from metrics.turtles and metrics.patches output
#'
#' @description Turn results from NetLogo in spatial data objects
#'
#' @param nl nl object
#' @param turtles if TRUE, the function generates spatial point objects (sf)
#' from metrics.turtles data
#' @param patches if TRUE, the function generates raster objects from
#' metrics.patches data
#' @param turtle_coords either "px" if turtle coordinates were measured as
#' "pxcor" and "pycor" or "x" if coordinates were measured as "xcor" and "ycor"
#' @param format string indication whether to return spatial objects
#' (RasterLayer, sf Points) or a rowbinded tibble
#' @return tibble with spatial data objects
#' @details
#'
#' get_nl_spatial generates spatial point objects and raster objects from data
#' that has been collected by metrics.turtles and metrics.patches.
#' metrics.turtles and metrics.patches need to collect coordinates of turtles
#' and patches.
#' For patches this can be easily done by adding "pxcor" and "pycor" to
#' metrics.patches.
#' For turtles you can either add "pxcor" and "pycor" to metrics.turtles or
#' "xcor" and "ycor".
#' It is also possible to measure both coordinates, and select the type that is
#'  used for spatial object creation through the function parameter
#'  turtle_coords.
#' "px" uses "pxcor" and "pycor", while "x" uses "xcor" and "ycor".
#'
#' @examples
#' \dontrun{
#'
#' # Run parallel on local machine:
#' future::plan(multisession)
#' # Run simulations:
#' results %<-% run_nl_all(nl = nl, cleanup = "all")
#' # Attach results to nl:
#' setsim(nl, "simoutput") <- results
#' # Get spatial data:
#' results_spatial <- get_nl_spatial(nl)
#' }
#' @aliases get_nl_spatial
#' @rdname get_nl_spatial
#'
#' @export

get_nl_spatial <- function(nl,
                           turtles = TRUE,
                           patches = TRUE,
                           turtle_coords = "px",
                           format = "spatial") {


  ## Check if results have been attached:
  if (purrr::is_empty(getsim(nl, "simoutput"))) {
    stop(
      "In order to run get_nl_spatial, output results have to be attached to
         the simdesign of the nl object first: setsim(nl, \"simoutput\")
         <- results"
    )
  }

  ## If no turtles shall be returned, create empty tibble
  if (!isTRUE(turtles)) {
    turtles_tib <- tibble::tibble(
      id = seq(1, nrow(getsim(nl, "simoutput"))),
      turtle.dat = rep(NA, nrow(getsim(nl, "simoutput")))
    )
  } else {

    turtles_tib <- getsim(nl, "simoutput") %>%
      dplyr::select(-metrics.patches) %>%
      tidyr::unnest(metrics.turtles)

  }

  ## If no patches shall be returned, create empty tibble
  if (!isTRUE(patches)) {
    patches_tib <- tibble::tibble(
      id = seq(1, nrow(getsim(nl, "simoutput"))),
      patch.dat = rep(NA, nrow(getsim(nl, "simoutput")))
    )
  } else {

    patches_tib <- getsim(nl, "simoutput") %>%
      dplyr::select(-metrics.turtles) %>%
      tidyr::unnest(metrics.patches)

  }

  if(format == "spatial"){

    if (isTRUE(turtles)) {

      if (turtle_coords == "px") {
        coord_ind <-
          grepl(c("\\bpxcor\\b|\\bpycor\\b"),
                names(getsim(nl,
                             "simoutput")$metrics.turtles[[1]]))
      }

      if (turtle_coords == "x") {
        coord_ind <-
          grepl(c("\\bxcor\\b|\\bycor\\b"),
                names(getsim(nl, "simoutput")$metrics.turtles[[1]]))
      }

      turtles <- getsim(nl, "simoutput") %>%
        dplyr::select(-metrics.patches) %>%
        dplyr::pull(metrics.turtles)

      turtles_tib <- getsim(nl, "simoutput") %>%
        dplyr::select(-metrics.patches) %>%
        dplyr::mutate(metrics.turtles = purrr::map(seq_along(turtles), function(x){

          turtles_ind <- turtles[[x]]

          if(anyNA(turtles_ind[ , which(coord_ind == TRUE)])){
            NA
          } else {
            sf::st_as_sf(turtles_ind, coords = which(coord_ind == TRUE))
          }


        }))

    }

    if (isTRUE(patches)) {

      ## grab x coords
      x_coord_ind <- grepl(
        c("pxcor"),
        names(getsim(nl, "simoutput")$metrics.patches[[1]])
      )
      x_coord_ind <- which(x_coord_ind == TRUE)

      ## grab y coords
      y_coord_ind <- grepl(
        c("pycor"),
        names(getsim(nl, "simoutput")$metrics.patches[[1]])
      )
      y_coord_ind <- which(y_coord_ind == TRUE)

      patches_own <-
        which(seq_len(ncol(getsim(nl, "simoutput")$metrics.patches[[1]])) %in%
                c(x_coord_ind, y_coord_ind) == FALSE)

      patches <- getsim(nl, "simoutput") %>%
        dplyr::select(-metrics.turtles) %>%
        dplyr::pull(metrics.patches)

      patches_tib <- getsim(nl, "simoutput") %>%
        dplyr::select(-metrics.turtles) %>%
        dplyr::mutate(metrics.patches = purrr::map(seq_along(patches), function(x){

          patches_ind <- patches[[x]]
          raster::rasterFromXYZ(patches_ind[, c(x_coord_ind,
                                                y_coord_ind,
                                                patches_own
          )
          ]
          )
        }
        )
        )

    }


  }

  dplyr::left_join(patches_tib, turtles_tib)



}
