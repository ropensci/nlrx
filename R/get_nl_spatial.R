#' Get spatial data from metrics.turtles and metrics.patches output
#'
#' @description Turn results from NetLogo in spatial data objects
#'
#' @param nl nl object
#' @param turtles if TRUE, the function generates spatial point objects (sf)
#' from metrics.turtles data
#' @param patches if TRUE, the function generates raster objects from
#' metrics.patches data
#' @param links if TRUE, the function generates xxx
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
#' library(future)
#' plan(multisession)
#' # Run simulations:
#' results %<-% run_nl_all(nl = nl)
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
                           links   = FALSE,
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


  if (format == "tibble")
  {
    ## If no turtles shall be returned, create empty tibble
    if (isTRUE(turtles)) {
      if (isTRUE(patches)) {
        turtles_tib <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.patches)
      } else {
        turtles_tib <- getsim(nl, "simoutput")
      }
      turtles_tib <- turtles_tib %>%
        tidyr::unnest(metrics.turtles)

      agentdata <- turtles_tib
    }

    ## If no patches shall be returned, create empty tibble
    if (isTRUE(patches)) {
      if (isTRUE(turtles)) {
        patches_tib <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.turtles)
      } else {
        patches_tib <- getsim(nl, "simoutput")
      }
      patches_tib <- patches_tib %>%
        tidyr::unnest(metrics.patches) %>%
        dplyr::rename(patches_x = pxcor,
                      patches_y = pycor)

      agentdata <- patches_tib
    }

    if (isTRUE(links)) {
      if (isTRUE(turtles) && isTRUE(patches))
      {
        links_tib <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.turtles, -metrics.patches)
      } else if (isTRUE(turtles)) {
        links_tib <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.turtles)
      } else if (isTRUE(patches)) {
        links_tib <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.patches)
      } else {
        links_tib <- getsim(nl, "simoutput")
      }

      links_tib <- links_tib %>%
        tidyr::unnest(metrics.links) %>%
        dplyr::mutate(end1 = as.numeric(stringr::str_replace_all(end1, "[turtle()]", "")),
                      end2 = as.numeric(stringr::str_replace_all(end2, "[turtle()]", "")))

      agentdata <- links_tib

    }

    if (all(isTRUE(turtles) && isTRUE(patches)))
    {
      turtles_tib$group <- "turtles"
      patches_tib$group <- "patches"
      agentdata <- dplyr::full_join(patches_tib, turtles_tib)
    } else if (all(isTRUE(turtles) && isTRUE(links))) {
      turtles_tib$group <- "turtles"
      links_tib$group <- "links"
      agentdata <- dplyr::full_join(links_tib, turtles_tib)
    } else if (all(isTRUE(links) && isTRUE(patches))) {
      links_tib$group <- "links"
      patches_tib$group <- "patches"
      agentdata <- dplyr::full_join(patches_tib, turtles_tib)
    } else if (all(isTRUE(turtles) && isTRUE(patches) && isTRUE(links))) {
      turtles_tib$group <- "turtles"
      patches_tib$group <- "patches"
      links_tib$group <- "patches"
      agentdata <- dplyr::full_join(patches_tib, turtles_tib, links_tib)
    }
  }


  ## Spatial output
  if (format == "spatial") {
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

      if (isTRUE(patches))
      {
        turtles_dat <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.patches)
      } else {
        turtles_dat <- getsim(nl, "simoutput")
      }
      turtles_tib <- turtles_dat

      turtles_dat <- turtles_dat %>%
        dplyr::pull(metrics.turtles)

      turtles_tib <- turtles_tib %>%
        dplyr::mutate(metrics.turtles = purrr::map(seq_along(turtles_dat), function(x) {
          turtles_ind <- turtles_dat[[x]]

          if (anyNA(turtles_ind[, which(coord_ind == TRUE)])) {
            NA
          } else {
            sf::st_as_sf(turtles_ind, coords = which(coord_ind == TRUE))
          }


        }))

      agentdata <- turtles_tib

    }

    if (isTRUE(patches)) {
      ## grab x coords
      x_coord_ind <- grepl(c("pxcor"),
                           names(getsim(nl, "simoutput")$metrics.patches[[1]]))
      x_coord_ind <- which(x_coord_ind == TRUE)

      ## grab y coords
      y_coord_ind <- grepl(c("pycor"),
                           names(getsim(nl, "simoutput")$metrics.patches[[1]]))
      y_coord_ind <- which(y_coord_ind == TRUE)

      patches_own <-
        which(seq_len(ncol(
          getsim(nl, "simoutput")$metrics.patches[[1]]
        )) %in%
          c(x_coord_ind, y_coord_ind) == FALSE)

      if (isTRUE(turtles))
      {
        patches_dat <- getsim(nl, "simoutput") %>%
          dplyr::select(-metrics.turtles)
      } else {
        patches_dat <- getsim(nl, "simoutput")
      }

      patches_tib <- patches_dat

      patches_dat <- patches_dat %>%
        dplyr::pull(metrics.patches)

      patches_tib <- patches_tib %>%
        dplyr::mutate(metrics.patches = purrr::map(seq_along(patches_dat), function(x) {
          patches_ind <- patches_dat[[x]]
          raster::rasterFromXYZ(patches_ind[, c(x_coord_ind,
                                                y_coord_ind,
                                                patches_own)])
        }))

      agentdata <- patches_tib

    }

    if (all(isTRUE(turtles) && isTRUE(patches)))
    {
      agentdata <-  dplyr::left_join(patches_tib, turtles_tib)
    }

  }

  ## Rename:
  agentdata <- agentdata %>% dplyr::rename(step = `[step]`,
                                           runnumber = `[run number]`)


  return(agentdata)

  }
