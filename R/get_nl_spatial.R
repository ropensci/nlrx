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


  if (!isTRUE(turtles)) {
    turtles_tib <- tibble::tibble(
      id = seq(1, nrow(getsim(nl, "simoutput"))),
      turtle.dat = rep(NA, nrow(getsim(nl, "simoutput")))
    )
  }


  if (!isTRUE(patches)) {
    patches_tib <- tibble::tibble(
      id = seq(1, nrow(getsim(nl, "simoutput"))),
      patch.dat = rep(NA, nrow(getsim(nl, "simoutput")))
    )
  }

  if (all(!is.na(getexp(nl, "metrics.patches"))) &&
    isTRUE(patches)) {
    if (!all(any(getexp(nl, "metrics.patches") %in% c("pxcor")) &
      any(getexp(nl, "metrics.patches") %in% c("pycor")))) {
      stop(
        "get_nl_spatial needs pxcor and pycor for creating raster
           from patches. Please add pxcor and pycor to metrics.patches."
      )
    }

    x_coord_ind <- grepl(
      c("pxcor"),
      names(getsim(nl, "simoutput")$metrics.patches[[1]])
    )
    x_coord_ind <- which(x_coord_ind == TRUE)
    y_coord_ind <- grepl(
      c("pycor"),
      names(getsim(nl, "simoutput")$metrics.patches[[1]])
    )
    y_coord_ind <- which(y_coord_ind == TRUE)

    patches_own <-
      which(seq_len(ncol(getsim(nl, "simoutput")$metrics.patches[[1]])) %in%
        c(x_coord_ind, y_coord_ind) == FALSE)

    patches_own_names <-
      names(getsim(nl, "simoutput")$metrics.patches[[1]])[patches_own]

    patch.dat <-
      purrr::map(
        seq_along(getsim(nl, "simoutput")$metrics.patches),
        function(raster_ind) {
          patches_raster <-
            raster::rasterFromXYZ(getsim(nl,
                                         "simoutput")$metrics.patches[[
                                           raster_ind]][, c(
              x_coord_ind,
              y_coord_ind,
              patches_own
            )])
          patches_raster <- raster::flip(patches_raster, 2)
          names(patches_raster) <-
            purrr::map_chr(patches_own_names, function(name) {
              paste(
                "S",
                getsim(nl, "simoutput")[raster_ind, "random-seed"],
                "_R",
                getsim(nl, "simoutput")[raster_ind, "siminputrow"],
                "_N",
                name,
                sep = ""
              )
            })

          return(patches_raster)
        }
      )

    patches_tib <- tibble::enframe(patch.dat, "id", "patches")
    patches_tib$step <- getsim(nl, "simoutput")$`[step]`
    patches_tib$siminputrow <- getsim(nl, "simoutput")$siminputrow
    patches_tib$`random-seed` <-
      getsim(nl, "simoutput")$`random-seed`
  }

  if (all(!is.na(getexp(nl, "metrics.turtles"))) &&
    isTRUE(turtles)) {
    if (!all(any(getexp(nl, "metrics.turtles") %in% c("xcor", "pxcor")) &
      any(getexp(nl, "metrics.turtles") %in% c("ycor", "pycor")))) {
      stop(
        "get_nl_spatial needs pxcor/xcor and pycor/ycor for creating sf points
        from turtles. Please add pxcor/xcor and pycor/ycor to metrics.turtles."
      )
    }

    turtle.dat <-
      purrr::map(
        seq_along(getsim(nl, "simoutput")$metrics.turtles),
        function(turtles_ind) {
          if (turtle_coords == "px") {
            coord_ind <-
              grepl(c("\\bpxcor\\b|\\bpycor\\b"), names(getsim(nl,
                                          "simoutput")$metrics.turtles[[
                                            turtles_ind]]))
          }

          if (turtle_coords == "x") {
            coord_ind <-
              grepl(c("\\bxcor\\b|\\bycor\\b"),
                names(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]]))
          }

          turtle.dat <-
            getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]] %>%
            dplyr::mutate_at(which(coord_ind == TRUE), function(x)
              as.numeric(as.character(x))) %>%
            tibble::as.tibble() %>%
            sf::st_as_sf(., coords = which(coord_ind == TRUE))

          return(turtle.dat)
        }
      )

    turtles_tib <- tibble::enframe(turtle.dat, "id", "turtles")
    turtles_tib$step <- getsim(nl, "simoutput")$`[step]`
    turtles_tib$siminputrow <- getsim(nl, "simoutput")$siminputrow
    turtles_tib$`random-seed` <-
      getsim(nl, "simoutput")$`random-seed`
  }

  nl_join <- dplyr::left_join(patches_tib, turtles_tib)

  if (format == "tibble") {
    if (!is.data.frame(patch.dat)) {
      patch.dat <-
        dplyr::mutate(patches_tib,
          maps = purrr::map(patches_tib$patches, function(x) {
            # Create empty tibble with the same dimension as the raster ----
            grd <-
              tibble::as_tibble(expand.grid(
                x = seq(
                  ceiling(raster::extent(x)[1]),
                  floor(raster::extent(x)[2]),
                  raster::res(x)[1]
                ),
                y = seq(
                  ceiling(raster::extent(x)[3]),
                  floor(raster::extent(x)[4]),
                  raster::res(x)[2]
                )
              ))
            # Fill with raster values ----
            patches_own_tib <- as.data.frame(raster::values(x))
            names(patches_own_tib) <-
              names(getsim(nl, "simoutput")$metrics.patches[[1]])[patches_own]
            grd <- dplyr::bind_cols(grd, patches_own_tib)
          })
        ) %>%
        tidyr::unnest(maps) %>%
        dplyr::rename(
          patches_x = x,
          patches_y = y
        )
    }

    if(isTRUE(turtles)) {
      turtle.dat <- turtles_tib %>%
        tidyr::unnest(turtle.dat) %>%
        sf::st_as_sf()
      turtle.dat <-
        turtle.dat %>%
        sf::st_set_geometry(NULL) %>%
        cbind(sf::st_coordinates(turtle.dat)) %>%
        dplyr::rename(
          turtles_x = X,
          turtles_y = Y
        )
    }

    ## Bind tibbles:
    if (isTRUE(patches) && isTRUE(turtles))
    {
      patch.dat$group <- "patches"
      turtle.dat$group <- "turtles"
      nl_join <- dplyr::bind_rows(patch.dat, turtle.dat) %>%
        dplyr::select(group, dplyr::everything())
    }
    if (isTRUE(patches) && !isTRUE(turtles))
    {
      patch.dat$group <- "patches"
      nl_join <- dplyr::bind_rows(patch.dat) %>%
        dplyr::select(group, dplyr::everything())
    }
    if (!isTRUE(patches) && isTRUE(turtles))
    {
      turtle.dat$group <- "turtles"
      nl_join <- dplyr::bind_rows(turtle.dat) %>%
        dplyr::select(group, dplyr::everything())
    }

  }

  return(nl_join)
}
