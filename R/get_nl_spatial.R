

get_nl_spatial <- function(nl,
                           turtles = TRUE,
                           patches = TRUE,
                           turtle_coords = "px"){

  if (!isTRUE(turtles)) {
    turtles <- tibble(id = seq(1, nrow(getsim(nl, "simoutput"))), turtles = rep(NA, nrow(getsim(nl, "simoutput"))))
  }

  if (!isTRUE(patches)) {
    patches <- tibble(id = seq(1, nrow(getsim(nl, "simoutput"))), patches = rep(NA, nrow(getsim(nl, "simoutput"))))
  }

  if (all(!is.na(getexp(nl, "metrics.patches"))) && isTRUE(patches)) {

    patches <-  purrr::map(seq_along(getsim(nl, "simoutput")$metrics.patches), function(raster_ind){

      patches_raster <- raster::rasterFromXYZ(getsim(nl, "simoutput")$metrics.patches[[raster_ind]])
      names(patches_raster) <- paste("S", getsim(nl, "simoutput")[raster_ind, "random-seed"],"_R", getsim(nl, "simoutput")[raster_ind, "siminputrow"], sep = "")
      return(patches_raster)
    })

    patches <- tibble::enframe(patches, "id", "patches")
    patches$step <- getsim(nl, "simoutput")$`[step]`
    patches$siminputrow <- getsim(nl, "simoutput")$siminputrow
    patches$`random-seed` <- getsim(nl, "simoutput")$`random-seed`
  }

  if (all(!is.na(getexp(nl, "metrics.turtles"))) && isTRUE(turtles)) {

    turtles <-  purrr::map(seq_along(getsim(nl, "simoutput")$metrics.turtles), function(turtles_ind){

      if (turtle_coords == "px") {
        coord_ind <- grepl(c("pxcor|pycor"), names(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]]))
      }

      if (turtle_coords == "x") {
        coord_ind <- grepl(c("xcor|ycor"), names(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]]), fixed = TRUE)
      }

      turtles <- getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]] %>%
        mutate_at(which(coord_ind == TRUE), function(x) as.numeric(as.character(x))) %>%
        as.tibble %>%
        sf::st_as_sf(., coords = which(coord_ind == TRUE))

      # if (turtle_coords == "both") {
      #   if (turtle_coords == "px") {
      #     coord_ind_px <- grepl(c("pxcor|pycor"), names(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]]))
      #   }
      #
      #   if (turtle_coords == "x") {
      #     coord_ind_x <- grepl(c("xcor|ycor"), names(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]]), fixed = TRUE)
      #   }
      #
      #   turtles <- list("px" = sf::st_as_sf(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]], coords = which(coord_ind_px == TRUE)),
      #                   "x" = sf::st_as_sf(getsim(nl, "simoutput")$metrics.turtles[[turtles_ind]], coords = which(coord_ind_x == TRUE)))
      #
      #       }

      return(turtles)
    })

    turtles <- tibble::enframe(turtles, "id", "turtles")
    turtles$step <- getsim(nl, "simoutput")$`[step]`
    turtles$siminputrow <- getsim(nl, "simoutput")$siminputrow
    turtles$`random-seed` <- getsim(nl, "simoutput")$`random-seed`
  }

  nl_join <- dplyr::left_join(patches, turtles)

  return(nl_join)
}



