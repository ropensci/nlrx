#' Get spatial data from metrics.turtles and metrics.patches output
#'
#' @description Turn results from NetLogo in spatial data objects
#'
#' @param nl nl object
#'
#' @return tibble with spatial data objects
#' @details
#' Unnests output from run_nl into long format.
#'
#' @examples
#'
#' @aliases unnest_simoutput
#' @rdname unnest_simoutput
#'
#' @export

unnest_simoutput <- function(nl){

  if (length(nl@experiment@metrics.turtles) > 0) {
    tmet_exist <- TRUE
  } else {
    tmet_exist <- FALSE
    nl@simdesign@simoutput$metrics.turtles <- NA
  }

  if (length(nl@experiment@metrics.patches) > 0) {
    pmet_exist <- TRUE
  } else {
    pmet_exist <- FALSE
    nl@simdesign@simoutput$metrics.patches <- NA
  }

  if (length(nl@experiment@metrics.links) > 0) {
    lmet_exist <- TRUE
  } else {
    lmet_exist <- FALSE
    nl@simdesign@simoutput$metrics.links <- NA
  }


  # get turtles column names and metrics vector
  if (tmet_exist) {
    turtles.cols <- c()
    turtle.metrics <- c()

    for (x in seq_along(nl@experiment@metrics.turtles)) {
      x.breed <- names(nl@experiment@metrics.turtles)[[x]]
      x.metrics <- c("breed", nl@experiment@metrics.turtles[[x]])
      turtles.cols[x] <- paste0("metrics.", x.breed)
      turtle.metrics <- c(turtle.metrics, x.metrics)
    }
  } else {
    turtles.cols <- "metrics.turtles"
    turtle.metrics <- NA
  }
  # get patches column names and metrics vector
  patches.cols <- "metrics.patches"
  if (pmet_exist) {
    patch.metrics <- c("breed", nl@experiment@metrics.patches)
  } else{
    patch.metrics <- NA
  }

  # get links column names and metrics vector
  if (lmet_exist) {
    links.cols <- c()
    links.metrics <- c()

    for (x in seq_along(nl@experiment@metrics.links)) {
      x.breed <- names(nl@experiment@metrics.links)[[x]]
      x.metrics <- c("breed", nl@experiment@metrics.links[[x]])
      links.cols[x] <- paste0("metrics.", x.breed)
      links.metrics <- c(links.metrics, x.metrics)
    }
  } else {
    links.cols <- "metrics.links"
    links.metrics <- NA
  }

  agents.cols <- c(turtles.cols, patches.cols, links.cols)
  common_names <- names(getsim(nl, "simoutput"))[!(names(getsim(nl, "simoutput")) %in% agents.cols)]

  # select turtle data
  if (tmet_exist) {
    turtles_tib <- getsim(nl, "simoutput") %>%
      dplyr::select(-dplyr::one_of(patches.cols),-dplyr::one_of(links.cols))

    # unnest turtle data
    turtles_data <- list()

    for (x in seq_along(turtles.cols)) {
      turtles <- turtles_tib %>%
        dplyr::select(-dplyr::one_of(turtles.cols[-x])) %>%
        tidyr::unnest()

      turtles_data[[x]] <- turtles

    }

    not_unique <-
      turtle.metrics[stats::ave(seq_along(turtle.metrics), turtle.metrics, FUN = length) == 1]
    grps <-
      names(turtles_data[[1]])[!(names(turtles_data[[1]]) %in% not_unique)]

    # join turtle data
    turtles <- turtles_data %>% purrr::reduce(dplyr::full_join, by = grps)
  } else {
    turtles <- getsim(nl, "simoutput")[common_names]
    turtles <- turtles[0,]
  }

  # unnest patches data
  if (pmet_exist) {
    patches <- getsim(nl, "simoutput") %>%
      dplyr::select(-dplyr::one_of(turtles.cols),-dplyr::one_of(links.cols)) %>%
      tidyr::unnest()
  } else {
    patches <- getsim(nl, "simoutput")[common_names]
    patches <- patches[0,]
  }

  # select links data
  if (lmet_exist) {
    links_tib <- getsim(nl, "simoutput") %>%
      dplyr::select(-dplyr::one_of(patches.cols),-dplyr::one_of(turtles.cols))

    # unnest turtle data
    links_data <- list()

    for (x in seq_along(links.cols)) {
      links <- links_tib %>%
        dplyr::select(-dplyr::one_of(links.cols[-x])) %>%
        tidyr::unnest()

      links_data[[x]] <- links

    }

    not_unique <-
      links.metrics[stats::ave(seq_along(links.metrics), links.metrics, FUN = length) == 1]
    grps <-
      names(links_data[[1]])[!(names(links_data[[1]]) %in% not_unique)]

    # join turtle data
    links <- links_data %>% purrr::reduce(dplyr::full_join, by = grps)
  } else {
    links <- getsim(nl, "simoutput")[common_names]
    links <- links[0,]
  }

  # join turtles, patches and links
  agents <- suppressMessages(list(turtles, patches, links) %>% purrr::reduce(dplyr::full_join))

  return(agents)
}
