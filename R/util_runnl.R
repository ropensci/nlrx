#' Create turtle/patches/links owns reporter
#'
#' @description The reporter is used to measure agent variables in NetLogo and parse them to a readable format
#'
#' @param metrics character vector, the names of variables to measure
#' @param breed character, the corresponding breed/agentset (e.g. "turtles", "patches", "links", "wolves", ...)
#' @aliases util_create_agentset_reporter
#' @rdname util_create_agentset_reporter
#' @keywords internal
util_create_agentset_reporter <- function(metrics, breed) {
  #reporter <- paste0("but-first but-last (word [remove \" \" (word ", paste(metrics, collapse = paste0("\",\"")), ")] of ", breed, ")")
  reporter <- paste0("but-first but-last (word [(word ", paste(metrics, collapse = paste0("\",\"")), ")] of ", breed, ")")
  return(reporter)
}




## Clean patch metrics
.util_clean_metrics_patches <- function(NLtable, nl) {

  patches_string <- NLtable[, grepl(c("metrics.patches"), names(NLtable))]  %>%
    dplyr::mutate_all(function(x) gsub('[\"]', '',x))

  # split by whitespace outside of brackets:
  patches_string <- gsub("\\[[^\\[\\]]*\\](*SKIP)(*F)|\\s+", ";;split;;", patches_string$metrics.patches, perl=TRUE)
  patches_string <- stringr::str_split(patches_string, ";;split;;")

  patches_string <- purrr::map(patches_string, function(x) {
    patches_owns <- tibble::as_tibble(x = x)
    patches_owns <- tidyr::separate(patches_owns, value,
                                    getexp(nl, "metrics.patches"), sep=",")
    patches_owns <- dplyr::mutate_all(patches_owns, function(x) {
      suppressWarnings(if(!all(is.na(x))) {ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x)))})
    })
    patches_owns$agent <- "patches"
    patches_owns$breed <- NA_character_
    return(patches_owns)
  })
  return(patches_string)
}


.util_clean_metrics_turtles <- function(NLtable, nl, col.name, metrics) {

  turtles_string <- NLtable[, grepl(col.name, names(NLtable))]  %>%
    dplyr::mutate_all(function(x) gsub('[\"]', '',x))

  # split by whitespace outside of brackets:
  turtles_string <- gsub("\\[[^\\[\\]]*\\](*SKIP)(*F)|\\s+", ";;split;;", dplyr::pull(turtles_string, col.name), perl=TRUE)
  turtles_string <- stringr::str_split(turtles_string, ";;split;;")

  turtles_string <- purrr::map(turtles_string, function(x) {
    turtles_owns <- tibble::as_tibble(x = x)
    turtles_owns <- tidyr::separate(turtles_owns,
                                    value,
                                    metrics,
                                    sep=",")
    turtles_owns <- dplyr::mutate_all(turtles_owns, function(x) {
      suppressWarnings(if(!all(is.na(x))) {ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x)))})
    })
    turtles_owns$agent <- "turtles"
    return(turtles_owns)
  })
  return(turtles_string)
}

# nocov start
.util_clean_metrics_links <- function(NLtable, nl, col.name, metrics) {

  links_string <- NLtable[, grepl(col.name, names(NLtable))]  %>%
    dplyr::mutate_all(function(x) gsub('[\"]', '',x))

  # split by whitespace outside of brackets:
  links_string <- gsub("\\[[^\\[\\]]*\\](*SKIP)(*F)|\\s+", ";;split;;", dplyr::pull(links_string, col.name), perl=TRUE)
  links_string <- stringr::str_split(links_string, ";;split;;")

  links_string <- purrr::map(links_string, function(x) {
    links_owns <- tibble::as_tibble(x = x)
    links_owns <- tidyr::separate(links_owns,
                                  value,
                                  metrics,
                                  sep=",")
    links_owns <- dplyr::mutate_all(links_owns, function(x) {
      suppressWarnings(if(!all(is.na(x))) {ifelse(is.na(as.numeric(as.character(x))),
                              as.character(x),
                              as.numeric(as.character(x)))})

    })
    links_owns$agent <- "links"
    return(links_owns)
  })
  return(links_string)
}
# nocov end




#' Backend function for collecting experiment metrics
#'
#' @description Internal helper for combining regular metrics with turtle, patch and link metrics.
#'
#' @param nl nl object
#' @return character vector of NetLogo reporter metrics
#' @details
#' Combines regular experiment metrics with additional agent metrics defined in
#' \code{metrics.turtles}, \code{metrics.patches} and \code{metrics.links}.
#' @keywords internal
util_collect_experiment_metrics <- function(nl) {
  metrics <- getexp(nl, "metrics")

  # Add turtle metrics if defined
  if (length(getexp(nl, "metrics.turtles")) > 0) {
    turtles_reporter <- purrr::map_chr(seq_along(nl@experiment@metrics.turtles), function(x) {
      x_breed <- names(nl@experiment@metrics.turtles)[[x]]
      x_metrics <- nl@experiment@metrics.turtles[[x]]

      if (!"breed" %in% x_metrics) {
        x_metrics <- c("breed", x_metrics)
      }

      util_create_agentset_reporter(x_metrics, x_breed)
    })

    metrics <- c(metrics, turtles_reporter)
  }

  # Add patch metrics if defined
  if (all(!is.na(getexp(nl, "metrics.patches")))) {
    patches_reporter <- util_create_agentset_reporter(
      getexp(nl, "metrics.patches"),
      "patches"
    )

    metrics <- c(metrics, patches_reporter)
  }

  # Add link metrics if defined
  if (length(getexp(nl, "metrics.links")) > 0) {
    links_reporter <- purrr::map_chr(seq_along(nl@experiment@metrics.links), function(x) {
      x_breed <- names(nl@experiment@metrics.links)[[x]]
      x_metrics <- nl@experiment@metrics.links[[x]]

      if (!"breed" %in% x_metrics) {
        x_metrics <- c("breed", x_metrics)
      }

      util_create_agentset_reporter(x_metrics, x_breed)
    })

    metrics <- c(metrics, links_reporter)
  }

  metrics
}




#' Backend function for cleaning agent metrics
#'
#' @description Internal helper for formatting turtle, patch and link metrics in simulation output.
#'
#' @param NLtable simulation output table
#' @param nl nl object
#' @param expect_cleaned_names TRUE/FALSE, if TRUE reporter column names are expected to be cleaned by \code{janitor::make_clean_names()}.
#' @return simulation output table with cleaned agent metrics
#' @details
#' Converts raw turtle, patch and link metric reporter output into nested output columns.
#' @keywords internal
util_clean_agent_metrics <- function(NLtable, nl, expect_cleaned_names = FALSE) {

  # Helper function used to identify the reporter column in the simulation output.
  # Logolink output currently applies janitor::make_clean_names(), so cleaned names
  # are checked optionally as well.
  find_reporter_col <- function(NLtable, reporter) {
    candidates <- if (expect_cleaned_names) {
      c(reporter, janitor::make_clean_names(reporter))
    } else {
      reporter
    }
    candidates[candidates %in% names(NLtable)][1]
  }

  # Clean turtle metrics if defined
  if (length(nl@experiment@metrics.turtles) > 0) {

    for (x in seq_along(nl@experiment@metrics.turtles)) {
      x.breed <- names(nl@experiment@metrics.turtles)[[x]]
      x.metrics <- nl@experiment@metrics.turtles[[x]]

      if (!"breed" %in% x.metrics) {
        x.metrics <- c("breed", x.metrics)
      }

      col.name <- paste0("metrics.", x.breed)
      turtles.reporter <- util_create_agentset_reporter(x.metrics, x.breed)
      reporter_col <- find_reporter_col(NLtable, turtles.reporter)

      if (is.na(reporter_col)) {
        warning(
          paste0("Could not find turtle metrics column for reporter: ", turtles.reporter),
          call. = FALSE
        )
        next
      }

      names(NLtable)[names(NLtable) == reporter_col] <- col.name

      NLtable[[col.name]] <- .util_clean_metrics_turtles(
        NLtable,
        nl,
        col.name,
        x.metrics
      )
    }
  }

  # Clean patch metrics if defined
  if (all(!is.na(getexp(nl, "metrics.patches")))) {

    col.name <- "metrics.patches"
    patches.reporter <- util_create_agentset_reporter(
      getexp(nl, "metrics.patches"),
      "patches"
    )

    reporter_col <- find_reporter_col(NLtable, patches.reporter)

    if (is.na(reporter_col)) {
      warning(
        paste0("Could not find patch metrics column for reporter: ", patches.reporter),
        call. = FALSE
      )
    } else {
      names(NLtable)[names(NLtable) == reporter_col] <- col.name

      NLtable[[col.name]] <- .util_clean_metrics_patches(
        NLtable,
        nl
      )
    }
  }

  # Clean link metrics if defined
  if (length(nl@experiment@metrics.links) > 0) {

    for (x in seq_along(nl@experiment@metrics.links)) {
      x.breed <- names(nl@experiment@metrics.links)[[x]]
      x.metrics <- nl@experiment@metrics.links[[x]]

      if (!"breed" %in% x.metrics) {
        x.metrics <- c("breed", x.metrics)
      }

      col.name <- paste0("metrics.", x.breed)
      links.reporter <- util_create_agentset_reporter(x.metrics, x.breed)
      reporter_col <- find_reporter_col(NLtable, links.reporter)

      if (is.na(reporter_col)) {
        warning(
          paste0("Could not find link metrics column for reporter: ", links.reporter),
          call. = FALSE
        )
        next
      }

      names(NLtable)[names(NLtable) == reporter_col] <- col.name

      NLtable[[col.name]] <- .util_clean_metrics_links(
        NLtable,
        nl,
        col.name,
        x.metrics
      )
    }
  }

  return(NLtable)
}
