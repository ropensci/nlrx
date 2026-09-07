#' Identify and report the current OS
#'
#' @description Identify and report the current OS
#'
#' @aliases util_get_os
#' @rdname util_get_os
#' @keywords internal
util_get_os <- function() {
  # nocov start
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  # nocov end
  } else if (.Platform$OS.type == "unix") {
    "unix"
  # nocov start
  } else {
    stop("Unknown OS")
  }
  # nocov end
}

#' Identify and report the current OS
#'
#' @description Identify and report the current OS
#'
#' @param input list with variables and value ranges
#' @param samples number of lhs samples
#' @param precision number of digits for the decimal fraction of parameter
#' values
#' @aliases util_create_lhs
#' @rdname util_create_lhs
#' @keywords internal
util_create_lhs <- function(input, samples, precision) {

  # create a random sample of input factor sets (Latin Hypercube Sampling)
  lhs.design <- lhs::randomLHS(samples, length(input))
  # transform the standardized random values to the real input value range
  # and apply the desired random distribution
  lhs.design <- lapply(seq(1, length(input)), function(i) {
    match.fun(input[[i]]$qfun)(lhs.design[, i], input[[i]]$min, input[[i]]$max)
  })
  names(lhs.design) <- names(input)
  lhs.final <- tibble::as_tibble(lhs.design)
  ## Precision:
  lhs.final <- round(lhs.final, digits = precision)

  return(lhs.final)
}


#' Generate a vector of random seeds
#'
#' @description Generate a vector of random seeds
#'
#' @param nseeds desired length of the random seeds vector
#' @aliases util_generate_seeds
#' @rdname util_generate_seeds
#' @keywords internal
util_generate_seeds <- function(nseeds) {

  ## possible NetLogo seed interval (only integers)
  nl.seed.min <- -2147483648
  nl.seed.max <- 2147483647
  ## Generate seeds
  seeds <- ceiling(stats::runif(nseeds, min=nl.seed.min, max=nl.seed.max))

  ## Check for duplicates and print warning:
  if(length(unique(seeds)) < length(seeds))
  {
    warning("The generated seed vector (nl@simdesign@simseeds) contains duplicates. This may be desired (true random numbers) or not and it depends on the specific use case if this may be a problem for your experiment. You can either repeat attaching a simdesign to generate a new vector of random seeds or replace the simseeds vector with your own vector of random seeds!")
  }
  return(seeds)
}


#' Derive a vector of replicate seeds from a single design seed
#'
#' @description Derive a reproducible vector of random seeds for replicated model runs
#'
#' @param seed the design seed the replicate seeds are derived from
#' @param nreplicates number of replicate seeds to derive
#' @details
#' The derived seeds are a deterministic function of \code{seed} and therefore do not need to be
#' stored: the same \code{seed} always yields the same replicate seeds.
#' Because the seeds are drawn from the head of one stream, the seed vector for a smaller
#' \code{nreplicates} is a prefix of the vector for a larger one.
#'
#' The random number generator state of the calling environment is saved and restored, so that
#' deriving replicate seeds does not interfere with the random number stream of the dynamic
#' simulation designs (\code{genalg} and \code{EasyABC} draw from it).
#' @return numeric vector of length \code{nreplicates}
#' @aliases util_generate_replicate_seeds
#' @rdname util_generate_replicate_seeds
#' @keywords internal
util_generate_replicate_seeds <- function(seed, nreplicates) {

  if (length(seed) != 1 || is.na(seed) || nreplicates <= 1) {
    return(seed)
  }

  ## Store the current RNG state and restore it on exit:
  has_state <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (isTRUE(has_state)) {
    old_state <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (isTRUE(has_state)) {
      assign(".Random.seed", old_state, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  set.seed(seed)
  util_generate_seeds(nreplicates)
}


#' Reduce replicated simulation results to one value per metric
#'
#' @description Aggregate simulation results over measured ticks and over replicates
#'
#' @param results tibble of simulation results, as returned by \code{run_nl_one()}
#' @param cols character vector of metric columns that should be reduced
#' @details
#' Reduction happens in two stages: first the mean over all measured ticks within each replicate,
#' then the mean over the replicates.
#' Replicates are identified by the \code{random-seed} column.
#' The two stages matter whenever replicates return a different number of ticks (for example when
#' an exit condition is defined), because a single mean over all rows would weight long runs
#' higher than short ones.
#' @return tibble with one row and one column per entry of \code{cols}
#' @aliases util_reduce_replicates
#' @rdname util_reduce_replicates
#' @keywords internal
util_reduce_replicates <- function(results, cols) {

  tick_mean <- function(x) mean(as.numeric(x))

  ## Stage one: mean over ticks, within each replicate:
  if ("random-seed" %in% names(results)) {
    results <- results %>%
      dplyr::group_by(dplyr::across(dplyr::all_of("random-seed"))) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(cols), tick_mean), .groups = "drop") %>%
      dplyr::select(-"random-seed")
  }

  ## Stage two: mean over replicates (or over ticks, if replicates are not identifiable):
  dplyr::summarise(results, dplyr::across(dplyr::all_of(cols), tick_mean))
}


#' Utility function for checking for deprecated or unsupported arguments
#'
#' @param dots Named list of arguments captured from \code{...}
#' @param deprecated_args Character vector of deprecated argument names that should trigger depreciation warnings (and not errors)
#' @keywords internal

util_check_deprecated_args <- function(dots, deprecated_args) {

  # Check if there is something to check
  if (length(dots) == 0) {
    return(invisible(NULL))
  }

  # Warn when depreciated arguments are detected
  deprecated_found <- intersect(names(dots), deprecated_args)

  if (length(deprecated_found) > 0) {
    for (arg in deprecated_found) {
      warning(
        "Argument '", arg, "' is deprecated.",
        call. = FALSE
      )
    }
  }

  # Error when unknown arguments are detected
  unsupported_found <- setdiff(names(dots), deprecated_args)

  if (length(unsupported_found) > 0) {
    stop(
      "Unsupported argument(s): ",
      paste("'", unsupported_found, "'", sep = "", collapse = ", "),
      call. = FALSE
    )
  }

  return(invisible(NULL))
}

