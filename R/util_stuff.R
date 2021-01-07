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

