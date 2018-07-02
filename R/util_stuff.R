#' Identify and report the current OS
#'
#' @description Identify and report the current OS
#'
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_get_os
#' @rdname util_get_os

util_get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

#' Identify and report the current OS
#'
#' @description Identify and report the current OS
#'
#' @param input list with variables and value ranges
#' @param samples number of lhs samples
#' @param precision number of digits for the decimal fraction of parameter values
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_create_lhs
#' @rdname util_create_lhs

util_create_lhs <- function(input, samples, precision) {

  # create a random sample of input factor sets (Latin Hypercube Sampling)
  lhs.design <- lhs::randomLHS(samples, length(input))
  # transform the standardized random values to the real input value range
  # and apply the desired random distribution
  lhs.design <- lapply(seq(1,length(input)), function(i) {
    match.fun(input[[i]]$qfun)(lhs.design[,i], input[[i]]$min, input[[i]]$max)
  })
  names(lhs.design) <- names(input)
  lhs.final <- tibble::as.tibble(lhs.design)
  ## Precision:
  lhs.final <- round(lhs.final, digits = precision)

  return(lhs.final)
}


#' Generate a vector of random seeds
#'
#' @description Generate a vector of random seeds
#'
#' @param nseeds desired length of the random seeds vector
#'
#' @details
#'
#' @examples
#' \dontrun{
#' }
#'
#' @aliases util_generate_seeds
#' @rdname util_generate_seeds

util_generate_seeds <- function(nseeds) {

  seeds <- ceiling(stats::runif(nseeds, 0, 10000))
  return(seeds)
}

