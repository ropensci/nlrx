#' nlrx: A package for running NetLogo simulations from R.
#'
#' The nlrx package provides tools to setup NetLogo simulations in R.
#' It uses a similar structure as NetLogos Behavior Space but offers more flexibility and additional tools for running sensitivity analyses.
#'
#' @docType package
#' @name nlrx-package
#' @author Jan Salecker \email{jsaleck@gwdg.de}
#' @keywords package
"_PACKAGE"

globalVariables(c(".",
                  "[run number]",
                  "[step]",
                  "funs",
                  "group_by",
                  "metrics",
                  "random-seed",
                  "vars"))


