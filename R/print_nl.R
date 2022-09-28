#' Print content of nl object
#'
#' @description Print content of nl object and embedded experiment and simdesign objects to console
#' @param x nl object to print
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of the provided nl object in a readable format.
#'
#' @examples
#'
#' print(nl_lhs)
#'
#' @aliases print.nl
#' @rdname print.nl
#'
#' @export
print.nl <- function(x, ...)
{
  util_print.nl(x)
  util_print.experiment(x@experiment)
  util_print.simdesign(x@simdesign)
  util_print.summary(x)
}

#' Print nl object content
#'
#' @description Print nl object content
#' @param x nl object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of nl object in a nice formatted overview to console
#' @aliases util_print.nl
#' @rdname util_print.nl
#' @keywords internal
util_print.nl <- function(x, ...) {

  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red


  cat(style_heading(paste0("\n", "   NL OBJECT   ", "\n")))

  cat("NetLogo version = ")
  output <- paste0(x@nlversion, "\n")
  cat(ifelse(nchar(x@nlversion) > 0, style_def(output), style_na(output)))


  cat("NetLogo path    = ")
  output <- paste0(x@nlpath, "\n")
  cat(ifelse(!identical(x@nlpath, character(0)), style_def(output), style_na(output)))


  cat("Model path      = ")
  output <- paste0(x@modelpath, "\n")
  cat(ifelse(!identical(x@modelpath, character(0)), style_def(output), style_na(output)))

  cat("JVM memory      = ")
  output <- paste0(x@jvmmem, "\n")
  cat(ifelse(!is.na(x@jvmmem), style_def(output), style_na(output)))

}

#' Print nl object summary
#'
#' @description Print nl object summary
#' @param x nl object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print summary of nl object and embedded experiment and simdesign objects in a nice formatted overview to console
#' @aliases util_print.summary
#' @rdname util_print.summary
#' @keywords internal
util_print.summary <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   SUMMARY   ", "\n")))

  cat("supported nlversion: ")
  output <- ifelse(check_netlogo_version(x@nlversion), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("nlpath exists on local system: ")
  output <- ifelse(dir.exists(x@nlpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("modelpath exists on local system: ")
  output <- ifelse(file.exists(x@modelpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("valid jvm memory: ")
  output <- ifelse(is.numeric(x@jvmmem), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("valid experiment name: ")
  output <- ifelse(is.na(x@experiment@expname) | grepl("\\s", getexp(x, "expname")), style_na("\u2717"), style_def("\u2713"))
  cat(paste0(output, "\n"))

  cat("outpath exists on local system: ")
  output <- ifelse(dir.exists(x@experiment@outpath), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("setup and go defined: ")
  output <- ifelse(!all(is.na(x@experiment@idsetup), is.na(x@experiment@idgo)), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))


  cat("variables defined: ")
  output <- ifelse(length(x@experiment@variables) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  if(!identical(x@modelpath, character(0))){
    if(file.exists(x@modelpath)){
      cat("variables present in model: ")
      output <- ifelse(length(x@experiment@variables) > 0 & all(names(x@experiment@variables) %in% names(report_model_parameters(x))),
                       style_def("\u2713"), style_na("\u2717"))
      cat(paste0(output, "\n"))
    }
  }

  cat("constants defined: ")
  output <- ifelse(length(x@experiment@constants) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  if(!identical(x@modelpath, character(0))){
    if(file.exists(x@modelpath)){
      cat("constants present in model: ")
      output <- ifelse(length(x@experiment@constants) > 0 & all(names(x@experiment@constants) %in% names(report_model_parameters(x))),
                       style_def("\u2713"), style_na("\u2717"))
      cat(paste0(output, "\n"))
    }
  }

  cat("metrics defined: ")
  output <- ifelse(length(x@experiment@metrics) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("spatial Metrics defined: ")
  output <- ifelse(length(x@experiment@metrics.turtles) > 0 | length(x@experiment@metrics.patches) > 0 | length(x@experiment@metrics.links) > 0,
                   style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))





  cat("simdesign attached: ")
  output <- ifelse(!is.na(x@simdesign@simmethod), style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("siminput parameter matrix: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("number of siminputrows: ")
  output <- ifelse(nrow(x@simdesign@siminput) > 0, style_def(nrow(x@simdesign@siminput)), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("number of random seeds: ")
  output <- ifelse(!all(is.na(x@simdesign@simseeds)), style_def(length(x@simdesign@simseeds)), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("estimated number of runs: ")
  output <- ifelse(!all(nrow(x@simdesign@siminput) == 0, is.na(x@simdesign@simseeds)),
                   style_def(nrow(x@simdesign@siminput) * length(x@simdesign@simseeds)),
                   style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("simoutput results attached: ")
  output <- ifelse(nrow(x@simdesign@simoutput) > 0, style_def("\u2713"), style_na("\u2717"))
  cat(paste0(output, "\n"))

  cat("number of runs calculated: ")
  output <- ifelse(nrow(x@simdesign@simoutput) > 0,
                   style_def(length(unique(paste(x@simdesign@simoutput$'random-seed', x@simdesign@simoutput$siminputrow)))),
                   style_na("\u2717"))
  cat(paste0(output, "\n"))

}

#' Print experiment object content
#'
#' @description Print experiment object content
#' @param x experiment object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of experiment object in a nice formatted overview to console
#' @aliases util_print.experiment
#' @rdname util_print.experiment
#' @keywords internal
util_print.experiment <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   EXPERIMENT   ", "\n")))

  cat("Experiment name        = ")
  output <- paste0(x@expname, "\n")
  cat(ifelse(!is.na(x@expname), style_def(output), style_na(output)))

  cat("Output path            = ")
  output <- paste0(x@outpath, "\n")
  cat(ifelse(!is.na(x@outpath), style_def(output), style_na(output)))

  cat("NetLogo repetitions    = ")
  output <- paste0(x@repetition, "\n")
  cat(ifelse(!is.na(x@repetition), style_def(output), style_na(output)))

  cat("Measure on each tick?  = ")
  output <- paste0(x@tickmetrics, "\n")
  cat(ifelse(!is.na(x@tickmetrics), style_def(output), style_na(output)))

  cat("Setup procedure(s)     = ")
  output <- paste0(paste(x@idsetup, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idsetup)), style_def(output), style_na(output)))

  cat("Go procedure(s)        = ")
  output <- paste0(paste(x@idgo, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idgo)), style_def(output), style_na(output)))

  cat("Final procedure(s)     = ")
  output <- paste0(paste(x@idfinal, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@idfinal)), style_def(output), style_opt(output)))

  cat("Run nr. widget name    = ")
  output <- paste0(x@idrunnum, "\n")
  cat(ifelse(!is.na(x@idrunnum), style_def(output), style_opt(output)))

  cat("Runtime (ticks)        = ")
  output <- paste0(x@runtime, "\n")
  cat(ifelse(!is.na(x@runtime), style_def(output), style_na(output)))

  cat("Report output on ticks = ")
  output <- paste0(paste(x@evalticks, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@evalticks)), style_def(output), style_opt(output)))

  cat("Stop condition         = ")
  output <- paste0(x@stopcond, "\n")
  cat(ifelse(!is.na(x@stopcond), style_def(output), style_opt(output)))

  cat("Metrics (output)       = ")
  output <- paste0(paste(x@metrics, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@metrics)), style_def(output), style_na(output)))

  cat(paste0("\n", "Turtle metrics (output)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@metrics.turtles)), paste(unlist(x@metrics.turtles), collapse = ", "), sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@metrics.turtles)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Patch metrics (output)", "\n"))
  output <- paste0("    ", paste(unlist(x@metrics.patches), collapse = ", "), "\n")
  cat(ifelse(!all(is.na(x@metrics.turtles)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Link metrics (output)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@metrics.links)), paste(unlist(x@metrics.links), collapse = ", "), sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@metrics.links)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Variable parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@variables)), x@variables, collapse="\n", sep=" = "), "\n")
  cat(ifelse(!all(is.na(x@variables)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Constant parameters (input)", "\n"))
  output <- paste0(paste(paste0("    ", names(x@constants)), x@constants, sep=" = ", collapse="\n"), "\n")
  cat(ifelse(!all(is.na(x@constants)), style_def(output), style_opt(output)))

}

#' Print simdesign object content
#'
#' @description Print simdesign object content
#' @param x simdesign object
#' @param ... further arguments passed to or from other methods
#' @details
#' Print content of simdesign object in a nice formatted overview to console
#' @aliases util_print.simdesign
#' @rdname util_print.simdesign
#' @keywords internal
util_print.simdesign <- function(x, ...)
{
  # Styles:
  style_heading <- crayon::black$bold$bgWhite
  style_def <- crayon::green
  style_opt <- crayon::yellow
  style_na <- crayon::red

  cat(style_heading(paste0("\n", "   SIMDESIGN   ", "\n")))

  cat("Simulation method      = ")
  output <- paste0(x@simmethod, "\n")
  cat(ifelse(!identical(x@simmethod, character(0)), style_def(output), style_na(output)))

  cat("Simulation object      = ")
  output <- paste0(x@simobject, "\n")
  cat(ifelse(length(x@simmethod) > 0, style_def(output), style_opt(output)))

  cat("Generated random seeds = ")
  output <- paste0(paste(x@simseeds, collapse=", "), "\n")
  cat(ifelse(!all(is.na(x@simseeds)), style_def(output), style_opt(output)))

  cat(paste0("\n", "Parameter matrix (input)", "\n"))
  print(x@siminput, width = Inf)

  cat(paste0("\n", "Simulation results (output)", "\n"))
  print(x@simoutput, width = Inf)

}


