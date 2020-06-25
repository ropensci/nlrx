#' Evaluate input/output integrity
#'
#' @description Evaluate input/output integrity
#' @param nl nl object with attached simulation output
#' @details
#' This function checks if the attached simulation output in the simoutput slot of the simdesign,
#' corresponds to the defined siminput matrix.
#'
#' Warning messages are thrown if data is missing in the simoutput tibble.
#' Additionally, missing combinations of siminputrow and random seed for which no data was found can be reported as tibble.
#' Such a tibble can then be used directly to rerun missing combinations conveniently (see examples below)
#'
#' @examples
#' \dontrun{
#' # Check eval_simoutput for testdata nl_lhs:
#' nl <- nl_lhs
#' eval_simoutput(nl)
#'
#' # Now remove one row of simoutput and check output:
#' nl <- nl_lhs
#' nl@@simdesign@@simoutput <- nl@@simdesign@@simoutput[-1,]
#' check <- eval_simoutput(nl)
#' check
#'
#' # Rerun missing combinations within check tibble:
#' rerun <- purrr::map_dfr(seq(nrow(check)), function(x) {
#'   res <- run_nl_one(nl, siminputrow=check$siminputrow[x], seed=check$seed[x])
#'     return(res)
#'     }) %>%
#'       dplyr::bind_rows(., nl@@simdesign@@simoutput)
#'
#'
#' }
#'
#' @aliases eval_simoutput
#' @rdname eval_simoutput
#' @export
eval_simoutput <- function(nl) {

  ## Check if siminput and simoutput are present
  if (purrr::is_empty(getsim(nl, "siminput")) | purrr::is_empty(getsim(nl, "simoutput"))) {
    stop("eval_simoutput can be executed only for nl objects with attached simdesign containing a siminput tibble (i.e. not the case for optimization simdesigns) and attached simoutput (simulation results)!")
  }

  ## Create a tibble with missing combinations:
  computed.combinations <- getsim(nl, "simoutput") %>%
    dplyr::select(`random-seed`, siminputrow) %>%
    dplyr::distinct(`random-seed`, siminputrow)
  ## Create a tibble with all possible combinations:
  all.combinations <- expand.grid(seed=getsim(nl, "simseeds"),
                                  siminputrow=seq(1, nrow(getsim(nl, "siminput"))))

  # Find missing combinations:
  missing.combinations <- all.combinations %>%
    dplyr::filter(!paste0(seed, "_", siminputrow) %in% paste0(computed.combinations$`random-seed`, "_", computed.combinations$siminputrow))

  # Print summary:
  if(nrow(missing.combinations) > 0) {
    #print(paste0(nrow(missing.combinations), " missing siminputrow/random-seed combinations were detected. Check output of eval_simoutput for more details."))
  } else {
    #print("No missing combinations detected!")
  }

  return(missing.combinations)

}
