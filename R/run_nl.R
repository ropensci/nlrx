#' Execute all NetLogo simulations from a nl object
#'
#' @description Execute all NetLogo simulations from a nl object with a defined experiment and simdesign
#'
#' @param nl nl object
#' @param block_size number of simulations bundled into one execution block (one NetLogo instance can only receive one block).
#' @param threads number of NetLogo threads used for execution (handled via NetLogo).
#' @param ... additional arguments; currently only used to detect and warn about arguments that were deprecated in earlier nlrx versions.
#' @return tibble with simulation output results
#' @details
#'
#' run_nl_all executes all simulations of the specified NetLogo model within the provided nl object.
#'
#' The function structures the simdesign table into blocks, sized via \code{block_size}.
#' Each block is passed to \code{logolink}, which writes a BehaviorSpace XML file and executes it.
#'
#' Multithreading is supported via NetLogo natively, and controlled using \code{threads}. See how many threads you can assign using \code{parallel::detectCores()}.
#'
#' Progress can be shown by wrapping run_nl_all() with \code{progressr} (see examples).
#' Progress updates on completion of one block.
#'
#' Small \code{block_size} results in frequent updates of simulation status, but causes simulations to take longer, as NetLogo has to restart after every block.
#' Large \code{block_size} results in infrequent updates but will generally be faster (although with rapidly diminishing returns).
#'
#' ### Debugging:
#' Make sure you can run your NetLogo model on your machine.
#' Make sure that Java is installed and available from the terminal (java -version).
#' Use \code{eval_variables_constants(nl)} to check the validity of model settings.
#' Lastly, the R console outputs XML filepaths. These XML files are \code{BehaviorSpace} definitions. Run these manually via NetLogo \code{BehaviorSpace} to diagnose issues with the experiment and simdesign setup.
#'
#' @section Reproducibility and Seeds:
#' Every simulation is executed with an explicit random seed, taken from the \code{nseeds} seeds of the attached simdesign (\code{nl@simdesign@simseeds}).
#' NetLogo is seeded with that value and reports it back, so each row of the results can be traced to the parameterisation (\code{siminputrow}) and the seed (\code{random-seed}) that produced it.
#' Re-running a design with the same seeds therefore reproduces the results exactly.
#' To replicate a parameterisation under different stochasticity, increase \code{nseeds} in the simdesign helper.
#'
#' @section Suppressing Messages:
#' Informational messages (e.g., XML file paths) are displayed using the \code{cli} package. To suppress these messages, wrap the function call with
#' \code{suppressMessages()}, e.g., \code{suppressMessages(run_nl_all(nl))}.
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from test data:
#' nl <- nl_lhs
#'
#' # Execute all simulations from an nl object with properly attached simdesign.
#' results <- run_nl_all(nl)
#'
#' # Run in parallel on local machine (NetLogo 7+):
#' library(progressr)
#' with_progress({ # progress bar
#' results <- run_nl_all(nl, threads = 10) # 10 threads
#' })
#'
#' # Adjust block size for performance tuning:
#' # Larger block_size means fewer NetLogo restarts (re-initialized once per block):
#' results <- run_nl_all(nl, block_size = 500, threads = 10)
#' }
#' @aliases run_nl_all
#' @rdname run_nl_all
#'
#' @export

run_nl_all <- function(nl,
                       block_size = 100,
                       threads = 1,
                       ...) {

  util_check_deprecated_args(
    dots = list(...),
    deprecated_args = c("split", "cleanup.csv", "cleanup.xml", "cleanup.bat", "writeRDS")
  )


  # Construct blocks of simulation definitions
  simulation_blocks_list <- create_simulation_blocks(nl, block_size)

  # Prepare simulation of blocks
  results_list <- vector("list", length(simulation_blocks_list)) # will store results of following loop
  p <- progressr::progressor(steps = length(simulation_blocks_list)) # prepare progress bar

  # Loop through simulation blocks. Threading occurs per block (via NetLogo), not across blocks.
  for (block_number in seq_along(simulation_blocks_list)) {
    p(paste0("block ", block_number, "/", length(simulation_blocks_list)))

    results_list[[block_number]] <- run_nl_block(
      nl = nl,
      block_df = simulation_blocks_list[[block_number]],
      block_number = block_number,
      threads = threads
    )
  }

  # Bind result-blocks into one and fix formatting issues.
  # nl is required for detecting formatting issues right now, it can be left out once logolink stops using janitor on column names
  nl_results <- merge_result_blocks(nl, results_list)

  # Catch issues with model not running, as often NetLogo silently fails and returns no results.
  if (nrow(nl_results) == 0 || all(vapply(results_list, is.null, logical(1)))) {
    warning(
      paste0(
        "No simulation results were returned. See ?run_nl_all for debugging guidance.\n",
        "NLRX will now run eval_variables_constants(nl) to check for easily detectable issues."
      ),
      call. = FALSE
    )

    eval_variables_constants(nl)

    stop("No simulation results returned.", call. = FALSE)
  }

  return(nl_results)
}




#' Backend function used by \code{run_nl_all()} and \code{run_nl_one()} to run simulations (NetLogo 7+)
#'
#' @description Creates a BehaviorSpace XML file for simulations defined in \code{block_df} and executes them via \code{logolink}
#'
#' @param nl See \code{run_nl_all()}
#' @param block_df Data frame of simulation definitions, with one row being one run
#' @param block_number Assigned by \code{run_nl_all()}, used for naming the XML file.
#' @return Tibble with simulation output results
#' @details
#' Runs a single block (\code{block_df}) by writing a single BehaviorSpace XML file and executing it via \code{logolink}.
#' @keywords internal

run_nl_block <- function(nl, block_df, block_number, threads) {
  stopifnot(length(threads) == 1, !is.na(threads), threads >= 1, threads == as.integer(threads))
  stopifnot(length(block_number) == 1, !is.na(block_number), block_number >= 0, block_number == as.integer(block_number))

  # 1. BEHAVIORSPACE DEFINITION PREPARATION
  # Handling NLRX features, ensuring correct formatting of settings and simulation input

  # Ensure boolean formatting
  for (col in names(block_df)) { # convert to logical, as Logolink expects logical
    if (is.character(block_df[[col]]) && all(block_df[[col]] %in% c("true", "false"))) { # all instances of "true" and "false"
      block_df[[col]] <- block_df[[col]] == "true" # "true" = TRUE, "false" = FALSE
    }
  }

  # Handle idrunnum (add it to the parameterizations if given)
  if (!is.na(nl@experiment@idrunnum)) {
    seed_part <- if ("random-seed" %in% names(block_df)) {
      block_df$`random-seed`
    } else {
      rep(NA, nrow(block_df))
    }

    block_df[[nl@experiment@idrunnum]] <- paste0(
      nl@experiment@expname, "_", seed_part, "_", block_df$siminputrow
    )
  }

  # Generate a NetLogo run_metrics_condition from 'evalticks'
  has_evalticks <- all(!is.na(nl@experiment@evalticks))

  if (has_evalticks) {
    run_metrics_condition <- paste0("member? ticks [",  paste(nl@experiment@evalticks, collapse = " "), "]")
  } else {
    run_metrics_condition <- NULL
  }

  # Make 'evalticks' and 'tickmetrics' adhere to legacy nlrx logic.
  # -> tickmetrics = 'true' in order for recording conditions to apply
  if (nl@experiment@tickmetrics != "true") {
    if (!is.null(run_metrics_condition)) {
      warning(
        "'evalticks' is ignored when 'tickmetrics' isn't 'true'.",
        call. = FALSE
      )
    }
    run_metrics_condition  <- NULL
  }

  # Only pass tickmetrics when it is the only recording rule defined
  # Otherwise it would override other recording conditions
  run_metrics_every_step <- nl@experiment@tickmetrics == "true" && is.null(run_metrics_condition)

  # Format the job configuration (NetLogo variables) in a way that can be passed as sub_experiments (nested list instead of df)
  # "siminputrow" column has to be left out, as its not a variable within NetLogo.
  # Every row of block_df is one NetLogo run, carrying its own 'random-seed' where one is defined.
  jobs_formatted <- lapply(seq_len(nrow(block_df)), function(i) {
    as.list(dplyr::select(block_df[i, , drop = FALSE], -siminputrow))
  })

  # 2. BEHAVIORSPACE FILE CREATION
  # Use Logolink to translate the simulation-parametersets (siminput) and NLRX user settings into a BehaviorSpace XML that can be ran by NetLogo:
  xml_path <- suppressWarnings(logolink::create_experiment(
    name = paste0("NLRX Experiment ", nl@experiment@expname, ". Job ID ", block_number),
    run_metrics_every_step = run_metrics_every_step,
    time_limit = nl@experiment@runtime,
    setup = if (!is.na(nl@experiment@idsetup)) nl@experiment@idsetup else NULL, # names of setup ..
    go = if (!is.na(nl@experiment@idgo)) nl@experiment@idgo else NULL, # .. and go buttons
    post_experiment = if (!is.na(nl@experiment@idfinal)) nl@experiment@idfinal else NULL,
    exit_condition = if (!is.na(nl@experiment@stopcond)) nl@experiment@stopcond else NULL,
    run_metrics_condition = run_metrics_condition, # when measurements are recorded
    metrics = util_collect_experiment_metrics(nl), # packages nl@experiment@metrics with nl@experiment@metrics.turtles/patches/links.
    constants = nl@experiment@constants,
    sub_experiments = jobs_formatted # specific parameter configurations
  ))

  cli::cli_alert_info("Model running via: {.file {xml_path}}")

  # 3. BEHAVIORSPACE FILE EXECUTION
  # Use logolink to execute the previously created XML file.
  res <- suppressWarnings(logolink::run_experiment(
    model_path = nl@modelpath,
    setup_file = xml_path,
    output = c("table"),
    other_arguments = c(paste0("--threads ", threads)), # multithreading
    tidy_output = TRUE
  ))

  results_final <- res$table

  # 4. BEHAVIORSPACE RESULTS FORMATTING

  # NetLogo returns the end-of-simulation record. Always.
  # Fix for evalticks:
  if (nl@experiment@tickmetrics == "true" && # ensure evalticks was actually applied
      all(!is.na(nl@experiment@evalticks))) { # ensure evalticks is defined
    results_final <- results_final |>
      dplyr::filter(step %in% nl@experiment@evalticks) # filter out any records not adhering to the rule.
  }

  # infer relation of resulting run_numbers and original siminputrow from block_df
  lookup <- block_df |>
    dplyr::mutate(run_number = dplyr::row_number())

  # random_seed is temporary, logolink will most likely give an option to always return as random-seed.
  seed_in_results <- any(c("random-seed", "random_seed") %in% names(results_final))
  lookup_cols <- c("run_number", "siminputrow")

  if (!seed_in_results && "random-seed" %in% names(lookup)) {
    # Only reached when the runs were executed without an explicit seed:
    # carry the column over so that downstream analysis finds it.
    lookup_cols <- c(lookup_cols, "random-seed")
  }

  lookup <- dplyr::select(lookup, dplyr::all_of(lookup_cols))

  # assign siminputrow to create results_final
  results_final <- results_final |>
    dplyr::mutate(run_number = as.integer(run_number)) |>
    dplyr::arrange(run_number) |>
    dplyr::left_join(lookup, by = "run_number")

  results_final <- util_clean_agent_metrics(results_final, nl, expect_cleaned_names = TRUE)

  return(results_final)
}


#' Backend function for \code{run_nl_all()} (NetLogo 7+)
#'
#' @description Splits \code{nl@simdesign@siminput} into blocks of size \code{block_size}, to be executed by \code{run_nl_block()}
#'
#' @param nl See \code{run_nl_all()}
#' @param block_size See \code{run_nl_all()}
#' @return A list of data frames, each representing one bundle of simulations
#' @keywords internal

create_simulation_blocks <- function(nl, block_size){
  siminput <- getsim(nl, "siminput")
  simseeds <- getsim(nl, "simseeds")

  # Ensure block_size is formatted correctly.
  stopifnot(length(block_size) == 1, !is.na(block_size), block_size >= 1, block_size == as.integer(block_size))

  # Construct blocks of simulation parameterizations ("jobs")
  siminput <- siminput |> dplyr::mutate(siminputrow = dplyr::row_number()) # required for mapping, later on.
  distinct_simulations <- siminput |> tidyr::expand_grid(`random-seed` = simseeds) # uses simseeds to construct fully fledged experiment definitions (1 row per simulation)

  required_blocks <- ceiling(seq_len(nrow(distinct_simulations)) / block_size) # determine number of job blocks
  simulation_blocks_list <- split(distinct_simulations, required_blocks) # split simulation-definitions into blocks.

  return(simulation_blocks_list)
}


#' Backend function for \code{run_nl_all()}
#'
#' @description Merges results produced by \code{run_nl_block()} into one tibble. Temporarily handles column name restoration until \code{logolink} supports preserving original column names.
#'
#' @param nl See \code{run_nl_all()}
#' @param results_list List of result tibbles returned by \code{run_nl_block()}
#' @return Tibble with simulation output results
#' @keywords internal

merge_result_blocks <- function(nl, results_list){
  nl_results <- dplyr::bind_rows(results_list)

  ## logolink (used in run_nl_block() ) renames outputs, but NLRX expects the original names downstream
  # rename back via explicit name mappings
  # logolink will offer a feature to preserve names, as of now this feature is not there, so it uses janitor::make_clean_names forcibly.
  # there are multiple of these sections (one before this, when metrics are merged, and one in run_nl_one_logolink.
  # -> ctrl + shift + f -> "janitor::make_clean_names" to find all instances.

  # mimic logolinks way of renaming stuff:
  metric_names <- nl@experiment@metrics
  metric_names_clean <- janitor::make_clean_names(metric_names)

  param_names <- names(nl@experiment@variables)
  param_names_clean <- janitor::make_clean_names(param_names)

  constant_names <- names(nl@experiment@constants)
  constant_names_clean <- janitor::make_clean_names(constant_names)

  # create rename map + manually rename run number, step, and random-seed
  rename_map <- c(
    stats::setNames(metric_names_clean, metric_names),
    stats::setNames(param_names_clean, param_names),
    stats::setNames(constant_names_clean, constant_names),
    c("[run number]" = "run_number", "[step]" = "step", "random-seed" = "random_seed")
  )

  # apply renames
  rename_map <- rename_map[unname(rename_map) %in% names(nl_results)]
  nl_results <- dplyr::rename(nl_results, !!!rename_map)

  return(nl_results)
}


#' Execute one NetLogo simulation from a nl object
#'
#' @description Execute one NetLogo simulation from a nl object with a defined experiment and simdesign
#'
#' @param nl nl object
#' @param seed a random seed for the NetLogo simulation, or a vector of random seeds to run the same parameterisation repeatedly
#' @param threads number of NetLogo threads used for execution (handled via NetLogo).
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @param ... additional arguments; currently only used to detect and warn about arguments that were deprecated in earlier nlrx versions.
#' @return tibble with simulation output results
#' @details
#' Uses the \code{logolink} backend for NetLogo 7+.
#' \code{run_nl_one()} executes one simulation of the specified NetLogo model within the provided nl object.
#' The random seed is set within the NetLogo model to control stochasticity. For further information, see the Reproducibility section in \code{run_nl_all()}.
#' The siminputrow number defines which row of the input data tibble within the simdesign object of the provided nl object is executed.
#'
#' If \code{seed} is a vector of more than one seed, the same parameterisation is executed once per seed.
#' These replicated runs are independent of each other, and each of them reports its own seed in the \code{random-seed} column of the results.
#'
#' The \code{threads} argument controls NetLogo's native multithreading.
#' This is relevant when more than one seed is executed, because the runs are then parallelized within one NetLogo instance.
#' For a single run, \code{threads} has no effect.
#'
#' @section Suppressing Messages:
#' Informational messages (e.g., XML file paths) are displayed using the \code{cli} package. To suppress these messages, wrap the function call with
#' \code{suppressMessages()}, e.g., \code{suppressMessages(run_nl_one(nl, 123, 1))}.
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object from test data:
#' nl <- nl_lhs
#'
#' # Run one simulation:
#' results <- run_nl_one(nl = nl,
#'                       seed = getsim(nl, "simseeds")[1],
#'                       siminputrow = 1)
#'
#' # Run the same parameterisation with three different seeds:
#' results <- run_nl_one(nl = nl,
#'                       seed = getsim(nl, "simseeds")[1:3],
#'                       siminputrow = 1)
#'
#' }
#' @aliases run_nl_one
#' @rdname run_nl_one
#'
#' @export

run_nl_one <- function(nl,
                       seed,
                       threads = 1,
                       siminputrow,
                       ...) {

  util_check_deprecated_args(
    dots = list(...),
    deprecated_args = c("cleanup.csv", "cleanup.xml", "cleanup.bat", "writeRDS")
  )

  # Get the parameterization
  block_df <- getsim(nl, "siminput")[siminputrow, , drop = FALSE]

  # Wrap the single parameterization as a block of size 1
  block_df$siminputrow <- siminputrow

  # Add the seed(s). One row per seed, so that each replicate is a NetLogo run of its own.
  seed <- seed[!is.na(seed)]
  if (length(seed) > 0) {
    block_df <- block_df[rep(1L, length(seed)), , drop = FALSE]
    block_df$`random-seed` <- seed
  }

  # Run via Logolink
  result_block <- run_nl_block(
    nl = nl,
    block_df = block_df,
    block_number = 1,
    threads = threads
  )

  # Unpack and clean results
  nl_results <- merge_result_blocks(
    nl = nl,
    results_list = list(result_block)
  )

  return(nl_results)
}


#' Execute NetLogo simulation without pregenerated parametersets
#'
#' @description Execute NetLogo simulation from a nl object with a defined experiment and simdesign but no pregenerated input parametersets
#'
#' @param nl nl object
#' @param seed a random seed for the NetLogo simulation
#' @param threads number of NetLogo threads used for execution (handled via NetLogo).
#' @param nreplicates number of replicated model runs per evaluation of the dynamic design (default 1)
#' @param ... additional arguments; currently only used to detect and warn about arguments that were deprecated in earlier nlrx versions.
#' @return simulation output results can be tibble, list, ... (structure depends on simdesign method)
#' @details
#'
#' run_nl_dyn can be used for simdesigns where no predefined parametersets exist.
#' This is the case for dynamic designs, such as Simulated Annealing and Genetic Algorithms, where parametersets are dynamically generated, based on the output of previous simulations.
#'
#' Simulations are executed sequentially, one parameterization at a time, as each new parameterization depends on the results of the previous simulation.
#' Internally, each simulation step is executed via \code{run_nl_one()}.
#'
#' @section Replicated evaluations:
#' Dynamic designs evaluate a stochastic model, so every evaluation of the objective carries simulation noise.
#' With \code{nreplicates > 1}, each parameterisation proposed by the algorithm is simulated \code{nreplicates} times with different random seeds, and the reported value is aggregated over these replicates.
#' Aggregation happens in two steps: first the mean over all measured ticks within a replicate, then the mean over the replicates.
#' The replicated runs of one evaluation are executed within a single NetLogo instance, so \code{threads} can be used to run them in parallel.
#'
#' The replicate seeds are derived from \code{seed} and are identical for every evaluation of one \code{run_nl_dyn()} call.
#' Reusing the same seeds across evaluations (known as common random numbers) means that differences between two proposed parameterisations reflect the parameters rather than the random draw, which makes the objective easier for the algorithm to optimize.
#' It also means that the optimum is found for this particular set of random realisations; it is good practice to re-evaluate it with different seeds.
#'
#' Because the replicate seeds are derived from \code{seed}, they are not stored in the nl object.
#' A dynamic design is reproduced by the nl object together with \code{seed} and \code{nreplicates}, so both should be recorded alongside the results.
#'
#' @section Reproducibility and Seeds:
#' \code{seed} is used as the random seed of the NetLogo runs, and, with \code{nreplicates > 1}, as the basis from which the replicate seeds are derived.
#' The stochastic elements of the optimization algorithms themselves (for example the starting values of \code{GenSA} or the initial population of \code{GenAlg}) are drawn from the random number generator of R and are not controlled by \code{seed}; call \code{set.seed()} before \code{run_nl_dyn()} if these should be reproducible as well.
#' The \code{nseeds} argument of the dynamic simdesign helpers generates several seeds: running \code{run_nl_dyn()} once per seed yields independent repetitions of the whole optimization, which shows whether the algorithm converges to the same solution under different stochasticity.
#'
#' @section Suppressing Messages:
#' Informational messages (e.g., XML file paths) are displayed using the \code{cli} package. To suppress these messages, wrap the function call with
#' \code{suppressMessages()}, e.g., \code{suppressMessages(run_nl_dyn(nl))}.
#'
#' @examples
#' \dontrun{
#'
#' # Load nl object form test data:
#' nl <- nl_lhs
#'
#' # Add genalg simdesign:
#' nl@@simdesign <- simdesign_GenAlg(nl=nl,
#'                                   popSize = 200,
#'                                   iters = 100,
#'                                   evalcrit = 1,
#'                                   nseeds = 1)
#'
#' # Run simulations:
#' results <- run_nl_dyn(nl, seed = getsim(nl, "simseeds")[1])
#'
#' # Average each evaluation over 5 replicated model runs to reduce simulation noise:
#' results <- run_nl_dyn(nl, seed = getsim(nl, "simseeds")[1], nreplicates = 5)
#'
#' }
#' @aliases run_nl_dyn
#' @rdname run_nl_dyn
#'
#' @export

run_nl_dyn <- function(nl,
                       seed,
                       threads = 1,
                       nreplicates = 1,
                       ...) {

  util_check_deprecated_args(
    dots = list(...),
    deprecated_args = c("cleanup.csv", "cleanup.xml", "cleanup.bat")
  )

  if (length(nreplicates) != 1 || is.na(nreplicates) ||
      nreplicates < 1 || nreplicates != as.integer(nreplicates)) {
    stop("`nreplicates` must be a single positive integer.", call. = FALSE)
  }

  nl_results <- NULL

  if (getsim(nl, "simmethod") == "GenSA") {
    nl_results <- util_run_nl_dyn_GenSA(
      nl = nl,
      seed = seed,
      threads = threads,
      nreplicates = nreplicates
    )
  }

  if (getsim(nl, "simmethod") == "GenAlg") {
    nl_results <- util_run_nl_dyn_GenAlg(
      nl = nl,
      seed = seed,
      threads = threads,
      nreplicates = nreplicates
    )
  }

  if (getsim(nl, "simmethod") == "ABCmcmc") {
    nl_results <- util_run_nl_dyn_ABCmcmc(
      nl = nl,
      seed = seed,
      threads = threads,
      nreplicates = nreplicates
    )
  }


  return(nl_results)
}

