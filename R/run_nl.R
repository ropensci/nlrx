

#' Execute all NetLogo simulations from a nl object
#'
#' @description Execute all NetLogo simulations from a nl object with a defined experiment and simdesign
#'
#' @param nl nl object
#' @param block_size number of simulations bundled into one execution block (NetLogo 7+).
#' @param threads number of NetLogo threads used for execution (NetLogo 7+).
#' @param split number of parts the job should be split into (NetLogo < 7).
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results (NetLogo < 7).
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results (NetLogo < 7).
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results (NetLogo < 7).
#' @param writeRDS TRUE/FALSE, if TRUE, for each single simulation an rds file with the simulation results will be written to the defined outpath folder of the experiment within the nl object (NetLogo < 7).
#' @return tibble with simulation output results
#' @details
#'
#' run_nl_all executes all simulations of the specified NetLogo model within the provided nl object.
#'
#' ## Logolink mode (NetLogo >= 7.0)
#' Uses internal function \code{run_nl_all_logolink(nl, block_size, threads)}.
#' The function structures the simdesign table into blocks, sized via \code{block_size}.
#' Blocks are passed to the logolink package, which writes a Behaviorspace XML for the passed block. The XML file is then executed via logolink.
#' Multithreading is supported via NetLogo natively, and controlled using \code{threads}. See how many threads you can assign using \code{parallel::detectCores()}.
#' For HPC usage, custom functions can be built 🔵 (SEE HPC VIGNETTE - TODO)
#'
#' Progress can be shown by wrapping run_nl_all() with progressr (see examples).
#' Progress updates on completion of one block.
#'
#' Small \code{block_size results} in frequent updates of simulation status, but causes simulations to take longer, as NetLogo has to restart after every block.
#' Large \code{block_size results} results in infrequent updates but will generally be faster (although with rapidly diminishing returns).
#'
#' ### Debugging:
#' Make sure you can run your NetLogo model on your machine.
#' Make sure that Java is installed and available from the terminal (java -version).
#' Use eval_variables_constants(nl) to check the validity of model settings.
#' Lastly, the R console outputs XML filepaths. These XML files are Behaviorspace definitions. Run these manually via NetLogo Behaviorspace to exclude issues with the experiment and simdesign setup.
#'
#' ## Legacy mode (NetLogo < 7.0)
#' Uses internal function \code{run_nl_all_legacy(nl, split, cleanup.csv, cleanup.xml, cleanup.bat, writeRDS)}
#' The function loops over all random seeds and all rows of the siminput table of the simdesign of nl.
#' The loops are created by calling \link[furrr]{future_map_dfr}, which allows running the function either locally or on remote HPC machines.
#' The logical cleanup variables can be set to FALSE to preserve temporary generated output files (e.g. for debugging).
#' cleanup.csv deletes/keeps the temporary generated model output files from each run.
#' cleanup.xml deletes/keeps the temporary generated experiment xml files from each run.
#' cleanup.bat deletes/keeps the temporary generated batch/sh commandline files from each run.
#'
#' When using run_nl_all in a parallelized environment (e.g. by setting up a future plan using the future package),
#' the outer loop of this function (random seeds) creates jobs that are distributed to available cores of the current machine.
#' The inner loop (siminputrows) distributes simulation tasks to these cores.
#' However, it might be advantageous to split up large jobs into smaller jobs for example to reduce the total runtime of each job.
#' This can be done using the split parameter. If split is > 1 the siminput matrix is split into smaller parts.
#' Jobs are created for each combination of part and random seed.
#' If the split parameter is set such that the siminput matrix can not be splitted into equal parts, the procedure will stop and throw an error message.
#'
#' ### Debugging "Temporary simulation output file not found" error message:
#'
#' Whenever this error message appears it means that the simulation did not produce any output.
#' Two main reasons can lead to this problem, either the simulation did not even start or the simulation crashed during runtime.
#' Both can happen for several reasons and here are some hints for debugging this:
#' 1. Missing software:
#' Make sure that java is installed and available from the terminal (java -version).
#' Make sure that NetLogo is installed and available from the terminal.
#' 2. Wrong path definitions:
#' Make sure your nlpath points to a folder containing NetLogo.
#' Make sure your modelpath points to a *.nlogo model file.
#' Make sure that the nlversion within your nl object matches the NetLogo version of your nlpath.
#' Use the convenience function of nlrx for checking your nl object (print(nl), eval_variables_constants(nl)).
#' 3. Temporary files cleanup:
#' Due to automatic temp file cleanup on unix systems temporary output might be deleted.
#' Try reassigning the default temp folder for this R session (the unixtools package has a neat function).
#' 4. NetLogo runtime crashes:
#' It can happen that your NetLogo model started but failed to produce output because of a NetLogo runtime error.
#' Make sure your model is working correctly or track progress using print statements.
#' Sometimes the java virtual machine crashes due to memory constraints.
#'
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
#' results <- run_nl_all(threads = 10, nl = nl) # 10 threads
#' })
#'
#' # Run in parallel on local machine (NetLogo < 7):
#' library(future)
#' plan(multisession)
#' results <- run_nl_all(nl)
#'
#' }
#' @aliases run_nl_all
#' @rdname run_nl_all
#'
#' @export

run_nl_all <- function(nl,
                       block_size = 100,
                       threads = 1,
                       split = 1,
                       cleanup.csv = TRUE,
                       cleanup.xml = TRUE,
                       cleanup.bat = TRUE,
                       writeRDS = FALSE) {

  if (getnl(nl, "nlversion") >= "7.0.0") {
    nl_results <- run_nl_all_logolink(nl, block_size, threads) # NetLogo 7.0 upwards
    return(nl_results)
  } else {
    nl_results <- run_nl_all_legacy(nl, split, cleanup.csv, cleanup.xml, cleanup.bat, writeRDS) # NetLogo < 7.0
    return(nl_results)
  }
}




#' Backend function for run_nl_all using the logolink execution path (supported for NetLogo 7+)
#'
#' @description Internal backend used by \code{run_nl_all()} for NetLogo 7+.
#'
#' @param nl nl object
#' @param block_size see \code{run_nl_all()}
#' @param threads see \code{run_nl_all()}
#' @return tibble with simulation output results
#' @details
#' Internal helper function wrapped by \code{run_nl_all()}.
#' The defined \code{nl@simdesign} is split into parameterization blocks.
#' Blocks are written to single BehaviorSpace XML files and executed afterwards via Logolink.
#' @keywords internal # 🟡 ????????????????

run_nl_all_logolink <- function(nl, block_size = 100, threads = 1) {
  # 🔵 Sebastian Example:
  # nl <- nl(tolles experiment).
  # jobs_list <- job_split(nl = nl, blocks = 100)
  # rslurm(fun = run_nl_block, data_var = jobs_list, data_fix = nl)

  # Settings check for run_metrics_condition and tickmetrics (conflict when both are used at the same time)
  if (!is.na(nl@experiment@run_metrics_condition) && nl@experiment@tickmetrics == "true") {
    warning(
      "Both tickmetrics and run_metrics_condition are set. As tickmetrics causes metrics to be recorded every tick, record_metrics_condition will have no effect.",
      call. = FALSE
    )
  }

  # Warn user, regarding repetition setting
  if (nl@experiment@repetition > 1 & getnl(nl, "nlversion") >= "7.0.0") {
    warning(
      paste0(
        "Experiment with repetition > 1 detected: Simdesign seeds won't be passed to NetLogo.\n",
        "Repeated runs will therefore not use nl@simdesign@simseeds as explicit random-seed values.\n",
        "Seeds will instead be chosen by NetLogo.\n",
        "For reproducible results, it is recommended to set repetition = 1 and use nseeds via the simdesign."
      ),
      call. = FALSE
    )
  }


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
  # 🟡 nl is required for detecting formatting issues right now, it can be left out once logolink stops using janitor on column names
  nl_results <- merge_result_blocks(nl, results_list)

  return(nl_results)
}


#' Backend function used by run_nl_all_logolink and run_nl_one_logolink to run simulations (NetLogo 7+)
#'
#' @description Creates XML file for all simulations defined in \code{block_df} and executes these simulations using the logolink package
#'
#' @param nl see \code{run_nl_all()}
#' @param block_df dataframe of simulations definition, with one row being one run
#' @param block_number assigned by \code{run_nl_all_logolink()} and only used for naming.
#' @return tibble with simulation output results
#' @details
#' Runs a single block "\code{block_df}" by writing a single BehaviorSpace XML file, executing it afterwards.
#' Relies on the logolink package.
#' @keywords internal # 🟡 ????????????????

run_nl_block <- function(nl, block_df, block_number, threads) {
  checkmate::assert_int(threads, lower = 1)
  checkmate::assert_int(block_number, lower = 0)


  # 1. BEHAVIORSPACE DEFINITION PREPARATION
  # Handling NLRX features, ensuring correct formatting of settings and simulation input

  # Ensure boolean formatting # 🔴 or just leave it at a warning? otherwise, could also clean double quotes.
  for (col in names(block_df)) { # convert to logical, as NetLogo expects logical!
    if (is.character(block_df[[col]]) && all(block_df[[col]] %in% c("true", "false"))) { # all instances of "true" and "false"
      block_df[[col]] <- block_df[[col]] == "true" # "true" = TRUE, "false" = FALSE
    }
  }

  # Handle idrunnum (add it to the parameterizations if given) # 🔴
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


  # Merge legacy 'evalticks' and 'run_metrics_condition' into 'unified_run_metrics_condition'
  # And handle cases of both or none being defined.
  has_run_metrics_condition <- !is.na(nl@experiment@run_metrics_condition)
  has_evalticks <- any(!is.na(nl@experiment@evalticks)) # any, because it can be a vector

  if (has_run_metrics_condition && has_evalticks) { # both are defined:
    warning(
      "Both run_metrics_condition and evalticks are set. Ignoring evalticks.",
      call. = FALSE
    )
    unified_run_metrics_condition <- nl@experiment@run_metrics_condition
  } else if (has_run_metrics_condition) {
    unified_run_metrics_condition <- nl@experiment@run_metrics_condition
  } else if (has_evalticks) { # handle evalticks when given as vector:
    unified_run_metrics_condition <- paste0("member? ticks [",  paste(nl@experiment@evalticks, collapse = " "), "]"
    )
  } else {
    unified_run_metrics_condition <- NULL
  }

  # Make 'evalticks', 'run_metrics_condition' and 'tickmetrics' adhere to legacy logic.
  # -> tickmetrics = 'true' in order for recording conditions to apply
  if (nl@experiment@tickmetrics != "true") {
    if (!is.null(unified_run_metrics_condition)) {
      warning(
        "'evalticks' and 'run_metrics_condition' are ignored when 'tickmetrics' isn't 'true'.",
        call. = FALSE
      )
    }
    unified_run_metrics_condition <- NULL
  }
  # Only pass tickmetrics when it is the only recording rule defined
  # Otherwise it would override other recording conditions
  run_metrics_every_step <- nl@experiment@tickmetrics == "true" && is.null(unified_run_metrics_condition)


  # Format the job configuration (NetLogo variables) in a way that can be passed as sub_experiments (nested list instead of df)
  # "siminputrow" column has to be left out, as its not a variable within NetLogo.
  # For when 'repetition' > 1, 'random-seed' also needs to be left out (as NetLogo has to pick seeds itself then).
  if (nl@experiment@repetition > 1) {
    jobs_formatted <- lapply(seq_len(nrow(block_df)), function(i) {
      as.list(dplyr::select(block_df[i, , drop = FALSE], -siminputrow, -`random-seed`))
    })
  } else { # when no repetitions:
    jobs_formatted <- lapply(seq_len(nrow(block_df)), function(i) {
      as.list(dplyr::select(block_df[i, , drop = FALSE], -siminputrow))
    })
  }

  # 2. BEHAVIORSPACE FILE CREATION
  # Use Logolink to translate the simulation-parametersets (siminput) and NLRX user settings into a Behaviorspace XML that can be ran by NetLogo:
  xml_path <- suppressWarnings(logolink::create_experiment(
    name = paste0("NLRX Experiment", nl@experiment@expname, ". Job ID ", block_number),
    repetitions = nl@experiment@repetition,
    run_metrics_every_step = run_metrics_every_step,
    time_limit = nl@experiment@runtime,
    setup = if (!is.na(nl@experiment@idsetup)) nl@experiment@idsetup else NULL, # names of setup ..
    go = if (!is.na(nl@experiment@idgo)) nl@experiment@idgo else NULL, # .. and go buttons
    post_experiment = if (!is.na(nl@experiment@idfinal)) nl@experiment@idfinal else NULL,
    exit_condition = if (!is.na(nl@experiment@stopcond)) nl@experiment@stopcond else NULL,
    run_metrics_condition = unified_run_metrics_condition, # when measurements are recorded
    metrics = util_collect_experiment_metrics(nl), # packages nl@experiment@metrics with nl@experiment@metrics.turtles/patches/links.
    constants = nl@experiment@constants,
    sub_experiments = jobs_formatted # specific parameter configurations
  ))

  cat("\n Model running via: ", xml_path, "\n")

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
  # 🔴 NetLogo returns the end-of-simulation record. Always.
  # THis is an issue for record rules (evalticks, run_metrics_condition) as the last tick will always be returned
  # .. even when the record condition isn't applying.
  # 🔵 Consider removing this feature (run_metrics_condition)
  # Fix for evalticks:
  if (nl@experiment@tickmetrics == "true" && # ensure evalticks was actually applied x1
      all(!is.na(nl@experiment@evalticks)) && # ensure evalticks was actually applied x2
      is.na(nl@experiment@run_metrics_condition)) { # ensure evalticks was actually applied x3
    results_final <- results_final |>
      dplyr::filter(step %in% nl@experiment@evalticks) # filter out any records not adhering to the rule.
  }

  # 🟡 IDEAS
  # in case of evalticks its easy: simply filter(step %in% evalticks) like in the legacy script
  # in case of run_metrics_condition, automatically filtering is close to impossible
  # 1) instruct users to set the stop condition in a way where it only stops if it coincides with run_metrics_condition
  #     --> eg. no sheep + tick as a multiple of 10.
  #    ... or simply accept the extra data point
  # 2) ... could also solve it with specific exit conditions, where values are set to something specific when they dont coincide
  # with the record ticks condition. But that is finnicky and no one will use it anyways.


  # infer relation of resulting run_numbers and original siminputrow from block_df
  lookup <- block_df |>
    dplyr::mutate(run_number = dplyr::row_number())

  if (any(c("random-seed", "random_seed") %in% names(results_final))) { # random_seed is temporary, logolink will most likely give an option to always return as random-seed.
    lookup <- dplyr::select(lookup, run_number, siminputrow)
  } else {
    # random-seed won't be returned when repetition > 1
    # but later analysis will expect this seed.
    # even though the simulation didn't run with this seed, it is reattached.
    # this was handled the same way pre NetLogo 7, see util_gather_results()
    lookup <- dplyr::select(lookup, run_number, siminputrow, `random-seed`)
  }

  # assign siminputrow to create results_final
  results_final <- results_final |>
    dplyr::mutate(run_number = as.integer(run_number)) |>
    dplyr::arrange(run_number) |>
    dplyr::left_join(lookup, by = "run_number")

  results_final <- util_clean_agent_metrics(results_final, nl, expect_cleaned_names = TRUE)

  return(results_final)
}


#' Backend function for run_nl_all_logolink (NetLogo 7+)
#'
#' @description Splits \code{nl@simdesign} into parameterization blocks, to be later executed with function \code{run_nl_block()}
#'
#' @param nl see \code{run_nl_all()}
#' @param block_size see \code{run_nl_all()}
#' @return a list of dataframes, each representing one bundle of simulations
#' @keywords internal # 🟡 ????????????????

create_simulation_blocks <- function(nl, block_size){
  siminput <- getsim(nl, "siminput")
  simseeds <- getsim(nl, "simseeds")

  # Ensure block_size is formatted correctly.
  checkmate::assert_int(block_size, lower = 1)

  # Construct blocks of simulation parameterizations ("jobs")
  siminput <- siminput |> dplyr::mutate(siminputrow = dplyr::row_number()) # required for mapping, later on.
  distinct_simulations <- siminput |> tidyr::expand_grid(`random-seed` = simseeds) # uses simseeds to construct fully fledged experiment definitions (1 row per simulation, not counting repetitions which are done in NetLogo)

  required_blocks <- ceiling(seq_len(nrow(distinct_simulations)) / block_size) # determine number of job blocks
  simulation_blocks_list <- split(distinct_simulations, required_blocks) # split simulation-definitions into blocks.

  return(simulation_blocks_list)
}


#' Backend function for run_nl_all_logolink (NetLogo 7+)
#'
#' @description Merges results produced in \code{run_nl_all_logolink} by \code{run_nl_block} into one dataframe, which is returned as final result of \code{run_nl_all}
#'
#' @param nl see \code{run_nl_all()}
#' @param block_size see \code{run_nl_all()}
#' @return  tibble with simulation output results
#' @keywords internal # 🟡 ????????????????

merge_result_blocks <- function(nl, results_list){
  nl_results <- dplyr::bind_rows(results_list)

  ## logolink (used in run_nl_block() ) renames outputs, but NLRX expects the original names downstream
  # rename back via explicit name mappings
  #🟡 logolink will offer a feature to preserve names, as of now this feature is not there, so it uses janitor::make_clean_names forcibly.
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






#' Backend function for \code{run_nl_all()} using the legacy execution path (supported for NetLogo < 7.0)
#'
#' @description Internal backend used by \code{run_nl_all()} for legacy NetLogo versions.
#'
#' @param nl nl object
#' @param split see \code{run_nl_all()}
#' @param cleanup.csv see \code{run_nl_all()}
#' @param cleanup.xml see \code{run_nl_all()}
#' @param cleanup.bat see \code{run_nl_all()}
#' @param writeRDS see \code{run_nl_all()}
#' @return tibble with simulation output results
#' @details
#' Helper function wrapped by \code{run_nl_all()}.
#' Allows for parallelization via \code{split} by using the future package.
#' Executes one simulation at a time, causing NetLogo to restart for every simulation.
#' @keywords internal

run_nl_all_legacy <- function(nl, split, cleanup.csv, cleanup.xml, cleanup.bat, writeRDS){
  ## Store the number of siminputrows
  siminput_nrow <- nrow(getsim(nl, "siminput"))
  ## Check if split parameter is valid:
  if (siminput_nrow %% split != 0) {
    stop(
      "Modulo of split parameter and number of rows of the siminput matrix is
      not 0. Please adjust split parameter to a valid value!",
      call. = FALSE
    )
  }

  ## Calculate size of one part:
  n_per_part <- siminput_nrow / split
  ## Generate job ids from seeds and parts:
  jobs <- as.list(expand.grid(getsim(nl, "simseeds"), seq(1:split)))

  ## Setup progress bar:
  total_steps <- siminput_nrow * length(getsim(nl, "simseeds"))
  p <- progressr::progressor(steps = total_steps)

  ## Execute on remote location
  nl_results <- furrr::future_map_dfr(
    seq_along(jobs[[1]]),
    function(job) {
      ## Extract current seed and part from job id:
      job_seed <- jobs[[1]][[job]]
      job_part <- jobs[[2]][[job]]

      ## Calculate rowids of the current part:
      rowids <-
        seq(1:n_per_part) +
        (job_part - 1) * n_per_part

      ## Start inner loop to run model simulations:
      res_job <- furrr::future_map_dfr(
        rowids,
        function(siminputrow) {

          # Update progress bar:
          p(sprintf("row %d/%d seed %d",
                    siminputrow, nrow(getsim(nl, "siminput")),
                    job_seed))
          # Run simulation
          res_one <- run_nl_one(
            nl = nl,
            seed = job_seed,
            siminputrow = siminputrow,
            cleanup.csv = cleanup.csv,
            cleanup.xml = cleanup.xml,
            cleanup.bat = cleanup.bat,
            writeRDS = writeRDS
          )
          return(res_one)
        })
      return(res_job)
    })
  return(nl_results)
}



#' Execute one NetLogo simulation from a nl object
#'
#' @description Execute one NetLogo simulation from a nl object with a defined experiment and simdesign
#'
#' @param nl nl object
#' @param seed a random seed for the NetLogo simulation (ignored when repetitions > 1)
#' @param threads number of NetLogo threads used for execution (NetLogo 7+).
#' @param siminputrow rownumber of the input tibble within the attached simdesign object that should be executed
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results (NetLogo < 7).
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results (NetLogo < 7).
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results (NetLogo < 7).
#' @param writeRDS TRUE/FALSE, if TRUE an rds file with the simulation results will be written to the defined outpath folder of the experiment within the nl object (NetLogo < 7).
#' @return tibble with simulation output results
#' @details
#'
#' run_nl_one executes one simulation of the specified NetLogo model within the provided nl object.
#' The random seed is set within the NetLogo model to control stochasticity.
#' The siminputrow number defines which row of the input data tibble within the simdesign object of the provided nl object is executed.
#' The logical cleanup variables can be set to FALSE to preserve temporary generated output files (e.g. for debugging).
#' cleanup.csv deletes/keeps the temporary generated model output files from each run.
#' cleanup.xml deletes/keeps the temporary generated experiment xml files from each run.
#' cleanup.bat deletes/keeps the temporary generated batch/sh commandline files from each run.
#'
#' This function can be used to run single simulations of a NetLogo model.
#'
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
#' }
#' @aliases run_nl_one
#' @rdname run_nl_one
#'
#' @export

run_nl_one <- function(nl,
                       seed,
                       threads = 1,
                       siminputrow,
                       cleanup.csv = TRUE,
                       cleanup.xml = TRUE,
                       cleanup.bat = TRUE,
                       writeRDS = FALSE) {

  if (getnl(nl, "nlversion") >= "7.0.0") {
    nl_results <- run_nl_one_logolink(nl, seed, threads, siminputrow)# NetLogo 7.0+
    return(nl_results)
  } else {
    nl_results <- run_nl_one_legacy(nl, seed, siminputrow, cleanup.csv, cleanup.xml, cleanup.bat, writeRDS) # NetLogo < 7.0
    return(nl_results)
  }
}



#' Backend function for run_nl_one using the logolink execution path (supported for NetLogo 7+)
#'
#' @description Internal backend used by \code{run_nl_one()} for NetLogo 7+.
#'
#' @param nl nl object
#' @param seed random seed passed to NetLogo if defined
#' @param threads see \code{run_nl_all()}
#' @param siminputrow row of the siminput table to be executed
#' @return tibble with simulation output results
#' @details
#' Internal helper function wrapped by \code{run_nl_one()}.
#' The defined \code{siminputrow} is transformed into a single-row parameterization block.
#' This block is then passed to \code{run_nl_block()} for Logolink execution.
#' @keywords internal

run_nl_one_logolink <- function(nl,
                                seed = NA,
                                threads = 1,
                                siminputrow) {

  # Get the parameterization
  block_df <- getsim(nl, "siminput")[siminputrow, , drop = FALSE]

  # That one parameterization will be the whole block.
  block_df$siminputrow <- siminputrow

  # Add the seed
  if (!is.na(seed)) {
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



#' Backend function for \code{run_nl_one()} using the legacy execution path (supported for NetLogo < 7.0)
#'
#' @description Internal backend used by \code{run_nl_one()} for legacy NetLogo versions.
#'
#' @param nl nl object
#' @param seed random seed passed to NetLogo
#' @param siminputrow row of the siminput table to be executed
#' @param cleanup.csv see \code{run_nl_one()}
#' @param cleanup.xml see \code{run_nl_one()}
#' @param cleanup.bat see \code{run_nl_one()}
#' @param writeRDS see \code{run_nl_one()}
#' @return tibble with simulation output results
#' @details
#' Helper function wrapped by \code{run_nl_one()}.
#' Executes a single simulation by writing a temporary BehaviorSpace XML file,
#' running NetLogo, and gathering the generated output.
#' @keywords internal

run_nl_one_legacy <- function(nl,
                              seed,
                              siminputrow,
                              cleanup.csv,
                              cleanup.xml,
                              cleanup.bat,
                              writeRDS) {

  util_eval_simdesign(nl)

  ## Write XML File:
  xmlfile <-
    tempfile(
      pattern = paste0("nlrx_seed_", seed, "_row_", siminputrow, "_"),
      fileext = ".xml"
    )

  util_create_sim_XML(nl, seed, siminputrow, xmlfile)

  ## Execute:
  outfile <-
    tempfile(
      pattern = paste0("nlrx_seed_", seed, "_row_", siminputrow, "_"),
      fileext = ".csv"
    )

  batchpath <- util_read_write_batch(nl)

  util_call_nl(nl, xmlfile, outfile, batchpath)

  ## Read results
  nl_results <- util_gather_results(nl, outfile, seed, siminputrow)

  ## Delete temporary files:
  cleanup.files <- list("csv" = outfile,
                        "xml" = xmlfile,
                        "bat" = batchpath)

  util_cleanup(nl, cleanup.csv, cleanup.xml, cleanup.bat, cleanup.files)


  if (isTRUE(writeRDS))
  {
    if(dir.exists(nl@experiment@outpath))
    {
      filename <- paste0("nlrx_seed_", seed, "_row_", siminputrow, ".rds")
      saveRDS(nl_results, file=file.path(nl@experiment@outpath, filename))
    } else
    {
      warning(paste0("Outpath of nl object does not exist on remote file system: ", nl@experiment@outpath, ". Cannot write rds file!"))
    }
  }

  return(nl_results)
}




#' Execute NetLogo simulation without pregenerated parametersets
#'
#' @description Execute NetLogo simulation from a nl object with a defined experiment and simdesign but no pregenerated input parametersets
#'
#' @param nl nl object
#' @param seed a random seed for the NetLogo simulation
#' @param threads number of NetLogo threads used for execution (NetLogo 7+).
#' @param cleanup.csv TRUE/FALSE, if TRUE temporary created csv output files will be deleted after gathering results.
#' @param cleanup.xml TRUE/FALSE, if TRUE temporary created xml output files will be deleted after gathering results.
#' @param cleanup.bat TRUE/FALSE, if TRUE temporary created bat/sh output files will be deleted after gathering results.
#' @return simulation output results can be tibble, list, ...
#' @details
#'
#' run_nl_dyn can be used for simdesigns where no predefined parametersets exist.
#' This is the case for dynamic designs, such as Simulated Annealing and Genetic Algorithms, where parametersets are dynamically generated, based on the output of previous simulations.
#' The logical cleanup variables can be set to FALSE to preserve temporary generated output files (e.g. for debugging).
#' cleanup.csv deletes/keeps the temporary generated model output files from each run.
#' cleanup.xml deletes/keeps the temporary generated experiment xml files from each run.
#' cleanup.bat deletes/keeps the temporary generated batch/sh commandline files from each run.
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
#' results <- run_nl_dyn(nl)
#'
#' }
#' @aliases run_nl_dyn
#' @rdname run_nl_dyn
#'
#' @export

run_nl_dyn <- function(nl,
                       seed,
                       threads = 1,
                       cleanup.csv = TRUE,
                       cleanup.xml = TRUE,
                       cleanup.bat = TRUE) {
  nl_results <- NULL

  if (getsim(nl, "simmethod") == "GenSA") {
    nl_results <- util_run_nl_dyn_GenSA(
      nl = nl,
      seed = seed,
      threads = threads,
      cleanup.csv = cleanup.csv,
      cleanup.xml = cleanup.xml,
      cleanup.bat = cleanup.bat
    )
  }

  if (getsim(nl, "simmethod") == "GenAlg") {
    nl_results <- util_run_nl_dyn_GenAlg(
      nl = nl,
      seed = seed,
      threads = threads,
      cleanup.csv = cleanup.csv,
      cleanup.xml = cleanup.xml,
      cleanup.bat = cleanup.bat
    )
  }

  if (getsim(nl, "simmethod") == "ABCmcmc") {
    nl_results <- util_run_nl_dyn_ABCmcmc(
      nl = nl,
      seed = seed,
      threads = threads,
      cleanup.csv = cleanup.csv,
      cleanup.xml = cleanup.xml,
      cleanup.bat = cleanup.bat
    )
  }


  return(nl_results)
}

