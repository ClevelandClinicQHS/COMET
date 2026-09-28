## =========================================================
## S4 Experiment layer (multiple simulator runs)
## File: R/s4_experiment.R
## =========================================================

## ---------------------------------------------------------
## Classes
## ---------------------------------------------------------
#' COMET multi-run experiment class
#' @exportClass COMETExperiment
setClass(
  "COMETExperiment",
  slots = c(
    simulator = "COMETSimulator",   # base simulator template
    n_runs = "numeric",
    seeds = "ANY",                  # NULL, scalar master seed, or vector of seeds
    parallel = "logical",
    workers = "ANY",                # NULL or numeric
    notes = "character"
  ),
  prototype = list(
    seeds = NULL,
    parallel = FALSE,
    workers = NULL,
    notes = ""
  )
)
#' COMET experiment result class
#' @exportClass COMETExperimentResult
setClass(
  "COMETExperimentResult",
  slots = c(
    experiment = "COMETExperiment",
    results = "list",     # list of COMETResult objects
    seeds = "numeric",
    timing = "list",
    result_table = "ANY", # tibble/data.frame with run_id, seed, result
    call = "ANY"
  ),
  prototype = list(
    results = list(),
    seeds = numeric(),
    timing = list(),
    result_table = NULL,
    call = quote(NULL)
  )
)

## ---------------------------------------------------------
## Generics
## ---------------------------------------------------------
#' Validate a COMET experiment
#' @param experiment A COMETExperiment object.
#' @export
setGeneric("validateExperiment", function(experiment) standardGeneric("validateExperiment"))

#' Run a COMET experiment
#' @param experiment A COMETExperiment object.
#' @export
setGeneric("runExperiment", function(experiment) standardGeneric("runExperiment"))

#' Get the result list from a COMETExperimentResult
#' @param x A COMETExperimentResult object.
#' @export
setGeneric("getResults", function(x) standardGeneric("getResults"))

#' Get seeds from a COMETExperimentResult
#' @param x A COMETExperimentResult object.
#' @export
setGeneric("getSeeds", function(x) standardGeneric("getSeeds"))

#' Get the run table from a COMETExperimentResult
#' @param x A COMETExperimentResult object.
#' @export
setGeneric("getResultTable", function(x) standardGeneric("getResultTable"))

#' Get per-run metrics from a COMETExperimentResult
#' @param x A COMETExperimentResult object.
#' @export
setGeneric("experimentMetrics", function(x) standardGeneric("experimentMetrics"))

## ---------------------------------------------------------
## Validation
## ---------------------------------------------------------
setMethod("validateExperiment", "COMETExperiment", function(experiment) {
  if (!is(experiment@simulator, "COMETSimulator")) {
    stop("simulator must be a COMETSimulator.", call. = FALSE)
  }

  validateSimulator(experiment@simulator)

  if (!is.numeric(experiment@n_runs) ||
      length(experiment@n_runs) != 1L ||
      is.na(experiment@n_runs) ||
      experiment@n_runs < 1) {
    stop("n_runs must be a single positive integer.", call. = FALSE)
  }

  if (length(experiment@parallel) != 1L || is.na(experiment@parallel)) {
    stop("parallel must be TRUE/FALSE.", call. = FALSE)
  }

  if (!is.null(experiment@workers)) {
    if (!is.numeric(experiment@workers) ||
        length(experiment@workers) != 1L ||
        is.na(experiment@workers) ||
        experiment@workers < 1) {
      stop("workers must be NULL or a single positive integer.", call. = FALSE)
    }
  }

  sds <- experiment@seeds
  n_runs <- as.integer(experiment@n_runs)

  if (!is.null(sds)) {
    if (!is.numeric(sds)) {
      stop("seeds must be NULL or numeric.", call. = FALSE)
    }

    if (!(length(sds) == 1L || length(sds) == n_runs)) {
      stop("seeds must be NULL, a single master seed, or a vector of length n_runs.", call. = FALSE)
    }

    if (any(is.na(sds))) {
      stop("seeds cannot contain NA.", call. = FALSE)
    }
  }

  TRUE
})

## ---------------------------------------------------------
## Constructors
## ---------------------------------------------------------
#' Create a COMET experiment
#'
#' @param simulator A COMETSimulator object.
#' @param n_runs Number of runs.
#' @param seeds Optional seed or seed vector.
#' @param parallel Whether to run in parallel.
#' @param workers Optional number of workers.
#' @param notes Optional notes.
#' @return A COMETExperiment object.
#' @export
COMETExperiment <- function(simulator,
                            n_runs,
                            seeds = NULL,
                            parallel = FALSE,
                            workers = NULL,
                            notes = "") {
  exp <- new(
    "COMETExperiment",
    simulator = simulator,
    n_runs = as.integer(n_runs),
    seeds = seeds,
    parallel = parallel,
    workers = workers,
    notes = notes
  )
  validateExperiment(exp)
  exp
}

#' Create a COMET experiment result
#'
#' @param experiment A COMETExperiment object.
#' @param results A list of COMETResult objects.
#' @param seeds Numeric seeds used.
#' @param timing Timing information.
#' @param result_table Per-run result table.
#' @param call Matched call.
#' @return A COMETExperimentResult object.
#' @export
COMETExperimentResult <- function(experiment,
                                  results,
                                  seeds,
                                  timing = list(),
                                  result_table = NULL,
                                  call = quote(NULL)) {
  if (!is(experiment, "COMETExperiment")) {
    stop("experiment must be a COMETExperiment.", call. = FALSE)
  }

  if (!is.list(results)) {
    stop("results must be a list.", call. = FALSE)
  }

  if (length(results) > 0 && !all(vapply(results, is, logical(1), "COMETResult"))) {
    stop("All elements of results must be COMETResult objects.", call. = FALSE)
  }

  if (!is.numeric(seeds)) {
    stop("seeds must be numeric.", call. = FALSE)
  }

  new(
    "COMETExperimentResult",
    experiment = experiment,
    results = results,
    seeds = as.numeric(seeds),
    timing = timing,
    result_table = result_table,
    call = call
  )
}

## ---------------------------------------------------------
## Show methods
## ---------------------------------------------------------
setMethod("show", "COMETExperiment", function(object) {
  cat("<COMETExperiment>\n")
  cat("  policy: ", policyLabel(object@simulator@policy), "\n", sep = "")
  cat("  days: ", object@simulator@config@days, "\n", sep = "")
  cat("  n_runs: ", as.integer(object@n_runs), "\n", sep = "")
  cat("  parallel: ", object@parallel, "\n", sep = "")

  if (is.null(object@seeds)) {
    cat("  seeds: NULL (will be generated)\n")
  } else if (length(object@seeds) == 1L) {
    cat("  seeds: master seed ", as.integer(object@seeds), "\n", sep = "")
  } else {
    cat("  seeds: vector of length ", length(object@seeds), "\n", sep = "")
  }

  if (!is.null(object@workers)) {
    cat("  workers: ", as.integer(object@workers), "\n", sep = "")
  }

  if (nzchar(object@notes)) cat("  notes: ", object@notes, "\n", sep = "")
})

setMethod("show", "COMETExperimentResult", function(object) {
  cat("<COMETExperimentResult>\n")
  cat("  runs: ", length(object@results), "\n", sep = "")
  cat("  policy: ", policyLabel(object@experiment@simulator@policy), "\n", sep = "")
  cat("  days: ", object@experiment@simulator@config@days, "\n", sep = "")
  if (!is.null(object@timing$total)) {
    cat("  total runtime (sec): ", round(object@timing$total, 2), "\n", sep = "")
  }
})

## ---------------------------------------------------------
## Accessors
## ---------------------------------------------------------
setMethod("getResults", "COMETExperimentResult", function(x) x@results)
setMethod("getSeeds", "COMETExperimentResult", function(x) x@seeds)
setMethod("getResultTable", "COMETExperimentResult", function(x) x@result_table)

## ---------------------------------------------------------
## Internal helpers
## ---------------------------------------------------------

## Resolve experiment seeds:
## - NULL            -> generate n_runs seeds
## - length 1        -> use as master seed, generate n_runs seeds deterministically
## - length n_runs   -> use directly
.resolve_experiment_seeds <- function(n_runs, seeds = NULL) {
  if (is.null(seeds)) {
    seeds <- sample.int(1e8, n_runs)

  } else if (length(seeds) == 1L) {
    master_seed <- as.integer(seeds)

    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }

    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)

    set.seed(master_seed)
    seeds <- sample.int(1e8, n_runs)

  } else {
    if (length(seeds) != n_runs) {
      stop("Length of 'seeds' must equal n_runs.", call. = FALSE)
    }
    seeds <- as.integer(seeds)
  }

  as.numeric(seeds)
}

## Clone a COMETConfig but replace seed only
.copy_config_with_seed <- function(config, seed) {
  COMETConfig(
    days = config@days,
    seed = seed,
    desired = config@desired,
    can_start = config@can_start,
    include_matches = config@include_matches,
    return_params = config@return_params,
    notes = config@notes
  )
}

## Clone a COMETSimulator but replace config only
.copy_simulator_with_seed <- function(simulator, seed) {
  new_cfg <- .copy_config_with_seed(simulator@config, seed)

  COMETSimulator(
    config = new_cfg,
    policy = simulator@policy,
    candidate_generator = simulator@candidate_generator,
    donor_generator = simulator@donor_generator,
    iteration_fn = simulator@iteration_fn,
    preflight_fn = simulator@preflight_fn,
    notes = simulator@notes
  )
}

## ---------------------------------------------------------
## runExperiment()
## ---------------------------------------------------------
setMethod("runExperiment", "COMETExperiment", function(experiment) {
  validateExperiment(experiment)

  tic_overall <- Sys.time()

  base_sim <- experiment@simulator
  n_runs <- as.integer(experiment@n_runs)
  seeds <- .resolve_experiment_seeds(n_runs = n_runs, seeds = experiment@seeds)

  run_one <- function(one_seed, simulator_template) {
    cfg0 <- simulator_template@config

    cfg_i <- COMETConfig(
      days = cfg0@days,
      seed = one_seed,
      desired = cfg0@desired,
      can_start = cfg0@can_start,
      include_matches = cfg0@include_matches,
      return_params = cfg0@return_params,
      notes = cfg0@notes
    )

    sim_i <- COMETSimulator(
      config = cfg_i,
      policy = simulator_template@policy,
      candidate_generator = simulator_template@candidate_generator,
      donor_generator = simulator_template@donor_generator,
      iteration_fn = simulator_template@iteration_fn,
      preflight_fn = simulator_template@preflight_fn,
      notes = simulator_template@notes
    )

    res_i <- run(sim_i)
    list(seed = one_seed, result = res_i)
  }

  if (!experiment@parallel) {

    res_list <- lapply(seeds, function(s) run_one(s, base_sim))

  } else {

    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("parallel = TRUE requires the 'future' and 'future.apply' packages.", call. = FALSE)
    }

    workers <- experiment@workers
    cat("available cores: ", future::availableCores(), "\n", sep = "")
    if (is.null(workers)) {
      workers <- min(n_runs, future::availableCores())
    } else {
      workers <- min(as.integer(workers), n_runs, future::availableCores())
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = workers)

    res_list <- future.apply::future_lapply(
      seeds,
      function(s, simulator_template) {
        cfg0 <- simulator_template@config

        cfg_i <- COMETConfig(
          days = cfg0@days,
          seed = s,
          desired = cfg0@desired,
          can_start = cfg0@can_start,
          include_matches = cfg0@include_matches,
          return_params = cfg0@return_params,
          notes = cfg0@notes
        )

        sim_i <- COMETSimulator(
          config = cfg_i,
          policy = simulator_template@policy,
          candidate_generator = simulator_template@candidate_generator,
          donor_generator = simulator_template@donor_generator,
          iteration_fn = simulator_template@iteration_fn,
          preflight_fn = simulator_template@preflight_fn,
          notes = simulator_template@notes
        )

        res_i <- run(sim_i)
        list(seed = s, result = res_i)
      },
      simulator_template = base_sim,
      future.seed = TRUE
    )
  }

  results <- lapply(res_list, `[[`, "result")

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required to build the result table.", call. = FALSE)
  }

  result_tbl <- tibble::tibble(
    run_id = seq_len(n_runs),
    seed = vapply(res_list, `[[`, numeric(1L), "seed"),
    result = results
  )

  toc_overall <- Sys.time()
  total_runtime <- as.numeric(difftime(toc_overall, tic_overall, units = "secs"))

  cat("Experiment total runtime: ", round(total_runtime, 2), " secs\n", sep = "")

  COMETExperimentResult(
    experiment = experiment,
    results = results,
    seeds = seeds,
    timing = list(total = total_runtime),
    result_table = result_tbl,
    call = match.call()
  )
})

## ---------------------------------------------------------
## summary() for COMETExperimentResult
## ---------------------------------------------------------
setMethod("summary", "COMETExperimentResult", function(object, ...) {
  cat("Policy:", policyLabel(object@experiment@simulator@policy), "\n")
  cat("Days:", object@experiment@simulator@config@days, "\n")
  cat("Runs:", length(object@results), "\n")

  if (!is.null(object@timing$total)) {
    cat("Experiment total runtime (sec):", round(object@timing$total, 2), "\n")
  }

  if (length(object@results) == 0) {
    cat("No results available.\n")
    return(invisible(object))
  }

  metric_list <- lapply(object@results, metrics)

  get_num <- function(name) {
    vals <- vapply(metric_list, function(x) {
      val <- x[[name]]
      if (is.null(val) || length(val) != 1 || is.na(val)) NA_real_ else as.numeric(val)
    }, numeric(1))
    vals
  }

  summarize_one <- function(name, label = name, digits = 2) {
    vals <- get_num(name)
    if (all(is.na(vals))) {
      cat(label, ": NA\n", sep = "")
    } else {
      cat(
        label, ": mean=", round(mean(vals, na.rm = TRUE), digits),
        ", sd=", round(stats::sd(vals, na.rm = TRUE), digits),
        ", min=", round(min(vals, na.rm = TRUE), digits),
        ", max=", round(max(vals, na.rm = TRUE), digits),
        "\n", sep = ""
      )
    }
  }

  summarize_one("runtime_total_sec", "Per-run runtime (sec)")
  summarize_one("waitlist_deaths", "Waitlist deaths")
  summarize_one("post_tx_deaths", "Post-tx deaths")
  summarize_one("non_used_donors", "Non-used donors")
  summarize_one("total_matches", "Total matches")
  summarize_one("total_transplants", "Total transplants")
  summarize_one("avg_alive_candidates", "Avg alive candidates")
  summarize_one("avg_donors", "Avg donors")

  invisible(object)
})

## ---------------------------------------------------------
## experimentMetrics()
## Returns a data.frame-like object with one row per run
## ---------------------------------------------------------
setMethod("experimentMetrics", "COMETExperimentResult", function(x) {
  if (length(x@results) == 0) {
    return(data.frame())
  }

  metric_list <- lapply(x@results, metrics)

  metric_df <- do.call(
    rbind,
    lapply(metric_list, function(m) {
      data.frame(
        policy = if (!is.null(m$policy)) as.character(m$policy) else NA_character_,
        days = if (!is.null(m$days)) as.numeric(m$days) else NA_real_,
        runtime_total_sec = if (!is.null(m$runtime_total_sec)) as.numeric(m$runtime_total_sec) else NA_real_,
        waitlist_deaths = if (!is.null(m$waitlist_deaths)) as.numeric(m$waitlist_deaths) else NA_real_,
        post_tx_deaths = if (!is.null(m$post_tx_deaths)) as.numeric(m$post_tx_deaths) else NA_real_,
        non_used_donors = if (!is.null(m$non_used_donors)) as.numeric(m$non_used_donors) else NA_real_,
        total_matches = if (!is.null(m$total_matches)) as.numeric(m$total_matches) else NA_real_,
        total_transplants = if (!is.null(m$total_transplants)) as.numeric(m$total_transplants) else NA_real_,
        avg_alive_candidates = if (!is.null(m$avg_alive_candidates)) as.numeric(m$avg_alive_candidates) else NA_real_,
        avg_donors = if (!is.null(m$avg_donors)) as.numeric(m$avg_donors) else NA_real_,
        stringsAsFactors = FALSE
      )
    })
  )

  out <- cbind(
    data.frame(
      run_id = seq_along(x@results),
      seed = x@seeds
    ),
    metric_df
  )

  rownames(out) <- NULL
  out
})
