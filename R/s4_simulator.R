## =========================================================
## S4 Simulator (COMETSimulator) + run()
## File: R/s4_simulator.R
## =========================================================
#' COMET simulator class
#' @exportClass COMETSimulator
setClass(
  "COMETSimulator",
  slots = c(
    config = "COMETConfig",
    policy = "COMETPolicy",

    ## Function hooks: keep existing procedural code untouched
    candidate_generator = "function",   # e.g. gen_and_spawn_candidates
    donor_generator = "function",       # e.g. gen_and_spawn_donors
    iteration_fn = "function",          # e.g. iteration

    ## Optional: pre-run hook (loads data, etc.)
    preflight_fn = "ANY",               # NULL or function
    notes = "character"
  ),
  prototype = list(
    preflight_fn = NULL,
    notes = ""
  )
)

#' Run a COMET simulator
#' @param simulator A COMETSimulator object.
#' @export
setGeneric("run", function(simulator) standardGeneric("run"))

#' Validate a COMET simulator
#' @param simulator A COMETSimulator object.
#' @export
setGeneric("validateSimulator", function(simulator) standardGeneric("validateSimulator"))

setMethod("show", "COMETSimulator", function(object) {
  cat("<COMETSimulator>\n")
  cat("  config days: ", object@config@days, "\n", sep = "")
  cat("  policy: ", policyLabel(object@policy), "\n", sep = "")
  cat("  candidate_generator: ", deparse(substitute(object@candidate_generator)), "\n", sep = "")
  cat("  donor_generator: ", deparse(substitute(object@donor_generator)), "\n", sep = "")
  cat("  iteration_fn: ", deparse(substitute(object@iteration_fn)), "\n", sep = "")
  if (nzchar(object@notes)) cat("  notes: ", object@notes, "\n", sep = "")
})

setMethod("validateSimulator", "COMETSimulator", function(simulator) {
  validateConfig(simulator@config)
  validatePolicy(simulator@policy)

  if (!is.function(simulator@candidate_generator)) stop("candidate_generator must be a function.", call. = FALSE)
  if (!is.function(simulator@donor_generator)) stop("donor_generator must be a function.", call. = FALSE)
  if (!is.function(simulator@iteration_fn)) stop("iteration_fn must be a function.", call. = FALSE)

  if (!is.null(simulator@preflight_fn) && !is.function(simulator@preflight_fn)) {
    stop("preflight_fn must be NULL or a function.", call. = FALSE)
  }

  TRUE
})

## ---- constructor ----
#' Create a COMET simulator
#'
#' @param config A COMETConfig object.
#' @param policy A COMETPolicy object.
#' @param candidate_generator Candidate generation function.
#' @param donor_generator Donor generation function.
#' @param iteration_fn Iteration function.
#' @param preflight_fn Optional preflight function.
#' @param notes Optional notes.
#' @return A COMETSimulator object.
#' @export
COMETSimulator <- function(config,
                           policy,
                           candidate_generator = gen_and_spawn_candidates,
                           donor_generator = gen_and_spawn_donors,
                           iteration_fn = iteration,
                           preflight_fn = data_present,
                           notes = "") {
  sim <- new(
    "COMETSimulator",
    config = config,
    policy = policy,
    candidate_generator = candidate_generator,
    donor_generator = donor_generator,
    iteration_fn = iteration_fn,
    preflight_fn = preflight_fn,
    notes = notes
  )
  validateSimulator(sim)
  sim
}

## ---- run() method ----
setMethod("run", "COMETSimulator", function(simulator) {

  validateSimulator(simulator)

  cfg <- simulator@config
  pol <- simulator@policy
  pol_params <- policyParams(pol)

  ## ---- Resolve match_alg (allow string or function) ----
  if (!is.null(pol_params$match_alg)) {
    # If user passed a function, we're good
    if (is.function(pol_params$match_alg)) {
      # do nothing
    } else if (is.character(pol_params$match_alg) && length(pol_params$match_alg) == 1) {
      nm <- pol_params$match_alg

      # 1) If there is a function literally named "LAS15" (or whatever), use it
      if (exists(nm, mode = "function")) {
        pol_params$match_alg <- get(nm, mode = "function")
      } else {
        # 2) Try common COMET naming patterns
        candidates <- c(
          paste0("match_", nm),
          paste0("match", nm),
          paste0(nm, "_match"),
          paste0("matching_", nm),
          paste0("matching", nm)
        )
        hit <- candidates[candidates %in% ls()]
        if (length(hit) >= 1 && exists(hit[1], mode = "function")) {
          pol_params$match_alg <- get(hit[1], mode = "function")
        } else {
          stop(
            "match_alg was provided as '", nm, "' but I couldn't find a function for it.\n",
            "Try running: ls(pattern='LAS') and ls(pattern='match.*LAS') to see available match functions."
          )
        }
      }
    } else {
      stop("match_alg must be either a function or a single string name of a function.")
    }
  }
  ## ------------------------------------------------------

  ## Preflight: load data, check globals, etc.
  if (!is.null(simulator@preflight_fn)) simulator@preflight_fn()

  ## Seed
  if (!is.null(cfg@seed)) set.seed(as.integer(cfg@seed))

  ## Timers
  tic_overall <- Sys.time()
  decision_log <- character()

  decision_log <- c(decision_log, paste0("Policy: ", policyLabel(pol)))
  decision_log <- c(decision_log, paste0("Days: ", cfg@days, ", desired: ", cfg@desired))
  decision_log <- c(decision_log, paste0("include_matches: ", cfg@include_matches,
                                         ", return_params: ", cfg@return_params))

  ## -------------------------------------------------------
  ## 1) Old candidates (same behavior as your original script)
  ## -------------------------------------------------------
  tic_old <- Sys.time()

  if (is.numeric(cfg@can_start)) {
    decision_log <- c(decision_log, paste0("can_start numeric=", cfg@can_start, " → using cust_day() logic (from run_simulation)"))

    ## NOTE:
    ## This cust_day() relies on objects/functions from your existing codebase:
    ## odd3, prior_dist_gammas, calculate_las(), gen_and_spawn_candidates(), etc.
    ## We keep it as-is to minimize changes.
    cust_day <- function(x) {
      dy <- ceiling(x / 350) + 10
      dy <- dy * 50

      q1 <- simulator@candidate_generator(days = dy) |>
        dplyr::left_join(odd3, by = "dx_grp")

      q1lu <- calculate_las(
        q1,
        wl_model = "LAS15", post_tx_model = "LAS15",
        wl_weight = 2, post_tx_weight = 1,
        wl_cap = 365, post_tx_cap = 365
      ) |>
        dplyr::select("c_id", "lu_score") |>
        dplyr::left_join(q1, by = "c_id") |>
        dplyr::slice_sample(n = x * 3, weight_by = .data$dx_weight) |>
        dplyr::slice_sample(n = x, weight_by = (.data$lu_score^(-3)))

      q1o <- sample(1:x, x, prob = q1lu$lu_score)

      q2 <- q1lu[q1o, ] |>
        dplyr::select(-"listing_day") |>
        tidyr::nest(.by = "dx_grp") |>
        dplyr::left_join(prior_dist_gammas, by = "dx_grp") |>
        dplyr::mutate(
          listing_day = mapply(
            function(x, y, z) sort(round(-stats::rgamma(n = nrow(x), shape = y, rate = z))),
            .data$data, .data$shape, .data$rate
          )
        ) |>
        dplyr::select("dx_grp", "data", "listing_day") |>
        tidyr::unnest(cols = c("data", "listing_day")) |>
        dplyr::arrange(.data$listing_day) |>
        dplyr::relocate("listing_day", .after = "c_id") |>
        dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number() - 1) |>
        dplyr::arrange(.data$c_id) |>
        dplyr::select(-c("lu_score", "dx_weight"))

      q2
    }

    old_candidates <- cust_day(as.integer(cfg@can_start))

  } else {
    decision_log <- c(decision_log, "can_start is data.frame/tibble → using provided old candidates")
    old_candidates <- cfg@can_start
  }

  toc_old <- Sys.time()
  time_old_candidates <- as.numeric(difftime(toc_old, tic_old, units = "secs"))

  ## -------------------------------------------------------
  ## 2) New candidates + donors
  ## -------------------------------------------------------
  tic_cand <- Sys.time()
  if (cfg@return_params) {
    nc_p <- simulator@candidate_generator(days = cfg@days, desired = cfg@desired, return_params = TRUE)
    new_candidates <- nc_p$data
  } else {
    new_candidates <- simulator@candidate_generator(days = cfg@days, desired = cfg@desired)
    nc_p <- NULL
  }
  toc_cand <- Sys.time()
  time_new_candidates <- as.numeric(difftime(toc_cand, tic_cand, units = "secs"))

  tic_don <- Sys.time()
  if (cfg@return_params) {
    nd_p <- simulator@donor_generator(days = cfg@days, desired = cfg@desired, return_params = TRUE)
    new_donors <- nd_p$data
  } else {
    new_donors <- simulator@donor_generator(days = cfg@days, desired = cfg@desired)
    nd_p <- NULL
  }
  toc_don <- Sys.time()
  time_new_donors <- as.numeric(difftime(toc_don, tic_don, units = "secs"))

  ## -------------------------------------------------------
  ## 3) Initialize state list (same as your current iteration contract)
  ## -------------------------------------------------------
  cl <- dplyr::bind_rows(old_candidates, new_candidates)
  dl <- dplyr::filter(new_donors, .data$don_util == 1)

  recipient_d <- dplyr::tibble(c_id = 0)[0, ]

  state <- list(
    current_candidates = old_candidates,
    recipient_database = recipient_d,
    waitlist_death_database = dplyr::tibble(),
    post_tx_death_database = dplyr::tibble(),
    non_used_donors = dplyr::tibble(),
    all_matches = dplyr::tibble()
  )

  ## -------------------------------------------------------
  ## 4) Daily loop (calls your existing iteration())
  ## -------------------------------------------------------
  ds <- 1:cfg@days
  qs <- ceiling(stats::quantile(ds, probs = seq(.1, 1, .1)))

  tic_sim <- Sys.time()
  daily_timings <- vector("list", length(ds))

  for (i in ds) {
    ## Minimal-change: pass policy params into iteration via do.call()
    day_result <- do.call(
      simulator@iteration_fn,
      c(
        list(
          i,
          cl,
          dl,
          include_matches = cfg@include_matches,
          updated_list = state
        ),
        pol_params
      )
    )

    state <- day_result
    daily_timings[[i]] <- c(day_result$timing, day = i)

    if (i %in% qs) cat(paste0(names(qs)[max(which(i == qs))], " done "))
  }

  toc_sim <- Sys.time()
  time_simulation <- as.numeric(difftime(toc_sim, tic_sim, units = "secs"))

  ## -------------------------------------------------------
  ## 5) Timing summary (same as your previous OOP wrapper)
  ## -------------------------------------------------------
  timing_df <- do.call(rbind, lapply(daily_timings, as.data.frame))
  print(names(timing_df))
  print(head(timing_df))
  summary_timing <- timing_df |>
    dplyr::summarise(
      total_pre_tx = sum(.data$pre_tx_update, na.rm = TRUE),
      total_post_tx = sum(.data$post_tx_update, na.rm = TRUE),
      total_match = sum(.data$match_phase, na.rm = TRUE),
      total_transplant = sum(.data$transplant_phase, na.rm = TRUE),
      total_iteration_time = sum(.data$total, na.rm = TRUE),
      avg_alive_candidates = mean(.data$alive_candidates, na.rm = TRUE),
      avg_donors = mean(.data$available_donors, na.rm = TRUE),
      total_matches = sum(.data$num_matches, na.rm = TRUE),
      total_transplants = sum(.data$num_transplants, na.rm = TRUE)
    )

  toc_overall <- Sys.time()
  total_runtime <- as.numeric(difftime(toc_overall, tic_overall, units = "secs"))

  cat("\n----- Simulation Runtime Summary -----\n")
  cat("Old candidates generation:  ", round(time_old_candidates, 2), " secs\n")
  cat("New candidates generation:  ", round(time_new_candidates, 2), " secs\n")
  cat("New donors generation:      ", round(time_new_donors, 2), " secs\n")
  cat("Main simulation (all iterations): ", round(time_simulation, 2), " secs\n")
  cat("Total runtime:              ", round(total_runtime, 2), " secs\n")
  cat("--------------------------------------\n")

  timing <- list(
    old_candidates = time_old_candidates,
    new_candidates = time_new_candidates,
    new_donors = time_new_donors,
    simulation = time_simulation,
    total = total_runtime,
    per_iteration_timing = timing_df,
    per_iteration_timing_summary = summary_timing
  )

  ## -------------------------------------------------------
  ## 6) Build the “raw” result list in the same shape as before
  ## -------------------------------------------------------
  raw <- list(
    call = match.call(),
    time_run = toc_overall - tic_overall,
    all_candidates = cl,
    all_donors = new_donors
  )

  ## append state
  raw <- append(raw, state)

  ## drop all_matches if user didn't want matches
  if (!cfg@include_matches && "all_matches" %in% names(raw)) {
    raw$all_matches <- NULL
  }

  ## include params if requested
  if (cfg@return_params && !is.null(nc_p) && !is.null(nd_p)) {
    raw$params <- append(nd_p$params, nc_p$params)
  }

  raw$timing <- timing
  raw$decision_log <- decision_log

  ## Wrap into COMETResult (S4)
  COMETResult(
    policy = pol,
    config = cfg,
    data = raw,
    timing = timing,
    decision_log = decision_log,
    call = match.call()
  )
})
