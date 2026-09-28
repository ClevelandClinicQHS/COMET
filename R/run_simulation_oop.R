## =========================================================
## S4 OOP wrapper for run_simulation() using methods package
## =========================================================

setClass(
  "COMETSimulation",
  slots = c(
    ## configuration / inputs
    days = "numeric",
    can_start = "ANY",          # numeric OR data.frame/tibble
    match_alg = "function",
    seed = "ANY",               # NULL or numeric
    desired = "character",
    return_params = "logical",
    include_matches = "logical",
    dots = "list",              # captures ... arguments

    ## run-time artifacts / outputs
    call = "language",
    decision_log = "character",
    timing = "list",
    results = "ANY"            # the final COMET-like list
  )
)

## constructor helper (so you can do: sim <- COMETSimulation(...))
COMETSimulation <- function(days,
                            can_start = 1250,
                            match_alg = match_cas,
                            ...,
                            seed = NULL,
                            desired = "random",
                            return_params = FALSE,
                            include_matches = FALSE) {
  new(
    "COMETSimulation",
    days = as.numeric(days),
    can_start = can_start,
    match_alg = match_alg,
    seed = seed,
    desired = desired,
    return_params = return_params,
    include_matches = include_matches,
    dots = list(...),
    call = match.call(),
    decision_log = character(),
    timing = list(),
    results = list()
  )
}

## generics
setGeneric("run", function(object) standardGeneric("run"))
setGeneric("getResults", function(object) standardGeneric("getResults"))
setGeneric("getTiming", function(object) standardGeneric("getTiming"))
setGeneric("getDecisionLog", function(object) standardGeneric("getDecisionLog"))

## nice printing
setMethod("show", "COMETSimulation", function(object) {
  cat("<COMETSimulation>\n")
  cat("  days: ", object@days, "\n", sep = "")
  cat("  desired: ", object@desired, "\n", sep = "")
  cat("  return_params: ", object@return_params, "\n", sep = "")
  cat("  include_matches: ", object@include_matches, "\n", sep = "")
  cat("  seed: ", if (is.null(object@seed)) "NULL" else as.character(object@seed), "\n", sep = "")
  cat("  has results: ", length(object@results) > 0, "\n", sep = "")
})

setMethod("getResults", "COMETSimulation", function(object) object@results)
setMethod("getTiming", "COMETSimulation", function(object) object@timing)
setMethod("getDecisionLog", "COMETSimulation", function(object) object@decision_log)

## core: run() method (ported from your function)
setMethod("run", "COMETSimulation", function(object) {

  data_present()

  call1 <- object@call

  if (!is.null(object@seed)) set.seed(as.integer(object@seed))

  tic_overall <- Sys.time()

  decision_log <- character()

  ## matching algorithm label for logging
  m1 <- tryCatch(deparse(substitute(object@match_alg)), error = function(e) "match_alg")
  ## deparse(substitute()) on slots is not super informative; use function name if possible:
  m1 <- tryCatch(as.character(call1$match_alg), error = function(e) m1)

  alg_label <- "calculate_sub_cas"

  if (is.character(m1) && length(m1) == 1) {
    if (grepl("las", tolower(m1))) {
      alg_label <- "calculate_las"
    }
  }

  decision_log <- c(
    decision_log,
    paste0(
      "Matching algorithm detected: ",
      if (length(m1) == 0) "<unknown>" else m1,
      " → using ", alg_label
    )
  )

  ## -------- old candidates generation --------
  tic_old <- Sys.time()

  can_start <- object@can_start

  if (is.numeric(can_start)) {

    decision_log <- c(
      decision_log,
      paste0("can_start is numeric (", can_start, "), generating old candidates via cust_day()")
    )

    cust_day <- function(x) {
      dy <- ceiling(x / 350) + 10
      dy <- dy * 50

      q1 <- gen_and_spawn_candidates(days = dy) |>
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

    old_candidates <- cust_day(can_start)

  } else {
    decision_log <- c(decision_log, "can_start is not numeric — using provided dataset for old candidates")
    old_candidates <- can_start
  }

  toc_old <- Sys.time()
  time_old_candidates <- as.numeric(difftime(toc_old, tic_old, units = "secs"))

  ## -------- new candidates/donors generation --------
  days <- object@days
  desired <- object@desired
  return_params <- object@return_params
  include_matches <- object@include_matches

  tic_cand <- Sys.time()
  if (return_params) {
    decision_log <- c(decision_log, "return_params = TRUE — generating candidates with parameter recording")
    new_candidates_p <- gen_and_spawn_candidates(days = days, desired = desired, return_params = TRUE)
    new_candidates <- new_candidates_p$data
  } else {
    decision_log <- c(decision_log, "return_params = FALSE — generating candidates without parameter recording")
    new_candidates <- gen_and_spawn_candidates(days = days, desired = desired)
  }
  toc_cand <- Sys.time()
  time_new_candidates <- as.numeric(difftime(toc_cand, tic_cand, units = "secs"))

  tic_don <- Sys.time()
  if (return_params) {
    decision_log <- c(decision_log, "return_params = TRUE — generating donors with parameter recording")
    new_donors_p <- gen_and_spawn_donors(days = days, desired = desired, return_params = TRUE)
    new_donors <- new_donors_p$data
  } else {
    decision_log <- c(decision_log, "return_params = FALSE — generating donors without parameter recording")
    new_donors <- gen_and_spawn_donors(days = days, desired = desired)
  }
  toc_don <- Sys.time()
  time_new_donors <- as.numeric(difftime(toc_don, tic_don, units = "secs"))

  ## -------- main simulation loop --------
  cl <- dplyr::bind_rows(old_candidates, new_candidates)
  dl <- dplyr::filter(new_donors, .data$don_util == 1)

  recipient_d <- dplyr::tibble(c_id = 0)[0, ]
  iter0 <- list(
    current_candidates = old_candidates,
    recipient_database = recipient_d,
    waitlist_death_database = dplyr::tibble(),
    post_tx_death_database = dplyr::tibble(),
    non_used_donors = dplyr::tibble(),
    all_matches = dplyr::tibble()
  )

  test_i <- iter0
  ds <- 1:days
  qs <- ceiling(stats::quantile(ds, probs = seq(.1, 1, .1)))

  tic_sim <- Sys.time()
  daily_timings <- vector("list", length(ds))

  for (i in ds) {
    day_result <- do.call(
      iteration,
      c(
        list(
          i,
          cl,
          dl,
          include_matches = include_matches,
          updated_list = test_i,
          match_alg = object@match_alg
        ),
        object@dots
      )
    )

    test_i <- day_result
    daily_timings[[i]] <- c(day_result$timing, day = i)

    if (i %in% qs) {
      cat(paste0(names(qs)[max(which(i == qs))], " done "))
    }
  }

  toc_sim <- Sys.time()
  time_simulation <- as.numeric(difftime(toc_sim, tic_sim, units = "secs"))

  timing_df <- do.call(rbind, lapply(daily_timings, as.data.frame))
  summary_timing <- timing_df |>
    dplyr::summarise(
      total_pre_tx = sum(pre_tx_update),
      total_post_tx = sum(post_tx_update),
      total_match = sum(match_phase),
      total_transplant = sum(transplant_phase),
      total_iteration_time = sum(total),
      avg_alive_candidates = mean(alive_candidates),
      avg_donors = mean(available_donors),
      total_matches = sum(num_matches),
      total_transplants = sum(num_transplants)
    )

  toc_overall <- Sys.time()
  total_runtime <- as.numeric(difftime(toc_overall, tic_overall, units = "secs"))

  cat("\n----- Simulation Runtime Summary -----\n")
  cat("Old candidates generation: ", round(time_old_candidates, 2), " secs\n")
  cat("New candidates generation: ", round(time_new_candidates, 2), " secs\n")
  cat("New donors generation: ", round(time_new_donors, 2), " secs\n")
  cat("Main simulation (all iterations): ", round(time_simulation, 2), " secs\n")
  cat("Total runtime: ", round(total_runtime, 2), " secs\n")
  cat("--------------------------------------\n")

  ## output list (same shape as before)
  og <- list(
    call = call1,
    time_run = toc_overall - tic_overall,
    all_candidates = cl,
    all_donors = new_donors
  )

  l <- append(og, test_i)
  if (!include_matches) l <- l[-length(l)]

  if (return_params) {
    pp <- list(params = append(new_donors_p$params, new_candidates_p$params))
    l <- append(pp, l)
  }

  l$timing <- list(
    old_candidates = time_old_candidates,
    new_candidates = time_new_candidates,
    new_donors = time_new_donors,
    simulation = time_simulation,
    total = total_runtime,
    per_iteration_timing = timing_df,
    per_iteration_timing_summary = summary_timing
  )
  l$decision_log <- decision_log
  class(l) <- "COMET"   # keep your existing downstream expectations

  ## write back into S4 object
  object@decision_log <- decision_log
  object@timing <- l$timing
  object@results <- l

  object
})

## Optional: a lightweight summary method
setGeneric("summary", function(object, ...) standardGeneric("summary"))
setMethod("summary", "COMETSimulation", function(object, ...) {
  if (length(object@results) == 0) {
    cat("No results yet. Call run(sim) first.\n")
    return(invisible(object))
  }
  s <- object@timing$per_iteration_timing_summary
  print(s)
  invisible(s)
})

## ======================
## Example usage
## ======================
## sim <- COMETSimulation(
##   days = 20, can_start = 1000,
##   match_alg = match_cas,
##   wl_model = "CAS23", post_tx_model = "CAS23",
##   wl_weight = 0.25, post_tx_weight = 0.25,
##   wl_cap = 365, post_tx_cap = 1826,
##   bio_weight = .15, pld_weight = 0.05,
##   peds_weight = 0.2, efficiency_weight = 0.1,
##   seed = 123
## )
## sim <- run(sim)
## res <- getResults(sim)
## summary(sim)
