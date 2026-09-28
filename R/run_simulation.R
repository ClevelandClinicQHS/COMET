#' Run Simulation
#'
#' @param days number of days to simulate
#' @param can_start number of candidates to start on the waitlist, they will be randomly selected to be more reflective of a waiting period listing day, all of these candidates will have a negative c_id and listing_day
#' @param match_alg function of how to screen and match individuals
#' @param ... arguments for match_alg
#' @param seed seed to set
#' @param desired random or mean for parameter generation
#' @param return_params if TRUE returns the parameters used for generating donors and candidates
#' @param include_matches if TRUE returns the donor and candidates matches
#'
#' @importFrom stats rgamma
#' @importFrom utils head
#'
#' @return list of full donors and candidates simulated at beginning and final datasets, who's left on waiting list, transplanted, died on waiting list, died post transplant and non utililized donors
#' @export
#'
#' @examples
#'\dontrun{
#'# Please ensure that data package is downloaded before running this example.
#'# If it is not, see the ‘COMET’ package readme for details
#'# on installing the required data package.
#'if (requireNamespace("cometdata", quietly = TRUE)) {
#' r1 <- run_simulation(days = 20, can_start = 1000,
#'  match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' r2 <- run_simulation(days = 20, can_start = 1000,
#'  match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
#'  wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365,
#'  post_tx_cap = 1826, bio_weight = .15, pld_weight = 0.05,
#'  peds_weight = 0.2, efficiency_weight = 0.1)
#'  }
#'  }
run_simulation <- function(days, can_start = 1250,
                           match_alg = match_cas,
                           ...,
                           seed = NULL, desired = "random", return_params = FALSE, include_matches = FALSE){

  data_present()

  call1 <- match.call(expand.dots = TRUE)

  if (!is.null(seed)) set.seed(as.integer(seed))
  ## overall timer start
  tic_overall <- Sys.time()

  # store logic decisions
  decision_log <- c()

  # deciding what matching score to use
  match_f <- match_alg
  m1 <- dplyr::quo_name(enquo(match_alg))
  # lu_f <- if(grepl("las", m1)){calculate_las}else{calculate_sub_cas}
  # decision log for the matching score selection
  decision_log <- c(decision_log, paste0("Matching algorithm detected: ",
                                         " → using ",
                                         if (grepl("las", m1)) "calculate_las" else "calculate_sub_cas"))

  # old candidate generation timer start
  tic_old <- Sys.time()

  if(is.numeric(can_start)){
    #add decision log as model generate old candidates based on the start_can number
    decision_log <- c(decision_log, paste0("can_start is numeric (", can_start, "), generating old candidates via cust_day()"))
    cust_day <- function(x){
      dy <- ceiling(x/350)+10
      dy <- dy*50
      q1 <- gen_and_spawn_candidates(days = dy) |>
        dplyr::left_join(odd3, by = "dx_grp")

      q1lu <- calculate_las(q1, wl_model = "LAS15", post_tx_model = "LAS15", wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365) |>
        dplyr::select("c_id", "lu_score") |>
        dplyr::left_join(q1, by = "c_id") |>
        dplyr::slice_sample(n = x*3, weight_by = .data$dx_weight) |>
        dplyr::slice_sample(n = x, weight_by = (.data$lu_score^(-3)))

      q1o <- sample(1:x, x, prob = q1lu$lu_score)

      q2 <- q1lu[q1o,] |>
        dplyr::select(-"listing_day") |>
        tidyr::nest(.by = "dx_grp") |>
        dplyr::left_join(prior_dist_gammas, by = "dx_grp") |>
        dplyr::mutate(listing_day = mapply(function(x, y, z) sort(round(-rgamma(n = nrow(x), shape = y, rate = z))), .data$data, .data$shape, .data$rate)) |>
        dplyr::select("dx_grp", "data", "listing_day") |>
        tidyr::unnest(cols = c("data", "listing_day")) |>
        dplyr::arrange(.data$listing_day) |>
        dplyr::relocate("listing_day", .after = "c_id") |>
        dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1) |>
        dplyr::arrange(.data$c_id) |>
        dplyr::select(-c("lu_score", "dx_weight"))

      return(q2)

    }

    old_candidates <- cust_day(can_start)

  }else{
    # the can_start is not numeric, added to log
    decision_log <- c(decision_log, "can_start is not numeric — using provided dataset for old candidates")
    old_candidates <- can_start
  }

  #timing for generating old candidate ends
  toc_old <- Sys.time()
  time_old_candidates <- as.numeric(difftime(toc_old, tic_old, units = "secs"))

  ## initializing
  # start timer for new candidate generation
  tic_cand <- Sys.time()
  if (return_params) {
    decision_log <- c(decision_log, "return_params = TRUE — generating candidates with parameter recording")
    new_candidates_p <- gen_and_spawn_candidates(days = days, desired = desired, return_params = TRUE)
    new_candidates <- new_candidates_p$data
  } else {
    decision_log <- c(decision_log, "return_params = FALSE — generating candidates without parameter recording")
    new_candidates <- gen_and_spawn_candidates(days = days, desired = desired)
  }
  # end timer for new candidate generation
  toc_cand <- Sys.time()
  time_new_candidates <- as.numeric(difftime(toc_cand, tic_cand, units = "secs"))

  # start timer for new donor generation
  tic_don <- Sys.time()
  if (return_params) {
    decision_log <- c(decision_log, "return_params = TRUE — generating donors with parameter recording")
    new_donors_p <- gen_and_spawn_donors(days = days, desired = desired, return_params = TRUE)
    new_donors <- new_donors_p$data
  } else {
    decision_log <- c(decision_log, "return_params = FALSE — generating donors without parameter recording")
    new_donors <- gen_and_spawn_donors(days = days, desired = desired)
  }
  # end timer for new donor generation
  toc_don <- Sys.time()
  time_new_donors <- as.numeric(difftime(toc_don, tic_don, units = "secs"))

  ## Combine everything to for candidate list and donor list
  cl <- dplyr::bind_rows(old_candidates, new_candidates)
  dl <- dplyr::filter(new_donors, .data$don_util == 1)

  ## creating empty list for initial state
  recipient_d <- dplyr::tibble(c_id = 0)[0,]
  waitlist_death_d <- dplyr::tibble()
  post_tx_death_d <- dplyr::tibble()
  non_used_donors <- dplyr::tibble()
  all_matches <- dplyr::tibble()

  ## create the initial state
  iter0 <- list(
    current_candidates = old_candidates,
    recipient_database = recipient_d,
    waitlist_death_database = waitlist_death_d,
    post_tx_death_database = post_tx_death_d,
    non_used_donors = non_used_donors,
    all_matches = all_matches
  )

  ## iteration
  test_i <- iter0
  ds <- 1:days
  ### For progress bar in timing may eventually remove
  qs <- ceiling(stats::quantile(ds, probs = seq(.1, 1, .1)))
  ## Run main simulation
  # start timer for main simulation
  tic_sim <- Sys.time()
  # storage for per-day timing results
  daily_timings <- list()

  for(i in ds){
    day_result  <- iteration(i,
                             cl, dl, include_matches = include_matches, updated_list = test_i
                             , match_alg = match_alg, ...

    )
    # record the detailed timing
    test_i <- day_result
    daily_timings[[i]] <- c(day_result$timing, day = i)

    #### progress bar
    if(i %in% qs){
      cat(paste0(names(qs)[max(which(i == qs))], " done "))
    }
  }
  # end timer for main simulation
  toc_sim <- Sys.time()
  time_simulation <- as.numeric(difftime(toc_sim, tic_sim, units = "secs"))

  ## --- Aggregate iteration timing ---
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

  ## End-to-end runtime
  toc_overall <- Sys.time()
  total_runtime <- as.numeric(difftime(toc_overall, tic_overall, units = "secs"))

  ## Print summary
  cat("\n----- Simulation Runtime Summary -----\n")
  cat("Old candidates generation: ", round(time_old_candidates, 2), " secs\n")
  cat("New candidates generation: ", round(time_new_candidates, 2), " secs\n")
  cat("New donors generation: ", round(time_new_donors, 2), " secs\n")
  cat("Main simulation (all iterations): ", round(time_simulation, 2), " secs\n")
  cat("Total runtime: ", round(total_runtime, 2), " secs\n")
  cat("--------------------------------------\n")

  ## Print decision log
  # cat("\nLogic decisions made during run:\n")
  # cat(paste0(" - ", decision_log, collapse = "\n"), "\n")

  ## Build output
  og <- list(call = call1,
             time_run = toc_overall - tic_overall,
             all_candidates = cl,
             all_donors = new_donors)

  l <- append(og, test_i)
  if (!include_matches) l <- l[-length(l)]
  if (return_params) {
    pp <- list("params" = append(new_donors_p$params, new_candidates_p$params))
    l <- append(pp, l)
  }

  ## store timing info
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

  class(l) <- "COMET"

  return(l)
}
