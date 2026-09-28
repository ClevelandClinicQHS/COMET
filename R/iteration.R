#' Iteration
#'
#' @param date current date to simulate
#' @param candidate_database full candidate database
#' @param donor_database full donor database
#' @param updated_list named list of databases that updated at each time point, originally all empty, \cr
#' @param include_matches if TRUE returns the donor and candidates matches
#' @param match_alg function for matching
#' @param ... arguments for match algorithm
#'
#' @return a named list of databases that updated at each time point, originally all empty, current_candidates, waitlist_death_database, recipient_database, and post_tx_death_database, non_used_donors
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr anti_join
#' @importFrom dplyr select
#' @importFrom stats na.omit
#' @importFrom dplyr ungroup
#' @importFrom tidyr complete
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#'
iteration <- function(date, candidate_database, donor_database, include_matches,
                      updated_list, match_alg, modules = NULL, ...) {

  # initialize timing record
  tic_total <- Sys.time()
  timings <- list(pre_tx_update = 0, post_tx_update = 0,
                  match_phase = 0, transplant_phase = 0,
                  total = 0, alive_candidates = 0,
                  available_donors = 0, num_matches = 0,
                  num_transplants = 0)

  ## unpack the current state
  match_alg <- match_alg
  current_candidates <- updated_list$current_candidates
  recipient_database <- updated_list$recipient_database
  waitlist_death_database <- updated_list$waitlist_death_database
  post_tx_death_database <- updated_list$post_tx_death_database
  non_used_donors <- updated_list$non_used_donors
  all_matches <- updated_list$all_matches

  ## create default
  default_modules <- list(
    pre_tx_update  = update_patients,
    post_tx_update = update_patients,
    match          = match_alg,
    transplant     = transplant_candidates
  )

  if (is.null(modules)) modules <- list()
  modules <- utils::modifyList(default_modules, modules)

  ## --- PRE-TRANSPLANT UPDATE ---
  current_candidates <- mutate(current_candidates, days_on_waitlist = date - .data$listing_day)
  can_up <- filter(current_candidates, .data$days_on_waitlist > 0)
  can_con <- anti_join(current_candidates, can_up, by = "c_id")

  if (nrow(can_up) > 0) {
    tic_pre <- Sys.time()
    updated_wl_candidates <- update_patients(
      patients = can_up, model = "CAS23r",
      elapsed_time = .data$days_on_waitlist, pre_tx = TRUE, cap = 365, date = date
    )
    toc_pre <- Sys.time()
    timings$pre_tx_update <- as.numeric(difftime(toc_pre, tic_pre, units = "secs"))

    can_up <- updated_wl_candidates$new_char
    removed <- can_up |>
      filter(.data$c_id %in% updated_wl_candidates$Removed$c_id) |>
      mutate(removal_day = date)
    dead <- can_up |>
      filter(.data$c_id %in% updated_wl_candidates$Dead$c_id) |>
      mutate(death_day = date)
    if (nrow(dead) > 0) waitlist_death_database <- bind_rows(waitlist_death_database, dead)
    if (nrow(removed) > 0) waitlist_death_database <- bind_rows(waitlist_death_database, removed)
    can_up_alive <- filter(can_up, .data$c_id %in% updated_wl_candidates$Alive$c_id)
    alive <- bind_rows(can_con, can_up_alive) |> arrange(.data$c_id)
  } else {
    alive <- current_candidates
  }

  ## --- POST-TRANSPLANT UPDATE ---
  if (nrow(recipient_database) > 0) {
    rec_up <- recipient_database |>
      mutate(days_after_tx = date - .data$transplant_day) |>
      filter(.data$days_after_tx > 0)
    rec_con <- recipient_database |> anti_join(rec_up, by = "c_id")

    if (nrow(rec_up) > 0) {
      tic_post <- Sys.time()
      updated_post_tx_recipients <- update_patients(
        patients = rec_up, model = "CAS23r",
        elapsed_time = .data$days_after_tx, pre_tx = FALSE, cap = 1826, date = date
      )
      toc_post <- Sys.time()
      timings$post_tx_update <- as.numeric(difftime(toc_post, tic_post, units = "secs"))

      rec_up <- updated_post_tx_recipients$new_char
      post_tx_dead <- rec_up |>
        filter(.data$c_id %in% updated_post_tx_recipients$Dead$c_id) |>
        mutate(death_day = date)
      if (nrow(post_tx_dead) > 0)
        post_tx_death_database <- bind_rows(post_tx_death_database, post_tx_dead)
      rec_up_alive <- filter(rec_up, .data$c_id %in% updated_post_tx_recipients$Alive$c_id)
      post_tx_alive <- bind_rows(rec_up_alive, rec_con) |> arrange(.data$c_id)
    } else {
      post_tx_alive <- recipient_database
    }
  } else {
    post_tx_alive <- recipient_database
  }

  ## --- EARLY EXIT if no alive candidates ---
  if (nrow(alive) == 0) {
    recipient_database <- post_tx_alive
    new_candidates <- filter(candidate_database, .data$listing_day == date)
    current_candidates <- alive
    if (nrow(new_candidates) > 0L)
      current_candidates <- bind_rows(current_candidates, new_candidates)
    toc_total <- Sys.time()
    timings$total <- as.numeric(difftime(toc_total, tic_total, units = "secs"))
    l <- list(current_candidates = current_candidates,
              recipient_database = recipient_database,
              waitlist_death_database = waitlist_death_database,
              post_tx_death_database = post_tx_death_database,
              non_used_donors = non_used_donors,
              all_matches = all_matches,
              timing = timings)
    return(l)
  }

  ## --- MATCHING PHASE ---
  donors_avl <- donor_database[donor_database$recovery_day == date, , drop = FALSE]
  ndon <- nrow(donors_avl)
  timings$alive_candidates   <- nrow(alive)
  timings$available_donors   <- nrow(donors_avl)

  # --- Case: no available donors -------------------------------------------
  if (ndon == 0L) {

    recipient_database <- post_tx_alive
    current_candidates <- alive

    # Filter new candidates
    new_candidates <- candidate_database[candidate_database$listing_day == date, , drop = FALSE]

    # Only bind if needed (avoid bind_rows when empty)
    if (nrow(new_candidates) > 0L) {
      current_candidates <- vctrs::vec_rbind(current_candidates, new_candidates)
    }

    toc_total <- Sys.time()
    timings$total <- as.numeric(difftime(toc_total, tic_total, units = "secs"))

    return(list(
      current_candidates      = current_candidates,
      recipient_database      = recipient_database,
      waitlist_death_database = waitlist_death_database,
      post_tx_death_database  = post_tx_death_database,
      non_used_donors         = non_used_donors,
      all_matches             = all_matches,
      timing                  = timings
    ))
  }

  # --- Case: donors found ---------------------------------------------------
  tic_match <- Sys.time()
  matches   <- modules$match(alive, donors_avl, ...)
  validate_matches(matches)
  toc_match <- Sys.time()

  timings$match_phase <- as.numeric(difftime(toc_match, tic_match, units = "secs"))
  timings$num_matches <- nrow(matches)

  # --- Store matches --------------------------------------------------------
  if (include_matches) {
    # Faster mutate: avoid ungroup() unless needed
    if (dplyr::is_grouped_df(matches)) {
      matches_ng <- dplyr::ungroup(matches)
    } else {
      matches_ng <- matches
    }
    # Faster lapply extract
    matches_ng$data <- lapply(matches_ng$data, function(x) x["c_id"])
    # vec_rbind is faster than bind_rows for iterative accumulation
    all_matches <- vctrs::vec_rbind(all_matches, matches_ng)

  } else {
    # Preserve original behavior: an empty tibble with same columns
    all_matches <- matches[0, , drop = FALSE]
  }

  ## --- TRANSPLANT PHASE ---
  tic_tx <- Sys.time()

  if (nrow(matches) > 0L) {

    tr <- modules$transplant(matches, recipient_database$c_id)

    # --- Fast summarization of organs_rec by donor ID ---
    d_ids <- tr$d_id
    o_rec <- tr$organs_rec
    tr_sum <- tibble(
      d_id = sort(unique(d_ids)),
      organs_rec = as.numeric(rowsum(o_rec, d_ids)[, 1])
    )

    # --- Fast sequence complete ---
    dmin <- min(donors_avl$d_id)
    dmax <- max(donors_avl$d_id)
    full_ids <- tibble(d_id = dmin:dmax)

    tr_x <- dplyr::left_join(full_ids, tr_sum, by = "d_id")
    tr_x$organs_rec[is.na(tr_x$organs_rec)] <- 0L

    # --- Merge into donor table ---
    nu_donors <- dplyr::left_join(donors_avl, tr_x, by = "d_id")
    nu_donors$organs_non_used <- nu_donors$organs_avl - nu_donors$organs_rec

    # filter only non-used or NA
    keep <- (nu_donors$organs_non_used > 0L) | is.na(nu_donors$organs_non_used)
    nu_donors <- nu_donors[keep, c("d_id", "don_org", "organs_non_used")]

    nu_donors$non_used_day <- date

    # join with matches
    nu_donors <- dplyr::left_join(nu_donors, matches, by = c("d_id", "don_org"))

    # offers = nrow(data) (use length instead of sapply + nrow)
    nu_donors$offers <- vapply(
      nu_donors$data,
      function(x) {
        nr <- nrow(x)
        if (is.null(nr)) NA_integer_ else as.integer(nr)
      },
      integer(1L)
    )
    nu_donors$nm <- FALSE

    # remove the heavy list column
    nu_donors$data <- NULL

    tr_cids <- tr$c_id

  } else {

    # no matches, same behavior as original
    nu_donors <- donors_avl[, c("d_id", "don_org", "organs_avl"), drop = FALSE]
    names(nu_donors)[names(nu_donors) == "organs_avl"] <- "organs_non_used"

    nu_donors$non_used_day <- date
    nu_donors$offers <- 0L
    nu_donors <- dplyr::relocate(nu_donors, offers, .before = don_org)
    nu_donors$nm <- TRUE

    tr_cids <- NA

    # preserve factor levels exactly as original code
    tr <- tibble(
      c_id = integer(),
      surg_type = factor(character(), levels = c('D', 'E', 'S'))
    )
  }

  toc_tx <- Sys.time()
  timings$transplant_phase <- as.numeric(difftime(toc_tx, tic_tx, units = "secs"))
  timings$num_transplants <- ifelse(is.null(tr_cids), 0, length(na.omit(tr_cids)))

  ## --- DATABASE UPDATES ---

  # --- Normalize offers column ------------------------------------------------
  if (nrow(nu_donors) > 0L) {

    # offers may contain NULL inside list-column → normalize to numeric
    of2 <- vapply(
      nu_donors$offers,
      function(x) if (is.null(x)) 0 else as.numeric(x),
      numeric(1L)
    )

    # only rewrite column if needed (same as original)
    if (any(of2 == 0L, na.rm = TRUE)) {
      nu_donors$offers <- of2
    }

  } else {
    # empty numeric vector — identical to original behavior
    nu_donors$offers <- numeric()
  }

  # --- Append to non_used_donors (faster than bind_rows) ---------------------
  non_used_donors <- vctrs::vec_rbind(non_used_donors, nu_donors)

  # --- Build new recipients ---------------------------------------------------
  # avoid filter() tidy eval:
  idx_rec <- alive$c_id %in% tr_cids
  new_recipients <- alive[idx_rec, , drop = FALSE]

  if (nrow(new_recipients) > 0L) {
    new_recipients$transplant_day <- date
    new_recipients$days_on_waitlist <- NULL  # remove column faster than select()

    # left_join with transplant info
    new_recipients <- dplyr::left_join(
      new_recipients,
      tr,
      by = c("c_id", "surg_type")
    )
  }

  # --- Update recipient database ----------------------------------------------
  recipient_database <- vctrs::vec_rbind(post_tx_alive, new_recipients)

  # --- Update current candidates ----------------------------------------------
  # remove transplanted candidates
  idx_keep <- !(alive$c_id %in% tr_cids)
  current_candidates <- alive[idx_keep, , drop = FALSE]

  # new candidates on this date
  new_candidates <- candidate_database[candidate_database$listing_day == date, ,
                                       drop = FALSE]

  # append to current candidates
  if (nrow(new_candidates) > 0L) {
    current_candidates <- vctrs::vec_rbind(current_candidates, new_candidates)
  }

  ## --- finalize and return ---
  toc_total <- Sys.time()
  timings$total <- as.numeric(difftime(toc_total, tic_total, units = "secs"))

  l <- list(current_candidates = current_candidates,
            waitlist_death_database = waitlist_death_database,
            recipient_database = recipient_database,
            post_tx_death_database = post_tx_death_database,
            non_used_donors = non_used_donors,
            all_matches = all_matches,
            timing = timings)
  return(l)
}
