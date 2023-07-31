#' Iteration
#'
#' @param date current date to simulate
#' @param candidate_database full candidate database, from initialization
#' @param donor_database full candidate database, from initialization
#' @param updated_list named list of databases that updated at each time point, originally all empty, \cr
#' @param include_matches TRUE or FALSE, whether or not the include the donor and candidates matches
#'  current_candidates, waitlist_death_database, recipient_database, and post_tx_death_database, non_used_donors
#' @param match_alg function for matching
#' @param ... arguemnts for match algorithm
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
#' @importFrom purrr pluck
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#'
iteration <- function(date, candidate_database, donor_database, include_matches,
                       ## Things that will change after each iteration excluding date
                       updated_list,
                       ## arguments for matching
                       match_alg, ...
)
{

  match_alg <- match_alg

  current_candidates <- updated_list$current_candidates
  recipient_database <- updated_list$recipient_database
  waitlist_death_database <- updated_list$waitlist_death_database
  post_tx_death_database <- updated_list$post_tx_death_database
  non_used_donors <- updated_list$non_used_donors
  all_matches <- updated_list$all_matches

  current_candidates <- mutate(current_candidates, days_on_waitlist = date - listing_day)

  can_up <- filter(current_candidates, days_on_waitlist > 0)

  can_con <- anti_join(current_candidates, can_up, by = "c_id")

  if(nrow(can_up) > 0){
    ## updated patients right now just calculates LAS Conditional Survival and die and ages patients
    updated_wl_candidates <- update_patients(patients = can_up, model = "CAS23r", elapsed_time = days_on_waitlist,
                                             pre_tx = TRUE, cap = 730, date = date)
    # updated_wl_candidates <- update_patients(patients = can_up, model = "CAS23", elapsed_time = days_on_waitlist,
    #                                           pre_tx = TRUE, cap = 365, date = date)

    can_up <- updated_wl_candidates$new_char

    ## Patients that were removed
    removed <- can_up |>
      filter(c_id %in% updated_wl_candidates$Removed$c_id) |>
      mutate(removal_day = date)

    ## Patients that died
    dead <- can_up |>
      filter(c_id %in% updated_wl_candidates$Dead$c_id) |>
      mutate(death_day = date)

    if(nrow(dead) > 0){
      waitlist_death_database <- bind_rows(waitlist_death_database, dead)
    }

    if(nrow(removed) > 0){
      waitlist_death_database <- bind_rows(waitlist_death_database, removed)
    }

    can_up_alive <- filter(can_up, c_id %in% updated_wl_candidates$Alive$c_id)

    ## Patients still alive
    alive <- bind_rows(can_con, can_up_alive) |>
      arrange(c_id)

  }else{

    alive <- current_candidates
  }

  ## update post_tx_recipients
  if(nrow(recipient_database) > 0){

    ## if the patients are at the step mark
    rec_up <- recipient_database |>
      mutate(days_after_tx = date - transplant_day) |>
      filter(days_after_tx > 0)

    rec_con <- recipient_database |>
      anti_join(rec_up, by = "c_id")

    if(nrow(rec_up) > 0){

      updated_post_tx_recipients <- update_patients(patients = rec_up, model = "CAS23r", elapsed_time = days_after_tx,
                                                    pre_tx = FALSE, cap = 1825, date = date)
      # updated_post_tx_recipients <- update_patients(patients = rec_up, model = "CAS23", elapsed_time = days_after_tx,
      #                                               pre_tx = FALSE, cap = 1825, date = date)

      rec_up <- updated_post_tx_recipients$new_char

      post_tx_dead <- rec_up |>
        filter(c_id %in% updated_post_tx_recipients$Dead$c_id) |>
        mutate(death_day = date)

      if(nrow(post_tx_dead) > 0){
        post_tx_death_database <- bind_rows(post_tx_death_database, post_tx_dead)
      }

      rec_up_alive <- filter(rec_up, c_id %in% updated_post_tx_recipients$Alive$c_id)

      post_tx_alive <- bind_rows(rec_up_alive, rec_con) |>
        arrange(c_id)

    }else{

      post_tx_alive <- recipient_database

    }
  }else{
    post_tx_alive <- recipient_database
  }

  if(nrow(alive) == 0){

    recipient_database <- post_tx_alive
    current_candidates <- alive

    l <- list(current_candidates = current_candidates,
              recipient_database = recipient_database,
              waitlist_death_database = waitlist_death_database,
              post_tx_death_database = post_tx_death_database,
              non_used_donors = non_used_donors,
              all_matches = all_matches
    )
    return(l)
  }

  ## Finds available donors
  donors_avl <- filter(donor_database, recovery_day == date)

  ## match
  ## if no availabe donors stop iteration and return what we have
  if(nrow(donors_avl) == 0){

    recipient_database <- post_tx_alive
    current_candidates <- alive

    l <- list(current_candidates = current_candidates,
              recipient_database = recipient_database,
              waitlist_death_database = waitlist_death_database,
              post_tx_death_database = post_tx_death_database,
              non_used_donors = non_used_donors,
              all_matches = all_matches
    )
    return(l)
  }

  ## Matching
  matches <- match_alg(alive, donors_avl, ...)

  if(nrow(matches) == 0) {
    ## makes sure this is updated
    recipient_database <- post_tx_alive
    current_candidates <- alive

    l <- list(current_candidates = current_candidates,
              recipient_database = recipient_database,
              waitlist_death_database = waitlist_death_database,
              post_tx_death_database = post_tx_death_database,
              non_used_donors = non_used_donors,
              all_matches = all_matches
    )
    return(l)
  }

  if(include_matches){
    matches1 <- dplyr::mutate(ungroup(matches), data = lapply(data, function(x) dplyr::select(x, c_id)))
    all_matches <- bind_rows(all_matches, matches1)
  }

  ## transplant candidate to accepted donor
  tr <- transplant_candidates(matches, recipient_database$c_id)

  tr_x <- tr |>
    group_by(d_id) |>
    dplyr::summarise(organs_rec = sum(organs_rec)) |>
    tidyr::complete(d_id = min(donors_avl$d_id):max(donors_avl$d_id), fill = list(organs_rec = 0))

  dead_donors <- left_join(donors_avl, tr_x, by = c("d_id")) |>
    mutate(organs_non_used = organs_avl - organs_rec) |>
    filter(organs_non_used > 0 |is.na(organs_non_used)) |>
    select(d_id, don_org, organs_non_used) |>
    mutate(non_used_day = date) |>
    left_join(matches, by = c("d_id", "don_org")) |>
    mutate(offers = sapply(data, nrow)) |>
    select(-data)

  ## keep track of how many donor organs were used
  if(nrow(dead_donors) >=1){
    of2 <- sapply(dead_donors$offers, function(x) ifelse(is.null(x), 0, as.numeric(x)))
    if(any(of2 == 0)){
     dead_donors$offers <- of2
    }
  }else{
    dead_donors$offers <- as.numeric(c())
  }

  ## adds them to the database
  non_used_donors <- bind_rows(non_used_donors, dead_donors)

  ## moves the patients from waitlist to transplanted and matches them to their actual donor
  new_recipients <- alive |>
    filter(c_id %in% tr$c_id) |>
    mutate(transplant_day = date) |>
    select(-days_on_waitlist) |>
    left_join(tr, by = c("c_id", "surg_type"))

  recipient_database <- bind_rows(post_tx_alive, new_recipients)

  ## updates waitlist
  current_candidates <- alive |>
    filter(!(c_id %in% tr$c_id))

  ## Adds new candidates to for the next day
  new_candidates <- filter(candidate_database, listing_day == date)

  ## if no new candidates proceed
  if(nrow(new_candidates) > 0L){
    current_candidates <- bind_rows(current_candidates, new_candidates)
  }

  l <- list(current_candidates = current_candidates,
            waitlist_death_database = waitlist_death_database,
            recipient_database = recipient_database,
            post_tx_death_database = post_tx_death_database,
            non_used_donors = non_used_donors,
            all_matches = all_matches

  )
  return(l)

}

