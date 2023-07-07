#' Run Simulation
#'
#' @param days number of days to simulate
#' @param can_start number of candidates to start on the waitlist, they will be randomly selected to be mor listing day, all of these candidates will have a negative c_id and listing_day
#' @param waitlist_update_freq how often to update waitlist patients
#' @param post_tx_update_freq how often to update post_tx patients
#' @param match_alg function of how to screen and match individuals
#' @param extra_days how many days to simulate deaths
#' @param ... arguments for match_alg
#' @param seed seed to set
#' @param desired random or mean for parameter generation
#' @param return_params TRUE or FALSE see the paramters used for generating donors and candidates
#' @param include_matches TRUE or FALSE, whether or not the include the donor and candidates matches
#'
#' @importFrom stats rgamma
#' @importFrom utils head
#'
#' @return list of full donors and candidates simluated at beginning and final datasets, who's left on waitlist, transplanted, death on both and discarded donors
#' @export
#'
#' @examples
#'
#' r1 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1, match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15", wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' r2 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1, match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1)
run_simulation <- function(days, can_start = 1250, waitlist_update_freq = 1, post_tx_update_freq = 1,
                           match_alg = match_cas(), extra_days = 0,
                           ...,
                           seed = NULL, desired = "random", return_params = FALSE, include_matches = FALSE){

  call1 <- match.call(expand.dots = TRUE)

  ## Other warning and stuff to check
  # model <- match.arg(model, c("LAS15", "LAS21", "CAS23"))

  set.seed(seed = seed)
  ## For timing purposes
  tic <- Sys.time()
  ### Starting the simulation with 1000 candidates to start
  match_f <- match_alg
  # m1 <- paste(quote(match_las))
  m1 <- dplyr::quo_name(enquo(match_alg))
  lu_f <- if(grepl("las", m1)){calculate_las}else{calculate_sub_cas}

  if(is.numeric(can_start)){


  # cust_day <- function(x){
  #   dy <- ceiling(x/350)+10
  #   dy <- dy*50
  #   q1 <- gen_and_spawn_candidates(days = dy) |>
  #     # dplyr::left_join(odd3, by = "dx_grp") %>%
  #     # dplyr::slice_sample(n = x, weight_by = o1) %>%
  #     # dplyr::select(-o1) %>%
  #     # dplyr::mutate(listing_day = -ceiling(rgamma(x, shape = 0.68, scale = 500))) %>%
  #     # dplyr::arrange(listing_day) %>%
  #     # dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1)
  #     # dplyr::mutate(listing_day = -ceiling(rgamma(nrow(.), shape = 0.68, scale = 500))) |>
  #     dplyr::mutate(listing_day = -round(rgamma(n = max(dplyr::row_number()), shape = 0.6816127092994521685654, rate = 0.0017683888980645867499))) |>
  #     # dplyr::mutate(listing_day = -ceiling(rgamma(n = max(dplyr::row_number()), shape = 0.45857445548312275596, rate = 0.0011901870568239542817))) |>
  #     dplyr::filter(hy_removal_day > -listing_day) |>
  #     dplyr::left_join(odd3, by = "dx_grp") |>
  #     dplyr::slice_sample(n = x, weight_by = dx_weight) |>
  #     dplyr::select(-c(dx_weight)) |>
  #     dplyr::arrange(listing_day) |>
  #     dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1)
  #
  #   return(q1)
  # }

    # cust_day <- function(x){
    #   dy <- ceiling(x/350)+10
    #   dy <- dy*50
    #   q1 <- gen_and_spawn_candidates(days = dy)|>
    #     dplyr::left_join(prior_dist_gammas, by = dplyr::join_by(dx_grp)) |>
    #     dplyr::rowwise() |>
    #     dplyr::mutate(listing_day = -floor(rsamp(rgamma, n = 1, limits = c(0, max(1, hy_removal_day-1)), shape = shape, rate = rate))) |>
    #     # dplyr::mutate(listing_day = -round(rsamp(rgamma, n = 1, limits = c(0, hy_removal_day-1), shape = 0.6816127092994521685654, rate = 0.0017683888980645867499))) |>
    #     dplyr::ungroup() |>
    #     dplyr::select(-c(shape, rate))
    #     # dplyr::mutate(listing_day = -round(rgamma(n = max(dplyr::row_number()), shape = 0.6816127092994521685654, rate = 0.0017683888980645867499))) |>
    #     # dplyr::filter(hy_removal_day > -listing_day)
    #   # |>
    #   #   dplyr::left_join(odd3, by = "dx_grp") |>
    #   # dplyr::slice_sample(n = x, weight_by = dx_weight) |>
    #   # dplyr::select(-c(dx_weight)) |>
    #   # dplyr::arrange(listing_day) |>
    #   # dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1)
    #
    #   q1lu <- lu_f(q1, ...) |> select(c_id, lu_score) |> left_join(q1, by = join_by(c_id)) |>
    #     # dplyr::slice_sample(n = x, weight_by = (lu_score^(-5))) |>
    #     dplyr::slice_sample(n = x, weight_by = (lu_score^(-3))) |>
    #     # dplyr::select(-c(dx_weight)) |>
    #     dplyr::arrange(listing_day) |>
    #     dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1) |>
    #     dplyr::select(-lu_score)
    #
    #   return(q1lu)
    # }

    cust_day <- function(x){
      dy <- ceiling(x/350)+10
      dy <- dy*50
      q1 <- gen_and_spawn_candidates(days = dy) |>
        dplyr::left_join(odd3, by = dplyr::join_by(dx_grp))

      q1lu <- calculate_las(q1, wl_model = "LAS15", post_tx_model = "LAS15", wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365) |>
        dplyr::select(c_id, lu_score) |>
        dplyr::left_join(q1, by = dplyr::join_by(c_id)) |>
        dplyr::slice_sample(n = x*3, weight_by = dx_weight) |>
        dplyr::slice_sample(n = x, weight_by = (lu_score^(-3)))

      q1o <- sample(1:x, x, prob = q1lu$lu_score)

      q2 <- q1lu[q1o,] |>
        dplyr::select(-listing_day) |>
        tidyr::nest(.by = dx_grp) |>
        dplyr::left_join(prior_dist_gammas, by = dplyr::join_by(dx_grp)) |>
        dplyr::mutate(listing_day = purrr::pmap(list(data, shape, rate), ~sort(round(-rgamma(n = nrow(..1), shape = ..2, rate = ..3))))) |>
        dplyr::select(dx_grp, data, listing_day) |>
        tidyr::unnest(cols = c(data, listing_day)) |>
        dplyr::arrange(listing_day) |>
        dplyr::relocate(listing_day, .after = c_id) |>
        dplyr::mutate(c_id = -dplyr::n() + dplyr::row_number()-1) |>
        dplyr::arrange(c_id) |>
        dplyr::select(-c(lu_score, dx_weight))


      return(q2)
    }

  old_candidates <- cust_day(can_start)

  }else{

  old_candidates <- can_start
}
  ## initializing
  if(return_params){
    new_candidates_p <- gen_and_spawn_candidates(days = days, desired = desired, return_params = TRUE)
    new_donors_p <- gen_and_spawn_donors(days = days, desired = desired, return_params = TRUE)

    new_candidates <- new_candidates_p$data
    new_donors <- new_donors_p$data
  }else{

    new_candidates <- gen_and_spawn_candidates(days = days, desired = desired)

    new_donors <- gen_and_spawn_donors(days = days, desired = desired)
  }

  cl <- dplyr::bind_rows(old_candidates, new_candidates)

  dl <- dplyr::filter(new_donors, don_util == 1)

  ## creating empty lists
  current_candidates_d <- cl |>
    dplyr::filter(c_id == -Inf)

  recipient_d <- cl |>
    dplyr::filter(c_id == -Inf) |>
    dplyr::mutate(transplant_day = NA_real_)

  waitlist_death_d <- cl |>
    dplyr::filter(c_id == -Inf) |>
    dplyr::mutate(death_day = NA_real_, days_on_waitlist = NA_real_)

  post_tx_death_d <- cl |>
    dplyr::filter(c_id == -Inf) |>
    dplyr::mutate(transplant_day = NA_real_, d_id = NA_real_, death_day = NA_real_)

  discarded_donors <- dl |>
    dplyr::filter(d_id == -Inf) |>
    dplyr::select(d_id) |>
    dplyr::mutate(discard_day = NA_real_)

  ## add matches

  # match_args <- dots[rlang::fn_fmls_names(match_f)]
  # match_args <- match_args[lapply(match_args, length)>0]
  # match_args <- purrr::prepend(purrr::prepend(match_args, list(head(dl, 5))), list(old_candidates))
  all_matches <- match_f(old_candidates, head(dl, 5), ...) |>
    # tidyr::unnest(cols = data) |>
    dplyr::filter(d_id == -Inf) #|>
    # tidyr::nest()
  # all_matches <- do.call(match_f, match_args) %>% dplyr::nest_by(d_id) %>% dplyr::filter(d_id == -Inf)

  iter0 <- list(
    current_candidates = old_candidates,
    recipient_database = recipient_d,
    waitlist_death_database = waitlist_death_d,
    post_tx_death_database = post_tx_death_d,
    discarded_donors = discarded_donors,
    all_matches = all_matches
  )

  ## iteration

  test_i <- iter0
  if(extra_days >0L){days <- days + extra_days}
  ds <- 1:days
  ### For progress bar in timing may eventually remove
  qs <- ceiling(quantile(ds, probs = seq(.1, 1, .1)))

  #################
  for(i in ds){
    test_i <- iteration(i,
                        cl, dl, include_matches = include_matches, updated_list = test_i, waitlist_update_freq = waitlist_update_freq, post_tx_update_freq = post_tx_update_freq
                        , match_alg = match_alg, ...

    )
    #### progress bar
    if(i %in% qs){
      print(glue::glue(names(qs)[which(i == qs)], " done"))
    }
  }
  ## Timing
  toc <- Sys.time()
  print(toc - tic)

  og <- list(call = call1,
             time_run = toc - tic,
             all_candidates = cl,
             all_donors = new_donors)

  l <- append(og, test_i)

  if(!include_matches){
    l <- l[-length(l)]
  }


  if(return_params){
    pp <- list("params" = append(new_donors_p$params, new_candidates_p$params))
    # return(params)
    l <- append(pp, l)
  }

  class(l) <- "COMET"

  return(l)

}


######

