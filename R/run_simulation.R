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
                           match_alg = match_cas(),
                           ...,
                           seed = NULL, desired = "random", return_params = FALSE, include_matches = FALSE){

  data_present()

  call1 <- match.call(expand.dots = TRUE)

  set.seed(seed = seed)
  ## For timing purposes
  tic <- Sys.time()
  match_f <- match_alg
  m1 <- dplyr::quo_name(enquo(match_alg))
  lu_f <- if(grepl("las", m1)){calculate_las}else{calculate_sub_cas}

  if(is.numeric(can_start)){

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

  dl <- dplyr::filter(new_donors, .data$don_util == 1)

  ## creating empty list
  recipient_d <- dplyr::tibble(c_id = 0)[0,]

  waitlist_death_d <- dplyr::tibble()

  post_tx_death_d <- dplyr::tibble()

  non_used_donors <- dplyr::tibble()

  all_matches <- dplyr::tibble()

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

  for(i in ds){
    test_i <- iteration(i,
                        cl, dl, include_matches = include_matches, updated_list = test_i
                        , match_alg = match_alg, ...

    )
    #### progress bar
    if(i %in% qs){
      cat(paste0(names(qs)[max(which(i == qs))], " done "))
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
    l <- append(pp, l)
  }

  class(l) <- "COMET"

  return(l)

}
