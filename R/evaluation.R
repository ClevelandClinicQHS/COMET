#' Evaluation
#'
#' A few functions to help look at and evaluate the results from a COMET object.
#'
#' @rdname evaluation
#'
#' @param COMET a COMET object
#' @param min_enroll_date date to enroll candidates at default is 1
#' @param max_enroll_date date to stop the enrollment period, defaults is maximum date found
#' @param wl_censor_date date to censor waitlist outcomes, default is maximum date found
#' @param post_tx_censor_date date to censor post-transplant outcomes, default is maximum date found
#' @param group summary statistics by a certain group
#'
#' @return \code{eval_simulation()} returns a tibble of summary statistics. See details for data dictionary
#'
#' @details Data dictionary for \code{eval_simulation()}
#' \itemize{
#' \item{can_count}{ - total candidate count}
#' \item{tx_count}{ - total number of transplants}
#' \item{wait_death}{ - number of deaths on the waiting list}
#' \item{removal}{ - number of removals for "other" reasons}
#' \item{cens}{ - number of people with censored outcomes or still waiting for a transplant}
#' \item{wait_death_yrs}{ - number of waitlist patient years}
#' \item{post_tx_death}{ - number of of post-transplant deahts}
#' \item{tx_ppy}{ - number of transplants per patient year}
#' \item{wld_ppy}{ - wait list deaths per patient year}
#' \item{ptd_ppy}{ - post transplant deaths per patient year}
#' \item{pt_1yr}{ - pecentage of transplant recipients that survived 1 year}
#' \item{pt_2yr}{ - pecentage of transplant recipients that survived 1 year}
#' \item{med_wlt}{ - median time on the waiting list}
#' \item{med_ptt}{ - median post transplant time}
#' \item{med_offer}{ - median offer rank of those who were transplanted}
#' \item{don_count}{ - total number of donors}
#' \item{don_util}{ - number of donor utilized}
#' \item{d_med_offer}{ - median times a donor was offered if not utilized}
#' \item{d_orgs}{ - number of organs not utilized}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r1 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1,
#'  match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' eval_simulation(r1)
#' spec_day(r1, 5)
#' concat2(r1)
#' }
eval_simulation <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, wl_censor_date = NA, post_tx_censor_date = NA, group = NULL){

  if(!is(COMET, "COMET")){stop("Must supply a COMET object")}

  ct <- concat2(COMET, min_enroll_date = min_enroll_date, max_enroll_date = max_enroll_date,
                wl_censor_date = wl_censor_date, post_tx_censor_date = post_tx_censor_date)

  dd <- COMET$discarded_donors

  dd1 <- dplyr::filter(dd, don_org == "DLU" & organs_discard == 1) |>
    dplyr::left_join(select(ct, d_id, offer_rank), by = c("d_id")) |>
    dplyr::mutate(offers = offers - offer_rank) |>
    dplyr::select(-offer_rank) |>
    dplyr::bind_rows(dd)

  dd1 <- dplyr::summarise(dd1, d_med_offer = median(offers, na.rm = TRUE), d_orgs = sum(organs_discard))

  don_stats <- dplyr::summarise(COMET$all_donors, don_count = dplyr::n(), don_util = sum(don_util))

  don_stats  <- cbind(don_stats, dd1)

  can_stats <- ct |>
    dplyr::group_by({{group}}) |>
    dplyr::summarise(can_count = dplyr::n(),
                     tx_count = sum(tx),
                     wait_death = sum(wait_death, na.rm = TRUE),
                     removal = sum(removal, na.rm = TRUE),
                     cens = dplyr::n() - tx_count - wait_death - removal,
                     wait_death_yrs = sum(wait_death_time, na.rm = TRUE)/365.25,
                     post_tx_death = sum(post_tx_death, na.rm = TRUE),
                     post_tx_years = sum(post_tx_time, na.rm = TRUE)/365.25,
                     tx_ppy = tx_count/wait_death_yrs,
                     wld_ppy = wait_death/wait_death_yrs,
                     ptd_ppy = post_tx_death/post_tx_years,
                     ## added
                     pt_1yr = mean(post_tx_time > 365, na.rm = TRUE),
                     pt_2yr = mean(post_tx_time > 730, na.rm = TRUE),
                     med_wlt = median(wait_death_time, na.rm  = TRUE),
                     med_ptt = median(post_tx_time, na.rm = TRUE),
                     med_offer = median(offer_rank, na.rm = TRUE)
    )

  sim_stats <- cbind(can_stats, don_stats)

  return(sim_stats)

}

#' @name evaluation
#'
#' @param min_enroll_date start date for daily las count
#' @param max_enroll_date end date for find daily las_count (default is maximum listing day of COMET)
#' @param LAS_FUN function to calculate LAS/CAS score
#' @param ... arguments pased to LAS_FUN
#'
#' @importFrom stats median
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble
#'
#' @return \code{daily_count_las} returns a tibble of daily counts and mean and median LAS/CAS score per day
#' @export
daily_count_las <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, LAS_FUN = calculate_las, ...){

  if(!is(COMET, "COMET")){stop("Must supply a COMET object")}

  las <- LAS_FUN(COMET$all_candidates, ...)

  if(is.na(max_date)) max_date <- max(COMET$all_candidates$listing_day)

  dy <- min_date:max_date

  sp_dy <- lapply(dy, function(x) spec_day(COMET, x))
  spdy_las <- lapply(sp_dy, function(x) left_join(x, las, by = "c_id", suffix = c("_t", "_s")))

  dy_count <- sapply(sp_dy, nrow)
  dy_las <- sapply(spdy_las, function(x) mean(x$lu_score_s))
  dy_las2 <- sapply(spdy_las, function(x) median(x$lu_score_s))

  tb <- dplyr::tibble(day = dy, count = dy_count, las_mean = dy_las, las_median = dy_las2)

  return(tb)
}

#' @name evaluation
#'
#' @param COMET a COMET object
#' @param day date of simluation
#'
#' @return \code{spec_day} returns a tibble of candidates on waiting list at a specific day
#' @export
spec_day <- function(COMET, day){
  ct <- concat2(COMET)

  if(day < min(ct$listing_day) | day > max(ct$listing_day)){
    stop(paste0(day, " must be between ", min(ct$listing_day), " and ", max(ct$listing_day)))
  }

  all_x <- ct |>  dplyr::filter(listing_day <= day) |>
    dplyr::filter(wait_death_day > day | is.na(wait_death_day))

  return(all_x)
}

#' @name evaluation
#'
#' @param COMET a COMET object
#' @param min_enroll_date date to enroll candidates at (default is 1)
#' @param max_enroll_date date to stop the enrollment period, defaults is maximum date found
#' @param wl_censor_date date to censor waitlist outcomes, default is maximum date found
#' @param post_tx_censor_date date to censor post-transplant outcomes, default is maximum date found
#'
#' @return \code{concat2} returns a tibble of candidates with their survival data
#' @export
concat2 <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, wl_censor_date = NA, post_tx_censor_date = NA){

  if(!is(COMET, "COMET")){stop("Must supply a COMET object")}

  mld <- max(COMET$all_candidates$listing_day)

  if(is.na(max_enroll_date)) max_enroll_date <- mld
  if(is.na(wl_censor_date)) wl_censor_date <- mld
  if(is.na(post_tx_censor_date)) post_tx_censor_date <- mld

  if(post_tx_censor_date > mld){warning(paste("post_tx_censor_date:", post_tx_censor_date, "is greater than the date of maximum listing of", mld))
    post_tx_censor_date <- min(mld, post_tx_censor_date)}
  if(wl_censor_date > mld){warning(paste("wl_censor_date:", wl_censor_date, "is greater than the date of maximum listing of", mld))
    wl_censor_date <- min(mld, post_tx_censor_date)}
  if(max_enroll_date > mld){warning(paste("max_enroll_date:", max_enroll_date, "is greater than the date of maximum listing of", mld))
    max_enroll_date <- min(mld, post_tx_censor_date)}


  alls <- dplyr::bind_rows(COMET$current_candidates, COMET$waitlist_death_database, COMET$recipient_database, COMET$post_tx_death_database) |>
    dplyr::mutate(wl_event_day = pmin(transplant_day, death_day, removal_day, na.rm = TRUE)) |>
    ## remove those listed after certain day
    dplyr::filter(listing_day <= max_enroll_date) |>
    ## remove those who were experienced an event from the
    dplyr::filter(wl_event_day >= min_enroll_date | is.na(wl_event_day)) |>
    ## will do nothing if set to the max
    dplyr::mutate(days_after_tx = if_else(!is.na(transplant_day) & transplant_day >= post_tx_censor_date & is.na(death_day), NA_real_, post_tx_censor_date - transplant_day),
                  transplant_day = if_else(!is.na(transplant_day) & transplant_day > wl_censor_date, NA_real_, transplant_day),
                  # death_day = if_else(!is.na(death_day) & death_day >= post_tx_censor_date, NA_real_, death_day),
                  removal_day = if_else(!is.na(removal_day) & removal_day > wl_censor_date, NA_real_, removal_day),
                  # death_day = if_else((is.na(transplant_day) & death_day >= wl_censor_date), NA_real_, death_day),
                  death_day = case_when((is.na(transplant_day) & death_day > wl_censor_date) ~ NA_real_,
                                        !is.na(death_day) & death_day > post_tx_censor_date ~ NA_real_,
                                        .default = death_day
                                        ),
                  days_after_tx = case_when(
                    is.na(transplant_day) ~ NA_real_,
                    death_day < post_tx_censor_date ~ death_day - transplant_day,
                    .default = post_tx_censor_date - transplant_day
                  )
                  ) |>
    ## set aside other variables
    dplyr::mutate(tx = as.numeric(!is.na(transplant_day)),
                  wait_death = as.numeric(is.na(transplant_day) & !is.na(death_day)),
                  removal = as.numeric(!is.na(removal_day)),
                  post_tx_death = ifelse(is.na(transplant_day), NA_real_, as.numeric(!is.na(death_day) & !is.na(transplant_day))),
                  wait_death_day = dplyr::case_when(tx == 1 ~ transplant_day,
                                                    wait_death == 1 ~ death_day,
                                                    removal == 1 ~ removal_day,
                                                    tx == 0 & wait_death == 0 & removal == 0 ~ NA_real_),
                  # wait_death_time = dplyr::case_when(!is.na(wait_death_day) ~ wait_death_day - listing_day,
                                                     # .default = waitlist_date - listing_day),
                  wait_death_time = dplyr::case_when(!is.na(wait_death_day) ~ wait_death_day - listing_day,
                                                     .default = wl_censor_date - listing_day),
                  time_to_transplant = transplant_day - listing_day,
                  post_tx_time = dplyr::case_when(post_tx_death == 1 ~ death_day - transplant_day,
                                                  post_tx_death == 0 ~ days_after_tx),
                  post_tx_day = post_tx_time + transplant_day) |>
    dplyr::arrange(c_id)

  # vctrs::vec_assigr1
  return(alls)
}

## need to build these
#
# print.COMET <- function(x){
#
# }
#
# summary.COMET <- function(x){
#
# }
# validate_COMET <- function(x){
#
# }

