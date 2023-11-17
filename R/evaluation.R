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
#' \item{post_tx_death}{ - number of of post-transplant deatjs}
#' \item{tx_ppy}{ - number of transplants per patient year}
#' \item{wld_ppy}{ - wait list deaths per patient year}
#' \item{ptd_ppy}{ - post transplant deaths per patient year}
#' \item{pt_1yr}{ - pecentage of transplant recipients that survived 1 year}
#' \item{pt_2yr}{ - pecentage of transplant recipients that survived 2 years}
#' \item{med_wlt}{ - median time on the waiting list}
#' \item{med_ptt}{ - median post transplant time}
#' \item{med_offer}{ - median offer rank of those who were transplanted}
#' \item{don_count}{ - total number of donors}
#' \item{don_util}{ - number of donor utilized}
#' \item{nu_med_offer}{ - median times a donor was offered if not utilized}
#' \item{nu_orgs}{ - number of organs not utilized}
#' }
#'
#' @importFrom rlang .data
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r1 <- run_simulation(days = 20, can_start = 1000,
#'  match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' eval_simulation(r1)
#' spec_day(r1, 5)
#' concat2(r1)
#' }
eval_simulation <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, wl_censor_date = NA, post_tx_censor_date = NA, group = NULL){

  if(!is(COMET, "COMET")){stop("Must supply a COMET object")}

  if(is.na(wl_censor_date)) wl_censor_date <- max(COMET$all_candidates$listing_day)

  ct <- concat2(COMET, min_enroll_date = min_enroll_date, max_enroll_date = max_enroll_date,
                wl_censor_date = wl_censor_date, post_tx_censor_date = post_tx_censor_date)

  dd <- filter(COMET$non_used_donors, .data$non_used_day <= wl_censor_date)

  dd1 <- dplyr::filter(dd, .data$don_org == "DLU" & .data$organs_non_used == 1) |>
    dplyr::left_join(select(ct, "d_id", "offer_rank"), by = c("d_id")) |>
    dplyr::mutate(offers = .data$offers - .data$offer_rank) |>
    dplyr::select(-"offer_rank") |>
    dplyr::bind_rows(dd)

  dd1 <- dplyr::summarise(dd1, nu_med_offer = median(.data$offers, na.rm = TRUE), nu_orgs = sum(.data$organs_non_used))

  don_stats <- dplyr::summarise(COMET$all_donors, don_count = dplyr::n(), don_util = sum(.data$don_util))

  don_stats  <- cbind(don_stats, dd1)

  can_stats <- ct |>
    dplyr::group_by({{group}}) |>
    dplyr::summarise(can_count = dplyr::n(),
                     tx_count = sum(.data$tx),
                     wait_death = sum(.data$wait_death, na.rm = TRUE),
                     removal = sum(.data$removal, na.rm = TRUE),
                     cens = dplyr::n() - .data$tx_count - .data$wait_death - .data$removal,
                     wait_death_yrs = sum(.data$wait_death_time, na.rm = TRUE)/365.25,
                     post_tx_death = sum(.data$post_tx_death, na.rm = TRUE),
                     post_tx_years = sum(.data$post_tx_time, na.rm = TRUE)/365.25,
                     tx_ppy = .data$tx_count/.data$wait_death_yrs,
                     wld_ppy = .data$wait_death/.data$wait_death_yrs,
                     ptd_ppy = .data$post_tx_death/.data$post_tx_years,
                     med_dist = median(.data$distance_nm, na.rm = TRUE),
                     pt_1yr = mean(.data$post_tx_time > 365, na.rm = TRUE),
                     pt_2yr = mean(.data$post_tx_time > 730, na.rm = TRUE),
                     med_wlt = median(.data$wait_death_time, na.rm  = TRUE),
                     med_ptt = median(.data$post_tx_time, na.rm = TRUE),
                     med_offer = median(.data$offer_rank, na.rm = TRUE)
    )

  sim_stats <- cbind(can_stats, don_stats)

  return(sim_stats)

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

  all_x <- ct |>  dplyr::filter(.data$listing_day <= day) |>
    dplyr::filter(.data$wait_death_day > day | is.na(.data$wait_death_day))

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
#'
#' @importFrom rlang .data
concat2 <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, wl_censor_date = NA, post_tx_censor_date = NA){

  if(!is(COMET, "COMET")){stop("Must supply a COMET object")}

  mld <- max(COMET$all_candidates$listing_day)

  if(is.na(max_enroll_date)) max_enroll_date <- mld
  if(is.na(wl_censor_date)) wl_censor_date <- mld
  if(is.na(post_tx_censor_date)) post_tx_censor_date <- mld

  if(post_tx_censor_date > mld){warning(paste("post_tx_censor_date:", post_tx_censor_date, "is greater than the date of maximum listing of", mld))}

  if(wl_censor_date > mld){warning(paste("wl_censor_date:", wl_censor_date, "is greater than the date of maximum listing of", mld))}

  if(max_enroll_date > mld){warning(paste("max_enroll_date:", max_enroll_date, "is greater than the date of maximum listing of", mld))}



  alls <- dplyr::bind_rows(COMET$current_candidates, COMET$waitlist_death_database, COMET$recipient_database, COMET$post_tx_death_database) |>
    dplyr::mutate(wl_event_day = pmin(.data$transplant_day, .data$death_day, .data$removal_day, na.rm = TRUE)) |>
    ## remove those listed after certain day
    dplyr::filter(.data$listing_day <= max_enroll_date) |>
    ## remove those who were experienced an event from the
    dplyr::filter(.data$wl_event_day >= min_enroll_date | is.na(.data$wl_event_day)) |>
    ## will do nothing if set to the max
    dplyr::mutate(days_after_tx = if_else(!is.na(.data$transplant_day) & .data$transplant_day >= post_tx_censor_date & is.na(.data$death_day), NA_real_, post_tx_censor_date - .data$transplant_day),
                  transplant_day = if_else(!is.na(.data$transplant_day) & .data$transplant_day > wl_censor_date, NA_real_, .data$transplant_day),
                  removal_day = if_else(!is.na(.data$removal_day) & .data$removal_day > wl_censor_date, NA_real_, .data$removal_day),
                  death_day = case_when((is.na(.data$transplant_day) & .data$death_day > wl_censor_date) ~ NA_real_,
                                        !is.na(.data$death_day) & .data$death_day > post_tx_censor_date ~ NA_real_,
                                        .default = .data$death_day
                                        ),
                  days_after_tx = case_when(
                    is.na(.data$transplant_day) ~ NA_real_,
                    .data$death_day < post_tx_censor_date ~ .data$death_day - .data$transplant_day,
                    .default = post_tx_censor_date - .data$transplant_day
                  )
                  ) |>
    ## set aside other variables
    dplyr::mutate(tx = as.numeric(!is.na(.data$transplant_day)),
                  wait_death = as.numeric(is.na(.data$transplant_day) & !is.na(.data$death_day)),
                  removal = as.numeric(!is.na(.data$removal_day)),
                  post_tx_death = ifelse(is.na(.data$transplant_day), NA_real_, as.numeric(!is.na(.data$death_day) & !is.na(.data$transplant_day))),
                  wait_death_day = dplyr::case_when(tx == 1 ~ transplant_day,
                                                    wait_death == 1 ~ death_day,
                                                    removal == 1 ~ removal_day,
                                                    tx == 0 & wait_death == 0 & removal == 0 ~ NA_real_),
                  wait_death_time = dplyr::case_when(!is.na(wait_death_day) ~ wait_death_day - .data$listing_day,
                                                     .default = wl_censor_date - .data$listing_day),
                  time_to_transplant = .data$transplant_day - .data$listing_day,
                  post_tx_time = dplyr::case_when(post_tx_death == 1 ~ death_day - .data$transplant_day,
                                                  post_tx_death == 0 ~ days_after_tx),
                  post_tx_day = .data$post_tx_time + .data$transplant_day) |>
    dplyr::arrange(.data$c_id)

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

