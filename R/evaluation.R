#' Evaluation
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
#' @return \code{eval_simulation()} returns a tibble of summary statistics
#'
#' @details
#'  \item
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r1 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1,
#'  match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' eval_simulation(r1)
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

#'@name evaluation
#'
#' @param min_date start date for daily las count
#' @param max_date end date for find daily las_count (default is maximum listing day of COMEt)
#' @param LAS_FUN function to calculate LAS/CAS score
#' @param ... arguments pased to LAS_FUN
#'
#' @importFrom stats median
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble
#'
#' @return a tibble of daily counts and mean and median las per COMET
#' @export
daily_count_las <- function(COMET, min_date = 1, max_date = NA, LAS_FUN = calculate_las, ...){

  las <- LAS_FUN(COMET$all_candidates, ...)

  if(is.na(max_date)) max_date <- max(COMET$all_candidates$listing_day)

  dy <- min_date:max_date

  sp_dy <- lapply(dy, function(x) spec_day(COMET, x))
  spdy_las <- lapply(sp_dy, function(x) left_join(x, las, by = "c_id", suffix = c("_t", "_s")))

  dy_count <- sapply(sp_dy, nrow)
  dy_las <- sapply(spdy_las, function(x) mean(x$lu_score_s))
  dy_las2 <- sapply(spdy_las, function(x) median(x$lu_score_s))

  tb <- tibble(day = dy, count = dy_count, las_mean = dy_las, las_median = dy_las2)

  return(tb)
}

#' @name evaluation
#'
#' @param COMET a COMET object
#' @param enroll_date last date to enroll for simulation
#' @param censor_date last date to censor individuals post-transplant
#'
#' @return a tibble with simulation data and calucated survival data
cutoff <- function(COMET, enroll_date, waitlist_date, post_tx_date){

  c0 <- concat(COMET)

  c00 <- dplyr::filter(c0, listing_day <= enroll_date) |>
    dplyr::mutate(#days_after_tx = if_else(!is.na(transplant_day) & transplant_day > post_tx_date & is.na(death_day), NA_real_, post_tx_date - transplant_day),

                  transplant_day = if_else(!is.na(transplant_day) & transplant_day > waitlist_date, NA_real_, transplant_day),
                  death_day = if_else(!is.na(death_day) & death_day > post_tx_date, NA_real_, death_day),
                  removal_day = if_else(!is.na(removal_day) & removal_day > waitlist_date, NA_real_, removal_day),
                  death_day = if_else((is.na(transplant_day) & death_day > waitlist_date), NA_real_, death_day),
                  days_after_tx = case_when(
                    is.na(transplant_day) ~ NA,
                    death_day < post_tx_date ~ death_day - transplant_day,
                    .default = post_tx_date - transplant_day
                  )

    ) |>
    dplyr::mutate(tx = as.numeric(!is.na(transplant_day)),
                  wait_death = as.numeric(is.na(transplant_day) & !is.na(death_day)),
                  removal = as.numeric(!is.na(removal_day)),
                  # post_tx_death = as.numeric(!is.na(death_day) & !is.na(transplant_day) & !is.na(removal_day)),
                  post_tx_death = if_else(is.na(transplant_day), NA, as.numeric(!is.na(death_day) & !is.na(transplant_day))),
                  wait_death_day = dplyr::case_when(tx == 1 ~ transplant_day,
                                                    wait_death == 1 ~ death_day,
                                                    removal == 1 ~ removal_day,
                                                    tx == 0 & wait_death == 0 ~ NA_real_),
                  wait_death_time = dplyr::case_when(!is.na(wait_death_day) ~ wait_death_day - listing_day,
                                                     .default = waitlist_date - listing_day),
                  time_to_transplant = transplant_day - listing_day,
                  # post_tx_time = days_after_tx,
                  post_tx_time = dplyr::case_when(post_tx_death == 1 ~ death_day - transplant_day,
                                                  post_tx_death == 0 ~ days_after_tx),
                  post_tx_day = post_tx_time + transplant_day) |>
    arrange(c_id)

  return(c00)
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

#' Title
#'
#' @name evaluation
#'
#' @param COMET a COMET object
#' @param min_enroll_date date to enroll candidates at default is 1
#' @param max_enroll_date date to stop the enrollment period, defaults is maximum date found
#' @param wl_censor_date date to censor waitlist outcomes, default is maximum date found
#' @param post_tx_censor_date date to censor post-transplant outcomes, default is maximum date found
#'
#' @return a tibble of candidates and time information
#' @export
#'
#' @examples
#' \dontrun{concat2(r1)}
concat2 <- function(COMET, min_enroll_date = 1, max_enroll_date = NA, wl_censor_date = NA, post_tx_censor_date = NA){

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
                  death_day = case_when((is.na(transplant_day) & death_day >= wl_censor_date) ~ NA_real_,
                                        !is.na(death_day) & death_day >= post_tx_censor_date ~ NA_real_,
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
    arrange(c_id)

  # vctrs::vec_assigr1
  return(alls)
}

#' Simulation Evaluation Helps
#'
#' @param COMET a COMET object
#'
#' @return a complete tibble of all the COMET's list, along with survival information
#' @export
concat <- function(COMET){


  alls <- dplyr::bind_rows(COMET$current_candidates, COMET$waitlist_death_database, COMET$recipient_database, COMET$post_tx_death_database) |>
    dplyr::mutate(tx = as.numeric(!is.na(transplant_day)),
                  wait_death = as.numeric(is.na(transplant_day) & is.na(removal_day) & !is.na(death_day)),
                  removal = as.numeric(!is.na(removal_day)),
                  post_tx_death = if_else(is.na(transplant_day), NA, as.numeric(!is.na(death_day) & !is.na(transplant_day))),
                  wait_death_day = dplyr::case_when(tx == 1 ~ transplant_day,
                                                    wait_death == 1 ~ death_day,
                                                    removal == 1 ~ removal_day,
                                                    tx == 0 & wait_death == 0 ~ NA_real_),
                  wait_death_time = dplyr::case_when(!is.na(wait_death_day) ~ wait_death_day - listing_day,
                                                     .default = days_on_waitlist),
                  time_to_transplant = transplant_day - listing_day,
                  # post_tx_time = days_after_tx,
                  post_tx_time = dplyr::case_when(post_tx_death == 1 ~ death_day - transplant_day,
                                                  post_tx_death == 0 ~ days_after_tx),
                  post_tx_day = post_tx_time + transplant_day)

  # vctrs::vec_assign()
  return(alls)
}


#' Grab those alive on specific day on the waitlist
#'
#' @param COMET a COMET object
#' @param day a number
#'
#' @return a tibble of candidates on waitling list at a specific day
#' @export
#'
#' @examples
#' ## will add later
spec_day <- function(COMET, day){
  ct <- concat2(COMET)

  if(day < min(ct$listing_day) | day > max(ct$listing_day)){
    stop(paste0(day, " must be between ", min(ct$listing_day), " and ", max(ct$listing_day)))
  }

  all_x <- ct |>  dplyr::filter(listing_day <= day) |>
    dplyr::filter(wait_death_day > day | is.na(wait_death_day))

  return(all_x)
}



##
# r0 <- rs2[[1]]
# ct2b <- cutoff(r0, 730, 730, 802) |> select(-c(male:pcpra)) |> arrange(c_id)
# ct2g <- concat(r0) |> select(-c(male:pcpra)) |> arrange(c_id) |> filter(listing_day <= 730)
#
# ct2in <- anti_join(ct2g, ct2b)
#
# gin <- ct2g |> filter(c_id %in% ct2in$c_id)
# bin <- ct2b |> filter(c_id %in% ct2in$c_id)
#
# ## One on waitlist, no die, no tranplant, no removal
# ## Good
# gwl <- filter(gin, is.na(death_day) & is.na(transplant_day) & is.na(removal_day))
# bwl <- bin |> filter(c_id %in% gwl$c_id)
# ## all 72 days longer
# mean((gwl$wait_death_time - bwl$wait_death_time) == 72)
# nrow(gwl)
#
# gin2 <- anti_join(gin, gwl)
# bin2 <- anti_join(bin, bwl)
#
# ## those removed
# ## These are good,
# gr <- filter(gin2, !is.na(removal_day))
# ## all removed after day 730
# mean(gr$removal_day >730)
# br <- filter(bin2, c_id %in% gr$c_id)
# mean((gr$wait_death_day - 730) == (gr$wait_death_time - br$wait_death_time))
#
# ##
# gin3 <- anti_join(gin2, gr)
# bin3 <- anti_join(bin2, br)
#
# ### waitlist death
# gwld <- filter(gin3, is.na(transplant_day) & !is.na(death_day))
# bwld <- filter(bin3, c_id %in% gwld$c_id)
# ## Good
# mean((gwld$death_day - 730) == (gwld$wait_death_time - bwld$wait_death_time))
#
# ## those transplanted
# gtt <- filter(gin3, !is.na(transplant_day))
# btt <- filter(bin3, c_id %in% gtt$c_id)
#
# ## Transplant and no deaths
# gtn <- filter(gtt, is.na(death_day))
# btn <- filter(btt, c_id %in% gtn$c_id)
#
# waldo::compare(head(gtn), head(btn))
# waldo::compare(tail(gtn), tail(btn))
#
#
#
# mean((gtn$transplant_day - 730) == (gtn$wait_death_time - btn$wait_death_time))
#
# # mean(gtn$li)
#
# ## those who died post transplant
# gptd <- filter(gtt, !is.na(death_day))
# bptd <- filter(btt, c_id %in% gptd$c_id)
#
# waldo::compare(head(gptd), head(bptd))
# waldo::compare(tail(gtn), tail(btn))
#
# mean((gptd$transplant_day - 730) == (gptd$wait_death_time - bptd$wait_death_time))
#
#
