#' Update Patients
#'
#' Updating patients status and characteristics
#'
#' @name update
#'
#' @param patients dataset of lung transplant patients
#' @param model which model to use for identifying deaths
#' @param elapsed_time name of variable for elapsed time
#' @param pre_tx is the updating for pre or post transplatn
#' @param cap cap for survival calculation
#' @param date current date (numeric)
#'
#' @importFrom stats setNames
#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#'
#' @return \code{update_patients} returns a list of candidates who were Alive, Dead, new_char for new characteristics and removed if on waiting list
#' @export
#'
#' @examples
#' identify_deaths(syn_cands, model = "CAS23", elapsed_time = days_on_waitlist,
#'    pre_tx = TRUE, cap = 365,  date = 16)
#' identify_removals(syn_cands, elapsed_time =  days_on_waitlist, cap = 2365)
#' update_patients(syn_cands, model = "CAS23r", elapsed_time =days_on_waitlist,
#'   pre_tx = TRUE, cap = 365, date = 16)
update_patients <- function(patients, model = NULL, elapsed_time, pre_tx = TRUE, cap = NA, date){

  ## string so one can join by them later
  ela_str <- as_label(enquo(elapsed_time))

  ## The idea behind this step is update patients as they age on the waitlist, will eventually be replaced with update characteristics
  patients <- mutate(patients, age = age_at_listing + (date - listing_day)/365)
  up_patients <- patients

  if(pre_tx){
    removed <- identify_removals(patients, elapsed_time = {{elapsed_time}}, cap = 2365)
    patients <- filter(patients, !(c_id %in% removed$c_id))
  }else{
    removed <- patients[0,]
  }

  ## change this to find_deaths()
  deaths <- identify_deaths(patients, model = model, elapsed_time = {{elapsed_time}}, pre_tx = pre_tx, cap = cap, date = date)

  dead <- filter(deaths, death == 1)

  alive <- filter(deaths, death == 0)

  return(list(Dead = dead, Alive = alive, Removed = removed, new_char = up_patients))

}

#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr select
#' @importFrom tidyr contains
#' @importFrom rlang sym
#' @importFrom methods is
#' @importFrom stats setNames
#'
#' @name update
#'
#' @return \code{identify_removals} returns a dataset of candidates and whether or not they were removed from the waiting list (not for being too sick)
#' @export
identify_removals <- function(patients, elapsed_time, cap = NA){

  ela_str <- as_label(enquo(elapsed_time))

  if(is.na(cap) | cap > max(rem_rt$Days)){cap <- max(rem_rt$Days)}

  rl <- select(patients, c_id, dx_grp, {{elapsed_time}}) |>
    mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
    mutate(days_ago = {{elapsed_time}} - 1) |>
    left_join(rem_rt, by = setNames(c("Days", "dx_grp"), c(ela_str, "dx_grp"))) |>
    left_join(rem_rt, by = c("days_ago" = "Days", "dx_grp" = "dx_grp"), suffix = c("_y", "_x")) |>
    mutate(cond_x = Survival_y/Survival_x,
           removal = rbinom(n = max(row_number()), size = 1, prob = 1 - cond_x)
    )

  rl <- dplyr::filter(rl, removal == 1)

  return(rl)

}

#' @name update
#'
#' @return \code{identify_deaths} returns a dataset of candidates and whether or not they experienced death or were too sick to transplant while on the waiting list
#' @export
identify_deaths <- function(patients, model = NULL, elapsed_time, pre_tx = TRUE, cap = NA, date){

  ela_str <- as_label(enquo(elapsed_time))

  if(!pre_tx){
    switch(model,
           CAS23r = {
             surv_rt_rec <- post_tx_cas23_survrates_rec365
             surv_rt <- post_tx_cas23_survratesS
             lp_f <- calc_post_tx_cas23
           },
           CAS23 = {
             surv_rt <- post_tx_cas23_survratesS
             lp_f <- calc_post_tx_cas23
           },
           LAS15 = {
             surv_rt <- post_tx_las15_survrates[-2,]
             lp_f <- calc_post_tx_las15
           },
           LAS21 = {
             surv_rt <- post_tx_las21_survrates
             lp_f <- calc_post_tx_las21
           }
    )
  }else{
    switch(model,
           CAS23r = {
             surv_rt <- wl_cas23_survrates_rec365
             lp_f <- calc_wl_cas23
           },
           CAS23 = {
             surv_rt <- wl_cas23_survratesS
             lp_f <- calc_wl_cas23
           },
           LAS15 = {
             surv_rt <- wl_las15_survrates
             lp_f <- calc_wl_las15
           },
           LAS21 = {
             surv_rt <- wl_las21_survrates
             lp_f <- calc_wl_las21
           }
    )
  }
  lp_data <- lp_f(patients)


  if(cap > max(surv_rt$Days)){cap <- max(surv_rt$Days)}

  lp_val <- sym(colnames(lp_data)[str_which(colnames(lp_data), "c_id", negate = TRUE)])

  lp_data2 <- lp_data

  dl <- left_join(lp_data2, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
    mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
    mutate(days_ago = {{elapsed_time}} - 1) |>
    left_join(surv_rt, by = setNames("Days", ela_str)) |>
    left_join(surv_rt, by = c("days_ago" = "Days"), suffix = c("_y", "_x")) |>
    mutate(cond_x = (Survival_y^exp(!!lp_val))/(Survival_x^exp(!!lp_val)),
           death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1 - cond_x)
    )

  if(any(dl[ela_str] >= 300)){

    if(pre_tx){

      dl_max <- left_join(lp_data, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
        filter({{elapsed_time}} >= 365) |>
        left_join(max_death_day, by = "dx_grp") |>
        mutate(across(contains("days"), ~ifelse(.x > max_days, max_days, ifelse(.x <0, 0, .x)))) |>
        left_join(wl_survpost365, by = setNames(c("Days", "dx_grp"), c(ela_str, "dx_grp"))) |>
        mutate(days_ago = {{elapsed_time}} - 1) |>
        left_join(wl_survpost365, by = c("days_ago" = "Days", "dx_grp"), suffix = c("_y", "_x")) |>
        mutate(cond_x = (Survival_y/Survival_x),
               death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1 - cond_x)
        )
      dlr <- filter(dl, !(c_id %in% dl_max$c_id))
      dl <- bind_rows(dlr, dl_max)
    }

    if(model == "CAS23r" & !pre_tx){

      dl_pre <- left_join(lp_data, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
        mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
        mutate(days_ago = {{elapsed_time}} - 1) |>
        filter(days_ago <= 298) |>
        left_join(surv_rt_rec, by = setNames("Days", ela_str)) |>
        left_join(surv_rt_rec, by = c("days_ago" = "Days"), suffix = c("_y", "_x")) |>
        mutate(cond_x = (Survival_y^exp(!!lp_val))/(Survival_x^exp(!!lp_val)),
               death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1 - cond_x),
        )

      dlr <- filter(dl, !(c_id %in% dl_pre$c_id))
      dl <- bind_rows(dlr, dl_pre)
    }

  }

  return(dl)

}
