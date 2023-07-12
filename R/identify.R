#' Update Patients
#'
#' @name update
#'
#' @param patients dataset of lung transplant patients
#' @param model which model to use for identifying deaths
#' @param elapsed_time name of variable for elasped time
#' @param step_size how far back for conditional survival (default is 1 day)
#' @param pre_tx is deaths pre or post transplant
#' @param cap cap for suvival estiamte
#' @param date date
#'
#' @importFrom stats setNames
#'
#' @return A list of candidates who were Alive, Dead, new_char for new characteristics and removed if on waiting list
#' @export
#'
#' @examples
#' identify_deaths(cands, "CAS23", days_on_waitlist, 1, TRUE, 365, 16)
#' identify_removals(cands, days_on_waitlist, 1, TRUE, 2365, 16)
#' update_patients(cands, "CAS23r", days_on_waitlist, 1, TRUE, 365, 16)
update_patients <- function(patients, model = NULL, elapsed_time, step_size, pre_tx = TRUE, cap = NA, date){

  ## string so one can join by them later
  ela_str <- as_label(enquo(elapsed_time))

  patients <- mutate(patients, age = age_at_listing + (date - listing_day)/365)
  up_patients <- patients

  if(pre_tx){
    removed <- identify_removals(patients, elapsed_time = {{elapsed_time}}, step_size = step_size, cap = 2365)
    patients <- filter(patients, !(c_id %in% removed$c_id))
  }else{
    removed <- patients[0,]
  }

  ## change this to find_deaths()
  deaths <- identify_deaths(patients, model = model, elapsed_time = {{elapsed_time}}, step_size = step_size, pre_tx = pre_tx, cap = cap, date = date)

  dead <- filter(deaths, death == 1)

  alive <- filter(deaths, death == 0)

  return(list(Dead = dead, Alive = alive, Removed = removed, new_char = up_patients))

}

#'
#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr contains
#' @importFrom rlang sym
#' @importFrom methods is
#'
#' @name update
#'
#' @return a dataset of candidates and whether or not they were removed from the waiting list
#' @export
identify_removals <- function(patients, elapsed_time, step_size = 1, cap = NA){

  ela_str <- as_label(enquo(elapsed_time))

  if(is.na(cap) | cap > max(rem_rt$Days)){cap <- max(rem_rt$Days)}

  rl <- select(patients, c_id, dx_grp, {{elapsed_time}}) |>
    mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
    mutate(days_ago = {{elapsed_time}} - step_size) |>
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
#' @return a dataset of candidates and whether or not they experienced death on the waitlist
#' @export
identify_deaths <- function(patients, model = NULL, elapsed_time, step_size, pre_tx = TRUE, cap = NA, date){

  ela_str <- as_label(enquo(elapsed_time))

  if(is(model, "coxph")){

    if(any(str_detect(names(model$xlevels), "strata"))){
      stop("stratified coxph not supported yet")
    }
    surv_rt = create_surv_rates(model, cap = cap) |>
      rename(Days = time, Survival = surv)

    lp_data <- tibble(c_id = patients$c_id, lp = predict(model, newdata = patients, type = "lp"))

  }else{

    if(!pre_tx){
      switch(model,
             CAS23r = {
               surv_rt_rec <- post_tx_cas23_survrates_rec365
               surv_rt <- post_tx_cas23_survrates
               lp_f <- calc_post_tx_cas23
             },
             CAS23 = {
               surv_rt <- post_tx_cas23_survrates
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
  }

  if(cap > max(surv_rt$Days)){cap <- max(surv_rt$Days)}

  lp_val <- sym(colnames(lp_data)[str_which(colnames(lp_data), "c_id", negate = TRUE)])

  # dl <- left_join(lp_data, select(patients, c_id, dx_grp, {{elapsed_time}}), by = "c_id")
  lp_data2 <- lp_data

  dl <- left_join(lp_data2, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
    mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
    mutate(days_ago = {{elapsed_time}} - step_size) |>
    left_join(surv_rt, by = setNames("Days", ela_str)) |>
    left_join(surv_rt, by = c("days_ago" = "Days"), suffix = c("_y", "_x")) |>
    mutate(cond_x = (Survival_y^exp(!!lp_val))/(Survival_x^exp(!!lp_val)),
           death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1 - cond_x)
    )

  if(any(dl[ela_str] > 365)){

    if(pre_tx){

      dl_max <- left_join(lp_data, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
        filter({{elapsed_time}} > 365) |>
        left_join(max_death_day, by = "dx_grp") |>
        mutate(across(contains("days"), ~ifelse(.x > max_days, max_days, ifelse(.x <0, 0, .x)))) |>
        left_join(wl_survpost365, by = setNames(c("Days", "dx_grp"), c(ela_str, "dx_grp"))) |>
        mutate(days_ago = {{elapsed_time}} - step_size) |>
        left_join(wl_survpost365, by = c("days_ago" = "Days", "dx_grp"), suffix = c("_y", "_x")) |>
        # left_join(wl_survpost365, by = setNames("Days", ela_str)) |>
        # left_join(wl_survpost365, by = c("days_ago" = "Days"), suffix = c("_y", "_x")) |>
        mutate(cond_x = (Survival_y/Survival_x),
               death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1 - cond_x)
        )
      dlr <- filter(dl, !(c_id %in% dl_max$c_id))
      dl <- bind_rows(dlr, dl_max)
    }

    ## only if
    if(model == "CAS23r" & !pre_tx){

      dl_pre <- left_join(lp_data, select(patients, c_id, {{elapsed_time}}, dx_grp), by = c("c_id")) |>
        mutate(across(contains("days"), ~ifelse(.x > cap, cap, ifelse(.x <0, 0, .x)))) |>
        mutate(days_ago = {{elapsed_time}} - step_size) |>
        filter(days_ago <= 363) |>
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
