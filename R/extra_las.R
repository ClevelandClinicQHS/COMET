#' Truncated Survival days
#'
#' @param object survival object (only coxph models currently are accepted)
#' @param ... arguement given from other methods
#'
#' @return a dateset of candidates, linear predictor and mean survival
#' @export
#' @name trunc_days
#'
#' @examples
#' trunc_days("LAS21", syn_cands, 365, TRUE)
trunc_days <- function(object, ...){
  UseMethod("trunc_days")
}


#' @export
#'
#' @param cand_data candidate data
#' @param cap cap for truncation survival
#' @param wl boolean for waitlist or not
#'
#' @importFrom dplyr select
#' @importFrom stats predict
#' @importFrom dplyr mutate
#' @importFrom dplyr nest_by
#' @importFrom dplyr left_join
#' @importFrom tidyr unnest
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#'
#' @rdname trunc_days
trunc_days.coxph <- function(object, cand_data, cap = NA, wl = TRUE, ...){

  surv <- create_surv_rates(object, cap = cap)
  lp <- predict(object, newdata = cand_data, type = "lp")
  cond <- colnames(surv) %in% c("time", "surv")

  if(!all(cond)){
    str1 <- colnames(surv)[which(!cond)]
    data_n <- select(cand_data, c_id, !!str1) |>
      mutate(lp = lp) |>
      nest_by(!!sym(str1))

    surv1 <- nest_by(surv, !!sym(str1)) |>
      left_join(data_n, by = str1) |>
      mutate(expected = list(mean_survival(data.y$lp, data.x$surv))) |>
      unnest(cols = c(data.y, expected)) |>
      ungroup() |>
      select(c_id, expected) |>
      arrange(c_id)

  }else{
    surv1 <- mutate(cand_data, expected = mean_survival(lp, surv$surv)) |>
      select(c_id, expected)
  }
  return(surv1)
}


#' @param wl TRUE (waitlist) or FALSE (post_transplant)
#' @rdname trunc_days
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @export
trunc_days.character <- function(object, cand_data, cap = NA, wl = TRUE, ...){

  model <- match.arg(toupper(object), c("LAS15", "LAS21", "CAS23"))
  if(wl){
  switch(model,
         LAS15 = {
           surv <- wl_las15_survrates
           f1 <- calc_wl_las15
         },
         LAS21 = {
           surv <- wl_las21_survrates
           f1 <- calc_wl_las21
         },
         CAS23 = {
           surv <- wl_cas23_survrates
           f1 <- calc_wl_cas23
         }
  )
  }else{
  switch(model,
         LAS15 = {
           surv <- post_tx_las15_survrates
           f1 <- calc_post_tx_las15
         },
         LAS21 = {
           surv <- post_tx_las21_survrates
           f1 <- calc_post_tx_las21
         },
         CAS23 = {
           surv <- post_tx_cas23_survrates
           f1 <- calc_post_tx_cas23
         }
  )
  }

  if(is.na(cap)){cap <- max(surv$Days)}

  surv <- filter(surv, .data$Days <= cap)
  surv1 <- f1(cand_data) |>
    mutate(expected = mean_survival(lp, surv$Survival))
  return(surv1)
}


