#' Restricted Meant Survival Time
#'
#' @param object only character objects of 'LAS15', 'LAS21' and 'CAS23' are acceptable
#' @param ... arguement given from other methods
#'
#' @return a dateset of candidates, linear predictor and restricted mean survival time in days
#' @export
#' @name rmst
#'
#' @examples
#' rmst("LAS21", syn_cands, 365, TRUE)
rmst <- function(object, ...){
  UseMethod("rmst")
}

#' @param cand_data candidate data
#' @param cap cap for truncation survival
#' @param wl TRUE (waitlist) or FALSE (post_transplant)
#'
#' @rdname rmst
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @export
rmst.character <- function(object, cand_data, cap = NA, wl = TRUE, ...){

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
    mutate(expected = expected_survival(.data$lp, surv$Survival))
  return(surv1)
}


