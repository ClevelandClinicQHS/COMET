#' Calcualate LAS scores
#'
#' @param wl_data data to calculate las
#' @param wl_model "LAS15","LAS21", "CAS23" or own coxph model for waitlist  (in days)
#' @param post_tx_model "LAS15","LAS21", "CAS23" or own coxph model for post-transplant mortality (in days)
#' @param wl_cap cap of waitlist survival
#' @param post_tx_cap cap of post-transplant survival
#' @param wl_weight relative weight of waitlist mortality
#' @param post_tx_weight relative weight of post-transplant mortality
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#' @importFrom dplyr sym
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr nest_by
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#'
#' @return a dataset of candidates and their LAS score on a 0 to 1 scale
#' @export
#'
#' @examples
#' calculate_las(cands, wl_model = "LAS21", post_tx_model = "LAS21", wl_cap = 365, post_tx_cap = 365,
#' wl_weigth = 2, post_tx_weight = 1)
calculate_las <- function(wl_data, wl_model = NULL, post_tx_model = NULL,  wl_cap = NA, post_tx_cap = NA,
                          wl_weight = NA, post_tx_weight = NA, checks = TRUE){

  if(checks){
    checkmate::assert(
      checkmate::checkClass(post_tx_model, "coxph"),
      checkmate::checkChoice(post_tx_model, c("LAS15","LAS21", "CAS23"))
    )

    checkmate::assert(
      checkmate::checkClass(wl_model, "coxph"),
      checkmate::checkChoice(wl_model, c("LAS15","LAS21", "CAS23"))
    )

    if((!is.character(wl_model) & !class(wl_model) == "coxph")){
      stop("Only coxph objects, \"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for wl_model")
    }
    if((!is.character(post_tx_model) & !class(post_tx_model) == "coxph")){
      stop("Only coxph objects, \"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for post_tx_model")
    }
  }

  wl <- trunc_days(wl_model, cand_data = wl_data, cap = wl_cap, wl = TRUE)

  post_tx <- trunc_days(post_tx_model, cand_data = wl_data, cap = post_tx_cap, wl = FALSE)

  ## joins pre and post las
  both <- left_join(wl, post_tx, by = "c_id", suffix = c(".wl",".ptx"))

  ## sclaes the data
  scaled <- scale_pre_post(both, pre = expected.wl, post = expected.ptx, pre_cap = wl_cap, post_cap = post_tx_cap,
                           pre_weight = wl_weight, post_weight = post_tx_weight)

  return(scaled)

}


#' Custom Scaling
#'
#' @param x raw value double
#' @param min minimum possible value
#' @param max maximum possible value of scale
#'
#'
#' @return a value between 0 and 1
#'
#' @examples
#' ## Should return 1
#' custom_scale(24, 0, 24)
#' ## Should return 0.07038462
#' custom_scale(0.745, -2, 37)
custom_scale <- function(x, min, max){
  y <- (x - min)/(max- min)
  return(y)
}

#' Pre and Post Survival Scaling
#'
#' @param data set containing waitlist and post expected days survived
#' @param pre variable for waitlist expected survival
#' @param post variable for post transplant expected survival
#' @param pre_weight how much you wanted to weight waitlist (default is 1)
#' @param post_weight how much you want to weight post-transplant (default is 1)
#' @param pre_cap cap on waitlist survival
#' @param post_cap cap on post-transplant survival
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return a dataset with the added variable las_0_100 which is a score 0
scale_pre_post <- function(data, pre, post, pre_weight = NA, post_weight = NA, pre_cap = NA, post_cap = NA){

  ## max number for the las i.e living the whole Post transplant and dying immediately
  d.max <- (post_weight * post_cap)
  ## min number of days
  d.min <- (-pre_weight * pre_cap)

  ## scales wl and post_tx to 0 to 100
  data2 <- data %>%
    mutate(raw_las = (post_weight * {{ post }}) - (pre_weight * {{ pre }}),
           # las_0_100 = custom_scale(raw_las, min = d.min, max = d.max) * 100,
           lu_score = custom_scale(raw_las, min = d.min, max = d.max),
           .keep = "unused") %>%
    select(-raw_las)

  return(data2)
}

