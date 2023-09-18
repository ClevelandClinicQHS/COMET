#' Calculate LAS scores
#'
#' @param wl_data data to calculate las
#' @param wl_model which waitlist model to use.
#' Options are "LAS15","LAS21", "CAS23"
#' @param post_tx_model which post transplant model to use.
#' Options are "LAS15","LAS21", "CAS23"
#' @param wl_cap cap of waitlist survival
#' @param post_tx_cap cap of post-transplant survival
#' @param wl_weight relative weight of waitlist mortality
#' @param post_tx_weight relative weight of post-transplant mortality
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every day the simulation is iterated
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
#' @importFrom methods is
#' @importFrom rlang .data
#'
#' @return a dataset of candidates and their LAS score on a 0 to 1 scale and linear predictors for waitlist and post-transplant components.
#' To get LAS/CAS score simply multiply by 100.
#' @export
#'
#' @examples
#' calculate_las(syn_cands, wl_model = "LAS21", post_tx_model = "LAS21",
#'  wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
calculate_las <- function(wl_data, wl_model = NULL, post_tx_model = NULL,  wl_cap = NA, post_tx_cap = NA,
                          wl_weight = NA, post_tx_weight = NA, checks = TRUE){

  if(checks){

    if((!is.character(wl_model))){
      stop("\"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for wl_model")
    }
    if((!is.character(post_tx_model))){
      stop("\"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for post_tx_model")
    }
  }

  wl <- rmst(wl_model, cand_data = wl_data, cap = wl_cap, wl = TRUE)

  post_tx <- rmst(post_tx_model, cand_data = wl_data, cap = post_tx_cap, wl = FALSE)

  ## joins pre and post las
  both <- left_join(wl, post_tx, by = "c_id", suffix = c(".wl",".ptx"))

  ## sclaes the data
  scaled <- scale_pre_post(both, pre = .data$expected.wl, post = .data$expected.ptx,
                           pre_cap = wl_cap, post_cap = post_tx_cap,
                           pre_weight = wl_weight, post_weight = post_tx_weight)

  return(scaled)

}


custom_scale <- function(x, min, max){
  y <- (x - min)/(max- min)
  return(y)
}


#' @importFrom dplyr mutate
#' @importFrom dplyr select
scale_pre_post <- function(data, pre, post, pre_weight = NA, post_weight = NA, pre_cap = NA, post_cap = NA){

  ## max number for the las i.e living the whole Post transplant and dying immediately
  d.max <- (post_weight * post_cap)
  ## min number of days
  d.min <- (-pre_weight * pre_cap)

  ## scales wl and post_tx to 0 to 100
  data2 <- data |>
    mutate(raw_las = (post_weight * {{ post }}) - (pre_weight * {{ pre }}),
           # las_0_100 = custom_scale(raw_las, min = d.min, max = d.max) * 100,
           lu_score = custom_scale(raw_las, min = d.min, max = d.max),
           .keep = "unused") |>
    select(-raw_las)

  return(data2)
}

