#' Calculate continuous distribution
#'
#' @param match_data dataset of patients containing covaraites for las calculations and other continuous distribution factors
#' @param efficienty_weight weight given for efficiency (cost and distance)
#'
#' @return a dataset complete with the efficient/distance metric
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr between
#'
#' @examples
#' ## will add real examples later
#' ## This needs to be thought out, only the distance changes with the donors
calculate_cas_dist <- function(match_data, efficiency_weight = NA, cost_weight = NA, distance_weight = NA){

  if(!is.na(cost_weight) & !is.na(distance_weight)){
    efficiency_weight1 <- cost_weight
    efficiency_weight2 <- distance_weight
  }else{
    efficiency_weight2 <- efficiency_weight1 <- efficiency_weight/2
  }

  data <- match_data |>
    mutate(
      pcost = 6.3 * distance_nm +
        as.numeric(distance_nm > 43.44) * 247.63 * (distance_nm - 43.44) -
        as.numeric(distance_nm > 67.17) * 104.44 * (distance_nm - 67.17) -
        as.numeric(distance_nm > 86.90) * 128.34 * (distance_nm - 86.90),
      lu_score = lu_score +
        efficiency_weight1/2 * (1- pcost/118981.1) +
        efficiency_weight2/2 *
        (as.numeric(distance_nm <= 45) +
           as.numeric(between(distance_nm, 45, 90)) * (1 - 0.15/45 * (distance_nm - 45)) +
           as.numeric(distance_nm >=90) * 0.875/(1 + exp(0.0025 * (distance_nm - 1500))))
    )

  return(data)

}
#' Calculate continuous distribution
#'
#' @param data dataset of patients containing covaraites for las calculations and other continuous distribution factors
#' @param wl_weight weight applied to waitlist model
#' @param post_tx_weight weight applied to post transplant model
#' @param bio_weight weight applied to biologoical measures (height, abo (blood), and hla)
#' @param peds_weight weight applied to give a bonus to pediatric patients
#' @param pld_weight weight given as bonus if a prior living donor
#' @param abo_weight weight for blood type compatability (if not specified will default to 1/3 of bio weight)
#' @param height_weight weight for height compatability (if not specified will default to 1/3 of bio weight)
#' @param cpra_weight weight for cpra compatability (if not specified will default to 1/3 of bio weight)
#'
#' @return a dataset of cas calcualted metrics without the efficiency/distance metric
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr nest_by
#' @importFrom dplyr sym
#' @importFrom tidyr unnest
#'
#' @examples
#' ## will add real examples later
#' ## This needs to be thought out, only the distance changes with the donors
#' cands <- gen_and_spawn_candidates(days = 10)
#' cas_rank <- calculate_cas_nodist(cands, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25, bio_weight = 0.15, pld_weight = 0.05, peds_weight = 0.2)
calculate_cas_nodist <- function(data, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = NA, post_tx_weight = NA, bio_weight = NA,
                                 peds_weight = NA, pld_weight = NA, wl_cap = NA, post_tx_cap = NA, abo_weight = NA, height_weight = NA, cpra_weight = NA){

  if(!is.na(abo_weight) & !is.na(height_weight) & !is.na(cpra_weight)){
    bio_weight1 <- abo_weight
    bio_weight2 <- height_weight
    bio_weight3 <- cpra_weight

  }else{
    bio_weight1 <- bio_weight2 <- bio_weight3 <- bio_weight/3
  }

  # weights <- wl_weight + post_tx_weight + bio_weight + peds_weight + pld_weight

  # if(round(weights, 0) > 1L){
  #   warning("sum of weights > 1")
  # }

  if((!is.character(wl_model) & !class(wl_model) == "coxph")){
    stop("Only coxph objects, \"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for wl_model")
  }
  if((!is.character(post_tx_model) & !class(post_tx_model) == "coxph")){
    stop("Only coxph objects, \"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for post_tx_model")
  }


  # if(class(wl_model) %in%  "coxph"){
    wl <- trunc_days(wl_model, cand_data = data, cap = wl_cap, wl = TRUE)
  # }

  # if(class(post_tx_model) %in%  "coxph"){
    post_tx <- trunc_days(post_tx_model, cand_data = data, cap = post_tx_cap, wl = FALSE)
  # }



  data <- left_join(data, wl, by = "c_id") |>
    left_join(post_tx, by = "c_id", suffix = c(".wl",".ptx"))

  data <- data |>
    mutate(
      lu_score = wl_weight * (25^(1 - expected.wl/wl_cap) - 1)/24 +
        post_tx_weight * expected.ptx/post_tx_cap +
        bio_weight1 * (100^pabo - 1)/99 + bio_weight2 * (100^pcpra - 1)/99 + bio_weight3 * (100^phgt - 1)/99 +
        peds_weight * as.numeric(age < 18) +
        pld_weight * pld,
      ov_rank = rank(-lu_score)
    ) |>
    select(c_id, lu_score, ov_rank)

  return(data)
}
