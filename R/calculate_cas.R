#' Calculate Composite Allocation Score (CAS)
#'
#' Calcualte the subCAS
#'
#' @param data dataset of patients containing covaraites for las calculations and other continuous distribution factors
#' @param wl_model model to use in waitlist component of the CAS
#' @param post_tx_model model to use in the post-transplant component of the CAS
#' @param wl_cap number of days to cap the waitlist component CAS score at CAS
#' @param post_tx_cap number of days to cap the post transplant component of the CAS
#' @param wl_weight weight applied to waitlist model
#' @param post_tx_weight weight applied to post transplant model
#' @param bio_weight weight applied to biologoical measures (height, abo (blood), and hla)
#' @param peds_weight weight applied to give a bonus to pediatric patients
#' @param pld_weight weight given as bonus if a prior living donor
#' @param abo_weight weight for blood type compatability (if not specified will default to 1/3 of bio weight)
#' @param height_weight weight for height compatability (if not specified will default to 1/3 of bio weight)
#' @param cpra_weight weight for cpra compatability (if not specified will default to 1/3 of bio weight)
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#'
#' @return \code{calculate_sub_cas} a dataset of candidate, CAS score on a 0 to 1 scale (lu_score) without the efficiency weight,
#' and ov_rank which is the overall rank of each candidate. 1 is the highest CAS score
#' \code{calculate_cas_dist} a dataset of matches with lu_score that contains the efficiency weight. It will contain the estimaed cost (pcost) for efficiency calculation.
#' @export
#'
#' @rdname calculate_cas
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
#' @importFrom dplyr between
#' @importFrom methods is
#'
#' @examples
#' calculate_sub_cas(data = syn_cands, wl_model = "CAS23", post_tx_model = "CAS23",
#'  wl_weight = .25, post_tx_weight = .25, bio_weight = .15, peds_weight = .2,
#'  pld_weight = 0.05, wl_cap = 365, post_tx_cap = 1825)
#'
#' calculate_cas_dist(syn_matches, efficiency_weight = 0.1)
calculate_sub_cas <- function(data, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = NA, post_tx_weight = NA, bio_weight = NA,
                                 peds_weight = NA, pld_weight = NA, wl_cap = NA, post_tx_cap = NA, abo_weight = NA, height_weight = NA, cpra_weight = NA, checks = TRUE){

  if(!is.na(abo_weight) & !is.na(height_weight) & !is.na(cpra_weight)){
    bio_weight1 <- abo_weight
    bio_weight2 <- height_weight
    bio_weight3 <- cpra_weight

  }else{
    bio_weight1 <- bio_weight2 <- bio_weight3 <- bio_weight/3
  }
  if(checks){
    if(is.na(bio_weight1)){
      stop("abo_weight is missing")
    }
    if(is.na(bio_weight2)){
      stop("height_weight is missing")
    }
    if(is.na(bio_weight3)){
      stop("cpra_weight is missing")
    }
    if(!is.character(wl_model)){
      stop("\"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for wl_model")
    }
    if((!is.character(post_tx_model))){
      stop("\"LAS15\",\"LAS21\" or \"CAS23\" are acceptable options for post_tx_model")
    }
  }

  wl <- trunc_days(wl_model, cand_data = data, cap = wl_cap, wl = TRUE)

  post_tx <- trunc_days(post_tx_model, cand_data = data, cap = post_tx_cap, wl = FALSE)

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

#' @name calculate_cas
#'
#' @param match_data dataset of patients containing covaraites for las calculations and other continuous distribution factors
#' @param efficiency_weight weight given for efficiency (cost and distance)
#' @param cost_weight weight given for cost part of CAS (if not specified is half of efficiency weight)
#' @param distance_weight weight given for distance part of CAS (if not specified is half of efficiency weight)
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#' @return a dataset complete with the efficient/distance metric
#' @export
calculate_cas_dist <- function(match_data, efficiency_weight = NA, cost_weight = NA, distance_weight = NA, checks = TRUE){

  if(!is.na(cost_weight) & !is.na(distance_weight)){
    efficiency_weight1 <- cost_weight
    efficiency_weight2 <- distance_weight
  }else{
    efficiency_weight2 <- efficiency_weight1 <- efficiency_weight/2
  }
  if(checks){
  if(is.na(efficiency_weight1)){
    stop("cost_weight is missing")
  }
  if(is.na(efficiency_weight2)){
    stop("Distance_weight is missing")
  }
  }

  data <- match_data |>
    mutate(
      pcost = 6.3 * distance_nm +
        as.numeric(distance_nm > 43.44) * 247.63 * (distance_nm - 43.44) -
        as.numeric(distance_nm > 67.17) * 104.44 * (distance_nm - 67.17) -
        as.numeric(distance_nm > 86.90) * 128.34 * (distance_nm - 86.90),
      lu_score = lu_score +
        efficiency_weight1 * (1- pcost/118981.1) +
        efficiency_weight2 *
        (as.numeric(distance_nm <= 45) +
           as.numeric(between(distance_nm, 45, 90)) * (1 - 0.15/45 * (distance_nm - 45)) +
           as.numeric(distance_nm >= 90) * 0.875/(1 + exp(0.0025 * (distance_nm - 1500))))
    )

  return(data)

}
