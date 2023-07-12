#' LAS/CAS Matching
#'
#' @name match_
#'
#' @param cands candidates on waiting list
#' @param dons donors avaible
#' @param wl_model model used for waitlist component of LAS
#' @param post_tx_model model used for post-transplant componet of LAS
#' @param wl_weight weight of waitlist componet
#' @param wl_cap cap for waitlist survival
#' @param post_tx_weight weight of post-transplant compoent
#' @param post_tx_cap cap for post-transplant survival
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#' @return a nested tibble of donors and candidates
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom tidyr nest
#'
#' @examples
#' match_las(cands, dons, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365)
#' match_cas(cands, dons, wl_model = "CAS23", post_tx_model = "CAS23",
#'  wl_weight =.25, post_tx_weight = .25, wl_cap = 365, post_tx_cap = 1825,
#'  bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1)
match_las <- function(cands, dons, wl_model = NA, post_tx_model = NA, wl_weight = NA, wl_cap = NA, post_tx_weight = NA, post_tx_cap = NA, checks = TRUE){

  hm <- height_screen(cands, dons)
  bm <- abo_screen(cands, dons)
  pm <- pra_screen(cands, dons)
  dm <- las_dist_calc(cands, dons)
  cm <- count_screen(cands, dons)

  las_scores <- calculate_las(cands, wl_model = wl_model, post_tx_model = post_tx_model, wl_weight = wl_weight,
                               wl_cap = wl_cap, post_tx_weight = post_tx_weight, post_tx_cap = post_tx_cap, checks = checks) |>
    mutate(ov_rank = rank(-lu_score))

  matches <- las_offer_rank(hm, bm, pm, dm, cm, overall_ranking = las_scores)
  if(nrow(matches) == 0){
    return(matches)
  }
  matches <- acceptance_prob(matched_data = matches, dons = dons, cands = cands)
  ## acceptance
  don_ov <- select(dons, d_id, don_org)
  can_ov <- select(cands, c_id, surg_type)

  matches <- left_join(matches, can_ov, by = "c_id") |>
    left_join(don_ov, by = "d_id") |>
    # mutate(accept =  ifelse(accept_double == 1|accept_single == 1, 1 ,0)) |>
    arrange(d_id, offer_rank) |>
    nest(.by = c(d_id, don_org))

  return(matches)

}


#' @param cands candidates of waiting list
#' @param dons donors available on given day
#' @param wl_model model used for waitlist component of the CAS
#' @param post_tx_model model used for post-transplant component of the CAS
#' @param wl_weight weight for waitlist survival in days
#' @param wl_cap cap for weightlist survival in days
#' @param post_tx_weight weight for post-transplant component of CAS
#' @param post_tx_cap cap of post-transplatn survival in days
#' @param bio_weight weight for biological compoent (divided evenly for blood type, hieght, and pra)
#' @param peds_weight weight for pediatric patient
#' @param pld_weight weight for prior living donor
#' @param efficiency_weight weight for travel efficiency
#' @param abo_weight weight for blood type compatability (if not specified will default to 1/3 of bio weight)
#' @param height_weight weight for height compatability (if not specified will default to 1/3 of bio weight)
#' @param cpra_weight weight for cpra compatability (if not specified will default to 1/3 of bio weight)
#' @param cost_weight weight given for cost part of CAS (if not specified is half of efficiency weight)
#' @param distance_weight weight given for distance part of CAS (if not specified is half of efficiency weight)
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#'
#' @export
#'
#' @rdname match_
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom tidyr nest
#' @importFrom dplyr near
match_cas <- function(cands, dons, wl_model = NA, post_tx_model = NA, wl_weight = NA, wl_cap = NA,
                        post_tx_weight = NA, post_tx_cap = NA, bio_weight = NA, peds_weight = NA,
                      pld_weight = NA, efficiency_weight = NA, abo_weight = NA, height_weight = NA,
                      cpra_weight = NA, cost_weight = NA, distance_weight = NA, checks = TRUE
){

  # weights_x <- wl_weight + post_tx_weight + bio_weight + peds_weight + pld_weight + efficiency_weight
  # if(!near(weights_x, 1)){
  #   warning("Weights do not sum to 1")
  # }

  hm <- height_screen(cands, dons)
  bm <- abo_screen(cands, dons)
  pm <- pra_screen(cands, dons)
  dm <- dist_calc(cands, dons)
  cm <- count_screen(cands, dons)

  cas_ov <- calculate_sub_cas(cands, wl_model = wl_model, post_tx_model = post_tx_model, wl_weight = wl_weight, wl_cap = wl_cap,
                              post_tx_weight = post_tx_weight, post_tx_cap = post_tx_cap, bio_weight = bio_weight, peds_weight = peds_weight,
                              pld_weight = pld_weight, abo_weight = abo_weight, height_weight = height_weight, cpra_weight = cpra_weight, checks = checks)

  matches <- cas_offer_rank(hm, bm, pm, dm, cm, overall_ranking = cas_ov, efficiency_weight = efficiency_weight, cost_weight = cost_weight, distance_weight = distance_weight, checks = checks)
  if(nrow(matches) == 0){
    return(matches)
  }
  matches <- acceptance_prob(matches, dons = dons, cands = cands)

  don_ov <- select(dons, d_id, don_org)
  can_ov <- select(cands, c_id, surg_type)

  matches <- left_join(matches, can_ov, by = "c_id") |>
    left_join(don_ov, by = "d_id") |>
    arrange(d_id, offer_rank) |>
    nest(.by = c(d_id, don_org))

  return(matches)

}

