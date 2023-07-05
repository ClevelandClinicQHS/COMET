#' Screen Candidates and Donors for Compatibility
#'
#' These functions screen for compatible candidates and donors.
#'
#' \code{height_screen} screens compatible donors and candidates based on acceptable height ranges.
#'
#' \code{abo_screen} screens donors and compatible donors based on blood type
#'
#' \code{pra_screen} not yet implemented will screen on pra compatibility organs
#'
#' \code{count_screen} Makes sure double lung transplant are matched with double lung donors, singles can be matched and received a one of the double lung
#'
#' \code{las_dist_calc} Returns the distance between the matched candidates and donors with las distance categories.
#'
#' @param cands tibble of candidates
#' @param dons tibble of donors
#'
#' @return a dataset containing compatible donors and candidates based on specific criteria
#' @name screen
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr rowwise
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom dplyr between
#' @importFrom dplyr filter
#' @importFrom dplyr join_by
#'
#' @examples
#' ## The functions will use the internal cands and dons stored in the package
#' height_match <- height_screen(cands, dons)
#' abo_match <- abo_screen(cands, dons)
#' pra_match <- pra_screen(cands, dons)
#' count_match <- count_screen(cands, dons)
#' las_dists <- las_dist_calc(cands, dons)
#' cas_dists <- dist_calc(cands, dons)
#' las_rank <- calculate_las(cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
#' las_offers <- las_offer_rank(height_match, abo_match, count_match, pra_match, las_dists, overall_ranking = las_rank)
#' cas_rank <- calculate_sub_cas(cands, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25, bio_weight = 0.15, pld_weight = 0.05, peds_weight = 0.2)
#' cas_offers <- cas_offer_rank(height_match, abo_match, count_match, pra_match, cas_dists, overall_ranking = cas_rank)
height_screen <- function(cands, dons){

  cand_x <- left_join(cands, single_rg, by = "dx_grp") |>
    left_join(double_rg, by = "dx_grp") |>
    mutate(s_lower = hgt_in + s_lower,
           s_upper = hgt_in + s_upper,
           d_lower = hgt_in + d_lower,
           d_upper = hgt_in + d_upper) |>
    rename(hgt_in_c = hgt_in)

  don_x <- rename(dons, hgt_in_d = hgt_in) |>
    select(d_id, hgt_in_d, don_org)

  # s_match <- fuzzy_left_join(cand_x, don_x, by = c("s_lower" = "hgt_in_d", "s_upper" = "hgt_in_d"), match_fun = list(`<=`, `>=`))
  # d_match <- fuzzy_left_join(cand_x, don_x, by = c("d_lower" = "hgt_in_d", "d_upper" = "hgt_in_d"), match_fun = list(`<=`, `>=`))

  s_match <- inner_join(don_x, cand_x, by = join_by(between(hgt_in_d, s_lower, s_upper)))
  d_match <- inner_join(don_x, cand_x, by = join_by(between(hgt_in_d, d_lower, d_upper)))

  ## Just as a checker
  height_match <- bind_rows(s_match, d_match) |>
    distinct(c_id, d_id, .keep_all = TRUE) |>
    mutate(match_single = between(hgt_in_d, s_lower, s_upper),
           match_double = between(hgt_in_d, d_lower, d_upper)) |>
    # ungroup() %>%
    ## if one only one lung must match a single
    filter(don_org == "DLU" |(match_single & don_org %in% c("LUL", "LUR"))) |>
    ## if the candidate is only a double transplant, make sure they match with double lungs
    filter(!(!match_double & surg_type == "D")) |>
    select(c_id, d_id, match_single, match_double)

    return(height_match)

}



#' @rdname screen
#' @export
abo_screen <- function(cands, dons){

  # abo_match_df <- data.frame(abo = c("A","B","AB", "O"),
  #                            match = c("A|AB","B|AB","AB","O|A|B|AB"))
  # ## merges it to the donor dataset
  # don_x <- left_join(dons, abo_match_df, by = "abo")
  #
  #
  # abo_match <- regex_inner_join(cands, don_x,
  #                               by = c(abo = "match")) %>%
  #   mutate(abo_exact = (abo.x == abo.y)) %>%
  #   select(c_id, d_id, abo_exact)

  don_x <- inner_join(dons, abo_match_df, by = "abo", multiple = "all")

  abo_match <- inner_join(cands, don_x, by = join_by(abo == match), multiple = "all") |>
    mutate(abo_exact = (abo == abo.y)) |>
    select(c_id, d_id, abo_exact)

  return(abo_match)
}

#' @export
#'
#' @rdname screen
#' @importFrom tidyr crossing
pra_screen <- function(cands, dons){

  ## Simply a placeholder for now
  pra_match <- crossing(c_id = cands$c_id, d_id = dons$d_id)

  return(pra_match)

}

#' @export
#'
#' @rdname screen
count_screen <- function(cands, dons){

  # count_match_df <- data.frame(surg_type = c("E","S","D"),
  #                              ## Either can be matched to either
  #                              ## a single can be matched to a double, but only accept 1
  #                              ## double can only accept 2
  #                              match = c("DLU|LUR|LUL","LUR|LUL|DLU","DLU"))
  #
  # ## merges it to the donor dataset
  # cand_x <- left_join(cands, count_match_df, by = "surg_type")
  #
  # count_match <- regex_inner_join(dons, cand_x,
  #                                 by = c(don_org = "match")) %>%
  #   select(c_id, d_id)


  don_x <- inner_join(dons, count_match_df, by = "don_org", multiple = "all")

  count_match <- inner_join(don_x, cands, by = join_by(match == surg_type), multiple = "all") |>
    select(c_id, d_id)

  return(count_match)

}

#' @export
#'
#' @details Matching rules for LAS distances are broken into the following zones, 250 nautical miles, 500, 1000, 1500, 2500, the US
#'
#' @rdname screen
#' @importFrom dplyr arrange
las_dist_calc <- function(cands, dons){

  dist_match <- crossing(select(cands, c_id, center), select(dons, d_id, hospital)) |>
    left_join(dist_data, by = c("center" = "can_center", "hospital" = "don_hosp")) |>
    mutate(proximity_class = cut(distance_nm, c(-1, 250, 500, 1000, 1500, 2500, 100000),
                                 labels = c("<250", "250-500", "500-1000", "1000-1500", "1500-2500", ">2500"), right = FALSE)
    ) |>
    arrange(proximity_class) |>
    select(c_id, d_id, distance_nm, proximity_class)

  return(dist_match)

}

#' @export
#'
#' @rdname screen
dist_calc <- function(cands, dons){

  dist_match <- crossing(select(cands, c_id, center), select(dons, d_id, hospital)) |>
    left_join(dist_data, by = c("center" = "can_center", "hospital" = "don_hosp")) |>
    select(c_id, d_id, distance_nm)

  return(dist_match)

}



#' @param ... tibbles of screening conditions, height, blood, pra, distance, etc
#' @param overall_ranking tibble containing overall ranking by candidate
#'
#' @return a dataset of all comparable matches and their respective rankings
#' @export
#'
#' @rdname screen
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr desc
#' @importFrom dplyr row_number
#' @importFrom dplyr join_by
#' @importFrom purrr reduce
las_offer_rank <- function(..., overall_ranking){

  l <- list(...)

  # all_x <- reduce(l, inner_join, by = c("d_id", "c_id")) |>
  all_x <- Reduce(function(x,y) inner_join(x, y, by = join_by(c_id, d_id)), l) |>
    left_join(overall_ranking, by = join_by(c_id)) |>
    group_by(d_id) |>
    arrange(d_id, proximity_class, desc(abo_exact), desc(lu_score)) |>
    mutate(offer_rank = row_number()) |>
    ungroup()

  return(all_x)
}


#' @param efficiency_weight weight of efficiency for distance between candidate and donor
#' @param cost_weight weight given for cost part of CAS (if not specified is half of efficiency weight)
#' @param distance_weight weight given for distance part of CAS (if not specified is half of efficiency weight)
#' @param checks whether or not to check the conditions and display warnings, this is there to not check conditions every time the simulation is iterated
#'
#' @export
#'
#' @rdname screen
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr join_by
#' @importFrom purrr reduce
cas_offer_rank <- function(..., overall_ranking, efficiency_weight = 0.10, cost_weight = NA, distance_weight = NA, checks = TRUE){

  l <- list(...)

  # all_x <- reduce(l, inner_join, by = c("d_id", "c_id")) |>
  all_x <- Reduce(function(x,y) inner_join(x, y, by = join_by(c_id, d_id)), l) |>
    left_join(overall_ranking, by = join_by(c_id)) |>
    calculate_cas_dist(match_data = _, efficiency_weight = efficiency_weight,
                       cost_weight = cost_weight, distance_weight = distance_weight, checks = checks) |>
    group_by(d_id) |>
    mutate(offer_rank = rank(-lu_score)) |>
    ungroup()

  return(all_x)

}

