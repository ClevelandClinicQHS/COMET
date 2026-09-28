#' Acceptance Probability from matched data
#'
#' @param matched_data output from matching module
#' @param dons generated donor data
#' @param cands generated candidate data
#'
#' @importFrom splines bs
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom stats rbinom
#' @importFrom stats model.matrix
#' @importFrom stats plogis
#' @importFrom rlang .data
#'
#' @return matched_data with acceptance probabilities
#' @export
#'
#' @examples
#' dplyr::mutate(syn_matches, offer_rank = rank(-lu_score), .by = d_id) |>
#'  acceptance_prob(dons = syn_dons, cands = syn_cands)
acceptance_prob <- function(matched_data, dons, cands){

  can_need <- select(cands, "c_id", "center") |>
    mutate(center2 = factor(ifelse(.data$center %in% accpt_centers, .data$center, -99), levels = c(-99, accpt_centers)))

  don_need <- select(dons, "d_id", "smoke_hist", "age", "don_dcd") |>
    mutate(age_55 = (.data$age > 55))

  mz1 <- matched_data |>
    left_join(don_need, by = "d_id") |>
    left_join(can_need, by = "c_id")

  mm <- model.matrix(~splines::bs(offer_rank, knots = seq(10, 100, 10)) + center2 + don_dcd + smoke_hist + age_55, data = mz1) %*% accpt_coef

  odds <- plogis(mm)

  probs <- mutate(matched_data,
                  pred = odds[, 1],
                  accept = rbinom(n = length(odds), size = 1, prob = odds)
    )

  return(probs)
}

#' @export
# acceptance_prob2 <- function(matched_data, dons, cands){
#
#   can_need <- select(cands, "c_id", "center", "hgt_in", "dx_grp", "surg_type") |>
#     mutate(center2 = factor(ifelse(.data$center %in% accpt_centers, .data$center, -99), levels = c(-99, accpt_centers))) |>
#     rename(hgt_in_c = hgt_in) |>
#     mutate(dx_grp = factor(dx_grp, levels = c(LETTERS[1:4]))) |>
#     mutate(surg_type = factor(surg_type, levels = c("D", "E", "S")))
#
#   don_need <- select(dons, "d_id", "smoke_hist", "age", "don_dcd", "hgt_in", "organs_avl") |>
#     mutate(age_55 = (.data$age > 55)) |>
#     mutate(organs_avl = factor(organs_avl, levels = c(1,2)))
#
#   mz1 <- matched_data |>
#     left_join(don_need, by = "d_id") |>
#     left_join(can_need, by = "c_id") |>
#     mutate(hgt_diff = hgt_in - hgt_in_c)
#
#   mz2 <- mutate(mz1, organs_avl = factor(1, levels = c(1:2)))
#
#   # splines::bs(ptr_sequence_num, knots = seq(10,70,10), Boundary.knots = c(1,1000), degree = 3) + ctr_id_trunc + don_dcd + smoke_hist + age_greater55 + as.factor(organs_avl)*tx_o + canhx_grouping*hgt_diff + I(hgt_diff^2)
#   # tx_o + ctr_id_trunc + don_dcd + smoke_hist + age_greater55 + as.factor(organs_avl)*tx_o + canhx_grouping*hgt_diff + I(hgt_diff^2)
#
#   mm <- model.matrix(~splines::bs(offer_rank, knots = seq(10, 90, 10), Boundary.knots = c(1, 1000)) +
#                        center2 + don_dcd + smoke_hist + age_55 + dx_grp * hgt_diff #+ I(hgt_diff^2) #+ dx_grp * I(hgt_diff ^ 2) + surg_type * organs_avl
#                      ,
#                      data = mz1)
#
#   # return(mm)
#
#   mm2 <- model.matrix(~splines::bs(offer_rank, knots = seq(10, 90, 10), Boundary.knots = c(1, 1000)) +
#                         center2 + don_dcd + smoke_hist + age_55 + dx_grp * hgt_diff #+ I(hgt_diff^2)
#                       # + dx_grp * I(hgt_diff ^ 2) + surg_type * organs_avl
#                       ,
#                       data = mz2)
#
#   pr1 <- apply(accpt_coef, 1, function(x) mm %*% x)
#   pr2 <- apply(accpt_coef, 1, function(x) mm2 %*% x)
#
#   odds <- calc_multi_odds(pr1, floor = TRUE)
#   odds2 <- calc_multi_odds(pr2, floor = TRUE)
#
#   ac <- apply(odds, 1, function(x) sample(c(2, 0, 1), size = 1, prob = x))
#
#   matched_data <- left_join(matched_data, can_need, by = "c_id")
#
#   probs <- mutate(matched_data,
#                   op = ac,
#                   ## check this is working later
#                   pred = case_when(
#                     surg_type == "D" ~ odds[, 1],
#                     surg_type == "S" ~ odds[, 3],
#                     surg_type == "E" & op == 1  ~ odds[, 3],
#                     surg_type == "E" ~ odds[, 1]
#
#                   ),
#                   # pred = odds[, 3] + odds[, 1],
#                   pred2 = odds2[, 3],
#                   accept = as.numeric(op > 0),
#                   accept2 = rbinom(n = nrow(odds2), size = 1, prob = odds2[, 3])) |>
#     select(-c("op", "center", "hgt_in_c", "dx_grp", "surg_type", "center2"))
#   # )
#
#   return(probs)
#
#
#
# }
#
