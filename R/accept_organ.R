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
#'
#' @return matched_data with acceptance probabilities
acceptance_prob <- function(matched_data, dons, cands){

  can_need <- select(cands, c_id, center) |>
    mutate(center2 = factor(ifelse(center %in% accpt_centers, center, -99), levels = c(-99, accpt_centers)))

  don_need <- select(dons, d_id, smoke_hist, age, don_dcd) |>
    mutate(age_55 = (age > 55))

  mz1 <- matched_data |>
    left_join(don_need, by = "d_id") |>
    left_join(can_need, by = "c_id")

  mm <- model.matrix(~splines::bs(offer_rank, knots = seq(10, 100, 10)) + center2 + don_dcd + smoke_hist + age_55, data = mz1) %*% accpt_coef

  odds <- inv_log(mm)

  probs <- mutate(matched_data,
                  pred = odds[, 1],
                  accept = rbinom(n = length(odds), size = 1, prob = odds)
    )

  return(probs)
}
