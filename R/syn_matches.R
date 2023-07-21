#' Synthetic Match Set
#'
#' @format `syn_matches` A tibble of 275 synthetic donor and candidate matched pairs
#' \describe{
#' \item{d_id}{unqiue donor identifier}
#' \item{don_org}{integer from 1-1691 of donor hospital}
#' \item{c_id}{unqiue candidate identifier}
#' \item{match_single}{match for single lung transplant; boolean}
#' \item{match_double}{match for double lung transplant; boolean}
#' \item{abo_exact}{exact blood type match; boolean}
#' \item{distance_nm}{distance between candidate and donor in nautical miles}
#' \item{proximity}{LAS promiximty class}
#' \item{lu_score}{LAS score on 0-1 scale}
#'
#' }
"syn_matches"
