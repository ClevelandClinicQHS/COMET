#' COMET
#'
#' @section Initialization components:
#' \itemize{
#' \item \code{\link{agent_gen}}
#' \item \code{\link{hosp_sjoin}}
#' \item \code{\link{trait_assign}}
#'}
#'
#' @section Iterative components:
#' \itemize{
#' \item \code{\link{age_can}}
#' \item \code{\link{age_lung}}
#' \item \code{\link{proc_lung}}
#' \item \code{\link{operation}}
#' \item \code{\link{match_rank}}
#' \item \code{\link{offer}}
#' \item \code{\link{follow_up}}
#'}
#' @section Families:
#' In addition to the partition of the simulation into initialization and iterative aspects,
#' functions generally track either only candidates, only donors, or both candidates and donors, so
#' allow for package description in this manner.  This allows for families of functions to be classified by the
#' primary agents they manipulate.
#'
#' @docType package
#' @name COMET
NULL
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
## usethis namespace: start
#' @useDynLib COMET, .registration = TRUE
## usethis namespace: end
NULL
