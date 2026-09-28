## =========================================================
## S4 Run configuration (COMETConfig)
## File: R/s4_config.R
## =========================================================


#' COMET run configuration
#'
#' @slot days Number of simulation days.
#' @slot seed Random seed or NULL.
#' @slot desired Candidate/donor generation mode.
#' @slot can_start Starting candidate count or candidate data.frame.
#' @slot include_matches Logical.
#' @slot return_params Logical.
#' @slot notes Optional notes.
#' @exportClass COMETConfig
setClass(
  "COMETConfig",
  slots = c(
    days = "numeric",
    seed = "ANY",              # NULL or numeric
    desired = "character",     # e.g. "random"
    can_start = "ANY",         # numeric OR data.frame/tibble
    include_matches = "logical",
    return_params = "logical",

    ## optional run metadata
    notes = "character"
  ),
  prototype = list(
    days = 365,
    seed = NULL,
    desired = "random",
    can_start = 1250,
    include_matches = FALSE,
    return_params = FALSE,
    notes = ""
  )
)

## ---- generics ----
#' Validate a COMET configuration
#' @param config A COMETConfig object.
#' @export
setGeneric("validateConfig", function(config) standardGeneric("validateConfig"))
#' Summarize a COMET configuration
#' @param config A COMETConfig object.
#' @export
setGeneric("configSummary", function(config) standardGeneric("configSummary"))

## ---- validation ----
setMethod("validateConfig", "COMETConfig", function(config) {
  if (length(config@days) != 1 || is.na(config@days) || config@days <= 0) {
    stop("COMETConfig@days must be a single positive number.", call. = FALSE)
  }

  if (!(is.null(config@seed) || (is.numeric(config@seed) && length(config@seed) == 1))) {
    stop("COMETConfig@seed must be NULL or a single numeric value.", call. = FALSE)
  }

  if (length(config@desired) != 1 || !nzchar(config@desired)) {
    stop("COMETConfig@desired must be a single non-empty string (e.g., 'random').", call. = FALSE)
  }

  if (!(is.numeric(config@can_start) && length(config@can_start) == 1) &&
      !(is.data.frame(config@can_start))) {
    stop("COMETConfig@can_start must be either a single numeric or a data.frame/tibble.", call. = FALSE)
  }

  if (length(config@include_matches) != 1) stop("include_matches must be TRUE/FALSE.", call. = FALSE)
  if (length(config@return_params) != 1) stop("return_params must be TRUE/FALSE.", call. = FALSE)

  TRUE
})

## ---- printing / helper ----
setMethod("show", "COMETConfig", function(object) {
  cat("<COMETConfig>\n")
  cat("  days: ", object@days, "\n", sep = "")
  cat("  desired: ", object@desired, "\n", sep = "")
  cat("  seed: ", if (is.null(object@seed)) "NULL" else as.character(object@seed), "\n", sep = "")
  cat("  can_start: ", if (is.numeric(object@can_start)) object@can_start else "<data.frame>", "\n", sep = "")
  cat("  include_matches: ", object@include_matches, "\n", sep = "")
  cat("  return_params: ", object@return_params, "\n", sep = "")
  if (nzchar(object@notes)) cat("  notes: ", object@notes, "\n", sep = "")
})

setMethod("configSummary", "COMETConfig", function(config) {
  validateConfig(config)
  list(
    days = config@days,
    desired = config@desired,
    seed = config@seed,
    can_start = if (is.numeric(config@can_start)) config@can_start else "data.frame",
    include_matches = config@include_matches,
    return_params = config@return_params,
    notes = config@notes
  )
})

## ---- constructor ----
#' Create a COMET configuration
#'
#' @param days Number of simulation days.
#' @param seed Optional random seed.
#' @param desired Candidate/donor generation mode.
#' @param can_start Starting candidate count or a data.frame of old candidates.
#' @param include_matches Whether to keep all match records.
#' @param return_params Whether to return generator parameters.
#' @param notes Optional notes.
#' @return A COMETConfig object.
#' @export
COMETConfig <- function(days,
                        seed = NULL,
                        desired = "random",
                        can_start = 1250,
                        include_matches = FALSE,
                        return_params = FALSE,
                        notes = "") {
  cfg <- new(
    "COMETConfig",
    days = as.numeric(days),
    seed = seed,
    desired = desired,
    can_start = can_start,
    include_matches = include_matches,
    return_params = return_params,
    notes = notes
  )
  validateConfig(cfg)
  cfg
}
