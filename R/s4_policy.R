## =========================================================
## S4 Policy layer (minimal-change, swap-in/out policies)
## File: R/s4_policy.R
## =========================================================


## ---------- Base policy (virtual) ----------
#' Base COMET policy class
#' @exportClass COMETPolicy
setClass(
  "COMETPolicy",
  contains = "VIRTUAL",
  slots = c(
    name  = "character",
    params = "list",
    notes = "character"
  ),
  prototype = list(
    name = "unnamed policy",
    params = list(),
    notes = ""
  )
)

## ---------- Concrete policies ----------
#' CAS policy class
#' @exportClass CASPolicy
setClass("CASPolicy", contains = "COMETPolicy")
#' LAS policy class
#' @exportClass LASPolicy
setClass("LASPolicy", contains = "COMETPolicy")
#' Custom policy class
#' @exportClass CustomPolicy
setClass("CustomPolicy", contains = "COMETPolicy")  # for user-defined knobs

## ---------- Generics ----------
#' Get policy parameters
#' @param policy A COMETPolicy object.
#' @export
setGeneric("policyParams", function(policy) standardGeneric("policyParams"))

#' Get a policy label
#' @param policy A COMETPolicy object.
#' @export
setGeneric("policyLabel", function(policy) standardGeneric("policyLabel"))

#' Validate a policy object
#' @param policy A COMETPolicy object.
#' @export
setGeneric("validatePolicy", function(policy) standardGeneric("validatePolicy"))

## ---------- Base methods ----------
setMethod("policyParams", "COMETPolicy", function(policy) policy@params)

setMethod("policyLabel", "COMETPolicy", function(policy) {
  if (length(policy@name) == 1 && nzchar(policy@name)) policy@name else class(policy)[1]
})

## Default: no validation (subclasses should implement)
setMethod("validatePolicy", "COMETPolicy", function(policy) TRUE)

## ---------- Helper: ensure keys exist ----------
.ensure_keys <- function(params, required_keys, fill = NA) {
  for (k in required_keys) {
    if (!k %in% names(params)) params[[k]] <- fill
  }
  params
}

## ---------- CASPolicy validation ----------
## Why these keys? They appear in match_cas() / match_cas_pre_abo() signatures (matching.R),
## and missing ones cause "xxx is missing" at runtime (your abo_weight error).
.cas_required_keys <- c(
  "match_alg",
  "wl_model", "post_tx_model",
  "wl_weight", "wl_cap",
  "post_tx_weight", "post_tx_cap",
  "bio_weight", "peds_weight", "pld_weight",
  "efficiency_weight",
  "abo_weight", "height_weight", "cpra_weight",
  "cost_weight", "distance_weight",
  "checks"
)

setMethod("validatePolicy", "CASPolicy", function(policy) {
  missing <- setdiff(.cas_required_keys, names(policy@params))
  if (length(missing) > 0) {
    stop(
      "CASPolicy is missing required parameter key(s): ",
      paste(missing, collapse = ", "),
      "\nProvide them in CASPolicy(params=...) or use CASPolicy() constructor which auto-fills with NA.",
      call. = FALSE
    )
  }
  TRUE
})

## ---------- LASPolicy validation ----------
.las_required_keys <- c(
  "match_alg",
  "wl_model", "post_tx_model",
  "wl_weight", "wl_cap",
  "post_tx_weight", "post_tx_cap",
  "checks"
)

setMethod("validatePolicy", "LASPolicy", function(policy) {
  missing <- setdiff(.las_required_keys, names(policy@params))
  if (length(missing) > 0) {
    stop(
      "LASPolicy is missing required parameter key(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  TRUE
})

## ---------- Constructors (recommended) ----------
## These auto-fill missing keys with NA so you never get "xxx is missing" again.
#' Create a CAS policy
#' @param name Policy name.
#' @param params A named list of policy parameters.
#' @param notes Optional notes.
#' @param autofill Whether to fill missing required keys with NA.
#' @return A CASPolicy object.
#' @export
CASPolicy <- function(name = "CAS policy", params = list(), notes = "", autofill = TRUE) {
  if (autofill) params <- .ensure_keys(params, .cas_required_keys, fill = NA)
  pol <- new("CASPolicy", name = name, params = params, notes = notes)
  validatePolicy(pol)
  pol
}
#' Create a LAS policy
#' @param name Policy name.
#' @param params A named list of policy parameters.
#' @param notes Optional notes.
#' @param autofill Whether to fill missing required keys with NA.
#' @return A LASPolicy object.
#' @export
LASPolicy <- function(name = "LAS policy", params = list(), notes = "", autofill = TRUE) {
  if (autofill) params <- .ensure_keys(params, .las_required_keys, fill = NA)
  pol <- new("LASPolicy", name = name, params = params, notes = notes)
  validatePolicy(pol)
  pol
}
#' Create a custom policy
#' @param name Policy name.
#' @param params A named list of policy parameters.
#' @param notes Optional notes.
#' @return A CustomPolicy object.
#' @export
CustomPolicy <- function(name = "Custom policy", params = list(), notes = "") {
  new("CustomPolicy", name = name, params = params, notes = notes)
}
