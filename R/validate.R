validate_matches <- function(matches) {
  if (!is.data.frame(matches)) {
    stop("match_alg must return a data.frame/tibble.")
  }

  req <- c("d_id", "don_org", "data")
  miss <- setdiff(req, names(matches))
  if (length(miss)) {
    stop("match output missing required columns: ", paste(miss, collapse = ", "))
  }

  if (!is.list(matches$data)) {
    stop("match output column `data` must be a list-column.")
  }

  if (nrow(matches) > 0) {
    ok <- vapply(matches$data, function(x) {
      is.data.frame(x) && ("c_id" %in% names(x))
    }, logical(1))

    if (!all(ok)) {
      stop("Each element of matches$data must be a data.frame containing column `c_id`.")
    }
  }

  invisible(TRUE)
}
