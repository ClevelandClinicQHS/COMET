#' Get Survival Data
#' @rdname get_surv_data
#' @param object a survfit object
#' @export
get_surv_data.survfit <- function(object){
  s1 <- summary(object)
  df <- data.frame(time = s1$time, surv = s1$surv, std.err = s1$std.err,
                   lower = s1$lower, upper = s1$upper, n.risk = s1$n.risk,
                   n.event = s1$n.event, n.censor = s1$n.censor, cumhaz = s1$cumhaz)
  if(!is.null(object$strata)){
    str1 <- gsub("*=.","",s1$strata[1])
    df[str1] <- gsub(".*=", "", s1$strata)
  }
  return(df)
}
#' @rdname get_surv_data
#' @param object an survival object
#' @param ... other arguments for survfit.coxph
#' @export
get_surv_data.coxph <- function(object, ...){
  object1 <- survival::survfit(object, ...)
  s1 <- summary(object1)
  df <- data.frame(time = s1$time, surv = s1$surv, std.err = s1$std.err,
                   lower = s1$lower, upper = s1$upper, n.risk = s1$n.risk,
                   n.event = s1$n.event, n.censor = s1$n.censor, cumhaz = s1$cumhaz)
  if(!is.null(object1$strata)){
    snm <- names(object$xlevels)[stringr::str_which(names(object$xlevels), "strata")]
    str1 <- gsub( "\\).*", "", gsub(".*strata\\(", "", snm))
    df[str1] <- s1$strata
  }
  return(df)
}

#' Get Survival Data from coxph and survfit object
#'
#' @param object either a survfit or coxph object
#' @param ... other arguments depending on class
#'
#' @return a data.frame of survival times from a survfit of coxph
#' @export
#'
#' @examples
#' # Create the simplest test data set
#' ## same example from survival package
#' test1 <- list(time=c(4,3,1,1,2,2,3),
#'             status=c(1,1,1,0,1,1,0),
#'            x=c(0,2,1,1,1,0,0),
#'            sex=c(0,0,0,0,1,1,1))
#' # Fit a stratified model
#' c1 <- coxph(Surv(time, status) ~ x + strata(sex), test1)
#' km1 <- survfit(Surv(time, status) ~ 1, test1)
#' get_surv_data(c1)
#' get_surv_data(km1)
get_surv_data <- function(object, ...){
  UseMethod("get_surv_data")
}

#' Creating complete survival estimates
#'
#' expand and fill in the data.frame from start to finish. No more missing days
#'
#' @param object either survift or coxph object
#' @param cap if you want survival capped at any timepoint, default is max of data
#' @param ignore ignore warnings or not
#'
#'
#' @importFrom dplyr sym
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom tidyr crossing
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom tidyr fill
#' @importFrom tidyr complete
#' @importFrom dplyr any_of
#'
#' @return a complete dataset of survival estimates and time
#' @export
create_surv_rates <- function(object, cap = NA, ignore = FALSE){
  dW <- getOption("warn")

  if(ignore){
    options(warn = -1)
  }

  sd <- get_surv_data(object) |>
    select(-any_of(c("std.err", "lower", "upper", "n.risk", "n.event", "n.censor", "cumhaz")))

  if(any(sd$time<0)){
    warning(glue::glue("Model contains negative times, estimates may be unreliable"))
  }

  f_df <- tibble(time = c(-1, max(sd$time) + 1), surv = c(1, NA))

  cond <- colnames(sd) %in% c("time", "surv")

  if(!all(cond)){
    str1 <- colnames(sd)[which(!cond)]
    f_df <- crossing(f_df, !!str1 := levels(as.factor(sd[,str1])))
    sd <- bind_rows(sd, f_df) |> arrange(time) |>
      group_by(!!sym(str1))

  }else{
    sd <- dplyr::bind_rows(sd, f_df) |> dplyr::arrange(time)
  }

  if(is.na(cap)){cap <- max(sd$time)}

  sd <- sd |>
    complete(time = -1:max(time)) |>
    fill(surv, .direction = "down") |>
    filter(time >= 0) |>
    filter(time <= cap) |>
    ungroup()

  options(warn = dW)

  return(sd)

}

############
#' Truncated Survival days
#'
#' @param object survival object (only coxph models currently are accepted)
#' @param ... arguement given from other methods
#'
#' @return a dateset of candidates, linear predictor and mean survival
#' @export
#' @name trunc_days
#'
#' @examples
#' trunc_days("LAS21", cands, 365, TRUE)
trunc_days <- function(object, ...){
  UseMethod("trunc_days")
}


#' @export
#'
#' @param cand_data candidate data
#' @param cap cap for truncation survival
#' @param wl boolean for waitlist or not
#'
#' @rdname trunc_days
trunc_days.coxph <- function(object, cand_data, cap = NA, wl = TRUE, ...){

  surv <- create_surv_rates(object, cap = cap)
  lp <- predict(object, newdata = cand_data, type = "lp")
  cond <- colnames(surv) %in% c("time", "surv")

  if(!all(cond)){
    str1 <- colnames(surv)[which(!cond)]
    data_n <- select(cand_data, c_id, !!str1) |>
      mutate(lp = lp) |>
      nest_by(!!sym(str1))

    surv1 <- nest_by(surv, !!sym(str1)) |>
      left_join(data_n, by = str1) |>
      mutate(expected = list(mean_survival(data.y$lp, data.x$surv))) |>
      unnest(cols = c(data.y, expected)) |>
      ungroup() |>
      select(c_id, expected) |>
      arrange(c_id)

  }else{
    surv1 <- mutate(cand_data, expected = mean_survival(lp, surv$surv)) |>
      select(c_id, expected)
  }
  return(surv1)
}


#' @param wl TRUE (waitlist) or FALSE (post_transplant)
#' @rdname trunc_days
#' @export
trunc_days.character <- function(object, cand_data, cap = NA, wl = TRUE, ...){

  model <- match.arg(toupper(object), c("LAS15", "LAS21", "CAS23"))
  if(wl){
  switch(model,
         LAS15 = {
           surv <- wl_las15_survrates
           f1 <- calc_wl_las15
         },
         LAS21 = {
           surv <- wl_las21_survrates
           f1 <- calc_wl_las21
         },
         CAS23 = {
           surv <- wl_cas23_survrates
           f1 <- calc_wl_cas23
         }
  )
  }else{
  switch(model,
         LAS15 = {
           surv <- post_tx_las15_survrates
           f1 <- calc_post_tx_las15
         },
         LAS21 = {
           surv <- post_tx_las21_survrates
           f1 <- calc_post_tx_las21
         },
         CAS23 = {
           surv <- post_tx_cas23_survrates
           f1 <- calc_post_tx_cas23
         }
  )
  }

  if(is.na(cap)){cap <- max(surv$Days)}

  surv <- filter(surv, Days <= cap)
  surv1 <- f1(cand_data) |>
    mutate(expected = mean_survival(lp, surv$Survival))
  return(surv1)
}


