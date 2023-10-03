#' Generate bayesian parameters and spawn donors/candidates
#'
#' @name gen_and_spawn
#'
#' @param desired whether the Bayesian parameters should be "random" or the "mean"
#' @param days number of days (must be greater than > 1)
#' @param return_params if TRUE returns the parameters used for generating donors and candidates
#'
#' @return if return_params = TRUE a list containing synthetic donors or candidates and parameters used to generate their attributes
#'  Otherwise a data set of synthetic donors or candidates
#' @export
#'
#' @examples
#' \dontrun{
#' d1 <- gen_and_spawn_donors("random", days = 10)
#' c1 <- gen_and_spawn_candidates("random", days = 10)
#' }
gen_and_spawn_donors <- function(desired = "random", days,
                                 return_params = FALSE){

  if(days <= 1L)stop('days must be greater than 1')
  if(!(desired %in% c("mean", "random")))stop('desired must be "random" or "mean"')
  if(!is.logical(return_params))stop('return_params must be "TRUE" or "FALSE"')

  gen_count <- generate_params("don_count", desired = desired)
  gen_sex <- generate_params("don_sex", desired = desired)
  gen_race <- generate_params("don_race", desired = desired)
  gen_hgt <- generate_params("don_hgt", desired = desired)
  gen_abo <- generate_params("don_abo", desired = desired)
  gen_cod <- generate_params("don_cod", desired = desired)
  gen_smoke <- generate_params("don_smoke", desired = desired)
  gen_org <- generate_params("don_org", desired = desired)
  gen_dcd <- generate_params("don_dcd", desired = desired)
  gen_age <- generate_params("don_age", desired = desired)
  gen_util <- generate_params("don_util", desired = desired)

  gen_data <- spawn_donors(days = days, daily_lambdas_center = gen_count, sex_thetas = gen_sex, race_mat_beta = gen_race,
                           hgt_params = gen_hgt, abo_mat_beta = gen_abo, age_params = gen_age, cod_mat_beta = gen_cod,
                           smoke_hist_mat_beta = gen_smoke, don_org_odds = gen_org, dcd_params = gen_dcd, don_util_odds = gen_util)

  if(return_params == TRUE){

    gen_par <- list("don_count" = gen_count,
                    "don_sex" = gen_sex,
                    "don_race" = gen_race,
                    "don_hgt" = gen_hgt,
                    "don_abo" = gen_abo,
                    "don_age" = gen_age,
                    "don_cod" = gen_cod,
                    "don_smoke" = gen_smoke,
                    "don_org" = gen_org,
                    "don_dcd" = gen_dcd,
                    "don_util" = gen_util)

    l <- list("params" = gen_par,
              "data" = gen_data)
    return(l)
  }

  return(gen_data)

}
#' @rdname gen_and_spawn
#'
#' @export
gen_and_spawn_candidates <- function(desired = "random", days,
                                     return_params = FALSE){

  if(days <= 1L)stop('days must be greater than 1')
  if(!(desired %in% c("mean", "random")))stop('desired must be "random" or "mean"')
  if(!is.logical(return_params))stop('return_params must be "TRUE" or "FALSE"')

  gen_count <- generate_params("can_count", desired = desired)
  gen_sex <- generate_params("can_sex", desired = desired)
  gen_age <- generate_params("can_age", desired = desired)
  gen_hgt <- generate_params("can_hgt", desired = desired)
  gen_wgt <- generate_params("can_wgt", desired = desired)
  gen_wgt_c <- generate_params("can_wgt_c", desired = desired)
  gen_race <- generate_params("can_race", desired = desired)
  gen_abo <- generate_params("can_abo", desired = desired)
  gen_airway <- generate_params("can_airway", desired = desired)
  gen_oxygen <- generate_params("can_oxygen", desired = desired)
  gen_resp_supp <- generate_params("can_resp_supp", desired = desired)
  gen_fev <- generate_params("can_fev", desired = desired)
  gen_fvc <- generate_params("can_fvc", desired = desired)
  gen_pco2 <- generate_params("can_pco2", desired = desired)
  gen_pf <- generate_params("can_pf", desired = desired)
  gen_po2 <- generate_params("can_po2", desired = desired)
  gen_mpap <- generate_params("can_meanpap", desired = desired)
  gen_o2f <- generate_params("can_o2freq", desired = desired)
  gen_vent <- generate_params("can_vent", desired = desired)
  ##
  gen_suppo2 <- generate_params("can_suppo2", desired = desired)
  gen_walk6m <- generate_params("can_walk6m", desired = desired)
  gen_surg <- generate_params("can_surg", desired = desired)
  gen_bili <- generate_params("can_bili", desired = desired)
  gen_creat <- generate_params("can_creat", desired = desired)
  ##
  gen_spap <- generate_params("can_systpap", desired = desired)
  gen_ci <- generate_params("can_ci", desired = desired)
  gen_fstat <- generate_params("can_funstat", desired = desired)
  gen_pco215 <- generate_params("can_pco2thresh", desired = desired)

  gen_sarcA <- generate_params("can_sarcA", desired = desired)
  gen_sarcD <- generate_params("can_sarcD", desired = desired)
  gen_specA <- generate_params("can_specA", desired = desired)
  gen_specD <- generate_params("can_specD", desired = desired)

  ##
  gen_lam <- generate_params("can_lam", desired = desired)
  gen_eisen <- generate_params("can_eisen", desired = desired)
  gen_cvp <- generate_params("can_cvp", desired = desired)
  gen_diab <- generate_params("can_diab", desired = desired)

  gen_data <- spawn_candidates(days = days, daily_lambdas_center = gen_count, sex_thetas = gen_sex,
                               race_mat_beta = gen_race,
                               abo_mat_beta = gen_abo,
                               age_params = gen_age, hgt_params = gen_hgt, wgt_params = gen_wgt,
                               wgt_params_c = gen_wgt_c, airway_params = gen_airway, oxygen_params = gen_oxygen,
                               resp_mat_beta = gen_resp_supp, surg_mat_beta = gen_surg, fev_params = gen_fev,
                               fvc_params = gen_fvc, pco2_params = gen_pco2, pf_params = gen_pf, po2_params = gen_po2,
                               mpap_params = gen_mpap,
                               o2_freq_mat = gen_o2f,vent_mat_beta = gen_vent, walk6m_params = gen_walk6m, bili_params = gen_bili,
                               creat_params = gen_creat, spap_params = gen_spap, ci_params = gen_ci, fs_mat_beta = gen_fstat,
                               pco2_15_params = gen_pco215, sa_params = gen_sarcA, sd_params = gen_sarcD, ar_mat_beta = gen_specA,
                               dr_mat_beta = gen_specD, supp_o2_params = gen_suppo2, diab_params = gen_diab, cvp_params = gen_cvp,
                               lam_params = gen_lam, eisen_params = gen_eisen)
  if(return_params == TRUE){

    l2 <- list("can_count" = gen_count,
               "can_sex" = gen_sex,
               "can_age" = gen_age,
               "can_hgt" = gen_hgt,
               "can_wgt" = gen_wgt,
               "can_wgt_c" = gen_wgt_c,
               "can_diab" = gen_diab,
               "can_race" = gen_race,
               "can_abo"= gen_abo,
               "can_airway" = gen_airway,
               "can_oxygen" = gen_oxygen,
               "can_resp_supp" = gen_resp_supp,
               "can_fev" = gen_fev,
               "can_fvc" = gen_fvc,
               "can_pco2" = gen_pco2,
               "can_pf" = gen_pf,
               "can_po2" = gen_po2,
               "can_meanpap" = gen_mpap,
               "can_o2freq" = gen_o2f,
               "can_vent" = gen_vent,
               "can_suppo2" = gen_suppo2,
               "can_walk6m" = gen_walk6m,
               "can_surg" = gen_surg,
               "can_creat" = gen_creat,
               "can_systpap" = gen_spap,
               "can_ci" = gen_ci,
               "can_cvp" = gen_cvp,
               "can_funstat" = gen_fstat,
               "can_pco2thresh" = gen_pco215,
               "can_sarcA" = gen_sarcA,
               "can_lam" = gen_lam,
               "can_eisen" = gen_eisen,
               "can_sarcD" = gen_sarcD,
               "can_specA" = gen_specA,
               "can_specD" = gen_specD
    )

    l <- list("params" = l2,
              "data"= gen_data)

    return(l)
  }

  return(gen_data)

}

#' @importFrom rlang .data
generate_params <- function(characteristic, desired = "random"){

  char_sel <- characteristic
  l1 <- all_params_dist[[char_sel]]

  d_o <- c("means","random")

  if(tolower(desired) == "random"){

    params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])

  }else{
    params <- l1[[1]]
  }

  if(any(grepl("^theta\\[\\d+\\]$", names(params)))){
  # if(any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
    # while(any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) | any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
    while(any(params[grep("^theta\\[\\d+\\]$", names(params))]<0) | any(params[grep("^theta\\[\\d+\\]$", names(params))]>1)){

    #    params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
      params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
      params_x2 <- params_x[, grep("^theta\\[\\d+\\]$", names(params))]
      # params2 <- which(map_lgl(1:nrow(params_x), ~(sum(params_x2[.x,]<0) + sum(params_x2[.x,]>1))==0))
      params2 <- which(sapply(1:nrow(params_x),  function(x) (sum(params_x2[x, ] < 0) + sum(params_x2[x, ] > 1)) == 0))
      if(length(params2)==0L){params <- params}else{
        params <- params_x[min(params2),]
      }


    }
  }


  if(any(grepl("^c\\[\\d+\\]$", names(params)))){
  # if(any(str_detect(names(params), "^c\\[\\d+\\]$"))){
    # while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")])){
    while(is.unsorted(params[grep("^c\\[\\d+\\]$", names(params))])){
      params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
    }
  }

  if(any(grepl("^c\\[\\d+\\]$", names(params))) & any(grepl("^theta\\[\\d+\\]$", names(params)))){
  # if(any(str_detect(names(params), "^c\\[\\d+\\]$")) & any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
    # while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")]) |
    #       any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) |
    #       any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
    while(is.unsorted(params[grep("^c\\[\\d+\\]$", names(params))]) |
          any(params[grep("^theta\\[\\d+\\]$", names(params))]<0) |
          any(params[grep("^theta\\[\\d+\\]$", names(params))]>1)){
      params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
    }
  }
  if(any(grepl("^cs\\[\\d+,\\d+]$", names(params)))){
  # if(any(str_detect(names(params), "^cs\\[\\d+,\\d+]$"))){
    # ncols = max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE)
    # c_pars <- str_detect(names(params), "^cs\\[\\d+,\\d+]$")
    c_pars <- grepl("^cs\\[\\d+,\\d+]$", names(params))
    ncols <- max(as.numeric(regmatches(names(params), m = regexpr("(?<=,)\\d+", text = names(params), perl = TRUE))), na.rm = TRUE)
    # ncols <- max(as.numeric(regmatches(names(params), m = regexpr("(?<=,)\\d+", text = names(params))), na.rm = TRUE))
    tmp_mat <- matrix(params[c_pars],
                      ncol = ncols)
    uns_ck <- apply(tmp_mat, 2, is.unsorted)
    while(sum(uns_ck) > 0){
      params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
      tmp_mats <- lapply(1:1000, function(x) matrix(params_x[x, c_pars], ncol = ncols))
      cks <- sapply(lapply(tmp_mats,  function(x) apply(x, 2, is.unsorted)), sum)
      uns_ck <- cks[which.min(cks)]
      params <- params_x[which.min(cks),]

    }
  }

  # if(any(str_detect(names(params), "^lambda\\[\\d+\\]$"))){
  if(any(grepl("lambda\\[\\d+", names(params)))){
    while(any(params < 0)){
      params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
      # params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
    }
  }

  ## These a specfic
  ## because some of the donor hosptials are quite small so the race and sex params need to set to more than one hospital

  if(char_sel == "don_sex"){
    small_param <- params[length(params)]
    small_params <- rep(small_param, length(small_ids))
    names(small_params) <- paste0("theta[", small_ids, "]")
    ot_params <- params[-length(params)]
    names(ot_params) <- paste0("theta[", ot_ids, "]")
    params <- c(ot_params, small_params)
    params <- params[order(factor(names(params), levels = paste0("theta[", all_ids, "]")))]

  }
  if(char_sel == "don_count"){
    small_param <- params[length(params)]
    small_params <- rep(small_param, length(small_ids))/length(small_ids)
    names(small_params) <- paste0("lambda[", small_ids, "]")
    ot_params <- params[-length(params)]
    names(ot_params) <- paste0("lambda[", ot_ids, "]")
    params <- c(ot_params, small_params)
    params <- params[order(factor(names(params), levels = paste0("lambda[", all_ids, "]")))]
  }
  if(char_sel %in% c("don_cod","don_race")){
    # small_param <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"))]
    small_param <- params[grep(paste0("betas\\[", mx_id,",\\d+\\]"), names(params))]
    zeros <- rep(small_param, length(small_ids))
    # names(zeros) <- pmap_chr(expand.grid("betas[", small_ids, ",", seq_along(small_param),"]") |>
    #                            arrange(.data$Var2, .data$Var4), paste0)
    names(zeros) <- c(sapply(small_ids, function(x) sapply(seq_along(small_param), function(y) paste0("betas[", x, ",", y, "]")), simplify = TRUE))

    # ot_params <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"), negate = TRUE)]
    ot_params <- params[-grep(paste0("betas\\[", mx_id,",\\d+\\]"), names(params))]
    # names(ot_params) <- pmap_chr(expand.grid("betas[", ot_ids, ",", seq_along(small_param),"]"), paste0)
    names(ot_params) <- c(sapply(seq_along(small_param), function(x) sapply(ot_ids, function(y) paste0("betas[", y, ",", x, "]")), simplify = TRUE))


    params <- c(ot_params, zeros)
    # params <- params[order(factor(names(params), levels = pmap_chr(expand.grid("betas[", all_ids, ",", seq_along(small_param),"]"), paste0)))]
    params <- params[order(factor(names(params), levels = c(sapply(seq_along(small_param), function(x) sapply(all_ids, function(y) paste0("betas[", y, ",", x, "]")), simplify = TRUE))))]

  }

  ## central venous pressue is different structure
  if(char_sel %in% c("can_cvp", "can_count", "can_suppo2")){
    return(params)
  }


  # if(any(str_detect(names(params), ","))){
  if(any(grepl(",", names(params)))){
    # params <- matrix(params, ncol = max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE),
    #                  nrow = max(as.numeric(str_extract(names(params), "\\d+(?=,)")), na.rm = TRUE))
    params <- matrix(params, ncol = max(as.numeric(regmatches(names(params), m = regexpr("(?<=,)\\d+", text = names(params), perl = TRUE))), na.rm = TRUE),
                     nrow = max(as.numeric(regmatches(names(params), m = regexpr("\\d+(?=,)", text = names(params), perl = TRUE))), na.rm = TRUE)
                     )
  }

  return(params)

}


#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr as_tibble
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom dplyr select
#' @importFrom stats rbinom
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats rpois
#' @importFrom rlang .data
#' @importFrom stats plogis
spawn_donors <- function(days, daily_lambdas_center, sex_thetas,
                         race_mat_beta, hgt_params, abo_mat_beta, age_params, cod_mat_beta,
                         smoke_hist_mat_beta, don_org_odds, dcd_params, don_util_odds){

  if(days <= 1L)stop("days must be greater than 1")

  Hosps <- seq_along(daily_lambdas_center)
  names(Hosps) <- Hosps

  df_b <- as.data.frame(mapply(function(x) rpois(daily_lambdas_center[x], n = days), Hosps)) |>
    mutate(recovery_day = row_number())

  df_b_long <- pivot_longer(df_b, -"recovery_day", names_to = "hospital") |>
    filter(.data$value > 0)

  ## only if there's more than one donor per hospital on a given day
  if(any(df_b_long$value >1)){
    v2 <- filter(df_b_long, .data$value > 1)

    v2_f <- apply(v2, 1,  function(x) tibble(hospital = rep(x[2], x[3]), recovery_day = as.integer(x[1]))) |>
      bind_rows()
    v1 <- filter(df_b_long, .data$value == 1)
    df_g <- bind_rows(v2_f, v1) |> select("hospital", "recovery_day")
  }else{
    df_g <- select(df_b_long, "hospital", "recovery_day")
  }

  df_g <- df_g |>
    arrange(.data$recovery_day, .data$hospital) |>
    mutate(d_id = row_number(), .before = 1) |>
    mutate(hospital = factor(.data$hospital, levels = Hosps),
           male = sapply(.data$hospital, function(x) rbinom(n = 1, size = 1, prob = sex_thetas[as.numeric(x)])),
           sex = .data$male + 1,
           don_gender = if_else(.data$sex == 1, "F", "M")
    )

  ov_n <- nrow(df_g)
  ov_nv <- 1:ov_n

  race_levels <- c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")

  hospital_mm <- model.matrix(~hospital, data = df_g)

  cod_levels <- c(1, 2, 3, 4, 999)
  cod_mm <- hospital_mm %*% cod_mat_beta[,-1]
  cod_odds <- t(apply(cod_mm, 1, function(x) exp(x)/(1+sum(exp(x)))))

  race_mm <- hospital_mm %*% race_mat_beta[,-1]
  race_odds <- t(apply(race_mm, 1, function(x) exp(x)/(1+sum(exp(x)))))

  # hgt_betas <- hgt_params[str_which(names(hgt_params), "betas")]
  # hgt_sigma <- hgt_params[str_which(names(hgt_params), "sigma")]
  hgt_betas <- hgt_params[grep("betas", names(hgt_params))]
  hgt_sigma <- hgt_params[grep("sigma", names(hgt_params))]
  hgt_ms <- model.matrix(~don_gender, data = df_g) %*% hgt_betas

  ## Height and Race
  df_g2 <- df_g |>
    mutate(
      race_eth = factor(sapply(ov_nv, function(x) sample(race_levels, size = 1, prob = c(1 - sum(race_odds[x, ]), race_odds[x, ]))), levels = race_levels),
      # hgt_in = plyr::round_any(rnorm(n = ov_n, mean = hgt_ms, sd = hgt_sigma)/2.54, 0.5),
      hgt_in = round(rnorm(n = ov_n, mean = hgt_ms, sd = hgt_sigma)/2.54/0.5)*0.5,
      hgt_cm = .data$hgt_in * 2.54,
      cod = factor(sapply(ov_nv, function(x) sample(cod_levels, size = 1, prob = c(1 - sum(cod_odds[x, ]), cod_odds[x, ]))), levels = cod_levels)
    )


  ## Blood type
  abo_levels <- c("A", "AB", "B", "O")
  abo_bm <- t(apply(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))

  ##   AGE
  # age_betas <- age_params[str_which(names(age_params), "beta")]
  # age_sigma <- age_params[str_which(names(age_params), "sigma")]
  # age_skew <- age_params[str_which(names(age_params), "skew")]
  age_betas <- age_params[grep("beta", names(age_params))]
  age_sigma <- age_params[grep("sigma", names(age_params))]
  age_skew <- age_params[grep("skew", names(age_params))]

  don_org_lvs <- c("DLU", "LUL", "LUR")

  df_g3 <- df_g2 |>
    mutate(abo =  factor(sapply(ov_nv, function(x) sample(abo_levels, size = 1, prob = c(1 - sum(abo_bm[x, ]), abo_bm[x, ]))), levels = abo_levels),
           age = floor(mapply(function(x) rsamp(sn::rsn, n = 1, limits = c(12, 80), xi = age_betas[x], omega = age_sigma[x], alpha = age_skew[x]), .data$cod)),
           don_org = sample(don_org_lvs, size = ov_n, don_org_odds, replace = TRUE)
    )

  smoke_mm <- plogis(model.matrix(~race_eth + don_gender + I(age/10) + I((age/10)^2) + race_eth:don_gender, data = df_g3) %*% smoke_hist_mat_beta)

  dcd_mm <- plogis(model.matrix(~cod, data = df_g3) %*% dcd_params)

  don_org_lvs <- c("DLU", "LUL", "LUR")

  df_g4 <- df_g3 |>
    mutate(
      smoke_hist = rbinom(n = ov_n, size = 1, prob = smoke_mm),
      gt_20pkyr = ifelse(.data$smoke_hist == 1, "Y", "N"),
      don_org = factor(sample(don_org_lvs, size = ov_n, replace = TRUE, prob = don_org_odds)),
      don_dcd = rbinom(n = ov_n, size = 1, prob = dcd_mm),
      don_util = rbinom(n = ov_n, size = 1, prob = don_util_odds[2])
    )

  df_g00 <- df_g4 |>
    mutate(
      hospital = as.double(.data$hospital),
      organs_avl = ifelse(.data$don_org == "DLU", 2, 1)
    ) |>
    select(-c("sex", "don_gender"))

  return(df_g00)

}

#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr as_tibble
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom stats rgamma
#' @importFrom stats rbinom
#' @importFrom stats rpois
#' @importFrom rlang .data
spawn_candidates <- function(days, daily_lambdas_center, sex_thetas, race_mat_beta, #dx_mat_beta,
                             abo_mat_beta,
                             age_params,  hgt_params, wgt_params, wgt_params_c, diab_params,
                             airway_params, oxygen_params, resp_mat_beta, surg_mat_beta,
                             fev_params, fvc_params, pco2_params, pf_params, po2_params, mpap_params,
                             o2_freq_mat, vent_mat_beta,
                             supp_o2_params, walk6m_params, bili_params, creat_params,
                             spap_params, ci_params, cvp_params, fs_mat_beta, pco2_15_params,
                             sa_params, lam_params, eisen_params, sd_params,
                             ar_mat_beta, dr_mat_beta){

  if(days <= 1L)stop("days must be greater than 1")

  # Centers <- 1:max(as.numeric(str_extract(names(daily_lambdas_center), "\\d+")))
  Centers <- 1:max(as.numeric(regmatches(names(daily_lambdas_center), m = regexpr("\\d+", text = names(daily_lambdas_center)))))
  names(Centers) <- Centers

  df_b <- sapply(daily_lambdas_center, function(x) rpois(n = days, lambda = x)) |>
    as.data.frame() |>
    mutate(listing_day = row_number()) |>
    pivot_longer(-"listing_day", names_sep = ",", names_to = c("center", "dx_num"),
                 # names_transform = list("center" = ~as.numeric(str_extract(.x, "\\d+")),
                 #                        "dx_num" = ~as.numeric(str_extract(.x, "\\d+")))) |>
                 names_transform = list("center" = ~as.numeric(regmatches(.x, m = regexpr("\\d+", text = .x))),
                                        "dx_num" = ~as.numeric(regmatches(.x, m = regexpr("\\d+", text = .x))))) |>
    filter(.data$value > 0)

  if(any(df_b$value > 1)){
    v2 <- filter(df_b, .data$value > 1)
    colnames(v2) <- NULL

    # v2_f <- purrr::pmap(v2, ~tibble(center = rep(..2, ..4), listing_day = ..1, dx_num = ..3)) |>
    #   purrr::list_rbind()
    v2_f <- apply(v2, 1,  function(x) tibble(center = x[2], listing_day = as.integer(x[1]), dx_num = x[3], .rows = x[4])) |>
      bind_rows()

    v1 <- filter(df_b, .data$value == 1)
    df_g <- bind_rows(v2_f, v1) |>
      select("center", "listing_day", "dx_num")
  }else{
    df_g <- select(df_b, "center", "listing_day", "dx_num")
  }

  ov_n <- nrow(df_g)
  ov_nv <- 1:ov_n

  df_g <- df_g |>
    arrange(.data$listing_day, .data$center) |>
    mutate(c_id = row_number(), .before = 1) |>
    mutate(center = factor(.data$center, levels = Centers),
           male = sapply(.data$dx_num, function(x) rbinom(n = 1, size = 1, prob = sex_thetas[as.numeric(x)])),
           sex = .data$male + 1,
           can_gender = factor(if_else(.data$sex == 1, "F", "M"), levels = c("F", "M")),
           dx_grp = factor(LETTERS[.data$dx_num], LETTERS[1:4])
    )

  ## Interaction of sex and diagnosis group
  dx_s <- data.frame(dx_grp = factor(c('A', 'A', 'B', 'B', 'C', 'C', 'D', 'D'), levels = c('A', 'B', 'C', 'D')),
             can_gender = factor(c('F', 'M', 'F', 'M', 'F', 'M', 'F', 'M'), levels = c('F', 'M')),
             dx_sex_int = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8)))

  race_levels <- c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")
  race_odds <- calc_multi_odds(model.matrix(~center, data = df_g) %*% race_mat_beta[,-1])

  df_g2 <- df_g |>
    mutate(
      race_eth = factor(sapply(ov_nv,
                               function(x) sample(race_levels, size = 1, prob = race_odds[x, ])), levels = race_levels),
      ## age
      # age_at_listing = plyr::round_any(
      #   sapply(.data$dx_num,
      #          function(x) rsamp(sn::rsn, n = 1, limits = c(18, 85),
      #                            # xi = age_params[str_which(names(age_params), "xi")][x],
      #                            # omega = age_params[str_which(names(age_params), "omega")][x],
      #                            # alpha = age_params[str_which(names(age_params), "alpha")][x])),
      #                            xi = age_params[grep("xi", names(age_params))][x],
      #                            omega = age_params[grep("omega", names(age_params))][x],
      #                            alpha = age_params[grep("alpha", names(age_params))][x])),
      #   (1/365)),
      age_at_listing = round(
        sapply(.data$dx_num,
               function(x) rsamp(sn::rsn, n = 1, limits = c(18, 85),
                                 # xi = age_params[str_which(names(age_params), "xi")][x],
                                 # omega = age_params[str_which(names(age_params), "omega")][x],
                                 # alpha = age_params[str_which(names(age_params), "alpha")][x])),
                                 xi = age_params[grep("xi", names(age_params))][x],
                                 omega = age_params[grep("omega", names(age_params))][x],
                                 alpha = age_params[grep("alpha", names(age_params))][x]))/(1/365))*(1/365),
    ) |>
    left_join(as_tibble(dx_s), by = c("can_gender", "dx_grp"))

  # hgt_betas <- hgt_params[str_which(names(hgt_params), "betas")]
  # hgt_sigma <- hgt_params[str_which(names(hgt_params), "sigma")]
  hgt_betas <- hgt_params[grep("betas", names(hgt_params))]
  hgt_sigma <- hgt_params[grep("sigma", names(hgt_params))]
  hgt_ms <- model.matrix(~can_gender + dx_grp, data = df_g2) %*% hgt_betas

  abo_levels <- c("A", "AB", "B", "O")
  abo_odds <- calc_multi_odds(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1])

  # airway_xis <- airway_params[str_which(names(airway_params), "tau")]
  # airway_sigs <- airway_params[str_which(names(airway_params), "sigmas")]
  # airway_skews <- airway_params[str_which(names(airway_params), "skews")]
  #
  # oxygen_nus <- oxygen_params[str_which(names(oxygen_params), "nu")]
  # oxygen_taus <- oxygen_params[str_which(names(oxygen_params), "tau")]
  # oxygen_beta <- oxygen_params[str_which(names(oxygen_params), "beta")]
  # oxygen_sig <- oxygen_params[str_which(names(oxygen_params), "sigma")]

  airway_xis <- airway_params[grep("tau", names(airway_params))]
  airway_sigs <- airway_params[grep("sigmas", names(airway_params))]
  airway_skews <- airway_params[grep("skews", names(airway_params))]

  oxygen_nus <- oxygen_params[grep("nu", names(oxygen_params))]
  oxygen_taus <- oxygen_params[grep("tau", names(oxygen_params))]
  oxygen_beta <- oxygen_params[grep("beta", names(oxygen_params))]
  oxygen_sig <- oxygen_params[grep("sigma", names(oxygen_params))]

  ## I might store this elsewhere
  rtc <- function(n, df, sigma = 1, mean = 0){
    y <- stats::rt(n=n, df = df) * sigma + mean
    return(y)
  }

  df_g2b <- df_g2 |>
    mutate(
      airway = mapply(function(y, z) rsamp(sn::rsn, limits = c(-1.8, 3.3), n = 1, xi = airway_xis[z],
                                           omega = airway_sigs[y], alpha = airway_skews[y]), .data$dx_num, .data$dx_sex_int),
      oxygen = mapply(function(z, af) rsamp(rtc, limits = c(-2.6, 4.5), n = 1, df = oxygen_nus[z],
                                            sigma = oxygen_sig[z], mean = oxygen_taus[z] + oxygen_beta * af), .data$dx_sex_int, .data$airway),
      abo =  factor(sapply(ov_nv, function(x) sample(abo_levels, size = 1, prob = abo_odds[x, ])),
                    levels = abo_levels),

      ## Height will be rounded to nearest 0.5 inch and converted to centimeters
      # hgt_in = plyr::round_any(sapply(hgt_ms, function(x) rnorm(n = 1, mean = x, sd = hgt_sigma))/2.54, .5),
      hgt_in = round(sapply(hgt_ms, function(x) rnorm(n = 1, mean = x, sd = hgt_sigma))/2.54/.5)*.5,
      # hgt_in = round(rnorm(n = 1, mean = hgt_ms, sd = hgt_sigma)/2.54/.5)*.5,
      hgt_cm = .data$hgt_in * 2.54
    )

  # wgt_betas <- wgt_params[str_which(names(wgt_params), "betas")]
  # wgt_sigma <- wgt_params[str_which(names(wgt_params), "sigma")]
  #
  # wgt_betas_c <- wgt_params_c[str_which(names(wgt_params_c), "betas")]
  # wgt_sigma_c <- wgt_params_c[str_which(names(wgt_params_c), "sigma")]
  # wgt_skew_c <- wgt_params_c[str_which(names(wgt_params_c), "skew")]

  wgt_betas <- wgt_params[grep("betas", names(wgt_params))]
  wgt_sigma <- wgt_params[grep("sigma", names(wgt_params))]

  wgt_betas_c <- wgt_params_c[grep("betas", names(wgt_params_c))]
  wgt_sigma_c <- wgt_params_c[grep("sigma", names(wgt_params_c))]
  wgt_skew_c <- wgt_params_c[grep("skew", names(wgt_params_c))]

  ## Splitting and recombining Group C to esimate weight
  df_g2b_c <- filter(df_g2b, .data$dx_grp == "C") |>  mutate(dx_grp = factor(.data$dx_grp, levels = c("C")))
  df_g2b_mc <- filter(df_g2b, .data$dx_grp != "C") |>  mutate(dx_grp = factor(.data$dx_grp, levels = c("A", "B", "D")))

  wgt_dm <- model.matrix(~male + dx_grp + hgt_cm, data = df_g2b_mc) %*% wgt_betas
  df_g2b_mc <- mutate(df_g2b_mc,
                      wgt_kg = round(sapply(wgt_dm, function(x)
                        rsamp(rnorm, limits = c(20, 300), n = 1, mean = x, sd = wgt_sigma)), 2)
  )

  if(nrow(df_g2b_c) > 0L){
    wgt_dm_c <- model.matrix(~male + hgt_cm, data = df_g2b_c) %*% wgt_betas_c

    df_g3 <- df_g2b_c |>
      mutate(wgt_kg = round(sapply(wgt_dm_c, function(x) rsamp(sn::rsn, limits = c(20, 300),
                                                               n = 1, xi = x, omega = wgt_sigma_c, alpha = wgt_skew_c)), 2)) |>
      bind_rows(df_g2b_mc) |>
      arrange(.data$c_id) |>
      mutate(dx_grp = factor(.data$dx_grp, levels = LETTERS[1:4]),
             bmi = .data$wgt_kg/((.data$hgt_cm/100)^2))
  }else{

    df_g3 <- df_g2b_mc |>
      arrange(.data$c_id) |>
      mutate(dx_grp = factor(.data$dx_grp, levels = LETTERS[1:4]),
             bmi = .data$wgt_kg/((.data$hgt_cm/100)^2))
  }

  ## Diabmetes
  diab_odds <- plogis(model.matrix(~male + age_at_listing + race_eth + race_eth:male, data = df_g3) %*% diab_params)

  ## Respiratory Support Cluster Odds
  rc_odds <- calc_multi_odds(model.matrix(~male + dx_grp + factor(dx_sex_int, levels = 1:8) + age_at_listing + airway + oxygen, data = df_g3) %*% (resp_mat_beta)[,-1])

  ## interaction of diagnosis group and respiratory support cluster
  int2 <- data.frame(dx_grp = factor(c('A', 'B', 'C', 'D', 'A', 'B', 'C', 'D', 'A', 'B', 'C', 'D', 'A', 'B', 'C', 'D'), levels = c('A', 'B', 'C', 'D')),
                     resp_supp = factor(c('1', '1', '1', '1', '2', '2', '2', '2', '3', '3', '3', '3', '4', '4', '4', '4'), levels = c('1', '2', '3', '4')),
                     dx_rc = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)))

  ## Adding suregey type
  ## Double (D), ## Single (S) or Either (E)
  surg_lvs <- c("D", "E", "S")

  surg_odds <- calc_multi_odds(model.matrix(~dx_grp, data = df_g3) %*% surg_mat_beta[,-1])

  df_g4 <- df_g3 |>
    mutate(resp_supp = as.factor(sapply(ov_nv,
                                        function(x) sample(1:4, size = 1, prob = rc_odds[x,]))),
           surg_type = as.factor(sapply(ov_nv,
                                        function(x) sample(surg_lvs, size = 1, prob = surg_odds[x, ]))),
           diab = rbinom(n = ov_n, size = 1 , prob = diab_odds)
    ) |>
    left_join(int2, by = c("dx_grp", "resp_supp"))

  air_mat <- model.matrix(~airway + can_gender + dx_grp + age_at_listing + airway:dx_grp + age_at_listing:dx_grp, data = df_g4)
  oxy_mat <- model.matrix(~oxygen + can_gender + dx_grp + age_at_listing + oxygen:dx_grp + age_at_listing:dx_grp, data = df_g4)
  ## PCO2 and PO2 are special
  air_mat2 <- model.matrix(~airway + can_gender + dx_grp + airway:dx_grp, data = df_g4)
  oxy_mat2 <- model.matrix(~oxygen + can_gender + dx_grp + oxygen:dx_grp, data = df_g4)


  resp_mat <- model.matrix(~can_gender + dx_grp + resp_supp + age_at_listing + airway + oxygen, data = df_g4)
  walk6m_mat <- model.matrix(~can_gender + dx_grp + resp_supp + age_at_listing + airway + oxygen + dx_grp:resp_supp + age_at_listing:dx_grp, data = df_g4)

  ##airway
  # fev_beta_m <- fev_params[str_which(names(fev_params), "betas")]
  # fev_sigma <- fev_params[str_which(names(fev_params), "sigma")]
  # fvc_beta_m <- fvc_params[str_which(names(fvc_params), "betas")]
  # fvc_sigma <- fvc_params[str_which(names(fvc_params), "sigma")]
  # pco2_beta_m <- pco2_params[str_which(names(pco2_params), "betas")]
  # pco2_sigma <- pco2_params[str_which(names(pco2_params), "sigma")]
  # ## oxygen
  # pf_beta_m <- pf_params[str_which(names(pf_params), "betas")]
  # pf_sigma <- pf_params[str_which(names(pf_params), "sigma")]
  # po2_beta_m <- po2_params[str_which(names(po2_params), "betas")]
  # po2_sigma <- po2_params[str_which(names(po2_params), "sigma")]
  # mpap_beta_m <- mpap_params[str_which(names(mpap_params), "betas")]
  # mpap_sigma <- mpap_params[str_which(names(mpap_params), "sigma")]
  fev_beta_m <- fev_params[grep("betas", names(fev_params))]
  fev_sigma <- fev_params[grep("sigma", names(fev_params))]
  fvc_beta_m <- fvc_params[grep("betas", names(fvc_params))]
  fvc_sigma <- fvc_params[grep("sigma", names(fvc_params))]
  pco2_beta_m <- pco2_params[grep("betas", names(pco2_params))]
  pco2_sigma <- pco2_params[grep("sigma", names(pco2_params))]
  ## oxygen
  pf_beta_m <- pf_params[grep("betas", names(pf_params))]
  pf_sigma <- pf_params[grep("sigma", names(pf_params))]
  po2_beta_m <- po2_params[grep("betas", names(po2_params))]
  po2_sigma <- po2_params[grep("sigma", names(po2_params))]
  mpap_beta_m <- mpap_params[grep("betas", names(mpap_params))]
  mpap_sigma <- mpap_params[grep("sigma", names(mpap_params))]

  # supp_o2_odds <- calc_multi_odds(supp_o2_mat)
  o2f_odds <- calc_multi_odds(resp_mat %*% o2_freq_mat[,-1])
  vent_odds <- calc_multi_odds(resp_mat %*% vent_mat_beta[,-1])

  ## levels
  o2f_levels <- c("At Rest", "At Night",  "While Exercising", "None")
  vent_levels <- c("BiPap", "Mechanical", "None", "CPAP")

  ## Six Minute Walk
  # walk6m_theta <- walk6m_params[str_which(names(walk6m_params), "theta")]
  # walk6m_sigma <- walk6m_params[str_which(names(walk6m_params), "sigma")]
  # walk6m_beta <- walk6m_params[str_which(names(walk6m_params), "beta")]
  # walk6m_skew <- walk6m_params[str_which(names(walk6m_params), "skew")]
  walk6m_theta <- walk6m_params[grep("theta", names(walk6m_params))]
  walk6m_sigma <- walk6m_params[grep("sigma", names(walk6m_params))]
  walk6m_beta <- walk6m_params[grep("beta", names(walk6m_params))]
  walk6m_skew <- walk6m_params[grep("skew", names(walk6m_params))]

  walk6m_ms <- walk6m_mat %*% walk6m_beta

  df_g5 <- df_g4 |>
    mutate(
      ## airway
      fev1 = round(rsamp(rnorm, limits = c(4.5, 120.5), n = ov_n, mean = air_mat %*% fev_beta_m, sd = fev_sigma), 0),
      fvc = round(rsamp(rnorm, limits = c(-0.5, 150), n = ov_n, mean = air_mat %*% fvc_beta_m, sd = fvc_sigma), 0),
      pco2 = round(1/(rsamp(rnorm, limits = c(1/160, Inf), n = ov_n, mean = air_mat2 %*% pco2_beta_m, sd = pco2_sigma)), 0),
      ## Oxygen
      pf = (rsamp(rnorm, limits = c(sqrt(30), Inf), n = ov_n, mean = oxy_mat %*% pf_beta_m, sd = pf_sigma)^2),
      po2 = round(1/rsamp(rnorm, limits = c(1/500, 1/30), n = ov_n, mean = oxy_mat2 %*% po2_beta_m, sd = po2_sigma) ,0),
      meanpap = round(exp(rsamp(rnorm, limits = c(log(5), log(110)), n = ov_n, mean = oxy_mat %*% mpap_beta_m, sd = mpap_sigma)) * 3, 0)/3,
      o2_freq = factor(
        sapply(ov_nv, function(x) sample(o2f_levels, size = 1, prob = o2f_odds[x, ])),
        levels = o2f_levels),
      vent = factor(
        sapply(ov_nv, function(x) sample(vent_levels, size = 1, prob = vent_odds[x, ])),
        levels = vent_levels),
      ## Extra
      walk6m = round(sapply(walk6m_ms, function(x) sn::rsn(n = 1, xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
                       sapply(.data$dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),
      ##### Limit to positive scope here(?)
      # walk6m = round(sapply(walk6m_ms, function(x) rsamp(sn::rsn, n = 1, limits = c(0,5000), xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
      #                  sapply(dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),

    ) |>
    ## in case there are a few negative values
    mutate(walk6m = ifelse(.data$walk6m < 0, 0, .data$walk6m))

  ## resp support
  # supp_o2_betas <- supp_o2_params[str_which(names(supp_o2_params), "beta")]
  # supp_o2_ints <- supp_o2_params[str_which(names(supp_o2_params), "cs")]
  supp_o2_betas <- supp_o2_params[grep("beta", names(supp_o2_params))]
  supp_o2_ints <- supp_o2_params[grep("cs", names(supp_o2_params))]

  df_g5_dx_num <- df_g5$dx_num
  df5_supp_o2_ints <- lapply(df_g5_dx_num, function(x) supp_o2_ints[grepl(paste0(",", x, "\\]"), names(supp_o2_ints))])
  resp_mat2 <- model.matrix(~can_gender + resp_supp + age_at_listing + airway + oxygen + o2_freq, data = df_g5)
  supp_o2_mat <- apply(mapply(function(x, y) plogis(x - y), resp_mat2 %*% supp_o2_betas, df5_supp_o2_ints, SIMPLIFY = TRUE)
                       , 2, function(x) lag(x, 1, default = 1) - x)

  # ## Bilirubin
  # bili_betas <- bili_params[str_which(names(bili_params), "betas")]
  # bili_sigma <- bili_params[str_which(names(bili_params), "sigma")]
  # ## Creatinie
  # creat_betas <- creat_params[str_which(names(creat_params), "betas")]
  # creat_sigma <- creat_params[str_which(names(creat_params), "sigma")]
  # ## Systolic PAP
  # spap_betas <- spap_params[str_which(names(spap_params), "betas")]
  # spap_sigma <- spap_params[str_which(names(spap_params), "sigma")]
  bili_betas <- bili_params[grep("betas", names(bili_params))]
  bili_sigma <- bili_params[grep("sigma", names(bili_params))]
  ## Creatinie
  creat_betas <- creat_params[grep("betas", names(creat_params))]
  creat_sigma <- creat_params[grep("sigma", names(creat_params))]
  ## Systolic PAP
  spap_betas <- spap_params[grep("betas", names(spap_params))]
  spap_sigma <- spap_params[grep("sigma", names(spap_params),)]
  spap_sigmas <- rep(spap_sigma[1], ov_n)
  spap_sigmas[which(df_g5$dx_grp == "B")] <- spap_sigma[2]
  ## Cardiac Index
  # ci_betas <- ci_params[str_which(names(ci_params), "betas")]
  # ci_sigma <- ci_params[str_which(names(ci_params), "sigma")]
  # ## Central Venous Pressure
  # cvp_betas <- cvp_params[str_which(names(cvp_params), "betas")]
  # cvp_beta_mat <- matrix(cvp_betas, ncol = max(as.numeric(str_extract(names(cvp_betas), "(?<=,)\\d+")), na.rm = TRUE),
  #                        nrow = max(as.numeric(str_extract(names(cvp_betas), "\\d+(?=,)")), na.rm = TRUE))
  # cvp_alphas <- cvp_params[str_which(names(cvp_params), "alpha")]
  ci_betas <- ci_params[grep("betas", names(ci_params))]
  ci_sigma <- ci_params[grep("sigma", names(ci_params))]
  ## Central Venous Pressure
  cvp_betas <- cvp_params[grep("betas", names(cvp_params),)]
  cvp_beta_mat <- matrix(cvp_betas, ncol = max(as.numeric(regmatches(names(cvp_betas), m = regexpr("(?<=,)\\d+", text = names(cvp_betas), perl = TRUE))), na.rm = TRUE),
                         nrow = max(as.numeric(regmatches(names(cvp_betas), m = regexpr("\\d+(?=,)", text = names(cvp_betas), perl = TRUE))), na.rm = TRUE))
  cvp_alphas <- cvp_params[grep("alpha", names(cvp_params))]

  ##
  bili_mat <- model.matrix(~dx_grp + age_at_listing + can_gender, data = df_g5) %*% bili_betas
  creat_mat <- model.matrix(~dx_grp + age_at_listing + can_gender + dx_grp:age_at_listing, data = df_g5) %*% creat_betas
  spap_mat <- model.matrix(~meanpap + dx_grp + meanpap:dx_grp + age_at_listing*dx_grp, data = df_g5) %*% spap_betas
  ci_mat <- model.matrix(~dx_grp + hgt_cm + wgt_kg, data = df_g5) %*% ci_betas
  ci_sigmas <- ifelse(df_g5$dx_grp == "B", ci_sigma[2], ci_sigma[1])
  cvp_mat <- model.matrix(~meanpap, data = df_g5)
  cvp_g <- dplyr::pull(df_g5, .data$dx_num)

  ## functional status
  fs_levels <- c("None", "Some", "Total")
  fs_odds <- calc_multi_odds(model.matrix(~ resp_supp + dx_grp, data = df_g5) %*% fs_mat_beta[,-1])

  df_g5b <- df_g5 |>
    mutate(
      o2 = apply(supp_o2_mat, 2, function(x) sample(0:nrow(supp_o2_mat), size = 1, prob = c(x, 1 - sum(x)))),
      bili = round(exp(rnorm(n = ov_n, mean = bili_mat, sd = bili_sigma)), 2),
      creat = round(exp(rnorm(n = ov_n, mean = creat_mat, sd = creat_sigma)), 2),
      syst_pap = round(rsamp(rnorm, limits = c(4, 170), n = ov_n, mean = spap_mat, sd = spap_sigmas), 0),
      ci = round(exp(rnorm(n = ov_n, mean = ci_mat, sd = ci_sigmas)), 2),
      funstat = factor(sapply(ov_nv, function(x) sample(fs_levels, size = 1, prob = fs_odds[x, ])),
                       fs_levels),
      ecmo = ifelse(.data$resp_supp == 4, 1, 0),
      cvp = round(sapply(ov_nv, function(x) rgamma(n = 1,
                                                   shape = cvp_mat[x,] %*% cvp_beta_mat[, cvp_g[x]],
                                                   rate = cvp_alphas[cvp_g[x]])), 0)
    )

  # return(df_g5b)

  df_g5b_p15 <- filter(df_g5b, .data$pco2 >= 46)
  df_g5b_p15r <- filter(df_g5b, .data$pco2 < 46) |>  mutate(pco2_15 = 0)

  if(nrow(df_g5b_p15)>0L){
    pco2_15_ms <- model.matrix(~ as.factor(resp_supp) + dx_grp + pco2, data = df_g5b_p15) %*% pco2_15_params

    df_g5b_p15 <- mutate(df_g5b_p15, pco2_15 = rbinom(n = nrow(pco2_15_ms), size = 1, prob = plogis(pco2_15_ms)))

    df_g6 <- bind_rows(df_g5b_p15, df_g5b_p15r) |>
      replace_na(list(pco2_15 = 0)) |>
      arrange(.data$c_id)
  }else{
    df_g6 <- df_g5b_p15r |>
      arrange(.data$c_id)
  }

  ## Adding specific diseases
  ## Group A
  df_g6a <- filter(df_g6, .data$dx_grp == "A")

  df_g6as <- filter(df_g6a, .data$meanpap <= 30)

  ar_levels <- c("bronch" = 1, "NA" = 99)

  if(nrow(df_g6as) > 0L){
    sa_odds <- plogis(model.matrix(~center, data = df_g6as) %*% sa_params)
    df_g6as <- df_g6as |>
      mutate(sarcoidA = rbinom(n = max(dplyr::row_number()), size = 1, prob = sa_odds))

    sarcA <- filter(df_g6as, .data$sarcoidA == 1)

    df_g6ar <- filter(df_g6a, !(.data$c_id %in% sarcA$c_id))
  }else{
    df_g6ar <- mutate(df_g6a, sarcoidA = 0)
    sarcA <- filter(df_g6ar, .data$sarcoidA == 1)
  }

  if(nrow(df_g6ar) > 0L){
    df_g6ar_f <- filter(df_g6ar, .data$male == 0)
    lam_odds <- plogis(model.matrix(~ 1, data = df_g6ar_f) %*% lam_params)
    # df_g6ar_f <- mutate(df_g6ar_f, lymphan = sapply(lam_odds, function(x) rbinom(n = 1, size = 1, prob = x)))
    df_g6ar_f <- mutate(df_g6ar_f, lymphan = rbinom(n = length(lam_odds), size = 1, prob = lam_odds))
    lamA <- filter(df_g6ar_f, .data$lymphan == 1)
    df_g6ar2 <- filter(df_g6ar, !(.data$c_id %in% lamA$c_id))
  }else{
    df_g6ar2 <- mutate(df_g6ar, lymphan = 0)
    lamA <- filter(df_g6ar2, .data$lymphan == 1)
  }

  if(nrow(df_g6ar2) > 0L){

    # ar_odds <- t(apply(model.matrix(~center, data = df_g6ar2) %*% ar_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))

    ar_odds <- calc_binom_odds(model.matrix(~center, data = df_g6ar2) %*% ar_mat_beta[,-1])

    # return(ar_odds)

    df_g6ar2 <- df_g6ar2 |>
      ## if more than two diseases
      mutate(ga_sd = sapply(1:nrow(ar_odds), function(x) sample(ar_levels, size = 1, prob = ar_odds[x, ])))

    ga_sas <- sapply(ar_levels[-length(ar_levels)],  function(x) ifelse(df_g6ar2$ga_sd == x, 1, 0))

    if(class(ga_sas)[1] == "numeric"){
      # ga_sas = dplyr::tibble(bronch = ga_sas)
      # ga_sas <- tibble(ga_sas)
      # colnames(ga_sas) <- names(ar_levels)[-length(ar_levels)]

      ga_sas <- tibble(ga_sas) |> mutate(name = names(ar_levels[-length(ar_levels)])) |>
        tidyr::pivot_wider(values_from = ga_sas)
      # ga_sas <- tibble::as_tibble_row(ga_sas)
      # colnames(ga_sas) <- gsub("\\..*", "", colnames(ga_sas))
    }

    df_g6ar2 <- cbind(df_g6ar2, ga_sas)

    ## All the A's put back together
    df_g6a2 <- bind_rows(sarcA, lamA, df_g6ar2)
  }else{
    df_g6are <- tibble(name = names(ar_levels[-length(ar_levels)]), value = 0) |> tidyr::pivot_wider() |>
      mutate(ga_sd = ar_levels[length(ar_levels)])

    df_g6a2 <- bind_cols(bind_rows(sarcA, lamA), df_g6are)
  }

  ## Group B
  df_g6b <- filter(df_g6, .data$dx_grp == "B")

  if(nrow(df_g6b) >0L){
    eisen_odds <- plogis(model.matrix(~ 1, data = df_g6b) %*% eisen_params)
    # df_g6b2 <- mutate(df_g6b, eisen = sapply(eisen_odds, function(x) rbinom(n = 1, size = 1, prob = x)))
    df_g6b2 <- mutate(df_g6b, eisen = rbinom(n = length(eisen_odds), size = 1, prob = eisen_odds))
  }else{
    df_g6b2 <- mutate(df_g6b, eisen = 0)
  }

  ## Group D
  df_g6d <- filter(df_g6, .data$dx_grp == "D")

  df_g6ds <- filter(df_g6d, .data$meanpap > 30)

  dr_levels <- c("pulm_fib" = 1, "co_bronch" = 2, "oblit" = 3, "NA" = 99)

  if(nrow(df_g6ds) >0L){

    sd_odds <- plogis(model.matrix(~center, data = df_g6ds) %*% sd_params)

    df_g6ds <- df_g6ds |>
      mutate(sarcoidD = rbinom(n = max(dplyr::row_number()), size = 1, prob = sd_odds))

    sarcD <- filter(df_g6ds, .data$sarcoidD == 1)

    df_g6dr <- filter(df_g6d, !(.data$c_id %in% sarcD$c_id))
  }else{
    df_g6dr <- mutate(df_g6d, sarcoidD = 0)
    sarcD <- filter(df_g6dr, .data$sarcoidD == 1)
  }
  if(nrow(df_g6dr) > 0L){

    # dr_odds <- t(apply(model.matrix(~center, data = df_g6dr) %*% dr_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))

    dr_odds <- calc_multi_odds(model.matrix(~center, data = df_g6dr) %*% dr_mat_beta[,-1])

    df_g6dr <- df_g6dr |>
      mutate(gd_sd = sapply(1:nrow(dr_odds), function(x) sample(dr_levels, size = 1, prob = dr_odds[x, ])))

    gd_sds <- sapply(dr_levels[-length(dr_levels)],  function(x) ifelse(df_g6dr$gd_sd == x, 1, 0))

    if(class(gd_sds)[1] == "numeric"){
      # gd_sds <- tibble::as_tibble_row(gd_sds)
      # colnames(gd_sds) <- gsub("\\..*", "", colnames(gd_sds))
      gd_sds <- tibble(gd_sds) |> mutate(name = names(dr_levels[-length(dr_levels)])) |> tidyr::pivot_wider(values_from = gd_sds)
      # colnames(gd_sds) <- names(dr_levels)[-length(dr_levels)]
    }

    df_g6dr <- cbind(df_g6dr, gd_sds)

    ## Put the D's back together
    df_g6d2 <- bind_rows(sarcD, df_g6dr)
  }else{
    df_g6dre <- tibble(name = names(dr_levels[-length(dr_levels)]), value = 0) |> tidyr::pivot_wider() |>
      mutate(gd_sd = dr_levels[length(dr_levels)])
    df_g6d2 <- bind_cols(sarcD, df_g6dre)
  }

  ## Put it all back togehter
  df_g7 <- filter(df_g6, .data$dx_grp == "C") |>
    bind_rows(df_g6a2, df_g6d2, df_g6b2) |>
    arrange(.data$c_id)

  df_g7[is.na(df_g7)] <- 0

  ## PRA and PLD
  ## Creatine and Bilirubin thresholds
  df_g8 <- mutate(df_g7, pra = 0, pld = 0, creat_150 = 0, bili_50 = 0)

  ## Remove and renaming columns, collapsing variables
  df_g00 <- df_g8 |>
    mutate(age = .data$age_at_listing,
           hgt_cm1 = round(.data$hgt_cm, 0),
           ## max height for unos
           # hgt_cm1 = ifelse(hgt_cm1 > 223, 223, ifelse(hgt_cm1 < 63, 63, hgt_cm1)),
           o2rest = ifelse(.data$o2_freq == "At Rest", .data$o2, 0),
           cont_mech = ifelse(.data$vent == "Mechanical", 1, 0),
           funstat = as.numeric(.data$funstat),
           o2_freq = as.numeric(.data$o2_freq),
           dx = case_when(
             sarcoidA == 1 | sarcoidD == 1 ~ 1605,
             bronch == 1 ~ 1608,
             lymphan == 1 ~ 1611,
             eisen == 1 ~ 1500,
             pulm_fib == 1 ~ 1613,
             co_bronch == 1 ~ 106,
             oblit == 1 ~ 1612,
             TRUE ~ -99) ## Marker for other diagnosis, may add more later
    ) |>
    select(-c("dx_rc", "dx_num", "dx_sex_int", "can_gender", "sex", "ga_sd", "gd_sd", "sarcoidA", "sarcoidD", "bronch", "lymphan", "pulm_fib", "co_bronch", "oblit", "eisen")) |>
    rename(pap_syst = "syst_pap", pap_mean = "meanpap") |>
    relocate("dx_grp", .after = "male") |>
    left_join(pABOs, by = "abo") |>
    left_join(pHGTs, by = c("hgt_cm1"= "cand_hgt", "dx_grp")) |>
    left_join(pCPRAs, by = c("pra" = "CPRA"))

  return(df_g00)

}

## This only works when ... are constant
rsamp <- function(FUN, limits, ...){
  dots <- list(...)
  l <- formals(FUN)
  if(!("n" %in% names(l))){
    stop("sample size (n) not present in called FUN")
  }
  y <- do.call(FUN, dots)

  if(length(limits)!=2L){
    stop("Limits must be of length two")
  }
  mn <- min(limits)
  mx <- max(limits)
  i <- 1
  while(any(y<mn|y>mx)){
    n1 <- sum(y<mn|y>mx)
    dots["n"] <- n1
    i
    wn1 <- which(y<mn|y>mx)
    ## This has to change the x changes
    dots2 <- lapply(dots, function(x) if(length(x)>1L) x[wn1] else x)
    y[wn1] <- do.call(FUN, dots2)

  }
  return(y)
}

## designed so ... can be vectors
# rsamp2 <- rsamp <- function(FUN, limits, ...){
#   dots <- list(...)
#   return(dots)
#   l <- formals(FUN)
#   if(!("n" %in% names(l))){
#     stop("sample size (n) not present in called FUN")
#   }
#   y <- do.call(FUN, dots)
#
#   if(length(limits)!=2L){
#     stop("Limits must be of length two")
#   }
#   mn <- min(limits)
#   mx <- max(limits)
#   i <- 1
#   while(any(y<mn|y>mx)){
#     n1 <- sum(y<mn|y>mx)
#     dots["n"] <- n1
#     wn1 <- which(y<mn|y>mx)
#     arg_c <- sapply(dots, length)
#     if(any(arg_c>1)){
#       arg_c <- arg_c[which(arg_c>1)]
#     }
#
#     ## This has to change the x changes
#     dots2 <- lapply(dots, function(x) if(length(x)>1L) x[wn1] else x)
#     y[wn1] <- do.call(FUN, dots2)
#
#   }
#   return(y)
# }
# s1 <- c(1, 6, 9)
# ms <- c(10, 100, 500)
# sds <- c(0, 25)
# vl <- 2
#
# function(locations, values){
#   if(any(locations > values)){
#     locations[which(locations > values)] <- locations[which(locations > vl)] %% values
#     locations[]
#   }
# }
#
# if(any(s1 > vl)){
#   s1[which(s1 > vl)] <- s1[which(s1 > vl)] %% vl
#   s1 <- ifelse(s1==0, s1 + vl, s1)
#   ms2 <- ms[s1]
#   # sds2 <- sds[s1]
#
# }

calc_multi_odds <- function(mat){
  mat2 <- t(apply(mat, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  mat2 <- cbind(1-rowSums(mat2), mat2)
  return(mat2)
}

calc_binom_odds <- function(mat){
  imat <- plogis(mat)
  mat2 <- cbind(1-imat, imat)
  return(mat2)
}
