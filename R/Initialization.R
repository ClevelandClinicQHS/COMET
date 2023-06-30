#' @rdname gen_and_spawn
#'
#' @param days number of days to spawn for
#' @param daily_lambdas_center lambba
#' @param sex_thetas sex
#' @param race_mat_beta race
#' @param hgt_params height
#' @param abo_mat_beta abo blood type
#' @param age_params age
#' @param cod_mat_beta cause of death
#' @param smoke_hist_mat_beta smoking history
#' @param don_org_odds left, right or double lung
#' @param dcd_params dcd status
#' @param don_util_odds utilization rate (transplanted in US)
#'
#' @returns dataset of donors
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr as_tibble
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom stringr str_which
#' @importFrom dplyr select
spawn_donors <- function(days, daily_lambdas_center, sex_thetas,
                         race_mat_beta, hgt_params, abo_mat_beta, age_params, cod_mat_beta,
                         smoke_hist_mat_beta, don_org_odds, dcd_params, don_util_odds){

  if(days <= 1L)stop("days must be greater than 1")

  Hosps <- 1:length(daily_lambdas_center)
  names(Hosps) <- Hosps

  df_b <- as.data.frame(mapply(function(x) rpois(daily_lambdas_center[x], n = days), Hosps)) |>
    mutate(recovery_day = row_number())

  df_b_long <- pivot_longer(df_b, -recovery_day, names_to = "hospital") |>
    filter(value > 0)

  ## only if there's more than one donor per hospital on a given day
  if(any(df_b_long$value >1)){
    v2 <- filter(df_b_long, value >1)
    ## I need to change that
    # v2_f <- purrr::pmap_dfr(v2, ~tibble(hospital = rep(..2, ..3), recovery_day = ..1))
    v2_f <- purrr::pmap(v2, ~tibble(hospital = rep(..2, ..3), recovery_day = ..1)) |> purrr::list_rbind()
    v1 <- filter(df_b_long, value ==1)
    df_g <- bind_rows(v2_f, v1) |> select(hospital, recovery_day)
  }else{
    df_g <- select(df_b_long, hospital, recovery_day)
  }

  df_g <- df_g |>
    arrange(recovery_day, hospital) |>
    mutate(d_id = row_number(), .before = everything()) |>
    mutate(hospital = factor(hospital, levels = Hosps),
           male = sapply(hospital, function(x) rbinom(n = 1, size = 1, prob = sex_thetas[as.numeric(x)])),
           sex = male + 1,
           don_gender = if_else(sex == 1, "F", "M")
    )

  ov_n <- nrow(df_g)
  ov_nv <- 1:ov_n

  race_levels <- c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")

  hospital_mm <- model.matrix(~hospital, data = df_g)
##
  cod_levels <- c(1, 2, 3, 4, 999)
  cod_mm <- hospital_mm %*% cod_mat_beta[,-1]
  cod_odds <- t(apply(cod_mm, 1, function(x) exp(x)/(1+sum(exp(x)))))

  race_mm <- hospital_mm %*% race_mat_beta[,-1]
  race_odds <- t(apply(race_mm, 1, function(x) exp(x)/(1+sum(exp(x)))))
  #
  hgt_betas <- hgt_params[str_which(names(hgt_params), "betas")]
  hgt_sigma <- hgt_params[str_which(names(hgt_params), "sigma")]
  hgt_ms <- model.matrix(~don_gender, data = df_g) %*% hgt_betas

  ## Height and Race
  df_g2 <- df_g |>
    mutate(
      race_eth = factor(sapply(ov_nv, function(x) sample(race_levels, size = 1, prob = c(1 - sum(race_odds[x, ]), race_odds[x, ]))), levels = race_levels),
      hgt_in = plyr::round_any(rnorm(n = ov_n, mean = hgt_ms, sd = hgt_sigma)/2.54, 0.5),
      hgt_cm = hgt_in * 2.54,
      cod = factor(sapply(ov_nv, function(x) sample(cod_levels, size = 1, prob = c(1 - sum(cod_odds[x, ]), cod_odds[x, ]))), levels = cod_levels)
    )


  ## Blood type
  abo_levels <- c("A", "AB", "B", "O")
  abo_bm <- t(apply(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))

  ##   AGE
  age_betas <- age_params[str_which(names(age_params), "beta")]
  age_sigma <- age_params[str_which(names(age_params), "sigma")]
  age_skew <- age_params[str_which(names(age_params), "skew")]

  don_org_lvs <- c("DLU", "LUL", "LUR")

  df_g3 <- df_g2 |>
    mutate(abo =  factor(sapply(ov_nv, function(x) sample(abo_levels, size = 1, prob = c(1 - sum(abo_bm[x, ]), abo_bm[x, ]))), levels = abo_levels),
           age = floor(mapply(function(x) rsamp(sn::rsn, n = 1, limits = c(12, 80), xi = age_betas[x], omega = age_sigma[x], alpha = age_skew[x]), cod)),
           don_org = sample(don_org_lvs, size = ov_n, don_org_odds, replace = TRUE)
    )

  smoke_mm <- inv_log(model.matrix(~race_eth + don_gender + I(age/10) + I((age/10)^2) + race_eth:don_gender, data = df_g3) %*% smoke_hist_mat_beta)

  dcd_mm <- inv_log(model.matrix(~cod, data = df_g3) %*% dcd_params)

  don_org_lvs <- c("DLU", "LUL", "LUR")

  df_g4 <- df_g3 |>
    mutate(
      smoke_hist = rbinom(n = ov_n, size = 1, prob = smoke_mm),
      gt_20pkyr = ifelse(smoke_hist == 1, "Y", "N"),
      don_org = factor(sample(don_org_lvs, size = ov_n, replace = TRUE, prob = don_org_odds)),
       don_dcd = rbinom(n = ov_n, size = 1, prob = dcd_mm),
       # don_util = rbinom(n = ov_n, size = 1, prob = don_util_odds[2])
      ## 2018 to 2019 rate
      don_util = rbinom(n = ov_n, size = 1, prob = 0.9239855)
    )

  df_g00 <- df_g4 |>
    mutate(
      hospital = as.double(hospital),
      organs_avl = ifelse(don_org == "DLU", 2, 1)
      ) |>
    select(-c(sex, don_gender))

  return(df_g00)

}

#' Generate bayesian parameters and spawn donors/candidates
#'
#' @rdname gen_and_spawn
#'
#' @param desired "random" or "mean"
#' @param days number of days (must be greater than > 1)
#' @param return_params TRUE or FALSE, number of paramters, ouptut will be a list of a dataset and the parameters
#'
#' @return either a list containing synthetic donors and parameters used to generate their attritubutes or a dataset of synthetic donors
#' @export
#'
#' @examples
#' d1 <- gen_and_spawn_donors("random", days = 10)
#' c1 <- gen_and_spawn_candidates("random", days = 10)
gen_and_spawn_donors <- function(desired = "random", days, return_params = FALSE){

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

    l <- list("parameters" = gen_par,
              "data" = gen_data)
    return(l)
  }

  return(gen_data)

}

#' @rdname gen_and_spawn
#'
#' @param days number of days to simulate over
#' @param daily_lambdas_center lambda per candidate center
#' @param sex_thetas theta paramter to assign sex
#' @param race_mat_beta beta matrix for race multinomial
#' @param dx_mat_beta diangosis group
#' @param abo_mat_beta blood type
#' @param age_params age
#' @param hgt_params height
#' @param wgt_params weight
#' @param wgt_params_c weight diagnosis for Group C
#' @param diab_params diabetes
#' @param airway_params airway function
#' @param oxygen_params oxygen function
#' @param resp_mat_beta respiratory support
#' @param surg_mat_beta surgery needed
#' @param fev_params fev1
#' @param fvc_params fvc
#' @param pco2_params pco2
#' @param pf_params pf
#' @param po2_params po2
#' @param mpap_params mean pulmonary artery pressure
#' @param o2_freq_mat oxygen frequency
#' @param vent_mat_beta ventilator status
#' @param supp_o2_params supplemental oxygen
#' @param walk6m_params six minute walk distance
#' @param bili_params bilirubin
#' @param creat_params creatinine
#' @param spap_params systolic pulmonary artery pressue
#' @param ci_params cardiac index
#' @param cvp_params central venous pressure
#' @param fs_mat_beta functional status
#' @param pco2_15_params pco2 threshold
#' @param sa_params sarcoidosis Group A
#' @param sd_params sarcoidosis Group D
#' @param lam_params lam
#' @param eisen_params eisenmengers
#' @param ar_mat_beta other Group A, diseases
#' @param dr_mat_beta other Group D, disesase
#
#' @returns dataset of synthetic candidates over a set number of days
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_which
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
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

  # if(days <= 1L)stop("days must be greater than 1")
  #
  # ## to include the time component
  # # daily_lambdas_beta <- daily_lambdas_center[str_which(names(daily_lambdas_center), "beta")]
  # # daily_lambdas_center <- daily_lambdas_center[-str_which(names(daily_lambdas_center), "beta")]
  # #
  # # dyc <- floor(days/365)
  # # yrs18 <- sort(c(rep(4 + 0:(dyc-1), 365),rep(4 + dyc, days %% 365)))
  # ###
  #
  # Centers <- 1:length(daily_lambdas_center)
  # names(Centers) <- Centers
  #
  # df_b <- as.data.frame(
  #   mapply(function(x) rpois(daily_lambdas_center[x], n = days), Centers)
  #   # mapply(function(x) rpois(n = days, lambda = daily_lambdas_center[x] + daily_lambdas_beta * yrs18), Centers)
  #   ) |>
  #   mutate(listing_day = row_number())
  #
  # df_b_long <- pivot_longer(df_b, -listing_day, names_to = "center") |>
  #   filter(value > 0)
  #
  # ## only if more than one candidate per center per day
  # if(any(df_b_long$value >1)){
  #   v2 <- filter(df_b_long, value >1)
  #   ## This needs to update after dplyr 1.1.0
  #   ## list_rbind()
  #   # v2_f <- purrr::pmap_dfr(v2, ~tibble(center = rep(..2, ..3), listing_day = ..1))
  #   v2_f <- purrr::pmap(v2, ~tibble(center = rep(..2, ..3), listing_day = ..1)) |> purrr::list_rbind()
  #   v1 <- filter(df_b_long, value ==1)
  #   df_g <- bind_rows(v2_f, v1) |>  select(center, listing_day)
  # }else{
  #   df_g <- select(df_b_long, center, listing_day)
  # }
  #
  # ov_n <- nrow(df_g)
  # ov_nv <- 1:ov_n
  #
  # df_g <- df_g |>
  #   arrange(listing_day, center) |>
  #   mutate(c_id = row_number(), .before = everything()) |>
  #   mutate(center = factor(center, levels = Centers),
  #          male = sapply(center, function(x) rbinom(n = 1, size = 1, prob = sex_thetas[as.numeric(x)])),
  #          sex = male + 1,
  #          can_gender = factor(if_else(sex == 1, "F", "M"), levels = c("F", "M"))
  #   )
  # ## Interaction of sex and dx_grp
  # dx_s <- expand.grid(dx_grp = LETTERS[1:4], male = c(0, 1)) |>
  #   arrange(dx_grp) |>
  #   mutate(dx_sex_int = row_number()
  #   )
  #
  # ## Race odds
  # ## and levels
  # race_levels <- c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")
  # rm0 <- model.matrix(~center, data = df_g) %*% race_mat_beta[,-1]
  # rms <- t(apply(rm0, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # ## dx_grp needs to be separate
  # bm0 <- model.matrix(~center + male, data = df_g) %*% (dx_mat_beta)[,-1]
  # bms <- t(apply(bm0, 1, function(x) exp(x)/(1+sum(exp(x)))))
  # ## Assign Diagnosis Group
  # ## these appear to be fine
  # df_g2 <- df_g |>
  #   mutate(dx_grp = factor(sapply(1:nrow(bms), function(x)
  #     sample(LETTERS[1:4], size = 1, prob = c(1 - sum(bms[x, ]), bms[x, ]))), levels = LETTERS[1:4]),
  #     dx_num = as.numeric(dx_grp),
  #     race_eth = factor(sapply(1:nrow(rms), function(x)
  #       sample(race_levels, size = 1, prob = c(1 - sum(rms[x, ]), rms[x, ]))), levels = race_levels),
  #     ## age
  #     age_at_listing = plyr::round_any(sapply(dx_num,
  #                                             function(x) rsamp(sn::rsn, n = 1, limits = c(18, 85),
  #                                                               xi = age_params[str_which(names(age_params), "xi")][x],
  #                                                               omega = age_params[str_which(names(age_params), "omega")][x],
  #                                                               alpha = age_params[str_which(names(age_params), "alpha")][x])), (1/365)),
  #     age_at_listing = case_when(
  #       ## added this step solely because  to be able to include peds at somepoint
  #       age_at_listing <= 18 ~ 18,
  #       # age <= 0 ~ 0,
  #       TRUE ~ age_at_listing
  #     )
  #   ) |>
  #   left_join(as_tibble(dx_s), by = c("male", "dx_grp"))
  #
  # hgt_betas <- hgt_params[str_which(names(hgt_params), "betas")]
  # hgt_sigma <- hgt_params[str_which(names(hgt_params), "sigma")]
  # hgt_ms <- model.matrix(~male + dx_grp, data = df_g2) %*% hgt_betas
  #
  # abo_levels <- c("A", "AB", "B", "O")
  # abo_bm <- t(apply(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # airway_xis <- airway_params[str_which(names(airway_params), "tau")]
  # airway_sigs <- airway_params[str_which(names(airway_params), "sigmas")]
  # airway_skews <- airway_params[str_which(names(airway_params), "skews")]
  #
  # oxygen_nus <- oxygen_params[str_which(names(oxygen_params), "nu")]
  # oxygen_taus <- oxygen_params[str_which(names(oxygen_params), "tau")]
  # oxygen_beta <- oxygen_params[str_which(names(oxygen_params), "beta")]
  # oxygen_sig <- oxygen_params[str_which(names(oxygen_params), "sigma")]
  #
  # ## I might store this elsewhere
  # rtc <- function(n, df, sigma=1, mean=0){
  #   y <- rt(n=n, df = df) * sigma + mean
  #   return(y)
  # }
  #
  # df_g2b <- df_g2 |>
  #   mutate(
  #     airway = mapply(function(y,z) rsamp(sn::rsn, limits = c(-1.8, 3.3), n=1, xi = airway_xis[z], omega = airway_sigs[y], alpha = airway_skews[y]),
  #                     dx_num, dx_sex_int),
  #     oxygen = mapply(function(z, af) rsamp(rtc, limits = c(-3, 4.5), n = 1, df = oxygen_nus[z], sigma = oxygen_sig[z],
  #                                           mean = oxygen_taus[z] + oxygen_beta * af),
  #                     dx_sex_int, airway),
  #     abo =  factor(sapply(ov_nv, function(x) sample(abo_levels, size = 1, prob = c(1 - sum(abo_bm[x, ]), abo_bm[x, ]))),
  #                   levels = abo_levels),
  #
  #     ## Height will be rounded to nearest 0.5 inch and converted to centimeters
  #     hgt_in = plyr::round_any(sapply(hgt_ms, function(x) rnorm(n = 1, mean = x, sd = hgt_sigma))/2.54, .5),
  #     hgt_cm = hgt_in * 2.54
  #   )
  #
  # wgt_betas <- wgt_params[str_which(names(wgt_params), "betas")]
  # wgt_sigma <- wgt_params[str_which(names(wgt_params), "sigma")]
  #
  # wgt_betas_c <- wgt_params_c[str_which(names(wgt_params_c), "betas")]
  # wgt_sigma_c <- wgt_params_c[str_which(names(wgt_params_c), "sigma")]
  # wgt_skew_c <- wgt_params_c[str_which(names(wgt_params_c), "skew")]
  #
  # ## Splitting and recombinning
  # df_g2b_c <- filter(df_g2b, dx_grp == "C") |> mutate(dx_grp = factor(dx_grp, levels = c("C")))
  # df_g2b_mc <- filter(df_g2b, dx_grp != "C") |> mutate(dx_grp = factor(dx_grp, levels = c("A", "B", "D")))
  #
  # wgt_dm <- model.matrix(~male + dx_grp + hgt_cm, data = df_g2b_mc) %*% wgt_betas
  # df_g2b_mc <- mutate(df_g2b_mc,
  #                     wgt_kg = round(sapply(wgt_dm, function(x)
  #                       rsamp(rnorm, limits = c(20, 300), n = 1, mean = x, sd = wgt_sigma)), 2)
  # )
  #
  # if(nrow(df_g2b_c) > 0L){
  #   wgt_dm_c <- model.matrix(~male + hgt_cm, data = df_g2b_c) %*% wgt_betas_c
  #
  #   df_g3 <- df_g2b_c |>
  #     mutate(wgt_kg = round(sapply(wgt_dm_c, function(x) rsamp(sn::rsn, limits = c(20, 300),
  #                                                              n = 1, xi = x, omega = wgt_sigma_c, alpha = wgt_skew_c)), 2)) |>
  #     bind_rows(df_g2b_mc) |>
  #     arrange(c_id) |>
  #     mutate(dx_grp = factor(dx_grp, levels = LETTERS[1:4]),
  #            bmi = wgt_kg/((hgt_cm/100)^2))
  # }else{
  #
  #   df_g3 <- df_g2b_mc |>
  #     arrange(c_id) |>
  #     mutate(dx_grp = factor(dx_grp, levels = LETTERS[1:4]),
  #            bmi = wgt_kg/((hgt_cm/100)^2))
  # }
  #
  # ## Diabetes
  # diab_odds <- inv_log(model.matrix(~male + age_at_listing + race_eth + race_eth:male, data = df_g3) %*% diab_params)
  #
  # ## Respiratory Support Cluster Odds
  # rc_bm0 <- model.matrix(~male + dx_grp + factor(dx_sex_int, levels = 1:8) + age_at_listing + airway + oxygen, data = df_g3) %*% (resp_mat_beta)[,-1]
  # rc_bms <- t(apply(rc_bm0, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # int2 <- expand.grid(dx_grp = LETTERS[1:4], resp_supp = factor(1:4)) |>
  #   mutate(dx_rc = row_number())
  #
  # ## Adding suregey type
  # ## Double (D), ## Single (S) or Either (E)
  # surg_lvs <- c("D", "E", "S")
  #
  # surg_bm <- model.matrix(~dx_grp, data = df_g3) %*% surg_mat_beta[,-1]
  # surg_bms <- t(apply(surg_bm, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # df_g4 <- df_g3 |>
  #   mutate(resp_supp = as.factor(sapply(ov_nv,
  #                                       function(x) sample(1:4, size = 1, prob = c(1 - sum(rc_bms[x, ]), rc_bms[x, ])))),
  #          surg_type = as.factor(sapply(ov_nv,
  #                                       function(x) sample(surg_lvs, size = 1, prob = c(1 - sum(surg_bms[x, ]), surg_bms[x, ])))),
  #          diab = rbinom(n = ov_n, size = 1 , prob = diab_odds)
  #   ) |>
  #   left_join(int2, by = c("dx_grp", "resp_supp"))
  #
  #
  # air_mat <- model.matrix(~airway + male + dx_grp + age_at_listing + airway:dx_grp + age_at_listing:dx_grp, data = df_g4)
  # oxy_mat <- model.matrix(~oxygen + male + dx_grp + age_at_listing + oxygen:dx_grp + age_at_listing:dx_grp, data = df_g4)
  # ## PCO2 and PO2 are special
  # air_mat2 <- model.matrix(~airway + male + dx_grp + airway:dx_grp, data = df_g4)
  # oxy_mat2 <- model.matrix(~oxygen + male + dx_grp + oxygen:dx_grp, data = df_g4)
  #
  # resp_mat <- model.matrix(~male + dx_grp + resp_supp + age_at_listing + airway + oxygen, data = df_g4)
  # walk6m_mat <- model.matrix(~male + dx_grp + resp_supp + age_at_listing + airway + oxygen + dx_grp:resp_supp + age_at_listing:dx_grp, data = df_g4)
  #
  # ##airway
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
  # ## resp support
  # supp_o2_betas <- supp_o2_params[str_which(names(supp_o2_params), "beta")]
  # supp_o2_ints <- supp_o2_params[str_which(names(supp_o2_params), "c")]
  # supp_o2_thetas <- supp_o2_params[str_which(names(supp_o2_params), "theta")]
  #
  # supp_o2_mat <- apply(sapply(resp_mat %*% supp_o2_betas, function(x) inv_log(supp_o2_ints - x), simplify = "matrix"), 2, function(x) x - lag(x, 1, default = 0))
  # o2f_mat  <- t(apply(resp_mat %*% o2_freq_mat[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  # vent_mat <- t(apply(resp_mat %*% vent_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # ## levels
  # o2f_levels <- c("At Rest", "At Night",  "While Exercising", "None")
  # vent_levels <- c("BiPap", "Mechanical", "None", "CPAP")
  #
  # ## Six Minute Walk
  # walk6m_theta <- walk6m_params[str_which(names(walk6m_params), "theta")]
  # walk6m_sigma <- walk6m_params[str_which(names(walk6m_params), "sigma")]
  # walk6m_beta <- walk6m_params[str_which(names(walk6m_params), "beta")]
  # walk6m_skew <- walk6m_params[str_which(names(walk6m_params), "skew")]
  #
  # walk6m_ms <- walk6m_mat %*% walk6m_beta
  #
  # df_g5 <- df_g4 |>
  #   mutate(
  #     fev1 = round(rsamp(rnorm, limits = c(4.5, 120.5), n = ov_n, mean = air_mat %*% fev_beta_m, sd = fev_sigma), 0),
  #     fvc = round(rsamp(rnorm, limits = c(-0.5, 150), n = ov_n, mean = air_mat %*% fvc_beta_m, sd = fvc_sigma), 0),
  #     pco2 = round(1/(rsamp(rnorm, limits = c(1/160, Inf), n = ov_n, mean = air_mat2 %*% pco2_beta_m, sd = pco2_sigma)), 0),
  #     ## Oxygen
  #     pf = (rsamp(rnorm, limits = c(sqrt(30), Inf), n = ov_n, mean = oxy_mat %*% pf_beta_m, sd = pf_sigma)^2),
  #     po2 = round(1/rsamp(rnorm, limits = c(1/500, 1/30), n = ov_n, mean = oxy_mat2 %*% po2_beta_m, sd = po2_sigma), 0),
  #     ## update to min of 5 later, it was 2
  #     meanpap = round(exp(rsamp(rnorm, limits = c(log(5), log(110)), n = ov_n, mean = oxy_mat %*% mpap_beta_m, sd = mpap_sigma)) * 3, 0)/3,
  #     ## Resp Support
  #     o2 = apply(supp_o2_mat, 2, function(x) sample((0:nrow(supp_o2_mat)), size = 1, prob = c(x, 1 - sum(x))))
  #     * sapply(resp_supp, function(x) rbinom(n = 1, size = 1, prob = 1 - supp_o2_thetas[x])),
  #     o2_freq = factor(
  #       sapply(ov_nv, function(x) sample(o2f_levels, size = 1, prob = c(1 - sum(o2f_mat[x, ]), o2f_mat[x, ]))),
  #       levels = o2f_levels),
  #     vent = factor(
  #       sapply(ov_nv, function(x) sample(vent_levels, size = 1, prob = c(1 - sum(vent_mat[x, ]), vent_mat[x, ]))),
  #       levels = vent_levels),
  #     ## Extra
  #     walk6m = round(sapply(walk6m_ms, function(x) sn::rsn(n = 1, xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
  #                      sapply(dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),
  #     ##### Limit to positive scope here(?)
  #     # walk6m = round(sapply(walk6m_ms, function(x) rsamp(sn::rsn, n = 1, limits = c(0,5000), xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
  #     #                  sapply(dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),
  #
  #   ) |>
  #   mutate(walk6m = ifelse(walk6m < 0, 0, walk6m))
  #
  # ## Bilirubin
  # bili_betas <- bili_params[str_which(names(bili_params), "betas")]
  # bili_sigma <- bili_params[str_which(names(bili_params), "sigma")]
  # ## Creatinie
  # creat_betas <- creat_params[str_which(names(creat_params), "betas")]
  # creat_sigma <- creat_params[str_which(names(creat_params), "sigma")]
  # ## Systolic PAP
  # spap_betas <- spap_params[str_which(names(spap_params), "betas")]
  # spap_sigma <- spap_params[str_which(names(spap_params), "sigma")]
  # ## Cardiac Index
  # ci_betas <- ci_params[str_which(names(ci_params), "betas")]
  # ci_sigma <- ci_params[str_which(names(ci_params), "sigma")]
  # ## Central Venous Pressure
  # cvp_betas <- cvp_params[str_which(names(cvp_params), "betas")]
  # cvp_beta_mat <- matrix(cvp_betas, ncol = max(as.numeric(str_extract(names(cvp_betas), "(?<=,)\\d+")), na.rm = TRUE),
  #                        nrow = max(as.numeric(str_extract(names(cvp_betas), "\\d+(?=,)")), na.rm = TRUE))
  # cvp_alphas <- cvp_params[str_which(names(cvp_params), "alpha")]
  #
  # bili_mat <- model.matrix(~dx_grp + age_at_listing + male, data = df_g5) %*% bili_betas
  # creat_mat <- model.matrix(~dx_grp + age_at_listing + male + dx_grp:age_at_listing, data = df_g5) %*% creat_betas
  # spap_mat <- model.matrix(~meanpap + dx_grp + meanpap:dx_grp + age_at_listing*dx_grp, data = df_g5) %*% spap_betas
  # ci_mat <- model.matrix(~dx_grp + hgt_cm + wgt_kg, data = df_g5) %*% ci_betas
  # cvp_mat <- model.matrix(~meanpap, data = df_g5)
  # cvp_g <- df_g5$dx_num
  #
  # fs_levels <- c("None", "Some", "Total")
  # fs_mat <- model.matrix(~ resp_supp + dx_grp, data = df_g5)
  # fs_bm <- t(apply(fs_mat %*% fs_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  #
  # df_g5b <- df_g5 |>
  #   mutate(
  #     bili = round(exp(rnorm(n = ov_n, mean = bili_mat, sd = bili_sigma)), 2),
  #     creat = round(exp(rnorm(n = ov_n, mean = creat_mat, sd = creat_sigma)), 2),
  #     syst_pap = round(rsamp(rnorm, limits = c(4,160), n = ov_n, mean = spap_mat, sd = spap_sigma), 0),
  #     ci = round(rnorm(n = ov_n, mean = ci_mat, sd = ci_sigma)^(-2), 2),
  #     funstat = factor(sapply(ov_nv, function(x) sample(fs_levels, size = 1, prob = c(1-sum(fs_bm[x, ]), fs_bm[x, ]))), fs_levels),
  #     ecmo = ifelse(resp_supp == 4, 1, 0),
  #     cvp = round(sapply(ov_nv, function(x) rgamma(n = 1,
  #                                                  shape = cvp_mat[x,] %*% cvp_beta_mat[, cvp_g[x]],
  #                                                  rate = cvp_alphas[cvp_g[x]])), 0)
  #   )
  #
  # df_g5b_p15 <- filter(df_g5b, pco2 >= 46)
  # df_g5b_p15r <- filter(df_g5b, pco2 < 46) |> mutate(pco2_15 = 0)
  #
  # if(nrow(df_g5b_p15)>0L){
  #   pco2_15_ms <- model.matrix(~ as.factor(resp_supp) + dx_grp + pco2, data = df_g5b_p15) %*% pco2_15_params
  #
  #   df_g5b_p15 <- mutate(df_g5b_p15, pco2_15 = rbinom(n = nrow(pco2_15_ms), size = 1, prob = inv_log(pco2_15_ms)))
  #
  #   df_g6 <- bind_rows(df_g5b_p15, df_g5b_p15r) |>
  #     replace_na(list(pco2_15 = 0)) |>
  #     arrange(c_id)
  # }else{
  #   df_g6 <- df_g5b_p15r |>  arrange(c_id)
  # }
  #
  # ## Adding specific diseases
  # ## Group A
  # df_g6a <- filter(df_g6, dx_grp == "A")
  #
  # df_g6as <- filter(df_g6a, meanpap <= 30)
  #
  # ar_levels <- c("bronch" = 1, "NA" = 99)
  #
  #  if(nrow(df_g6as) > 0L){
  #   sa_odds <- inv_log(model.matrix(~center, data = df_g6as) %*% sa_params)
  #   df_g6as <- df_g6as |>
  #     mutate(sarcoidA = rbinom(n = max(dplyr::row_number()), size = 1, prob = sa_odds))
  #
  #   sarcA <- filter(df_g6as, sarcoidA == 1)
  #
  #   df_g6ar <- filter(df_g6a, !(c_id %in% sarcA$c_id))
  # }else{
  #   df_g6ar <- mutate(df_g6a, sarcoidA = 0)
  #   sarcA <- filter(df_g6ar, sarcoidA == 1)
  # }
  #
  # if(nrow(df_g6ar) > 0L){
  #   df_g6ar_f <- filter(df_g6ar, male == 0)
  #   lam_odds <- inv_log(model.matrix(~ 1, data = df_g6ar_f) %*% lam_params)
  #   df_g6ar_f <- mutate(df_g6ar_f, lymphan = rbinom(n = length(lam_odds), size = 1, prob = lam_odds))
  #   lamA <- filter(df_g6ar_f, lymphan == 1)
  #   df_g6ar2 <- filter(df_g6ar, !(c_id %in% lamA$c_id))
  # }else{
  #   df_g6ar2 <- mutate(df_g6ar, lymphan = 0)
  #   lamA <- filter(df_g6ar, lymphan == 1)
  # }
  #
  # if(nrow(df_g6ar2) > 0L){
  #
  #   ar_odds <- t(apply(model.matrix(~center, data = df_g6ar2) %*% ar_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))
  #
  #   df_g6ar2 <- df_g6ar2 |>
  #     ## if more than two diseases
  #     # mutate(ga_sd = sapply(1:nrow(ar_odds), function(x) sample(ar_levels, size = 1, prob = c(1-sum(ar_odds[x, ]), ar_odds[x, ]))))
  #     mutate(ga_sd = sapply(1:length(ar_odds), function(x) sample(ar_levels, size = 1, prob = c(1-sum(ar_odds[, x]), ar_odds[,x ]))))
  #
  #
  #   ga_sas <- sapply(ar_levels[-length(ar_levels)],  function(x) ifelse(df_g6ar2$ga_sd == x, 1, 0))
  #
  #   if(class(ga_sas)[1] == "numeric"){
  #     ga_sas <- as_tibble_row(ga_sas)
  #     colnames(ga_sas) <- gsub("\\..*", "", colnames(ga_sas))
  #   }
  #
  #   df_g6ar2 <- cbind(df_g6ar2, ga_sas)
  #
  #   ## All the A's put back together
  #   df_g6a2 <- bind_rows(sarcA, lamA, df_g6ar2)
  # }else{
  #   df_g6are <- tibble(name = names(ar_levels[-length(ar_levels)]), value = 0) |> pivot_wider()
  #   df_g6a2 <- bind_cols(sarcA, lamA, df_g6are)
  # }
  #
  # ## Group B
  # df_g6b <- filter(df_g6, dx_grp == "B")
  #
  # if(nrow(df_g6b) >0L){
  #   eisen_odds <- inv_log(model.matrix(~ 1, data = df_g6b) %*% eisen_params)
  #   df_g6b2 <- mutate(df_g6b, eisen = rbinom(n = length(eisen_odds), size = 1, prob = eisen_odds))
  # }else{
  #   df_g6b2 <- mutate(df_g6b, eisen = 0)
  # }
  #
  # ## Group D
  # df_g6d <- filter(df_g6, dx_grp == "D")
  #
  # df_g6ds <- filter(df_g6d, meanpap > 30)
  #
  # dr_levels <- c("pulm_fib" = 1, "co_bronch" = 2, "oblit" = 3, "NA" = 99)
  #
  # if(nrow(df_g6ds) >0L){
  #   sd_odds <- inv_log(model.matrix(~center, data = df_g6ds) %*% sd_params)
  #   df_g6ds <- df_g6ds |>
  #     mutate(sarcoidD = rbinom(n = max(dplyr::row_number()), size = 1, prob = sd_odds))
  #
  #   sarcD <- filter(df_g6ds, sarcoidD == 1)
  #
  #   df_g6dr <- filter(df_g6d, !(c_id %in% sarcD$c_id))
  # }else{
  #   df_g6dr <- mutate(df_g6d, sarcoidD = 0)
  #   sarcD <- filter(df_g6dr, sarcoidD == 1)
  # }
  # if(nrow(df_g6dr) > 0L){
  #
  #   dr_odds <- t(apply(model.matrix(~center, data = df_g6dr) %*% dr_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))
  #
  #   df_g6dr <- df_g6dr |>
  #     mutate(gd_sd = sapply(1:nrow(dr_odds), function(x) sample(dr_levels, size = 1, prob = c(1-sum(dr_odds[x, ]), dr_odds[x, ]))))
  #
  #   gd_sds <- sapply(dr_levels[-length(dr_levels)],  function(x) ifelse(df_g6dr$gd_sd == x, 1, 0))
  #
  #   if(class(gd_sds)[1] == "numeric"){
  #     gd_sds <- as_tibble_row(gd_sds)
  #     colnames(gd_sds) <- gsub("\\..*", "", colnames(gd_sds))
  #   }
  #
  #   df_g6dr <- cbind(df_g6dr, gd_sds)
  #
  #   ## Put the D's back together
  #   df_g6d2 <- bind_rows(sarcD, df_g6dr)
  # }else{
  #   df_g6dre <- tibble(name = names(dr_levels[-length(dr_levels)]), value = 0) |> pivot_wider()
  #   df_g6d2 <- bind_cols(sarcD, df_g6dre)
  # }
  #
  # ## Put it all back togehter
  # df_g7 <- filter(df_g6, dx_grp == "C") |>
  #   bind_rows(df_g6a2, df_g6d2, df_g6b2) |>
  #   arrange(c_id)
  #
  # df_g7[is.na(df_g7)] <- 0
  #
  # ## PRA and PLD
  # ## Creatine and Bilirubin thresholds
  # df_g8 <- mutate(df_g7, pra = 0, pld = 0, creat_150 = 0, bili_50 = 0)
  if(days <= 1L)stop("days must be greater than 1")

  Centers <- 1:max(as.numeric(str_extract(names(daily_lambdas_center), "\\d+")))
  names(Centers) <- Centers

  df_b <- sapply(daily_lambdas_center, function(x) rpois(n = days, lambda = x)) |>
    as.data.frame() |>
    mutate(listing_day = row_number()) |>
    pivot_longer(-listing_day, names_sep = ",", names_to = c("center", "dx_num"),
                 names_transform = list("center" = ~as.numeric(str_extract(.x, "\\d+")),
                                        "dx_num" = ~as.numeric(str_extract(.x, "\\d+")))) |>
    filter(value > 0)

  # df_b_long <- pivot_longer(df_b, -day) |>
  #   filter(value > 0)

  if(any(df_b$value > 1)){
    v2 <- filter(df_b, value > 1)

    v2_f <- purrr::pmap(v2, ~tibble(center = rep(..2, ..4), listing_day = ..1, dx_num = ..3)) |>
      purrr::list_rbind()
    v1 <- filter(df_b, value == 1)
    df_g <- bind_rows(v2_f, v1) |>
      select(center, listing_day, dx_num)
  }else{
    df_g <- select(df_b, center, listing_day, dx_num)
  }

  # return(df_g)

  ov_n <- nrow(df_g)
  ov_nv <- 1:ov_n

  df_g <- df_g |>
    arrange(listing_day, center) |>
    mutate(c_id = row_number(), .before = everything()) |>
    mutate(center = factor(center, levels = Centers),
           male = sapply(dx_num, function(x) rbinom(n = 1, size = 1, prob = sex_thetas[as.numeric(x)])),
           sex = male + 1,
           can_gender = factor(if_else(sex == 1, "F", "M"), levels = c("F", "M")),
           dx_grp = factor(LETTERS[dx_num], LETTERS[1:4])
    )
  ## Interaction of sex and dx_grp
  dx_s <- expand.grid(dx_grp = LETTERS[1:4], can_gender = c("F", "M")) |>
    arrange(dx_grp) |>
    mutate(dx_sex_int = row_number()
    )

  ## Race odds
  ## and levels
  race_levels <- c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")
  race_odds <- calc_multi_odds(model.matrix(~center, data = df_g) %*% race_mat_beta[,-1])
  # return(race_odds)
  # rm0 <- model.matrix(~center, data = df_g) %*% race_mat_beta[,-1]
  # rms <- t(apply(rm0, 1, function(x) exp(x)/(1 + sum(exp(x)))))

  ## dx_grp needs to be separate
  # bm0 <- model.matrix(~center + male, data = df_g) %*% (dx_mat_beta)[,-1]
  # bms <- t(apply(bm0, 1, function(x) exp(x)/(1+sum(exp(x)))))
  ## Assign Diagnosis Group
  ## these appear to be fine

  df_g2 <- df_g |>
    mutate(
      race_eth = factor(sapply(ov_nv,
                               function(x) sample(race_levels, size = 1, prob = race_odds[x, ])), levels = race_levels),
      ## age
      age_at_listing = plyr::round_any(
        sapply(dx_num,
               function(x) rsamp(sn::rsn, n = 1, limits = c(18, 85),
                                 xi = age_params[str_which(names(age_params), "xi")][x],
                                 omega = age_params[str_which(names(age_params), "omega")][x],
                                 alpha = age_params[str_which(names(age_params), "alpha")][x])), (1/365)),
      # age_at_listing = case_when(
      #   ## added this step solely because I want to be able to include peds at somepoint
      #   age_at_listing <= 18 ~ 18,
      #   # age <= 0 ~ 0,
      #   TRUE ~ age_at_listing
      # )
    ) |>
    left_join(as_tibble(dx_s), by = c("can_gender", "dx_grp"))

  hgt_betas <- hgt_params[str_which(names(hgt_params), "betas")]
  hgt_sigma <- hgt_params[str_which(names(hgt_params), "sigma")]
  hgt_ms <- model.matrix(~can_gender + dx_grp, data = df_g2) %*% hgt_betas

  abo_levels <- c("A", "AB", "B", "O")
  # abo_bm <- t(apply(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  abo_odds <- calc_multi_odds(model.matrix(~race_eth, data = df_g2) %*% abo_mat_beta[,-1])

  airway_xis <- airway_params[str_which(names(airway_params), "tau")]
  airway_sigs <- airway_params[str_which(names(airway_params), "sigmas")]
  airway_skews <- airway_params[str_which(names(airway_params), "skews")]

  oxygen_nus <- oxygen_params[str_which(names(oxygen_params), "nu")]
  oxygen_taus <- oxygen_params[str_which(names(oxygen_params), "tau")]
  oxygen_beta <- oxygen_params[str_which(names(oxygen_params), "beta")]
  oxygen_sig <- oxygen_params[str_which(names(oxygen_params), "sigma")]

  ## I might store this elsewhere
  rtc <- function(n, df, sigma=1, mean=0){
    y <- rt(n=n, df = df) * sigma + mean
    return(y)
  }

  df_g2b <- df_g2 |>
    mutate(
      airway = mapply(function(y, z) rsamp(sn::rsn, limits = c(-1.8, 3.3), n = 1, xi = airway_xis[z],
                                           omega = airway_sigs[y], alpha = airway_skews[y]), dx_num, dx_sex_int),
      oxygen = mapply(function(z, af) rsamp(rtc, limits = c(-2.6, 4.5), n = 1, df = oxygen_nus[z],
                                            sigma = oxygen_sig[z], mean = oxygen_taus[z] + oxygen_beta * af), dx_sex_int, airway),
      abo =  factor(sapply(ov_nv, function(x) sample(abo_levels, size = 1, prob = abo_odds[x, ])),
                    levels = abo_levels),

      ## Height will be rounded to nearest 0.5 inch and converted to centimeters
      hgt_in = plyr::round_any(sapply(hgt_ms, function(x) rnorm(n = 1, mean = x, sd = hgt_sigma))/2.54, .5),
      hgt_cm = hgt_in * 2.54
    )

  wgt_betas <- wgt_params[str_which(names(wgt_params), "betas")]
  wgt_sigma <- wgt_params[str_which(names(wgt_params), "sigma")]

  wgt_betas_c <- wgt_params_c[str_which(names(wgt_params_c), "betas")]
  wgt_sigma_c <- wgt_params_c[str_which(names(wgt_params_c), "sigma")]
  wgt_skew_c <- wgt_params_c[str_which(names(wgt_params_c), "skew")]

  ## Splitting and recombinning
  df_g2b_c <- filter(df_g2b, dx_grp == "C") |>  mutate(dx_grp = factor(dx_grp, levels = c("C")))
  df_g2b_mc <- filter(df_g2b, dx_grp != "C") |>  mutate(dx_grp = factor(dx_grp, levels = c("A", "B", "D")))

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
      arrange(c_id) |>
      mutate(dx_grp = factor(dx_grp, levels = LETTERS[1:4]),
             bmi = wgt_kg/((hgt_cm/100)^2))
  }else{

    df_g3 <- df_g2b_mc |>
      arrange(c_id) |>
      mutate(dx_grp = factor(dx_grp, levels = LETTERS[1:4]),
             bmi = wgt_kg/((hgt_cm/100)^2))
  }

  ## Diabmetes
  diab_odds <- inv_log(model.matrix(~male + age_at_listing + race_eth + race_eth:male, data = df_g3) %*% diab_params)


  ## Respiratory Support Cluster Odds
  # rc_bm0 <- model.matrix(~male + dx_grp + factor(dx_sex_int, levels = 1:8) + age_at_listing + airway + oxygen, data = df_g3) %*% (resp_mat_beta)[,-1]
  # rc_bms <- t(apply(rc_bm0, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  rc_odds <- calc_multi_odds(model.matrix(~male + dx_grp + factor(dx_sex_int, levels = 1:8) + age_at_listing + airway + oxygen, data = df_g3) %*% (resp_mat_beta)[,-1])

  int2 <- expand.grid(dx_grp = LETTERS[1:4], resp_supp = factor(1:4)) |>
    mutate(dx_rc = row_number())

  ## Adding suregey type
  ## Double (D), ## Single (S) or Either (E)
  surg_lvs <- c("D", "E", "S")

  # surg_bm <- model.matrix(~dx_grp, data = df_g3) %*% surg_mat_beta[,-1]
  # surg_bms <- t(apply(surg_bm, 1, function(x) exp(x)/(1 + sum(exp(x)))))
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
  fev_beta_m <- fev_params[str_which(names(fev_params), "betas")]
  fev_sigma <- fev_params[str_which(names(fev_params), "sigma")]
  fvc_beta_m <- fvc_params[str_which(names(fvc_params), "betas")]
  fvc_sigma <- fvc_params[str_which(names(fvc_params), "sigma")]
  pco2_beta_m <- pco2_params[str_which(names(pco2_params), "betas")]
  pco2_sigma <- pco2_params[str_which(names(pco2_params), "sigma")]
  ## oxygen
  pf_beta_m <- pf_params[str_which(names(pf_params), "betas")]
  pf_sigma <- pf_params[str_which(names(pf_params), "sigma")]
  po2_beta_m <- po2_params[str_which(names(po2_params), "betas")]
  po2_sigma <- po2_params[str_which(names(po2_params), "sigma")]
  mpap_beta_m <- mpap_params[str_which(names(mpap_params), "betas")]
  mpap_sigma <- mpap_params[str_which(names(mpap_params), "sigma")]


  # supp_o2_mat <- apply(sapply(resp_mat %*% supp_o2_betas, function(x) inv_log(supp_o2_ints - x), simplify = "matrix"), 2, function(x) x - lag(x, 1, default = 0))

  # o2f_mat  <- t(apply(resp_mat %*% o2_freq_mat[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  # vent_mat <- t(apply(resp_mat %*% vent_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))

  # supp_o2_odds <- calc_multi_odds(supp_o2_mat)
  o2f_odds <- calc_multi_odds(resp_mat %*% o2_freq_mat[,-1])
  vent_odds <- calc_multi_odds(resp_mat %*% vent_mat_beta[,-1])


  ## levels
  o2f_levels <- c("At Rest", "At Night",  "While Exercising", "None")
  vent_levels <- c("BiPap", "Mechanical", "None", "CPAP")

  ## Six Minute Walk
  walk6m_theta <- walk6m_params[str_which(names(walk6m_params), "theta")]
  walk6m_sigma <- walk6m_params[str_which(names(walk6m_params), "sigma")]
  walk6m_beta <- walk6m_params[str_which(names(walk6m_params), "beta")]
  walk6m_skew <- walk6m_params[str_which(names(walk6m_params), "skew")]

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
      ## Resp Support
      # o2 = apply(supp_o2_mat, 2, function(x) sample(0:nrow(supp_o2_mat), size = 1, prob = c(x, 1 - sum(x))))
      # * sapply(resp_supp, function(x) rbinom(n = 1, size = 1, prob = 1 - supp_o2_thetas[x])),
      o2_freq = factor(
        sapply(ov_nv, function(x) sample(o2f_levels, size = 1, prob = o2f_odds[x, ])),
        levels = o2f_levels),
      vent = factor(
        sapply(ov_nv, function(x) sample(vent_levels, size = 1, prob = vent_odds[x, ])),
        levels = vent_levels),
      ## Extra
      walk6m = round(sapply(walk6m_ms, function(x) sn::rsn(n = 1, xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
                       sapply(dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),
      ##### Limit to positive scope here(?)
      # walk6m = round(sapply(walk6m_ms, function(x) rsamp(sn::rsn, n = 1, limits = c(0,5000), xi = x, omega = walk6m_sigma, alpha = walk6m_skew)) *
      #                  sapply(dx_rc, function(x) rbinom(n = 1, size = 1, prob = 1 - walk6m_theta[x])), 0),

    ) |>
    mutate(walk6m = ifelse(walk6m < 0, 0, walk6m))

  ## resp support
  supp_o2_betas <- supp_o2_params[str_which(names(supp_o2_params), "beta")]
  supp_o2_ints <- supp_o2_params[str_which(names(supp_o2_params), "cs")]
  # supp_o2_thetas <- supp_o2_params[str_which(names(supp_o2_params), "theta")]

  # resp_mat2 <- model.matrix(~can_gender + dx_grp + resp_supp + age_at_listing + airway + oxygen + o2_freq, data = df_g5)
  # supp_o2_mat <- apply(sapply(resp_mat2 %*% supp_o2_betas, function(x) inv_log(x - supp_o2_ints), simplify = "matrix")
  #                      , 2, function(x) lag(x, 1, default = 1) - x)
  df_g5_dx_num <- df_g5$dx_num
  df5_supp_o2_ints <- lapply(df_g5_dx_num, function(x) supp_o2_ints[grepl(paste0(",", x, "\\]"), names(supp_o2_ints))])
  resp_mat2 <- model.matrix(~can_gender + resp_supp + age_at_listing + airway + oxygen + o2_freq, data = df_g5)
  supp_o2_mat <- apply(mapply(function(x, y) inv_log(x - y), resp_mat2 %*% supp_o2_betas, df5_supp_o2_ints, SIMPLIFY = TRUE)
                       , 2, function(x) lag(x, 1, default = 1) - x)

  # ## Bilirubin
  bili_betas <- bili_params[str_which(names(bili_params), "betas")]
  bili_sigma <- bili_params[str_which(names(bili_params), "sigma")]
  ## Creatinie
  creat_betas <- creat_params[str_which(names(creat_params), "betas")]
  creat_sigma <- creat_params[str_which(names(creat_params), "sigma")]
  ## Systolic PAP
  spap_betas <- spap_params[str_which(names(spap_params), "betas")]
  spap_sigma <- spap_params[str_which(names(spap_params), "sigma")]
  spap_sigmas <- rep(spap_sigma[1], ov_n)
  spap_sigmas[which(df_g5$dx_grp == "B")] <- spap_sigma[2]
  ## Cardiac Index
  ci_betas <- ci_params[str_which(names(ci_params), "betas")]
  ci_sigma <- ci_params[str_which(names(ci_params), "sigma")]
  ## Central Venous Pressure
  cvp_betas <- cvp_params[str_which(names(cvp_params), "betas")]
  cvp_beta_mat <- matrix(cvp_betas, ncol = max(as.numeric(str_extract(names(cvp_betas), "(?<=,)\\d+")), na.rm = TRUE),
                         nrow = max(as.numeric(str_extract(names(cvp_betas), "\\d+(?=,)")), na.rm = TRUE))
  cvp_alphas <- cvp_params[str_which(names(cvp_params), "alpha")]

  ##
  bili_mat <- model.matrix(~dx_grp + age_at_listing + can_gender, data = df_g5) %*% bili_betas
  creat_mat <- model.matrix(~dx_grp + age_at_listing + can_gender + dx_grp:age_at_listing, data = df_g5) %*% creat_betas
  spap_mat <- model.matrix(~meanpap + dx_grp + meanpap:dx_grp + age_at_listing*dx_grp, data = df_g5) %*% spap_betas
  ci_mat <- model.matrix(~dx_grp + hgt_cm + wgt_kg, data = df_g5) %*% ci_betas
  ci_sigmas <- ifelse(df_g5$dx_grp == "B", ci_sigma[2], ci_sigma[1])
  cvp_mat <- model.matrix(~meanpap, data = df_g5)
  cvp_g <- dplyr::pull(df_g5, dx_num)

  fs_levels <- c("None", "Some", "Total")
  # fs_mat <- model.matrix(~ resp_supp + dx_grp, data = df_g5)
  # fs_bm <- t(apply(fs_mat %*% fs_mat_beta[,-1], 1, function(x) exp(x)/(1 + sum(exp(x)))))
  fs_odds <- calc_multi_odds(model.matrix(~ resp_supp + dx_grp, data = df_g5) %*% fs_mat_beta[,-1])

  df_g5b <- df_g5 |>
    mutate(
      o2 = apply(supp_o2_mat, 2, function(x) sample(0:nrow(supp_o2_mat), size = 1, prob = c(x, 1 - sum(x)))),
      # * sapply(resp_supp, function(x) rbinom(n = 1, size = 1, prob = 1 - supp_o2_thetas[x])),
      bili = round(exp(rnorm(n = ov_n, mean = bili_mat, sd = bili_sigma)), 2),
      creat = round(exp(rnorm(n = ov_n, mean = creat_mat, sd = creat_sigma)), 2),
      syst_pap = round(rsamp(rnorm, limits = c(4, 170), n = ov_n, mean = spap_mat, sd = spap_sigmas), 0),
      ci = round(exp(rnorm(n = ov_n, mean = ci_mat, sd = ci_sigmas)), 2),
      # ci = round(rnorm(n = ov_n, mean = ci_mat, sd = ci_sigma)^(-2), 2),
      funstat = factor(sapply(ov_nv, function(x) sample(fs_levels, size = 1, prob = fs_odds[x, ])),
                       fs_levels),
      ecmo = ifelse(resp_supp == 4, 1, 0),
      cvp = round(sapply(ov_nv, function(x) rgamma(n = 1,
                                                   shape = cvp_mat[x,] %*% cvp_beta_mat[, cvp_g[x]],
                                                   rate = cvp_alphas[cvp_g[x]])), 0)
    )

  # return(df_g5b)

  df_g5b_p15 <- filter(df_g5b, pco2 >= 46)
  df_g5b_p15r <- filter(df_g5b, pco2 < 46) |>  mutate(pco2_15 = 0)

  if(nrow(df_g5b_p15)>0L){
    pco2_15_ms <- model.matrix(~ as.factor(resp_supp) + dx_grp + pco2, data = df_g5b_p15) %*% pco2_15_params

    df_g5b_p15 <- mutate(df_g5b_p15, pco2_15 = rbinom(n = nrow(pco2_15_ms), size = 1, prob = inv_log(pco2_15_ms)))

    df_g6 <- bind_rows(df_g5b_p15, df_g5b_p15r) |>
      replace_na(list(pco2_15 = 0)) |>
      arrange(c_id)
  }else{
    df_g6 <- df_g5b_p15r |>  arrange(c_id)
  }

  ## Adding specific diseases
  ## Group A
  df_g6a <- filter(df_g6, dx_grp == "A")

  df_g6as <- filter(df_g6a, meanpap <= 30)

  ar_levels <- c("bronch" = 1, "NA" = 99)

  if(nrow(df_g6as) > 0L){
    sa_odds <- inv_log(model.matrix(~center, data = df_g6as) %*% sa_params)
    df_g6as <- df_g6as |>
      mutate(sarcoidA = rbinom(n = max(dplyr::row_number()), size = 1, prob = sa_odds))

    sarcA <- filter(df_g6as, sarcoidA == 1)

    df_g6ar <- filter(df_g6a, !(c_id %in% sarcA$c_id))
  }else{
    df_g6ar <- mutate(df_g6a, sarcoidA = 0)
    sarcA <- filter(df_g6ar, sarcoidA == 1)
  }

  if(nrow(df_g6ar) > 0L){
    df_g6ar_f <- filter(df_g6ar, male == 0)
    lam_odds <- inv_log(model.matrix(~ 1, data = df_g6ar_f) %*% lam_params)
    # df_g6ar_f <- mutate(df_g6ar_f, lymphan = sapply(lam_odds, function(x) rbinom(n = 1, size = 1, prob = x)))
    df_g6ar_f <- mutate(df_g6ar_f, lymphan = rbinom(n = length(lam_odds), size = 1, prob = lam_odds))
    lamA <- filter(df_g6ar_f, lymphan == 1)
    df_g6ar2 <- filter(df_g6ar, !(c_id %in% lamA$c_id))
  }else{
    df_g6ar2 <- mutate(df_g6ar, lymphan = 0)
    lamA <- filter(df_g6ar, lymphan == 1)
  }

  if(nrow(df_g6ar2) > 0L){

    # ar_odds <- t(apply(model.matrix(~center, data = df_g6ar2) %*% ar_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))

    ar_odds <- calc_binom_odds(model.matrix(~center, data = df_g6ar2) %*% ar_mat_beta[,-1])

    # return(ar_odds)

    df_g6ar2 <- df_g6ar2 |>
      ## if more than two diseases
      mutate(ga_sd = sapply(1:nrow(ar_odds), function(x) sample(ar_levels, size = 1, prob = ar_odds[x, ])))
    # mutate(ga_sd = sapply(1:length(ar_odds), function(x) sample(ar_levels, size = 1, prob = c(1-sum(ar_odds[,x ]), ar_odds[,x ]))))


    ga_sas <- sapply(ar_levels[-length(ar_levels)],  function(x) ifelse(df_g6ar2$ga_sd == x, 1, 0))

    if(class(ga_sas)[1] == "numeric"){
      ga_sas <- as_tibble_row(ga_sas)
      colnames(ga_sas) <- gsub("\\..*", "", colnames(ga_sas))
    }

    df_g6ar2 <- cbind(df_g6ar2, ga_sas)

    ## All the A's put back together
    df_g6a2 <- bind_rows(sarcA, lamA, df_g6ar2)
  }else{
    df_g6are <- tibble(name = names(ar_levels[-length(ar_levels)]), value = 0) |>  pivot_wider()
    df_g6a2 <- bind_cols(sarcA, lamA, df_g6are)
  }

  ## Group B
  df_g6b <- filter(df_g6, dx_grp == "B")

  if(nrow(df_g6b) >0L){
    eisen_odds <- inv_log(model.matrix(~ 1, data = df_g6b) %*% eisen_params)
    # df_g6b2 <- mutate(df_g6b, eisen = sapply(eisen_odds, function(x) rbinom(n = 1, size = 1, prob = x)))
    df_g6b2 <- mutate(df_g6b, eisen = rbinom(n = length(eisen_odds), size = 1, prob = eisen_odds))
  }else{
    df_g6b2 <- mutate(df_g6b, eisen = 0)
  }

  ## Group D
  df_g6d <- filter(df_g6, dx_grp == "D")

  df_g6ds <- filter(df_g6d, meanpap > 30)

  dr_levels <- c("pulm_fib" = 1, "co_bronch" = 2, "oblit" = 3, "NA" = 99)

  if(nrow(df_g6ds) >0L){
    sd_odds <- inv_log(model.matrix(~center, data = df_g6ds) %*% sd_params)
    df_g6ds <- df_g6ds |>
      # mutate(sarcoidD = sapply(sd_odds, function(x) rbinom(n = 1, size = 1, prob = x)))
      mutate(sarcoidD = rbinom(n = max(dplyr::row_number()), size = 1, prob = sd_odds))

    sarcD <- filter(df_g6ds, sarcoidD == 1)

    df_g6dr <- filter(df_g6d, !(c_id %in% sarcD$c_id))
  }else{
    df_g6dr <- mutate(df_g6d, sarcoidD = 0)
    sarcD <- filter(df_g6dr, sarcoidD == 1)
  }
  if(nrow(df_g6dr) > 0L){

    # dr_odds <- t(apply(model.matrix(~center, data = df_g6dr) %*% dr_mat_beta[,-1], 1, function(x) exp(x)/(1+sum(exp(x)))))

    dr_odds <- calc_multi_odds(model.matrix(~center, data = df_g6dr) %*% dr_mat_beta[,-1])

    df_g6dr <- df_g6dr |>
      mutate(gd_sd = sapply(1:nrow(dr_odds), function(x) sample(dr_levels, size = 1, prob = dr_odds[x, ])))

    gd_sds <- sapply(dr_levels[-length(dr_levels)],  function(x) ifelse(df_g6dr$gd_sd == x, 1, 0))

    if(class(gd_sds)[1] == "numeric"){
      gd_sds <- as_tibble_row(gd_sds)
      colnames(gd_sds) <- gsub("\\..*", "", colnames(gd_sds))
    }

    df_g6dr <- cbind(df_g6dr, gd_sds)

    ## Put the D's back together
    df_g6d2 <- bind_rows(sarcD, df_g6dr)
  }else{
    df_g6dre <- tibble(name = names(dr_levels[-length(dr_levels)]), value = 0) |>  pivot_wider()
    df_g6d2 <- bind_cols(sarcD, df_g6dre)
  }

  ## Put it all back togehter
  df_g7 <- filter(df_g6, dx_grp == "C") |>
    bind_rows(df_g6a2, df_g6d2, df_g6b2) |>
    arrange(c_id)

  df_g7[is.na(df_g7)] <- 0

  ## PRA and PLD
  ## Creatine and Bilirubin thresholds
  df_g8 <- mutate(df_g7, pra = 0, pld = 0, creat_150 = 0, bili_50 = 0)


  ##
  safe_log <- function(x){
    l0x <- which(x<=0)
    if(length(l0x)>0L) x[l0x] <- NA
    return(log(x))
  }

  ## Hyptothetical Removal Date
  # df_g9 <- tidyr::nest(df_g8, .by = dx_grp) |>
  #   left_join(remov_df2, by = join_by(dx_grp)) |>
  #   ## Under predict
  #   # mutate(remov = mapply(function(x, y) y(runif(n = nrow(x))), data, remov_f)) |>
  #   # select(-remov_f) |>
  #   ## over predict
  #   # mutate(remov = mapply(function(x,y,z) round((-log(runif(n = nrow(x)))/(1/y))^z), data, shapes, scales)) |>
  #   # select(-c(shapes, scales)) |>
  #   ## gompertz
  #   # mutate(remov = mapply(function(x, y, z) round(safe_log(1-(z*log(runif(nrow(x)))/y))*(1/z)), data, shapes, rates)) |>
  #   # mutate(remov = mapply(function(x, y, z, a) round(log(1-(z*log(runif(nrow(x)))/(y*a)))*(1/z)), data, shapes, rates, exp_c)) |>
  #   # select(-c(shapes, rates, exp_c)) |>
  #   mutate(remov = mapply(function(x, y, z) ceiling(rweibull(nrow(x), shape = y, scale = z)), data, shapes, scales)) |>
  #   select(-c(shapes, scales)) |>
  #   ##
  #   tidyr::unnest(cols = c(data, remov)) |>
  #   mutate(hy_removal_day = if_else(is.na(remov), Inf, round(remov))) |>
  #   # mutate(un_cens = rbinom(n = ov_n, size = 1, prob = 0.456915108603157),
  #   #        hy_removal_day = if_else(un_cens==1, Inf, round(remov))) |>
  #   arrange(c_id)

  ## Remove and renaming columns, collapsing variables
  df_g00 <- df_g8 |>
    mutate(age = age_at_listing,
           hgt_cm1 = round(hgt_cm, 0),
           ## max height for unos
           # hgt_cm1 = ifelse(hgt_cm1 > 223, 223, ifelse(hgt_cm1 < 63, 63, hgt_cm1)),
           o2rest = ifelse(o2_freq == "At Rest", o2, 0),
           cont_mech = ifelse(vent == "Mechanical", 1,0),
           funstat = as.numeric(funstat),
           o2_freq = as.numeric(o2_freq),
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
    select(-c(dx_rc, dx_num, dx_sex_int, can_gender, sex, ga_sd, gd_sd, sarcoidA, sarcoidD, bronch, lymphan, pulm_fib, co_bronch, oblit, eisen)) |>
    rename(pap_syst = syst_pap, pap_mean = meanpap) |>
    relocate(dx_grp, .after = male) |>
    left_join(pABOs, by = "abo") |>
    left_join(pHGTs, by = c("hgt_cm1"= "cand_hgt", "dx_grp")) |>
    left_join(pCPRAs, by= c("pra" = "CPRA"))

  return(df_g00)

}

#' @rdname gen_and_spawn
#'
#' @export
gen_and_spawn_candidates <- function(desired = "random", days, return_params = FALSE){

  if(days <= 1L)stop('days must be greater than 1')
  if(!(desired %in% c("mean", "random")))stop('desired must be "random" or "mean"')
  if(!is.logical(return_params))stop('return_params must be "TRUE" or "FALSE"')

  gen_count <- generate_params("can_count", desired = desired)
  gen_sex <- generate_params("can_sex", desired = desired)
  gen_age <- generate_params("can_age", desired = desired)
  gen_hgt <- generate_params("can_hgt", desired = desired)
  gen_wgt <- generate_params("can_wgt", desired = desired)
  gen_wgt_c <- generate_params("can_wgt_c", desired = desired)
  # gen_dx <- generate_params("can_dx_grp", desired = desired)
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
                               race_mat_beta = gen_race, #dx_mat_beta = gen_dx,
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
               # "can_dx" = gen_dx,
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



#' Generate Paramters for candidate/donor characterist
#'
#' @param characteristic a string specied with "can_" or "don_" and the thecharacteris see details for list of options
#' @param desired ## either "random" or "mean", a random set of parameters based on a multivariate normal distribution or sample means
#'
#' @return a set of parameters
#'
#' @importFrom stringr str_which
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr pmap_chr
generate_params <- function(characteristic, desired = "random"){

  ## Logic to identify which parameter
  # char_sel <- characteristic
  # l1 <- all_params_dist[[char_sel]]
  #
  # d_o <- c("means","random")
  #
  # if(tolower(desired) == "random"){
  #
  #   params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
  #
  # }else{
  #   params <- l1[[1]]
  # }
  #
  # if(any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
  #   while(any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) | any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
  #     params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
  #     params_x2 <- params_x[,str_which(names(params), "^theta\\[\\d+\\]$")]
  #     # params2 <- which(map_lgl(1:nrow(params_x), ~(sum(params_x2[.x,]<0) + sum(params_x2[.x,]>1))==0))
  #     params2 <- which(sapply(1:nrow(params_x), function(x) (sum(params_x2[x,]<0) + sum(params_x2[x,]>1))==0))
  #     if(length(params2)==0L){params <- params}else{
  #       params <- params_x[min(params2),]
  #     }
  #
  #   }
  # }
  #
  # if(any(str_detect(names(params), "^c\\[\\d+\\]$"))){
  #   while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")])){
  #     params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
  #   }
  # }
  #
  # if(any(str_detect(names(params), "^c\\[\\d+\\]$")) & any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
  #   while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")]) |
  #         any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) |
  #         any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
  #     params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
  #   }
  # }
  #
  # if(any(str_detect(names(params), "^lambda\\[\\d+\\]$"))){
  #   while(any(params < 0)){
  #     params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
  #   }
  # }
  #
  # ## These a specfic
  # ## because some of the donor hosptials are quite small so the race and sex params need to set to more than one hospital
  #
  # if(char_sel == "don_sex"){
  #   small_param <- params[length(params)]
  #   small_params <- rep(small_param, length(small_ids))
  #   names(small_params) <- paste0("theta[", small_ids, "]")
  #   ot_params <- params[-length(params)]
  #   names(ot_params) <- paste0("theta[", ot_ids, "]")
  #   params <- c(ot_params, small_params)
  #   params <- params[order(factor(names(params), levels = paste0("theta[", all_ids, "]")))]
  #
  # }
  # if(char_sel == "don_count"){
  #   small_param <- params[length(params)]
  #   small_params <- rep(small_param, length(small_ids))/length(small_ids)
  #   names(small_params) <- paste0("lambda[", small_ids, "]")
  #   ot_params <- params[-length(params)]
  #   names(ot_params) <- paste0("lambda[", ot_ids, "]")
  #   params <- c(ot_params, small_params)
  #   params <- params[order(factor(names(params), levels = paste0("lambda[", all_ids, "]")))]
  # }
  # if(char_sel %in% c("don_cod","don_race")){
  #   small_param <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"))]
  #   zeros <- rep(small_param, length(small_ids))
  #   names(zeros) <- pmap_chr(expand.grid("betas[", small_ids, ",", 1:length(small_param),"]") |>
  #                              arrange(Var2, Var4), paste0)
  #
  #   ot_params <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"), negate = TRUE)]
  #   names(ot_params) <- pmap_chr(expand.grid("betas[", ot_ids, ",", 1:length(small_param),"]"), paste0)
  #
  #   params <- c(ot_params, zeros)
  #   params <- params[order(factor(names(params), levels = pmap_chr(expand.grid("betas[", all_ids, ",", 1:length(small_param),"]"), paste0)))]
  #
  # }
  #
  # ## central venous pressue is different structure
  # if(char_sel == "can_cvp"){
  #   return(params)
  # }
  #
  #
  # if(any(str_detect(names(params), ","))){
  #   params <- matrix(params, ncol = max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE),
  #                    nrow = max(as.numeric(str_extract(names(params), "\\d+(?=,)")), na.rm = TRUE))
  # }
  #
  # return(params)

  char_sel <- characteristic
  l1 <- all_params_dist[[char_sel]]

  d_o <- c("means","random")

  if(tolower(desired) == "random"){

    params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])

  }else{
    params <- l1[[1]]
  }

  # return(params)

  if(any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
    while(any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) | any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
      # params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
      params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
      params_x2 <- params_x[,str_which(names(params), "^theta\\[\\d+\\]$")]
      params2 <- which(map_lgl(1:nrow(params_x), ~(sum(params_x2[.x,]<0) + sum(params_x2[.x,]>1))==0))
      if(length(params2)==0L){params <- params}else{
        params <- params_x[min(params2),]
      }


    }
  }

  if(any(str_detect(names(params), "^c\\[\\d+\\]$"))){
    while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")])){
      params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
    }
  }

  if(any(str_detect(names(params), "^c\\[\\d+\\]$")) & any(str_detect(names(params), "^theta\\[\\d+\\]$"))){
    while(is.unsorted(params[str_which(names(params), "^c\\[\\d+\\]$")]) |
          any(params[str_which(names(params), "^theta\\[\\d+\\]$")]<0) |
          any(params[str_which(names(params), "^theta\\[\\d+\\]$")]>1)){
      params <- MASS::mvrnorm(n = 1, mu = l1[[1]], Sigma = l1[[2]])
    }
  }
  if(any(str_detect(names(params), "^cs\\[\\d+,\\d+]$"))){
    # ncols = max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE)
    c_pars <- str_detect(names(params), "^cs\\[\\d+,\\d+]$")
    ncols <- max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE)
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
    # small_param <- params[str_which(names(params), "betas\\[1,\\d+\\]")]
    small_param <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"))]
    zeros <- rep(small_param, length(small_ids))
    names(zeros) <- pmap_chr(expand.grid("betas[", small_ids, ",", 1:length(small_param),"]") |>
                               arrange(Var2, Var4), paste0)

    ot_params <- params[str_which(names(params), paste0("betas\\[", mx_id,",\\d+\\]"), negate = TRUE)]
    names(ot_params) <- pmap_chr(expand.grid("betas[", ot_ids, ",", 1:length(small_param),"]"), paste0)

    params <- c(ot_params, zeros)
    params <- params[order(factor(names(params), levels = pmap_chr(expand.grid("betas[", all_ids, ",", 1:length(small_param),"]"), paste0)))]

  }

  ## central venous pressue is different structure
  if(char_sel %in% c("can_cvp", "can_count", "can_suppo2")){
    return(params)
  }


  if(any(str_detect(names(params), ","))){
    params <- matrix(params, ncol = max(as.numeric(str_extract(names(params), "(?<=,)\\d+")), na.rm = TRUE),
                     nrow = max(as.numeric(str_extract(names(params), "\\d+(?=,)")), na.rm = TRUE))
  }

  return(params)

}

#' Resample Function
#'
#' @param FUN sample function to use
#' @param limits limits of values set as a vector c(x,y), Inf and -Inf are acceptable values
#' @param ... other arguments for FUN
#'
#' @return a vector a randomly sampled values within the limits
#' @export
#'
#' @examples
#' y <- rsamp(rnorm, limits = c(0,5), n = 100, mean = 0, mean = 3)
#' y <- rsamp(rnorm, limits = c(0, Inf), n = 100, mean = 0, mean = 3)
rsamp <- function(FUN, limits, ...){
  dots <- list(...)
  l <- formals(FUN)
  if(!("n" %in% names(l))){
    stop("sample size (n) not present in called FUN")
  }
  y <- do.call(FUN, dots)

  if(length(limits)!=2L){
    stop("Limits must of length two")
  }
  mn <- min(limits)
  mx <- max(limits)
  i <- 1
  while(any(y<mn|y>mx)){
    n1 <- sum(y<mn|y>mx)
    dots["n"] <- n1
    wn1 <- which(y<mn|y>mx)
    ## This has to change the x changes
    dots2 <- lapply(dots, function(x) if(length(x)>1L) x[wn1] else x)
    y[wn1] <- do.call(FUN, dots2)

  }
  return(y)
}

#' Inverse logit
#'
#' @param x a number
#'
#' @return inverse log of the number
inv_log <- function(x){
  return(exp(x)/(1+(exp(x))))
}

#' Calculating multinomial odds
#'
#' @param mat matrix
#'
#' @return matrix of multinomial odds
calc_multi_odds <- function(mat){
  mat2 <- t(apply(mat, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  mat2 <- cbind(1-rowSums(mat2), mat2)
  return(mat2)
}

#' Calculating binomial odds
#'
#' @param mat matrix
#'
#' @return matrix of binomial odds
calc_binom_odds <- function(mat){
  imat <- inv_log(mat)
  mat2 <- cbind(1-imat, imat)
  return(mat2)
}
