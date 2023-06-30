# library(tidyverse)
# # # ##################################################################
# wl_las15_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/baseline_hazards2015LAS.xlsx", sheet = 1) %>%
#     complete(Days = 0:max(Days)) %>%
#     fill(Survival, .direction = "down")
#
# post_tx_las15_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/baseline_hazards2015LAS.xlsx", sheet = 2) %>%
#   complete(Days = 0:max(Days)) %>%
#   fill(Survival, .direction = "down")
#
#
# wl_las21_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/LAS_refit2_betas.xlsx", sheet = 2, skip = 2) %>%
#   complete(Days = 0:max(Days)) %>%
#   fill(Survival, .direction = "down")
# #
# post_tx_las21_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/LAS_refit2_betas.xlsx", sheet = 4, skip = 2) %>%
#   rename(Survival = `survival probability`) %>%
#   complete(Days = 0:max(Days)) %>%
#   fill(Survival, .direction = "down")
# #
# wl_cas23_survrates <- wl_las21_survrates
# # wl_cas23_survrates_rec365 <- read_csv("/proj/DaltonLab/projects/lungTx/p0054/data_raw/wl_cas_recal365.csv")
# # this one used a spline
# # wl_cas23_survrates_rec365 <- read_csv("/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp_wl_cas_recal365.csv")
# # Version2
# wl_cas23_survrates_rec365 <- read_csv("/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_wl_cas_recal365.csv")
#
# pt0 <- tibble(Days = 0, Survival = 1)
# post_tx_cas23_survrates <- read_csv("/proj/DaltonLab/projects/lungTx/p0054/data_raw/posttx_las_baseline_hazard.csv") %>%
#   mutate(Survival = exp(-cumhaz)) %>%
#   select(Days = time, Survival) %>%
#   ## Makes sure there is a survival rate for every date
#   complete(Days = 1:max(Days)) %>%
#   fill(Survival, .direction = "down") %>%
#   filter(Days <= 1825) %>%
#   bind_rows(pt0) %>%
#   arrange(Days)
# post_tx_cas23_survrates_rec365 <- read_csv("/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_ptx_cas_recal365.csv")
# #
# # ## This is needed to generate candidates and donors now
# # load("/proj/DaltonLab/projects/lungTx/p0054/data/distList.Rdata")
# # fixed suppo2 and syst_pap I think
# ## recalibrated donor and candidate count
# load("/proj/DaltonLab/projects/lungTx/p0054/data/distList62323_2.Rdata")
# # load("/proj/DaltonLab/projects/lungTx/p0054/data/distList2.Rdata")
# # recalibrated count
# # load("/proj/DaltonLab/projects/lungTx/p0054/data/distList60523_2.Rdata")
# # I put systap from 61223 into 60523_2 to test out a few things.
# # I will make a file later
# # zip code and lat and long for centers, this will be removed later I think (Paul)
# load("/proj/DaltonLab/projects/lungTx/p0054/data/ids_tracking2.Rdata")
#
# # institution <- haven::read_sas("/proj/lungtx/Data101421/institution.sas7bdat") %>%
# #   rename_all(tolower) %>%
# #   mutate(across(where(is.character), ~haven::zap_empty(.x)))
# # # ############################################################
# # Load zip code with geoid and shape files for institutions with valid zip codes
# # library(zipcodeR)
# # zip_distinct <- institution %>%
# #   mutate(
# #     primary_zip = substr(primary_zip, 1, 5)
# #   )%>%
# #   select(primary_zip) %>%
# #   distinct()
# # zip_all <- left_join(zip_distinct, zipzcta::zipzcta, by = c("primary_zip" = "zip")) %>%
# #   left_join(
# #     tigris::zctas(cb = TRUE, starts_with = ),
# #     by = c("zcta" = "ZCTA5CE10")
# #   )
# # zip_all <- left_join(zip_all, geocode_zip(zip_all$zcta), by = c("zcta" = "zipcode")) %>%
# #   drop_na()
#
# # from UNOS calculation I was told, values of incompatibility for ABO, CPRA(HLA) and HGT
# pABOs <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/All Lung Rating Scales-Equations For SRTR update 20201202.xlsx", sheet = 3) %>%
#   dplyr::select(abo = `Candidate blood group`, pabo = `Prob(ABO-incompatibility)`) %>%
#   filter(!is.na(abo))
#
# pCPRAs <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/All Lung Rating Scales-Equations For SRTR update 20201202.xlsx", sheet = 5) %>%
#   dplyr::select(CPRA, pcpra = `CPRA (Probability of incompatible donor)`)
#
# pHGTs <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/All Lung Rating Scales-Equations For SRTR update 20201202.xlsx", sheet = 6) %>%
#   dplyr::select(cand_hgt, dx_grp = Diag, phgt = incompatibility)
# # Transplant Size ranges
# single_rg <- dplyr::tibble(dx_grp = c("A", "B", "C", "D"), s_lower = c(0, -4, -2, -4), s_upper = c(8, 2, 4, 2))
# double_rg <- dplyr::tibble(dx_grp = c("A", "B", "C", "D"), d_lower = c(-2, -4, -2, -6), d_upper = c(6, 2, 4, 0))
# abo_match_df <- data.frame(abo = c("A", "A", "B", "B","AB", "O", "O", "O", "O"),
#                            match = c("A", "AB", "B", "AB", "AB", "O", "A", "B", "AB"))
# count_match_df <- data.frame(don_org = c("DLU", "DLU", "DLU", "LUL", "LUL", "LUR", "LUR"),
#                              match = c("D", "S", "E", "S", "E", "S", "E"))
#
#
# ## Acceptance model information
# load(file = "/proj/DaltonLab/projects/lungTx/p0054/data/accpt_data.Rdata")
# dist_data <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data/distance_matx2.RDS")
# # recal_f <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0029/data/orig_recal.RDS")
# odd3 <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/dx_odds.RDS")
# # removal_rates <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data/removal_surv.RDS")
# # remov_df <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df.RDS")
# # remov_df <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df2.RDS")
# # remov_df <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df_gomp.RDS")
# # remov_df2 <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df_weib_sep2.RDS")
# # remov_df3 <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df_weib_sep3.RDS")
# # remov_df4 <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_df_weib_sep4.RDS")
# # rem_rt <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_probs_weib_str.RDS")
# # rem_rt <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_probs_weib_str2.RDS")
# rem_rt <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_probs_kap_str.RDS")
# prior_dist_gammas <- readRDS(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/prior_days_gamma.RDS")
# wl_cas23_survrates_rec365_dx <- read_csv(file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_wl_cas_recal365_dx.csv")
# wl_survpost365 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data_raw/landmark365_death_dx.RDS")
# max_death_day <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data_raw/landmark365_death_dx_max.RDS")
# #######################################
# # # # load into internal data for package
# usethis::use_data(
#   ## acceptance pieces
#   accpt_centers,
#   accpt_coef,
#   ## Survival rates
#   wl_las15_survrates,
#   wl_las21_survrates,
#   wl_cas23_survrates,
#   post_tx_las15_survrates,
#   post_tx_las21_survrates,
#   post_tx_cas23_survrates,
#   ## Recalibrated
#   wl_cas23_survrates_rec365,
#   wl_cas23_survrates_rec365_dx,
#   post_tx_cas23_survrates_rec365,
#   ## landmark wl survival
#   wl_survpost365,
#   max_death_day,
#   ## candidates prior to cohort period
#   prior_dist_gammas,
#   odd3,
#   ## removal
#   rem_rt,
#   ## CAS stuff
#   dist_data,
#   pABOs,
#   pHGTs,
#   pCPRAs,
#   ## parameter generating
#   all_params_dist,
#   all_ids, ot_ids, small_ids, mx_id,
#   ## for distances/screening
#   can_ids_track2,
#   don_ids_track3,
#   single_rg,
#   double_rg,
#   abo_match_df,
#   count_match_df,
#   ## other arguments
#   internal = TRUE,
#   overwrite = TRUE)
#
# # save( accpt_centers,
# #       accpt_coef,
# #       # m_coef_v1,
# #       ####Current logit acceptance model coefficients
# #       ####
# #       # institution,
# #       # disposition,
# #       # tx_lu,
# #       # donor_raw,
# #       # candidate_raw,
# #       # recipient,
# #       ##
# #       wl_las15_survrates,
# #       wl_las21_survrates,
# #       wl_cas23_survrates,
# #       post_tx_las15_survrates,
# #       post_tx_las21_survrates,
# #       post_tx_cas23_survrates,
# #       ## CAS smoothed
# #       wl_cas23_survrates2,
# #       wl_cas23_survrates3,
# #       ## Recal
# #       wl_cas23_survrates_rec365,
# #       wl_cas23_survrates_rec730,
# #       wl_cas23_survrates_rec365_dx,
# #       post_tx_cas23_survrates_rec365,
# #       post_tx_cas23_survrates_rec365_dx,
# #       ## landmark
# #       wl_survpost365,
# #       max_death_day,
# #       prior_dist_gammas,
# #       odd3,
# #       remov_df,
# #       remov_df2,
# #       remov_df3,
# #       remov_df4,
# #       rem_rt,
# #       zip_all,
# #       dist_data,
# #       pABOs,
# #       pHGTs,
# #       pCPRAs,
# #       all_params_dist,
# #       all_ids, ot_ids, small_ids, mx_id,
# #       can_ids_track2,
# #       don_ids_track3,
# #       single_rg,
# #       double_rg,
# #       abo_match_df,
# #       count_match_df,
# #       removal_rates, file = "/proj/DaltonLab/projects/lungTx/p0054/packageDev/Old COMET/Raw_package_data.rda")
# # #
