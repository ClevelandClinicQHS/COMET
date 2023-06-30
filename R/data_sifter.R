# library(tidyverse)
# library(zipcodeR)
# library(here)
# ## Distance calculations
#
# load("/proj/DaltonLab/projects/lungTx/p0054/data/ids_tracking.Rdata")
# load("/proj/DaltonLab/projects/lungTx/p0054/data/institution2latlonraw.Rdata")
# ######## Donors
# don_coord <- haven::read_sas("/proj/lungtx/Data101421/supplemental/donorhospital.sas7bdat") %>%
#   rename_all(tolower)
#
# # don_coord1 <- don_coord %>% filter(active_flg == 1)
#
# don_coord2 <- don_coord %>% filter(provider_num %in% don_ids_tracks2$don_hosp_prov_num) %>%
#   distinct(longitude, latitude, hospital_zip, provider_num, .keep_all = TRUE)
#
# # don_coord3 <- don_coord[sort(c(which(duplicated(don_coord$provider_num)), which(duplicated(don_coord$provider_num, fromLast = TRUE)))),]
#
# # don_coord4 <- don_coord2[sort(c(which(duplicated(don_coord2$provider_num)), which(duplicated(don_coord2$provider_num, fromLast = TRUE)))),]
#
# # don_coords_needed[which(is.na(don_coords_needed$zcta)),]
# #
# don_coords_needed <- don_coord2 %>%
#   distinct(provider_num, .keep_all = TRUE) %>%
#   filter(provider_num %in% don_ids_tracks2$don_hosp_prov_num) %>%
#   select(provider_num, hospital_zip, longitude, latitude) %>%
#   left_join(zipzcta::zipzcta, by = c("hospital_zip" = "zip")) %>%
#   select(provider_num, longitude, latitude)
#
# # ############ Candidates
# ctr_coord <- haven::read_sas("/proj/lungtx/Data101421/institution.sas7bdat")  %>%
#   rename_all(tolower)%>%
#   # select(ctr_id, primary_zip) %>%
#   mutate(primary_zip = gsub("[^0-9.-]", "", primary_zip) %>%
#            str_sub(1, 5)) %>% #select last five for omission of trailing values
#   filter(between(as.numeric(primary_zip), 1, 99950)) %>%  #preliminary screen of missing data
#   left_join(zipzcta::zipzcta, by = c("primary_zip" = "zip")) %>%
#   select(ctr_id, zcta)
#
# ctr_coord_needed <- ctr_coord %>%
#   filter(ctr_id %in% can_ids_track$can_listing_ctr_id)
#
# # ctr_coord_zipped <- wl_clean %>%  select(can_listing_ctr_id, lat_wl, lon_wl) %>%
# #   distinct(can_listing_ctr_id, .keep_all = TRUE) %>%
# #   filter(can_listing_ctr_id %in% can_ids_track$can_listing_ctr_id)
#
# ##Join donors/candidates to their respective coded ids (ids_tracks2 and can_ids_track)
# ctr_perm <- ctr_coord_zipped %>%
#   left_join(can_ids_track, by = "can_listing_ctr_id") %>%
#   select( -can_listing_ctr_id)
#
# don_perm <- don_coords_needed %>%
#   left_join(don_ids_tracks2, by = c("provider_num" = "don_hosp_prov_num")) %>%
#   select( - provider_num)
#
# ####Calculate distance for each pair, then pivot wider to make a matrix
# ###Columns provide can tx center names, while rows provide donor hospital ids
# distance_matx <- geodist::geodist(
#   x = don_perm %>% select(longitude, latitude),
#   y = ctr_perm %>% select(lon_wl, lat_wl),
#   measure = "geodesic"
# )/1852
#
# distance_matx <- distance_matx %>%
#   as.data.frame() %>%
#   mutate(don_hosp = row_number(), .before = everything())
#
# colnames(distance_matx)[2:ncol(distance_matx)] <- 1:length(ctr_perm$new_id)
#
# distance_matx2 <- distance_matx %>%
#   pivot_longer(cols = -don_hosp, names_to = "can_center", values_to = "distance_nm")
#
# ## a few spot checks it's the same
# # distance_mat2 <- geosphere::distGeo(
# #   p1 = select(don_perm, longitude, latitude) %>% .[52,],
# #   p2 = select(ctr_perm, lon_wl, lat_wl) %>% .[16,])/1852
#
#
#
# # save(distance_matx, file = "/proj/DaltonLab/projects/lungTx/p0054/data/distance_matx.Rdata")
# saveRDS(distance_matx2, file = "/proj/DaltonLab/projects/lungTx/p0054/data/distance_matx2.RDS")
#
# ## If we want latitude and longitidue
# # ctr_coord_needed2 <- full_join(ctr_coord_needed, zipcodeR::geocode_zip(ctr_coord_needed$zcta), by = c("zcta" = "zipcode")) %>%
# #   mutate(latitude = lat,
# #          longitude = lng, .keep = "unused")
# #
# # # can_ids_track
# #
# # can_ids_track2 <- left_join(can_ids_track, ctr_coord_needed2, by = c("can_listing_ctr_id" = "ctr_id"))
# # don_ids_track3 <- left_join(don_ids_tracks2, don_coords_needed, by = c("don_hosp_prov_num" = "provider_num"))
# #
# # # new_donors <- new_donors %>%
# # #   mutate(center = as.numeric(center))
# # # #
# # # new_candidates2 <- left_join(new_candidates, can_ids_track2, by = c("center" = "new_id"))
# # # new_donors2 <- left_join(new_donors, don_ids_track3, by = c("center" = "new_id2"))
# # #
# # save(can_ids_track2, don_ids_track3, file = "/proj/DaltonLab/projects/lungTx/p0054/data/ids_tracking2.Rdata")
# # save(don_coords_needed, ctr_coord_zipped, file = here("data/institution2latlonraw.Rdata"))
# #
# #
# # # # ########################### RECALiBRATE CAS
# # # #
# library(COMET)
# library(survival)
# library(tidyverse)
# rs0 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs1.RDS")
# # # # # # # #
# # # # # # # # #rs1 <- rs2
# # # # # # ct1 <- map(rs1, concat)
# # # # # # # ## ct1 <- map(ct1, arrange, c_id)
# ct1 <- map(rs0, cutoff, 730, 730, 802)
# km1 <- map(ct1, ~survfit(Surv(wait_death_time, wait_death)~1, data = .x))
# # # # #
# sv1 <- #map_dfr(km1, get_surv_data, .id = "Run") %>%
#   map(km1, get_surv_data) %>%
#   bind_rows(.id = "Run") %>%
#   complete(Run, time = 1:max(time)) %>%
#   group_by(Run) |>
#   fill(-c(Run, time, n.event, n.censor), .direction = "down") |>
#   ungroup()
# # # #
# sv1_g <- sv1 %>%
#   # group_by(time) %>%
#   summarise(across(c(surv, n.risk, cumhaz), .fns = list(max = max, min = min, med = ~median(.x, na.rm = TRUE))), .by = time) |>
#   mutate(cumhaz3 = -log(surv_med))
# # #
# sv1_med <- sv1_g %>%
#   select(time, surv_med, cumhaz_med, cumhaz3) %>%
#   # filter(time <= 30)%>%
#   mutate(dhaz_med = cumhaz_med - lag(cumhaz_med, 1, default = 0)) %>%
#   mutate(llsurv_med = log(-log(surv_med)))
#
# #
# #
# #
# # # # #
# tsam16 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Cohort16_17.RDS") |>
#   mutate(disapp = case_when(
#     # tx != 1 & wait_death != 1 & (censor_dt <= "2017-12-31"|death_remov_dt <="2017-12-31")  ~ 1,
#     tx != 1 & wait_death != 1 & (censor_dt <= "2019-12-31"|death_remov_dt <="2019-12-31")  ~ 1,
#     .default = 0
#   ))
#
# LU_ALLOC <- haven::read_sas("/proj/lungtx/Data102022/supplemental/lu_alloc2209.sas7bdat") %>%
#   rename_all(tolower) %>%
#   select(px_id, can_listing_dt, canhx_grouping) %>%
#   distinct() %>%
#   group_by(px_id) %>%
#   slice_head(n=1) %>%
#   ungroup()
# #
# km16 <- survfit(Surv(wait_death_days, wait_death)~1, data = tsam16)
#
#
# sv16 <- get_surv_data(km16) %>%
#   complete(time = 1:max(time)) %>%
#   fill(surv, cumhaz, .direction = "down") %>%
#   mutate(dhaz = cumhaz - lag(cumhaz, 1, default = 0),
#          llsurv = log(-log(surv)))
# # # #
# day_rec <- 364
# # # # #
# sv16_c <- filter(sv16, time <= day_rec) %>%
#   # sv16 %>%
#   select(time, surv, cumhaz, dhaz, llsurv) %>%
#   left_join(sv1_med, by = "time") %>%
#   mutate(fct1 = surv/surv_med,
#          fct2 = cumhaz/cumhaz_med,
#          # fct3 = dhaz-dhaz_med,
#          fct3 = cumhaz/cumhaz3,
#          fct4 = llsurv_med/llsurv,
#          .keep = "unused")
# #
# #
# # # # #
# # # # #
# # # # # # # #
# wl_las21_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/LAS_refit2_betas.xlsx", sheet = 2, skip = 2) %>%
#   complete(Days = 0:max(Days)) %>%
#   fill(Survival, .direction = "down")
# # # # # #
# wl2 <- wl_las21_survrates %>%
#   filter(Days <= day_rec & Days >0) %>%
#   mutate(cumhaz = -log(Survival),
#          dhaz = cumhaz - lag(cumhaz, n = 1, default = 0),
#          llsurv = log(-log(Survival))) %>%
#   left_join(sv16_c, by = c("Days" = "time")) %>%
#   # rowwise() %>%
#   mutate(Survival2 = Survival*fct1,
#          cumhaz2 = cumhaz*fct2,
#          cumhaz3 = cumhaz*fct3,
#          llsurv2 = llsurv/fct4)
#
#
# ######### by group A's are completely off
# tsam16_dx <- left_join(tsam16, LU_ALLOC)
#
# km16dx <- survfit(Surv(wait_death_days, wait_death)~strata(canhx_grouping), data = tsam16_dx)
#
# day0s <- crossing(dx_grp = LETTERS[1:4], surv = 1, time = 0)
#
# sv16dx <- get_surv_data(km16dx) %>%
#   rename(dx_grp = `strata(canhx_grouping)`) |>
#   group_by(dx_grp) |>
#   complete(time = 1:max(time)) %>%
#   bind_rows(day0s) |>
#   arrange(dx_grp, time) |>
#   fill(surv, cumhaz, .direction = "down") %>%
#   ungroup() |>
#   mutate(dhaz = cumhaz - lag(cumhaz, 1, default = 0),
#          llsurv = log(-log(surv)), .by = dx_grp)
#
# ggplot()+
#   geom_line(aes(x = time, y = surv, group = dx_grp, color = dx_grp), data = sv16dx)+
#   facet_wrap(~dx_grp)+
#   coord_cartesian(xlim = c(0, 800))
#
# # # #
# day_rec <- 364
# # # # #
# sv16_cdx <- filter(sv16dx, time <= day_rec) %>%
#   # sv16 %>%
#   select(time, surv, cumhaz, dhaz, llsurv, dx_grp) %>%
#   left_join(sv1_med, by = "time") %>%
#   mutate(fct1 = surv/surv_med,
#          fct2 = cumhaz/cumhaz_med,
#          # fct3 = dhaz-dhaz_med,
#          fct3 = cumhaz/cumhaz3,
#          fct4 = llsurv_med/llsurv,
#          .keep = "unused")
#
#
# wl_las21_survrates <- readxl::read_xlsx("/proj/DaltonLab/projects/lungTx/p0054/data_raw/LAS_refit2_betas.xlsx", sheet = 2, skip = 2) %>%
#   complete(Days = 0:max(Days)) %>%
#   fill(Survival, .direction = "down")
#
# wl_las21lm <- lm(Survival~Days, data = wl_las21_survrates)
# wl_las21_survrates_ext <- tibble(Days = 1:364, Survival = predict(wl_las21lm, newdata = data.frame(Days = 1:364)))
#
#
# wl3_pred <- data.frame(Days = 1:364)
# # # # # #
# wl2_dx <- wl_las21_survrates_ext %>%
#   filter(Days <= day_rec & Days >0) %>%
#   mutate(cumhaz = -log(Survival),
#          dhaz = cumhaz - lag(cumhaz, n = 1, default = 0),
#          llsurv = log(-log(Survival))) %>%
#   left_join(sv16_cdx, by = c("Days" = "time")) %>%
#   # rowwise() %>%
#   mutate(Survival2 = Survival*fct1,
#          cumhaz2 = cumhaz*fct2,
#          cumhaz3 = cumhaz*fct3,
#          llsurv2 = llsurv/fct4) |>
#   nest(.by = dx_grp) |>
#   # mutate(mod = map(data, ~lm(log((cumhaz2)) ~ splines::bs(log(Days), knots = log(seq(15, 700, 25)), degree = 1), data = .x)),
#   mutate(mod = map(data, ~lm(log((cumhaz2)) ~ splines::bs(log(Days), knots = log(seq(15, 350, 25)), degree = 1), data = .x)),
#          preds = map(mod, ~exp(-exp(predict(.x, wl3_pred)))))
#
# day0 <- crossing(Days = 0, Survival = 1, dx_grp = LETTERS[1:4])
#
# wl3dx <- wl2_dx |>
#   select(dx_grp, preds) |>
#   unnest(cols = preds) |>
#   mutate(Days = row_number(), .by = dx_grp) |>
#   rename(Survival = preds) |>
#   bind_rows(day0) |>
#   arrange(dx_grp, Days)
#
# write_csv(wl3dx, file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_wl_cas_recal365_dx.csv")
#
# fit3l <- lm(log(cumhaz2) ~ splines::bs(log(Days), knots = log(seq(15, 350, 25)), degree = 1), data = wl2)
#
# # # # # # # # # # # #
# # # # # # # # # # # # #
# wl3_pred <- data.frame(Days = 1:364)
# wl3l <- wl3_pred %>% mutate(Survival = exp(-exp(predict(fit3l, wl3_pred))))
#
# day0 <- data.frame(Days = 0, Survival = 1)
#
# wl3l <- bind_rows(day0, wl3l)
# write_csv(wl3l, file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_wl_cas_recal365.csv")
# ## ################ post-transplant recal
#
# ptkm1 <- map(ct1, ~survfit(Surv(post_tx_time, post_tx_death)~1, data = .x))
#
# ptsv1 <- map(ptkm1, get_surv_data) %>%
#   bind_rows(., .id = "Run") %>%
#   # bind_rows(day0) %>%
#   arrange(Run, time) %>%
#   group_by(Run) %>%
#   complete(time = 1:max(time)) %>%
#   fill(surv, n.risk, cumhaz, .direction = "down") %>%
#   fill(n.risk, .direction = "up") %>%
#   ungroup()
#
# ptsv1_med <- ptsv1 %>%
#   group_by(time) %>%
#   summarise(across(.cols = c(surv, n.risk, cumhaz), .fns = list(max = max, min = min, med = ~median(.x, na.rm = TRUE)))) |>
#   mutate(cumhaz3 = -log(surv_med)) |>
#   select(time, surv_med, cumhaz_med, cumhaz3) %>%
#   # filter(time <= 30)%>%
#   mutate(dhaz_med = cumhaz_med - lag(cumhaz_med, 1, default = 0)) %>%
#   mutate(llsurv_med = log(-log(surv_med)))
#
# ptkm16 <- survfit(Surv(ptx_time, ptx_death)~1, data = tsam16)
# ptsv16 <- get_surv_data(ptkm16) %>%
#   complete(time = 1:max(time)) %>%
#   fill(surv, cumhaz, .direction = "down") %>%
#   mutate(dhaz = cumhaz - lag(cumhaz, 1, default = 0),
#          llsurv = log(-log(surv)))
#
#
# day_rec <- 364
# ptsv16_c <- filter(ptsv16, time <= day_rec) %>%
#   # sv16 %>%
#   select(time, surv, cumhaz, dhaz, llsurv) %>%
#   left_join(ptsv1_med, by = "time") %>%
#   mutate(fct1 = surv/surv_med,
#          fct2 = cumhaz/cumhaz_med,
#          # fct3 = dhaz-dhaz_med,
#          # fct3 = cumhaz/cumhaz3,
#          fct4 = llsurv_med/llsurv,
#          .keep = "unused")
# #
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
#
# pt2 <- post_tx_cas23_survrates %>%
#   mutate(cumhaz = -log(Survival),
#          dhaz = cumhaz - lag(cumhaz, n = 1, default = 0),
#          llsurv = log(-log(Survival))) %>%
#   left_join(ptsv16_c, by = c("Days" = "time")) %>%
#   # rowwise() %>%
#   mutate(#Survival2 = Survival*fct1,
#          cumhaz2 = cumhaz*fct2,
#          # cumhaz3 = cumhaz*fct3,
#          llsurv2 = llsurv/fct4) |>
#   filter(Days < 365)
#
# ptfit3l <- lm(log(cumhaz2) ~ splines::bs(log(Days), knots = log(seq(15, 350, 25)), degree = 1), data = subset(pt2, Days >0))
#
#
# pt3_pred <- data.frame(Days = 1:364)
# pt3l <- pt3_pred %>% mutate(Survival = exp(-exp(predict(ptfit3l, pt3_pred))))
# day0 <- data.frame(Days = 0, Survival = 1)
# pt3l <- bind_rows(day0, pt3l)
#
# write_csv(pt3l, file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/sp2_ptx_cas_recal365.csv")
#
#
# ################### Removal probability
# tsam16 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Cohort16_19.RDS")
# # tsam16 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Cohort16_17.RDS")
#
# LU_ALLOC <- haven::read_sas("/proj/lungtx/Data102022/supplemental/lu_alloc2209.sas7bdat") %>%
#   rename_all(tolower) %>%
#   select(px_id, can_listing_dt, canhx_grouping) %>%
#   distinct() %>%
#   group_by(px_id) %>%
#   slice_head(n=1) %>%
#   ungroup()
#
# ## find those who disappeared
# tsam16_dis2 <- tsam16 %>%
#   mutate(disapp = case_when(
#     # tx != 1 & wait_death != 1 & (censor_dt <= "2017-12-31"|death_remov_dt <="2017-12-31")  ~ 1,
#     tx != 1 & wait_death != 1 & (censor_dt <= "2019-12-31"|death_remov_dt <="2019-12-31")  ~ 1,
#       .default = 0
#   )) %>%
#   group_by(g = can_listing_dt < "2016-01-01") %>%
#   mutate(c_id = row_number(), .before = px_id,
#          c_id = ifelse(g, -c_id, c_id)) %>%
#   ungroup()%>%
#   left_join(LU_ALLOC)%>%
#   mutate(wait_death_days = round(wait_death_wks*7, 0)) %>%
#   as.data.frame() %>% filter(wait_death_days>0)
#
# # rem16 <- survfit(Surv(wait_death_days, disapp)~1, data = tsam16_dis2)
# #
# # rem16d <- get_surv_data(rem16)
# #
# # tx <- tsam16_dis2 %>%
# #   filter(tx == 1)
#
#
# rdm1yr <- tsam16_dis2 |>
#   # filter(wait_death_days <= 365) |>
#   mutate(wait_death_days = ifelse(wait_death_days >364, 365, wait_death_days)) |>
#   nest(.by = canhx_grouping) |>
#   mutate(km = map(data, ~survfit(Surv(wait_death_days, disapp)~1, data = .x))) |>
#   mutate(surd = map(km, get_surv_data),
#          fitm = map(surd, ~lm(log(-log(surv)) ~ splines::bs(log(time), degree = 1), data = .x)),
#          preds = map2(fitm, surd, ~predict(.x, newdata = tibble(time = 1:max(.y$time)))),
#          surd2 = map(preds, ~tibble(Days = 1:length(.x), Surv = exp(-exp(.x))))
#   ) |>
#   select(canhx_grouping, surd2) |>
#   unnest(col = surd2) |>
#   arrange(canhx_grouping, Days) |>
#   rename(dx_grp = canhx_grouping, Survival = Surv) |>
#   mutate(ls = Survival/lag(Survival, 1, default = 1), .by = dx_grp)
#
# rdmld1yr <- tsam16_dis2 |>
#   filter(wait_death_days > 364) |>
#   mutate(wait_death_days = wait_death_days - 364) |>
#   mutate(wait_death_days = ifelse(wait_death_days >2000, 2001, wait_death_days)) |>
#   nest(.by = canhx_grouping) |>
#   mutate(km = map(data, ~survfit(Surv(wait_death_days, disapp)~1, data = .x))) |>
#   mutate(surd = map(km, get_surv_data),
#          fitm = map(surd, ~lm(log(-log(surv)) ~ splines::bs(log(time), degree = 1), data = .x)),
#          preds = map2(fitm, surd, ~predict(.x, newdata = tibble(time = 1:max(.y$time)))),
#          surd2 = map(preds, ~tibble(Days = 1:length(.x), Surv = exp(-exp(.x))))
#   ) |>
#   select(canhx_grouping, surd2) |>
#   unnest(col = surd2) |>
#   arrange(canhx_grouping, Days) |>
#   rename(dx_grp = canhx_grouping, Survival = Surv) |>
#   mutate(Days = Days + 364) |>
#   mutate(ls = Survival/lag(Survival, 1, default = 1), .by = dx_grp)
#
# day0 <- crossing(dx_grp = LETTERS[1:4], Days = 0, Survival =1, ls = 1)
#
# rdm_all <- bind_rows(day0, rdm1yr, rdmld1yr) |> arrange(dx_grp, Days) |>
#   mutate(Surv2 = cumprod(ls), .by = dx_grp) |>
#   select(dx_grp, Days, Survival = Surv2)
#
# ## This is removal one implemented
# # saveRDS(rdm_all, file = "/proj/DaltonLab/projects/lungTx/p0054/data_raw/removal_probs_kap_str.RDS")
#
#
#
# # # # # ######## weibull
#
# # # #
# # # #
# # I'll fix this
# # tidy_summary <- function(.data, ..., na.rm = TRUE, .names = NULL, .by = NULL){
# #
# #   summ <- select(.data, ...) %>%
# #     dplyr::summarise(dplyr::across(.cols = -.by, .fns = list(min = ~min(.x, na.rm = na.rm), q1 = ~quantile(.x, probs = .25, na.rm = na.rm),
# #                                                                             med = ~median(.x, na.rm = na.rm), q3 = ~quantile(.x, probs = .75, na.rm = na.rm),
# #                                                                             max = ~max(.x, na.rm = na.rm)),
# #                                    .names = .names), .by = .by) %>%
# #     tidyr::pivot_longer(cols = dplyr::everything()) %>%
# #     dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
# #            stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused")
# #
# # }
# # # # #
# # # # # ts <- tidy_summary(c1, c_id, listing_day)
# # # #
# ######### Fixing candidate count 2018
# LU_ALLOC <- haven::read_sas("/proj/lungtx/Data102022/supplemental/lu_alloc2209.sas7bdat") %>%
#   rename_all(tolower) %>%
#   select(px_id, can_listing_dt, canhx_grouping) %>%
#   distinct() %>%
#   group_by(px_id) %>%
#   slice_head(n=1) %>%
#   ungroup()
#
#
# tsam18 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Tsam18_19.RDS")
# tsam18_19 <- tsam18 |>
#   filter(can_listing_dt > "2017-12-31") |>
#   left_join(LU_ALLOC)
#
# tsam18_19_z <- tsam18_19 |>
#   summarise(Count = n(), .by = canhx_grouping) |>
#   arrange(canhx_grouping)
# load("/proj/DaltonLab/projects/lungTx/p0054/data/distList60523.Rdata")
#
# can_counts <- all_params_dist$can_count$mu
#
# can_sd <- all_params_dist$can_count$Sigma
#
# test_new_counts <- function(dlc){
#   Centers <- 1:max(as.numeric(str_extract(names(dlc), "\\d+")))
#   names(Centers) <- Centers
#
#   df_b <- sapply(dlc, function(x) rpois(n = 730, lambda = x)) |>
#     as.data.frame() |>
#     mutate(listing_day = row_number()) |>
#     pivot_longer(-listing_day, names_sep = ",", names_to = c("center", "dx_num"),
#                  names_transform = list("center" = ~as.numeric(str_extract(.x, "\\d+")),
#                                         "dx_num" = ~as.numeric(str_extract(.x, "\\d+")))) |>
#     filter(value > 0) |> summarise(value = sum(value), .by = dx_num) |> arrange(dx_num)
#
#   # to_count <- sum(df_b_long$value) #+1263
#
# }
#
# gen_new_lamdas <- function(seed){
#   set.seed(seed)
#   ls <- MASS::mvrnorm(n = 1, mu = can_counts, Sigma = can_sd)
#   return(ls)
# }
#
# t1s <- purrr::map(1:1000, ~test_new_counts(gen_new_lamdas(.x)))
# t1z <- bind_rows(t1s, .id = "R") |>
#   summarise(vm = median(value), .by = dx_num) |>
#   mutate(canhx_grouping = LETTERS[dx_num]) |>
#   left_join(tsam18_19_z) |>
#   mutate(adj = Count/vm)
#
# can_counts2 <- map2(1:4, t1z$adj, ~can_counts[str_which(names(can_counts), paste0(",", .x, "]"))]*.y) |> flatten_dbl()
#
# # can_counts2 <- can_counts * nrow(tsam18_19)/median(t1s)
#
# gen_new_lamdas2 <- function(seed){
#   set.seed(seed)
#   ls <- MASS::mvrnorm(n = 1, mu = can_counts2, Sigma = can_sd)
#   return(ls)
# }
#
# t2s <- purrr::map(1:1000, ~test_new_counts(gen_new_lamdas2(.x)))
# t2z <- bind_rows(t2s, .id = "R") |>
#   summarise(vm = median(value), .by = dx_num) |>
#   mutate(canhx_grouping = LETTERS[dx_num]) |>
#   left_join(tsam18_19_z) |>
#   mutate(adj = Count/vm)
# median(t2s)
# boxplot(t2s)
#
# all_params_dist$can_count$mu <- can_counts2
#
# save(all_params_dist, all_ids, mx_id, ot_ids, small_ids, file = "/proj/DaltonLab/projects/lungTx/p0054/data/distList60523_2.Rdata")
#
#
# ## Fixing Donor Count 2018
# don18 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data_raw/Tsam18_donors.RDS")
#
# don_ct <- all_params_dist$don_count$mu
# don_sd <- all_params_dist$don_count$Sigma
#
# test_donors_count_f <- function(dld){
#
#   # set.seed(sed)
#
#   Hosps <- 1:length(dld)
#   names(Hosps) <- Hosps
#
#   df_b <- as.data.frame(mapply(function(x) rpois(dld[x], n = 730), Hosps)) |>
#     mutate(recovery_day = row_number())
#
#   df_b_long <- pivot_longer(df_b, -recovery_day, names_to = "hospital") |>
#   filter(value > 0)
#
#   to_count <- sum(df_b_long$value)
#   return(to_count)
#
# }
#
# gen_donor_lams <- function(sed){
#   set.seed(sed)
#   params <- MASS::mvrnorm(n = 1, mu = don_ct, Sigma = don_sd)
#   if(any(grepl("lambda\\[\\d+", names(params)))){
#     while(any(params < 0)){
#       params <- MASS::mvrnorm(n = 1, mu = don_ct, Sigma = don_sd)
#       # params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
#     }
#   }
#   return(params)
# }
#
# test_donors_count_f(gen_donor_lams(2))
#
# t3s <- purrr::map_dbl(1:1000, ~test_donors_count_f(gen_donor_lams(.x)))
# don_adj <- nrow(don18)/median(t3s)
# don_ct2 <- don_ct * don_adj
#
# gen_donor_lams2 <- function(sed){
#   set.seed(sed)
#   params <- MASS::mvrnorm(n = 1, mu = don_ct2, Sigma = don_sd)
#   if(any(grepl("lambda\\[\\d+", names(params)))){
#     while(any(params < 0)){
#       params <- MASS::mvrnorm(n = 1, mu = don_ct2, Sigma = don_sd)
#       # params_x <- MASS::mvrnorm(n = 1000, mu = l1[[1]], Sigma = l1[[2]])
#     }
#   }
#   return(params)
# }
#
# t4s <- purrr::map_dbl(1:1000, ~test_donors_count_f(gen_donor_lams2(.x)))
#
# all_params_dist$don_count$mu <- don_ct2
#
# save(all_params_dist, all_ids, mx_id, ot_ids, small_ids, file = "/proj/DaltonLab/projects/lungTx/p0054/data/distList62323_2.Rdata")
#
#
