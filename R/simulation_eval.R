# # # ########### Run simulation in parallel
# #
# library(COMET)
# # library(dplyr)
# # library(survival)
# library(furrr)
# # # # # # # #
# plan(cluster, workers = 55)
# # sds <- 1001:2000
# # sds <- 1:300
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
# #                                         match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
# #                                         wl_weight = 2, wl_cap = 365, post_tx_weight = 1, post_tx_cap = 365, seed = .x),
# #                   .progress = TRUE)
# # # # # # #
# # # saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec7.RDS")
# # saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs7.RDS")
# # # # # # # # #
# # # # # # ##
# sds <- 1001:2000
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                        match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#                                        wl_weight = 2, wl_cap = 365, post_tx_weight = 1, post_tx_cap = 365, seed = .x), .progress = TRUE)
# # # # # #
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec7.RDS")
#
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                         match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                         wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1,
#                                         seed = .x), .progress = TRUE)
# # # # #
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec7_cas.RDS")
#
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                         match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                         wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0,
#                                         seed = .x), .progress = TRUE)
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec7_cas0eff.RDS")
# ####
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                         match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                         wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.2,
#                                         seed = .x), .progress = TRUE)
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec7_cas2eff.RDS")
#

# ##
#
# # ## So I can study some matches
# sds <- 1:300
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                        match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
#                                        wl_weight = 2, wl_cap = 365, post_tx_weight = 1, post_tx_cap = 365, seed = .x, include_matches = TRUE), .progress = TRUE)
# # # # # #
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec7M.RDS")
#
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                        match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                        wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1,
#                                        seed = .x, include_matches = TRUE), .progress = TRUE)
# # # #
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec7_casM.RDS")
# #
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                        match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                        wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0,
#                                        seed = .x, include_matches = TRUE), .progress = TRUE)
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec7_cas0effM.RDS")
# ###
# rs0 <- future_map(sds, ~run_simulation(days = 802, can_start = 1275,
#                                        match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
#                                        wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.2,
#                                        seed = .x, include_matches = TRUE), .progress = TRUE)
# saveRDS(rs0, "/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec7_cas2effM.RDS")
# # # #
# # # #
# # # # # ##### Evaluating
# # # # # #
# rs1 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs9.RDS")
# # # # # # # #
# # # # # # # #
# rs2 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec1.RDS")
# rs3 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs_rec1_cas.RDS")
# # # # # # #
# # # # # ## ct1 <- map(rs1, concat)
# library(COMET)
# library(survival)
# library(tidyverse)
# theme_embiggen <- function(){
#   theme_minimal() %+replace% theme(axis.text = element_text(size = 16))
# }
# #
# tic <- Sys.time()
# 2+2
# toc <- Sys.time()
# ct1 <- map(rs1, cutoff,  enroll_date = 730, waitlist_date = 730, post_tx_date = 802)
#
# km1 <- map(ct1, ~survfit(Surv(wait_death_time, wait_death)~1, data = .x))
#
# day0 <- crossing(Run = 1:length(km1), time = 0, surv = 1)
#
# sv1 <- map(km1, get_surv_data) %>%
#   list_rbind(names_to = "Run") %>%
#   bind_rows(day0) %>%
#   arrange(Run, time) %>%
#   group_by(Run) %>%
#   complete(time = 0:max(time)) %>%
#   fill(surv, n.risk, .direction = "down") %>%
#   fill(n.risk, .direction = "up") %>%
#   ungroup()
#
# sv1_med <- sv1 %>%
#   group_by(time) %>%
#   summarise(across(.cols = c(surv, n.risk), .fns = list(q1 = ~quantile(.x, probs = 0.25),
#                                                         med = ~median(.x, na.rm = TRUE),
#                                                         q3 = ~quantile(.x, probs = .75))))
# # # ####
# # # ## ct2 <- map(rs2, concat)
# ct2 <- map(rs2, cutoff, enroll_date = 730, waitlist_date = 730, post_tx_date = 802)
# # #
# km2 <- map(ct2, ~survfit(Surv(wait_death_time, wait_death)~1, data = .x))
#
# day0 <- crossing(Run = as.character(1:length(km2)), time = 0, surv = 1)
#
# sv2 <- map(km2, get_surv_data) %>%
#   bind_rows(., .id = "Run") %>%
#   bind_rows(day0) %>%
#   arrange(Run, time) %>%
#   group_by(Run) %>%
#   complete(time = 0:max(time)) %>%
#   fill(surv, n.risk, .direction = "down") %>%
#   fill(n.risk, .direction = "up") %>%
#   ungroup()
#
# sv2_med <- sv2 %>%
#   group_by(time) %>%
#   summarise(across(.cols = c(surv, n.risk, n.event, n.censor), .fns = list(q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
#                                                         med = ~median(.x, na.rm = TRUE),
#                                                         q3 = ~quantile(.x, probs = .75, na.rm = TRUE))))
# #
# # csv2 <- sv2_med |>
# #   select(time, n.event_med) |>
# #   mutate(n.event_med = if_else(is.na(n.event_med), 0, n.event_med)) |>
# #   mutate(csn = cumsum(n.event_med))
#
# LU_ALLOC <- haven::read_sas("/proj/lungtx/Data102022/supplemental/lu_alloc2209.sas7bdat") %>%
#   rename_all(tolower) %>%
#   select(px_id, can_listing_dt, canhx_grouping) %>%
#   distinct() %>%
#   group_by(px_id) %>%
#   slice_head(n=1) %>%
#   ungroup()
# #
# # # ## tsam cohorts
# tsam18 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Tsam18_19.RDS") |>
#   mutate(disapp = case_when(
#         tx != 1 & wait_death != 1 & censor_dt <= "2019-12-31" ~ 1,
#           .default = 0
#       )) |>
#   mutate(p18 = ifelse(can_listing_dt < "2018-01-01", 1, 0)) |>
#   left_join(LU_ALLOC)
#
# km18 <- survfit(Surv(wait_death_days, wait_death)~1, data = tsam18)
#
# rem18km <- survfit(Surv(wait_death_days, disapp)~1, data = tsam18)
# rem18 <- filter(tsam18, disapp == 1) |>
#   mutate(days = as.numeric(can_listing_dt - as.Date("2018-01-01")))
# rem18d <- get_surv_data(rem18km)
#
# rmkms <- map(ct2, ~survfit(Surv(wait_death_time, removal)~1, data = .x))
# rmkms_s <- map(ct2, ~survfit(Surv(wait_death_time, removal)~strata(dx_grp), data = .x))
# remsv_s <- map(rmkms_s, get_surv_data) |>
#   bind_rows(.id = "Run") |>
#   rename(dx_grp = `strata(dx_grp)`) |>
#   arrange(Run, dx_grp, time) |>
#   group_by(Run, dx_grp) |>
#   # group_by(Run) |>
#   complete(time = 0:max(time)) |>
#   fill(surv, n.risk, .direction = "down") |>
#   fill(n.risk, .direction = "up") |>
#   ungroup()
#
#
# remsv <- map(rmkms, get_surv_data) |>
#   bind_rows(.id = "Run") |>
#   # rename(dx_grp = `strata(dx_grp)`) |>
#   arrange(Run, time) |>
#   # group_by(Run, dx_grp) |>
#   group_by(Run) |>
#   complete(time = 0:max(time)) |>
#   fill(surv, n.risk, .direction = "down") |>
#   fill(n.risk, .direction = "up") |>
#   ungroup()
#
# remsv_med <- remsv |>
#   group_by(time) |>
#   summarise(across(.cols = c(surv, n.risk, n.event, n.censor), .fns = list(q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
#                                                                            med = ~median(.x, na.rm = TRUE),
#                                                                            q3 = ~quantile(.x, probs = .75, na.rm = TRUE))))
#
#
# rems2 <- map(ct2, filter, removal == 1)
#
# rems2y <- map(rems2, ~summarise(.x, across(wait_death_time, .fns = list(min = ~min(.x, na.rm = TRUE),
#                                                                         q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
#                                                                         med = ~median(.x, na.rm = TRUE),
#                                                                         q3 = ~quantile(.x, probs = .75, na.rm = TRUE),
#                                                                         max = ~max(.x, na.rm = TRUE))))) |>
#   bind_rows(.id = "Run")
# rem18s <- rem18 |>
#   summarise(across(wait_death_days, .fns = list(min = ~min(.x, na.rm = TRUE),
#                                                     q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
#                                                     med = ~median(.x, na.rm = TRUE),
#                                                     q3 = ~quantile(.x, probs = .75, na.rm = TRUE),
#                                                     max = ~max(.x, na.rm = TRUE))))
#
#
# remx <- map_dbl(rems2, ~median(.x$wait_death_time))
# p0 <- ggplot()+
#   geom_line(aes(x = time, y = surv), color = "red", data = rem18d)+
#   # geom_line(aes(x = time, y = surv), color = "blue", data = rem16d)+
#   geom_line(aes(x = time, y = surv_med), data = remsv_med)+
#   geom_line(aes(x = time, y = surv), data = filter(remsv, Run == 1))+
#   coord_cartesian(xlim = c(0, 1000), ylim = c(.65,1))
# # #
# # # ggplot()+
# # #   geom_density(aes(x = wait_death_days), fill = "blue", alpha = 0.2, data = rem18)+
# # #   geom_density(aes(x = wait_death_time), fill = "red", alpha = 0.2, data = rems2[[1]])
# # #
# # # ggplot()+
# # #   geom_density(aes(x = days), fill = "blue", alpha = 0.2, data = rem18)+
# # #   geom_density(aes(x = listing_day), fill = "red", alpha = 0.2, data = rems2[[1]])
# # #
# # #
# # # ggplot()+
# # #   geom_density(aes(x = listing_day), fill = "blue", alpha = 0.2, data = q01)+
# # #   geom_density(aes(x = listing_day), fill = "red", alpha = 0.2, data = q3)
# #
# #
# # ## have to check on a few things
# # ## I want to know how many people were on the list at the end of this cohort
# # ## easiest thing is just to create the 2020-21 cohort
# # ## I'm going to go that
# # #
# end19 <- tsam18 %>%
#   filter(is.na(transplant_dt)) %>%
#   filter(is.na(removal_dt)) %>%
#   filter(death_dt > "2019-12-31"|is.na(death_dt)) %>%
#   filter(wait_death == 0) %>%
#   filter(censor_dt > "2019-12-31")
# # # #
# # # # #
# sv18 <- get_surv_data(km18)
# #
# # csv18 <- sv18 |>
# #   mutate(csn = cumsum(n.event))
# #
# # ##
# tsam16 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Cohort16_17.RDS") |>
#   mutate(disapp = case_when(
#     tx != 1 & wait_death != 1 & (censor_dt <= "2017-12-31"|death_remov_dt <="2017-12-31")  ~ 1,
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
#
# tsam16_dis2 <- left_join(tsam16, LU_ALLOC) %>%
#   mutate(wait_death_days = round(wait_death_wks*7, 0)) %>%
#   as.data.frame() %>% filter(wait_death_days>0)
#
# rem16 <- survfit(Surv(wait_death_days, disapp)~1, data = tsam16_dis2)
#
# rem16_s <- survfit(Surv(wait_death_days, disapp)~strata(canhx_grouping), data = tsam16_dis2)
#
# rem16d <- get_surv_data(rem16)
# rem16d_s <- get_surv_data(rem16_s) |>
#   rename(dx_grp = `strata(canhx_grouping)`)
#
# ggplot()+
#   geom_step(aes(x = time, y = surv, color = dx_grp), data = rem16d_s)+
#   geom_step(aes(x = time, y = surv, color = dx_grp), linetype = "dashed", data = filter(remsv_s, Run == 1))+
#   facet_wrap(~dx_grp)+
#   coord_cartesian(xlim = c(0, 1000), ylim = c(.7 ,1))
#
# ggplot()+
#   geom_step(aes(x = time, y = surv), data = rem16d)+
#   coord_cartesian(xlim = c(0, 1000), ylim = c(.7 ,1))
#
# #
# km16 <- survfit(Surv(wait_death_days, wait_death)~1, data = tsam16)
# sv16 <- get_surv_data(km16) %>%
#   complete(time = 1:max(time)) %>%
#   fill(surv, cumhaz, .direction = "down") %>%
#   mutate(dhaz = cumhaz - lag(cumhaz, 1, default = 0),
#          llsurv = log(-log(surv)))
#
# csv16 <- sv16 |>
#   mutate(csn = cumsum(n.event))
#
# # in1 <- map_dbl(1:300, ~sum(ct2[[1]]$wait_death_time < .x))
# in1 <- map_dfc(1:120, ~map_dbl(1:365, ~sum(ct2[[.y]]$wait_death_time < .x)/nrow(ct2[[.y]]), .y = .x)) |>
#   mutate(.id = row_number()-1) |>
#   pivot_longer(cols = -.id)
# in2 <- map_dbl(1:365, ~sum(tsam18$wait_death_days < .x))/nrow(tsam18)
# in3 <- map_dbl(1:365, ~sum(tsam16$wait_death_days < .x))/nrow(tsam16)
# #
# p1 <- ggplot()+
#   # geom_line(aes(x = .id, y = value, group = name), color = "blue")+
#   geom_smooth(aes(x = .id, y = value), color = "black", data = in1)+
#   geom_line(aes(x = 1:365, y = in2), color = "orange")+
#   geom_line(aes(x = 1:365, y = in3), color = "blue")
#
# tsam18_2 <- tsam18 |>
#   left_join(LU_ALLOC)
# # # #
#
# # ggplot()+
# #   geom_line(aes(x = time, y = csn), data = csv2)+
# #   geom_line(aes(x = time, y = csn), color = "red", data = csv18)+
# #   geom_line(aes(x = time, y = csn), color = "blue", data = csv16)+
# #   coord_cartesian(xlim = c(0, 50), ylim = c(0, 400))
# # +
#   coord_fixed()
# # #
# p2 <- ggplot()+
#   geom_step(aes(x = time, y = surv_med),linetype = "dashed", data = sv1_med)+
#   # geom_step(aes(x = time, y = surv_med),linetype = "dashed", color = "orange", data = sv0_med)+
#   # geom_step(aes(x= time, y = surv, group = Run), data = sv2)+
#   geom_step(aes(x = time, y = surv_med), linetype = "solid", data = sv2_med)+
#   geom_step(aes(x = time, y = surv), color = "red", linetype = "solid", data = sv18)+
#   # geom_step(aes(x = time, y = surv), color = "blue", linetype = "solid", data = sv16)+
# # coord_cartesian(xlim = c(0, 50), ylim = c(.9, 1))
#   coord_cartesian(xlim = c(0, 365), ylim = c(.7, 1))+
#   theme_embiggen()

# ggplot()+
#   geom_line(aes(x = time, y = n.risk), color ="red", data = sv18)+
#   geom_line(aes(x = time, y = n.risk_med), linetype = "solid", data = sv2_med)+
#   coord_cartesian(xlim = c(0, 365))
#
# # ## post-transplant
# #
# ptkm1 <- map(ct1, ~survfit(Surv(post_tx_time, post_tx_death)~1, data = .x))
# ptkm2 <- map(ct2, ~survfit(Surv(post_tx_time, post_tx_death)~1, data = .x))
# #
# ptsv1 <- map(ptkm1, get_surv_data) %>%
#   bind_rows(., .id = "Run") %>%
#   bind_rows(day0) %>%
#   arrange(Run, time) %>%
#   group_by(Run) %>%
#   complete(time = 0:max(time)) %>%
#   fill(surv, n.risk, .direction = "down") %>%
#   fill(n.risk, .direction = "up") %>%
#   ungroup()
#
# ptsv2 <- map(ptkm2, get_surv_data) %>%
#   bind_rows(., .id = "Run") %>%
#   bind_rows(day0) %>%
#   arrange(Run, time) %>%
#   group_by(Run) %>%
#   complete(time = 0:max(time)) %>%
#   fill(surv, n.risk, .direction = "down") %>%
#   fill(n.risk, .direction = "up") %>%
#   ungroup()
#
#
#
# ptsv1_med <- ptsv1 %>%
#   group_by(time) %>%
#   summarise(across(.cols = c(surv, n.risk), .fns = list(q1 = ~quantile(.x, probs = 0.25),
#                                                         med = ~median(.x, na.rm = TRUE),
#                                                         q3 = ~quantile(.x, probs = .75))))
#
# ptsv2_med <- ptsv2 %>%
#     group_by(time) %>%
#     summarise(across(.cols = c(surv, n.risk), .fns = list(q1 = ~quantile(.x, probs = 0.25),
#                                                           med = ~median(.x, na.rm = TRUE),
#                                                           q3 = ~quantile(.x, probs = .75))))
#
# ptkm18 <- survfit(Surv(ptx_time, ptx_death)~1, data = tsam18)
#
# ptsv18 <- get_surv_data(ptkm18)
#
# ggplot()+
#   geom_step(aes(x = time, y = surv_med), linetype = "dashed", data = ptsv1_med)+
#   geom_step(aes(x = time, y = surv_med), data = ptsv2_med)+
#   geom_step(aes(x = time, y = surv), color = "red", data = ptsv18)+
#   coord_cartesian(xlim = c(0, 400), ylim = c(.8, 1))+
#   labs(y = "Post-Transplant Survival", x = "Days")+
#   theme_embiggen()
#
#
#
#
#
# ##########
#
# ########
# sim_stat <- function(concat, group = NULL){
#   sim_stats <- concat |>
#     dplyr::group_by({{group}}) |>
#     dplyr::summarise(tx_count = sum(tx),
#                      wait_death = sum(wait_death, na.rm = TRUE),
#                      wait_death_yrs = sum(wait_death_time, na.rm = TRUE)/365.25,
#                      post_tx_death = sum(post_tx_death, na.rm = TRUE),
#                      post_tx_years = sum(post_tx_day, na.rm = TRUE)/365.25,
#                      txppy = tx_count/wait_death_yrs,
#                      wdppy = wait_death/wait_death_yrs,
#                      pdppy = post_tx_death/post_tx_years)
#
#   return(sim_stats)
# }
# ct3_ss <- map(ct3, sim_stat) |>
#   list_rbind(names_to = "Run")
#
# ct2_ss <- map(ct2, sim_stat) |>
#   list_rbind(names_to = "Run")
#
# ct2dx_ss <- map(ct2, sim_stat, group = dx_grp) |>
#   list_rbind(names_to = "Run")
#
# ct1_ss <- map(ct1, sim_stat) |>
#   list_rbind(names_to = "Run")
# #
# #
##
# c0 <- ct1[[8]]
# init0 <- nrow(filter(c0, listing_day <= 0))
#
# added0 <- c0 |>
#   filter(listing_day >0) |>
#   complete(listing_day = 1:max(listing_day)) |>
#   # mutate(listing_day = listing_day - 1) |>
#   summarise(Count = n(), .by = listing_day)
#
# tx0 <- c0 |>
#   filter(tx == 1) |>
#   summarise(Count = n(), .by = transplant_day) |>
#   arrange(transplant_day)
#
# rem0 <- c0 |>
#   filter(tx == 0 & !is.na(removal_day)) |>
#   summarise(Count = n(), .by = removal_day)|>
#   arrange(removal_day)
#
# wl_per0 <- c0 |>
#   filter(tx == 0 & !is.na(death_day)) |>
#   summarise(Count = n(), .by = death_day)|>
#   arrange(death_day)
#
# all_x0 <- full_join(added0, tx0, by = join_by(listing_day == transplant_day), suffix = c(".l", ".t")) |>
#   full_join(rem0, by = join_by(listing_day == removal_day)) |>
#   full_join(wl_per0, by = join_by(listing_day == death_day), suffix = c(".r", ".d")) |>
#   replace_na(list(Count.l = 0, Count.t = 0, Count.r = 0, Count.d = 0)) |>
#   arrange(listing_day) |>
#   mutate(ov = Count.l - Count.t - Count.r - Count.d) |>
#   pull(ov)
#
# dlc0 <- cumsum(c(init0, all_x0))
# # # #
# # # #
# # # # # # # # # #
# faster_daily_count <- function(COMET){
#
#   init <- nrow(filter(COMET, listing_day <= 0))
#
#   added <- COMET |>
#     filter(listing_day >0) |>
#     complete(listing_day = 1:max(listing_day)) |>
#     # mutate(listing_day = listing_day - 1) |>
#     summarise(Count = n(), .by = listing_day)
#
#   tx <- COMET |>
#     filter(tx == 1) |>
#     summarise(Count = n(), .by = transplant_day)
#
#   rem <- COMET |>
#     filter(tx == 0 & !is.na(removal_day)) |>
#     summarise(Count = n(), .by = removal_day)|>
#     arrange(removal_day)
#
#   wl_per <- COMET |>
#     filter(tx == 0 & !is.na(death_day)) |>
#     summarise(Count = n(), .by = death_day)|>
#     arrange(death_day)
#
#   all_x <- full_join(added, tx, by = join_by(listing_day == transplant_day), suffix = c(".l", ".t")) |>
#     full_join(rem, by = join_by(listing_day == removal_day)) |>
#     full_join(wl_per, by = join_by(listing_day == death_day), suffix = c(".r", ".d")) |>
#     replace_na(list(Count.l = 0, Count.t = 0, Count.r = 0, Count.d = 0)) |>
#     arrange(listing_day) |>
#     mutate(ov = Count.l - Count.t - Count.r - Count.d) |>
#     pull(ov)
#
#   dlc <- cumsum(c(init, all_x))
#
#   return(dlc)
# }
# # # # #
# # # dlc1z <- map_int(0:800, ~nrow(spec_day(rs1[[1]], .x)))
# # # dlc2 <- map_dbl(1:800, ~nrow(spec_day(rs2[[1]], .x)))
# # # # # #
# # # # # ct11 <- map(rs1, concat)
# # # # # dlc11 <- map(ct11, faster_daily_count)
# # # # # # ct22 <- map(rs2, concat)
# # # # # # dlc22 <- map(ct22, faster_daily_count)
# # # # # # # # # #
# # # # # dlc1 <- faster_daily_count(ct1[[1]])
# # # # # # dlc2 <- faster_daily_count(ct2[[1]])
# # # # # # #
# # # # # # # names(ct1) <- paste0("Run", 1:length(ct1))
# # # # # # #
# dlcs <- map(ct1, faster_daily_count) %>%
#   # map(~head(.x, 729)) %>%
#   bind_cols() %>%
#   mutate(Day = row_number() - 1) %>%
#   pivot_longer(-Day, names_to = "Run", names_transform = list(Run = ~str_extract(.x, "\\d+")),
#                values_to = "Count")
# # # # # # # #
# # # # # # # #
# dlcs2 <- map(ct2, faster_daily_count) %>%
#   map(~head(.x, 730)) %>%
#   bind_cols() %>%
#   mutate(Day = row_number() - 1) %>%
#   pivot_longer(-Day, names_to = "Run", names_transform = list(Run = ~str_extract(.x, "\\d+")),
#                values_to = "Count")
# # # # # # # # # #
# end_dayc <- filter(dlcs2, Day == 730) %>% pull(Count)
# # # # # # # #
# end19 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/Cohort20_21.RDS") %>%
#   filter(can_listing_dt <= "2019-12-31")
# # # # # # # #
# ggplot()+
#   geom_boxplot(aes(x = end_dayc, y = 0))+
#   geom_point(aes(x = nrow(end19), y = 0), shape = 17, size = 5, color = "red")+
#   labs(x = "Ending Cohort Count")+
#   theme_embiggen()
# ggplot()+
#   # geom_line(aes(x = Day, y= Count, group = Run),alpha = 0.2, data = dlcs)+
#   geom_line(aes(x = Day, y= Count, group = Run),alpha = 0.4, color = "black", data = dlcs2)+
#   geom_smooth(aes(x = Day, y= Count),alpha = 0.4, color = "red", data = dlcs2)+
#   geom_line(aes(x = Day, y= Count),alpha = 0.4, color = "lightblue", linewidth = 3, data = subset(dlcs2, Run == "7"))+
#   theme_embiggen()
# # # # # # #
# # # # # # # # dlc2 <- map_dbl(1:800, ~nrow(spec_day(rs1[[1]], .x)))
# # # # # # # #
# # # # # # # # dlc_df <- list_rbind(dlcs, names_to = "Run")
# tcnts <- map_dbl(ct2, nrow)
# txs1 <- map_dbl(ct1, ~sum(.x$tx))
# txs <- map_dbl(ct2, ~sum(.x$tx))
# wldeaths1 <- map_dbl(ct1, ~sum(.x$wait_death))
# wldeaths <- map_dbl(ct2, ~sum(.x$wait_death))
# remvls1 <- map_dbl(ct1, ~sum(.x$removal))
# remvls <- map_dbl(ct2, ~sum(.x$removal))
# ptdeaths1 <- map_dbl(ct1, ~sum(.x$post_tx_death, na.rm = TRUE))
# ptdeaths <- map_dbl(ct2, ~sum(.x$post_tx_death, na.rm = TRUE))
# # # # #
# # # # # remals <- map(ct2, ~.x %>% filter(removal == 1))
# # # # # #
# # # # # # #
# ggplot()+
#   geom_boxplot(aes(x = tcnts, y = 0))+
#   geom_point(aes(x = nrow(tsam18), y = 0), shape = 17, size = 5, color = "red")+
#   labs(x = "Total Candidates")
# # # #
# # # # # dsdc_orgs <- map_dbl(rs2, )
# # # # #
# ggplot()+
#   geom_boxplot(aes(x = txs, y = 0))+
#   geom_point(aes(x = sum(tsam18$tx), y = 0), shape = 17, size = 5, color = "red")+
#   labs(x = "Number of Transplants")
# # # # # # # # #
# ggplot()+
#   geom_boxplot(aes(x = wldeaths, y = 0))+
#   # geom_boxplot(aes(x = wldeaths1, y = 1))+
#   geom_point(aes(x = sum(tsam18$wait_death), y = 0),shape = 17, size = 5, color = "red")+
#   labs(x = "Waitlist Deaths")
# # # # # # # # # #
# ggplot()+
#   geom_boxplot(aes(x = remvls, y = 0))+
#   geom_boxplot(aes(x = remvls1, y = 1))+
#   geom_point(aes(x = sum(tsam18$disapp), y = 0),shape = 17, size = 5, color = "red")+
#   labs(x = "Waitlist Removals")
# # # # # #
# ggplot()+
#   geom_boxplot(aes(x = ptdeaths, y = 0))+
#   geom_point(aes(x = sum(tsam18$ptx_death), y = 0),shape = 17, size = 5, color = "red")+
#   labs(x = "Post Transplant Deaths")
# # # # #
# # rms <- map(ct1, ~filter(.x, removal == 1))
# # rms2 <- map(ct2, ~filter(.x, removal == 1))
# #
# # rmst <- map_dbl(rms, ~median(.x$wait_death_time))
# # rmst2 <- map_dbl(rms2, ~median(.x$wait_death_time))
# # #
# # # # tx1 <- map()
# # #
# # # ###### look at offer acceptance
# # # ctm1 <- list_rbind(map(rs1, pluck, "all_matches"), names_to = "Run") |>
# # #   tidyr::unnest(cols = c(data)) |>
# # #   select(Run, d_id, don_org, c_id, match_single, match_double, ov_rank, offer_rank, accept, pred)
# # #
# # #
# # #
# # ct1s <- list_rbind(ct1, names_to = "Run")
# # # ct1_need <- select(ct1s, Run, d_id, c_id, organs_rec)
# # # ct1_need1 <- filter(ct1_need, Run == 1, d_id %in% c(122, 139, 156))
# # #
# # # ct1_c <- left_join(ctm1, ct1_need)
# # #
# # # ct1_inv <- ct1_c |>
# # #   filter(Run == 1) |>
# # #   # filter(d_id %in% c(122, 139, 156)) |>
# # #   group_by(d_id) |>
# # #   mutate(organs_rec = if_else(is.na(organs_rec), 0, organs_rec),
# # #          org2 = cumsum(organs_rec))
# # #
# # # top2 <- filter(ct1_inv, org2 == 2) |> slice_min(offer_rank)
# # # ct1_inv2 <- ct1_inv |>
# # #   filter(org2 <2) |>
# # #   bind_rows(top2) |>
# # #   arrange(Run, d_id, offer_rank)
# # #
# # # ct1_inv2 |>
# # #   ungroup() |>
# # #   filter(offer_rank <= 10) |>
# # #   summarise(mp = mean(pred))
# # #
# # #
# # ct2s <- list_rbind(ct2, names_to = "Run")
# # # #
# # ctx1 <- ct1s %>%
# #   filter(!is.na(offer_rank)) %>%
# #   mutate(offer_rank2 = cut(offer_rank, breaks = c(seq(0, 100, 10), 10000)))
# # # #
# # ctx_min <- ct2s |>
# #   mutate(offer_rank2 = cut(offer_rank, breaks = c(seq(0, 100, 10), 10000))) |>
# #   filter(!is.na(offer_rank)) |>
# #   slice_min(offer_rank, by = c(Run, d_id))
# # #
# # #
# # ctx <- ct2s |>
# #   mutate(offer_rank2 = cut(offer_rank, breaks = c(seq(0, 100, 10), 10000))) |>
# #   filter(!is.na(offer_rank))
# # # # #
# # pac <- ggplot()+
# #   # geom_bar(aes(x = offer_rank2, y = after_stat(count)/sum(after_stat(count)), fill = offer_rank2), data = ctx)+
# #   geom_bar(aes(x = offer_rank2, y = after_stat(count)/sum(after_stat(count)), fill = offer_rank2), data = ctx_min)+
# #   labs(x = "Offer Rank", y = "Proportion")+
# #   scale_y_continuous(breaks = seq(0, .8, .1))+
# #   theme_embiggen()
# # #
# # ma0x <-  glm(accept ~ splines::bs(ptr_sequence_num, knots = seq(10,100,10))+as.factor(ctr_id_trunc) + don_dcd + smoke_hist + age_greater55, data = acceptance2, family = "binomial")
# #
# # ggplot()+
# #   geom_histogram(aes(x = offer_rank), binwidth = 10, data = ct2[[1]])
# #
