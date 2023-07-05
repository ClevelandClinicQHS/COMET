# #########
# ## comparison
# #########
# library(COMET)
# library(tidyverse)
#
#
# # rs1 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/300TSAM_LAS_Runs5.RDS")
# rs2 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9.RDS")
# rs3 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas.RDS")
# rs4 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas0eff.RDS")
# rs5 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas2eff.RDS")
#
# las <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9.RDS")
# cas <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas.RDS")
# cas0 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas0eff.RDS")
# cas2 <- readRDS("/proj/DaltonLab/projects/lungTx/p0054/data/1000TSAM_LAS_Runs_rec9_cas2eff.RDS")
#
# theme_embiggen <- function(){
#   theme_minimal() %+replace% theme(axis.text = element_text(size = 16),
#                                    strip.text = element_text(size = 16),
#                                    strip.background = element_rect(fill = "grey80"))
# }
#
#
# # r5 <- rs5[[1]]
# # r2 <- rs2[[1]]
# # r5c <- cutoff(r5, 730,730, 802)
# # r2c <- cutoff(r2, 730,730, 802)
# #
# # r5ev <- map(1:730, ~filter(r5c, wait_death_day == .x) |> select(c_id, wait_death:time_to_transplant, tx))
# # r2ev <- map(1:730, ~filter(r2c, wait_death_day == .x) |> select(c_id, wait_death:time_to_transplant, tx))
# #
# # # r4 <- cutoff(rs4[[1]], 730, 730, 802)
# #
# # sp_cas <- map(0:729, ~spec_day(r5, .x), .progress= TRUE)
# # sp_las <- map(0:729, ~spec_day(r2, .x), .progress= TRUE)
# #
# # dl_cas <- map(sp_cas, calculate_sub_cas, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = .25, post_tx_weight = .25, bio_weight = .15, peds_weight = .2, pld_weight = .1, wl_cap = 365,
# #     post_tx_cap = 1825, .progress = TRUE)
# #
# # dl_las <- map(sp_las, ~(calculate_las(.x, wl_model = "LAS15", post_tx_model = "LAS15", wl_weight = 2, post_tx_weight = 1, wl_cap = 365,
# #               post_tx_cap =365) |> mutate(ov_rank = rank(-lu_score))), .progress = TRUE)
# #
# # dl_xlas <- map2(r2ev, dl_las, ~.y |> inner_join(.x, by = "c_id")) |> list_rbind()
# # dl_xcas <- map2(r5ev, dl_cas, ~.y |> inner_join(.x, by = "c_id")) |> list_rbind()
# #
# # dlxlas_s <- dl_xlas |>
# #   summarise(across(c(wait_death_time, ov_rank),min), .by = c(wait_death, removal, tx, wait_death_day)) |>
# #   mutate(event = case_when(
# #     wait_death == 1 ~ "Waitlist_death",
# #     removal == 1 ~ "Removal",
# #     tx == 1 ~ "Transplant",
# #     .default = "ZZ"
# #   )) |>
# #   mutate(mod = "LAS")
# #
# # dlxcas_s <- dl_xcas |>
# #   summarise(across(c(wait_death_time, ov_rank),min), .by = c(wait_death, removal, tx, wait_death_day)) |>
# #   mutate(event = case_when(
# #     wait_death == 1 ~ "Waitlist_death",
# #     removal == 1 ~ "Removal",
# #     tx == 1 ~ "Transplant",
# #     .default = "ZZ"
# #   ))|>
# #   mutate(mod = "CAS")
# #
# # allx <- bind_rows(dlxcas_s, dlxlas_s)
# #
# # ggplot()+
# #   geom_smooth(aes(x = wait_death_day, y = ov_rank, color = event, linetype = mod), data = allx)+
# #   theme_embiggen()
#
#
#
# ## Look at number of transplants, waitlist deaths, removals,
# ## ALl of those by group
# ## For those transplanted, overall rank, and offer rank, distance traveled, time to transplant
#
# # ov5 <- ct5 |>
# #   summarise(across(c(tx, wait_death, removal), sum))
# #
# # ov5g <- ct5 |>
# #   summarise(across(c(tx, wait_death, removal), sum), .by = dx_grp) |>
# #   arrange(dx_grp)
# #
# # ov5x <- ct5 |>
# #   summarise(across(wait_death_time, .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
# #                                                 q3 = ~quantile(.x, probs = .75), mean = mean, max = max)), .by = c(tx, wait_death, removal)) |>
# #   tidyr::pivot_longer(cols = -c(tx, wait_death, removal)) |>
# #   dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
# #                 stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused")
# #
# # tx5 <- filter(ct5, tx == 1) |>
# #   summarise(across(c(ov_rank, offer_rank, distance_nm, wait_death_time), .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
# #                                                                                      q3 = ~quantile(.x, probs = .75), mean = mean, max = max))) |>
# #       tidyr::pivot_longer(cols = dplyr::everything()) %>%
# #       dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
# #              stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused")
# #
# # tx5g <- filter(ct5, tx == 1) |>
# #   summarise(across(c(ov_rank, offer_rank, distance_nm, wait_death_time), .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
# #                                                                                      q3 = ~quantile(.x, probs = .75), mean = mean, max = max)),
# #               .by = dx_grp) |>
# #   tidyr::pivot_longer(cols = -dx_grp) %>%
# #   dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
# #                 stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused")
#
#
# tf1 <- function(COMET, .by  = NULL){
#   ov_stat <- COMET |>
#     summarise(across(c(tx, wait_death, removal), sum), .by = {{.by}}) |>
#     arrange({{.by}})
#   return(ov_stat)
# }
#
# tf2 <- function(COMET){
#   ov_stat2 <- COMET |>
#     summarise(across(wait_death_time, .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
#                                                   q3 = ~quantile(.x, probs = .75), mean = mean, max = max)), .by = c(tx, wait_death, removal)) |>
#     mutate(event = case_when(
#       tx == 1 ~ "tx",
#       wait_death == 1 ~ "wld",
#       removal == 1 ~ "removal",
#       .default = "cens"
#     ), .before = everything()) |>
#     tidyr::pivot_longer(cols = -c(tx, wait_death, removal, event)) |>
#     dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
#                   stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused") |>
#     select(var, stat, event, value)
#   return(ov_stat2)
# }
#
# tf2g <- function(COMET){
#   ov_stat2 <- COMET |>
#     summarise(across(wait_death_time, .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
#                                                   q3 = ~quantile(.x, probs = .75), mean = mean, max = max)), .by = c(tx, wait_death, removal, dx_grp)) |>
#     mutate(event = case_when(
#       tx == 1 ~ "tx",
#       wait_death == 1 ~ "wld",
#       removal == 1 ~ "removal",
#       .default = "cens"
#     ), .before = everything()) |>
#     tidyr::pivot_longer(cols = -c(tx, wait_death, removal, event, dx_grp)) |>
#     dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
#                   stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused") |>
#     select(var, stat, event, value, dx_grp)
#   return(ov_stat2)
# }
#
# tf3 <- function(COMET, .by = NULL){
#   if(all(colnames(COMET) != "distance_nm")){
#     COMET <- left_join(COMET, dist_data, by = c("center" = "can_center", "hospital" = "don_hosp"))
#   }
#
#   tx_stat <- COMET |>
#     filter(tx ==1) |>
#     summarise(across(c(ov_rank, offer_rank, distance_nm, wait_death_time), .fns = list(min = min, q1 = ~quantile(.x, probs = .25), median = median,
#                                                                                        q3 = ~quantile(.x, probs = .75), mean = mean, max = max)),
#               .by = {{.by}}) |>
#     tidyr::pivot_longer(cols = -{{.by}}) %>%
#     dplyr::mutate(var = stringr::str_extract(name ,".+?(?=\\_[^\\_]*$)"),
#                   stat = stringr::str_remove(stringr::str_extract(name ,"\\_[^\\_]*$"), "\\_"), .before = dplyr::everything(), .keep = "unused")
#   return(tx_stat)
# }
#
# tf4 <- function(COMET, .by = NULL){
#   sim_stats <- COMET |>
#     dplyr::group_by({{.by}}) |>
#     dplyr::summarise(tx_count = sum(tx),
#                      wait_death = sum(wait_death, na.rm = TRUE),
#                      removal = sum(removal, na.rm = TRUE),
#                      wait_death_yrs = sum(wait_death_time, na.rm = TRUE)/365.25,
#                      post_tx_death = sum(post_tx_death, na.rm = TRUE),
#                      post_tx_years = sum(post_tx_time, na.rm = TRUE)/365.25,
#                      txppy = tx_count/wait_death_yrs,
#                      wdppy = wait_death/wait_death_yrs,
#                      pdppy = post_tx_death/post_tx_years)
#
#   return(sim_stats)
# }
#
#
# # ct1 <- map(rs1, cutoff, 730, 730, 802)
# ct2 <- map(rs2, cutoff, 730, 730, 802)
# ct3 <- map(rs3, cutoff, 730, 730, 802)
# ct4 <- map(rs4, cutoff, 730, 730, 802)
# ct5 <- map(rs5, cutoff, 730, 730, 802)
#
# # ct1_d <- map2(ct1, rs1, ~left_join(.x, select(.y$all_donors, d_id, hospital), by = join_by(d_id)))
# # ct2_d <- map2(ct2, rs2, ~left_join(.x, select(.y$all_donors, d_id, hospital), by = join_by(d_id)))
#
# # or_st <- map(ct1_d, tf3) |>
#   # list_rbind(names_to = "Seed") |>
#   # mutate(sim = "bLAS")
# rec_st <- map(ct2, tf3) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# rec_cas <- map(ct3, tf3) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# rec_cas0 <- map(ct4, tf3) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# rec_cas2 <- map(ct5, tf3) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
# all_sim_s <- bind_rows(#or_st,
#                        rec_st, rec_cas, rec_cas0, rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   filter(!(stat %in% c("min", "max"))) |>
#   filter(sim != "UN LAS")|>
#   mutate(var = factor(var, levels = c("distance_nm", "offer_rank", "ov_rank", "wait_death_time"), labels = c("Nautical Miles", "Offer Rank", "Overall Rank", "Transplant Time")))
#
#
# pal2 <- RColorBrewer::brewer.pal(4, "Set2")
#
# # ggplot()+
# #   geom_boxplot(aes(x = var, y = value), data = subset(rec_cas2, stat == "median"))+
# #   facet_wrap(~var, scales = "free")+
# #   scale_fill_discrete(type = pal2)+
# #   theme_embiggen()
#
# ggplot()+
#   geom_boxplot(aes(x = sim, y = value, fill = sim), data = filter(all_sim_s, stat == "median"))+
#   facet_wrap(~var, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   theme_embiggen()
#
# tb3 <- all_sim_s |>
#   filter(stat == "median") |>
#   select(-stat) |>
#   pivot_wider(id_cols = c(Seed, sim), names_from = "var") |>
#   mutate(sim = fct_drop(sim)) |>
#   tbl_summary(by = sim, include = -c(Seed, `Offer Rank`))
# ##
#
# # dx_or_st <- map(ct1_d, tf3, .by = dx_grp) |>
# #   list_rbind(names_to = "Seed") |>
# #   mutate(sim = "bLAS")
# dx_rec_st <- map(ct2, tf3, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# dx_rec_cas <- map(ct3, tf3, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# dx_rec_cas0 <- map(ct4, tf3, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# dx_rec_cas2 <- map(ct5, tf3, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
# dx_all_sim_s <- bind_rows(#dx_or_st,
#                           dx_rec_st, dx_rec_cas, dx_rec_cas0, dx_rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   filter(!(stat %in% c("min", "max"))) |>
#   filter(sim != "UN LAS") |>
#   mutate(var = factor(var, levels = c("distance_nm", "offer_rank", "ov_rank", "wait_death_time"), labels = c("Nautical Miles", "Offer Rank", "Overall Rank", "Transplant Time")))
#
# ggplot()+
#   geom_boxplot(aes(x = sim, y = value, group = dx_grp, fill = dx_grp), data = filter(dx_all_sim_s, stat == "median"))+
#   facet_wrap(~var, scales = "free")
#
# ggplot()+
#   geom_boxplot(aes(x = dx_grp, y = value, fill = sim), data = filter(dx_all_sim_s, stat == "median"))+
#   facet_wrap(~var, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   theme_embiggen()
#
# ov_st1 <- map(ct2, tf1) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# ov_rec_cas <- map(ct3, tf1) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# ov_rec_cas0 <- map(ct4, tf1) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# ov_rec_cas2 <- map(ct5, tf1) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
# ov_all_st <- bind_rows(ov_st1, ov_rec_cas, ov_rec_cas0, ov_rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   # filter(!(stat %in% c("min", "max"))) |>
#   filter(sim != "UN LAS") |>
#   pivot_longer(cols = tx:removal)
#
# tb1 <
#
#
#
# ggplot()+
#   geom_boxplot(aes(x = sim, y = value, fill =sim), data = ov_all_st)+
#   facet_wrap(~name, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   theme_embiggen()
#
#
# ##
# dx_ov_st1 <- map(ct2, tf1, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# dx_ov_rec_cas <- map(ct3, tf1, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# dx_ov_rec_cas0 <- map(ct4, tf1, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# dx_ov_rec_cas2 <- map(ct5, tf1, .by = dx_grp) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
# dx_ov_all_st <- bind_rows(dx_ov_st1, dx_ov_rec_cas, dx_ov_rec_cas0, dx_ov_rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   filter(sim != "UN LAS") |>
#   pivot_longer(cols = tx:removal) |>
#   mutate(name = factor(name, levels = "reva"))
#
# ggplot()+
#   geom_boxplot(aes(x = dx_grp, y = value, fill = sim), data = dx_ov_all_st)+
#   facet_wrap(~name, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   theme_embiggen()
#
# ev_st1 <- map(ct2_d, tf2) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# ev_rec_cas <- map(ct3, tf2) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# ev_rec_cas0 <- map(ct4, tf2) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# ev_rec_cas2 <- map(ct5, tf2) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
#
# ev_all_st <- bind_rows(ev_st1, ev_rec_cas, ev_rec_cas0, ev_rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   # filter(!(stat %in% c("min", "max"))) |>
#   filter(sim != "UN LAS") |>
#   filter(!(stat %in% c(min, max))) |>
#   mutate(event = factor(event, levels = c("cens", "removal", "tx", "wld"), labels = c("Censor", "Removal", "Transplant", "Waitlist Death")))
#
# ggplot()+
#   geom_boxplot(aes(x = sim, y = value, fill = sim), data = filter(ev_all_st, stat == "median"))+
#   facet_wrap(~event, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   labs(y = "Time to Event")+
#   theme_embiggen()
#
# dx_ev_st1 <- map(ct2_d, tf2g) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# dx_ev_rec_cas <- map(ct3, tf2g) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# dx_ev_rec_cas0 <- map(ct4, tf2g) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# dx_ev_rec_cas2 <- map(ct5, tf2g) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
#
# dx_ev_all_st <- bind_rows(dx_ev_st1, dx_ev_rec_cas, dx_ev_rec_cas0, dx_ev_rec_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE)) |>
#   # filter(!(stat %in% c("min", "max"))) |>
#   filter(sim != "UN LAS") |>
#   filter(!(stat %in% c(min, max))) |>
#   mutate(event = factor(event, levels = c("cens", "removal", "tx", "wld"), labels = c("Censor", "Removal", "Transplant", "Waitlist Death")))
#
# ggplot()+
#   geom_boxplot(aes(x = dx_grp, y = value, fill = sim), data = filter(dx_ev_all_st, stat == "median"))+
#   facet_wrap(~event, scales = "free")+
#   scale_fill_discrete(type = pal2)+
#   labs(y = "Time to Event")+
#   theme_embiggen()
#
# ##########
#
# ss_las <- map(ct2, tf4) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "LAS")
# ss_cas <- map(ct3, tf4) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff1")
# ss_cas0 <- map(ct4, tf4) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff0")
# ss_cas2 <- map(ct5, tf4) |>
#   list_rbind(names_to = "Seed") |>
#   mutate(sim = "CASeff2")
#
# ss_all_st <- bind_rows(#dx_ev_st1,
#   ss_las, ss_cas, ss_cas0, ss_cas2) |>
#   mutate(sim = factor(sim, levels = c("bLAS", "LAS", "CASeff1", "CASeff0", "CASeff2"),
#                       labels = c("UN LAS", "LAS", "CAS", "CAS 0eff", "CAS eff*2"), ordered = TRUE))
#
# ######
# ## Tsam cohort
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
#     tx != 1 & wait_death != 1 & censor_dt <= "2019-12-31" ~ 1,
#     .default = 0
#   )) |>
#   mutate(p18 = ifelse(can_listing_dt < "2018-01-01", 1, 0)) |>
#   left_join(LU_ALLOC)
#
# ss_18 <- tsam18 |>
#   summarise(tx = sum(tx),
#             wait_death = sum(wait_death),
#             removal = sum(disapp),
#             wait_death_yrs = sum(wait_death_days, na.rm = TRUE)/365.25,
#             post_tx_death = sum(ptx_death, na.rm = TRUE),
#             post_tx_years = sum(ptx_time, na.rm = TRUE)/365.25,
#             txppy = tx/wait_death_yrs,
#             wdppy = wait_death/wait_death_yrs,
#             pdppy = post_tx_death/post_tx_years
#             )
#
#
#
#
#
