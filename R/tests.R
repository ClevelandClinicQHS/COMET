# ## tests
#
# t_don <- tibble(d_id = 1:4, hospital = 1:4, recovery_day = 1, male = rep(c(0,1), 2),
#                 race_eth = factor(c("NH White", "NH Black", "Hispanic", "Hawaiian"), levels = c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")),
#                 hgt_in = c(60, 78, 68, 62), hgt_cm = c(152.40, 198.12, 172.72, 157.48), cod = factor(1:4, levels = c(1:4, 999)),
#                 abo = factor(c("O", "AB", "A", "B"), levels = c("A", "B", "AB", "O")), age = seq(18,33, 5),
#                 don_org = factor(c("DLU", "DLU", "LUL", "LUR"), levels = c("DLU", "LUL", "LUR")),
#                 smoke_hist = c(0,0,1,0), gt_20pkyr = c("N","N","Y","N"), don_dcd = c(0,0,0,1), don_util = 1
#                 )
#
# t_can <- tibble(c_id = 1:4, center = factor(1:4, levels = 1:61), listing_day = 1, male = rep(c(0,1),2), dx_grp = LETTERS[1:4],
#                 race_eth = factor(c("NH White", "NH Black", "Hispanic", "Hawaiian"), levels = c("NH White", "NH Black", "Hispanic", "Asian", "Amer. Ind", "Hawaiian", "Multi/Other")),
#                 age_at_listing = seq(60,63,1), airway = seq(-.1,0.2, .1),
#                 oxygen = seq(-.1,0.2, .1), abo = c("O", "AB", "A", "B"), hgt_in = c(65,70,60,68.5), hgt_cm = c(165.10, 177.80, 152.40, 173.99),
#                 wgt_kg = seq(60,76,5), bmi = c(22.01188, 20.56127, 30.13895, 24.77494), resp_supp = factor(c(1,2,3,4), levels = 1:4),
#                 surg_type = factor(c("S", "E", "D", "E"), levels = c("D", "E", "S")), diab = rep(c(0,1),2), fev1 = seq(35,50,5), fvc = seq(50, 35, -5), pco2 = seq(39, 50, 3),
#                 pf = seq(225, 300, 25), po2 = seq(55, 80, 7), pap_mean = seq(23, 30, 2), o2 = c(2L,0L,3L,15L),
#                 o2_freq = c(4,3,2,1), vent = factor(c("BiPAP","None","CPAP","Mechanical"), levels = c("BiPap", "Mechanical", "None", "CPAP")),
#                 walk6m = seq(1000,250,-250), bili = c(.3, .7, .5, .2), creat = seq(0.6,.9,.1), pap_syst = seq(30,45,5), ci = seq(2.1,2.4,.1),
#                 funstat = c(1,2,2,3), ecmo = c(0,0,0,1), cvp = seq(3,6,1), pco2_15 = c(0,0,0,1), pra = 0, pld = 0, creat_150  = 0, bili_50 = 0,
#                 age = seq(60, 63, 1), hgt_cm1 = c(165,178, 152, 174), wgt = seq(60,76,5), o2rest = c(0, 0, 0, 15), cont_mech = c(0,0,0,1),
#                 dx = c(-99, -99, -99, 150), pabo = c(0.500181752090149, 0, 0.13958560523446, 0.382769901853871),
#                 phgt = c(0.080943738656987, 0.247912885662432, 0.337205081669691, 0.067876588021779), pcpra = 0L
#                 )
#
# ## Check this
# mz <- match_cas(canwl = t_can, don_avl = t_don, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25,
#                 post_tx_weight = 0.25,wl_cap = 365,post_tx_cap = 1826,bio_weight = 0.15, peds_weight = 0.2,
#                 pld_weight = 0.05, efficiency_weight = .1)
#
# mla <- match_las(canwl = t_can, don_avl = t_don, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25,
#                 post_tx_weight = 0.25,wl_cap = 365,post_tx_cap = 1826)
#
# ##
# calc_wl_las15(t_can)
# calc_wl_las21(t_can)
# wl15 <- tibble(c_id = 1:4, lp_wl = c(-0.044153147811174076054, 2.902302442847061847431, 2.414470767289233599229, 7.119497767640454277682))
# wl21 <- tibble(c_id = 1:4, lp_wl = c(0.66233093189656189193, 3.26593146213539275990, 4.16897933857661939783, 7.25573635852797504953))
# wl23 <- tibble(c_id = 1:4, lp_wl = c(0.66233093189656189193, 3.26593146213539275990, 4.16897933857661939783, 7.25573635852797504953))
# pt15 <- tibble(c_id = 1:4, lp_post_tx = c(0.23359530425982746871, 1.06877235499154332743, 0.85353274744930929252, 1.84444751129893869113))
# pt21 <- tibble(c_id = 1:4, lp_post_tx = c(0.50492839977061199086, 1.11327564879164753187, 0.92667475752216321716, 1.38716043501698327312))
# pt23 <- tibble(c_id = 1:4, lp_post_tx = c(0.24646650673565495548, 0.44170824300000000040, 0.35552156833899795085, 1.01870541196277253349))
#
# ## las 15
# calculate_las(t_can, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
# las15 <-tibble(c_id = 1:4, lu_score = c(0.32623802801370410664, 0.42579888762015338788, 0.38758107695252935754, 0.91192239400490238133))
# ## las 21
# las21 <- tibble(c_id = 1:4, lu_score = c(0.32217965660850805820, 0.35548317785022059967, 0.42378180681952992703, 0.90166253644412319712))
# ## 1 to 1
# las21_2 <- tibble(c_id = 1:4, lu_score = c(0.48042626506142815934, 0.49647223284204572424, 0.55098523374398244989, 0.90016299959664014541))
#
# ## las 23
#
# ## cas23
#

# height_screen(t_can, t_don)

# c_id  d_id match_single match_double
# <int> <int> <lgl>        <lgl>
#   1     1     3 TRUE         TRUE
# 2     2     3 TRUE         TRUE
# 3     3     1 TRUE         TRUE
# 4     3     4 TRUE         TRUE
# 5     4     3 TRUE         TRUE
