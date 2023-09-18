
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COMET

<!-- badges: start -->
<!-- badges: end -->

The goal of COMET is to simulate provide an open source framework to
simulate the United States Lung Allocation System

## Installation

You can install the development version of COMET like so:

``` r
# install.packages("devtools")
devtools::install_github("ClevelandClinicQHS/COMET")
```

## Basic example

This will simulate the 2015 Lung Allocation Score (LAS) policy for 400
days (`r1`) and then the same scenario under the Composite Allocaiton
Score (CAS) policy (`r2`).

``` r
library(COMET)

r1 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.091253 mins

r2 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.657694 mins
```

A simple way to evaluate the COMET runs, one can also look at the
statistics by diagnosis group or another variable. For details on what
is reported run `help('eval_simulation')`.

``` r
# see how many were transplanted, donor count, waitlist deaths
eval_simulation(r1)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4248     2761        275     132 1080       2434.661           201
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1      1444.019 1.134039 0.1129521 0.1391948  200.713 0.08801159      0      80
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     187         6      2884     2718          127     688

eval_simulation(r2)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4248     2756        270     149 1073       2442.768           178
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1      1452.402 1.128228 0.1105304 0.1225556 379.4788 0.08744557      0      74
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     189         7      2884     2718           96     652

## See results by diagnosis group
eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1207      844         31      39  293      1142.9733
#> 2      B       258      122         24      23   89       191.3676
#> 3      C       350      221         21       9   99       149.8453
#> 4      D      2433     1574        199      61  599       950.4750
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy   ptd_ppy med_dist
#> 1            59      461.2786 0.7384249 0.02712224 0.1279054 222.5336
#> 2             9       64.2026 0.6375166 0.12541311 0.1401812 155.5433
#> 3            13      112.0164 1.4748543 0.14014453 0.1160544 199.6450
#> 4           120      806.5216 1.6560141 0.20936900 0.1487871 193.0029
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.09478673      0     137   200.5        13      2884     2718          127
#> 2 0.10655738      0     103   194.5         4      2884     2718          127
#> 3 0.05429864      0      83   184.0         6      2884     2718          127
#> 4 0.08767471      0      62   179.0         4      2884     2718          127
#>   nu_orgs
#> 1     688
#> 2     688
#> 3     688
#> 4     688
eval_simulation(r2, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1207      898         28      31  250      1105.8042
#> 2      B       258      125         22      23   88       196.3833
#> 3      C       350      271          9       6   64       124.9856
#> 4      D      2433     1462        211      89  671      1015.5948
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy    ptd_ppy med_dist
#> 1            52     510.65845 0.8120786 0.02532094 0.10182931 378.9343
#> 2            13      63.34018 0.6365103 0.11202582 0.20524098 349.3513
#> 3            14     143.75633 2.1682493 0.07200828 0.09738702 379.4836
#> 4            99     734.64750 1.4395505 0.20776002 0.13475851 384.5771
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.10356347      0     113   214.5        11      2884     2718           96
#> 2 0.07200000      0     110   184.0         7      2884     2718           96
#> 3 0.11808118      0      39   187.0         3      2884     2718           96
#> 4 0.07318741      0      63   177.5         6      2884     2718           96
#>   nu_orgs
#> 1     652
#> 2     652
#> 3     652
#> 4     652
```

One can also mix and match the models and weights under the LAS/CAS
policies. `r3` simulates the but the LAS policy but with the CAS
waitlist and post-transplant mortality models (post-transplant survival
truncated at 1 year). `r4` is the same scenario as `r3`, but waitlist
mortality receives the same weight as post-transplant (1 to 1).

``` r
r3 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.276705 mins

r4 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.338412 mins
```

## A more indepth demonstration to show what is going on behind the scenes

### Generates candidates and donors

One major advantage of the simulation is the use of synthetic candidates
and donors.

``` r
set.seed(26638)
syn_cands <- gen_and_spawn_candidates(days = 10)
syn_dons <- gen_and_spawn_donors(days = 10)
head(syn_cands)
#> # A tibble: 6 × 47
#>    c_id center listing_day  male dx_grp race_eth age_at_listing airway  oxygen
#>   <int> <fct>        <dbl> <int> <chr>  <fct>             <dbl>  <dbl>   <dbl>
#> 1     1 7                1     0 B      NH White           64.3  1.70   0.0632
#> 2     2 8                1     0 D      NH Black           66.8  1.27  -0.402 
#> 3     3 15               1     1 D      NH White           67.6 -0.158  0.637 
#> 4     4 33               1     1 D      NH White           45.1  0.430  0.688 
#> 5     5 42               1     0 A      NH White           55.7 -0.782  0.112 
#> 6     6 18               2     1 D      Asian              78.2  0.552  0.327 
#> # ℹ 38 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>,
#> #   age <dbl>, hgt_cm1 <dbl>, o2rest <dbl>, cont_mech <dbl>, dx <dbl>, …
head(syn_dons)
#> # A tibble: 6 × 16
#>    d_id hospital recovery_day  male race_eth hgt_in hgt_cm cod   abo     age
#>   <int>    <dbl>        <int> <int> <fct>     <dbl>  <dbl> <fct> <fct> <dbl>
#> 1     1     1394            1     0 NH White   59     150. 2     A        48
#> 2     2      385            1     1 NH White   73.5   187. 3     B        51
#> 3     3     1102            2     0 NH White   67.5   171. 1     O        28
#> 4     4     1180            2     0 Hawaiian   61.5   156. 3     O        29
#> 5     5     1392            2     1 NH White   66     168. 2     A        59
#> 6     6      162            2     0 NH White   66     168. 3     A        46
#> # ℹ 6 more variables: don_org <fct>, smoke_hist <int>, gt_20pkyr <chr>,
#> #   don_dcd <int>, don_util <int>, organs_avl <dbl>
```

For this scenario I’m going to add 400 days to the listing and recovery
day to `syn_cands` and `syn_donors` and also make sure the candidate and
donor ids do not conflict. I’m going to continue the results from CAS
simulation (`r2`). The notation for the LAS simulation (`r1`) is very
similar.

``` r
syn_cands <- dplyr::mutate(syn_cands,
                           listing_day = listing_day + max(r2$all_candidates$listing_day),
                           c_id = c_id + max(r2$all_candidates$c_id))

syn_dons <- dplyr::mutate(syn_dons,
                          recovery_day = recovery_day + max(r2$all_candidates$listing_day),
                          d_id = d_id + max(r2$all_donors$d_id))

r2cc <- dplyr::mutate(r2$current_candidates,
                      days_on_waitlist = 401 - listing_day)

r2rd <- dplyr::mutate(r2$recipient_database,
                      days_after_tx = 401 - transplant_day)

l <- list("current_candidates" = r2cc,
          "recipient_database" = r2$recipient_database,
          "waitlist_death_database" = r2$waitlist_death_database,
          "post_tx_death_database" = r2$post_tx_death_database,
          "non_used_donors" = r2$non_used_donors)

l_1 <- iteration(401, syn_cands, syn_dons, include_matches = FALSE, updated_list = l, match_alg = match_cas,
                 wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
                 wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

l_2 <- iteration(402, syn_cands, syn_dons, include_matches = FALSE, updated_list = l_1, match_alg = match_cas,
                 wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
                 wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

nrow(l_2$waitlist_death_database) - nrow(l_1$waitlist_death_database)
#> [1] 3
nrow(l_2$post_tx_death_database)-nrow(l_1$post_tx_death_database)
#> [1] 0
```

Looking in depth, 3 died on the waiting list, and 0 died post transplant
on Day 402 in this scenario. The following would run ten days
sequentially

``` r
l_i <- l
for(i in 401:410){
  l_i <- iteration(i,
                   syn_cands, syn_dons, include_matches = FALSE, updated_list = l_i,
                   match_alg = match_cas,  wl_model = "CAS23", post_tx_model = "CAS23",
                   wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                   bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1)
  
}
```

### Within each day

#### Updating candidates/recipients

`update_patients` is one of the steps that occur each day, it will
output a list of those that died, were removed for other reasons if on
the waiting list, those that are still alive and updated
characteristics. Currently, the only characteristic that is updated is
the candidate’s/recipient’s age, but it is made to be flexible to adjust
any clinical characteristic (i.e. $\textrm{pCO}_2$,$\textrm{FEV}_1$,
bilirubin, etc.)

Within `update_patients` are

- `identify_deaths` returns a dataset of those who died either on the
  waiting list or post-transplant[^1]

- `identify_removals` returns a dataset of those who were removed on the
  waiting list[^2]

``` r
update_patients(patients = r2cc, model = "CAS23r", 
                elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 401)
#> $Dead
#> # A tibble: 0 × 10
#> # ℹ 10 variables: c_id <dbl>, lp <dbl>, days_on_waitlist <dbl>, dx_grp <chr>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   death <int>, max_days <dbl>
#> 
#> $Alive
#> # A tibble: 1,073 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   305  1.24              364 A           363      0.995      0.995  1.00 
#>  2   315  3.15              362 D           361      0.995      0.995  1.00 
#>  3   321  1.73              361 A           360      0.995      0.995  1.00 
#>  4   326  4.13              360 D           359      0.995      0.995  0.999
#>  5   332  3.70              360 D           359      0.995      0.995  0.999
#>  6   333  3.64              360 B           359      0.995      0.995  0.999
#>  7   336  4.63              359 D           358      0.995      0.995  0.998
#>  8   347  4.10              357 D           356      0.995      0.995  0.999
#>  9   353  1.55              356 A           355      0.995      0.995  1.00 
#> 10   364  2.60              355 A           354      0.995      0.995  1.00 
#> # ℹ 1,063 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>
#> 
#> $Removed
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <dbl>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>
#> 
#> $new_char
#> # A tibble: 1,073 × 48
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing  airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>   <dbl>   <dbl>
#>  1 B       -999       -4605 7          0 NH White           53.5  0.233   0.679 
#>  2 A       -998       -3842 21         0 NH White           65.7 -0.347   0.0699
#>  3 A       -997       -3618 55         1 Asian              56.3 -0.0723 -0.241 
#>  4 A       -993       -3052 18         1 NH White           72.1 -1.15   -0.0170
#>  5 A       -990       -2703 42         0 NH White           63.0  1.34    0.184 
#>  6 A       -989       -2697 33         0 NH White           64.1  0.0716 -0.536 
#>  7 A       -987       -2492 8          1 Hispanic           63.9 -0.501   0.0506
#>  8 A       -977       -2127 1          0 NH White           67.4 -0.220   0.0270
#>  9 D       -959       -1819 33         1 NH White           52.8  0.489   0.155 
#> 10 D       -956       -1782 13         1 NH White           75.3  1.53    1.06  
#> # ℹ 1,063 more rows
#> # ℹ 39 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …

identify_deaths(patients = r2cc, model = "CAS23r",
                elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 401)
#> # A tibble: 1,073 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   305  1.24              364 A           363      0.995      0.995  1.00 
#>  2   315  3.15              362 D           361      0.995      0.995  1.00 
#>  3   321  1.73              361 A           360      0.995      0.995  1.00 
#>  4   326  4.13              360 D           359      0.995      0.995  0.999
#>  5   332  3.70              360 D           359      0.995      0.995  0.999
#>  6   333  3.64              360 B           359      0.995      0.995  0.999
#>  7   336  4.63              359 D           358      0.995      0.995  0.998
#>  8   347  4.10              357 D           356      0.995      0.995  0.999
#>  9   353  1.55              356 A           355      0.995      0.995  1.00 
#> 10   364  2.60              355 A           354      0.995      0.995  1.00 
#> # ℹ 1,063 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>

identify_removals(patients = r2cc, elapsed_time = days_on_waitlist, cap = 2365)
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <dbl>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>

## post-transplant deaths
update_patients(r2rd, model = "CAS23r", 
                elapsed_time = days_after_tx, pre_tx = FALSE, cap = 1825, date = 401)
#> $Dead
#> # A tibble: 0 × 9
#> # ℹ 9 variables: c_id <dbl>, lp <dbl>, days_after_tx <dbl>, dx_grp <chr>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   death <int>
#> 
#> $Alive
#> # A tibble: 2,578 × 9
#>     c_id      lp days_after_tx dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl>   <dbl>         <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1  -996  0.180            332 A           331      0.938      0.938   1.00
#>  2  -994  0.162            382 D           381      0.930      0.930   1.00
#>  3  -992  0.0928           353 C           352      0.935      0.935   1.00
#>  4  -991  0.850            316 A           315      0.940      0.940   1.00
#>  5  -985  0.165            349 B           348      0.935      0.936   1.00
#>  6  -983  0.643            369 A           368      0.933      0.933   1.00
#>  7  -981  0.667            313 A           312      0.941      0.941   1.00
#>  8  -979  0.0603           362 A           361      0.934      0.934   1.00
#>  9  -976  0.401            368 D           367      0.933      0.933   1.00
#> 10  -973 -0.150            371 A           370      0.932      0.933   1.00
#> # ℹ 2,568 more rows
#> # ℹ 1 more variable: death <int>
#> 
#> $Removed
#> # A tibble: 0 × 62
#> # ℹ 62 variables: dx_grp <chr>, c_id <dbl>, listing_day <dbl>, center <fct>,
#> #   male <int>, race_eth <fct>, age_at_listing <dbl>, airway <dbl>,
#> #   oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>, …
#> 
#> $new_char
#> # A tibble: 2,578 × 62
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>  <dbl>   <dbl>
#>  1 B      -1000       -4606 38         0 Hispanic           43.4  1.94  -0.466 
#>  2 A       -996       -3305 30         1 NH White           43.5 -0.848  0.0260
#>  3 D       -994       -3178 18         1 NH White           35.8  1.45  -0.205 
#>  4 C       -992       -2827 61         0 NH White           35.0 -0.615  0.493 
#>  5 A       -991       -2749 3          1 NH White           60.3  0.120  0.707 
#>  6 B       -988       -2510 45         0 NH White           57.6  1.22   0.0661
#>  7 B       -986       -2434 41         0 NH White           37.2  0.976 -0.340 
#>  8 B       -985       -2381 8          0 NH Black           40.2  0.416 -0.272 
#>  9 A       -983       -2281 33         0 NH White           63.7 -0.459  0.0942
#> 10 A       -982       -2260 54         1 NH White           52.0 -0.309 -0.118 
#> # ℹ 2,568 more rows
#> # ℹ 53 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```

#### Matching

`match_cas` and `match_las` are the two main functions that handle
screening, matching and acceptance.

``` r
##
dons_401 <- dplyr::filter(syn_dons, recovery_day == 401)

mtc <- match_cas(cands = r2cc, dons = dons_401, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25,
                 post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825, bio_weight = .15,
                 pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1)

mtc
#> # A tibble: 2 × 3
#>    d_id don_org data               
#>   <int> <fct>   <list>             
#> 1  2885 DLU     <tibble [115 × 12]>
#> 2  2886 DLU     <tibble [10 × 12]>
```

#### Screening

These functions are designed to input candidates and donors
respectively. Here are displayed screening for height (`hm1`), blood
type (`am1`), count (`cm1` double vs single transplant), and a distance
calculation (`lm1`). One can also create addition functions for
screening

``` r
## within matching
## there is screening
## Screening is done for height, blood type, organ count,
hm1 <- height_screen(cands = r2cc, dons = dons_401)
hm1
#> # A tibble: 482 × 4
#>     c_id  d_id match_single match_double
#>    <dbl> <int> <lgl>        <lgl>       
#>  1  -999  2885 TRUE         TRUE        
#>  2  -998  2885 FALSE        TRUE        
#>  3  -997  2886 TRUE         TRUE        
#>  4  -942  2885 TRUE         TRUE        
#>  5  -941  2885 FALSE        TRUE        
#>  6  -939  2885 TRUE         TRUE        
#>  7  -912  2886 TRUE         TRUE        
#>  8  -886  2885 FALSE        TRUE        
#>  9  -873  2886 TRUE         TRUE        
#> 10  -867  2885 TRUE         TRUE        
#> # ℹ 472 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
am1 <- abo_screen(cands = r2cc, dons = dons_401)
am1
#> # A tibble: 414 × 3
#>     c_id  d_id abo_exact
#>    <dbl> <int> <lgl>    
#>  1  -999  2885 TRUE     
#>  2  -998  2885 TRUE     
#>  3  -987  2885 TRUE     
#>  4  -977  2885 TRUE     
#>  5  -942  2885 TRUE     
#>  6  -941  2885 TRUE     
#>  7  -930  2885 TRUE     
#>  8  -904  2885 TRUE     
#>  9  -888  2885 TRUE     
#> 10  -881  2886 TRUE     
#> # ℹ 404 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
cm1 <- count_screen(cands = r2cc, dons = dons_401)
cm1
#> # A tibble: 2,146 × 2
#>     c_id  d_id
#>    <dbl> <int>
#>  1  -999  2885
#>  2  -999  2886
#>  3  -998  2885
#>  4  -998  2886
#>  5  -997  2885
#>  6  -997  2886
#>  7  -993  2885
#>  8  -993  2886
#>  9  -990  2885
#> 10  -990  2886
#> # ℹ 2,136 more rows

## Distance calculation 
lm1 <- dist_calc(cands = r2cc, dons = dons_401)
lm1
#> # A tibble: 2,146 × 3
#>     c_id  d_id distance_nm
#>    <dbl> <int>       <dbl>
#>  1  -999  2885       1834.
#>  2  -999  2886       2035.
#>  3  -998  2885        253.
#>  4  -998  2886        636.
#>  5  -997  2885        664.
#>  6  -997  2886        765.
#>  7  -993  2885        399.
#>  8  -993  2886        780.
#>  9  -990  2885        247.
#> 10  -990  2886        622.
#> # ℹ 2,136 more rows
```

In our simulation you will see to rankings `ov_rank` which stands for
overall ranking and `offer_rank`. `ov_rank` is the candidates rank with
respective to all candidates on that day based on their LAS/CAS score,
regardless of donor/offer. `offer_rank` is the rank of the candidate
with respect to all compatible candidates to the given donor. The part
of the CAS score that does not change called the “sub-CAS” calculates
the `ov_rank`

``` r
## ranking
lr1 <- calculate_sub_cas(r2cc, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25,
                         post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825, bio_weight = .15,
                         pld_weight = 0.05, peds_weight = 0.2)

lr1
#> # A tibble: 1,073 × 3
#>     c_id lu_score ov_rank
#>    <dbl>    <dbl>   <dbl>
#>  1  -999    0.175    1019
#>  2  -998    0.173    1029
#>  3  -997    0.198     667
#>  4  -993    0.147    1069
#>  5  -990    0.198     676
#>  6  -989    0.200     588
#>  7  -987    0.177    1012
#>  8  -977    0.173    1031
#>  9  -959    0.194     779
#> 10  -956    0.137    1072
#> # ℹ 1,063 more rows
```

#### Offering and Acceptance

Once all of the screening criteria, the candidates are matched on all
screening criteria and made the offers in successive order based on
either the CAS or LAS policy. There is an `acceptance_prop` function
which will calculate the acceptance probability and run binomial
acceptance for whether or not the candidate will accept the organ. And
lastly `transplant_candidates` finds the first candidate that accepts
the donor lung and transplants them so the donor identifier `d_id` is
tracked with the candidate.

``` r

## matching and offering rank
## Pra screening is not currently implemented,
## you'll notice that if you run the function it returns a combination of every candidate and donor
offs <- cas_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1, efficiency_weight = 0.1)
offs
#> # A tibble: 125 × 10
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -999  2885 TRUE         TRUE         TRUE            1834.    0.219    1019
#>  2  -998  2885 FALSE        TRUE         TRUE             253.    0.260    1029
#>  3  -942  2885 TRUE         TRUE         TRUE             389.    0.250    1057
#>  4  -941  2885 FALSE        TRUE         TRUE             486.    0.252    1047
#>  5  -867  2885 TRUE         TRUE         TRUE             482.    0.276     805
#>  6  -866  2885 TRUE         TRUE         TRUE             402.    0.269     956
#>  7  -788  2886 TRUE         TRUE         TRUE             712.    0.264     940
#>  8  -777  2885 TRUE         TRUE         TRUE             482.    0.262     997
#>  9  -740  2885 FALSE        TRUE         TRUE             163.    0.278     860
#> 10  -596  2885 TRUE         TRUE         TRUE            1395.    0.260     577
#> # ℹ 115 more rows
#> # ℹ 2 more variables: pcost <dbl>, offer_rank <dbl>

set.seed(26638)
acceptance_prob(offs, dons = dons_401, cands = r2cc)
#> # A tibble: 125 × 12
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -999  2885 TRUE         TRUE         TRUE            1834.    0.219    1019
#>  2  -998  2885 FALSE        TRUE         TRUE             253.    0.260    1029
#>  3  -942  2885 TRUE         TRUE         TRUE             389.    0.250    1057
#>  4  -941  2885 FALSE        TRUE         TRUE             486.    0.252    1047
#>  5  -867  2885 TRUE         TRUE         TRUE             482.    0.276     805
#>  6  -866  2885 TRUE         TRUE         TRUE             402.    0.269     956
#>  7  -788  2886 TRUE         TRUE         TRUE             712.    0.264     940
#>  8  -777  2885 TRUE         TRUE         TRUE             482.    0.262     997
#>  9  -740  2885 FALSE        TRUE         TRUE             163.    0.278     860
#> 10  -596  2885 TRUE         TRUE         TRUE            1395.    0.260     577
#> # ℹ 115 more rows
#> # ℹ 4 more variables: pcost <dbl>, offer_rank <dbl>, pred <dbl>, accept <int>

transplant_candidates(matches = mtc)
#> # A tibble: 2 × 15
#>    d_id don_org  c_id match_single match_double abo_exact distance_nm lu_score
#>   <int> <fct>   <dbl> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>
#> 1  2885 DLU      3087 TRUE         TRUE         TRUE             427.    0.283
#> 2  2886 DLU      2795 TRUE         FALSE        TRUE             935.    0.267
#> # ℹ 7 more variables: ov_rank <dbl>, pcost <dbl>, offer_rank <dbl>, pred <dbl>,
#> #   accept <int>, surg_type <fct>, organs_rec <dbl>
```

## Other tips and tricks

You will notice a `pra_screen` function. This is a place-holder for now
that returns a grid of every candidate and donor pairing. Future
implementation will incorporate PRA screening criteria.

``` r
pra_screen(cands = r2cc, dons = dons_401)
#> # A tibble: 2,146 × 2
#>     c_id  d_id
#>    <dbl> <int>
#>  1  -999  2885
#>  2  -999  2886
#>  3  -998  2885
#>  4  -998  2886
#>  5  -997  2885
#>  6  -997  2886
#>  7  -993  2885
#>  8  -993  2886
#>  9  -990  2885
#> 10  -990  2886
#> # ℹ 2,136 more rows
```

If you want to return the matches along with the simulated output. All
one has to do in the `run_simulation` or `iteration` function is set
`include_matches = TRUE`. The output can be seen below is a nested
tibble with a vector of donor ids (`d_id`) that are ranked in order for
the subsequent offer, called `all_matches`

``` r
l_1m <- iteration(401, syn_cands, syn_dons, include_matches = TRUE, updated_list = l, match_alg = match_cas,
                 wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
                 wl_cap = 365, post_tx_cap = 1825, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

l_1m$all_matches
#> # A tibble: 2 × 3
#>    d_id don_org data              
#>   <int> <fct>   <list>            
#> 1  2885 DLU     <tibble [115 × 1]>
#> 2  2886 DLU     <tibble [10 × 1]>
```

## A few helpful tips

`eval_simulation` behind the seens runs a function callled `concat2`. It
concatenates the several candidates/recipients datasests to one and
calculates survival data as well. By default it will use the minimum and
maximum number of days simulated from `all_candidates` in a COMET
object, but those can also be specified if one wanted to exclude
candidates after day 30 `min_enroll_date` and before day 300 days
`max_enroll_date` or censor post-transplant early at day 330
`post_tx_censor_date`. Waitlist censoring `wl_censor_date` is also
possible to change, but we would recommend keeping it the same as the
`max_enroll_date`.

``` r
concat2(r1)
#> # A tibble: 4,248 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing  airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>   <dbl>   <dbl>
#>  1 B      -1000       -4606 38         0 Hispanic           43.4  1.94   -0.466 
#>  2 B       -999       -4605 7          0 NH White           53.5  0.233   0.679 
#>  3 A       -998       -3842 21         0 NH White           65.7 -0.347   0.0699
#>  4 A       -997       -3618 55         1 Asian              56.3 -0.0723 -0.241 
#>  5 A       -996       -3305 30         1 NH White           43.5 -0.848   0.0260
#>  6 B       -995       -3252 38         0 NH White           56.0  1.23   -0.880 
#>  7 D       -994       -3178 18         1 NH White           35.8  1.45   -0.205 
#>  8 A       -993       -3052 18         1 NH White           72.1 -1.15   -0.0170
#>  9 C       -992       -2827 61         0 NH White           35.0 -0.615   0.493 
#> 10 A       -991       -2749 3          1 NH White           60.3  0.120   0.707 
#> # ℹ 4,238 more rows
#> # ℹ 68 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
concat2(r1, min_enroll_date = 31, max_enroll_date = 300, wl_censor_date = 300, post_tx_censor_date = 330)
#> # A tibble: 3,180 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing  airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>   <dbl>   <dbl>
#>  1 B      -1000       -4606 38         0 Hispanic           43.4  1.94   -0.466 
#>  2 B       -999       -4605 7          0 NH White           53.5  0.233   0.679 
#>  3 A       -997       -3618 55         1 Asian              56.3 -0.0723 -0.241 
#>  4 A       -996       -3305 30         1 NH White           43.5 -0.848   0.0260
#>  5 B       -995       -3252 38         0 NH White           56.0  1.23   -0.880 
#>  6 D       -994       -3178 18         1 NH White           35.8  1.45   -0.205 
#>  7 C       -992       -2827 61         0 NH White           35.0 -0.615   0.493 
#>  8 A       -991       -2749 3          1 NH White           60.3  0.120   0.707 
#>  9 A       -990       -2703 42         0 NH White           63.0  1.34    0.184 
#> 10 B       -988       -2510 45         0 NH White           57.6  1.22    0.0661
#> # ℹ 3,170 more rows
#> # ℹ 68 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```

The function `spec_day` will allow one to grab the waiting list on a
specific day.

``` r
spec_day(r1, day = 300)
#> # A tibble: 1,037 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>  <dbl>   <dbl>
#>  1 B      -1000       -4606 38         0 Hispanic           43.4  1.94  -0.466 
#>  2 B       -995       -3252 38         0 NH White           56.0  1.23  -0.880 
#>  3 A       -991       -2749 3          1 NH White           60.3  0.120  0.707 
#>  4 A       -990       -2703 42         0 NH White           63.0  1.34   0.184 
#>  5 B       -988       -2510 45         0 NH White           57.6  1.22   0.0661
#>  6 A       -980       -2228 32         0 NH White           54.9 -0.190 -0.379 
#>  7 A       -973       -2117 13         0 NH White           39.8 -0.274  0.254 
#>  8 A       -960       -1826 41         1 NH White           58.0 -0.656  0.139 
#>  9 A       -957       -1799 52         1 NH White           51.8 -0.175  0.642 
#> 10 A       -951       -1716 39         0 NH White           68.7 -0.504  0.169 
#> # ℹ 1,027 more rows
#> # ℹ 68 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```

`rmst` will output the restricted mean survival time (RMST) or the Area
Under the Curve (AUC) for the LAS and CAS waitlist mortality models as
well as the linear predictor. The linear predictor is calculated using
the `calc_post_tx_cas23` and `calc_wl_cas23` and other respective
functions for the waitlist and post-transplant models for the CAS/LAS
policies. Currently implemented are the 2015 LAS (`"LAS15"`), 2021 LAS
(`"LAS21"`) and 2023 CAS (`"CAS23"`). This also runs `expected_survival`
which actually calculates the AUC for the candidate based on their
linear predictor.

``` r
rmst("LAS21", cand_data = r2cc, cap = 365, wl = TRUE)
#> # A tibble: 1,073 × 3
#>     c_id    lp expected
#>    <dbl> <dbl>    <dbl>
#>  1  -999  4.09     307.
#>  2  -998  2.13     356.
#>  3  -997  2.81     348.
#>  4  -993  4.26     298.
#>  5  -990  1.64     360.
#>  6  -989  2.85     347.
#>  7  -987  2.26     355.
#>  8  -977  2.59     351.
#>  9  -959  3.26     338.
#> 10  -956  4.23     300.
#> # ℹ 1,063 more rows
rmst("LAS21", cand_data = r2cc, cap = 365, wl = FALSE)
#> # A tibble: 1,073 × 3
#>     c_id    lp expected
#>    <dbl> <dbl>    <dbl>
#>  1  -999 1.45      325.
#>  2  -998 0.914     341.
#>  3  -997 0.802     343.
#>  4  -993 1.41      326.
#>  5  -990 0.724     345.
#>  6  -989 0.922     341.
#>  7  -987 0.935     340.
#>  8  -977 0.889     341.
#>  9  -959 0.815     343.
#> 10  -956 1.51      322.
#> # ℹ 1,063 more rows
```

One can calculate RMST if one supplies a vector of survival times, or
one can calculate the survival given the raw survival value on a given
day. For example below we will use Day 364 of the CAS23 waitlist
survival model which has a baseline survival value of 0.994390.

``` r
expected_survival(lp = c(0, 1, -0.5), 0.994390)
#> [1] 0.9943900 0.9848238 0.9965936
```

[^1]: If one wonders what model is “CAS23r”, it is the baseline hazard
    for the “CAS23” models ‘recalibrated’ after research and initial
    simulations runs showed that the “CAS23” model did not accurately
    simulate waitlist and post-transplant deaths.

[^2]: Models for waitlist removal for other reasons are not part of the
    LAS/CAS and were modeled separately. The maximum day for this 2365
    days after being placed on the waiting list
