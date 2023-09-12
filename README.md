
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

``` r
library(COMET)
## basic example code
## Simulates under LAS rules
r1 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 2.782266 mins

## Simulates the CAS rules
r2 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.326494 mins

## You can mix and match systems, change weights
## for example simulate the LAS using the CAS models
# r3 <- run_simulation(days = 400, can_start = 1000,
#                      match_alg = match_las, wl_model = "CAS23", post_tx_model = "CAS23",
#                      wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
# 
# 
# 
# ## or maybe you want to test simulate what would happen if waitlist mortality was weighted the same as post-transplant under the LAS
# r4 <- run_simulation(days = 400, can_start = 1000,
#                      match_alg = match_las, wl_model = "CAS23", post_tx_model = "CAS23",
#                      wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
```

## A comparison of the models

``` r
# see how many were transplanted, donor count, waitlist deaths
eval_simulation(r1)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4325     2701        288     130 1206       2432.309           200
#>   post_tx_years   tx_ppy  wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1       1402.65 1.110467 0.118406 0.1425872 198.2884 0.08034061      0      76
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     187         6      2827     2626          134     531

eval_simulation(r2)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4325     2719        275     146 1185       2434.327           175
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1      1420.151 1.116941 0.1129676 0.1232264 340.5805 0.08201545      0      73
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     186         6      2827     2626          143     575

## See results by diagnosis group
eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1226      842         24      26  334      1097.2594
#> 2      B       279      140         21      27   91       167.3922
#> 3      C       340      207         16       9  108       152.8378
#> 4      D      2480     1512        227      68  673      1014.8200
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy   ptd_ppy med_dist
#> 1            54     448.22998 0.7673664 0.02187268 0.1204739 215.6939
#> 2            11      74.07529 0.8363592 0.12545388 0.1484976 171.9993
#> 3            13     108.46270 1.3543771 0.10468616 0.1198569 188.4009
#> 4           122     771.88227 1.4899194 0.22368499 0.1580552 190.9366
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.08669834      0   126.5   195.0        10      2827     2626          134
#> 2 0.08571429      0    85.0   191.5         4      2827     2626          134
#> 3 0.06763285      0    69.5   195.0         5      2827     2626          134
#> 4 0.07804233      0    61.0   181.0         4      2827     2626          134
#>   nu_orgs
#> 1     531
#> 2     531
#> 3     531
#> 4     531
eval_simulation(r2, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1226      881         30      28  287      1060.1342
#> 2      B       279      135         26      25   93       168.3203
#> 3      C       340      256         11       2   71       123.9124
#> 4      D      2480     1447        208      91  734      1081.9603
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy   ptd_ppy med_dist
#> 1            59     480.29843 0.8310269 0.02829831 0.1228403 349.3196
#> 2            12      67.62491 0.8020422 0.15446738 0.1774494 310.0576
#> 3            13     143.38672 2.0659758 0.08877240 0.0906639 374.5947
#> 4            91     728.84052 1.3373873 0.19224365 0.1248558 327.8875
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.09534620      0   104.0     200         9      2827     2626          143
#> 2 0.08888889      0   102.0     179         7      2827     2626          143
#> 3 0.08984375      0    33.5     213         3      2827     2626          143
#> 4 0.07187284      0    67.0     176         5      2827     2626          143
#>   nu_orgs
#> 1     575
#> 2     575
#> 3     575
#> 4     575
```

## A further demonstration to show what is going on behind the scenes

### Generates candidates and donors

``` r
## One major advantage of the simulation is the use of synthetic candidates and donors
## All of these rely on bayesian hyperparametric distributions for the candidate and donor characteristics
set.seed(26638)
syn_cands <- gen_and_spawn_candidates(days = 10)
syn_dons <- gen_and_spawn_donors(days = 10)
```

## the function iterate runs each day, if you specify a list

For this scenario I’m going to add 400 days the listing and recovery day
to syn_cands and syn_donors and also make sure the candidate and donor
ids do not conflict. I’m going to continue the results from CAS
simulation (`r2`). The notation for the LAS simulation (`r1`) is very
similar.

``` r
syn_cands <- dplyr::mutate(syn_cands,
                    listing_day = listing_day + max(r2$all_candidates$listing_day),
                    c_id = c_id + max(r2$all_candidates$c_id))

syn_dons <- dplyr::mutate(syn_dons,
                    recovery_day = recovery_day + max(r2$all_candidates$listing_day),
                    d_id = d_id + max(r2$all_donors$d_id))

r2cc <- r2$current_candidates

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
```

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

## within each day

`update_patients` is the function that runs each day, it will output a
list of those that died, were removed for other reasons if on the
waiting list, those that a still alive and updated characteristics.
Currently the only characteristic is that updated is the candidates age,
but it is made to be flexible to adjust any clinical characteristic
i.e. ($\textrm{pCO}_2$,$\textrm{FEV}_1$, bilirubin, etc.

`identify_deaths`, returns a dataset of those who died either on the
waiting list or post-transplant

`identify_removals`, returns a dataset of those who were removed on the
waiting list

``` r
update_patients(patients = r2cc, model = "CAS23r", 
                elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 401)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `removal = rbinom(n = max(row_number()), size = 1, prob = 1 -
#>   cond_x)`.
#> Caused by warning in `rbinom()`:
#> ! NAs produced
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1
#>   - cond_x)`.
#> Caused by warning in `rbinom()`:
#> ! NAs produced
#> $Dead
#> # A tibble: 0 × 10
#> # ℹ 10 variables: c_id <dbl>, lp <dbl>, days_on_waitlist <dbl>, dx_grp <chr>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   death <int>, max_days <dbl>
#> 
#> $Alive
#> # A tibble: 1,178 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   287  3.19              363 B           362      0.995      0.995  1.00 
#>  2   292  3.83              362 B           361      0.995      0.995  0.999
#>  3   295  4.40              362 D           361      0.995      0.995  0.999
#>  4   299  4.03              362 D           361      0.995      0.995  0.999
#>  5   310  4.18              361 D           360      0.995      0.995  0.999
#>  6   320  4.02              360 D           359      0.995      0.995  0.999
#>  7   321  3.18              360 D           359      0.995      0.995  1.00 
#>  8   340  3.40              358 B           357      0.995      0.995  1.00 
#>  9   343  4.42              358 D           357      0.995      0.995  0.999
#> 10   351  2.48              356 A           355      0.995      0.995  1.00 
#> # ℹ 1,168 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>
#> 
#> $Removed
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <dbl>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>
#> 
#> $new_char
#> # A tibble: 1,185 × 48
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>  <dbl>   <dbl>
#>  1 A       -999       -5731 26         1 Asian              56.3 -0.941  0.0101
#>  2 A       -996       -3648 6          0 NH White           63.1 -0.668  0.336 
#>  3 A       -994       -3200 55         1 NH White           58.1 -0.802 -0.245 
#>  4 A       -990       -2575 44         0 NH White           66.9 -0.781  0.0624
#>  5 A       -989       -2467 47         0 NH White           63.8 -0.487 -0.200 
#>  6 A       -983       -2205 18         1 Asian              69.9 -0.780  0.272 
#>  7 A       -979       -1978 34         0 NH White           67.6 -0.591  0.429 
#>  8 A       -975       -1943 41         0 NH White           59.7 -0.253  0.781 
#>  9 A       -971       -1854 54         0 NH White           66.4 -0.457 -0.147 
#> 10 A       -967       -1802 24         0 NH White           62.6 -0.703  0.462 
#> # ℹ 1,175 more rows
#> # ℹ 39 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …

## which includes

identify_deaths(patients = r2cc, model = "CAS23r",
                elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 401)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1
#>   - cond_x)`.
#> Caused by warning in `rbinom()`:
#> ! NAs produced
#> # A tibble: 1,185 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   287  3.19              363 B           362      0.995      0.995  1.00 
#>  2   292  3.83              362 B           361      0.995      0.995  0.999
#>  3   295  4.40              362 D           361      0.995      0.995  0.999
#>  4   299  4.03              362 D           361      0.995      0.995  0.999
#>  5   310  4.18              361 D           360      0.995      0.995  0.999
#>  6   320  4.02              360 D           359      0.995      0.995  0.999
#>  7   321  3.18              360 D           359      0.995      0.995  1.00 
#>  8   340  3.40              358 B           357      0.995      0.995  1.00 
#>  9   343  4.42              358 D           357      0.995      0.995  0.999
#> 10   351  2.48              356 A           355      0.995      0.995  1.00 
#> # ℹ 1,175 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>

identify_removals(patients = r2cc, elapsed_time = days_on_waitlist, cap = 2365)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `removal = rbinom(n = max(row_number()), size = 1, prob = 1 -
#>   cond_x)`.
#> Caused by warning in `rbinom()`:
#> ! NAs produced
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <dbl>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>

## post-transplant deaths
update_patients(r2$recipient_database,  model = "CAS23r", 
                elapsed_time = days_after_tx, pre_tx = FALSE, cap = 1825, date = 401)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `death = rbinom(n = max(dplyr::row_number()), size = 1, prob = 1
#>   - cond_x)`.
#> Caused by warning in `rbinom()`:
#> ! NAs produced
#> $Dead
#> # A tibble: 0 × 9
#> # ℹ 9 variables: c_id <dbl>, lp <dbl>, days_after_tx <dbl>, dx_grp <chr>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   death <int>
#> 
#> $Alive
#> # A tibble: 2,537 × 9
#>     c_id     lp days_after_tx dx_grp days_ago Survival_y Survival_x cond_x death
#>    <dbl>  <dbl>         <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl> <int>
#>  1 -1000  0.423           361 B           360      0.934      0.934   1.00     0
#>  2  -998  0.296           385 A           384      0.930      0.930   1.00     0
#>  3  -995  0.481           362 D           361      0.934      0.934   1.00     0
#>  4  -992  0.423           361 A           360      0.934      0.934   1.00     0
#>  5  -991  0.356           323 D           322      0.939      0.939   1.00     0
#>  6  -985  0.274           376 A           375      0.931      0.932   1.00     0
#>  7  -977  0.440           326 A           325      0.939      0.939   1.00     0
#>  8  -974 -0.180           356 D           355      0.935      0.935   1.00     0
#>  9  -972  0.513           371 A           370      0.932      0.933   1.00     0
#> 10  -968  0.287           387 D           386      0.929      0.930   1.00     0
#> # ℹ 2,527 more rows
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
#> # A tibble: 2,544 × 62
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing   airway
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>    <dbl>
#>  1 B      -1000       -5908 61         1 NH White           30.3  0.940  
#>  2 A       -998       -3757 8          0 Hispanic           50.3 -0.949  
#>  3 A       -997       -3713 45         1 NH White           65.5 -0.00918
#>  4 D       -995       -3298 40         0 NH White           30.9  0.857  
#>  5 D       -993       -3091 1          0 NH Black           41.7 -0.374  
#>  6 A       -992       -2816 46         0 NH Black           51.4 -0.105  
#>  7 D       -991       -2704 2          0 NH White           47.8 -0.244  
#>  8 A       -988       -2423 22         0 NH White           63.8 -0.0923 
#>  9 A       -987       -2300 17         1 NH White           59.1 -0.0230 
#> 10 A       -985       -2244 55         1 NH White           52.4 -0.586  
#> # ℹ 2,534 more rows
#> # ℹ 54 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```

## Matching

For screening, matching, and donor acceptance, all of that is bundled in
two main functions and .

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
#> 1  2828 DLU     <tibble [132 × 12]>
#> 2  2829 DLU     <tibble [13 × 12]>
```

An in depth look at screening. One can create custom screen functions or
screen on whatever criteria one would like. They are designed to input
candidates and donors respectively.

``` r

## within matching
## there is screening
## Screening is done for height, blood type, organ count,
hm1 <- height_screen(cands = r2cc, dons = dons_401)
hm1
#> # A tibble: 564 × 4
#>     c_id  d_id match_single match_double
#>    <dbl> <int> <lgl>        <lgl>       
#>  1  -996  2828 FALSE        TRUE        
#>  2  -994  2829 TRUE         TRUE        
#>  3  -989  2829 TRUE         TRUE        
#>  4  -983  2829 TRUE         TRUE        
#>  5  -979  2828 TRUE         TRUE        
#>  6  -971  2828 FALSE        TRUE        
#>  7  -965  2828 FALSE        TRUE        
#>  8  -964  2828 TRUE         TRUE        
#>  9  -946  2829 TRUE         TRUE        
#> 10  -930  2829 TRUE         TRUE        
#> # ℹ 554 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
am1 <- abo_screen(cands = r2cc, dons = dons_401)
am1
#> # A tibble: 497 × 3
#>     c_id  d_id abo_exact
#>    <dbl> <int> <lgl>    
#>  1  -996  2828 TRUE     
#>  2  -990  2828 TRUE     
#>  3  -983  2829 TRUE     
#>  4  -975  2828 TRUE     
#>  5  -971  2828 TRUE     
#>  6  -967  2828 TRUE     
#>  7  -964  2829 TRUE     
#>  8  -960  2828 TRUE     
#>  9  -946  2828 TRUE     
#> 10  -943  2828 TRUE     
#> # ℹ 487 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
cm1 <- count_screen(cands = r2cc, dons = dons_401)
cm1
#> # A tibble: 2,370 × 2
#>     c_id  d_id
#>    <dbl> <int>
#>  1  -999  2828
#>  2  -999  2829
#>  3  -996  2828
#>  4  -996  2829
#>  5  -994  2828
#>  6  -994  2829
#>  7  -990  2828
#>  8  -990  2829
#>  9  -989  2828
#> 10  -989  2829
#> # ℹ 2,360 more rows

## Distance calculation 
lm1 <- dist_calc(cands = r2cc, dons = dons_401)
lm1
#> # A tibble: 2,370 × 3
#>     c_id  d_id distance_nm
#>    <dbl> <int>       <dbl>
#>  1  -999  2828        710.
#>  2  -999  2829        935.
#>  3  -996  2828       1844.
#>  4  -996  2829       2049.
#>  5  -994  2828        664.
#>  6  -994  2829        765.
#>  7  -990  2828        666.
#>  8  -990  2829        847.
#>  9  -989  2828        479.
#> 10  -989  2829        712.
#> # ℹ 2,360 more rows
```

In our simulation you will see to rankings `ov_rank` which stands for
overall ranking and `offer_rank`. `ov_rank` is the candidates rank with
respective to all candidates on that day based on their LAS/CAS score,
regardless of donor/offer. `offer_rank` is the rank of the candidate
with respect to all compatible candidates to the given donor. The CAS
score that does not change called the “sub_cas” calculcates the
`ov_rank`

``` r
## ranking
lr1 <- calculate_sub_cas(syn_cands, wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25,
                         post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825, bio_weight = .15,
                         pld_weight = 0.05, peds_weight = 0.2)

lr1
#> # A tibble: 78 × 3
#>     c_id lu_score ov_rank
#>    <dbl>    <dbl>   <dbl>
#>  1  3326    0.207      51
#>  2  3327    0.211      39
#>  3  3328    0.213      33
#>  4  3329    0.197      68
#>  5  3330    0.221      18
#>  6  3331    0.157      78
#>  7  3332    0.192      72
#>  8  3333    0.249       7
#>  9  3334    0.212      35
#> 10  3335    0.211      38
#> # ℹ 68 more rows
```

``` r

## matching and offering rank
## Pra screening is not currently implemented,
## you'll notice that if you run the function it returns a combination of every candidate and donor
offs <- cas_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1)
offs
#> # A tibble: 145 × 10
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -996  2828 FALSE        TRUE         TRUE            1844.       NA      NA
#>  2  -983  2829 TRUE         TRUE         TRUE             780.       NA      NA
#>  3  -971  2828 FALSE        TRUE         TRUE             666.       NA      NA
#>  4  -834  2828 FALSE        TRUE         TRUE             379.       NA      NA
#>  5  -737  2828 FALSE        TRUE         TRUE             710.       NA      NA
#>  6  -715  2828 FALSE        TRUE         TRUE             689.       NA      NA
#>  7  -638  2828 TRUE         TRUE         FALSE           1836.       NA      NA
#>  8  -630  2828 TRUE         TRUE         TRUE             479.       NA      NA
#>  9  -574  2828 TRUE         TRUE         TRUE             558.       NA      NA
#> 10  -462  2828 TRUE         TRUE         TRUE             247.       NA      NA
#> # ℹ 135 more rows
#> # ℹ 2 more variables: pcost <dbl>, offer_rank <dbl>

## This function is here to model acceptance probability
## Others are happy to glad their own
acceptance_prob(offs, dons = dons_401, cands = r2cc)
#> # A tibble: 145 × 12
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -996  2828 FALSE        TRUE         TRUE            1844.       NA      NA
#>  2  -983  2829 TRUE         TRUE         TRUE             780.       NA      NA
#>  3  -971  2828 FALSE        TRUE         TRUE             666.       NA      NA
#>  4  -834  2828 FALSE        TRUE         TRUE             379.       NA      NA
#>  5  -737  2828 FALSE        TRUE         TRUE             710.       NA      NA
#>  6  -715  2828 FALSE        TRUE         TRUE             689.       NA      NA
#>  7  -638  2828 TRUE         TRUE         FALSE           1836.       NA      NA
#>  8  -630  2828 TRUE         TRUE         TRUE             479.       NA      NA
#>  9  -574  2828 TRUE         TRUE         TRUE             558.       NA      NA
#> 10  -462  2828 TRUE         TRUE         TRUE             247.       NA      NA
#> # ℹ 135 more rows
#> # ℹ 4 more variables: pcost <dbl>, offer_rank <dbl>, pred <dbl>, accept <int>

## transplant_recipeints
transplant_candidates(matches = mtc)
#> # A tibble: 1 × 15
#>    d_id don_org  c_id match_single match_double abo_exact distance_nm lu_score
#>   <int> <fct>   <dbl> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>
#> 1  2828 DLU      2494 TRUE         TRUE         TRUE             689.    0.279
#> # ℹ 7 more variables: ov_rank <dbl>, pcost <dbl>, offer_rank <dbl>, pred <dbl>,
#> #   accept <int>, surg_type <fct>, organs_rec <dbl>
```

## 

``` r
## screening
# cas_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1)
# 
# ## calculating LAS/CAS scores
# calculate_las(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
# calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
#                   post_tx_weight = 0.25, bio_weight = .15, peds_weight = .2, pld_weight = 0.05)
# 
# ## One may know that CAS biological component is actually 3 of them. one for height, cpra, and blood type.
# ## Can instead specify these weights
# calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
#                   post_tx_weight = 0.25, bio_weight = NA, peds_weight = .2, pld_weight = 0.05,
#                   abo_weight = 0.10, height_weight = 0.05, cpra_weight = 0)
# 
# ## updating patients
# set.seed(266388)
# update_patients(syn_cands, model = "CAS23", elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 11)
# 
# 
# 
# 
# 
# ## transplanting
# # set.seed(367473)
# 
# mz <- match_las(syn_cands, syn_dons, wl_model = "LAS15", post_tx_model = "LAS15", 
#                 wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
# mz
# 
# transplant_candidates(mz, rec_ids = NA, max_offer = NA)
## post-transplant
```

## A few helpful tips

``` r
concat2(r1)
#> # A tibble: 4,325 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing   airway
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>    <dbl>
#>  1 B      -1000       -5908 61         1 NH White           30.3  0.940  
#>  2 A       -999       -5731 26         1 Asian              56.3 -0.941  
#>  3 A       -998       -3757 8          0 Hispanic           50.3 -0.949  
#>  4 A       -997       -3713 45         1 NH White           65.5 -0.00918
#>  5 A       -996       -3648 6          0 NH White           63.1 -0.668  
#>  6 D       -995       -3298 40         0 NH White           30.9  0.857  
#>  7 A       -994       -3200 55         1 NH White           58.1 -0.802  
#>  8 D       -993       -3091 1          0 NH Black           41.7 -0.374  
#>  9 A       -992       -2816 46         0 NH Black           51.4 -0.105  
#> 10 D       -991       -2704 2          0 NH White           47.8 -0.244  
#> # ℹ 4,315 more rows
#> # ℹ 69 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …

## To grab who was on the waitlist on a specific day
spec_day(r1, day = 300)
#> # A tibble: 1,162 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing   airway
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>    <dbl>
#>  1 A       -997       -3713 45         1 NH White           65.5 -0.00918
#>  2 D       -995       -3298 40         0 NH White           30.9  0.857  
#>  3 A       -979       -1978 34         0 NH White           67.6 -0.591  
#>  4 A       -971       -1854 54         0 NH White           66.4 -0.457  
#>  5 A       -965       -1752 24         0 NH White           72.3 -0.535  
#>  6 D       -959       -1707 45         0 NH White           55.1  0.569  
#>  7 A       -953       -1652 41         1 Hispanic           38.8 -0.793  
#>  8 D       -951       -1643 47         0 NH White           69.3  0.393  
#>  9 A       -944       -1508 48         0 NH White           64.6 -0.119  
#> 10 A       -942       -1495 27         0 NH White           60.3 -1.03   
#> # ℹ 1,152 more rows
#> # ℹ 69 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```
