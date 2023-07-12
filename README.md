
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

## The rest to be added later

``` r
library(COMET)
## basic example code
## Simulates under LAS rules
r1 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1,
                     match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 367473)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 24.34172 secs

## Simulates the CAS rules
r2 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 367473)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 20.35377 secs

## You can mix and match systems, change weights
## for example simulate the LAS using the CAS models

## or maybe you want to test simualte what would happen if waitlist was weighted the same as post-transplatn under the LAS

## you can change the weights as well in the CAS, 
```

## A comparison of the models

``` r
# eval_simulation()
# see how many were transplanted, donor count, waitlist deaths
eval_simulation(r1)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      1151      138         17       6  990       1257.966             1
#>   post_tx_years    tx_ppy    wld_ppy   ptd_ppy pt_1yr pt_2yr med_wlt med_ptt
#> 1      3.720739 0.1097009 0.01351388 0.2687638      0      0     200     9.5
#>   med_offer don_count don_util d_med_offer d_orgs
#> 1       5.5       140      130         356     22

eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A       484       57          0       3  424      682.89391
#> 2      B        69        8          3       1   57       77.30869
#> 3      C        96        5          1       1   89       84.10130
#> 4      D       502       68         13       1  420      413.66188
#>   post_tx_death post_tx_years     tx_ppy    wld_ppy   ptd_ppy pt_1yr pt_2yr
#> 1             0     1.5660507 0.08346831 0.00000000 0.0000000      0      0
#> 2             0     0.1889117 0.10348125 0.03880547 0.0000000      0      0
#> 3             0     0.1122519 0.05945211 0.01189042 0.0000000      0      0
#> 4             1     1.8535250 0.16438547 0.03142663 0.5395126      0      0
#>   med_wlt med_ptt med_offer don_count don_util d_med_offer d_orgs
#> 1     342     9.0       8.0       140      130         356     22
#> 2     108     9.5       5.0       140      130         356     22
#> 3     174     7.0       5.0       140      130         356     22
#> 4     115    10.5       5.5       140      130         356     22
```

## A further demonstration to show what is going on behind the scenes

``` r
## One major advantage of the simulation is the use of synthetic candidates and donors
## All of these rely on bayesian hyperparametric distributions for the candidate and donor characteristics
syn_cands <- gen_and_spawn_candidates(days = 100)

syn_dons <- gen_and_spawn_donors(days = 2)
```

## 

``` r
## screening
## Screening is done for height, blood type, organ count,
height_screen(syn_cands, syn_dons)
#> # A tibble: 4,064 × 4
#>     c_id  d_id match_single match_double
#>    <int> <int> <lgl>        <lgl>       
#>  1     1     1 TRUE         TRUE        
#>  2     1     3 TRUE         FALSE       
#>  3     1     4 TRUE         FALSE       
#>  4     1     5 FALSE        TRUE        
#>  5     1     6 TRUE         TRUE        
#>  6     1     7 FALSE        TRUE        
#>  7     1     8 TRUE         TRUE        
#>  8     1    11 TRUE         FALSE       
#>  9     2     1 TRUE         TRUE        
#> 10     2     2 TRUE         FALSE       
#> # ℹ 4,054 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
abo_screen(syn_cands, syn_dons)
#> # A tibble: 6,150 × 3
#>     c_id  d_id abo_exact
#>    <int> <int> <lgl>    
#>  1     1     1 TRUE     
#>  2     1     2 TRUE     
#>  3     1     3 TRUE     
#>  4     1     4 TRUE     
#>  5     1     6 TRUE     
#>  6     1     7 TRUE     
#>  7     2     1 FALSE    
#>  8     2     2 FALSE    
#>  9     2     3 FALSE    
#> 10     2     4 FALSE    
#> # ℹ 6,140 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
count_screen(syn_cands, syn_dons)
#> # A tibble: 8,756 × 2
#>     c_id  d_id
#>    <int> <int>
#>  1     1     1
#>  2     1     2
#>  3     1     3
#>  4     1     4
#>  5     1     5
#>  6     1     6
#>  7     1     7
#>  8     1     8
#>  9     1     9
#> 10     1    10
#> # ℹ 8,746 more rows

## Distance calculation 

## matching

## ranking

## calculating LAS/CAS scores
calculate_las(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
#> # A tibble: 796 × 4
#>     c_id lp.wl lp.ptx lu_score
#>    <int> <dbl>  <dbl>    <dbl>
#>  1     1  3.41  1.19     0.487
#>  2     2  1.57  0.738    0.346
#>  3     3  2.38  0.821    0.386
#>  4     4  2.61  0.659    0.408
#>  5     5  1.26  0.668    0.338
#>  6     6  5.34  1.64     0.814
#>  7     7  2.51  1.28     0.381
#>  8     8  1.02 -1.02     0.351
#>  9     9  2.65  0.867    0.407
#> 10    10  4.45  1.27     0.679
#> # ℹ 786 more rows
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = .15, peds_weight = .2, pld_weight = 0.05)
#> # A tibble: 796 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.242     388
#>  2     2    0.237     607
#>  3     3    0.235     634
#>  4     4    0.242     394
#>  5     5    0.235     642
#>  6     6    0.342      58
#>  7     7    0.226     768
#>  8     8    0.253     172
#>  9     9    0.241     456
#> 10    10    0.277     105
#> # ℹ 786 more rows

## One may know that CAS biological component is actually 3 of them. one for height, cpra, and blood type.
## Can instead specify these weights
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = NA, peds_weight = .2, pld_weight = 0.05,
                  abo_weight = 0.10, height_weight = 0.05, cpra_weight = 0)
#> # A tibble: 796 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.247     296
#>  2     2    0.239     568
#>  3     3    0.235     667
#>  4     4    0.244     375
#>  5     5    0.235     658
#>  6     6    0.342      59
#>  7     7    0.226     770
#>  8     8    0.249     236
#>  9     9    0.245     349
#> 10    10    0.277     110
#> # ℹ 786 more rows

## updating patients



## transplanting

## post-transplant
```
