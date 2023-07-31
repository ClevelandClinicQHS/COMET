
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
r1 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.70737 mins

## Simulates the CAS rules
r2 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 4.280185 mins

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
#> 1      4325     2700        328     138 1159       2403.231           193
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1      1402.053 1.123488 0.1364829 0.1376552 202.4022 0.08148148      0      75
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     185         5      2827     2626          144     558

eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1226      844         28      33  321      1090.7926
#> 2      B       279      154         25      21   79       164.4079
#> 3      C       340      200         19       9  112       152.5394
#> 4      D      2480     1502        256      75  647       995.4908
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy   ptd_ppy med_dist
#> 1            56      449.3032 0.7737493 0.02566941 0.1246374 219.3709
#> 2            11       75.3347 0.9366944 0.15206078 0.1460150 179.9796
#> 3            16      104.4764 1.3111370 0.12455802 0.1531447 188.2805
#> 4           110      772.9391 1.5088036 0.25715959 0.1423139 197.9192
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.09478673      0     126   190.0        11      2827     2626          144
#> 2 0.08441558      0      94   159.0         5      2827     2626          144
#> 3 0.06500000      0      75   183.0         5      2827     2626          144
#> 4 0.07589880      0      57   185.5         4      2827     2626          144
#>   nu_orgs
#> 1     558
#> 2     558
#> 3     558
#> 4     558
```

## A further demonstration to show what is going on behind the scenes

``` r
## One major advantage of the simulation is the use of synthetic candidates and donors
## All of these rely on bayesian hyperparametric distributions for the candidate and donor characteristics
syn_cands <- gen_and_spawn_candidates(days = 100)

syn_dons <- gen_and_spawn_donors(days = 10)
```

## 

``` r
## screening
## Screening is done for height, blood type, organ count,
hm1 <- height_screen(syn_cands, syn_dons)
hm1
#> # A tibble: 492 × 4
#>     c_id  d_id match_single match_double
#>    <int> <int> <lgl>        <lgl>       
#>  1     1     1 TRUE         TRUE        
#>  2     1     2 FALSE        TRUE        
#>  3     1     3 TRUE         TRUE        
#>  4     1     4 FALSE        TRUE        
#>  5     1     5 FALSE        TRUE        
#>  6     1     6 TRUE         TRUE        
#>  7     1     8 TRUE         TRUE        
#>  8     1    10 FALSE        TRUE        
#>  9     2     1 TRUE         FALSE       
#> 10     2     2 TRUE         TRUE        
#> # ℹ 482 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
am1 <- abo_screen(syn_cands, syn_dons)
am1
#> # A tibble: 623 × 3
#>     c_id  d_id abo_exact
#>    <int> <int> <lgl>    
#>  1     1     2 TRUE     
#>  2     1     6 TRUE     
#>  3     1     8 TRUE     
#>  4     1     9 TRUE     
#>  5     1    10 TRUE     
#>  6     2     1 TRUE     
#>  7     2     2 FALSE    
#>  8     2     3 TRUE     
#>  9     2     5 TRUE     
#> 10     2     6 FALSE    
#> # ℹ 613 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
cm1 <- count_screen(syn_cands, syn_dons)
cm1
#> # A tibble: 946 × 2
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
#> # ℹ 936 more rows

## Distance calculation 
lm1 <- las_dist_calc(syn_cands, syn_dons)
lm1
#> # A tibble: 1,000 × 4
#>     c_id  d_id distance_nm proximity_class
#>    <int> <int>       <dbl> <fct>          
#>  1     2     3       178.  <250           
#>  2     4     3       241.  <250           
#>  3     9     8        50.3 <250           
#>  4    10     1       233.  <250           
#>  5    10     9        84.7 <250           
#>  6    11     2       221.  <250           
#>  7    11     3       131.  <250           
#>  8    13     3       241.  <250           
#>  9    15     7       201.  <250           
#> 10    16     6        70.2 <250           
#> # ℹ 990 more rows

## ranking

lr1 <- calculate_las(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1) |> dplyr::mutate(ov_rank = rank(-lu_score)) |> dplyr::arrange(ov_rank)

lr1
#> # A tibble: 100 × 5
#>     c_id lp.wl lp.ptx lu_score ov_rank
#>    <int> <dbl>  <dbl>    <dbl>   <dbl>
#>  1    42  8.36  1.60     0.937       1
#>  2     5  8.02  1.71     0.930       2
#>  3    73  7.08  1.60     0.925       3
#>  4    87  6.20  0.932    0.920       4
#>  5    43  7.53  1.92     0.913       5
#>  6    55  6.09  1.26     0.902       6
#>  7    70  5.25  1.24     0.820       7
#>  8    94  4.92  1.27     0.767       8
#>  9    59  4.80  1.44     0.739       9
#> 10    13  4.69  1.22     0.728      10
#> # ℹ 90 more rows

## matching and offering rank
las_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1)
#> # A tibble: 275 × 12
#>     c_id  d_id match_single match_double abo_exact distance_nm proximity_class
#>    <int> <int> <lgl>        <lgl>        <lgl>           <dbl> <fct>          
#>  1    28     1 TRUE         TRUE         FALSE            193. <250           
#>  2    49     1 TRUE         TRUE         TRUE             483. 250-500        
#>  3    63     1 TRUE         TRUE         TRUE             483. 250-500        
#>  4    21     1 TRUE         TRUE         TRUE             483. 250-500        
#>  5    41     1 TRUE         TRUE         TRUE             336. 250-500        
#>  6    30     1 TRUE         TRUE         TRUE             486. 250-500        
#>  7    47     1 FALSE        TRUE         TRUE             444. 250-500        
#>  8    46     1 TRUE         TRUE         FALSE            416. 250-500        
#>  9    23     1 TRUE         FALSE        FALSE            380. 250-500        
#> 10    13     1 TRUE         TRUE         TRUE             583. 500-1000       
#> # ℹ 265 more rows
#> # ℹ 5 more variables: lp.wl <dbl>, lp.ptx <dbl>, lu_score <dbl>, ov_rank <dbl>,
#> #   offer_rank <int>
cas_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1)
#> # A tibble: 275 × 13
#>     c_id  d_id match_single match_double abo_exact distance_nm proximity_class
#>    <int> <int> <lgl>        <lgl>        <lgl>           <dbl> <fct>          
#>  1     1     2 FALSE        TRUE         TRUE            1257. 1000-1500      
#>  2     1     6 TRUE         TRUE         TRUE             385. 250-500        
#>  3     1     8 TRUE         TRUE         TRUE             541. 500-1000       
#>  4     1    10 FALSE        TRUE         TRUE            1143. 1000-1500      
#>  5     2     1 TRUE         FALSE        TRUE             753. 500-1000       
#>  6     2     2 TRUE         TRUE         FALSE            395. 250-500        
#>  7     2     3 TRUE         FALSE        TRUE             178. <250           
#>  8     2     5 TRUE         TRUE         TRUE             690. 500-1000       
#>  9     2     6 TRUE         FALSE        FALSE           1397. 1000-1500      
#> 10     2     8 TRUE         TRUE         FALSE            615. 500-1000       
#> # ℹ 265 more rows
#> # ℹ 6 more variables: lp.wl <dbl>, lp.ptx <dbl>, lu_score <dbl>, ov_rank <dbl>,
#> #   pcost <dbl>, offer_rank <dbl>

## calculating LAS/CAS scores
calculate_las(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
#> # A tibble: 100 × 4
#>     c_id lp.wl lp.ptx lu_score
#>    <int> <dbl>  <dbl>    <dbl>
#>  1     1  1.74  0.750    0.352
#>  2     2  3.04  1.24     0.436
#>  3     3  2.43  0.403    0.398
#>  4     4  2.93  1.06     0.430
#>  5     5  8.02  1.71     0.930
#>  6     6  2.18  1.14     0.363
#>  7     7  4.22  1.45     0.624
#>  8     8  2.00  0.406    0.371
#>  9     9  3.40  1.83     0.456
#> 10    10  2.39  0.955    0.383
#> # ℹ 90 more rows
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = .15, peds_weight = .2, pld_weight = 0.05)
#> # A tibble: 100 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.239      51
#>  2     2    0.231      86
#>  3     3    0.246      32
#>  4     4    0.239      52
#>  5     5    0.448       2
#>  6     6    0.232      85
#>  7     7    0.258      21
#>  8     8    0.249      27
#>  9     9    0.219      99
#> 10    10    0.234      80
#> # ℹ 90 more rows

## One may know that CAS biological component is actually 3 of them. one for height, cpra, and blood type.
## Can instead specify these weights
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = NA, peds_weight = .2, pld_weight = 0.05,
                  abo_weight = 0.10, height_weight = 0.05, cpra_weight = 0)
#> # A tibble: 100 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.243      46
#>  2     2    0.232      86
#>  3     3    0.247      38
#>  4     4    0.243      45
#>  5     5    0.452       2
#>  6     6    0.236      75
#>  7     7    0.258      19
#>  8     8    0.249      31
#>  9     9    0.224      99
#> 10    10    0.233      83
#> # ℹ 90 more rows

## updating patients
set.seed(266388)
update_patients(syn_cands, model = "CAS23", elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 11)
#> $Dead
#> # A tibble: 2 × 9
#>    c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>   <int> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#> 1    68  4.91                5 D             4       1.00       1.00  0.997
#> 2    70  5.08                5 D             4       1.00       1.00  0.997
#> # ℹ 1 more variable: death <int>
#> 
#> $Alive
#> # A tibble: 98 × 9
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <int> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1     1  3.83               15 D            14       1.00       1.00  0.998
#>  2     2  3.90               15 D            14       1.00       1.00  0.998
#>  3     3  3.35               15 C            14       1.00       1.00  0.999
#>  4     4  3.49               15 D            14       1.00       1.00  0.999
#>  5     5  7.86               15 D            14       1.00       1.00  0.891
#>  6     6  4.11               15 D            14       1.00       1.00  0.997
#>  7     7  5.10               15 D            14       1.00       1.00  0.993
#>  8     8  2.99               15 C            14       1.00       1.00  0.999
#>  9     9  4.15               14 D            13       1.00       1.00  0.997
#> 10    10  3.76               14 D            13       1.00       1.00  0.998
#> # ℹ 88 more rows
#> # ℹ 1 more variable: death <int>
#> 
#> $Removed
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <int>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>
#> 
#> $new_char
#> # A tibble: 100 × 48
#>     c_id center listing_day  male dx_grp race_eth age_at_listing  airway  oxygen
#>    <int> <fct>        <int> <int> <chr>  <fct>             <dbl>   <dbl>   <dbl>
#>  1     1 2                1     1 D      NH White           55.2  1.19   -0.200 
#>  2     2 17               1     1 D      NH White           68.1 -0.254  -0.0935
#>  3     3 26               1     0 C      NH White           37.6  0.299   0.441 
#>  4     4 30               1     0 D      NH White           64.3 -0.806   0.876 
#>  5     5 38               1     1 D      NH Black           48.2 -0.0218 -0.0112
#>  6     6 41               1     1 D      Amer. I…           67.8  1.21    0.709 
#>  7     7 46               1     1 D      NH White           69.5  0.232  -0.864 
#>  8     8 47               1     1 C      NH White           23.4 -1.06   -0.220 
#>  9     9 10               2     1 D      NH White           68.0  0.231  -0.0419
#> 10    10 15               2     0 D      NH White           61.6  0.0120 -0.362 
#> # ℹ 90 more rows
#> # ℹ 39 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …





## transplanting
# set.seed(367473)

mz <- match_las(syn_cands, syn_dons, wl_model = "LAS15", post_tx_model = "LAS15", 
                wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
mz
#> # A tibble: 10 × 3
#>     d_id don_org data              
#>    <int> <fct>   <list>            
#>  1     1 DLU     <tibble [18 × 14]>
#>  2     2 DLU     <tibble [62 × 14]>
#>  3     3 DLU     <tibble [17 × 14]>
#>  4     4 DLU     <tibble [4 × 14]> 
#>  5     5 DLU     <tibble [20 × 14]>
#>  6     6 LUL     <tibble [23 × 14]>
#>  7     7 DLU     <tibble [3 × 14]> 
#>  8     8 DLU     <tibble [50 × 14]>
#>  9     9 DLU     <tibble [17 × 14]>
#> 10    10 DLU     <tibble [61 × 14]>

transplant_candidates(mz, rec_ids = NA, max_offer = NA)
#> # A tibble: 8 × 17
#>    d_id don_org  c_id match_single match_double abo_exact distance_nm
#>   <int> <fct>   <int> <lgl>        <lgl>        <lgl>           <dbl>
#> 1     1 DLU        23 TRUE         FALSE        FALSE            380.
#> 2     1 DLU         2 TRUE         FALSE        TRUE             753.
#> 3     2 DLU        48 TRUE         TRUE         TRUE             287.
#> 4     4 DLU        43 TRUE         TRUE         TRUE             272.
#> 5     5 DLU        26 TRUE         TRUE         TRUE             831.
#> 6     6 LUL         1 TRUE         TRUE         TRUE             385.
#> 7     8 DLU        64 TRUE         TRUE         TRUE             613.
#> 8    10 DLU        93 FALSE        TRUE         TRUE             402.
#> # ℹ 10 more variables: proximity_class <fct>, lp.wl <dbl>, lp.ptx <dbl>,
#> #   lu_score <dbl>, ov_rank <dbl>, offer_rank <int>, pred <dbl>, accept <int>,
#> #   surg_type <fct>, organs_rec <dbl>
## post-transplant
```
