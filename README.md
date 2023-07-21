
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
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 2.815812 mins

## Simulates the CAS rules
r2 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.248416 mins

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
#> 1      4258     2758        321     136 1043       2440.802           219
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy     pt_1yr pt_2yr med_wlt med_ptt
#> 1       1400.81 1.129956 0.1315141 0.1563381 0.06925308      0      82     179
#>   med_offer don_count don_util nu_med_offer nu_orgs
#> 1         5      2904     2687          138     552

eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1222      876         33      27  286      1152.6516
#> 2      B       276      168         20      17   71       155.2197
#> 3      C       366      244         20      12   90       181.0157
#> 4      D      2394     1470        248      80  596       951.9151
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy   ptd_ppy     pt_1yr pt_2yr
#> 1            63     458.54073 0.7599868 0.02862964 0.1373924 0.07191781      0
#> 2            17      85.62628 1.0823368 0.12884961 0.1985372 0.05952381      0
#> 3            14     121.45106 1.3479491 0.11048763 0.1152728 0.06557377      0
#> 4           125     735.19233 1.5442553 0.26052743 0.1700235 0.06938776      0
#>   med_wlt med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1   134.0   191.0        10      2904     2687          138     552
#> 2    81.0   179.0         3      2904     2687          138     552
#> 3    83.0   170.0         5      2904     2687          138     552
#> 4    62.5   175.5         4      2904     2687          138     552
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
#>  1     2     8     245.    <250           
#>  2     2    10      65.5   <250           
#>  3     3     2     118.    <250           
#>  4     3     3       0.207 <250           
#>  5     4     2     232.    <250           
#>  6     4     3     186.    <250           
#>  7     6     6       6.02  <250           
#>  8     8     5     218.    <250           
#>  9     8     7     201.    <250           
#> 10     8     8     232.    <250           
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
#>  1     2     1 TRUE         FALSE        TRUE            1483. 1000-1500      
#>  2    71     1 TRUE         FALSE        TRUE            1342. 1000-1500      
#>  3    79     1 TRUE         TRUE         TRUE            1065. 1000-1500      
#>  4    89     1 TRUE         TRUE         TRUE            1399. 1000-1500      
#>  5    50     1 TRUE         TRUE         TRUE            1399. 1000-1500      
#>  6    41     1 TRUE         TRUE         TRUE            1354. 1000-1500      
#>  7    30     1 TRUE         TRUE         TRUE            1343. 1000-1500      
#>  8    28     1 TRUE         TRUE         FALSE           1364. 1000-1500      
#>  9    13     1 TRUE         TRUE         TRUE            1525. 1500-2500      
#> 10    26     1 TRUE         FALSE        TRUE            1520. 1500-2500      
#> # ℹ 265 more rows
#> # ℹ 5 more variables: lp.wl <dbl>, lp.ptx <dbl>, lu_score <dbl>, ov_rank <dbl>,
#> #   offer_rank <int>
cas_offer_rank(hm1, am1, cm1, lm1, overall_ranking = lr1)
#> # A tibble: 275 × 13
#>     c_id  d_id match_single match_double abo_exact distance_nm proximity_class
#>    <int> <int> <lgl>        <lgl>        <lgl>           <dbl> <fct>          
#>  1     1     2 FALSE        TRUE         TRUE             262. 250-500        
#>  2     1     6 TRUE         TRUE         TRUE            1592. 1500-2500      
#>  3     1     8 TRUE         TRUE         TRUE             700. 500-1000       
#>  4     1    10 FALSE        TRUE         TRUE             528. 500-1000       
#>  5     2     1 TRUE         FALSE        TRUE            1483. 1000-1500      
#>  6     2     2 TRUE         TRUE         FALSE            494. 250-500        
#>  7     2     3 TRUE         FALSE        TRUE             376. 250-500        
#>  8     2     5 TRUE         TRUE         TRUE             440. 250-500        
#>  9     2     6 TRUE         FALSE        FALSE           1450. 1000-1500      
#> 10     2     8 TRUE         TRUE         FALSE            245. <250           
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
#> # A tibble: 7 × 17
#>    d_id don_org  c_id match_single match_double abo_exact distance_nm
#>   <int> <fct>   <int> <lgl>        <lgl>        <lgl>           <dbl>
#> 1     1 DLU        13 TRUE         TRUE         TRUE            1525.
#> 2     2 DLU        96 TRUE         TRUE         FALSE            118.
#> 3     4 DLU        23 TRUE         TRUE         TRUE             472.
#> 4     5 DLU         2 TRUE         TRUE         TRUE             440.
#> 5     6 LUL        18 TRUE         FALSE        FALSE            836.
#> 6     8 DLU        49 TRUE         TRUE         FALSE            232.
#> 7    10 DLU        38 FALSE        TRUE         TRUE             229.
#> # ℹ 10 more variables: proximity_class <fct>, lp.wl <dbl>, lp.ptx <dbl>,
#> #   lu_score <dbl>, ov_rank <dbl>, offer_rank <int>, pred <dbl>, accept <int>,
#> #   surg_type <fct>, organs_rec <dbl>
## post-transplant
```
