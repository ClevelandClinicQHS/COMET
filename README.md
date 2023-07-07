
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
#> 10% done
#> 20% done
#> 30% done
#> 40% done
#> 50% done
#> 60% done
#> 70% done
#> 80% done
#> 90% done
#> 100% done
#> Time difference of 42.51077 secs

## Simulates the CAS rules
r2 <- run_simulation(days = 20, can_start = 1000, waitlist_update_freq = 1, post_tx_update_freq = 1,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1825,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 367473)
#> 10% done
#> 20% done
#> 30% done
#> 40% done
#> 50% done
#> 60% done
#> 70% done
#> 80% done
#> 90% done
#> 100% done
#> Time difference of 35.51084 secs

## You can mix and match systems, change weights
## for example simulate the LAS using the CAS models

## or maybe you want to test simualte what would happen if waitlist was weighted the same as post-transplatn under the LAS

## you can change the weights as well in the CAS, 
```

## A comparison of the models

``` r
# eval_simulation()
# see how many were transplanted, donor count, waitlist deaths
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
#> # A tibble: 7,293 × 4
#>     c_id  d_id match_single match_double
#>    <int> <int> <lgl>        <lgl>       
#>  1     3     1 TRUE         TRUE        
#>  2     7     1 TRUE         TRUE        
#>  3     9     1 TRUE         TRUE        
#>  4    10     1 TRUE         TRUE        
#>  5    14     1 TRUE         TRUE        
#>  6    18     1 TRUE         FALSE       
#>  7    20     1 TRUE         TRUE        
#>  8    21     1 TRUE         TRUE        
#>  9    23     1 TRUE         TRUE        
#> 10    27     1 TRUE         TRUE        
#> # ℹ 7,283 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
abo_screen(syn_cands, syn_dons)
#> # A tibble: 9,920 × 3
#>     c_id  d_id abo_exact
#>    <int> <int> <lgl>    
#>  1     1     6 TRUE     
#>  2     1     7 TRUE     
#>  3     1     9 TRUE     
#>  4     1    10 TRUE     
#>  5     1    11 TRUE     
#>  6     1    13 TRUE     
#>  7     1    14 TRUE     
#>  8     1    17 TRUE     
#>  9     2     6 TRUE     
#> 10     2     7 TRUE     
#> # ℹ 9,910 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
# count_screen()

## Distance calculation 

## matching

## ranking

## calculating LAS/CAS scores
calculate_las(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 2, post_tx_weight = 1)
#> # A tibble: 840 × 4
#>     c_id lp.wl lp.ptx lu_score
#>    <int> <dbl>  <dbl>    <dbl>
#>  1     1  1.52  1.04     0.336
#>  2     2  3.05  0.655    0.455
#>  3     3  1.77  0.750    0.353
#>  4     4  3.51  1.17     0.504
#>  5     5  1.88  0.788    0.357
#>  6     6  3.01  0.660    0.449
#>  7     7  2.62  1.47     0.382
#>  8     8  1.65  0.746    0.349
#>  9     9  1.90  0.909    0.355
#> 10    10  2.78  0.736    0.422
#> # ℹ 830 more rows
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = .15, peds_weight = .2, pld_weight = 0.05)
#> # A tibble: 840 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.234     695
#>  2     2    0.249     208
#>  3     3    0.239     501
#>  4     4    0.245     279
#>  5     5    0.241     405
#>  6     6    0.248     228
#>  7     7    0.224     810
#>  8     8    0.239     508
#>  9     9    0.236     610
#> 10    10    0.244     308
#> # ℹ 830 more rows

## One may know that CAS biological component is actually 3 of them. one for height, cpra, and blood type.
## Can instead specify these weights
calculate_sub_cas(syn_cands, wl_model = "LAS15", post_tx_model = "LAS15", wl_cap = 365, post_tx_cap = 365, wl_weight = 0.25,
                  post_tx_weight = 0.25, bio_weight = NA, peds_weight = .2, pld_weight = 0.05,
                  abo_weight = 0.10, height_weight = 0.05, cpra_weight = 0)
#> # A tibble: 840 × 3
#>     c_id lu_score ov_rank
#>    <int>    <dbl>   <dbl>
#>  1     1    0.236     618
#>  2     2    0.253     184
#>  3     3    0.243     384
#>  4     4    0.249     234
#>  5     5    0.243     397
#>  6     6    0.252     189
#>  7     7    0.228     788
#>  8     8    0.234     711
#>  9     9    0.240     477
#> 10    10    0.248     252
#> # ℹ 830 more rows

## updating patients

## transplanting

## post-transplant
```
