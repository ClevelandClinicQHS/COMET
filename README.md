
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

You will be prompted to install this package if it is not installed
already. It is needed for `run_simulation` and
`gen_and_spawn_candidates/gen_and_spawn_donors`

``` r
devtools::install_github("ClevelandClinicQHS/cometdata")
```

## Basic example

This will simulate the 2015 Lung Allocation Score (LAS) policy for 400
days (`r1`) and then the same scenario under the Composite Allocation
Score (CAS) policy (`r2`).

``` r
library(COMET)

r1 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "LAS15", post_tx_model = "LAS15",
                     wl_weight = 2, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 2.620258 mins

r2 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_cas, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1826,
                     bio_weight = .15, pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 3.084634 mins
```

A simple way to evaluate the COMET runs, one can also look at the
statistics by diagnosis group or another variable. For details on what
is reported run `help('eval_simulation')`.

``` r
# see how many were transplanted, donor count, waitlist deaths
eval_simulation(r1)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4325     2709        297     141 1178       2411.967           211
#>   post_tx_years  tx_ppy  wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1       1400.06 1.12315 0.123136 0.1507078 204.0456 0.08194906      0      77
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     183         7      2827     2654        154.5     617

eval_simulation(r2)
#>   can_count tx_count wait_death removal cens wait_death_yrs post_tx_death
#> 1      4325     2690        273     125 1237       2442.491           174
#>   post_tx_years   tx_ppy   wld_ppy   ptd_ppy med_dist     pt_1yr pt_2yr med_wlt
#> 1      1415.871 1.101334 0.1117711 0.1228925 373.0126 0.08252788      0      77
#>   med_ptt med_offer don_count don_util nu_med_offer nu_orgs
#> 1     189         7      2827     2654          122     637

## See results by diagnosis group
eval_simulation(r1, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1226      861         26      29  310      1090.7598
#> 2      B       279      144         25      26   84       162.3847
#> 3      C       340      191         29      12  108       152.5832
#> 4      D      2480     1513        217      74  676      1006.2396
#>   post_tx_death post_tx_years    tx_ppy   wld_ppy    ptd_ppy med_dist
#> 1            60     450.52430 0.7893581 0.0238366 0.13317817 222.9906
#> 2            18      74.49692 0.8867832 0.1539554 0.24162073 181.6068
#> 3             8     100.80767 1.2517764 0.1900603 0.07935904 191.0525
#> 4           125     774.23135 1.5036181 0.2156544 0.16145045 198.4964
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.07665505      0   122.5   192.0        12      2827     2654        154.5
#> 2 0.10416667      0    76.0   181.5         6      2827     2654        154.5
#> 3 0.07853403      0    71.5   198.0         5      2827     2654        154.5
#> 4 0.08327826      0    64.0   179.0         5      2827     2654        154.5
#>   nu_orgs
#> 1     617
#> 2     617
#> 3     617
#> 4     617
eval_simulation(r2, group = dx_grp)
#>   dx_grp can_count tx_count wait_death removal cens wait_death_yrs
#> 1      A      1226      886         27      27  286      1048.5448
#> 2      B       279      136         22      16  105       175.8631
#> 3      C       340      257         13       6   64       123.6769
#> 4      D      2480     1411        211      76  782      1094.4066
#>   post_tx_death post_tx_years    tx_ppy    wld_ppy    ptd_ppy med_dist
#> 1            45     501.64545 0.8449806 0.02574997 0.08970479 361.1846
#> 2            12      69.56605 0.7733288 0.12509730 0.17249793 294.1683
#> 3            11     141.53046 2.0779946 0.10511257 0.07772179 370.7179
#> 4           106     703.12936 1.2892832 0.19279855 0.15075462 395.7196
#>       pt_1yr pt_2yr med_wlt med_ptt med_offer don_count don_util nu_med_offer
#> 1 0.10496614      0     103   213.5        10      2827     2654          122
#> 2 0.08823529      0      97   175.0         8      2827     2654          122
#> 3 0.10116732      0      33   202.0         4      2827     2654          122
#> 4 0.06449327      0      69   176.0         7      2827     2654          122
#>   nu_orgs
#> 1     637
#> 2     637
#> 3     637
#> 4     637
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
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 2.530677 mins

r4 <- run_simulation(days = 400, can_start = 1000,
                     match_alg = match_las, wl_model = "CAS23", post_tx_model = "CAS23",
                     wl_weight = 1, post_tx_weight = 1, wl_cap = 365, post_tx_cap = 365, seed = 26638)
#> 10% done 20% done 30% done 40% done 50% done 60% done 70% done 80% done 90% done 100% done Time difference of 2.538058 mins
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
#>    c_id center listing_day  male dx_grp race_eth age_at_listing   airway  oxygen
#>   <int> <fct>        <int> <int> <chr>  <fct>             <dbl>    <dbl>   <dbl>
#> 1     1 7                1     0 B      NH White           61.4  1.44    -0.330 
#> 2     2 8                1     0 D      NH Black           44.9 -0.269   -0.0946
#> 3     3 15               1     1 D      NH White           55.0  0.390    1.24  
#> 4     4 33               1     1 D      NH White           78.0  0.00234  0.757 
#> 5     5 42               1     0 A      NH White           65.4 -0.885    0.364 
#> 6     6 18               2     1 D      Hispanic           65.8  0.0885  -0.322 
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
#> 1     1     1078            1     1 Asian      68     173. 2     O        35
#> 2     2     1413            1     1 Hispanic   70     178. 3     O        30
#> 3     3     1638            1     0 NH White   61.5   156. 3     O        20
#> 4     4      467            1     1 Hispanic   64     163. 2     B        40
#> 5     5      833            1     0 NH White   67     170. 2     O        44
#> 6     6      945            1     1 NH White   65     165. 2     A        53
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
                 wl_cap = 365, post_tx_cap = 1826, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

l_2 <- iteration(402, syn_cands, syn_dons, include_matches = FALSE, updated_list = l_1, match_alg = match_cas,
                 wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
                 wl_cap = 365, post_tx_cap = 1826, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

nrow(l_2$waitlist_death_database) - nrow(l_1$waitlist_death_database)
#> [1] 1
nrow(l_2$post_tx_death_database)-nrow(l_1$post_tx_death_database)
#> [1] 1
```

Looking in depth, 1 died on the waiting list, and 1 died post transplant
on Day 402 in this scenario. The following would run ten days
sequentially

``` r
l_i <- l
for(i in 401:410){
  l_i <- iteration(i,
                   syn_cands, syn_dons, include_matches = FALSE, updated_list = l_i,
                   match_alg = match_cas,  wl_model = "CAS23", post_tx_model = "CAS23",
                   wl_weight = 0.25, post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1826,
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
#> # A tibble: 3 × 10
#>    c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>   <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#> 1   966  2.40              281 B           280      0.996      0.996  1.00 
#> 2  1128  4.48              259 D           258      0.997      0.997  0.999
#> 3  3304  4.16                3 D             2      1.00       1.00   0.999
#> # ℹ 2 more variables: death <int>, max_days <dbl>
#> 
#> $Alive
#> # A tibble: 1,233 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   284  4.75              364 B           363      0.995      0.995  0.998
#>  2   286  4.67              364 B           363      0.995      0.995  0.998
#>  3   292  3.83              363 B           362      0.995      0.995  0.999
#>  4   295  4.40              363 D           362      0.995      0.995  0.999
#>  5   296  4.61              363 D           362      0.995      0.995  0.999
#>  6   304  3.03              362 D           361      0.995      0.995  1.00 
#>  7   307  2.53              362 A           361      0.995      0.995  1.00 
#>  8   320  4.02              361 D           360      0.995      0.995  0.999
#>  9   323  3.81              360 A           359      0.995      0.995  0.999
#> 10   343  4.42              359 D           358      0.995      0.995  0.999
#> # ℹ 1,223 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>
#> 
#> $Removed
#> # A tibble: 1 × 8
#>    c_id dx_grp days_on_waitlist days_ago Survival_y Survival_x cond_x removal
#>   <dbl> <chr>             <dbl>    <dbl>      <dbl>      <dbl>  <dbl>   <int>
#> 1  3259 B                     9        8      0.992      0.993  0.999       1
#> 
#> $new_char
#> # A tibble: 1,237 × 48
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing  airway  oxygen
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>   <dbl>   <dbl>
#>  1 A       -999       -5731 26         1 Asian              56.3 -0.941   0.0101
#>  2 A       -990       -2575 44         0 NH White           66.9 -0.781   0.0624
#>  3 A       -989       -2467 47         0 NH White           63.8 -0.487  -0.200 
#>  4 A       -987       -2300 17         1 NH White           59.1 -0.0230 -0.190 
#>  5 D       -984       -2238 23         1 NH White           67.1  1.38    0.203 
#>  6 A       -982       -2124 16         0 NH White           60.5  0.0386 -0.0573
#>  7 A       -979       -1978 34         0 NH White           67.6 -0.591   0.429 
#>  8 A       -978       -1970 21         1 NH White           63.9 -1.06    0.111 
#>  9 A       -969       -1816 49         0 NH White           61.4 -0.399   0.195 
#> 10 A       -967       -1802 24         0 NH White           62.6 -0.703   0.462 
#> # ℹ 1,227 more rows
#> # ℹ 39 more variables: abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …

identify_deaths(patients = r2cc, model = "CAS23r",
                elapsed_time = days_on_waitlist, pre_tx = TRUE, cap = 365, date = 401)
#> # A tibble: 1,237 × 10
#>     c_id    lp days_on_waitlist dx_grp days_ago Survival_y Survival_x cond_x
#>    <dbl> <dbl>            <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl>
#>  1   284  4.75              364 B           363      0.995      0.995  0.998
#>  2   286  4.67              364 B           363      0.995      0.995  0.998
#>  3   292  3.83              363 B           362      0.995      0.995  0.999
#>  4   295  4.40              363 D           362      0.995      0.995  0.999
#>  5   296  4.61              363 D           362      0.995      0.995  0.999
#>  6   304  3.03              362 D           361      0.995      0.995  1.00 
#>  7   307  2.53              362 A           361      0.995      0.995  1.00 
#>  8   320  4.02              361 D           360      0.995      0.995  0.999
#>  9   323  3.81              360 A           359      0.995      0.995  0.999
#> 10   343  4.42              359 D           358      0.995      0.995  0.999
#> # ℹ 1,227 more rows
#> # ℹ 2 more variables: death <int>, max_days <dbl>

identify_removals(patients = r2cc, elapsed_time = days_on_waitlist, cap = 2365)
#> # A tibble: 0 × 8
#> # ℹ 8 variables: c_id <dbl>, dx_grp <chr>, days_on_waitlist <dbl>,
#> #   days_ago <dbl>, Survival_y <dbl>, Survival_x <dbl>, cond_x <dbl>,
#> #   removal <int>

## post-transplant deaths
update_patients(r2rd, model = "CAS23r", 
                elapsed_time = days_after_tx, pre_tx = FALSE, cap = 1826, date = 401)
#> $Dead
#> # A tibble: 1 × 9
#>    c_id    lp days_after_tx dx_grp days_ago Survival_y Survival_x cond_x death
#>   <dbl> <dbl>         <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl> <int>
#> 1   491 0.677           164 D           163      0.965      0.965   1.00     1
#> 
#> $Alive
#> # A tibble: 2,515 × 9
#>     c_id     lp days_after_tx dx_grp days_ago Survival_y Survival_x cond_x death
#>    <dbl>  <dbl>         <dbl> <chr>     <dbl>      <dbl>      <dbl>  <dbl> <int>
#>  1 -1000  0.423           303 B           302      0.942      0.942   1.00     0
#>  2  -998  0.296           384 A           383      0.930      0.930   1.00     0
#>  3  -995  0.481           363 D           362      0.934      0.934   1.00     0
#>  4  -986  0.494           308 A           307      0.941      0.941   1.00     0
#>  5  -985  0.274           366 A           365      0.933      0.933   1.00     0
#>  6  -980  0.652           375 A           374      0.932      0.932   1.00     0
#>  7  -975  0.493           338 A           337      0.937      0.937   1.00     0
#>  8  -974 -0.180           347 D           346      0.936      0.936   1.00     0
#>  9  -972  0.513           345 A           344      0.936      0.936   1.00     0
#> 10  -968  0.287           383 D           382      0.930      0.930   1.00     0
#> # ℹ 2,505 more rows
#> 
#> $Removed
#> # A tibble: 0 × 62
#> # ℹ 62 variables: c_id <dbl>, dx_grp <chr>, listing_day <dbl>, center <fct>,
#> #   male <int>, race_eth <fct>, age_at_listing <dbl>, airway <dbl>,
#> #   oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>, wgt_kg <dbl>,
#> #   bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>, fev1 <dbl>,
#> #   fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>, o2_freq <dbl>,
#> #   vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>, …
#> 
#> $new_char
#> # A tibble: 2,516 × 62
#>     c_id dx_grp listing_day center  male race_eth age_at_listing   airway
#>    <dbl> <chr>        <dbl> <fct>  <int> <fct>             <dbl>    <dbl>
#>  1 -1000 B            -5908 61         1 NH White           30.3  0.940  
#>  2  -998 A            -3757 8          0 Hispanic           50.3 -0.949  
#>  3  -997 A            -3713 45         1 NH White           65.5 -0.00918
#>  4  -995 D            -3298 40         0 NH White           30.9  0.857  
#>  5  -994 A            -3200 55         1 NH White           58.1 -0.802  
#>  6  -992 A            -2816 46         0 NH Black           51.4 -0.105  
#>  7  -991 D            -2704 2          0 NH White           47.8 -0.244  
#>  8  -988 A            -2423 22         0 NH White           63.8 -0.0923 
#>  9  -986 A            -2245 24         0 NH White           59.8 -0.664  
#> 10  -985 A            -2244 55         1 NH White           52.4 -0.586  
#> # ℹ 2,506 more rows
#> # ℹ 54 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
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
                 post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1826, bio_weight = .15,
                 pld_weight = 0.05, peds_weight = 0.2, efficiency_weight = 0.1)

mtc
#> # A tibble: 6 × 3
#>    d_id don_org data               
#>   <int> <fct>   <list>             
#> 1  2828 LUR     <tibble [174 × 12]>
#> 2  2829 DLU     <tibble [398 × 12]>
#> 3  2830 LUL     <tibble [127 × 12]>
#> 4  2831 DLU     <tibble [82 × 12]> 
#> 5  2832 DLU     <tibble [637 × 12]>
#> 6  2833 DLU     <tibble [219 × 12]>
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
#> # A tibble: 3,423 × 4
#>     c_id  d_id match_single match_double
#>    <dbl> <int> <lgl>        <lgl>       
#>  1  -999  2828 TRUE         TRUE        
#>  2  -999  2829 TRUE         FALSE       
#>  3  -999  2831 TRUE         TRUE        
#>  4  -999  2832 TRUE         TRUE        
#>  5  -999  2833 TRUE         TRUE        
#>  6  -990  2828 TRUE         TRUE        
#>  7  -990  2829 TRUE         TRUE        
#>  8  -990  2831 FALSE        TRUE        
#>  9  -990  2832 TRUE         TRUE        
#> 10  -990  2833 FALSE        TRUE        
#> # ℹ 3,413 more rows
## returns if compabitible for single, double transplant, if a double transplant they will match for double lung
am1 <- abo_screen(cands = r2cc, dons = dons_401)
am1
#> # A tibble: 5,445 × 3
#>     c_id  d_id abo_exact
#>    <dbl> <int> <lgl>    
#>  1  -999  2828 TRUE     
#>  2  -999  2829 TRUE     
#>  3  -999  2830 TRUE     
#>  4  -999  2832 TRUE     
#>  5  -990  2828 FALSE    
#>  6  -990  2829 FALSE    
#>  7  -990  2830 FALSE    
#>  8  -990  2832 FALSE    
#>  9  -990  2833 TRUE     
#> 10  -989  2828 TRUE     
#> # ℹ 5,435 more rows
## returns abo_exact abo compatible organs since that is a tie breaker in the LAS

## Ensure double with double, singles may take a double if only is assigned or either can
cm1 <- count_screen(cands = r2cc, dons = dons_401)
cm1
#> # A tibble: 5,648 × 2
#>     c_id  d_id
#>    <dbl> <int>
#>  1  -999  2828
#>  2  -999  2829
#>  3  -999  2830
#>  4  -999  2831
#>  5  -999  2832
#>  6  -999  2833
#>  7  -990  2829
#>  8  -990  2831
#>  9  -990  2832
#> 10  -990  2833
#> # ℹ 5,638 more rows

## Distance calculation 
lm1 <- dist_calc(cands = r2cc, dons = dons_401)
lm1
#> # A tibble: 7,422 × 3
#>     c_id  d_id distance_nm
#>    <dbl> <int>       <dbl>
#>  1  -999  2828        143.
#>  2  -999  2829       1345.
#>  3  -999  2830        767.
#>  4  -999  2831        780.
#>  5  -999  2832        596.
#>  6  -999  2833       2063.
#>  7  -990  2828       1167.
#>  8  -990  2829        168.
#>  9  -990  2830        668.
#> 10  -990  2831        849.
#> # ℹ 7,412 more rows
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
                         post_tx_weight = 0.25, wl_cap = 365, post_tx_cap = 1826, bio_weight = .15,
                         pld_weight = 0.05, peds_weight = 0.2)

lr1
#> # A tibble: 1,237 × 3
#>     c_id lu_score ov_rank
#>    <dbl>    <dbl>   <dbl>
#>  1  -999    0.191     978
#>  2  -990    0.161    1229
#>  3  -989    0.184    1114
#>  4  -987    0.207     455
#>  5  -984    0.179    1158
#>  6  -982    0.211     260
#>  7  -979    0.198     797
#>  8  -978    0.207     424
#>  9  -969    0.193     949
#> 10  -967    0.184    1100
#> # ℹ 1,227 more rows
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
#> # A tibble: 1,637 × 10
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -999  2828 TRUE         TRUE         TRUE             143.    0.279     978
#>  2  -999  2829 TRUE         FALSE        TRUE            1345.    0.252     978
#>  3  -999  2832 TRUE         TRUE         TRUE             596.    0.273     978
#>  4  -990  2829 TRUE         TRUE         FALSE            168.    0.249    1229
#>  5  -990  2832 TRUE         TRUE         FALSE            745.    0.240    1229
#>  6  -990  2833 FALSE        TRUE         TRUE             858.    0.237    1229
#>  7  -989  2829 TRUE         TRUE         TRUE            1127.    0.252    1114
#>  8  -989  2832 FALSE        TRUE         TRUE             453.    0.267    1114
#>  9  -984  2829 TRUE         TRUE         TRUE             679.    0.258    1158
#> 10  -984  2832 FALSE        TRUE         TRUE             281.    0.265    1158
#> # ℹ 1,627 more rows
#> # ℹ 2 more variables: pcost <dbl>, offer_rank <dbl>

set.seed(26638)
acceptance_prob(offs, dons = dons_401, cands = r2cc)
#> # A tibble: 1,637 × 12
#>     c_id  d_id match_single match_double abo_exact distance_nm lu_score ov_rank
#>    <dbl> <int> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>   <dbl>
#>  1  -999  2828 TRUE         TRUE         TRUE             143.    0.279     978
#>  2  -999  2829 TRUE         FALSE        TRUE            1345.    0.252     978
#>  3  -999  2832 TRUE         TRUE         TRUE             596.    0.273     978
#>  4  -990  2829 TRUE         TRUE         FALSE            168.    0.249    1229
#>  5  -990  2832 TRUE         TRUE         FALSE            745.    0.240    1229
#>  6  -990  2833 FALSE        TRUE         TRUE             858.    0.237    1229
#>  7  -989  2829 TRUE         TRUE         TRUE            1127.    0.252    1114
#>  8  -989  2832 FALSE        TRUE         TRUE             453.    0.267    1114
#>  9  -984  2829 TRUE         TRUE         TRUE             679.    0.258    1158
#> 10  -984  2832 FALSE        TRUE         TRUE             281.    0.265    1158
#> # ℹ 1,627 more rows
#> # ℹ 4 more variables: pcost <dbl>, offer_rank <dbl>, pred <dbl>, accept <int>

transplant_candidates(matches = mtc)
#> # A tibble: 7 × 15
#>    d_id don_org  c_id match_single match_double abo_exact distance_nm lu_score
#>   <int> <fct>   <dbl> <lgl>        <lgl>        <lgl>           <dbl>    <dbl>
#> 1  2828 LUR      -999 TRUE         TRUE         TRUE          143.       0.279
#> 2  2829 DLU      3324 TRUE         FALSE        TRUE         1130.       0.352
#> 3  2829 DLU      3158 TRUE         TRUE         FALSE           0.350    0.305
#> 4  2830 LUL      2017 TRUE         FALSE        TRUE          142.       0.319
#> 5  2831 DLU      3243 TRUE         TRUE         TRUE          262.       0.303
#> 6  2832 DLU      3077 TRUE         TRUE         TRUE          248.       0.306
#> 7  2833 DLU      2778 TRUE         TRUE         TRUE          220.       0.304
#> # ℹ 7 more variables: ov_rank <dbl>, pcost <dbl>, offer_rank <dbl>, pred <dbl>,
#> #   accept <int>, surg_type <fct>, organs_rec <dbl>
```

## Other tips and tricks

You will notice a `pra_screen` function. This is a place-holder for now
that returns a grid of every candidate and donor pairing. Future
implementation will incorporate PRA screening criteria.

``` r
pra_screen(cands = r2cc, dons = dons_401)
#> # A tibble: 7,422 × 2
#>     c_id  d_id
#>    <dbl> <int>
#>  1  -999  2828
#>  2  -999  2829
#>  3  -999  2830
#>  4  -999  2831
#>  5  -999  2832
#>  6  -999  2833
#>  7  -990  2828
#>  8  -990  2829
#>  9  -990  2830
#> 10  -990  2831
#> # ℹ 7,412 more rows
```

If you want to return the matches along with the simulated output. All
one has to do in the `run_simulation` or `iteration` function is set
`include_matches = TRUE`. The output can be seen below is a nested
tibble with a vector of donor ids (`d_id`) that are ranked in order for
the subsequent offer, called `all_matches`

``` r
l_1m <- iteration(401, syn_cands, syn_dons, include_matches = TRUE, updated_list = l, match_alg = match_cas,
                 wl_model = "CAS23", post_tx_model = "CAS23", wl_weight = 0.25, post_tx_weight = 0.25,
                 wl_cap = 365, post_tx_cap = 1826, bio_weight = .15, pld_weight = 0.05,
                 peds_weight = 0.2, efficiency_weight = 0.1)

l_1m$all_matches
#> # A tibble: 6 × 3
#>    d_id don_org data              
#>   <int> <fct>   <list>            
#> 1  2828 LUR     <tibble [174 × 1]>
#> 2  2829 DLU     <tibble [398 × 1]>
#> 3  2830 LUL     <tibble [127 × 1]>
#> 4  2831 DLU     <tibble [82 × 1]> 
#> 5  2832 DLU     <tibble [637 × 1]>
#> 6  2833 DLU     <tibble [219 × 1]>
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
concat2(r1, min_enroll_date = 31, max_enroll_date = 300, wl_censor_date = 300, post_tx_censor_date = 330)
#> # A tibble: 3,221 × 77
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
#> # ℹ 3,211 more rows
#> # ℹ 69 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
#> #   pap_syst <dbl>, ci <dbl>, funstat <dbl>, ecmo <dbl>, cvp <dbl>,
#> #   pco2_15 <dbl>, pra <dbl>, pld <dbl>, creat_150 <dbl>, bili_50 <dbl>, …
```

The function `spec_day` will allow one to grab the waiting list on a
specific day.

``` r
spec_day(r1, day = 300)
#> # A tibble: 1,143 × 77
#>    dx_grp  c_id listing_day center  male race_eth age_at_listing   airway
#>    <chr>  <dbl>       <dbl> <fct>  <int> <fct>             <dbl>    <dbl>
#>  1 A       -997       -3713 45         1 NH White           65.5 -0.00918
#>  2 A       -996       -3648 6          0 NH White           63.1 -0.668  
#>  3 D       -995       -3298 40         0 NH White           30.9  0.857  
#>  4 A       -989       -2467 47         0 NH White           63.8 -0.487  
#>  5 A       -979       -1978 34         0 NH White           67.6 -0.591  
#>  6 A       -978       -1970 21         1 NH White           63.9 -1.06   
#>  7 A       -971       -1854 54         0 NH White           66.4 -0.457  
#>  8 A       -970       -1828 59         1 NH White           65.5 -0.361  
#>  9 B       -964       -1745 34         0 NH White           46.4  1.05   
#> 10 C       -962       -1722 45         0 NH White           51.5 -0.307  
#> # ℹ 1,133 more rows
#> # ℹ 69 more variables: oxygen <dbl>, abo <chr>, hgt_in <dbl>, hgt_cm <dbl>,
#> #   wgt_kg <dbl>, bmi <dbl>, resp_supp <fct>, surg_type <fct>, diab <int>,
#> #   fev1 <dbl>, fvc <dbl>, pco2 <dbl>, pf <dbl>, po2 <dbl>, pap_mean <dbl>,
#> #   o2_freq <dbl>, vent <fct>, walk6m <dbl>, o2 <int>, bili <dbl>, creat <dbl>,
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
#> # A tibble: 1,237 × 3
#>     c_id    lp expected
#>    <dbl> <dbl>    <dbl>
#>  1  -999  3.48     332.
#>  2  -990  2.35     354.
#>  3  -989  2.78     348.
#>  4  -987  3.26     338.
#>  5  -984  3.72     324.
#>  6  -982  1.97     357.
#>  7  -979  2.67     350.
#>  8  -978  3.28     338.
#>  9  -969  1.99     357.
#> 10  -967  1.88     358.
#> # ℹ 1,227 more rows
rmst("CAS23", cand_data = r2cc, cap = 1826, wl = FALSE)
#> # A tibble: 1,237 × 3
#>     c_id    lp expected
#>    <dbl> <dbl>    <dbl>
#>  1  -999 0.792    1337.
#>  2  -990 1.18     1166.
#>  3  -989 0.907    1290.
#>  4  -987 0.616    1403.
#>  5  -984 1.04     1233.
#>  6  -982 0.417    1470.
#>  7  -979 0.813    1329.
#>  8  -978 0.779    1342.
#>  9  -969 0.715    1367.
#> 10  -967 0.792    1337.
#> # ℹ 1,227 more rows
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
