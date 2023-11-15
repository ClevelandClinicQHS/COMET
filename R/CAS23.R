#' Calculate LAS/CAS Linear Predictors
#'
#' @name calc_las
#'
#' @param data dataset of patient data
#' @param c_id unique candidate identifier
#' @param dx_grp diagnosis group (A, B, C, or D)
#' @param dx diagnosis (numeric, see details for ones in LAS/CAS)
#' @param age age in years
#' @param bmi \ifelse{html}{\out{(body mass index (kg/m<sup>2</sup>)}}{\eqn{\textrm{body mass index (kg/m}^{2})}}
#' @param funstat functional status (1 = No assistance needed, 2 = some assistance needed, 3 = total assistance needed)
#' @param walk6m six minute walk distance (ft)
#' @param o2rest amount of oxygen received if needed while resting (L/min)
#' @param pap_syst systolic pulmonary artery pressure (mmHg)
#' @param pap_mean mean pulmonary artery pressure (mmHg)
#' @param pco2 \ifelse{html}{\out{pCO<sub>2</sub> (mmHg)}}{\eqn{\textrm{pCO}_{2}\textrm{ (mmHg)}}}
#' @param pco2_15 \ifelse{html}{\out{increase in pCO<sub>2</sub> by 15 percent (1,0)}}{\eqn{\textrm{increase in pCO}_{2}\textrm{ by 15 percent (1,0)}}}
#' @param cont_mech continuous mechangical ventilation (0 or 1)
#' @param creat creatinine (mg/dL)
#' @param bili bilirubin (mg/dL)
#' @param ecmo ECMO (0 or 1)
#'
#' @return a dataset with c_id and linear predictor for waitlist or post-transplant linear predictor according to LAS/CAS equations
#'
#' @details Numeric Diagnosis Options
#' * 1608: bronchiestais
#' * 1605:  sarcoidosis
#' * 1519,1613: pulmonary fibrosis (non-idiopathetic),
#' * 1611: lymphangioleiomyomatosis
#' * 1612: Obliterative bronchiolitis
#' * 1500,1501,1502,1548,1549: eisenmenger's
#' * 1617: COVID caused fibrosis
#' @md
#'
#' @export
#'
#' @examples
#' calc_wl_cas23(syn_cands)
#' calc_post_tx_cas23(syn_cands)
#' calc_wl_las21(syn_cands)
#' calc_post_tx_las21(syn_cands)
#' calc_wl_las15(syn_cands)
#' calc_post_tx_las15(syn_cands)
calc_wl_cas23 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, bmi = bmi, funstat = funstat, walk6m = walk6m,
                          o2rest = o2rest, pap_syst = pap_syst, pap_mean = pap_mean, pco2 = pco2, pco2_15 = pco2_15, cont_mech = cont_mech,
                          creat = creat, bili = bili, ecmo = ecmo){

  ## identifies variables
  c_id <- enquo(c_id)
  dx_grp <- enquo(dx_grp)
  dx <- enquo(dx)
  age <- enquo(age)
  bmi <- enquo(bmi)
  funstat <- enquo(funstat)
  walk6m <- enquo(walk6m)
  o2rest <- enquo(o2rest)
  pap_syst <- enquo(pap_syst)
  pap_mean <- enquo(pap_mean)
  pco2_15 <- enquo(pco2_15)
  pco2 <- enquo(pco2)
  cont_mech <- enquo(cont_mech)
  creat <- enquo(creat)
  bili <- enquo(bili)
  ecmo <- enquo(ecmo)

  c_id <- eval_tidy(c_id, data)
  dx_grp <- eval_tidy(dx_grp, data)
  dx <- eval_tidy(dx, data)
  age <- eval_tidy(age, data)
  bmi <- eval_tidy(bmi, data)
  funstat <- eval_tidy(funstat, data)
  walk6m <- eval_tidy(walk6m, data)
  o2rest <- eval_tidy(o2rest, data)
  pap_syst <- eval_tidy(pap_syst, data)
  pap_mean <- eval_tidy(pap_mean, data)
  pco2_15 <- eval_tidy(pco2_15, data)
  pco2 <- eval_tidy(pco2, data)
  cont_mech <- eval_tidy(cont_mech, data)
  creat <- eval_tidy(creat, data)
  bili <- eval_tidy(bili, data)
  ecmo <- eval_tidy(ecmo, data)

  if(any(duplicated(c_id))){stop(paste0(sum(duplicated(c_id)), " candidates have duplicated c_id value\nPlease give each candidate a unique identifier"))}

  data2 <- tibble(c_id, dx_grp, dx, age, bmi, funstat, walk6m, o2rest, pap_syst,
                  pap_mean, pco2_15, pco2, cont_mech, creat, bili, ecmo) |>
    ## adds the missing values for variables if they aren't give
    ## according to OPTN rules
    replace_na(list(bmi = 100,
                    funstat = 1,
                    walk6m = 4000,
                    o2rest = 0,
                    pap_syst = 20,
                    pco2_15 = 0,
                    pco2 = 40,
                    cont_mech = 0,
                    creat = 0.1,
                    bili = 0.7
    )
    ) |>
    ## truncates variables
    mutate(bili = if_else(bili < 0.7, 0.7, bili),
           pco2 = if_else(pco2 < 40, 40, pco2),
           pap_syst = if_else(pap_syst < 20, 20, pap_syst)
    )


  ## calculate wl cas
  data_cas <- data2 |>
    mutate(
      lp =
        ## diagnosis groups
        case_when(
          dx_grp == 'A' ~  0,
          dx_grp == 'B' ~  1.26319338239175,
          dx_grp == 'C' ~  1.78024171092307,
          dx_grp == 'D' ~  1.51440083414275
        )
      +
        ## specific diagnoses
        case_when(
          dx == 1608                                                    ~  0.40107198445555, # bronchiectasis
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~ -0.64590852776042, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~  1.39885489102977, # sarcoidosis w/ PAP <= 30mmHg (group A)
          dx %in% c(1519, 1613)                                         ~  0.2088684500011, # pulmonary Fibrosis, not idiopathic:
          dx == 1617                                                    ~  0.2088684500011, # pulmonary Fibrosis, COVID:
          .default = 0
        ) +
        ## age
        0.0281444188123287 * age +
        ## bmi
        if_else(bmi < 20, 0.10744133677215 * (20 - bmi), 0) +
        ## functional status
        if_else(funstat == 1, -0.59790409246653, 0) +
        ## six minute walk distance
        -0.09937981549564 * (walk6m / 100) +
        ## oxygen at rest
        case_when(
          dx_grp == 'B' ~ 0.0340531822566417 * o2rest,
          dx_grp %in% c('A', 'C', 'D') ~ 0.08232292818591 * o2rest
        ) +
        ## systolic pap
        case_when(
          dx_grp == 'A' ~ if_else(pap_syst > 40, 0.55767046368853 * ((pap_syst-40)/10), 0),
          dx_grp %in% c('B', 'C', 'D') & pap_syst < 20 ~ 0.1230478043299 * (20/10),
          dx_grp %in% c('B', 'C', 'D')  ~ 0.1230478043299 * pap_syst / 10
        ) +
        ## Pco2 threshoold
        0.15556911866376 * pco2_15 +
        ## if Pco2 >40
        if_else(pco2 >= 40, 0.12639905519026 * (pco2/10), 40/10 * 0.12639905519026) +
        ## Continuous mechanical ventilation or ECMO
        if_else(cont_mech == 1|ecmo == 1, 1.57618530736936, 0) +
        ## Creatinine if over 18
        if_else(age >= 18, 0.0996197163645 * creat, 0)+
        ## Bilirbuin
        if_else(bili > 1, (bili - 1) * 0.15572123729572, 0),
      .keep = "unused"
    )

  return(data_cas)

}

#' @rdname calc_las
#'
#' @export
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr tibble
#' @importFrom dplyr between
#' @importFrom tidyr replace_na
#' @importFrom rlang eval_tidy
calc_post_tx_cas23 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, funstat = funstat,
                             pap_mean = pap_mean, walk6m = walk6m, cont_mech = cont_mech, creat = creat, ci = ci, ecmo = ecmo){

  ## finds variables desired
  c_id <- enquo(c_id)
  dx_grp <- enquo(dx_grp)
  dx <- enquo(dx)
  age <- enquo(age)
  funstat <- enquo(funstat)
  pap_mean <- enquo(pap_mean)
  walk6m <- enquo(walk6m)
  cont_mech <- enquo(cont_mech)
  creat <- enquo(creat)
  ci <- enquo(ci)
  ecmo <- enquo(ecmo)

  c_id <- eval_tidy(c_id, data)
  dx_grp <- eval_tidy(dx_grp, data)
  dx <- eval_tidy(dx, data)
  age <- eval_tidy(age, data)
  funstat <- eval_tidy(funstat, data)
  pap_mean <- eval_tidy(pap_mean, data)
  walk6m <- eval_tidy(walk6m, data)
  cont_mech <- eval_tidy(cont_mech, data)
  creat <- eval_tidy(creat, data)
  ci <- eval_tidy(ci, data)
  ecmo <- eval_tidy(ecmo, data)

  if(any(duplicated(c_id))){stop(paste0(sum(duplicated(c_id)), " candidates have duplicated c_id value\nPlease give each candidate a unique identifier"))}

  data2 <- tibble(c_id, dx_grp, dx, age,  funstat, cont_mech, creat, ci, walk6m, pap_mean, ecmo) |>
    ## adds the missing values for variables if they aren't give
    ## accoridng to OPTN rules
    replace_na(list(funstat = 3,
                    walk6m = 200,
                    cont_mech = 1,
                    ecmo = 1,
                    creat = 1.6,
                    ci = 5
    )
    ) |>
    ## truncates variables
    mutate(walk6m = if_else(walk6m > 1600, 1600, walk6m),
           creat = if_else(creat > 1.6, 1.6, creat),
           ci = if_else(ci > 5, 5, ci)
    )

  ## calculate post_tx las
  data_las <- data2 |>
    mutate(
      lp =
        ## age
        case_when(
          age < 20             ~  0.0676308559079852 * (20 - age) + 0.78241832,
          between(age, 20, 30) ~ -0.0782418319259552 * (age - 20) + 0.78241832,
          between(age, 30, 40) ~  0.0000000000000000 * (age - 30) + 0.00000000,
          between(age, 40, 50) ~  0.0025908121347866 * (age - 40) + 0.00000000,
          between(age, 50, 60) ~  0.0167463361760962 * (age - 50) + 0.02590812,
          between(age, 60, 70) ~  0.0227144625797883 * (age - 60) + 0.19337148,
          age > 70             ~  0.0612288624399672 * (age - 70) + 0.42051611,
        ) +
        ## cardiac index
        case_when(
          ci < 2                ~ -0.4837491139906200 * (2 - ci)   + 0.04030226,
          between(ci, 2, 2.5)   ~ -0.0806045255202868 * (ci - 2)   + 0.04030226,
          between(ci, 2.5, 3.5) ~  0.0136169358319050 * (ci - 2.5) + 0.00000000,
          between(ci, 3.5, 4.5) ~  0.0808432592591954 * (ci - 3.5) + 0.01361694,
          between(ci, 4.5, 5)   ~  0.0696938839239190 * (ci - 4.5) + 0.094460208,
          ci > 5                ~ -0.0023264599609358 * (ci - 5)   + 0.12930714 ,
        ) +
        ## creatinine
        case_when(
          creat < 0.4 & age >= 18              ~  -7.4016726145812200 * (0.4 - creat) + 0.41872820,
          between(creat, 0.4, 0.6) & age >= 18 ~  -1.2584103289549000 * (creat - 0.4) + 0.41872820,
          between(creat, 0.6, 0.8) & age >= 18 ~   0.3712348866558860 * (creat - 0.6) + 0.16704614,
          between(creat, 0.8, 1.4) & age >= 18 ~   0.6844301806854400 * (creat - 0.8) + 0.24129311,
          creat > 1.4 & age >= 18              ~   0.6881894154264970 * (creat - 1.4) + 0.65195122,
        ) +
        ## six minute walk distance
        case_when(
          walk6m < 200                ~  -0.0002535116049789 * (200 - walk6m)  +  0.11168755,
          between(walk6m, 200, 600)   ~  -0.0002841805913329 * (walk6m - 200)  +  0.11168755,
          between(walk6m, 600, 800)   ~  -0.0000049617083362 * (walk6m - 600)  + -0.00198468,
          between(walk6m, 800, 1200)  ~  -0.0001950464256370 * (walk6m - 800)  + -0.00297703,
          between(walk6m, 1200, 1600) ~  -0.0007428583659073 * (walk6m - 1200) + -0.08099560,
          walk6m > 1600               ~   0.0035374143842919 * (walk6m - 1600) + -0.37813894
        ) +
        ## specific disease group diagnoses
        case_when(
          dx == 1608                                                    ~ -0.026706663, # bronchiectasis
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~  0.0561853179859775, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~  0.501743373724746, # sarcoidosis w/ PAP <= 30mmHg (group A)
          dx %in% c(1519, 1613)                                         ~  0.046504644, # pulmonary Fibrosis, not idiopathic:
          dx == 1617                                                    ~  0.046504644, # COVID-19 Fibrosis:
          dx == 1611                                                    ~ -0.271420385917441, # Lymphangioleiomyomatosis
          dx == 1612                                                    ~ -0.13263497847748, # Obliterative bronchiolitis
          dx == 106                                                     ~ -0.13263497847748, # Constrictive bronchiolitis
          .default = 0
        ) +
        ## functional status
        case_when(
          ## none
          funstat == 1 ~ -0.005304128,
          ## total
          funstat == 3 ~  0.074378407,
          .default = 0
        ) +
        ## diagnosis group
        case_when(
          dx_grp == "A" ~ -0.098901796,
          dx_grp == "C" ~ -0.167126401,
          .default = 0
        ) +
        ## continuous mechanical ventilation
        if_else(cont_mech == 1|ecmo == 1, 0.267537018672253, 0),
      .keep = "unused"

    )

  return(data_las)

}
