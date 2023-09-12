#' @rdname calc_las
#'
#' @export
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr tibble
#' @importFrom rlang eval_tidy
#' @importFrom tidyr replace_na
calc_wl_las21 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, bmi = bmi, funstat = funstat, walk6m = walk6m,
                        o2rest = o2rest, pap_syst = pap_syst, pap_mean = pap_mean, pco2_15 = pco2_15, pco2 = pco2, cont_mech = cont_mech,
                        creat = creat, bili = bili){

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


  data2 <- tibble(c_id, dx_grp, dx, age, bmi, funstat, walk6m, o2rest, pap_syst,
                  pap_mean, pco2_15, pco2, cont_mech, creat, bili) |>
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

  ## calculate wl las
  data_las <- data2 |>
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
          dx %in% c(214, 1608)                                          ~  0.40107198445555, # bronchiectasis
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~ -0.64590852776042, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~  1.39885489102977, # sarcoidosis w/ PAP <= 30mmHg (group A)
          dx %in% c(1519, 1613)                                         ~ 0.2088684500011, # pulmonary Fibrosis, not idiopathic:
          # TRUE ~ 0
          .default = 0
        ) +
        ## age
        0.0281444188123287 * age +
        ## bmi
        if_else(bmi < 20, 0.10744133677215 * (20 - bmi), 0) +
        ## functiona status
        if_else(funstat == 1, -0.5979040924665, 0) +
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
          # dx_grp %in% c('B', 'C', 'D') & pap_syst < 20 ~ 0.1230478043299 * (20/10),
          dx_grp %in% c('B', 'C', 'D')  ~ 0.1230478043299 * pap_syst / 10
        ) +
        ## Pco2 threshoold
        0.15556911866376 * pco2_15 +
        ## if Pco2 >40
        if_else(pco2 >= 40, 0.12639905519026 * (pco2/10), 40/10 * 0.12639905519026) +
        ## Continuous mechanical ventilation
        cont_mech * 1.57618530736936 +
        ## Creatinine if over 18
        if_else(age >= 18, 0.0996197163645 * creat, 0)+
        ## Bilirbuin
        if_else(bili > 1, (bili - 1) * 0.15572123729572, 0),
      .keep = "unused"
    )

  return(data_las)
}

#' @rdname calc_las
#'
#' @export
calc_post_tx_las21 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, o2rest = o2rest,
                             pap_mean = pap_mean, walk6m = walk6m, cont_mech = cont_mech, creat = creat, ci = ci){

  ## finds variables desired
  c_id <- enquo(c_id)
  dx_grp <- enquo(dx_grp)
  dx <- enquo(dx)
  age <- enquo(age)
  pap_mean <- enquo(pap_mean)
  walk6m <- enquo(walk6m)
  cont_mech <- enquo(cont_mech)
  creat <- enquo(creat)
  ci <- enquo(ci)
  o2rest <- enquo(o2rest)

  c_id <- eval_tidy(c_id, data)
  dx_grp <- eval_tidy(dx_grp, data)
  dx <- eval_tidy(dx, data)
  age <- eval_tidy(age, data)
  pap_mean <- eval_tidy(pap_mean, data)
  walk6m <- eval_tidy(walk6m, data)
  cont_mech <- eval_tidy(cont_mech, data)
  creat <- eval_tidy(creat, data)
  ci <- eval_tidy(ci, data)
  o2rest <- eval_tidy(o2rest, data)

  data2 <- tibble(c_id, dx_grp, dx, age, cont_mech, creat, ci, walk6m, pap_mean, o2rest) |>
    ## adds the missing values for variables if they aren't give
    ## accoridng to OPTN rules
    replace_na(list(walk6m = 0,
                    o2rest = 26.33,
                    cont_mech = 1,
                    creat = 40,
                    bmi = 100,
                    ci = 3
    )
    ) |>
    ## truncates variables
    mutate(walk6m = if_else(walk6m < 0, 0, walk6m),
           creat = if_else(creat > 40, 40, creat)
    )

  ## calculate post_tx las
  data_las <- data2 |>
    mutate(
      lp =
        # age
        if_else(age > 45, 0.0208895929056676 * (age - 45), 0) +
        ## Creatinine
        if_else(age >= 18, 0.25451764981323 * creat, 0) +
        # ## caridiac index
        if_else(ci < 2, 0.1448727551614, 0) +
        # ## mechanical ventilation if hospitalizied
        (cont_mech * 0.33161555489537) +
        # ## diagnosis group
        case_when(
          dx_grp == "A" ~ 0,
          dx_grp == "B" ~ 0.51341349576197,
          dx_grp == "C" ~ 0.23187885123342,
          dx_grp == "D" ~ 0.12527366545917,
        ) +
        # ## specific diagnoses
        case_when(
          dx %in% c(214, 1608)                                          ~  0.12048575705296, # bronchiectasis
          dx == 1612                                                    ~ -0.33402539276216, # Obliterative bronchiolitis
          dx == 106                                                     ~ -0.33402539276216, # Constrictive bronchiolitis
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~  0.43537371336129, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~  0.98051166673574, # sarcoidosis w/ PAP <= 30mmHg (group A)
          # TRUE ~ 0
          .default = 0
        ) +
        # ## oxygen at rest
        case_when(
          dx_grp == "A" ~ 0.0100383613234584 * o2rest,
          dx_grp %in% c("B", "C", "D") ~ 0.0093694370076423 * o2rest
        ) +
        ## six-minute walk distance
        if_else(walk6m >= 1200, 0, 0.0001943695814883 * (1200-walk6m)),

      .keep = "unused"

    )

  return(data_las)
}
