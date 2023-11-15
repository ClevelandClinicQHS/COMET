#' @rdname calc_las
#'
#' @param ci cardiac index \ifelse{html}{\out{(L/min/m<sup>2</sup>)}}{\eqn{\textrm{L/min/m}^{2}}}
#' @param cvp central venous pressure (mmHg)
#' @param bili_50 bilirubin increase of 50\% (0 or 1)
#' @param fvc forced vital capapcity (\%)
#' @param diab diabetes (0 or 1)
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr tibble
#' @importFrom rlang eval_tidy
#' @importFrom tidyr replace_na
#'
#' @export
calc_wl_las15 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, bmi = bmi, funstat = funstat, walk6m = walk6m,
                          o2rest = o2rest, pap_syst = pap_syst, pap_mean = pap_mean, pco2_15 = pco2_15, pco2 = pco2, cont_mech = cont_mech,
                          creat = creat, bili = bili, cvp = cvp, bili_50 = bili_50, fvc = fvc, ci = ci, diab = diab){

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
  cvp <- enquo(cvp)
  bili_50 <- enquo(bili_50)
  fvc <- enquo(fvc)
  ci <- enquo(ci)
  diab <- enquo(diab)

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
  cvp <- eval_tidy(cvp, data)
  bili_50 <- eval_tidy(bili_50, data)
  fvc <- eval_tidy(fvc, data)
  ci <- eval_tidy(ci, data)
  diab <- eval_tidy(diab, data)

  if(any(duplicated(c_id))){stop(paste0(sum(duplicated(c_id)), " candidates have duplicated c_id value\nPlease give each candidate a unique identifier"))}

  data2 <- tibble(c_id, dx_grp, dx, age, bmi, funstat, walk6m, o2rest, pap_syst,
                         pap_mean, pco2_15, pco2, cont_mech, creat, bili, cvp, bili_50, fvc, ci, diab) |>
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
                    bili = 0.7,
                    diab = 0,
                    ci = 3,
                    cvp = 5,
                    creat = 0.1,
                    fvc = 150 ## technically if only group D, but that won't matter for calculation see below

    )
    ) |>
    ## truncates variables
    mutate(bili = if_else(bili < 0.7, 0.7, bili),
           pco2 = if_else(pco2 < 40, 40, pco2),
           pap_syst = if_else(pap_syst < 20, 20, pap_syst),
           cvp = if_else(cvp < 5, 5, cvp)
    )

  ## calculate wl las
  data_las <- data2 |>
    mutate(
      lp =
        0.0083990318885565 * age +
        if_else(bili > 1, 0.0431682188302477 * (bili - 1), 0) +
        if_else(dx_grp == "B", bili_50 * 1.4144058906830200, 0) +
        if_else(bmi <= 20, 0.1261444133358100 * (20 - bmi), 0) +
        if_else(ci < 2, 0.5435368888028200, 0) +
        if_else(cvp >= 7 & dx_grp == "B", 0.0173841981251578 * (cvp - 7), 0) +
        cont_mech * 1.6771121096052300 +
        if_else(age >= 18, 0.5034346761960600 * creat, 0) +
        diab * 0.4680254026735700 +
        case_when(
          dx_grp == 'A' ~  0,
          dx_grp == 'B' ~  1.5774243292137200,
          dx_grp == 'C' ~  1.2313926484343600,
          dx_grp == 'D' ~  0.6259577164157700
        )
      +
        case_when(
          dx %in% c(1608)                                               ~  0.6680518055684700, # bronchiectasis
          dx %in% c(1500, 1501, 1502, 1548, 1549)                       ~ -0.6278657824830000, # eisenmenger's
          dx == 1611                                                    ~ -0.3162937838984600, # Lymphangioleiomyomatosis
          dx == 1612                                                    ~  0.4453284411081100, # Obliterative bronchiolitis
          dx %in% c(1519, 1613)                                         ~ -0.2091170018125500, # pulmonary Fibrosis, not idiopathic:
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~ -0.4577749354638600, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~  0.9330846239906700, # sarcoidosis w/ PAP <= 30mmHg (group A)
          .default = 0
        ) +
        ## FVC
        if_else(dx_grp == "D" & fvc <= 80, 0.1829476350587400 * ((80 - fvc)/10), 0) +
        if_else(funstat == 1, -0.4471034284458400, 0) +
        case_when(
          dx_grp == "B" ~ 0.0213187586203456 * o2rest,
          dx_grp %in% c("A", "C", "D") ~ 0.1188479817592500 * o2rest
        ) +
        if_else(pco2 >= 40, 0.1104609835819100 * pco2/10, 0)+
        pco2_15 * 0.2331149280428300 +
        case_when(
          dx_grp == "A" ~ if_else(pap_syst > 40, 0.4155116686114300 * ((pap_syst - 40)/10), 0),
          dx_grp %in% c("B", "C", "D") ~ 0.0462410402627318 * (pap_syst/10)
        ) +
        -0.0844896372724000 * (walk6m/100),
      .keep = "unused"
    )

  return(data_las)
}



#' @rdname calc_las
#'
#' @param creat_150 creatinine increase >150\% (0 or 1)
#'
#' @export
calc_post_tx_las15 <- function(data = NULL, c_id = c_id, dx_grp = dx_grp, dx = dx, age = age, o2rest = o2rest,
                             pap_mean = pap_mean, walk6m = walk6m, cont_mech = cont_mech, creat = creat, ci = ci,
                             funstat = funstat, creat_150 = creat_150){

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
  funstat <- enquo(funstat)
  creat_150 <- enquo(creat_150)

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
  funstat <- eval_tidy(funstat, data)
  creat_150 <- eval_tidy(creat_150, data)

  if(any(duplicated(c_id))){stop(paste0(sum(duplicated(c_id)), " candidates have duplicated c_id value\nPlease give each candidate a unique identifier"))}

  data2 <- tibble(c_id, dx_grp, dx, age, cont_mech, creat, ci, walk6m, pap_mean, o2rest, funstat, creat_150) |>
    ## adds the missing values for variables if they aren't given
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
        if_else(age > 45, 0.0246579831271869 * (age - 45), 0) +
        ## Creatinine
        if_else(age >= 18, 0.0895569900508900 * creat, 0) +
        ## Creatinine >150%
        if_else(creat >= 1, creat_150 * 0.7708616024698100, 0) +
        # ## caridiac index
        if_else(ci < 2, 0.3499381679822400 * ci, 0) +
        # ## mechanical ventilation if hospitalizied
        cont_mech * 0.6094478988424900 +
        # ## diagnosis group
        case_when(
          dx_grp == "A" ~ 0,
          dx_grp == "B" ~ 0.6115547319209300,
          dx_grp == "C" ~ 0.3627014422464200,
          dx_grp == "D" ~ 0.4641392063023200,
        ) +
        # ## specific diagnoses
        case_when(
          dx %in% c(1608)                                               ~  0.1889100379099400, # bronchiectasis
          dx %in% c(1500, 1501, 1502, 1548, 1549)                       ~  0.9146727886744700, # eisenmenger's
          dx == 1611                                                    ~ -1.5194416206749400, # Lymphangioleiomyomatosis
          dx == 1612                                                    ~ -1.2050508750702600, # Obliterative bronchiolitis
          dx %in% c(1519, 1613)                                         ~ -0.0723596761367600, # pulmonary Fibrosis, not idiopathic:
          dx == 1605 & dx_grp == "D" & pap_mean > 30                    ~ -0.0437880049066331, # sarcoidosis w/ PAP  > 30mmHg (group D)
          dx == 1605 & dx_grp == "A" & (pap_mean <= 30|is.na(pap_mean)) ~ -0.1389363636019300, # sarcoidosis w/ PAP <= 30mmHg (group A)
          .default = 0
        ) +
        # ## oxygen at rest
        case_when(
          dx_grp == "A" ~ 0.0747978926517300 * o2rest,
          dx_grp %in% c("B", "C", "D") ~ 0.0164276945879309 * o2rest
        ) +
        if_else(funstat == 1, -0.1900086366785100, 0) +
        ## six-minute walk distance
        if_else(walk6m >= 1200, 0.0004594953809594 * (1200 - walk6m), 0),

      .keep = "unused"

    )

  return(data_las)
}
