#' Synthetic Candidate Set
#'
#' Similar data can be generated by \code{gen_and_spawn_candidates}
#' but this data was stored to keep examples and tests consistent.
#'
#' @format ## `syn_cands`
#' A tibble of 100 synethic candidates
#' \describe{
#'   \item{c_id}{unique candidate identifier}
#'   \item{center}{integer of 1-61 unique centers}
#'   \item{listing_day}{date of listing}
#'   \item{male}{binary 1 (male) or 0 (female)}
#'   \item{dx_grp}{diagnosis group}
#'   \item{race_eth}{race or ethnicity}
#'   \item{age_at_listing}{age in years, decimal years (nearest 1/365)}
#'   \item{airway}{airway function value}
#'   \item{oxygen}{oxygen function value}
#'   \item{abo}{blood type}
#'   \item{hgt_in}{height in inches (nearest 0.5)}
#'   \item{hgt_cm}{height in centimeters}
#'   \item{wgt_kg}{weight in kilograms}
#'   \item{bmi}{body mass index}
#'   \item{resp_supp}{respiratory support cluster}
#'   \item{surg_type}{double "D", single "S", either, "E" type of transplant is preferred}
#'   \item{diab}{diabetes}
#'   \item{fev1}{forced expiratory volume in the first second}
#'   \item{fvc}{fored vital capacity}
#'   \item{pco2}{partial pressure of pco2}
#'   \item{pf}{Po2/fio2 ratio}
#'   \item{po2}{po2}
#'   \item{pap_mean}{mean pulmonary artery pressure}
#'   \item{o2_freq}{oxygen frequency}
#'   \item{vent}{ventilator use}
#'   \item{walk6m}{six minute walk distance (feet)}
#'   \item{o2}{supplemental oxygen (L/min)}
#'   \item{bili}{bilirubin}
#'   \item{creat}{creatinine}
#'   \item{pap_syst}{systolic pulmonary artery pressure}
#'   \item{ci}{cardiac index}
#'   \item{funstat}{functional status}
#'   \item{ecmo}{ECMO}
#'   \item{cvp}{central venous pressure}
#'   \item{pco2_15}{increase in pco2 by 15 percent}
#'   \item{pra}{something to add later}
#'   \item{pld}{prior living donor}
#'   \item{creat_150}{creating increase by 150 percent}
#'   \item{bili_50}{increase of biliruinb by 50 percent}
#'   \item{age}{current age}
#'   \item{hgt_cm1}{height to the nearest centimter}
#'   \item{o2rest}{supplemental oxygen received at rest}
#'   \item{cont_mech}{continuous mechanical ventilation}
#'   \item{dx}{diagnosis}
#'   \item{pabo}{proprotion of incompabilible blood type}
#'   \item{phgt}{proportion of incompatible heihgt range}
#'   \item{pcpra}{proprotion of incompatabile pra}
#'   \item{days_on_waitlist}{how long the person has been waiting for a transplant}
#'
#' }
"syn_cands"