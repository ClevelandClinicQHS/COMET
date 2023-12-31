% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CAS23.R, R/LAS15.R, R/LAS21.R
\name{calc_las}
\alias{calc_las}
\alias{calc_wl_cas23}
\alias{calc_post_tx_cas23}
\alias{calc_wl_las15}
\alias{calc_post_tx_las15}
\alias{calc_wl_las21}
\alias{calc_post_tx_las21}
\title{Calculate LAS/CAS Linear Predictors}
\usage{
calc_wl_cas23(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  bmi = bmi,
  funstat = funstat,
  walk6m = walk6m,
  o2rest = o2rest,
  pap_syst = pap_syst,
  pap_mean = pap_mean,
  pco2 = pco2,
  pco2_15 = pco2_15,
  cont_mech = cont_mech,
  creat = creat,
  bili = bili,
  ecmo = ecmo
)

calc_post_tx_cas23(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  funstat = funstat,
  pap_mean = pap_mean,
  walk6m = walk6m,
  cont_mech = cont_mech,
  creat = creat,
  ci = ci,
  ecmo = ecmo
)

calc_wl_las15(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  bmi = bmi,
  funstat = funstat,
  walk6m = walk6m,
  o2rest = o2rest,
  pap_syst = pap_syst,
  pap_mean = pap_mean,
  pco2_15 = pco2_15,
  pco2 = pco2,
  cont_mech = cont_mech,
  creat = creat,
  bili = bili,
  cvp = cvp,
  bili_50 = bili_50,
  fvc = fvc,
  ci = ci,
  diab = diab
)

calc_post_tx_las15(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  o2rest = o2rest,
  pap_mean = pap_mean,
  walk6m = walk6m,
  cont_mech = cont_mech,
  creat = creat,
  ci = ci,
  funstat = funstat,
  creat_150 = creat_150
)

calc_wl_las21(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  bmi = bmi,
  funstat = funstat,
  walk6m = walk6m,
  o2rest = o2rest,
  pap_syst = pap_syst,
  pap_mean = pap_mean,
  pco2_15 = pco2_15,
  pco2 = pco2,
  cont_mech = cont_mech,
  creat = creat,
  bili = bili
)

calc_post_tx_las21(
  data = NULL,
  c_id = c_id,
  dx_grp = dx_grp,
  dx = dx,
  age = age,
  o2rest = o2rest,
  pap_mean = pap_mean,
  walk6m = walk6m,
  cont_mech = cont_mech,
  creat = creat,
  ci = ci
)
}
\arguments{
\item{data}{dataset of patient data}

\item{c_id}{unique candidate identifier}

\item{dx_grp}{diagnosis group (A, B, C, or D)}

\item{dx}{diagnosis (numeric, see details for ones in LAS/CAS)}

\item{age}{age in years}

\item{bmi}{\ifelse{html}{\out{(body mass index (kg/m<sup>2</sup>)}}{\eqn{\textrm{body mass index (kg/m}^{2})}}}

\item{funstat}{functional status (1 = No assistance needed, 2 = some assistance needed, 3 = total assistance needed)}

\item{walk6m}{six minute walk distance (ft)}

\item{o2rest}{amount of oxygen received if needed while resting (L/min)}

\item{pap_syst}{systolic pulmonary artery pressure (mmHg)}

\item{pap_mean}{mean pulmonary artery pressure (mmHg)}

\item{pco2}{\ifelse{html}{\out{pCO<sub>2</sub> (mmHg)}}{\eqn{\textrm{pCO}_{2}\textrm{ (mmHg)}}}}

\item{pco2_15}{\ifelse{html}{\out{increase in pCO<sub>2</sub> by 15 percent (1,0)}}{\eqn{\textrm{increase in pCO}_{2}\textrm{ by 15 percent (1,0)}}}}

\item{cont_mech}{continuous mechangical ventilation (0 or 1)}

\item{creat}{creatinine (mg/dL)}

\item{bili}{bilirubin (mg/dL)}

\item{ecmo}{ECMO (0 or 1)}

\item{ci}{cardiac index \ifelse{html}{\out{(L/min/m<sup>2</sup>)}}{\eqn{\textrm{L/min/m}^{2}}}}

\item{cvp}{central venous pressure (mmHg)}

\item{bili_50}{bilirubin increase of 50\% (0 or 1)}

\item{fvc}{forced vital capapcity (\%)}

\item{diab}{diabetes (0 or 1)}

\item{creat_150}{creatinine increase >150\% (0 or 1)}
}
\value{
a dataset with c_id and linear predictor for waitlist or post-transplant linear predictor according to LAS/CAS equations
}
\description{
Calculate LAS/CAS Linear Predictors
}
\details{
Numeric Diagnosis Options
\itemize{
\item 1608: bronchiestais
\item 1605:  sarcoidosis
\item 1519,1613: pulmonary fibrosis (non-idiopathetic),
\item 1611: lymphangioleiomyomatosis
\item 1612: Obliterative bronchiolitis
\item 1500,1501,1502,1548,1549: eisenmenger's
\item 1617: COVID caused fibrosis
}
}
\examples{
calc_wl_cas23(syn_cands)
calc_post_tx_cas23(syn_cands)
calc_wl_las21(syn_cands)
calc_post_tx_las21(syn_cands)
calc_wl_las15(syn_cands)
calc_post_tx_las15(syn_cands)
}
