# ## Utils
# validate_candidate_data <- function(data){
#
#   var_names <- c('c_id', 'center', 'listing_day', 'male', 'dx_grp', 'race_eth', 'age_at_listing', 'airway', 'oxygen',
#                  'abo', 'hgt_in', 'hgt_cm', 'wgt_kg', 'bmi', 'resp_supp', 'surg_type', 'diab', 'fev1',
#                  'fvc', 'pco2', 'pf', 'po2', 'pap_mean', 'o2_freq', 'vent', 'walk6m', 'o2',
#                  'bili', 'creat', 'pap_syst', 'ci', 'funstat', 'ecmo', 'cvp', 'pco2_15', 'pra',
#                  'pld', 'creat_150', 'bili_50', 'age', 'hgt_cm1', 'o2rest', 'cont_mech', 'dx', 'pabo',
#                  'phgt', 'pcpra')
#
#   var_classes <- c('integer', 'factor', 'integer', 'integer', 'character', 'factor', 'numeric', 'numeric', 'numeric',
#                    'character', 'numeric', 'numeric', 'numeric', 'numeric', 'factor', 'factor', 'integer', 'numeric',
#                    'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'factor', 'numeric', 'integer',
#                    'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
#                    'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
#                    'numeric', 'numeric')
#
#   names(var_classes) <- var_names
#
#   ck_nms <- var_names %in% colnames(data)
#
#   if(!all(ck_nms)){
#     mis_vars <- var_names[!ck_nms]
#     stop(paste("Var(s)", paste0(mis_vars, collapse = ", "), "is missing in donor data"))
#   }
#
#   ##
#   data2 <- data[var_names]
#   ck_cls <- sapply(data2, class)
#   if(!identical(var_classes, ck_cls)){
#     mis_cls <- var_classes[var_classes != ck_cls]
#     stop(paste(paste(names(mis_cls), "is not of class", mis_cls), collapse = ", "))
#   }
#
#   dataf <- data2[,ck_cls == "factor"]
#
#   f_levels <- list(center = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60', '61'),
#        race_eth = c('NH White', 'NH Black', 'Hispanic', 'Asian', 'Amer. Ind', 'Hawaiian', 'Multi/Other'),
#        resp_supp = c('1', '2', '3', '4'),
#        surg_type = c('D', 'E', 'S'),
#        vent = c('BiPap', 'Mechanical', 'None', 'CPAP'))
#
#   ck_lvs <- mapply(function(x, y) identical(x, y), f_levels, lapply(dataf, levels))
#
#   if(!all(ck_lvs)){
#     mis_lvs <- f_levels[which(!ck_lvs)]
#     stop(paste(paste(names(mis_lvs), "does not have the levels", paste0(gsub(pattern = "\\\"", replacement = "", paste0(mis_lvs)))), collapse = "; "))
#   }
#
#   return(data2)
#
#
#   }
#
# validate_donor_data <- function(data){
#
#   var_names <- c("d_id", "hospital", "recovery_day", "male", "race_eth", "hgt_in", "hgt_cm", "cod", "abo", "age",
#                  "don_org", "smoke_hist", "gt_20pkyr", "don_dcd", "don_util", "organs_avl")
#
#   var_classes <- c('integer', 'numeric', 'integer', 'integer', 'factor', 'numeric', 'numeric', 'factor', 'factor', 'numeric',
#                    'factor', 'integer', 'character', 'integer', 'integer', 'numeric')
#
#   names(var_classes) <- var_names
#
#   ck_nms <- var_names %in% colnames(data)
#
#   if(!all(ck_nms)){
#     mis_vars <- var_names[!ck_nms]
#     stop(paste("Var(s)", paste0(mis_vars, collapse = ", "), "is missing in donor data"))
#   }
#
#   ##
#   data2 <- data[var_names]
#   ck_cls <- sapply(data2, class)
#   if(!identical(var_classes, ck_cls)){
#     mis_cls <- var_classes[var_classes != ck_cls]
#     stop(paste(paste(names(mis_cls), "is not of class", mis_cls), collapse = ", "))
#   }
#
#   dataf <- data2[,ck_cls == "factor"]
#
#   f_levels <- list(race_eth = c('NH White', 'NH Black', 'Hispanic', 'Asian', 'Amer. Ind', 'Hawaiian', 'Multi/Other'),
#                    cod = c('1', '2', '3', '4', '999'),
#                    abo = c('A', 'AB', 'B', 'O'),
#                    don_org = c('DLU', 'LUL', 'LUR'))
#
#   ck_lvs <- mapply(function(x, y) identical(x, y), f_levels, lapply(dataf, levels))
#
#   if(!all(ck_lvs)){
#     mis_lvs <- f_levels[which(!ck_lvs)]
#     stop(paste(paste(names(mis_lvs), "does not have the levels", paste0(gsub(pattern = "\\\"", replacement = "", paste0(mis_lvs)))), collapse = "; "))
#   }
#
#   return(data2)
#
# }
