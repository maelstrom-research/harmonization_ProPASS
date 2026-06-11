### Crossreference check for validation

library(fabR)
library(madshapR)
library(tidyverse)

# load dataset
data_file <- list.files(
  path = "output_dataset",
  pattern = "harmonized_data",
  full.names = TRUE
)
data <- read_excel_allsheets(data_file)

# load dataschema
ds_file <- list.files(
  path = "input_documents",
  pattern = "dataschema",
  full.names = TRUE
)
ds <- read_excel_allsheets(ds_file)

# load dpe
dpe_file <- list.files(
  path = "input_documents",
  pattern = "data_processing_element",
  full.names = TRUE
)
dpe <- read_excel_allsheets(dpe_file)

# create a crossref check object
crossref <- list()

# verify education
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_highest_education")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_edu_secondary")] == "complete") {
  if (nrow(data %>% filter((sdc_highest_education == 0 & sdc_edu_secondary == 1))) > 0 | nrow(data %>% filter((sdc_highest_education > 0 & sdc_edu_secondary == 0))) > 0) {
    
    crossref["education"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((sdc_highest_education == 0 & sdc_edu_secondary == 1)))),
                                      " participant(s) have highest level of education = 0 but variable for at least upper secondary school set as Yes."),
                               paste0(as.character(nrow(data %>% filter((sdc_highest_education > 0 & sdc_edu_secondary == 0)))),
                                      " participant(s) have highest level of education > 0 but variable for at least upper secondary school set as No.")))
  }
}


# verify asian ethnicity
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_asian")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_east_asian")] == "complete") {
  if (nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_east_asian == 1))) > 0 | nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_east_asian == 0))) > 0) {
    
    crossref["East Asian"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_east_asian == 1)))),
                                                 " participant(s) have Asian as No but have East Asian as Yes."),
                                          paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_east_asian == 0)))),
                                                 " participant(s) have Asian as Yes but have East Asian as No (with East Asian set as complete).")))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_asian")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_south_asian")] == "complete") {
  if (nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_east_asian == 1))) > 0 | nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_south_asian == 0))) > 0) {
    
    crossref["South Asian"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_south_asian == 1)))),
                                                  " participant(s) have Asian as No but have South Asian as Yes."),
                                           paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_south_asian == 0)))),
                                                  " participant(s) have Asian as Yes but have South Asian as No (with South Asian set as complete).")))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_asian")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_southeast_asian")] == "complete") {
  if (nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_east_asian == 1))) > 0 | nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_southeast_asian == 0))) > 0) {
    
    crossref["Southeast Asian"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_southeast_asian == 1)))),
                                                  " participant(s) have Asian as No but have Southeast Asian as Yes."),
                                           paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_southeast_asian == 0)))),
                                                  " participant(s) have Asian as Yes but have Southeast Asian as No (with Southeast Asian set as complete).")))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_asian")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "sdc_ethn_west_asian")] == "complete") {
  if (nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_east_asian == 1))) > 0 | nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_west_asian == 0))) > 0) {
    
    crossref["West Asian"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 0 & sdc_ethn_west_asian == 1)))),
                                                  " participant(s) have Asian as No but have East West as Yes."),
                                           paste0(as.character(nrow(data %>% filter((sdc_ethn_asian == 1 & sdc_ethn_west_asian == 0)))),
                                                  " participant(s) have Asian as Yes but have East West as No (with West Asian set as complete).")))
  }
}


# verify height
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_height")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_height_method")] == "complete") {
  if (nrow(data %>% filter((!is.na(pm_height) & is.na(pm_height_method)))) > 0 | nrow(data %>% filter((is.na(pm_height) & !is.na(pm_height_method)))) > 0) {
    
    crossref["height"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((!is.na(pm_height) & is.na(pm_height_method))))),
                                                  " participant(s) have a height value with no height measurement method specified."),
                                           paste0(as.character(nrow(data %>% filter((!is.na(pm_height) & is.na(pm_height_method))))),
                                                  " participant(s) have height measurement method specified with no height value.")))
  }
}


# verify weight
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_weight")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_weight_method")] == "complete") {
  if (nrow(data %>% filter((!is.na(pm_weight) & is.na(pm_weight_method)))) > 0 | nrow(data %>% filter((is.na(pm_weight) & !is.na(pm_weight_method)))) > 0) {
    
    crossref["weight"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((!is.na(pm_weight) & is.na(pm_weight_method))))),
                                              " participant(s) have a weight value with no weight measurement method specified."),
                                       paste0(as.character(nrow(data %>% filter((!is.na(pm_weight) & is.na(pm_weight_method))))),
                                              " participant(s) have weight measurement method specified with no weight value.")))
  }
}

# verify waist cir
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_waist_circ")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "pm_waist_circ_method")] == "complete") {
  if (nrow(data %>% filter((!is.na(pm_waist_circ) & is.na(pm_waist_circ_method)))) > 0 | nrow(data %>% filter((is.na(pm_waist_circ) & !is.na(pm_waist_circ_method)))) > 0) {
    
    crossref["waist"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((!is.na(pm_waist_circ) & is.na(pm_waist_circ_method))))),
                                              " participant(s) have a waist circ value with no waist circ measurement method specified."),
                                       paste0(as.character(nrow(data %>% filter((!is.na(pm_waist_circ) & is.na(pm_waist_circ_method))))),
                                              " participant(s) have waist circ measurement method specified with no waist circ value.")))
  }
}


# verify cvd
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ihd_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_ihd_ever == 1))) > 0) {
    
    crossref["cvd_IHD"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_ihd_ever == 1)))),
                                                 " participant(s) have Yes to IHD but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_mi_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_mi_ever == 1))) > 0) {
    
    crossref["cvd_MI"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_mi_ever == 1)))),
                                      " participant(s) have Yes to MI but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_angina_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_angina_ever == 1))) > 0) {
    
    crossref["cvd_angina"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_angina_ever == 1)))),
                                      " participant(s) have Yes to Angina cvd but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_stroke_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_stroke_ever == 1))) > 0) {
    
    crossref["cvd_stroke"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_stroke_ever == 1)))),
                                      " participant(s) have Yes to IHD cvd but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_heart_failure_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_heart_failure_ever == 1))) > 0) {
    
    crossref["cvd_hf"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_heart_failure_ever == 1)))),
                                      " participant(s) have Yes to Heart failure cvd but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_atrial_fib_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_atrial_fib_ever == 1))) > 0) {
    
    crossref["cvd_atrial_fib"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_atrial_fib_ever == 1)))),
                                      " participant(s) have Yes to atrial fibrillation cvd but variable for general cvd is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_hbp_diag_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_hbp_diag_ever == 1))) > 0) {
    
    crossref["cvd_hbp"] <- c(paste0(as.character(nrow(data %>% filter((dis_cvd_ever == 0 & dis_cvd_hbp_diag_ever == 1)))),
                                           " participant(s) have Yes to hbp cvd but variable for general cvd is No"))
  }
}

# verify cvd diag/med
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_hbp_diag_or_med_ever")] == "complete" &
   (dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cvd_hbp_diag_ever")] == "complete" | dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "med_blood_pressure_cur")] == "complete")) {
  if (nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever == 0 & (dis_cvd_hbp_diag_ever == 1 | med_blood_pressure_cur == 1)))) > 0 | 
      nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever != 0 & (dis_cvd_hbp_diag_ever == 0 & med_blood_pressure_cur == 0)))) > 0 |
      nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever != 2 & ((dis_cvd_hbp_diag_ever == 0 & is.na(med_blood_pressure_cur)) | (is.na(dis_cvd_hbp_diag_ever) & med_blood_pressure_cur == 0))))) > 0) {
    
    crossref["HBP_diag_med"] <- data.frame(c(paste0(as.character(nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever == 0 & (dis_cvd_hbp_diag_ever == 1 | med_blood_pressure_cur == 1))))),
                                             " participant(s) have variable dis_cvd_hbp_diag_or_med_ever = No but hbp diag or med hbp is Yes. (dis_cvd_hbp_diag_or_med_ever should be Yes for these participants)"),
                                      paste0(as.character(nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever != 0 & (dis_cvd_hbp_diag_ever == 0 & med_blood_pressure_cur == 0))))),
                                             " participant(s) have variable dis_cvd_hbp_diag_or_med_ever not No but hbp diag and med hbp are both No. (dis_cvd_hbp_diag_or_med_ever should be No for these participants)"),
                                      paste0(as.character(nrow(data %>% filter((dis_cvd_hbp_diag_or_med_ever != 2 & ((dis_cvd_hbp_diag_ever == 0 & is.na(med_blood_pressure_cur)) | (is.na(dis_cvd_hbp_diag_ever) & med_blood_pressure_cur == 0)))))),
                                             " participant(s) have variable dis_cvd_hbp_diag_or_med_ever not Presumed no but one of the hbp diag and med hbp is missing and the other No. (dis_cvd_hbp_diag_or_med_ever should be Presumed No)")))
  }
}


# verify cancer
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_bladder_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_bladder_ever == 1))) > 0) {
    
    crossref["cancer_bladder"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_bladder_ever == 1)))),
                                    " participant(s) have Yes to bladder cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_breast_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_breast_ever == 1))) > 0) {
    
    crossref["cancer_breast"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_breast_ever == 1)))),
                                           " participant(s) have Yes to breast cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_cervix_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_cervix_ever == 1))) > 0) {
    
    crossref["cancer_cervix"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_cervix_ever == 1)))),
                                           " participant(s) have Yes to cervix cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_colorectal_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_colorectal_ever == 1))) > 0) {
    
    crossref["cancer_colorectal"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_colorectal_ever == 1)))),
                                           " participant(s) have Yes to colorectal cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_kidney_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_kidney_ever == 1))) > 0) {
    
    crossref["cancer_kidney"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_kidney_ever == 1)))),
                                           " participant(s) have Yes to kidney cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_throat_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_throat_ever == 1))) > 0) {
    
    crossref["cancer_throat"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_throat_ever == 1)))),
                                           " participant(s) have Yes to throat cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_leukaemia_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_leukaemia_ever == 1))) > 0) {
    
    crossref["cancer_leukemia"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_leukaemia_ever == 1)))),
                                           " participant(s) have Yes to leukemia cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_lung_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_lung_ever == 1))) > 0) {
    
    crossref["cancer_lung"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_lung_ever == 1)))),
                                           " participant(s) have Yes to lung cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_lymphoma_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_lymphoma_ever == 1))) > 0) {
    
    crossref["cancer_lymphoma"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_lymphoma_ever == 1)))),
                                           " participant(s) have Yes to lymphoma cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_prostate_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_prostate_ever == 1))) > 0) {
    
    crossref["cancer_prostate"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_prostate_ever == 1)))),
                                           " participant(s) have Yes to prostate cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_skin_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_skin_ever == 1))) > 0) {
    
    crossref["cancer_skin"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_skin_ever == 1)))),
                                           " participant(s) have Yes to skin cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_stomach_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_stomach_ever == 1))) > 0) {
    
    crossref["cancer_stomach"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_stomach_ever == 1)))),
                                           " participant(s) have Yes to stomach cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_testicle_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_testicle_ever == 1))) > 0) {
    
    crossref["cancer_testicle"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_testicle_ever == 1)))),
                                           " participant(s) have Yes to testicle cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_uterus_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_uterus_ever == 1))) > 0) {
    
    crossref["cancer_uterus"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_uterus_ever == 1)))),
                                            " participant(s) have Yes to uterus cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_brain_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_brain_ever == 1))) > 0) {
    
    crossref["cancer_brain"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_brain_ever == 1)))),
                                            " participant(s) have Yes to brain cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_liver_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_liver_ever == 1))) > 0) {
    
    crossref["cancer_liver"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_liver_ever == 1)))),
                                            " participant(s) have Yes to liver cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ovary_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_ovary_ever == 1))) > 0) {
    
    crossref["cancer_ovary"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_ovary_ever == 1)))),
                                            " participant(s) have Yes to ovary cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_pancreas_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_pancreas_ever == 1))) > 0) {
    
    crossref["cancer_pancreas"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_pancreas_ever == 1)))),
                                            " participant(s) have Yes to pancreas cancer but variable for general cancer is No"))
  }
}
if(dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_ever")] == "complete" & dpe$`Mlstr_harmo::status`[which(dpe$dataschema_variable == "dis_cancer_thyroid_ever")] == "complete") {
  if (nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_thyroid_ever == 1))) > 0) {
    
    crossref["cancer_thyroid"] <- c(paste0(as.character(nrow(data %>% filter((dis_cancer_ever == 0 & dis_cancer_thyroid_ever == 1)))),
                                            " participant(s) have Yes to thyroid cancer but variable for general cancer is No"))
  }
}


# scale check
if (nrow(data %>% filter(!is.na(psy_dep_cesd20) & psy_dep_cesd20 < 0 | psy_dep_cesd20 > 60)) > 0) {
  crossref["CESD20"] <- paste0(as.character(nrow(data %>% filter(psy_dep_cesd20 < 0 | psy_dep_cesd20 > 60))), " participant(s) have a CESD20 score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_dep_cesd10) & psy_dep_cesd10 < 0 | psy_dep_cesd10 > 30)) > 0) {
  crossref["CESD10"] <- paste0(as.character(nrow(data %>% filter(psy_dep_cesd10 < 0 | psy_dep_cesd10 > 30))), " participant(s) have a CESD10 score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_dep_malaise) & psy_dep_malaise < 0 | psy_dep_malaise > 24)) > 0) {
  crossref["Malaise"] <- paste0(as.character(nrow(data %>% filter(psy_dep_malaise < 0 | psy_dep_malaise > 24))), " participant(s) have a Malaise score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_dep_kessler) & psy_dep_kessler < 0 | psy_dep_kessler > 50)) > 0) {
  crossref["Kessler"] <- paste0(as.character(nrow(data %>% filter(psy_dep_kessler < 0 | psy_dep_kessler > 50))), " participant(s) have a Kessler score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_dep_phq9) & psy_dep_phq9 < 0 | psy_dep_phq9 > 27)) > 0) {
  crossref["PHQ9"] <- paste0(as.character(nrow(data %>% filter(psy_dep_phq9 < 0 | psy_dep_phq9 > 27))), " participant(s) have a PHQ9 score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_mci_mmse_score) & psy_mci_mmse_score < 0 | psy_mci_mmse_score > 30)) > 0) {
  crossref["MMSE"] <- paste0(as.character(nrow(data %>% filter(psy_mci_mmse_score < 0 | psy_mci_mmse_score > 30))), " participant(s) have a MMSE score out of accepted range")
}
if (nrow(data %>% filter(!is.na(psy_mci_moca_score) & psy_mci_moca_score < 0 | psy_mci_moca_score > 30)) > 0) {
  crossref["MoCa"] <- paste0(as.character(nrow(data %>% filter(psy_mci_moca_score < 0 | psy_mci_moca_score > 30))), " participant(s) have a MoCa score out of accepted range")
}
if (nrow(data %>% filter(!is.na(phy_mobility_limitation_SF36) & phy_mobility_limitation_SF36 < 0 | phy_mobility_limitation_SF36 > 100)) > 0) {
  crossref["SF36"] <- paste0(as.character(nrow(data %>% filter(phy_mobility_limitation_SF36 < 0 | phy_mobility_limitation_SF36 > 100))), " participant(s) have a MoCa score out of accepted range")
}

# save crossref checks
write_rds(cross_ref,paste0("output_documents/crossref_baseline_checks_",Sys.Date(),".rds"))
