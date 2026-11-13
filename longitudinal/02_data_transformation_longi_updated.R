#---- Load inputs ----
# v1.0
source("A-avant_apres_harmo.R")

#---- Transformation of data ----
harmonized_dossier <- harmo_process(dossier_to_harmonize,
                                    dataschema,
                                    DPE)

#---- if_warnings ----
harmo_warnings <- 
  attributes(harmonized_dossier)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::warning_status`= as.character())) %>%
  filter(!is.na(`Rmonize::warning_status`)) 

if(nrow(harmo_warnings) == 0){
  
  checks$harmonization_warnings <- FALSE
  checks$harmonization_warnings_detail <- NULL
  
}else{
  
  checks$harmonization_warnings <- TRUE  
  checks$harmonization_warnings_detail <- 
    attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`
  
}

#---- if_errors ----

harmo_errors <- 
  attributes(harmonized_dossier)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::error_status`= as.character())) %>%
  filter(!is.na(`Rmonize::error_status`))

if(nrow(harmo_errors) == 0){
  
  checks$harmonization_errors <- FALSE
  checks$harmonization_errors_detail <- NULL
  
}else{
  
  checks$harmonization_errors <- TRUE  
  checks$harmonization_errors_detail <-     
    attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`
  
}

saveRDS(checks, paste0("output_documents/checks-", checks$harmo_group, ".rds"))

if(any(checks$harmonization_errors,checks$harmonization_warnings)){
  
  message(
    "
Harmonization contains errors/warnings that may be associated to errors in the 
process.

Please contact Maelstrom Research at ",email_contact," and 
send the file '",paste0("output_documents/checks-", checks$harmo_group, ".rds"),"' from the folder 'output_documents'.")  
  
}



#---- before/after ----

if(!any(checks$harmonization_errors)){
  
  #@CHECK per dataset?
  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier = dossier_to_harmonize,
                      harmonized_dossier = harmonized_dossier,
                      split_by = "Mlstr_harmo::rule_category",
                      summarize_output = FALSE)

  write_excel_allsheets(
    before_after_report,
    paste0("output_documents/","before_after_report_longitudinal-",
           checks$harmo_group,'.xlsx'))
  
  # harmonization summary report
  output_summary <- 
    harmonized_dossier_summarize(harmonized_dossier)
  write_excel_allsheets(
    output_summary,
    paste0("output_documents/","harmonization_summary_longitudinal-",
           checks$harmo_group,'.xlsx'))
  
  # pooled harmonized dataset (harmonized dataset)
  pooled_harmonized_dossier <- ####
    pooled_harmonized_dataset_create(harmonized_dossier)
  write_excel_allsheets(
    pooled_harmonized_dossier,
    paste0("output_dataset/","harmonized_data_longitudinal-",
           checks$harmo_group,'.xlsx'))
  
  # harmonized data dict
  harmonized_data_dict <- 
    lapply(harmonized_dossier, data_dict_extract)
  
  for(ii in names(harmonized_data_dict)){
    dd <- harmonized_data_dict[[ii]]
    
    write_excel_allsheets(
      dd,
      paste0("output_documents/harmonized_data_dict_longitudinal-",
             ii,'.xlsx'))
    
  }

      ## Save dossier
  if (!dir.exists("output_dataset/dossier")) {
    dir.create("output_dataset/dossier", recursive = TRUE)
  }
  harmonized_dossier_path <- 
    
  saveRDS(harmonized_dossier, 
          paste0("output_dataset/dossier/harmonized_dossier_longitudinal-",
                 checks$harmo_group,'.rds'))

}else{
  
  # harmonization assessment report
  output_summary <- 
    harmonized_dossier_evaluate(harmonized_dossier)
  write_excel_allsheets(
    output_summary,
    paste0("output_documents/harmonization_summary_longitudinal-",
           checks$harmo_group,'.xlsx'))
  
}

#---- clean objects ----


#@CLEAN the cleaning
rm(list = c(
  "ii","dd","time_stamp",
  "harmo_errors","harmo_warnings","avant_apres_harmo",
  "email_contact"))

