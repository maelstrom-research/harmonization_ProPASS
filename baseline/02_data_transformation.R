#---- Load packages and inputs ----
# v1.3

source("A-avant_apres_harmo.R")

#---- Load datasets ----

# put dataset in a dossier
dossier <- list(data_clean)
names(dossier) <- checks$harmo_group

#---- Transformation of data ----
harmonized_dataset <- harmo_process(dossier, dataschema, DPE)


#---- if_warnings ----
harmo_warnings <- 
  attributes(harmonized_dataset)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::warning_status`= as.character())) %>%
  filter(!is.na(`Rmonize::warning_status`)) 

if(nrow(harmo_warnings) == 0){
  
  checks$harmonization_warnings <- FALSE
  checks$harmonization_warnings_detail <- NULL
  
}else{
  
  checks$harmonization_warnings <- TRUE  
  checks$harmonization_warnings_detail <- 
    attributes(harmonized_dataset)$`Rmonize::Data Processing Elements`
  
}

#---- if_errors ----

harmo_errors <- 
  attributes(harmonized_dataset)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::error_status`= as.character())) %>%
  filter(!is.na(`Rmonize::error_status`))

if(nrow(harmo_errors) == 0){
  
  checks$harmonization_errors <- FALSE
  checks$harmonization_errors_detail <- NULL
  
  checks$errors <- NA
}else{
  
  checks$harmonization_errors <- TRUE  
  checks$harmonization_errors_detail <-     
    attributes(harmonized_dataset)$`Rmonize::Data Processing Elements`
  
  checks$errors <- checks$harmonization_errors_detail %>% 
    select(index, dataschema_variable, valueType, input_variables, 
           `Mlstr_harmo::algorithm`, `Rmonize::r_script`, `Rmonize::error_status`) %>% 
    filter(!is.na(`Rmonize::error_status`)) 
}

saveRDS(checks, checks_path)

if(any(checks$harmonization_errors,checks$harmonization_warnings)){
  
  message(
    "
Harmonization contains errors/warnings that may be associated to errors in the 
process.

Please contact Maelstrom Research at ",email_contact," and 
send the file '",basename(checks_path),"' from the folder 'output_documents'.")  
  
}


#---- Save harmonization outputs ----

# Input 
data_dictionnary <- data_dict_extract(data_clean)
data_summary <- dataset_summarize(data_clean,
                                  data_dict = data_dictionnary)

# save the data dictionary in specified folder
write_excel_allsheets(
  data_dictionnary,
  paste0("output_documents/input data dictionaries/","extracted_data_dict_",input_dataset$name, ".xlsx"))
write_excel_allsheets(
  data_summary,
  paste0("output_documents/input data summaries/","data_summary_",input_dataset$name, ".xlsx"))

message("-input summary extracted-")

if(!any(checks$harmonization_errors)){

  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier,harmonized_dataset,split_by = "Mlstr_harmo::rule_category")
    
  write_excel_allsheets(
    before_after_report,
    paste0("output_documents/","before_after_report-", checks$harmo_group,'.xlsx'))
  
  # harmonization summary report
  output_summary <- 
    harmonized_dossier_summarize(harmonized_dataset)
    
  write_excel_allsheets(
    output_summary,
    paste0("output_documents/","harmonization_summary-", checks$harmo_group,'.xlsx'))
  
  # pooled harmonized dataset (harmonized dataset)
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(harmonized_dataset)
  
  write_excel_allsheets(
    pooled_harmonized_dataset,
    paste0("output_dataset/","harmonized_data-", checks$harmo_group,'.xlsx'))
  
  # harmonized data dict
  harmonized_data_dict <- 
    data_dict_extract(harmonized_dataset[[1]])

  write_excel_allsheets(
    harmonized_data_dict,
    paste0("output_documents/","harmonized_data_dict-", checks$harmo_group,'.xlsx'))

    ## Save dossier
  if (!dir.exists("output_dataset/dossier")) {
    dir.create("output_dataset/dossier", recursive = TRUE)
  } 
  saveRDS(harmonized_dataset,
          paste0("output_dataset/dossier/","harmonized_dossier-",
                 checks$harmo_group,'.rds'))
  
}else{
  
  # harmonization assessment report
  output_summary <- 
    harmonized_dossier_evaluate(harmonized_dataset)

    
  write_excel_allsheets(
    output_summary,
    paste0("output_documents/","harmonization_summary-",
           checks$harmo_group,'.xlsx'))
  
  
  # Harmonization for variable without errors
  harmonized_dataset <- harmo_process(
    dossier,
    data_dict_filter(dataschema, filter_var = "!name %in% checks$errors$dataschema_variable"),
    DPE %>% filter(!dataschema_variable %in% checks$errors$dataschema_variable)
  )
  
  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier,harmonized_dataset,split_by = "Mlstr_harmo::rule_category",summarize_output =FALSE)
  
  
  # Save and stop
  write_excel_allsheets(before_after_report,
                        paste0("output_documents/","before_after_report-",
                               checks$harmo_group,'.xlsx'))
  saveRDS(checks, paste0("output_documents/checks-", checks$harmo_group, ".rds"))
  
  stop("--------------------
       
       Errors found during harmonization! 
       Only variables without errors were harmonized.
       
       ----------------s
       ")
  
}

#---- clean objects ----

# objects <- 
#   list.files("output_documents/",pattern = ".rds",full.names = TRUE)[
#     !list.files("output_documents/",pattern = ".rds",full.names = TRUE) %in%
#       (list.files("output_documents/",pattern = ".rds",full.names = TRUE) %>%
#          str_subset(pattern = "checks") %>%
#          str_subset(pattern = "path",negate = TRUE))]
# 
# invisible(file.remove(objects))

rm(list = c("dossier","harmo_errors","harmo_warnings",
            # "objects",
            "checks_path", "input_dataset",
            "data_summary", "data_dictionnary",
            "email_contact","output_summary"))

