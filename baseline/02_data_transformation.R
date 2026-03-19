#---- Load packages and inputs ----
# v1.1
library(janitor)
library(htmltools)
library(purrr)
library(crayon)

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


# récupérer le dpe 

#---- before/after ----

if(!any(checks$harmonization_errors)){
  
  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier,harmonized_dataset,split_by = "Mlstr_harmo::rule_category")
  
  before_after_report_path <- 
    paste0("output_documents/","before_after_report-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    before_after_report,
    before_after_report_path)
  
  # harmonization summary report
  output_summary <- 
    harmonized_dossier_summarize(harmonized_dataset)
  output_summary_path <- 
    paste0("output_documents/","harmonization_summary-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    output_summary,
    output_summary_path)
  
  # pooled harmonized dataset (harmonized dataset)
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(harmonized_dataset)
  harmonized_data_path <- 
    paste0("output_dataset/","harmonized_data-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    pooled_harmonized_dataset,
    harmonized_data_path)
  
  # harmonized data dict
  harmonized_data_dict <- 
    data_dict_extract(pooled_harmonized_dataset)
  harmonized_data_dict_path <- 
    paste0("output_documents/","harmonized_data_dict-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    harmonized_data_dict,
    harmonized_data_dict_path)
  
  # visual report
#  viz_path <- 
#    paste0("output_documents/","visual_report-",checks$harmo_group)
#  harmonized_dossier_visualize(
#    harmonized_dossier = harmonized_dataset,
#    bookdown_path = viz_path,
#    harmonized_dossier_summary = output_summary)
  
}else{
  
  # harmonization assessment report
  output_summary <- 
    harmonized_dossier_evaluate(harmonized_dataset)
  output_summary_path <- 
    paste0("output_documents/","harmonization_summary-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    output_summary,
    output_summary_path)
  
  
  # Harmonization for variable without errors
  harmonized_dataset <- harmo_process(
    dossier,
    data_dict_filter(dataschema, filter_var = "!name %in% checks$errors$dataschema_variable"),
    DPE %>% filter(!dataschema_variable %in% checks$errors$dataschema_variable)
  )
  
  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier,harmonized_dataset,split_by = "Mlstr_harmo::rule_category")
  
  
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

objects <- 
  list.files("output_documents/",pattern = ".rds",full.names = TRUE)[
    !list.files("output_documents/",pattern = ".rds",full.names = TRUE) %in%
      (list.files("output_documents/",pattern = ".rds",full.names = TRUE) %>%
         str_subset(pattern = "checks") %>%
         str_subset(pattern = "path",negate = TRUE))]

invisible(file.remove(objects))

rm(list = c("objects","dossier","harmo_errors","harmo_warnings",
            "checks_path","col_harmonized_dataset","cols_dataset",
            "email_contact","output_summary"))

