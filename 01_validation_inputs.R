#---- Load packages and inputs ----
#v1.4
library(fabR)
library(madshapR)
library(dplyr)

# Get input
checks <- readRDS("output_documents/checks.rds")
DPE <- read_excel_allsheets(sort(list.files("archive/", 
                                            pattern = "data_processing_element", 
                                            full.names = TRUE), 
                                 decreasing = TRUE)[1])
dataschema <- read_excel_allsheets("input_documents/dataschema_ProPASS.xlsx")

#---- Prepare Data Processing Element ----
DPE <- DPE %>% 
  filter(input_dataset == checks$harmo_group)

#Save
write_excel_allsheets(DPE, paste0("data_processing_element-", checks$harmo_group, "-to_Validate.xlsx"))

checks$all_vars_in_DPE <- nrow(DPE) == nrow(dataschema$Variables)

# Get the variables needed for harmonization
DPE_variables <- DPE$input_variables
DPE_variables <- gsub("\\r", "", DPE_variables)
DPE_variables <- gsub("\\n", "", DPE_variables)

DPE_variables <- strsplit(DPE_variables, ";") %>% unlist %>% trimws()
DPE_variables <- DPE_variables[DPE_variables != "__BLANK__"]
DPE_variables <- DPE_variables[!duplicated(DPE_variables)]

#---- Checks input datasets ----
# Get input datasets
input_dataset <- data.frame(
  name = sub('\\..[^\\.]*$', '', list.files("input_dataset/")),
  file = list.files("input_dataset/"),
  path = list.files("input_dataset/", full.names=TRUE)
)

input_dataset$error_duplicated <- duplicated(input_dataset$name)
input_dataset$error_format <- !grepl("\\.csv$|\\.xlsx$|\\.rds$|\\.sav$|\\.dta$", input_dataset$file)
checks$input_dataset <- input_dataset

# Error messages
if(nrow(input_dataset)==0){stop("No input file found in the folder 'input_dataset'. Have you added your file(s)?")}
if(any(input_dataset == TRUE))stop("There is an issues/limitation with a file in 'input_dataset', Please contact Maelstrom for help.")

# Packages to read input_dataset
if(any(grepl("\\.sav$|\\.dta$",input_dataset$file))){
  if (!"haven" %in% installed.packages()) {install.packages("haven")}
  library(haven) # if dataset is in .sav or .dta
}

checks$packages <- installed.packages() %>% 
  as_tibble() %>% 
  select(Package, Version, Depends, Suggests, Built)
saveRDS(checks, "output_documents/checks.rds")


#---- Prepare reports ----


for(ii in 1:nrow(input_dataset)){
  print(paste0("Starting #", ii, " : ", input_dataset$file[ii]))
  
  # --------------------------------------------
  # get data
  if (grepl("\\.csv$", input_dataset$path[ii])) {
    data <- read.csv(input_dataset$path[ii], encoding = "latin1")
  } else if (grepl("\\.xlsx$", input_dataset$path[ii])) {
    data <- read_excel_allsheets(input_dataset$path[ii])
  } else if (grepl("\\.rds$", input_dataset$path[ii])) {
    data <- readRDS(input_dataset$path[ii])
  } else if (grepl("\\.sav$", input_dataset$path[ii])) {
    data <- read_sav(input_dataset$path[ii])
  } else if (grepl("\\.dta$", input_dataset$path[ii])) {
    data <- read_dta(input_dataset$path[ii])
  }
  
  # create a new data frame without confidential variables
  data_clean <- data[,which(names(data) %in% DPE_variables)]
  
  print("-data loaded and cleaned-")
  
  # --------------------------------------------
  # generate maelstrom datadictionary and summary
  
  data_dictionnary <- data_dict_extract(data_clean)
  data_summary <- dataset_summarize(data_clean)
  
  print("-summary extracted-")
  
  
  # save the data dictionary in specified folder
  fabR::write_excel_allsheets(data_dictionnary,paste0("output_documents/","extracted_data_dict_", 
                                                      input_dataset$name[ii], ".xlsx"))
  fabR::write_excel_allsheets(data_summary,paste0("output_documents/","data_summary_",
                                                  input_dataset$name[ii], ".xlsx"))
  print("-summary reports saved-")
  
  # --------------------------------------------
  # Check presence of DPE_variables
  
  if("DPE_vars_in_data" %in% names(checks)){
    DPE_vars_in_data <- checks$DPE_vars_in_data
  }else{
    DPE_vars_in_data <- tibble("DPE_variables" = DPE_variables)
  }
  DPE_vars_in_data[[input_dataset$name[ii]]] <- DPE_variables %in% names(data_clean)
  checks$DPE_vars_in_data <- DPE_vars_in_data
}

checks$DPE_vars_all_in <- mean(apply(DPE_vars_in_data[c(input_dataset$name)], 1, function(x){any(x)})) == 1 


#---- Clean and save ----
saveRDS(checks, "output_documents/checks.rds")
saveRDS(checks, paste0("archive/checks", Sys.Date(), ".rds"))
rm(list = c("data_dictionnary", "data_summary", "ii", "DPE_vars_in_data", "data_clean", "DPE_variables", "data", "input_dataset"))
