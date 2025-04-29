#---- Load packages ----
# v2.00
library(fabR)
library(madshapR)
library(Rmonize)

library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(tools)

# email contact if problems
email_contact <- "harmo-propass@maelstrom-research.org"

# get time track
time_stamp <- Sys.time()

#---- Monitor Checks ----
checks_list <-
  list.files("output_documents/", 
             pattern = "checks-",
             full.names = TRUE)

if(length(checks_list) > 1){
  stop("Should have only 1 'checks-'. There might be an issue please contact harmo-propass@maelstrom-research.org.")
  }
if(length(checks_list) == 1){
  checks <- readRDS(checks_list)
  file.copy(from = checks_list,
            to = gsub("output_documents/","archive/", checks_list))
  #@$ REMOVE?
}
if(length(checks_list) == 0){
  checks <- readRDS("output_documents/checks_init.rds")
}
rm(checks_list)
# replace time_stamp
checks$time_stamp <- time_stamp


#---- Open Inputs ----

#----check if any document is open is open
open_file <-
  any(str_detect(
    list.files("input_documents/"), "~\\$"),
    
    str_detect(
      list.files("input_dataset/"), "~\\$"))

if(open_file){
  stop(call. = FALSE,
       
       "

Make sure you have close your Excel files.
If you see this message again, please contact Maelstrom Research at ", email_contact)
}

#---- Get the dataschema ----


if(!file.exists("input_documents/dataschema_ProPASS.xlsx")){
  download.file(
    url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/longitudinal/dataschema_ProPASS_longitudinal.xlsx",
    destfile = "input_documents/dataschema_ProPASS.xlsx",
    mode = "wb")
  
  dataschema <- read_excel_allsheets("input_documents/dataschema_ProPASS.xlsx")
  checks$dataschema_uptodate <- "Ok: First download"
}else{
  # Download dataschema github for comparison
  download.file(
    url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/longitudinal/dataschema_ProPASS_longitudinal.xlsx",
    destfile = "input_documents/dataschema_ProPASS_github.xlsx",
    mode = "wb")
  
  dataschema_local <- read_excel_allsheets("input_documents/dataschema_ProPASS.xlsx")
  dataschema_github <- read_excel_allsheets("input_documents/dataschema_ProPASS_github.xlsx")
  
  # Compare dataschema
  test_all_equal <- try(all(dataschema_local$Variables  == dataschema_github$Variables,
                            dataschema_local$Categories == dataschema_github$Categories,na.rm = TRUE),silent = TRUE)
  if(class(test_all_equal)[1] == "try-error") test_all_equal <- FALSE
  
  # Keep proper dataschema
  if(test_all_equal){
    dataschema <- dataschema_local
    invisible(file.remove("input_documents/dataschema_ProPASS_github.xlsx"))
    checks$dataschema_uptodate <- "Ok: No change"
  }else{
    dataschema <- dataschema_github
    #Archive current dataschema 
    file.copy(from = "input_documents/dataschema_ProPASS.xlsx",
              to = paste0("archive/dataschema_archived", format(time_stamp,"_%Y%m%d%H%M%S"), ".xlsx"))
    invisible(file.remove("input_documents/dataschema_ProPASS.xlsx"))
    #Replace by github dataschema
    file.rename(from = "input_documents/dataschema_ProPASS_github.xlsx",
                to = "input_documents/dataschema_ProPASS.xlsx")
    
    checks$dataschema_uptodate <- "Warning: dataschema was updated"
    }
  
  rm(list = c("dataschema_local","dataschema_github","test_all_equal"))
}

#---- Get the DPE ----

# Download current version on Github
download.file(
  url = paste0(
    "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/longitudinal/data_processing_elements_longitudinal-",
    checks$harmo_group,".xlsx"), 
  destfile = "input_documents/data_processing_element-Github.xlsx",
  mode = "wb")

# Load DPE(s)
dpe_path <- paste0("input_documents/data_processing_element-",
                   checks$harmo_group,
                   ".xlsx")
if(file.exists(dpe_path)){
  dpe_local <- read_excel_allsheets(dpe_path)
}else{
  dpe_local <- tibble(None = TRUE)
}
dpe_github <- read_excel_allsheets("input_documents/data_processing_element-Github.xlsx")

# Compare data processing elements
test_all_equal <- try(all(dpe_local == dpe_github, na.rm = TRUE),silent = TRUE)
if(class(test_all_equal)[1] == "try-error") test_all_equal <- FALSE

# Keep correct DPE and archive if needed
if(test_all_equal){
  invisible(file.remove("input_documents/data_processing_element-Github.xlsx"))
  DPE <- dpe_local
}
if((!"None" %in% names(dpe_local)) & !test_all_equal){
  file.copy(from = dpe_path,
            to = gsub("input_documents", "archive", gsub(".xlsx", paste0(format(time_stamp,"_%Y%m%d%H%M%S"),".xlsx"), dpe_path))
            )
  invisible(file.remove(dpe_path))
}
if(!test_all_equal){
  file.rename(from = "input_documents/data_processing_element-Github.xlsx",
              to = dpe_path)
  DPE <- dpe_github
}

rm(list = c("dpe_github", "dpe_local", "dpe_path", "test_all_equal"))


#---- checks inputs ----

checks$input_documents_ok <- c(is_dataschema(dataschema), is_data_proc_elem(DPE))

if(any(!checks$input_documents)){
  message("Please review your inputs, we detect somme issue(s)")
  message("There are issues with the dataschema and/or dataprocessing element")
  
  saveRDS(checks, paste0("output_documents/checks-", checks$harmo_group, ".rds"))
  
  stop(call. = FALSE,
       "
Please contact Maelstrom Research at ",email_contact," 
and send the files in the folder 'input_documents'." )
}


#--- checks data processing element ----
checks$all_vars_in_DPE <- length(
  setdiff(dataschema$Variables$name, unique(DPE$dataschema_variable))
  ) == 0
checks$same_vars_in_each_DPE <- max(table(DPE$dataschema_variable)) == 
  min(table(DPE$dataschema_variable))

# internal function to extract names of objects in dpe
get_input_var_names <- function(data_proc_elem){
  
  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`")
    x = x[!is.na(x)]
    
    return(x)}
  
  unique(extract_var(
    data_proc_elem %>% 
      dplyr::filter(.data$`input_variables` != '__BLANK__') %>%
      select("input_variables") %>%
      separate_longer_delim(cols = "input_variables",";") %>%
      pull("input_variables"))) %>% str_squish() %>% unique
}
# Get the variables needed for harmonization
DPE_variables <- get_input_var_names(DPE)
rm(get_input_var_names)

#---- checks input datasets ----
# Get input datasets
input_datasets <- data.frame(
  name = sub('\\..[^\\.]*$', '', list.files("input_dataset/")),
  file = list.files("input_dataset/"),
  path = list.files("input_dataset/", full.names=TRUE)
)

input_datasets$error_duplicated <- any(duplicated(c(input_datasets$name)))
input_datasets$error_format <- !grepl("\\.csv$|\\.xlsx$|\\.rds$|\\.sav$|\\.dta$", input_datasets$file)

# Link dataset files to name in DPE
if((nrow(input_datasets) == 1) &
   length(unique(DPE$input_dataset)) == 1){
  input_datasets$dpe_dataset <- unique(DPE$input_dataset)
}

if(nrow(input_datasets) != length(unique(DPE$input_dataset))){
  stop(paste0("You have ", nrow(input_datasets), " files in the input_dataset folder,
      but there are ", length(unique(DPE$input_dataset)), " datasets declared in the DPE.
      This should be adressed.")
  )
}
if(nrow(input_datasets) > 1){
  input_datasets$dpe_dataset <- NA
  for(ii in unique(DPE$input_dataset)){
    choice_file <-  menu(choices = input_datasets$file,
                         title = paste0("Which file contains the data for: ", ii, " ?"))
    input_datasets$dpe_dataset[choice_file] <- ii
  }
}

checks$input_datasets <- input_datasets

# Error messages
if(nrow(input_datasets)==0){stop(call. = FALSE,
                                "
No input file found in the folder 'input_dataset'. Have you added your file(s)?")}
if(any(input_datasets == TRUE))stop(call. = FALSE,
                                   "
There is an issues/limitation with a file in 'input_dataset'.
Please contact Maelstrom Research at ",email_contact,"for help.")

#---- Prepare dossier and reports ----

dossier_to_harmonize <- list()

for(ii in 1:nrow(input_datasets)){
  message(paste0("Starting #", ii, " : ", input_datasets$file[ii]))
  
  # --------------------------------------------
  # get data
  if (file_ext(input_datasets$path[ii]) == "csv") {
    data <- fabR::read_csv_any_formats(input_datasets$path[ii])
    # if any format is error, put this line back instead :
    # data <- read.csv(input_datasets$path[ii], encoding = "latin1") 
  } else if (file_ext(input_datasets$path[ii]) == "xlsx") {
    data <- read_excel_allsheets(input_datasets$path[ii])
  } else if (file_ext(input_datasets$path[ii]) == "rds") {
    data <- readRDS(input_datasets$path[ii])
  } else if (file_ext(input_datasets$path[ii]) == "sav") {
    data <- read_sav(input_datasets$path[ii])
  } else if (file_ext(input_datasets$path[ii]) == "dta") {
    data <- read_dta(input_datasets$path[ii])
  }
  
  # create a new data frame without confidential variables
  data_clean <- data[,which(names(data) %in% DPE_variables)]
  
  message("-data loaded and cleaned-")
  
  # --------------------------------------------
  # generate maelstrom datadictionary and summary
  
  data_dictionnary <- data_dict_extract(data_clean)
  data_summary <- dataset_summarize(data_clean)
  
  message("-summary extracted-")
  
  # save the data dictionary in specified folder
  write_excel_allsheets(
    data_dictionnary,
    paste0("output_documents/input data dictionaries/","extracted_data_dict_",input_datasets$dpe_dataset[ii], ".xlsx"))
  write_excel_allsheets(
    data_summary,
    paste0("output_documents/input data summaries/","data_summary_",input_datasets$dpe_dataset[ii], ".xlsx"))
  
  # Add to dossier for harmonization later
  dossier_to_harmonize[[input_datasets$dpe_dataset[ii]]] <- data_clean
  
  
  message("-summary reports saved-")
  
  # --------------------------------------------
  # checks presence of DPE_variables
  
  if("DPE_vars_in_data" %in% names(checks)){
    DPE_vars_in_data <- checks$DPE_vars_in_data
  }else{
    DPE_vars_in_data <- tibble("DPE_variables" = DPE_variables)
    }
  
  DPE_vars_in_data[[input_datasets$dpe_dataset[ii]]] <- DPE_variables %in% names(data_clean)
  checks$DPE_vars_in_data <- DPE_vars_in_data
  
}

checks$DPE_vars_all_in <- !unlist(lapply(DPE_vars_in_data[c(input_datasets$name)], any))

#---- Final check ----
# Change in packages since last run
packages <- as_tibble(installed.packages()) %>%
  select(Package, Version, Depends, Suggests, Built)

checks$packages_changed <- !identical(packages, checks$packages)
rm(packages)


#---- Save and clean ----
saveRDS(checks, paste0("output_documents/checks-", checks$harmo_group, ".rds"))

rm(list = c("data_dictionnary", "data_summary", "ii", "DPE_vars_in_data", 
            "data_clean", "DPE_variables", "data", "input_datasets",
            "choice_file", "open_file"))
