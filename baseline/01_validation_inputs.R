#---- Load packages and inputs ----
# v1.10

library(tools)
library(fabR)
library(madshapR)
library(Rmonize)
library(dplyr)
library(tidyr)  
library(stringr)
library(haven)

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

# email contact if problems
email_contact <- "info@maelstrom-research.org"

# get time track
time_stamp <- Sys.time()

# monitor checks
checks <- readRDS("output_documents/checks_init.rds")
checks_list <-
  list.files("output_documents/", pattern = paste0("checks-",checks$harmo_group),full.names = TRUE)

if(length(checks_list) > 0){
  invisible(file.rename(
    from = checks_list,
    to = str_replace_all(checks_list,"output_documents/","archive/")))}

checks_path <- 
  paste0("output_documents/checks-",checks$harmo_group,"-",format(time_stamp,"%Y-%m-%H%M%S"), ".rds")

invisible(file.copy(from = "output_documents/checks_init.rds",to = checks_path))

checks <- readRDS(checks_path)

# replace time_stamp
checks$time_stamp <- time_stamp

# check if any document is open is open
open_file <-
  any(str_detect(
    list.files("input_documents/"), "~\\$"),
    
    str_detect(
      list.files("input_dataset/"), "~\\$"))

if(open_file){
  stop(call. = FALSE,
       
       "

Make sure you have close your Excel files.
If you see this message again, please contact Maelstrom Research at ",email_contact)
}

# get the dataschema

dataschema_path <- "input_documents/dataschema_ProPASS.xlsx"

if(!file.exists(dataschema_path)){
  download.file(
    url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/dataschema_ProPASS.xlsx",
    destfile = "input_documents/dataschema_ProPASS.xlsx",
    mode = "wb")
  
}else{
  
  # checks if dataschema local and on github are different
  download.file(
    url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/dataschema_ProPASS.xlsx",
    destfile = "input_documents/dataschema_ProPASS_github.xlsx",
    mode = "wb")
  
  dataschema_local <- read_excel_allsheets("input_documents/dataschema_ProPASS.xlsx")
  dataschema_github <- read_excel_allsheets("input_documents/dataschema_ProPASS_github.xlsx")
  
  invisible(file.remove("input_documents/dataschema_ProPASS_github.xlsx"))
  
  test_all_equal <- try(all(dataschema_local$Variables  == dataschema_github$Variables,
                            dataschema_local$Categories == dataschema_github$Categories,na.rm = TRUE),silent = TRUE)
  
  if(class(test_all_equal)[1] == "try-error") test_all_equal <- FALSE
  
  if(test_all_equal){
    
    checks$dataschema_uptodate <- TRUE
    
  }else{
    
    checks$dataschema_uptodate <- FALSE
    saveRDS(checks, checks_path)
    stop(call. = FALSE,
         "
The dataSchema present in your folder /input_documents is out of date. 
            
Please contact Maelstrom Research at ",email_contact," 
and send the file '",basename(checks_path),"' from the folder 'output_documents'.")
    
  }
  
  rm(list = c("dataschema_local","dataschema_github","test_all_equal"))
}

dpe_path <- 
  paste0(
    "input_documents/data_processing_element-",
    checks$harmo_group, "-",format(time_stamp, "%Y-%m-%d"),".xlsx")

DPE_list <- sort(
  list.files("input_documents/", 
             pattern = "data_processing_element", 
             full.names = TRUE), 
  decreasing = TRUE)

# put old dpe in archives
if(length(DPE_list) > 1){
  
  archived_dpe_path <-
    str_replace(
      DPE_list[-1],pattern = ".xlsx", 
      paste0(format(Sys.time(),"_%Y%m%d%H%M%S"),".xlsx")) %>%
    str_replace("input_documents/","archive/")
  
  invisible(file.rename(
    from = DPE_list[-1],
    to = archived_dpe_path))
}

# if no dpe
if(length(DPE_list) == 0){
  # get the data_proc_elem
  download.file(
    url = paste0(
      "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/data_processing_elements-",
      checks$harmo_group,".xlsx"), 
    destfile = dpe_path,
    mode = "wb")
  
}else{ # if one dpe
  
  # checks if data_proc_elem local and on github are different
  download.file(
    url = paste0(
      "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/data_processing_elements-",
      checks$harmo_group,".xlsx"), 
    destfile = "input_documents/data_proc_elem-github.xlsx",
    mode = "wb")
  
  dpe_local <- read_excel_allsheets(DPE_list[1])
  dpe_github <- read_excel_allsheets("input_documents/data_proc_elem-github.xlsx")
  
  if(all(dpe_local  == dpe_github,na.rm = TRUE)){
    
    checks$dpe_uptodate <- TRUE
    
  }else{
    
    checks$dpe_uptodate <- FALSE
    saveRDS(checks, checks_path)
    message(
      "
The Data Processing Elements (DPE) present in your folder /input_documents is 
out of date, or differs from the DPE present online in the github folder.

If you are not able to go further in the process, please contact 
Maelstrom Research at ",email_contact," and 
send the file '",basename(checks_path),"' from the folder 'output_documents'.")
    
  }
  
  archived_dpe_path <-
    str_replace(
      DPE_list[1],pattern = ".xlsx", 
      paste0(format(Sys.time(),"_%Y%m%d%H%M%S"),".xlsx")) %>%
    str_replace("input_documents/","archive/")
  
  invisible(file.rename(
    from = DPE_list[1],
    to = archived_dpe_path))
  
  invisible(file.rename(
    from = "input_documents/data_proc_elem-github.xlsx",
    to = dpe_path))
  
  rm(list = c("dpe_local","dpe_github","archived_dpe_path"))
  
}

#---- read dataschema and dpe ----
dataschema <- read_excel_allsheets(dataschema_path)
DPE <- read_excel_allsheets(dpe_path)

#---- checks inputs ----

checks$input_documents_ok <- c(is_dataschema(dataschema), is_data_proc_elem(DPE))

if(any(!c(checks$input_documents))){
  message("Please review your inputs, we detect somme issue(s)")
  message("There are issues with the dataschema and/or dataprocessing element")
  
  saveRDS(checks, checks_path)
  
  stop(call. = FALSE,
       "

Please contact Maelstrom Research at ",email_contact," 
and send the file '",basename(checks_path),"' from the folder 'output_documents'." )
  
}

#--- checks data processing element
checks$all_vars_in_DPE <- nrow(DPE) == nrow(dataschema$Variables)

# Get the variables needed for harmonization
DPE_variables <- get_input_var_names(DPE)

#---- checks input datasets ----
# Get input datasets
input_dataset <- data.frame(
  name = sub('\\..[^\\.]*$', '', list.files("input_dataset/")),
  file = list.files("input_dataset/"),
  path = list.files("input_dataset/", full.names=TRUE)
)

input_dataset$error_duplicated <- any(duplicated(c(input_dataset$name)))
input_dataset$error_format <- !grepl("\\.csv$|\\.xlsx$|\\.rds$|\\.sav$|\\.dta$", input_dataset$file)
checks$input_dataset <- input_dataset

# Error messages
if(nrow(input_dataset)==0){stop(call. = FALSE,
                                "
No input file found in the folder 'input_dataset'. Have you added your file(s)?")}
if(any(input_dataset == TRUE))stop(call. = FALSE,
                                   "
There is an issues/limitation with a file in 'input_dataset'.
Please contact Maelstrom Research at ",email_contact,"for help.")

#---- Prepare reports ----

for(ii in 1:nrow(input_dataset)){
  message(paste0("Starting #", ii, " : ", input_dataset$file[ii]))
  message("reading the Excel file. Please wait...")
  
  # --------------------------------------------
  # get data
  if (file_ext(input_dataset$path[ii]) == "csv") {
    data <- fabR::read_csv_any_formats(input_dataset$path[ii])
    # if any format is error, put this line back instead :
    # data <- read.csv(input_dataset$path[ii], encoding = "latin1") 
  } else if (file_ext(input_dataset$path[ii]) == "xlsx") {
    data <- read_excel_allsheets(input_dataset$path[ii])
  } else if (file_ext(input_dataset$path[ii]) == "rds") {
    data <- readRDS(input_dataset$path[ii])
  } else if (file_ext(input_dataset$path[ii]) == "sav") {
    data <- read_sav(input_dataset$path[ii])
  } else if (file_ext(input_dataset$path[ii]) == "dta") {
    data <- read_dta(input_dataset$path[ii])
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
    paste0("output_documents/input data dictionaries/","extracted_data_dict_",input_dataset$name[ii], ".xlsx"))
  write_excel_allsheets(
    data_summary,
    paste0("output_documents/input data summaries/","data_summary_",input_dataset$name[ii], ".xlsx"))
  
  # store information for later
  saveRDS(data_clean,paste0("output_documents/data_clean_",input_dataset$name[ii],".rds"))
  
  message("-summary reports saved-")
  
  # --------------------------------------------
  # checks presence of DPE_variables
  
  if("DPE_vars_in_data" %in% names(checks)){
    DPE_vars_in_data <- checks$DPE_vars_in_data
  }else{
    DPE_vars_in_data <- tibble("DPE_variables" = DPE_variables)}
  
  DPE_vars_in_data[[input_dataset$name[ii]]] <- DPE_variables %in% names(data_clean)
  checks$DPE_vars_in_data <- DPE_vars_in_data
  
}

checks$DPE_vars_all_in <- !unlist(lapply(DPE_vars_in_data[c(input_dataset$name)], any))

#---- Save and clean ----

saveRDS(checks, checks_path)
saveRDS(dataschema, "output_documents/dataschema.rds")
saveRDS(DPE, "output_documents/DPE.rds")
saveRDS(email_contact, "output_documents/email_contact.rds")
saveRDS(checks_path, "output_documents/checks_path.rds")

rm(list = c("data_dictionnary", "data_summary", "ii", "DPE_vars_in_data", 
            "data_clean", "DPE_variables", "data", "input_dataset",
            "dataschema_path","checks_list","time_stamp","DPE_list",
            "checks","dataschema","DPE","get_input_var_names",
            "checks_path","email_contact","open_file","dpe_path"))
