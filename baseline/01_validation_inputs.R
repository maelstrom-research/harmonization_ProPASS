#---- Load packages and basic inputs ----
# v2.0

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
email_contact <- "sbtiali@maelstrom-research.org"

# get time track
time_stamp <- Sys.time()

#---- check if any document is open is open
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

#---- Monitor checks ----
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

# replace time_stamp
checks$time_stamp <- time_stamp
rm(list = c("time_stamp", "checks_list"))



#---- Get the dataschema ----
download.file(
  url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/baseline/dataschema_ProPASS.xlsx",
  destfile = "input_documents/dataschema_ProPASS_github.xlsx",
  mode = "wb")

dataschema_github <- read_excel_allsheets("input_documents/dataschema_ProPASS_github.xlsx")

# Compare dataschema versions
if(file.exists("input_documents/dataschema_ProPASS.xlsx")){
  dataschema_local <- read_excel_allsheets("input_documents/dataschema_ProPASS.xlsx")
  
  
  test_all_equal <- try(all(dataschema_local$Variables  == dataschema_github$Variables,
                            dataschema_local$Categories == dataschema_github$Categories,na.rm = TRUE),silent = TRUE)
  
  if(class(test_all_equal)[1] == "try-error"){stop("An issue occured when validating the dataschema.")}
  
  if(test_all_equal){
    checks$dataschema_uptodate <- "No change since last run."
    invisible(file.remove("input_documents/dataschema_ProPASS_github.xlsx"))
  }else{
    file.rename(from = "input_documents/dataschema_ProPASS.xlsx",
                to = paste0("archive/dataschema_ProPASS_archived-", format(checks$time_stamp,"%Y-%m-%H%M%S"), ".xlsx"))
    file.rename(from = "input_documents/dataschema_ProPASS_github.xlsx",
                to = "input_documents/dataschema_ProPASS.xlsx")
    message("
    
          ----------------
          Change detected in dataschema!
          
          The local version of the dataschema is archived and,
          replaced by the new dataschema from Github.
          ----------------
          
          ")
    checks$dataschema_uptodate <- "Dataschema updated!"
  }
}

# If the first using the script
if(!file.exists("input_documents/dataschema_ProPASS.xlsx")){
  file.rename(from = "input_documents/dataschema_ProPASS_github.xlsx",
              to = "input_documents/dataschema_ProPASS.xlsx")
  
  checks$dataschema_uptodate <- "First download of the dataschema"
}

# Keep only most recent dataschema
dataschema <- dataschema_github
rm(list = c("dataschema_local","dataschema_github","test_all_equal"))



#---- Get the data processing element ----

dpe_list <- file.info(list.files("input_documents",
                                 pattern = "data_processing_element",
                                 full.names = TRUE
                                 )
                      ) %>%
  mutate(name = rownames(.)) %>% 
  as_tibble %>% 
  select(name, mtime, ctime) %>%
  arrange(desc(ctime))

# Get from Github
download.file(
  url = paste0(
    "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/baseline/data_processing_elements-",
    checks$harmo_group,".xlsx"), 
  destfile = paste0("input_documents/data_processing_element-",
                    checks$harmo_group, 
                    "-github.xlsx"),
  mode = "wb")
dpe_github <- read_excel_allsheets(paste0("input_documents/data_processing_element-",
                                          checks$harmo_group, 
                                          "-github.xlsx"))

# When too many DPE
if(nrow(dpe_list) > 2){stop("Why do you have many data processing elements?")}

if(nrow(dpe_list) == 2){ #Could happen if previously used older version of this script
  file.rename(
    from = dpe_list$name[2],
    to = gsub("input_documents/", "archive/",
              gsub(".xlsx", paste0(format(checks$time_stamp,"_%Y%m%d%H%M%S"),".xlsx"), 
                   dpe_list$name[2]) )
  )
  dpe_list <- dpe_list[1,]
}


# If older DPE present
if(nrow(dpe_list) == 1){
  dpe_local <- read_excel_allsheets(dpe_list$name)
  
  if(identical(dpe_local, dpe_github)){
    file.remove(paste0("input_documents/data_processing_element-",
                       checks$harmo_group, 
                       "-github.xlsx"))
  }else{
    file.rename(
      from = dpe_list$name,
      to = gsub("input_documents/", "archive/",
                gsub(".xlsx", paste0(format(checks$time_stamp,"_%Y%m%d%H%M%S"),".xlsx"), 
                     dpe_list$name) )
    )
    file.rename(
      from = paste0("input_documents/data_processing_element-",
                    checks$harmo_group, 
                    "-github.xlsx"),
      to = paste0("input_documents/data_processing_element-",
                  checks$harmo_group, 
                  "-", format(checks$time_stamp, "%Y-%m-%d"), ".xlsx")
    ) 
    #----------
    #@Optional: 
    #@Run harmo if harmo rules changed since last version
    #----------
    if(checks$dataschema_uptodate != "Dataschema updated!"){
      specific <- menu(choices = c("All [default]", "Only variables changed in DPE"), 
                       title = "
                       What variables do you want to harmonize?")
      if(specific == 2){
        checks$harmo_type <- "subset"
        var_changed <- c(setdiff(dpe_local %>% select(dataschema_variable, contains("mlstr")),
                               dpe_github %>% select(dataschema_variable, contains("mlstr"))
                               ) %>% 
          pull(dataschema_variable),
          setdiff(dpe_github %>% select(dataschema_variable, contains("mlstr")),
                  dpe_local %>% select(dataschema_variable, contains("mlstr"))
          ) %>% 
            pull(dataschema_variable)
        ) %>% unique
        
        dpe_github <- dpe_github %>% 
          filter((trimws(`Mlstr_harmo::rule_category`) == "id_creation") |
                   dataschema_variable %in% var_changed)
        write_excel_allsheets(dpe_github,
                              paste0("input_documents/data_processing_element-",
                                     checks$harmo_group, 
                                     "-", format(checks$time_stamp, "%Y-%m-%d"), ".xlsx")
        )
        rm(var_changed)
      }else{
        checks$harmo_type <- "all"
      }
      rm(specific)
    }
    
  }
  rm(dpe_local)
}


# If first time running script
if(nrow(dpe_list) == 0){
  file.rename(
    from = paste0("input_documents/data_processing_element-",
                  checks$harmo_group, 
                  "-github.xlsx"),
    to = paste0("input_documents/data_processing_element-",
                checks$harmo_group, 
                "-", format(checks$time_stamp, "%Y-%m-%d"), ".xlsx")
  )
  checks$harmo_type <- "all"
}

# Keep only relevent DPE
DPE <- dpe_github
rm(list = c("dpe_github", "dpe_list"))


#---- Checks inputs ----

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
checks$all_vars_in_DPE <- (nrow(DPE) == nrow(dataschema$Variables)) &
  all(DPE$dataschema_variable %in% dataschema$Variables$name)

# Get the variables needed for harmonization
DPE_variables <- get_input_var_names(DPE)

#---- Get input dataset ----

input_dataset <- tibble(
  name = sub('\\..[^\\.]*$', '', list.files("input_dataset/")),
  file = list.files("input_dataset/"),
  path = list.files("input_dataset/", full.names=TRUE)
)

input_dataset$error_format <- !grepl("\\.csv$|\\.xlsx$|\\.rds$|\\.sav$|\\.dta$", input_dataset$file)
checks$input_dataset <- input_dataset 


# Error messages
if(nrow(input_dataset) > 1){stop(call. = FALSE,
                                 "
Multiple input dataset files. Please keep only the one required, and remove the other.")}

if(nrow(input_dataset)==0){stop(call. = FALSE,
                                "
No input file found in the folder 'input_dataset'. Have you added your file(s)?")}
if(any(input_dataset == TRUE))stop(call. = FALSE,
                                   "
There is an issues/limitation with a file in 'input_dataset'.
Please contact Maelstrom Research at ",email_contact,"for help.")

#---- Open file

# get data
if (file_ext(input_dataset$path) == "csv") {
  data <- fabR::read_csv_any_formats(input_dataset$path)
  # if any format is error, put this line back instead :
  # data <- read.csv(input_dataset$path, encoding = "latin1") 
} else if (file_ext(input_dataset$path) == "xlsx") {
  data <- read_excel_allsheets(input_dataset$path)
} else if (file_ext(input_dataset$path) == "rds") {
  data <- readRDS(input_dataset$path)
} else if (file_ext(input_dataset$path) == "sav") {
  data <- read_sav(input_dataset$path)
} else if (file_ext(input_dataset$path) == "dta") {
  data <- read_dta(input_dataset$path)
}

# create a new data frame without confidential variables
data_clean <- data[,which(names(data) %in% DPE_variables)]

message("-data loaded and cleaned-")


if(checks$harmo_type == "all"){
  # --------------------------------------------
  # generate maelstrom datadictionary and summary
  
  data_dictionnary <- data_dict_extract(data_clean)
  data_summary <- dataset_summarize(data_clean)
  
  message("-summary extracted-")
  
  # save the data dictionary in specified folder
  write_excel_allsheets(
    data_dictionnary,
    paste0("output_documents/input data dictionaries/","extracted_data_dict_",input_dataset$name, ".xlsx"))
  write_excel_allsheets(
    data_summary,
    paste0("output_documents/input data summaries/","data_summary_",input_dataset$name, ".xlsx"))
  
  message("-summary reports saved-")
  
}


  
# --------------------------------------------
# checks presence of DPE_variables

if("DPE_vars_in_data" %in% names(checks)){
  DPE_vars_in_data <- checks$DPE_vars_in_data
}else{
  DPE_vars_in_data <- tibble("DPE_variables" = DPE_variables)}

DPE_vars_in_data[[input_dataset$name]] <- DPE_variables %in% names(data_clean)
checks$DPE_vars_in_data <- DPE_vars_in_data 
  
checks$DPE_vars_all_in <- !unlist(lapply(DPE_vars_in_data[c(input_dataset$name)], any))


#---- Save and clean ----

saveRDS(checks, checks_path)
saveRDS(dataschema, "output_documents/dataschema.rds")
saveRDS(DPE, "output_documents/DPE.rds")
# saveRDS(checks_path, "output_documents/checks_path.rds")

rm(list = c("data_dictionnary", "data_summary", "DPE_vars_in_data",
            "DPE_variables", "data", "input_dataset",
            "dataschema_path","checks_list","time_stamp","DPE_list",
            "get_input_var_names", "dpe_path"))
