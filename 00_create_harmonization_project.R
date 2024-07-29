
#---- Prepare packages and checks----
# v1.10 for ProPASS
# install required packages


#---- GLOBAL FUNCTIONS ----
intro <- function(){
  
  message(
    "This script was prepared by Maelstrom Research.
    
    It script will prepare your environnement:
      - Creates structured folders for your harmonization
      - Downloads required files
      - Opens a rproject where you will work
  
    Please contact us if you have any issues: harmo-propass@maelstrom-research.org")
  
  choice_list <-
    c("AGN - Active aging - The AGNES study",
      "AWH - Australian Longitudinal Study on Women's Health (ALSWH)",
      "BCS - 1970 British Birth Cohort Study",
      "CHS - Copenhagen City Heart Study (Denmark)",
      "DPC - Danish Physical Activity cohort with Objective measurements (DPhacto)",
      "DSE - Danish Observational Study of Eldercare work and MSK disorders (DOSES) (Denmark)",
      "ERC - Study on Nutrition and Cardiovascular Risk (Spain)",
      "FRA - Finish Retirement and Aging Study (FIREA)",
      "H16 - Health 2016",
      "LOF - Lolland-Falster Study (LOFUS) (Denmark)",
      "MSC - The Middle-age Soweto Cohort",
      "MSN - The Maastricht Study (The Netherlands)",
      "NES - Nijmegen Exercise Study/ Healthy Brain",
      "NHS - The Trøndelag Health Study (HUNT 4) (Norway)",
      "SHR - Survey of health, aging and Retirement in Europe",
      "SHS - Singapore Population Health Studies (Singapore)")
  
  harmo_group <- 
    menu(choices = choice_list,
         title = "What cohort do you want to harmonize?")
  
  harmo_group <- substr(choice_list,1,3)[harmo_group]
  
  message(paste0("We will now create the environment for ", harmo_group,"."))
  
  return(harmo_group)
}

# function to get the errors of the DPE and help correcting it 
show_harmo_error_proPASS <- function(checks){
  
  temp_dataset <- Rmonize_DEMO$harmonized_dossier
  
  errors_warnings <-
    bind_rows(checks$harmonization_warnings_detail,checks$harmonization_errors_detail) %>%
    distinct()
  
  if(nrow(errors_warnings) > 1){
    attributes(temp_dataset)$`Rmonize::Data Processing Elements` <-
      errors_warnings
    
    show_harmo_error(temp_dataset)
  }else{ return("no error")}
  
}

# init checks RDS file
checks <- list()

#temporary fix, installing testing version of madshapR (with restricted token)
remove.packages("madshapR")

# checks libraries
if(!require(devtools))    {install.packages("devtools")}
if(!require(tools))    {install.packages("tools")}
if(!require(fabR))     {install.packages("fabR")}
if(!require(madshapR)) {install_github("maelstrom-research/madshapR@testing", auth_token = "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")}
if(!require(Rmonize))  {install.packages("Rmonize")}
if(!require(dplyr))    {install.packages("dplyr")}
if(!require(tidyr))    {install.packages("tidyr")}
if(!require(stringr))  {install.packages("stringr")}
if(!require(haven))    {install.packages("haven")}
if(!require(crayon))    {install.packages("crayon")}
if(!require(htmltools))    {install.packages("htmltools")}
if(!require(car))    {install.packages("car")}

# checks if packages need update
if(packageVersion("fabR")     != "2.1.0"){install.packages("fabR")}
#if(packageVersion("madshapR") != "1.1.0"){install.packages("madshapR")}
if(packageVersion("Rmonize")  != "1.1.0"){install.packages("Rmonize")}

packages <- 
  as_tibble(installed.packages()) %>%
  select(Package, Version, Depends, Suggests, Built)

# catch time_stamp
time_stamp <- Sys.time()

# Track time
checks$time_stamp <- time_stamp

#---- checks packages and versions
checks$R.Version <- R.version.string
checks$RStudio.Version <- RStudio.Version()$version
checks$packages <- packages

cat("\014")
#---- Ask what cohort ----
harmo_group <- intro()

checks$harmo_group <- harmo_group

#---- Create project and folders ----
# Find the folder where your file is located
current_dir <- rstudioapi::getActiveDocumentContext()
current_dir <- current_dir$path
current_dir <- dirname(current_dir)

setwd(current_dir)

# Create folders for your harmonization
folder_name <- paste0("harmonization_", harmo_group)

rstudioapi::initializeProject(path = folder_name)

# Create the folder structure
dir.create(paste0(folder_name, "/archive"))
dir.create(paste0(folder_name, "/input_documents"))
dir.create(paste0(folder_name, "/input_dataset"))
dir.create(paste0(folder_name, "/output_documents"))
dir.create(paste0(folder_name, "/output_dataset"))

#---- Get files from Github ----

# get processing files
download.file(
  url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/01_validation_inputs.R",
  destfile = paste0(folder_name, "/01_validation_inputs.R"),
  mode = "wb")

download.file(
  url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/02_data_transformation.R",
  destfile = paste0(folder_name, "/02_data_transformation.R"),
  mode = "wb")

# Save initial parameters one in archives, one in current output project
saveRDS(checks, paste0(folder_name, "./output_documents/checks_init", ".rds"))

# The last line will open a new project, and ask you if you want to save (you don't need to)
rstudioapi::openProject(path = folder_name)

