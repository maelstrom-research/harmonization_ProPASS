
#---- Prepare packages and checks----
#v1.8 for ProPASS
# install required packages
if (!"remotes" %in% installed.packages()) {install.packages("remotes")}
if (!"fabR" %in% installed.packages()) {install.packages("fabR")}
if (!"madshapR" %in% installed.packages()) {install.packages("madshapR")}
if (!"Rmonize" %in% installed.packages()) {install.packages("Rmonize")}

library(dplyr)

# Check if packages need update
if(installed.packages() %>% as_tibble() %>% filter(Package == "fabR") %>% pull(Version) != "2.1.0"){
  install.packages("fabR")
}
if(installed.packages() %>% as_tibble() %>% filter(Package == "madshapR") %>% pull(Version) != "1.1.0"){
  install.packages("fabR")
}
if(installed.packages() %>% as_tibble() %>% filter(Package == "Rmonize") %>% pull(Version) != "1.1.0"){
  install.packages("Rmonize")
}

# Checks 
checks <- list()


#---- Ask what cohort ----
intro <- function(){
  message(
    "This script was prepared by Maelstrom Research.
    
    It script will prepare your environnement:
      - Creates structured folders for your harmonization
      - Downloads required files
      - Opens a rproject where you will work
  
    Please contact us if you have any issues: harmo-propass@maelstrom-research.org")
  
  harmo_group <- 
    menu(choices = c("AGN - Active aging - The AGNES study",
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
                     "SHS - Singapore Population Health Studies (Singapore)"),
         title = "What cohort do you want to harmonize?")
  
  harmo_group <- c("AGN", "AWH", "BCS", "CHS", "DPC", "DSE", "ERC", "FRA", "H16", "LOF", "MSC", "MSN", "NES", "NHS", "SHR", "SHS")[harmo_group]
  
  message(paste0("We will now create the environment for ", harmo_group,"."))
  
  return(harmo_group)
}

cat("\014")
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
# Get data processing elements... save in 2 folders
remotes:::download(path = paste0(folder_name,
                                 "/archive/data_processing_element-",
                                 harmo_group, "-",
                                 format(Sys.time(), "%Y-%m-%d"), 
                                 ".xlsx"), 
                   url = paste0("https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/data_processing_elements-",
                                harmo_group,
                                ".xlsx")
)
remotes:::download(path = paste0(folder_name,
                                 "/input_documents/data_processing_element-",
                                 harmo_group,
                                 "-to_Validate.xlsx"), 
                   url = paste0("https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/data_processing_elements-",
                                harmo_group,
                                ".xlsx")
)

remotes:::download(path = paste0(folder_name, "/input_documents/dataschema_ProPASS.xlsx"),
                   url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/dataschema_ProPASS.xlsx")
remotes:::download(path = paste0(folder_name, "/01_validation_inputs.R"),
                   url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/master/01_validation_inputs.R")


# Track time
checks$download_dpe <- Sys.time()

#---- Check packages, save and clean ----
checks$packages <- installed.packages() %>% 
  as_tibble() %>% 
  select(Package, Version, Depends, Suggests, Built)

saveRDS(checks, paste0(folder_name, "/output_documents/checks.rds"))
saveRDS(checks, paste0(folder_name, "/archive/checks", Sys.Date(), ".rds"))

# Clean a little
rm(list = c("checks", "current_dir", "harmo_group", "intro"))


# The last line will open a new project, and ask you if you want to save (you don't need to)
rstudioapi::openProject(path = folder_name)

