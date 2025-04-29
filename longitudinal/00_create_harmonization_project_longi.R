
#---- Prepare packages ----
# v2.00 for ProPASS
# install required packages
# # checks libraries

if(!require(fabR))     {install.packages("fabR")}
if(!require(madshapR))     {install.packages("madshapR")}
if(!require(Rmonize))  {install.packages("Rmonize")}

if(!require(htmltools))    {install.packages("htmltools")}
if(!require(car))    {install.packages("car")}

if(substr(packageVersion("fabR"), start = 1, stop = 3) < 2.1 |
   substr(packageVersion("madshapR"), start = 1, stop = 3) < 1.1 |
   substr(packageVersion("Rmonize"), start = 1, stop = 3) < 1.1){
  install.packages("fabR")
  install.packages("madshapR")
  install.packages("Rmonize")
}


#---- Prepare intro function and checks ----
intro <- function(){
  
  message(
    "This script was prepared by Maelstrom Research.
    
    It script will prepare your environnement:
      - Creates structured folders for your harmonization
      - Downloads required files
      - Opens a rproject where you will work
  
    Please contact us if you have any issues: harmo-propass@maelstrom-research.org")
  
  choice_list <-
    c(
      "AGN - Active aging - The AGNES study",
      #"AWH - Australian Longitudinal Study on Women's Health (ALSWH)",
      "BCS - 1970 British Birth Cohort Study",
      #"CHK - Chinese University of Hong Kong",
      "CHS - Copenhagen City Heart Study (Denmark)",
      #"DNH - Danish National Health Survey",
      #"DPC - Danish Physical Activity cohort with Objective measurements (DPhacto)",
      #"DSE - Danish Observational Study of Eldercare work and MSK disorders (DOSES) (Denmark)",
      #"ERC - Study on Nutrition and Cardiovascular Risk (Spain)",
      "FRA - Finish Retirement and Aging Study (FIREA)",
      #"H16 - Health 2016",
      #"HBS - Healthy Brain Study",
      #"KSA - King Faisal University Saudi Arabia",
      #"LOF - Lolland-Falster Study (LOFUS) (Denmark)",
      "MSC - The Middle-age Soweto Cohort",
      "MSN - The Maastricht Study (The Netherlands)",
      "NES - Nijmegen Exercise Study/ Healthy Brain",
      "NHS - The Trøndelag Health Study (HUNT 4) (Norway)",
      "SHR - Survey of health, aging and Retirement in Europe"
      #"SHS - Singapore Population Health Studies (Singapore)"
      )
  
  harmo_group <- 
    menu(choices = choice_list,
         title = "What cohort do you want to harmonize?")
  
  harmo_group <- substr(choice_list,1,3)[harmo_group]
  
  message(paste0("We will now create the environment for ", harmo_group,"."))
  
  return(harmo_group)
}

# init checks RDS file
checks <- list()

#---- Checks packages and environment ----
checks$R.Version <- R.version.string
checks$RStudio.Version <- RStudio.Version()$version
checks$packages <- as_tibble(installed.packages()) %>%
  select(Package, Version, Depends, Suggests, Built)

checks$time_stamp <- Sys.time()

#---- Ask what cohort ----
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
folder_name <- paste0("harmo_longitudinal_", harmo_group)
rstudioapi::initializeProject(path = folder_name)

# Create the folder structure
dir.create(paste0(folder_name, "/archive"))
dir.create(paste0(folder_name, "/input_documents"))
dir.create(paste0(folder_name, "/input_dataset"))
dir.create(paste0(folder_name, "/output_documents"))
dir.create(paste0(folder_name, "/output_dataset"))

#---- Get files from Github ----
#@CHANGE links when github
# get processing files
download.file(
  url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/refs/heads/master/longitudinal/01_validation_inputs_longi",
  destfile = paste0(folder_name, "/01_validation_inputs_longi.R"),
  mode = "wb")
download.file(
  url = "https://github.com/maelstrom-research/harmonization_ProPASS/raw/refs/heads/master/longitudinal/02_data_transformation_longi",
  destfile = paste0(folder_name, "/02_data_transformation_longi.R"),
  mode = "wb")

#---- Save and open project ----

# Save initial parameters one in archives, one in current output project
saveRDS(checks, paste0(folder_name, "/output_documents/checks_init", ".rds"))
rm(list = c("current_dir", "intro", "harmo_group", "checks"))

# The last line will open a new project, and ask you if you want to save (you don't need to)
rstudioapi::openProject(path = folder_name)

