### Crossreference check for validation - LONGITUDINAL
### v3 : More flexible wave check

source("propass_crosscheck_helpers.R")
source("propass_crosscheck_battery.R")

## ---------------------------------------------------------------------------
## Inputs
## ---------------------------------------------------------------------------
data_file <- pick_file("output_dataset",  "harmonized_data_longitudinal")
ds_file   <- pick_file("input_documents", "dataschema")
dpe_file  <- pick_file("input_documents", "data_processing_element.*longitudinal|longitudinal.*data_processing_element")

message("data       : ", basename(data_file))
message("dataschema : ", basename(ds_file))
message("dpe        : ", basename(dpe_file))

data <- read_sheet(data_file, required_cols = c("adm_participant_id", "adm_wave"))
ds   <- read_sheet(ds_file,   required_cols = c("name", "valueType"))
dpe  <- read_sheet(dpe_file,  required_cols = c("dataschema_variable", "Mlstr_harmo::status",
                                                "input_dataset"))

## ---------------------------------------------------------------------------
## Map DPE input_dataset -> the rows of the harmonized dataset it produced
## ---------------------------------------------------------------------------
## v1 used `filter(adm_wave == i + 1)`, i.e. it assumed the i-th element of
## unique(dpe$input_dataset) is wave i+1. unique() returns first-appearance
## order, which is not guaranteed to be wave order, so waves could be silently
## crosswired. Prefer an explicit link.

datasets <- unique(dpe$input_dataset)
datasets <- datasets[!is.na(datasets)]

## Option A (preferred): the dataset carries adm_dataset_id matching input_dataset.
## Option B: fill in the mapping by hand.
WAVE_MAP <- build_wave_map(data, datasets)
if (length(WAVE_MAP)) verify_wave_map(data, dpe, WAVE_MAP)

rows_for <- function(ds_name) {
  if ("adm_dataset_id" %in% names(data) && any(data$adm_dataset_id == ds_name, na.rm = TRUE))
    return(dplyr::filter(data, adm_dataset_id == ds_name))
  if (ds_name %in% names(WAVE_MAP))
    return(dplyr::filter(data, adm_wave == WAVE_MAP[[ds_name]]))
  stop("Cannot locate rows for input_dataset '", ds_name, "'. ",
       "Either add adm_dataset_id to the harmonized data, or add an entry to WAVE_MAP.",
       call. = FALSE)
}

## ---------------------------------------------------------------------------
## Run, one DPE input_dataset at a time
## ---------------------------------------------------------------------------
reports <- list()

for (ds_name in datasets) {
  
  dpe_i  <- dplyr::filter(dpe, input_dataset == ds_name)
  data_i <- rows_for(ds_name)
  
  message("\n=== ", ds_name, " : ", nrow(data_i), " participant(s), ",
          nrow(dpe_i), " DPE row(s), wave(s) ",
          paste(sort(unique(data_i$adm_wave)), collapse = "/"))
  
  if (nrow(data_i) == 0) {
    warning("No harmonized rows matched '", ds_name, "' - skipped.", call. = FALSE)
    next
  }
  
  ## FU 3.0 has no sdc_ethn_* variables
  chk <- run_all_checks(data_i, dpe_i, label = ds_name, include_ethnicity = FALSE)
  rep <- as_report(chk)
  print_report(rep)
  
  reports[[ds_name]] <- rep      # [[ ]], not [ ] - v1's `cross_ref_long[i] <- crossref`
}                                # errors as soon as crossref has >1 element

report <- dplyr::bind_rows(reports)

## ---------------------------------------------------------------------------
## Save
## ---------------------------------------------------------------------------
dir.create("output_documents", showWarnings = FALSE)
stamp <- Sys.Date()

cross_ref_long <- reports
write_rds(cross_ref_long, paste0("output_documents/crossref_long_checks_",ds_name, "_", stamp, ".rds"))
write_csv(report,         paste0("output_documents/crossref_long_checks_",ds_name, "_", stamp, ".csv"))

message("\nwrote output_documents/crossref_long_checks_",ds_name, "_", stamp, ".{rds,csv}")
