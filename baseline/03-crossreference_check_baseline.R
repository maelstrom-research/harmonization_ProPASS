### Crossreference check for validation - BASELINE
### v3: added harmo_group to identify files

source("propass_crosscheck_helpers.R")
source("propass_crosscheck_battery.R")

## ---------------------------------------------------------------------------
## Inputs
## ---------------------------------------------------------------------------
## exclude= keeps "harmonized_data" from also matching the longitudinal file,
## and keeps "data_processing_element" from matching the longitudinal DPE.

data_file <- pick_file("output_dataset",   "harmonized_data",         exclude = "longitudinal")
ds_file   <- pick_file("input_documents",  "dataschema")
dpe_file  <- pick_file("input_documents",  "data_processing_element", exclude = "longitudinal")

message("data       : ", basename(data_file))
message("dataschema : ", basename(ds_file))
message("dpe        : ", basename(dpe_file))

data <- read_sheet(data_file, required_cols = "adm_participant_id")
ds   <- read_sheet(ds_file,   required_cols = c("name", "valueType"))
dpe  <- read_sheet(dpe_file,  required_cols = c("dataschema_variable", "Mlstr_harmo::status"))

## ---------------------------------------------------------------------------
## Sanity checks on the inputs themselves
## ---------------------------------------------------------------------------
dup <- dpe$dataschema_variable[duplicated(dpe$dataschema_variable)]
if (length(dup))
  warning("DPE holds duplicate rows for: ", paste(unique(dup), collapse = ", "),
          " - checks touching these are skipped unless the statuses agree.", call. = FALSE)

missing_from_dpe <- setdiff(ds$name, dpe$dataschema_variable)
if (length(missing_from_dpe))
  message("note: ", length(missing_from_dpe), " DataSchema variable(s) absent from the DPE: ",
          paste(head(missing_from_dpe, 10), collapse = ", "),
          if (length(missing_from_dpe) > 10) " ..." else "")

bad_status <- setdiff(unique(as.character(dpe[["Mlstr_harmo::status"]])),
                      c("complete", "impossible", "undetermined", NA))
if (length(bad_status))
  warning("Unrecognised harmonization status token(s): ",
          paste(bad_status, collapse = ", "), call. = FALSE)

## ---------------------------------------------------------------------------
## Run
## ---------------------------------------------------------------------------
chk    <- run_all_checks(data, dpe, label = "baseline", include_ethnicity = TRUE)
report <- as_report(chk)
print_report(report)

## ---------------------------------------------------------------------------
## Save
## ---------------------------------------------------------------------------
dir.create("output_documents", showWarnings = FALSE)
stamp <- Sys.Date()

crossref <- chk$findings()                       # same shape as v1, for compatibility
write_rds(crossref, paste0("output_documents/crossref_baseline_checks_",harmo_group, "_", stamp, ".rds"))
write_csv(report,   paste0("output_documents/crossref_baseline_checks_",harmo_group, "_", stamp, ".csv"))

message("\nwrote output_documents/crossref_baseline_checks_",harmo_group, "_", stamp, ".{rds,csv}")
