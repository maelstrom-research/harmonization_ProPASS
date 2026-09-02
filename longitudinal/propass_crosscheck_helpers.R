### ProPASS crossreference checks - shared helpers
### Sourced by crossref_baseline_v2.R and crossref_longitudinal_v2.R

suppressPackageStartupMessages({
  library(fabR)
  library(madshapR)
  library(tidyverse)
})

## ---------------------------------------------------------------------------
## File discovery
## ---------------------------------------------------------------------------
## list.files() is case-sensitive by default and returns a VECTOR. Both facts
## broke v1: "dataschema" never matched "DataSchema", and "harmonized_data"
## also matched "harmonized_data_longitudinal".

pick_file <- function(path, pattern, exclude = NULL) {
  if (!dir.exists(path))
    stop("Directory not found: ", normalizePath(path, mustWork = FALSE), call. = FALSE)

  f <- list.files(path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]                       # Excel lock files
  if (!is.null(exclude)) f <- f[!grepl(exclude, basename(f), ignore.case = TRUE)]

  if (length(f) == 0)
    stop("No file in '", path, "' matches /", pattern, "/.\n  Files present: ",
         paste(list.files(path), collapse = ", "), call. = FALSE)
  if (length(f) > 1)
    stop("Pattern /", pattern, "/ matched ", length(f), " files:\n  ",
         paste(basename(f), collapse = "\n  "),
         "\n  Narrow the pattern or pass exclude=.", call. = FALSE)
  f
}

## read_excel_allsheets() returns a NAMED LIST of sheets, never a data frame.
## v1 treated the list as a tibble, so dpe$dataschema_variable was NULL.
read_sheet <- function(file, required_cols) {
  x <- fabR::read_excel_allsheets(file)
  if (is.data.frame(x)) x <- list(sheet_1 = x)

  hit <- vapply(x, function(s) all(required_cols %in% names(s)), logical(1))
  if (!any(hit))
    stop("No sheet in '", basename(file), "' has the required columns (",
         paste(required_cols, collapse = ", "), ").\n  Sheets: ",
         paste(names(x), collapse = ", "),
         "\n  Columns in first sheet: ", paste(names(x[[1]]), collapse = ", "),
         call. = FALSE)
  if (sum(hit) > 1)
    message("  note: ", sum(hit), " sheets qualify in ", basename(file),
            "; using '", names(x)[which(hit)[1]], "'")
  tibble::as_tibble(x[[which(hit)[1]]])
}

## ---------------------------------------------------------------------------
## Harmonization status lookup
## ---------------------------------------------------------------------------
## Returns character(0) if the variable is not in the DPE, or >1 value if the
## DPE holds several rows for it. Never feed these straight to if().

harmo_status <- function(dpe, var) {
  i <- which(dpe$dataschema_variable == var)
  if (length(i) == 0) return(character(0))
  unique(as.character(dpe[["Mlstr_harmo::status"]][i]))
}

## ---------------------------------------------------------------------------
## Checker object: guards every check and logs why any check was skipped
## ---------------------------------------------------------------------------

new_checker <- function(data, dpe, label = NA_character_) {

  findings <- list()
  skipped  <- list()

  why_not <- function(vars) {
    out <- character()
    for (v in vars) {
      s <- harmo_status(dpe, v)
      if (length(s) == 0)              out <- c(out, paste0(v, ": not in DPE"))
      else if (all(is.na(s)))          out <- c(out, paste0(v, ": status is NA"))
      else if (length(s) > 1)          out <- c(out, paste0(v, ": conflicting statuses (",
                                                            paste(s, collapse = " / "), ")"))
      else if (!identical(s, "complete")) out <- c(out, paste0(v, ": status = ", s))
      else if (!v %in% names(data))    out <- c(out, paste0(v, ": complete in DPE but column absent from dataset"))
      else if (all(is.na(data[[v]])))
        out <- c(out, paste0(v, ": complete in DPE but column is entirely NA"))
      else if (mean(is.na(data[[v]])) > 0.95)
        message("  note: ", v, " is ", round(100 * mean(is.na(data[[v]]))),
                "% NA despite status = complete")
    }
    out
  }

  ## key  : name of the finding
  ## vars : every variable the condition touches (all must be complete AND present)
  ## cond : function(d) -> logical vector
  ## msg  : sentence completing "N participant(s) ..."
  ck <- function(key, vars, cond, msg) {
    bad <- why_not(vars)
    if (length(bad)) {
      skipped[[key]] <<- bad
      return(invisible(NULL))
    }
    n <- sum(cond(data) %in% TRUE)          # %in% TRUE is NA-safe
    if (n > 0)
      findings[[key]] <<- c(findings[[key]], paste0(n, " participant(s) ", msg))
    invisible(NULL)
  }

  list(
    ck       = ck,
    findings = function() findings,
    skipped  = function() skipped,
    label    = function() label
  )
}

## condition builders (force() prevents the classic loop-variable capture bug)
subtype_fn <- function(parent, sub) {
  force(parent); force(sub)
  function(d) d[[parent]] == 0 & d[[sub]] == 1
}
pair_value_no_method <- function(value, method) {
  force(value); force(method)
  function(d) !is.na(d[[value]]) & is.na(d[[method]])
}
pair_method_no_value <- function(value, method) {
  force(value); force(method)
  function(d) is.na(d[[value]]) & !is.na(d[[method]])
}
out_of_range <- function(v, lo, hi) {
  force(v); force(lo); force(hi)
  function(d) !is.na(d[[v]]) & (d[[v]] < lo | d[[v]] > hi)
}

## ---------------------------------------------------------------------------
## Reporting
## ---------------------------------------------------------------------------

as_report <- function(chk) {
  f <- chk$findings(); s <- chk$skipped()
  dplyr::bind_rows(
    if (length(f))
      tibble::tibble(dataset = chk$label(), type = "finding",
                     check = rep(names(f), lengths(f)), detail = unlist(f, use.names = FALSE)),
    if (length(s))
      tibble::tibble(dataset = chk$label(), type = "skipped",
                     check = rep(names(s), lengths(s)), detail = unlist(s, use.names = FALSE))
  )
}

print_report <- function(rep) {
  fin <- dplyr::filter(rep, type == "finding")
  skp <- dplyr::filter(rep, type == "skipped")
  cat("\n--- findings (", nrow(fin), ") ---\n", sep = "")
  if (nrow(fin)) for (i in seq_len(nrow(fin))) cat("  [", fin$check[i], "] ", fin$detail[i], "\n", sep = "")
  else cat("  none\n")
  cat("--- skipped checks (", length(unique(skp$check)), ") ---\n", sep = "")
  if (nrow(skp)) for (i in seq_len(nrow(skp))) cat("  [", skp$check[i], "] ", skp$detail[i], "\n", sep = "")
  else cat("  none\n")
}
