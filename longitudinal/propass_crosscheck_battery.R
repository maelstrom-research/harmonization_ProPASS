### ProPASS crossreference checks - the check battery
### Variable names follow DataSchema BL 2.0 / FU 3.0.
###
### v1 -> v2 name corrections (v1 used pre-2.0 names that no longer exist):
###   sdc_highest_education        -> sdc_education_highest
###   sdc_edu_secondary            -> sdc_education_secondary
###   pm_height / _method          -> phy_height / phy_height_method
###   pm_weight / _method          -> phy_weight / phy_weight_method
###   pm_waist_circ / _method      -> phy_waist_circ / phy_waist_circ_method
###   dis_cvd_heart_failure_ever   -> dis_cvd_hf_ever
###   dis_cvd_atrial_fib_ever      -> dis_cvd_af_ever
###   med_blood_pressure_cur       -> med_blood_pressure_curr
###   psy_dep_*                    -> cog_dep_*
###   psy_mci_mmse_score           -> cog_mci_mmse
###   psy_mci_moca_score           -> cog_mci_moca
###   phy_mobility_limitation_SF36 -> phe_sf36_phys_function

CVD_SUBTYPES <- c(
  cvd_ihd        = "dis_cvd_ihd_ever",
  cvd_mi         = "dis_cvd_mi_ever",
  cvd_angina     = "dis_cvd_angina_ever",
  cvd_stroke     = "dis_cvd_stroke_ever",
  cvd_hf         = "dis_cvd_hf_ever",
  cvd_af         = "dis_cvd_af_ever",
  cvd_hbp        = "dis_cvd_hbp_diag_ever",
  cvd_hbp_or_med = "dis_cvd_hbp_diag_or_med_ever"   # ADDED: schema lists it as a dis_cvd_ subtype
)

CANCER_SUBTYPES <- c(
  bladder = "dis_cancer_bladder_ever",       breast   = "dis_cancer_breast_ever",
  cervix  = "dis_cancer_cervix_ever",        colorectal = "dis_cancer_colorectal_ever",
  kidney  = "dis_cancer_kidney_ever",        throat   = "dis_cancer_throat_ever",
  leukaemia = "dis_cancer_leukaemia_ever",   lung     = "dis_cancer_lung_ever",
  lymphoma  = "dis_cancer_lymphoma_ever",    prostate = "dis_cancer_prostate_ever",
  skin      = "dis_cancer_skin_ever",        stomach  = "dis_cancer_stomach_ever",
  testicle  = "dis_cancer_testicle_ever",    uterus   = "dis_cancer_uterus_ever",
  brain     = "dis_cancer_brain_ever",       liver    = "dis_cancer_liver_ever",
  ovary     = "dis_cancer_ovary_ever",       pancreas = "dis_cancer_pancreas_ever",
  thyroid   = "dis_cancer_thyroid_ever"
)

ASIAN_SUBTYPES <- c(
  east_asian      = "sdc_ethn_east_asian",
  south_asian     = "sdc_ethn_south_asian",
  southeast_asian = "sdc_ethn_southeast_asian"
)

MEASURE_PAIRS <- list(
  height = c("phy_height", "phy_height_method"),
  weight = c("phy_weight", "phy_weight_method"),
  waist  = c("phy_waist_circ", "phy_waist_circ_method")
)

## psychometric / functional scale plausible ranges
SCALE_RANGES <- list(
  CESD20  = list(var = "cog_dep_cesd20",          lo = 0, hi = 60),
  CESD10  = list(var = "cog_dep_cesd10",          lo = 0, hi = 30),
  Malaise = list(var = "cog_dep_malaise",         lo = 0, hi = 24),
  Kessler = list(var = "cog_dep_kessler",         lo = 0, hi = 50),
  PHQ9    = list(var = "cog_dep_phq9",            lo = 0, hi = 27),
  MMSE    = list(var = "cog_mci_mmse",            lo = 0, hi = 30),
  MoCA    = list(var = "cog_mci_moca",            lo = 0, hi = 30),
  SF36    = list(var = "phe_sf36_phys_function",  lo = 0, hi = 100)
)


run_all_checks <- function(data, dpe, label = NA_character_, include_ethnicity = TRUE) {

  chk <- new_checker(data, dpe, label)
  ck  <- chk$ck

  ## -- education ------------------------------------------------------------
  edu <- c("sdc_education_highest", "sdc_education_secondary")
  ck("education", edu,
     function(d) d$sdc_education_highest == 0 & d$sdc_education_secondary == 1,
     "have highest education = 0 but at-least-upper-secondary set to Yes.")
  ck("education", edu,
     function(d) d$sdc_education_highest > 0 & d$sdc_education_secondary == 0,
     "have highest education > 0 but at-least-upper-secondary set to No.")

  ## -- ethnicity (baseline schema only; absent from FU 3.0) ------------------
  if (include_ethnicity) {
    for (nm in names(ASIAN_SUBTYPES)) {
      sub <- ASIAN_SUBTYPES[[nm]]
      ck(paste0("ethn_", nm), c("sdc_ethn_asian", sub),
         subtype_fn("sdc_ethn_asian", sub),
         paste0("have Asian = No but ", sub, " = Yes."))
    }
  }

  ## -- employment (ADDED: schema crosscheck, commented out in v1) ------------
  emp <- c("sdc_employment_status", "sdc_employment_time")
  ck("employment", emp,
     function(d) d$sdc_employment_status == 0 & !is.na(d$sdc_employment_time),
     "are Unemployed but have a part-time/full-time value.")

  ## -- measurement / method pairs -------------------------------------------
  for (nm in names(MEASURE_PAIRS)) {
    p <- MEASURE_PAIRS[[nm]]
    ck(nm, p, pair_value_no_method(p[1], p[2]),
       paste0("have a ", nm, " value with no measurement method."))
    ck(nm, p, pair_method_no_value(p[1], p[2]),
       paste0("have a ", nm, " measurement method with no value."))   # v1 repeated the
                                                                     # condition above here
  }

  ## -- cvd subtypes ---------------------------------------------------------
  for (nm in names(CVD_SUBTYPES)) {
    sub <- CVD_SUBTYPES[[nm]]
    ck(nm, c("dis_cvd_ever", sub), subtype_fn("dis_cvd_ever", sub),
       paste0("have ", sub, " = Yes but dis_cvd_ever = No."))
  }

  ## -- hbp diagnosed / medication composite ---------------------------------
  ## v1 gated on (diag complete OR med complete) but used BOTH columns.
  hbp <- c("dis_cvd_hbp_diag_or_med_ever", "dis_cvd_hbp_diag_ever", "med_blood_pressure_curr")
  ck("hbp_diag_med", hbp,
     function(d) d$dis_cvd_hbp_diag_or_med_ever == 0 &
                 (d$dis_cvd_hbp_diag_ever == 1 | d$med_blood_pressure_curr == 1),
     "have dis_cvd_hbp_diag_or_med_ever = No but hbp diagnosis or hbp medication = Yes (should be Yes).")
  ck("hbp_diag_med", hbp,
     function(d) d$dis_cvd_hbp_diag_or_med_ever != 0 &
                 d$dis_cvd_hbp_diag_ever == 0 & d$med_blood_pressure_curr == 0,
     "have dis_cvd_hbp_diag_or_med_ever != No but hbp diagnosis and hbp medication both No (should be No).")
  ck("hbp_diag_med", hbp,
     function(d) d$dis_cvd_hbp_diag_or_med_ever != 2 &
                 ((d$dis_cvd_hbp_diag_ever == 0 & is.na(d$med_blood_pressure_curr)) |
                  (is.na(d$dis_cvd_hbp_diag_ever) & d$med_blood_pressure_curr == 0)),
     "have dis_cvd_hbp_diag_or_med_ever != Presumed no with one of diagnosis/medication missing and the other No (should be Presumed no).")

  ## -- cancer subtypes ------------------------------------------------------
  for (nm in names(CANCER_SUBTYPES)) {
    sub <- CANCER_SUBTYPES[[nm]]
    ck(paste0("cancer_", nm), c("dis_cancer_ever", sub), subtype_fn("dis_cancer_ever", sub),
       paste0("have ", sub, " = Yes but dis_cancer_ever = No."))
  }

  ## -- diabetes (ADDED: schema crosscheck) ----------------------------------
  dia <- c("dis_diabetes_ever", "dis_diabetes_type")
  ck("diabetes", dia,
     function(d) d$dis_diabetes_ever == 0 & !is.na(d$dis_diabetes_type),
     "have dis_diabetes_ever = No but a diabetes type recorded.")

  ## -- alcohol (ADDED: schema crosscheck) -----------------------------------
  alc <- c("lsb_alc_freq", "lsb_alc_qty")
  ck("alcohol", alc,
     function(d) d$lsb_alc_freq > 0 & d$lsb_alc_qty == 0,
     "report drinking monthly or more often but a quantity of 0 g/week.")

  ## -- smoking (ADDED: schema crosscheck) -----------------------------------
  ck("smoking_freq", c("lsb_smk_status", "lsb_smk_freq"),
     function(d) d$lsb_smk_status == 0 & !is.na(d$lsb_smk_freq),
     "have never smoked but a smoking frequency recorded.")
  ck("smoking_qty", c("lsb_smk_status", "lsb_smk_qty"),
     function(d) d$lsb_smk_status == 0 & d$lsb_smk_qty > 0,
     "have never smoked but a non-zero smoking quantity.")

  ## -- SF-36 completeness rule (ADDED: schema instruction) ------------------
  ck("sf36_items", c("phe_sf36_phys_function", "phe_sf36_n_items"),
     function(d) !is.na(d$phe_sf36_phys_function) & d$phe_sf36_n_items < 5,
     "have an SF-36 physical function score with fewer than 5 of 10 items completed (should be NA).")

  ## -- scale ranges ---------------------------------------------------------
  ## v1 ran these unguarded, so any cohort missing one of these columns
  ## crashed with "object not found".
  for (nm in names(SCALE_RANGES)) {
    s <- SCALE_RANGES[[nm]]
    ck(nm, s$var, out_of_range(s$var, s$lo, s$hi),
       paste0("have a ", nm, " score outside [", s$lo, ", ", s$hi, "]."))
  }

  chk
}
