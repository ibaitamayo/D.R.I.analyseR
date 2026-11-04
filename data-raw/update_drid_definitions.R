# Rewrites the DRID definition tables with the three rules agreed on these data:
#
#   1. Viral hepatitis before 1990 is hepatitis B. The virus was identified in 1989, so an
#      unspecified "viral hepatitis" recorded before then cannot be hepatitis C.
#   2. Hepatitis C is a specific code or an eradicating treatment. Unspecified viral hepatitis
#      from 1990 on cannot diagnose it, but in someone diagnosed or treated it is often the
#      first record of the condition, so it keeps dating it.
#   3. Injecting drug use is hepatitis C or an opioid use disorder. Its role is 'marker': it
#      says we saw that the person injected, it does not start their follow-up and it is never
#      an outcome. The cohort and its entry date are unchanged; the restriction is applied, or
#      not, when the results are read. Opioid agonist treatment and
#      skin and soft-tissue infections were tested as well and are not used to find people: on
#      their own, each carries about the same prevalence of hepatitis C as the rest of the
#      cohort (3.8 % and 6.9 % against 3.5 %), so they add members without adding injectors.
#      Opioid agonist treatment stays in the table as its own treatment event; the skin codes
#      are not in the definitions at all, they were only measured.
#
# Usage:  Rscript data-raw/update_drid_definitions.R
suppressMessages({library(readxl); library(writexl); library(data.table)})
DX  <- "examples/definition_dx_DRID.xlsx"
ATC <- "examples/definition_atc_DRID.xlsx"
D72_FROM <- "1990-01-01"; D72_TO <- "1989-12-31"

# ---- 0: sources checked against the originals ---------------------------------------------
# Each line is a label that was wrong or outdated, and what it is really. Checked one by one;
# where the original could not be reached, the label says so rather than naming a year.
SOURCE_FIX <- c(
  "Quan 2008 (+ Z86.4 Ball 2017)" =
    "Quan 2005, Med Care 43(11):1130-9, Elixhauser drug abuse (ICD-10 and enhanced ICD-9-CM); Z86.4 added from Ball 2017",
  "Quan 2008 (Elixhauser drug abuse)" =
    "Quan 2005, Med Care 43(11):1130-9, Elixhauser drug abuse (ICD-10 and enhanced ICD-9-CM)",
  "EUDA Selection B (T40.0-T40.9, T43.6)" =
    paste("EUDA definition for general mortality registers (until 2023 'Selection B'), T40.0-T40.9 and T43.6.",
          "There it is a cause of death and the T code is combined with an external cause (X42, X62, Y12, X44, X64, Y14);",
          "here it is applied to diagnosis records, where no external cause exists, so the T code is used alone"),
  "ICD-9 equivalent of EUDA Selection B" =
    "ICD-9 equivalent of the EUDA definition for general mortality registers (until 2023 'Selection B')",
  "Ball 2017 (IDU-associated endocarditis)" =
    "Ball 2017, validated algorithm for injection-drug-use-associated infective endocarditis: hepatitis C codes are a component of it, not a separate source",
  "INE list of 102 causes of death, group 95" =
    "INE reduced list of 102 causes of death, group 095 'Envenenamiento accidental por psicofarmacos y drogas de abuso' (ICD-10 X41, X42, X44, X45; ICD-9 E850-E855, E858.9, E860)",
  "Not in INE group 95 (methadone); kept by decision" =
    "Methadone poisoning. Whether INE group 095 includes it depends on reading 'E850.0-.9' as a range or as two codes; kept by decision")

dx <- as.data.table(read_excel(DX))
hit <- intersect(names(SOURCE_FIX), unique(dx$Source))
dx[Source %in% hit, Source := SOURCE_FIX[Source]]
done <- sum(SOURCE_FIX %in% unique(dx$Source))
cat("sources: ", length(hit), " corrected now, ", done, " of ", length(SOURCE_FIX),
    " already right (the script is idempotent)\n", sep = "")
miss <- names(SOURCE_FIX)[!SOURCE_FIX %in% unique(dx$Source)]
if (length(miss)) cat("  neither the old nor the new label is in the table, check by hand: ",
                      paste(miss, collapse = " | "), "\n", sep = "")
for (col in c("Use", "ValidFrom", "ValidTo")) if (!col %in% names(dx)) dx[, (col) := NA_character_]
dx[, Use := fifelse(is.na(Use), "case", Use)]

# ---- 1 and 2: what D72 is, and when -----------------------------------------------------
dx <- dx[!(Disease %in% c("HCV", "HBV") & DxCode == "D72")]
dx <- rbind(dx, data.table(
  Disease   = c("HBV", "HCV"),
  Role      = "disease",
  CodeType  = "CIAP-2",
  DxCode    = "D72",
  Source    = c("Decision: viral hepatitis recorded before 1990 cannot be hepatitis C, the virus was identified in 1989",
                "Decision: unspecified viral hepatitis dates a confirmed case and never diagnoses one"),
  Status    = NA_character_,
  Use       = c("case", "date"),
  ValidFrom = c(NA, D72_FROM),
  ValidTo   = c(D72_TO, NA)), fill = TRUE)

# ---- 3: injecting drug use ---------------------------------------------------------------
dx <- dx[Disease != "Injecting_drug_use"]
hcv_codes <- dx[Disease == "HCV" & Use == "case", .(CodeType, DxCode)]
opioid <- data.table(
  CodeType = c(rep("CIE-10-ES", 2), rep("CIE-9-MC", 4)),
  DxCode   = c("F11", "T40.1", "304.0", "305.5", "E850.0", "965.01"),
  Source   = paste("Decision: opioid use disorder, the only non-circular marker whose prevalence of",
                   "diagnosed hepatitis C (44.1 %) matches the national figure for people who inject drugs",
                   "(OEDA 2023: 40.4 % injecting in the last 12 months, 48.2 % ever). Opioid agonist treatment",
                   "alone (3.8 %) and skin infections alone (6.9 %) do not, against 3.5 % in the rest of the cohort"))
dx <- rbind(dx, rbind(
  data.table(Disease = "Injecting_drug_use", Role = "marker", opioid, Status = NA_character_,
             Use = "case", ValidFrom = NA_character_, ValidTo = NA_character_),
  data.table(Disease = "Injecting_drug_use", Role = "marker", hcv_codes,
             Source = "Decision: in a cohort of diagnosed drug users, hepatitis C is acquired by injecting; blood products have been screened since 1990",
             Status = NA_character_, Use = "case", ValidFrom = NA_character_, ValidTo = NA_character_)
), fill = TRUE)

setcolorder(dx, c("Disease", "Role", "CodeType", "DxCode", "Use", "ValidFrom", "ValidTo"))
setorder(dx, Disease, CodeType, DxCode)
write_xlsx(as.data.frame(dx), DX)

# ---- the ATC side: treatment makes the case, for both events -----------------------------
atc <- as.data.table(read_excel(ATC))
atc <- atc[!Disease %in% c("HCV", "Injecting_drug_use")]
erad <- unique(atc[Disease == "HCV_treatment_all", .(ATC, MinDispensations)])
atc <- rbind(atc, rbind(
  data.table(Disease = "HCV", erad, Role = "disease",
             Source = "Decision: an eradicating treatment is only dispensed to someone with hepatitis C"),
  data.table(Disease = "Injecting_drug_use", erad, Role = "marker",
             Source = "Decision: treated hepatitis C in a drug-using cohort")), fill = TRUE)
setorder(atc, Disease, ATC)
write_xlsx(as.data.frame(atc), ATC)

cat("definition_dx_DRID.xlsx :", nrow(dx), "rows\n")
print(dx[, .(filas = .N, case = sum(Use == "case"), date = sum(Use == "date")), by = Disease][order(-filas)])
cat("\ndefinition_atc_DRID.xlsx:", nrow(atc), "rows\n")
print(atc[, .(filas = .N), by = Disease][order(-filas)])
