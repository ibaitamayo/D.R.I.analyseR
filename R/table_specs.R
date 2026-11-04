# What each input table must contain: shown in the "Start here" tab and in each upload card.
# Single source, so both places always say the same.

DATE_HELP <- paste("Date. Accepted: YYYY-MM-DD (recommended), DD/MM/YYYY, DD-MM-YYYY, YYYY/MM/DD, DD.MM.YYYY or YYYYMMDD.",
                   "A time part is ignored. Excel date cells are accepted. The format is detected automatically and shown after loading.")
TYPE_HELP <- paste("Text: label of the coding system. Recognised labels (upper or lower case):",
                   "CIAP, CIAP2, ICPC, ICPC-2 → ICPC (primary care);",
                   "9, CIE9, CIE-9-MC, ICD9, ICD-9-CM → ICD-9;",
                   "10, 10ES, CIE10, CIE-10-ES, ICD10, ICD-10-CM, ICD-10-GM, ICD-10-CA, ICD-10 (WHO)… → ICD-10;",
                   "SNOMED, SNOMED CT → SNOMED.",
                   "SNOMED identifiers are compared whole, never as a prefix: there is no hierarchy inside the",
                   "identifier, so a list of them is an enumeration and is used exactly as given.",
                   "Please include the national version in the label (e.g. CIE-10-ES, ICD-10-CM, ICD-10-GM, ICD-10 (WHO)):",
                   "code lists differ between versions (e.g. HIV is B20 in ICD-10-CM / CIE-10-ES but B20–B24 in WHO ICD-10),",
                   "and the app warns when data and definitions use different versions.",
                   "Rows with other labels are not used.")

DRUG_TYPE_HELP <- paste("Text: label of the drug coding system. Recognised: ATC, atc; RxNorm, RxCUI.",
                        "If the column is missing the whole table is read as ATC, and the app says so.",
                        "ATC is compared by prefix (N07BC includes N07BC02); an RxNorm identifier is compared",
                        "whole, because it has no hierarchy inside. The two are never compared with each other.")

table_specs <- list(
  pdx = list(
    title = "Persons and diagnoses",
    row = "One row = one diagnosis recorded for one patient on one date.",
    fields = data.frame(
      Field = c("Patient ID", "Diagnosis code", "Code type", "Diagnosis date"),
      Required = c("Yes", "Yes", "Yes", "Yes"),
      Format = c("Text or number. Must be the same identifier used in the patient – drug table.",
                 "Text, as recorded, with or without dot (F11.20, F1120, 304.01, P19). One code per row.",
                 TYPE_HELP, DATE_HELP),
      Example = c("100234", "F11.20", "10ES", "2019-05-01")),
    example = data.frame(Pac_Unif_Cod = c("100234", "100234", "100871"), Diag_Cod = c("F11.20", "P19", "070.54"),
                         Diag_Tipo = c("10ES", "CIAP2", "9"), ResDiag_Inicio_Fecha = c("2019-05-01", "2018-01-15", "2009-11-03"))),
  pdrug = list(
    title = "Persons and treatments",
    row = "One row = one dispensation of one drug to one patient on one date.",
    fields = data.frame(
      Field = c("Patient ID", "Drug code", "Code type", "Prescription or dispensation date"),
      Required = c("Yes", "Yes", "No", "Yes"),
      Format = c("Text or number. Same identifier as in the patient – diagnosis table.",
                 "Text: code of the dispensed drug. ATC at any level (J05AP55, N07BC02), or an RxNorm identifier (1659131).",
                 DRUG_TYPE_HELP, DATE_HELP),
      Example = c("100234", "J05AP55", "ATC", "2018-01-10")),
    example = data.frame(Pac_Unif_Cod = c("100234", "100234", "100871"), Codigo = c("J05AP55", "J05AP55", "N07BC02"),
                         Tipo = c("ATC", "ATC", "ATC"),
                         Fecha_dispensacion = c("2018-01-10", "2018-02-10", "2016-06-01"))),
  ppat = list(
    title = "Patient covariates and death causes",
    row = "One row = one person. Everything except the identifier is optional; what is missing is simply not derived.",
    fields = data.frame(
      Field = c("Patient ID", "Sex", "Year of birth", "Date of birth", "Date of death", "Cause-of-death code",
                "Code type of the cause", "Area or zone", "End of follow-up"),
      Required = c("Yes", "No", "No", "No", "No", "No", "If there is a cause code", "No", "No"),
      Format = c("Text or number. Same identifier as in the other patient tables.",
                 "Text, as recorded. Values are taken as they come; no category is renamed.",
                 "Whole number. Age is NOT a column: a stored age would be the age at extraction, not at the time zero of the analysis, so the app derives it from the year of birth.",
                 paste(DATE_HELP, "Used instead of the year of birth when both are given: with only a year, the birth event is dated 1 July,",
                       "the midpoint of the year, and age carries that same uncertainty."),
                 DATE_HELP,
                 "Text: the basic cause of death, one code per person, as recorded.",
                 TYPE_HELP,
                 "Text: health area or zone, used for matching when the drug-user population is defined.",
                 paste(DATE_HELP, "Last contact, loss of entitlement or any other end of follow-up.",
                       "Without it, no date of death means alive until the end of the data, which counts as time at risk.")),
      Example = c("100234", "F", "1978", "1978-05-02", "2021-03-14", "B18.2", "CIE-10-ES", "ZBS07", "2024-12-31")),
    example = data.frame(Pac_Unif_Cod = c("100234", "100871"), Sexo = c("F", "M"),
                         Anio_nac = c("1978", "1965"), Fecha_defuncion = c("2021-03-14", ""),
                         Causa = c("B18.2", ""), Causa_tipo = c("CIE-10-ES", ""),
                         Zona = c("ZBS07", "ZBS02"))),
  ddx = list(
    title = "Disease – diagnosis code (definition)",
    row = "One row = one diagnosis code included in the definition of one disease.",
    fields = data.frame(
      Field = c("Disease", "Code type", "Diagnosis code", "Use of the code",
                "Code valid from / valid to", "Other columns"),
      Required = c("Yes", "Yes", "Yes", "No", "No", "No"),
      Format = c("Text: disease or event name. Rows with the same name form one definition. Use exactly the same name in the disease – ATC table to combine diagnoses and drugs.",
                 TYPE_HELP,
                 paste("Text. No dot = whole category (E860 includes E860.0, E860.4…). With a dot = that level and its children",
                       "(E86.0 includes E86.01, not E86.1). One code per row: no ranges or abbreviations",
                       "(write E850.0, E850.8, E850.9 in three rows, not E850.0.8.9)."),
                 paste("Text: 'case' (default) or 'date'. A 'case' code says the person has the disease and can date it.",
                       "A 'date' code is not specific enough to diagnose anyone, but in a person who has the disease on other",
                       "evidence it can be the earliest record of it: an unspecified code ('viral hepatitis', 'drug abuse')",
                       "often is. Nobody gets the disease from 'date' codes alone, and a definition made only of them is refused."),
                 paste("Date, same formats as everywhere else. Limits the records this code is compared against, so the same",
                       "code can belong to one disease up to a date and to another one after it (unspecified viral hepatitis",
                       "before 1990 cannot be hepatitis C). Either end may be left empty."),
                 "Allowed and kept for documentation (e.g. Source, Notes); not used in the search."),
      Example = c("HCV", "ICD-10-CM", "B18.2", "case", "1990-01-01", "Ball 2017")),
    example = data.frame(Disease = c("HCV", "HCV", "HBV", "Drug_use"),
                         CodeType = c("ICD-10-CM", "CIAP2", "CIAP2", "CIAP2"),
                         DxCode = c("B18.2", "D72", "D72", "P19"),
                         Use = c("case", "date", "case", "case"),
                         ValidFrom = c("", "1990-01-01", "", ""),
                         ValidTo = c("", "", "1989-12-31", ""),
                         Source = c("Ball 2017", "Dates a confirmed case", "Before 1990 it cannot be HCV", "BDCAP"))),
  dcause = list(
    title = "Cause of death – code (definition)",
    row = "One row = one code included in the definition of one cause of death.",
    fields = data.frame(
      Field = c("Cause", "Code type", "Code", "Other columns"),
      Required = c("Yes", "Yes", "Yes", "No"),
      Format = c("Text: name of the cause, as it should appear in the results (HCV_death, Chronic_liver_disease_death).",
                 TYPE_HELP,
                 "Text, as recorded, with or without dot. Same granularity rule as diagnoses.",
                 "Allowed and kept for documentation; not used in the search."),
      Example = c("HCV_death", "CIE-10-ES", "B18.2", "Source")),
    example = data.frame(Disease = c("HCV_death", "Chronic_liver_disease_death"),
                         CodeType = c("CIE-10-ES", "CIE-10-ES"), DxCode = c("B18.2", "K74"))),
  datc = list(
    title = "Disease – ATC code (definition)",
    row = "One row = one ATC code included in the definition of one disease.",
    fields = data.frame(
      Field = c("Disease", "Drug code", "Code type", "Minimum dispensations", "Other columns"),
      Required = c("Yes", "Yes", "No", "No", "No"),
      Format = c("Text: disease or event name. Same name as in the disease – diagnosis table if both are to be combined.",
                 "Text: ATC code or prefix (J05AP includes every J05AP* drug), or an RxNorm identifier, which is compared whole.",
                 DRUG_TYPE_HELP,
                 "Whole number ≥ 1: distinct dispensation dates needed. Default 1. The event date is that of the dispensation reaching it. Can be changed in the app.",
                 "Allowed and kept for documentation; not used in the search."),
      Example = c("HCV_treatment", "J05AP", "ATC", "2", "Source")),
    example = data.frame(Disease = c("HCV_treatment", "Opioid_agonists"), Codigo = c("J05AP", "N07BC"),
                         Tipo = c("ATC", "ATC"), MinDispensations = c("2", "1")))
)

GENERAL_HELP <- function() shiny::tags$ul(
  shiny::tags$li("Files: csv or txt (separator ; , or tab; UTF-8 or Latin-1), Excel (xlsx, xls) or R (rds)."),
  shiny::tags$li("The first row must contain column names. Names are free: you tell the app which column is which (key fields) when you load each table."),
  shiny::tags$li("Codes are compared only within their own coding system: an ICD-9 code never matches an ICD-10 code."),
  shiny::tags$li("National versions matter: ICD-10-CM, CIE-10-ES, WHO ICD-10, ICD-10-GM… do not contain exactly the same codes. State the version in the code type and make sure definitions were written (or adapted) for the version of your data; the app warns when they differ."),
  shiny::tags$li("Data stay on this computer: the app runs locally."))

html_table <- function(df) shiny::tags$table(class = "table table-sm table-bordered small mb-2",
  shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
  shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i) shiny::tags$tr(lapply(df[i, ], function(v) shiny::tags$td(as.character(v)))))))

spec_ui <- function(id) {
  s <- table_specs[[id]]
  shiny::tagList(shiny::p(shiny::strong(s$row)), html_table(s$fields),
          shiny::p(class = "small text-muted mb-1", "Example (column names are free):"), html_table(s$example))
}
