# Event finder based on diagnoses and drug dispensations
# (simplified MorbfindR, without Charlson or Elixhauser)
#
# Rules:
#  - Every code carries its coding system as a prefix (ICPC / ICD9 / ICD10 / ATC), both in
#    the data and in the definition, so a code is only compared within its own system.
#  - Granularity: prefix match on the code without dots. No dot = whole category (E860
#    includes E860.0); with a dot = that level and its children (E86.0 includes E86.01,
#    not E86.1).
#  - Optional: ICD-10 rows without diagnosis format are dropped (7-character ICD-10-PCS
#    procedure codes, ICD-O morphology...).
#  - Per event, a rule combines diagnoses and drugs: dx, drug, dx_or_drug, dx_and_drug.
#    For drugs, min_dispensations sets how many distinct dispensation dates are needed;
#    the event date is the date of the dispensation that reaches it.
if (!exists("%||%", baseenv())) `%||%` <- function(a, b) if (is.null(a)) b else a

coding_system <- function(type) {
  t <- tolower(trimws(as.character(type)))
  fifelse(grepl("ciap|icpc", t), "ICPC",
  # RxNorm before ATC: a label such as "RxNorm (drug)" would otherwise match the ATC branch.
  fifelse(grepl("rxnorm|rx norm|rxcui", t), "RXNORM",
  fifelse(grepl("atc|farmac|drug", t), "ATC",
  fifelse(grepl("snomed", t), "SNOMED",
  fifelse(grepl("(cie|icd)[^0-9]*10|^10", t), "ICD10",
  fifelse(grepl("(cie|icd)[^0-9]*9|^9", t), "ICD9", NA_character_))))))
}

# Systems that carry drugs. The split between diagnoses and drugs is by this set, never by
# equality with "ATC": an RxNorm code is a drug, and treating it as a diagnosis would send it
# to the wrong side of every definition.
DRUG_SYSTEMS <- c("ATC", "RXNORM")

# National version of the coding system, read from the code-type label. Matching is done by
# system (ICD-9 / ICD-10 / ICPC / ATC); the version is used to warn when data and definitions
# were coded with different versions, whose code lists differ (e.g. B24 exists in WHO ICD-10
# and ICD-10-CA but not in ICD-10-CM / CIE-10-ES, which use B20).
coding_version <- function(type) {
  t <- tolower(trimws(as.character(type))); sys <- coding_system(type)
  fcase(
    is.na(sys), NA_character_,
    sys == "ATC", "ATC",
    sys == "RXNORM", "RxNorm",
    sys == "ICPC", "ICPC",
    # Without this branch SNOMED would fall through to the ICD-10 default below and be
    # labelled "ICD-10 (version not stated)", which also fires a spurious version warning.
    sys == "SNOMED", "SNOMED CT",
    sys == "ICD9", fifelse(grepl("cm|mc", t), "ICD-9-CM", "ICD-9 (version not stated)"),
    grepl("(^|[^a-z])es([^a-z]|$)|^10es", t), "ICD-10-ES (Spain)",
    grepl("(^|[^a-z])(cm|mc)([^a-z]|$)", t), "ICD-10-CM (US)",
    grepl("(^|[^a-z])gm([^a-z]|$)", t), "ICD-10-GM (Germany)",
    grepl("(^|[^a-z])ca([^a-z]|$)", t), "ICD-10-CA (Canada)",
    grepl("(^|[^a-z])am([^a-z]|$)", t), "ICD-10-AM (Australia)",
    grepl("cim|(^|[^a-z])fr([^a-z]|$)", t), "CIM-10 (France)",
    grepl("who|oms", t), "ICD-10 (WHO)",
    default = "ICD-10 (version not stated)")
}
# Versions whose code lists are close enough to be treated as the same family
version_family <- function(v) fcase(v %in% c("ICD-10-CM (US)", "ICD-10-ES (Spain)"), "ICD-10-CM family",
                                    v == "ICD-9-CM", "ICD-9-CM", default = v)

# Warnings when data and definition versions of the same system differ
version_warnings <- function(data_types, def_types) {
  dv <- unique(na.omit(coding_version(data_types))); fv <- unique(na.omit(coding_version(def_types)))
  out <- character(0)
  for (sys in c("ICD9", "ICD10")) {
    in_sys <- function(v) v[grepl(if (sys == "ICD9") "^ICD-9" else "ICD-10|CIM-10", v)]
    d <- in_sys(dv); f <- in_sys(fv)
    if (!length(d) || !length(f)) next
    lab_of <- function(types) { types <- unique(types); types[coding_system(types) == sys & grepl("not stated", coding_version(types))] }
    unclear <- c(if (length(lab_of(data_types))) paste0("'", lab_of(data_types), "' in the patient data"),
                 if (length(lab_of(def_types))) paste0("'", lab_of(def_types), "' in the definitions"))
    if (length(unclear))
      out <- c(out, paste0(if (sys == "ICD9") "ICD-9" else "ICD-10", ": the code type ", paste(unclear, collapse = " and "),
                           " does not state the version (e.g. ICD-9-CM, CIE-10-ES, ICD-10 (WHO)). ",
                           "Codes can differ between national versions: check that data and definitions use the same one."))
    else if (!setequal(unique(version_family(d)), unique(version_family(f))))
      out <- c(out, paste0("Data coded in ", paste(d, collapse = ", "), " but definitions written for ", paste(f, collapse = ", "),
                           ". Some codes may not exist or may mean something else (e.g. HIV is B20 in ICD-10-CM / ICD-10-ES but B20–B24 in WHO ICD-10; ",
                           "poisoning intent is a 6th character in ICD-10-CM / ICD-10-ES (T40.1X1) but an external-cause code in WHO ICD-10 (X42)). Review the definition."))
  }
  out
}

# Rows per coding system, from the code-type labels and (optionally) how many rows carry each
# label. Labels whose system is not recognised are left out: those rows are dropped and counted
# by prepare_diagnoses() instead.
rows_by_system <- function(types, counts = NULL) {
  s <- coding_system(types)
  if (is.null(counts)) counts <- rep(1L, length(types))
  keep <- !is.na(s)
  if (!any(keep)) return(stats::setNames(numeric(0), character(0)))
  tapply(as.numeric(counts)[keep], s[keep], sum)
}

# Warning when a coding system exists on one side only. The search partitions by system, so a
# system present in the data but with no code in the definitions can never match. This has to be
# said out loud: a label the engine does not recognise is dropped and counted, but one it does
# recognise is kept and then quietly ignored, and the zero would look like absence.
system_coverage_warnings <- function(rows, def_systems) {
  def_systems <- unique(def_systems[!is.na(def_systems)])
  out <- character(0)
  n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  orphan <- setdiff(names(rows), def_systems)
  if (length(orphan))
    out <- c(out, paste0("The patient data contain ",
      paste0(orphan, " (", n(rows[orphan]), " rows)", collapse = ", "),
      " and the definitions have no code in ", if (length(orphan) == 1) "that system" else "those systems",
      ". Those rows are kept but cannot match anything, so they are ignored in the search."))
  missing <- setdiff(def_systems, names(rows))
  if (length(missing))
    out <- c(out, paste0("The definitions use ", paste(missing, collapse = ", "),
      " and the patient data contain no row in ", if (length(missing) == 1) "that system" else "those systems",
      ". Those definition codes cannot be found.",
      if (any(DRUG_SYSTEMS %in% missing)) " If a definition needs drugs, load the patient - drug table in tab 1." else ""))
  out
}

normalise_code <- function(x) toupper(gsub(".", "", trimws(as.character(x)), fixed = TRUE))
is_icd10_dx_format <- function(code) grepl("^[A-Z][0-9][0-9A-Z](\\.[0-9A-Z]{1,4})?$", toupper(trimws(code)))

# Accepted date formats; the one that reads most of a sample is used for the whole column
DATE_FORMATS <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%d.%m.%Y", "%Y%m%d")
detect_date_format <- function(x) {
  x <- substr(trimws(as.character(x[!is.na(x) & x != ""])), 1, 10)
  x <- head(x, 1000)
  if (!length(x)) return(DATE_FORMATS[1])
  ok <- vapply(DATE_FORMATS, function(f) mean(!is.na(as.Date(x, format = f))), 0)
  DATE_FORMATS[which.max(ok)]
}
as_date <- function(x, format = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))   # Excel serial dates
  x <- trimws(as.character(x))
  if (is.null(format)) format <- detect_date_format(x)
  as.Date(substr(x, 1, if (format == "%Y%m%d") 8 else 10), format = format)
}

# ---- Preparation --------------------------------------------------------------
prepare_diagnoses <- function(d, col_id, col_code, col_type, col_date, drop_procedures = TRUE) {
  d <- as.data.table(d)[, .(id = as.character(get(col_id)), code = as.character(get(col_code)),
                            type = get(col_type), date = as_date(get(col_date)))]
  n0 <- nrow(d)
  d[, system := coding_system(type)]
  no_system <- d[is.na(system) | system %in% DRUG_SYSTEMS, .N]
  d <- d[!is.na(system) & !system %in% DRUG_SYSTEMS]
  procedures <- 0L
  if (drop_procedures) {
    out <- d$system == "ICD10" & !is_icd10_dx_format(d$code)
    procedures <- sum(out); d <- d[!out]
  }
  no_date <- d[is.na(date) | is.na(id), .N]
  d <- d[!is.na(date) & !is.na(id)]
  d[, cod := normalise_code(code)]
  list(data = d[, .(id, system, cod, date)],
       summary = data.table(step = c("Rows read", "Coding system not recognised",
                                     "ICD-10 without diagnosis format (procedures)", "Missing date or id", "Rows used"),
                            rows = c(n0, no_system, procedures, no_date, nrow(d))))
}

# col_type is optional: a table with no code-type column is read as ATC, and the summary says so
# instead of the assumption being silent. With the column, each row carries its own system, so
# ATC and RxNorm can coexist and are never compared with each other.
prepare_drugs <- function(d, col_id, col_code, col_date, col_type = NULL) {
  d <- as.data.table(d)
  has_type <- !is.null(col_type) && length(col_type) == 1L && !is.na(col_type) &&
    nzchar(col_type) && col_type %in% names(d)
  type <- if (has_type) as.character(d[[col_type]]) else rep("ATC", nrow(d))
  d <- d[, .(id = as.character(get(col_id)), cod = normalise_code(get(col_code)), date = as_date(get(col_date)))]
  n0 <- nrow(d)
  d[, system := coding_system(type)]
  no_system <- d[is.na(system) | !system %in% DRUG_SYSTEMS, .N]
  d <- d[!is.na(system) & system %in% DRUG_SYSTEMS]
  no_date <- d[is.na(date) | is.na(id) | cod == "", .N]
  d <- d[!is.na(date) & !is.na(id) & cod != ""]
  list(data = d[, .(id, system, cod, date)],
       summary = data.table(
         step = c("Rows read", if (!has_type) "No code-type column: read as ATC" else NULL,
                  "Not a drug coding system", "Missing date or id", "Rows used"),
         rows = c(n0, if (!has_type) n0 else NULL, no_system, no_date, nrow(d))))
}

RULES <- c("dx", "drug", "dx_or_drug", "dx_and_drug")
RULE_ALIASES <- c(farmaco = "drug", dx_o_farmaco = "dx_or_drug", dx_y_farmaco = "dx_and_drug")

# What a code row is for. Most codes establish the event and date it; some can only date it.
#   case  (default) the code says the person has the event, and its date can be the first one.
#   date            the code is not specific enough to say anyone has the event, but in someone
#                   who has it on other evidence it can be the earliest record of it. An
#                   unspecified code ("viral hepatitis", "drug abuse") is the usual case: it
#                   cannot diagnose hepatitis C, and yet for a confirmed patient it is often
#                   where the condition first appears in the record. A person with nothing but
#                   "date" codes never gets the event.
# Dating with a supporting code would contradict dx_and_drug, whose date is when BOTH conditions
# are met, so there it is not applied.
USES <- c("case", "date")
USE_ALIASES <- c(caso = "case", diagnostico = "case", establish = "case", definitoria = "case",
                 fecha = "date", dating = "date", supporting = "date", apoyo = "date",
                 solo_fecha = "date", date_only = "date")

# What an event is, declared in the definition table and not guessed from the slot: the slot
# cannot tell a disease from a comorbidity, since both are defined by diagnosis codes.
#   cohort       decides membership of the population under study: who uses drugs and who does
#                not. May be defined by diagnoses and/or by drugs used as a proxy (methadone).
#   disease      a disease diagnosis. May also be defined by diagnoses AND drugs, which is what
#                links the two definition tables: diagnosed or treated.
#   comorbidity  a comorbidity or a determinant: context, neither the population nor the disease.
#   treatment    a treatment or a vaccine. Drugs only, because a diagnosis never makes anyone
#                treated.
#   marker       something observed about the person that says who the analysis is about, not
#                what happened to them: that they injected, that they were in prison. It never
#                starts or ends anyone's follow-up and is never an outcome; it restricts the
#                cohort, and the restriction is chosen when the results are read, not here.
#   mortality    the cause of death, from its own definition table.
ROLES <- c("birth", "cohort", "disease", "comorbidity", "treatment", "marker", "mortality")
ROLE_ALIASES <- c(cohorte = "cohort", poblacion = "cohort", population = "cohort",
                  enfermedad = "disease", diagnostico = "disease", dx = "disease",
                  comorbilidad = "comorbidity", condicionante = "comorbidity", determinant = "comorbidity",
                  tratamiento = "treatment", vacuna = "treatment", vaccine = "treatment",
                  marcador = "marker", filtro = "marker", filter = "marker",
                  mortalidad = "mortality", death = "mortality")

# Definition table: the user maps its columns. event, type and code are required;
# rule, role, min_dispensations, use, valid_from, valid_to and source are optional
# (NULL or "" = not provided). use and the validity window are properties of the single code
# row, not of the event: within one definition some codes diagnose and others only date, and a
# code can belong to one disease up to a date and to another one after it.
prepare_definition <- function(def, col_event, col_type, col_code, col_rule = NULL, col_min = NULL,
                               col_role = NULL, col_use = NULL, col_from = NULL, col_to = NULL) {
  def <- as.data.table(def)
  opt <- function(col) if (!is.null(col) && nzchar(col) && col %in% names(def)) def[[col]] else NA
  d <- data.table(event = trimws(as.character(def[[col_event]])), type = def[[col_type]],
                  code = as.character(def[[col_code]]),
                  rule = trimws(tolower(as.character(opt(col_rule)))),
                  role = trimws(tolower(as.character(opt(col_role)))),
                  use = trimws(tolower(as.character(opt(col_use)))),
                  from = as_date(opt(col_from)), to = as_date(opt(col_to)),
                  min_dispensations = suppressWarnings(as.numeric(opt(col_min))))
  d[rule %in% names(RULE_ALIASES), rule := RULE_ALIASES[rule]]
  d[role %in% names(ROLE_ALIASES), role := ROLE_ALIASES[role]]
  d[use %in% names(USE_ALIASES), use := USE_ALIASES[use]]
  d[is.na(use) | use == "", use := "case"]
  d[, `:=`(system = coding_system(type), cod = normalise_code(code))]

  warnings <- character(0)
  no_sys <- d[is.na(system)]
  if (nrow(no_sys)) warnings <- c(warnings, paste0("Code type not recognised (row ignored): ",
                                                  paste(unique(paste(no_sys$type, no_sys$code)), collapse = "; ")))
  odd <- d[!is.na(system) & (lengths(regmatches(code, gregexpr(".", code, fixed = TRUE))) > 1 |
                              grepl("[^0-9A-Za-z.]", trimws(code)))]
  if (nrow(odd)) warnings <- c(warnings, paste0("Codes with an unusual format (check, e.g. abbreviated notation): ",
                                               paste(unique(odd$code), collapse = "; ")))
  empty_event <- d[is.na(event) | event == "", .N]
  if (empty_event) warnings <- c(warnings, paste0(empty_event, " rows without event name (ignored)"))
  bad_use <- d[!use %in% USES, unique(use)]
  if (length(bad_use))
    stop("Unknown use for a code: ", paste(bad_use, collapse = ", "), ". Use: ", paste(USES, collapse = ", "), ".")
  bad_win <- d[!is.na(from) & !is.na(to) & from > to]
  if (nrow(bad_win))
    stop("These codes are valid from a date later than the date they are valid to: ",
         paste(unique(paste(bad_win$event, bad_win$code)), collapse = "; "), ".")
  d <- d[!is.na(system) & cod != "" & !is.na(event) & event != ""]

  # An event with nothing but dating codes can never happen to anyone, which is a mistake in the
  # table and not a result: it is named rather than silently producing an empty event.
  only_dating <- d[, .(all_dating = all(use == "date")), by = event][all_dating == TRUE, event]
  if (length(only_dating))
    stop("These events are defined only by codes that can date but not diagnose: ",
         paste(only_dating, collapse = ", "), ". At least one code must have use 'case'.")

  rules <- d[, .(rule = na.omit(rule[rule != ""])[1], min_dispensations = na.omit(min_dispensations)[1],
                 roles = paste(sort(unique(role[!is.na(role) & role != ""])), collapse = " / "),
                 has_dx = any(!system %in% DRUG_SYSTEMS & use == "case"),
                 has_drug = any(system %in% DRUG_SYSTEMS & use == "case")), by = event]
  rules[is.na(rule), rule := fifelse(has_dx & has_drug, "dx_or_drug", fifelse(has_drug, "drug", "dx"))]
  bad <- rules[!rule %in% RULES]
  if (nrow(bad)) stop("Invalid rule for: ", paste(bad$event, collapse = ", "), ". Use: ", paste(RULES, collapse = ", "))
  rules[is.na(min_dispensations), min_dispensations := 1]

  # One role per event. Two different roles for the same name is a contradiction, not a choice:
  # the same clinical entity cannot be the disease under study and its own treatment.
  split_role <- rules[grepl(" / ", roles)]
  if (nrow(split_role))
    stop("These events are given more than one role: ",
         paste0(split_role$event, " (", split_role$roles, ")", collapse = "; "),
         ". Each event must have a single role.")
  unknown <- rules[roles != "" & !roles %in% ROLES]
  if (nrow(unknown))
    stop("Unknown role for: ", paste0(unknown$event, " (", unknown$roles, ")", collapse = "; "),
         ". Use: ", paste(ROLES, collapse = ", "), ".")
  setnames(rules, "roles", "role")
  # Only worth saying when the column exists and some rows are empty: with no role column at all
  # the derivation is the documented default and warning on every run would be noise.
  has_role_col <- !is.null(col_role) && length(col_role) == 1L && !is.na(col_role) &&
    nzchar(col_role) && col_role %in% names(def)
  derived <- rules[role == ""]
  if (nrow(derived) && has_role_col)
    warnings <- c(warnings, paste0("No role given for ", nrow(derived), " event(s), so it was derived from",
      " the definition table (drugs only = treatment, otherwise disease): ",
      paste(derived$event, collapse = ", "), ". Declaring it makes the process map reliable."))
  rules[role == "", role := fifelse(has_drug & !has_dx, "treatment", "disease")]
  # A treatment is defined by the drug alone: a diagnosis never makes anyone treated.
  bad_tr <- rules[role == "treatment" & has_dx]
  if (nrow(bad_tr))
    stop("These events have the role 'treatment' and are defined by diagnosis codes: ",
         paste(bad_tr$event, collapse = ", "),
         ". A treatment must be defined by drugs only; a diagnosis never makes anyone treated.")
  # A cause of death is a diagnosis code: the mortality role belongs to the cause-of-death
  # definitions, and its date always comes from the patient table, never from a dispensation.
  bad_mort <- rules[role == "mortality" & has_drug]
  if (nrow(bad_mort))
    stop("These events have the role 'mortality' and are defined by drug codes: ",
         paste(bad_mort$event, collapse = ", "),
         ". A cause of death is a diagnosis code; mortality dates come from the patient table.")
  list(codes = unique(d[, .(event, system, cod, use, from, to)]), rules = rules, warnings = warnings)
}

# Systems whose codes are flat identifiers, with no hierarchy inside the string. In ICD, CIAP
# and ATC, truncating a code is how a definition says "the whole category", so the prefix is
# part of its meaning. A SNOMED identifier is an enumeration: a shorter one is not a parent, so
# it is compared by equality and expanding it is the job of whoever builds the list.
# The matching mode is derived from the system, never declared in the definition table.
# RxNorm is here for the same reason as SNOMED: an RxCUI is an arbitrary identifier, so a shorter
# one is not a parent. ATC stays on prefix matching, where N07BC does contain N07BC02.
EXACT_SYSTEMS <- c("SNOMED", "RXNORM")

# ---- Prefix matching within each coding system --------------------------------
# For each code length in the definition the prefix is computed once and joined by
# equality (hash join), instead of comparing code by code.
# A code row may be valid only within a window: the same code can mean one disease before a
# date and another one after it. An open end is simply no limit on that side.
in_window <- function(date, from, to) (is.na(from) | date >= from) & (is.na(to) | date <= to)

match_codes <- function(data, codes, progress = NULL) {
  res <- list(); unmatched <- list()
  # A code list may arrive without the optional columns (another caller, an older table): every
  # code then establishes its event and is valid at any date, which is what it meant before.
  codes <- as.data.table(codes)
  if (!"use"  %in% names(codes)) codes <- copy(codes)[, use := "case"]
  if (!"from" %in% names(codes)) codes[, from := as.Date(NA)]
  if (!"to"   %in% names(codes)) codes[, to := as.Date(NA)]
  # total work = rows scanned for each system × code length combination; an exact system is
  # a single pass, so it contributes one row and not one per code length.
  exact <- intersect(unique(codes$system), EXACT_SYSTEMS)
  work <- rbind(codes[!system %in% exact, .(L = unique(nchar(cod))), by = system],
                if (length(exact)) data.table(system = exact, L = NA_integer_))
  work[, rows := data[, .N, by = system][work, on = "system", N]]
  work[is.na(rows), rows := 0L]
  total <- max(sum(work$rows), 1); done <- 0
  for (s in unique(codes$system)) {
    ds <- data[system == s]; cs <- codes[system == s]
    if (!nrow(ds)) next
    if (s %in% EXACT_SYSTEMS) {
      # Flat identifiers: equality of the whole code. A shorter code is not a parent, so a
      # truncated or mistyped one matches nothing, which is why it is reported.
      res[[s]] <- merge(ds[, .(id, date, cod)], cs[, .(cod, event, use, from, to)], by = "cod",
                        allow.cartesian = TRUE)[in_window(date, from, to), .(id, event, date, use)]
      unmatched[[s]] <- cs[!cod %in% ds$cod, .(system, event, cod)]
      done <- done + nrow(ds)
      if (is.function(progress)) progress(done / total, paste0("Matching ", s, " codes"))
      next
    }
    for (L in unique(nchar(cs$cod))) {
      ds[, pref := substr(cod, 1, L)]
      m <- merge(ds[nchar(cod) >= L, .(id, date, pref)],
                 cs[nchar(cod) == L, .(pref = cod, event, use, from, to)],
                 by = "pref", allow.cartesian = TRUE)
      res[[paste(s, L)]] <- m[in_window(date, from, to), .(id, event, date, use)]
      done <- done + nrow(ds)
      if (is.function(progress)) progress(done / total, paste0("Matching ", s, " codes of length ", L))
    }
  }
  out <- if (!length(res)) data.table(id = character(), event = character(),
                                      date = as.Date(character()), use = character()) else rbindlist(res)
  setattr(out, "unmatched", if (length(unmatched)) rbindlist(unmatched) else NULL)
  out
}

# Definition codes that matched nothing, in the systems compared by equality. Only for those:
# in ICD or ATC a truncated code still matches, and more broadly, so the same report would be
# noise. Here it is the only way to see a typo.
unmatched_warnings <- function(x) {
  # Accepts the result of match_codes() (reads its attribute) or the unmatched table itself.
  u <- if (is.data.frame(x) && all(c("system", "cod") %in% names(x))) x else attr(x, "unmatched")
  if (is.null(u) || !nrow(u)) return(character(0))
  cods <- unique(u$cod)
  paste0(length(cods), " definition code(s) in ", paste(unique(u$system), collapse = ", "),
         " matched no row. These systems are compared by equality of the whole code, not by prefix,",
         " so a truncated or mistyped identifier finds nothing: ",
         paste(utils::head(cods, 20), collapse = ", "), if (length(cods) > 20) ", …" else "", ".")
}

# progress: optional function (fraction 0-1, text) to report advance
find_events <- function(dx = NULL, drugs = NULL, definition, include_without_event = FALSE, progress = NULL) {
  codes <- definition$codes; rules <- definition$rules
  segment <- function(from, to) if (is.function(progress)) function(f, txt) progress(from + f * (to - from), txt) else NULL
  use_drugs <- !is.null(drugs) && any(codes$system %in% DRUG_SYSTEMS)
  dx_end <- if (use_drugs) 0.75 else 0.95
  m_dx <- if (!is.null(dx)) match_codes(dx, codes[!system %in% DRUG_SYSTEMS], segment(0, dx_end)) else NULL
  # Who has the event comes from the codes that can establish it; the dating codes are kept
  # apart and only brought back below, for the people who already have it.
  # The nrow() guards are not decoration: data.table evaluates j once on an empty table to
  # learn its types, and min() of nothing warns and returns Inf. The row is discarded, so the
  # result is right and the warning is noise -- but a warning nobody can act on is how a real
  # one gets missed.
  m_case <- if (!is.null(m_dx)) m_dx[use == "case"] else NULL
  f_dx <- if (!is.null(m_case) && nrow(m_case)) m_case[, .(f_dx = min(date)), by = .(id, event)] else NULL
  f_date <- if (!is.null(m_dx) && nrow(m_dx)) m_dx[, .(f_any = min(date)), by = .(id, event)] else NULL
  f_drug <- NULL
  if (use_drugs) {
    h <- unique(match_codes(drugs, codes[system %in% DRUG_SYSTEMS], segment(dx_end, 0.95)))
    h <- merge(h, rules[, .(event, min_dispensations)], by = "event")
    setorder(h, id, event, date)
    f_drug <- h[, .(f_drug = if (.N >= min_dispensations[1]) date[min_dispensations[1]] else as.Date(NA)), by = .(id, event)]
  }
  base <- unique(rbindlist(list(if (!is.null(f_dx)) f_dx[, .(id, event)], if (!is.null(f_drug)) f_drug[, .(id, event)])))
  if (!nrow(base)) base <- data.table(id = character(), event = character())
  base <- merge(base, rules[, .(event, rule)], by = "event")
  base <- if (!is.null(f_dx)) merge(base, f_dx, by = c("id", "event"), all.x = TRUE) else base[, f_dx := as.Date(NA)]
  base <- if (!is.null(f_drug)) merge(base, f_drug, by = c("id", "event"), all.x = TRUE) else base[, f_drug := as.Date(NA)]
  base <- if (!is.null(f_date)) merge(base, f_date, by = c("id", "event"), all.x = TRUE) else base[, f_any := as.Date(NA)]
  base[, date := fcase(rule == "dx", f_dx,
                       rule == "drug", f_drug,
                       rule == "dx_or_drug", pmin(f_dx, f_drug, na.rm = TRUE),
                       rule == "dx_and_drug", fifelse(!is.na(f_dx) & !is.na(f_drug), pmax(f_dx, f_drug), as.Date(NA)))]
  # The dating codes can only move the first date earlier, and never for dx_and_drug, whose date
  # is the moment both conditions hold.
  base[rule != "dx_and_drug" & !is.na(date) & !is.na(f_any), date := pmin(date, f_any)]
  long <- base[!is.na(date), .(id, event, date)]
  if (is.function(progress)) progress(0.97, "Building the results table")

  wide <- dcast(long, id ~ event, value.var = "date")
  for (e in setdiff(rules$event, names(wide))) wide[, (e) := as.Date(NA)]
  if (include_without_event) {
    everyone <- unique(c(if (!is.null(dx)) dx$id, if (!is.null(drugs)) drugs$id))
    wide <- merge(data.table(id = everyone), wide, by = "id", all.x = TRUE)
  }
  setcolorder(wide, c("id", rules$event))
  list(long = long, wide = wide,
       summary = merge(rules[, .(event, rule)], long[, .(persons = uniqueN(id)), by = event], by = "event", all.x = TRUE)[
         is.na(persons), persons := 0L][],
       unmatched = attr(m_dx, "unmatched"))
}
