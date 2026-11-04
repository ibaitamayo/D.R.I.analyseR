# Engine tests with synthetic cases (Rscript tests/test_engine.R)
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date
dx <- data.table(
  patient = c("a","b","c","d","e","f","g","g","h"),
  dx_code = c("E86.0","E860.0","F11.20","F13ZLZZ","P19","B18.2","F11","F11.9","Z3A.36"),
  system  = c("10ES","9","10ES","10ES","CIAP2","10ES","10ES","10ES","10ES"),
  when    = D(c("2020-01-01","2020-01-01","2019-05-01","2020-01-01","2018-01-01","2017-03-03","2016-01-01","2015-06-01","2021-01-01")))
drugs <- data.table(patient = c("f","f","f","i","i","j","k"),
                    atc = c("J05AP55","J05AP55","J05AP55","N07BC02","N07BC02","N07BC01","J05AP55"),
                    when = D(c("2018-01-10","2018-02-10","2018-03-10","2019-01-01","2019-02-01","2019-01-01","2016-01-01")))
# column names deliberately different from the engine's internal names
def <- data.table(
  Disease = c("OD","OD","DrugUse","DrugUse","Sedatives","HCV","HCV","OAT","HCV_treated","HCV_treated"),
  System  = c("ICD-9-CM","ICD-10-CM","ICD-10-CM","CIAP2","ICD-10-CM","ICD-10-CM","ATC","ATC","ICD-10-CM","ATC"),
  Code    = c("E860","X42","F11","P19","F13","B18.2","J05AP","N07BC","B18.2","J05AP"),
  How     = c(NA,NA,NA,NA,NA,"dx_or_drug","dx_or_drug",NA,"dx_and_drug","dx_and_drug"),
  MinDisp = c(NA,NA,NA,NA,NA,NA,NA,2,NA,3))
pdx <- prepare_diagnoses(dx, "patient", "dx_code", "system", "when", drop_procedures = TRUE)
pdr <- prepare_drugs(drugs, "patient", "atc", "when")
pd  <- prepare_definition(def, "Disease", "System", "Code", "How", "MinDisp")
r   <- find_events(pdx$data, pdr$data, pd)
has <- function(i, e) nrow(r$long[id == i & event == e]) > 0
dt  <- function(i, e) r$long[id == i & event == e, date]

ok(!has("a","OD"),                        "E86.0 (ICD-10) does not trigger E860 (ICD-9)")
ok(has("b","OD"),                         "E860.0 (ICD-9) triggers E860: granularity")
ok(has("c","DrugUse"),                    "F11.20 triggers F11")
ok(!has("d","Sedatives"),                 "F13ZLZZ (procedure) dropped")
ok(has("e","DrugUse"),                    "CIAP2 P19 triggers ICPC P19")
ok(pdx$summary[step == "Rows used", rows] == 8, "Z3A.36 kept as valid diagnosis; only F13ZLZZ dropped")
ok(dt("g","DrugUse") == D("2015-06-01"),  "Event date = earliest date")
ok(dt("f","HCV") == D("2017-03-03"),      "dx_or_drug: earliest of dx and drug")
ok(dt("k","HCV") == D("2016-01-01"),      "dx_or_drug: drug alone is enough")
ok(dt("f","HCV_treated") == D("2018-03-10"), "dx_and_drug with 3 dispensations: date of the 3rd")
ok(!has("k","HCV_treated"),               "dx_and_drug: no dx, no event")
ok(dt("i","OAT") == D("2019-02-01"),      "min_dispensations = 2: date of the 2nd")
ok(!has("j","OAT"),                       "min_dispensations = 2 with only one: no event")
ok(pd$rules[event == "OAT", rule] == "drug", "Default rule: ATC only = drug")
def2 <- rbind(def[, .(Disease, System, Code)], data.table(Disease = "OD", System = "ICD-9-CM", Code = "E850.0.8.9"))
ok(length(prepare_definition(def2, "Disease", "System", "Code")$warnings) == 1, "Warning for abbreviated notation (E850.0.8.9)")
def3 <- copy(def)[How == "dx_and_drug", How := "dx_y_farmaco"]
ok(prepare_definition(def3, "Disease", "System", "Code", "How", "MinDisp")$rules[event == "HCV_treated", rule] == "dx_and_drug",
   "Spanish rule names accepted (dx_y_farmaco)")
ok(identical(as_date(c("31/12/2020", "01/02/2019")), D(c("2020-12-31", "2019-02-01"))), "Dates dd/mm/yyyy detected")
ok(identical(as_date(c("2020-12-31 10:15:00", "2019-02-01")), D(c("2020-12-31", "2019-02-01"))), "ISO dates with time")
ok(identical(as_date(c("20201231", "20190201")), D(c("2020-12-31", "2019-02-01"))), "Dates yyyymmdd")
ok(detect_date_format(c("12/01/2020", "31/01/2020")) == "%d/%m/%Y", "Day-first chosen when a day > 12 appears")
v <- coding_version(c("10ES", "CIE-10-ES", "ICD-10-CM", "CIE-10-MC", "ICD-10", "ICD-10 (WHO)", "ICD-10-GM", "ICD-10-CA", "9", "ICD-9-CM", "CIAP2", "ATC"))
ok(identical(v, c("ICD-10-ES (Spain)", "ICD-10-ES (Spain)", "ICD-10-CM (US)", "ICD-10-CM (US)", "ICD-10 (version not stated)", "ICD-10 (WHO)",
                  "ICD-10-GM (Germany)", "ICD-10-CA (Canada)", "ICD-9 (version not stated)", "ICD-9-CM", "ICPC", "ATC")), "National versions detected from labels")
ok(length(version_warnings(c("10ES", "CIE-9-MC"), c("CIE-10-ES", "ICD-9-CM"))) == 0, "Same versions: no warning")
ok(length(version_warnings(c("10ES"), c("ICD-10-CM"))) == 0, "ICD-10-ES and ICD-10-CM treated as same family")
ok(length(version_warnings(c("10ES"), c("ICD-10 (WHO)"))) == 1, "ICD-10-ES data vs WHO definitions: warning")
ok(length(version_warnings(c("ICD-10-GM"), c("ICD-10"))) == 1, "Version not stated: warning")

# ---- SNOMED as a fifth coding system (B1) --------------------------------------------
ok(coding_system("SNOMED") == "SNOMED", "SNOMED recognised")
ok(coding_system("snomed ct") == "SNOMED", "snomed ct recognised")
ok(identical(coding_system(c("CIE-10-ES", "ICD9CM", "CIAP2", "ATC")), c("ICD10", "ICD9", "ICPC", "ATC")),
   "the other four systems unchanged")
# B1b: without its own branch SNOMED fell through to the ICD-10 default
ok(coding_version("SNOMED") == "SNOMED CT", "SNOMED version label")
ok(!grepl("ICD-10", coding_version("SNOMED CT")), "SNOMED is not labelled ICD-10")
ok(length(version_warnings(c("CIE-10-ES", "SNOMED"), c("CIE-10-ES"))) == 0,
   "SNOMED data do not trigger an ICD-10 version warning")

# ---- Coverage of coding systems (C1) -------------------------------------------------
rows <- rows_by_system(c("CIE-10-ES", "SNOMED", "no-such-system"), c(100, 7, 55))
ok(identical(sort(names(rows)), c("ICD10", "SNOMED")) && rows[["SNOMED"]] == 7,
   "rows_by_system counts per system and ignores unrecognised labels")
w <- system_coverage_warnings(rows, c("ICD10"))
ok(length(w) == 1 && grepl("SNOMED (7 rows)", w, fixed = TRUE) && grepl("ignored", w),
   "data in a system the definitions do not have: warning with the row count")
w <- system_coverage_warnings(rows_by_system(c("CIE-10-ES"), 100), c("ICD10", "SNOMED"))
ok(length(w) == 1 && grepl("SNOMED", w) && grepl("cannot be found", w),
   "definitions in a system the data do not have: warning")
w <- system_coverage_warnings(rows_by_system(c("CIE-10-ES"), 100), c("ICD10", "ATC"))
ok(length(w) == 1 && grepl("patient - drug table", w, fixed = TRUE),
   "ATC codes with no drug table: the warning says where to load it")
ok(length(system_coverage_warnings(rows_by_system(c("CIE-10-ES", "SNOMED"), c(100, 7)),
                                   c("ICD10", "SNOMED"))) == 0,
   "both systems on both sides: no warning")

# ---- Exact matching for flat identifiers (B2) ----------------------------------------
sn <- data.table(id = c("a","b"), system = "SNOMED", cod = c("226034001","2260340012"), date = D("2020-01-01"))
m <- match_codes(sn, data.table(system = "SNOMED", cod = "226034001", event = "IDU"))
ok(nrow(m) == 1 && m$id == "a", "SNOMED matches by equality: a longer identifier is not a child")
ic <- data.table(id = "c", system = "ICD10", cod = "K720", date = D("2020-01-01"))
ok(nrow(match_codes(ic, data.table(system = "ICD10", cod = "K72", event = "Hepatic"))) == 1,
   "ICD-10 still matches by prefix")
# C2: a truncated identifier finds nothing, and it is reported
m2 <- match_codes(sn, data.table(system = "SNOMED", cod = "22603400", event = "IDU"))
ok(nrow(m2) == 0, "a truncated SNOMED identifier matches nothing")
ok(length(unmatched_warnings(m2)) == 1 && grepl("22603400", unmatched_warnings(m2), fixed = TRUE),
   "and the unmatched definition code is named")
ok(length(unmatched_warnings(m)) == 0, "nothing reported when every exact code matched")
ok(length(unmatched_warnings(match_codes(ic, data.table(system = "ICD10", cod = "Z99", event = "X")))) == 0,
   "unmatched prefix codes are not reported: a truncated ICD code still matches, and more broadly")
# the same person in two systems is one event, with the earliest date
mix <- data.table(pac = c("x","x"), cod = c("B18.2","226034001"), tipo = c("CIE-10-ES","SNOMED"),
                  fec = D(c("2018-01-01","2015-01-01")))
dmix <- prepare_definition(data.table(evento = "HCV", tipo = c("CIE-10-ES","SNOMED"),
                                      cod = c("B18.2","226034001")), "evento", "tipo", "cod")
ev <- find_events(dx = prepare_diagnoses(mix, "pac","cod","tipo","fec")$data, definition = dmix)$long
ok(nrow(ev) == 1 && ev$date == D("2015-01-01"),
   "one person coded in two systems is one event with the earliest date")

# ---- Drugs: ATC or RxNorm, never compared with each other ----------------------------
ok(coding_system("RxNorm") == "RXNORM", "RxNorm recognised")
ok(coding_system("RxNorm (drug)") == "RXNORM", "RxNorm wins over the 'drug' pattern of the ATC branch")
ok(coding_system("ATC") == "ATC", "ATC unchanged")
ok(coding_version("RxNorm") == "RxNorm", "RxNorm version label")
ok(all(c("ATC", "RXNORM") %in% DRUG_SYSTEMS), "both drug systems in DRUG_SYSTEMS")
ok("RXNORM" %in% EXACT_SYSTEMS && !"ATC" %in% EXACT_SYSTEMS,
   "RxNorm matches by equality; ATC keeps prefix matching")
# an RxCUI is flat: a shorter identifier is not a parent
rx <- data.table(id = c("a","b"), system = "RXNORM", cod = c("1659131","16591310"), date = D("2020-01-01"))
ok(nrow(match_codes(rx, data.table(system = "RXNORM", cod = "1659131", event = "Tto"))) == 1,
   "RxNorm matches by equality")
atc <- data.table(id = "c", system = "ATC", cod = "N07BC02", date = D("2020-01-01"))
ok(nrow(match_codes(atc, data.table(system = "ATC", cod = "N07BC", event = "OAT"))) == 1,
   "ATC still matches by prefix (N07BC contains N07BC02)")
# a drug table without a code-type column is read as ATC, and the summary says so
far <- data.table(pac = c("p1","p2"), cod = c("N07BC02","1659131"),
                  tipo = c("ATC","RxNorm"), fec = D("2020-01-01"))
pd0 <- prepare_drugs(far, "pac", "cod", "fec")
ok(all(pd0$data$system == "ATC") && any(grepl("read as ATC", pd0$summary$step)),
   "no code-type column: read as ATC and declared in the summary")
pd1 <- prepare_drugs(far, "pac", "cod", "fec", "tipo")
ok(identical(sort(pd1$data$system), c("ATC", "RXNORM")), "with the column each row carries its own system")
# a definition in RxNorm must be treated as a drug, not as a diagnosis
dd <- prepare_definition(data.table(e = "Tto", t = "RxNorm", c = "1659131", r = NA, m = 1),
                         "e", "t", "c", "r", "m")
ok(dd$rules$has_drug && !dd$rules$has_dx && dd$rules$rule == "drug",
   "an RxNorm definition is a drug definition")
ev2 <- find_events(drugs = pd1$data, definition = dd)$long
ok(nrow(ev2) == 1 && ev2$id == "p2", "find_events sends RxNorm to the drug side")

# ---- Declared role of each event -----------------------------------------------------
mk <- function(ev, ty, co, ro = NA) prepare_definition(
  data.table(e = ev, t = ty, c = co, r = ro), "e", "t", "c", col_role = "r")
r1 <- mk(c("HCV", "Homelessness"), "CIE-10-ES", c("B18.2", "Z59.0"), c("disease", "comorbidity"))
ok(identical(r1$rules[order(event), role], c("disease", "comorbidity")), "roles read from the table")
ok(mk("Tto", "ATC", "J05AP", "tratamiento")$rules$role == "treatment", "Spanish role names accepted")
ok(mk("HCV_death", "CIE-10-ES", "B18.2", "mortality")$rules$role == "mortality",
   "the cause-of-death definitions may declare mortality")
ok(mk("Drug_use", "CIE-10-ES", "F11", "cohort")$rules$role == "cohort", "cohort is a role")
ok(mk("Drug_use", "ATC", "N07BC", "cohorte")$rules$role == "cohort",
   "a drug used as a proxy can define the cohort, with its Spanish alias")
# derived when not declared, and said so
d0 <- prepare_definition(data.table(e = c("HCV", "Tto"), t = c("CIE-10-ES", "ATC"),
  c = c("B18.2", "J05AP")), "e", "t", "c")      # sin columna de rol
ok(identical(d0$rules[order(event), role], c("disease", "treatment")), "role derived when absent")
ok(!length(d0$warnings), "no role column at all: derived in silence, it is the documented default")
d1 <- prepare_definition(data.table(e = c("HCV", "Tto"), t = c("CIE-10-ES", "ATC"),
  c = c("B18.2", "J05AP"), r = c("disease", NA)), "e", "t", "c", col_role = "r")
ok(any(grepl("No role given", d1$warnings)) && d1$rules[event == "Tto", role] == "treatment",
   "a half-filled role column is derived and said so")
# contradictions are refused
ok(inherits(try(prepare_definition(data.table(e = c("HCV", "HCV"), t = c("CIE-10-ES", "ATC"),
  c = c("B18.2", "J05AP"), r = c("disease", "treatment")), "e", "t", "c", col_role = "r"), silent = TRUE), "try-error"),
  "the same event with two roles is refused")
ok(inherits(try(mk("Tto", "CIE-10-ES", "B18.2", "treatment"), silent = TRUE), "try-error"),
   "a treatment defined by a diagnosis is refused: a diagnosis never makes anyone treated")
ok(inherits(try(mk("Death_x", "ATC", "J05AP", "mortality"), silent = TRUE), "try-error"),
   "mortality defined by a drug is refused")
ok(inherits(try(mk("HCV", "CIE-10-ES", "B18.2", "whatever"), silent = TRUE), "try-error"),
   "an unknown role is refused")
# a disease may be defined by diagnoses AND drugs: that is what links the two tables
link <- prepare_definition(data.table(e = c("HCV", "HCV"), t = c("CIE-10-ES", "ATC"),
  c = c("B18.2", "J05AP"), r = c("disease", "disease")), "e", "t", "c", col_role = "r")
ok(link$rules$role == "disease" && link$rules$has_dx && link$rules$has_drug && link$rules$rule == "dx_or_drug",
   "a disease can be diagnosed or treated, which links the two definition tables")

# ---- a code that dates but does not diagnose, and codes with a validity window ----------
D <- as.Date
dd <- data.frame(
  ev   = c("Hep_C",      "Hep_C",      "Hep_B"),
  ty   = c("CIE-10-ES",  "CIAP-2",     "CIAP-2"),
  co   = c("B18.2",      "D72",        "D72"),
  use  = c("case",       "date",       "case"),
  from = c(NA,           "1990-01-01", NA),
  to   = c(NA,           NA,           "1989-12-31"))
pd <- prepare_definition(dd, "ev", "ty", "co", col_use = "use", col_from = "from", col_to = "to")
ok(nrow(pd$codes) == 3 && setequal(pd$codes$use, c("case", "date")), "use is read per code row, not per event")

pdx <- data.table(
  id   = c("a","a", "b",   "c",   "d"),
  system = c("ICD10","ICPC", "ICPC", "ICPC", "ICPC"),
  cod  = c("B182","D72",  "D72",  "D72",  "D72"),
  date = D(c("2005-01-01","1998-03-02", "2001-06-01", "1985-04-04", "1985-04-04")))
ev <- find_events(dx = pdx, definition = pd)
long <- as.data.table(ev$long)
ok(nrow(long[event == "Hep_C" & id == "a"]) == 1, "a confirmed case gets the disease")
ok(long[event == "Hep_C" & id == "a", date] == D("1998-03-02"),
   "and the dating code moves its first date earlier, although it could not diagnose it")
ok(!"b" %in% long[event == "Hep_C", id], "a person with only a dating code does not get the disease")
ok(!"c" %in% long[event == "Hep_C", id], "a dating code outside its window is not used at all")
ok(nrow(long[event == "Hep_B" & id == "c"]) == 1 && long[event == "Hep_B" & id == "c", date] == D("1985-04-04"),
   "the same code before the window belongs to the other disease")
ok(!"b" %in% long[event == "Hep_B", id], "and after it, it belongs to neither")

bad <- data.frame(ev = "X", ty = "CIE-10-ES", co = "B18.2", use = "date")
ok(inherits(try(prepare_definition(bad, "ev", "ty", "co", col_use = "use"), silent = TRUE), "try-error"),
   "a definition made only of dating codes is refused, not silently empty")
bad2 <- data.frame(ev = "X", ty = "CIE-10-ES", co = "B18.2", f = "2000-01-01", t = "1990-01-01")
ok(inherits(try(prepare_definition(bad2, "ev", "ty", "co", col_from = "f", col_to = "t"), silent = TRUE), "try-error"),
   "a window that ends before it starts is refused")
old_style <- prepare_definition(data.frame(ev = "X", ty = "CIE-10-ES", co = "B18.2"), "ev", "ty", "co")
ok(all(old_style$codes$use == "case") && all(is.na(old_style$codes$from)),
   "a table without the new columns behaves exactly as before")

# ---- the marker role ------------------------------------------------------------------
mk <- prepare_definition(data.frame(ev = c("PWID", "PWID"), ty = c("CIE-10-ES", "CIE-9-MC"),
                                    co = c("F11", "304.0"), rol = c("marker", "marcador")),
                         "ev", "ty", "co", col_role = "rol")
ok(identical(mk$rules$role, "marker"), "marker is a role, and its Spanish alias reaches it")
ok(!inherits(try(prepare_definition(data.frame(ev = "X", ty = "CIE-10-ES", co = "B18.2", rol = "marker"),
                                    "ev", "ty", "co", col_role = "rol"), silent = TRUE), "try-error"),
   "a marker defined by diagnoses is allowed, unlike a treatment")

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
