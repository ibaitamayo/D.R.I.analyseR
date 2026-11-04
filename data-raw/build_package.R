# Assembles the installable package D.R.I.analyseR from this working folder.
#
# The working folder is an app with its engine beside it; a package is the same code with its
# dependencies declared instead of attached, its app under inst/ and an entry point. Everything
# here is mechanical, so it is a script and not a one-off: the app stays the place where the
# code is edited, and the package is rebuilt from it.
#
# Usage:  Rscript data-raw/build_package.R
suppressMessages(library(data.table))
SRC <- normalizePath(".")
PKG <- normalizePath(file.path("..", "D.R.I.analyseR"), mustWork = FALSE)
if (dir.exists(file.path(PKG, "R")))   unlink(file.path(PKG, "R"), recursive = TRUE)
if (dir.exists(file.path(PKG, "inst"))) unlink(file.path(PKG, "inst"), recursive = TRUE)
if (dir.exists(file.path(PKG, "tests"))) unlink(file.path(PKG, "tests"), recursive = TRUE)
for (d in c("R", "inst/app", "inst/extdata", "tests", "man")) dir.create(file.path(PKG, d), recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- R/ : the same code with library() removed ---------------------------------------------
# library() inside a package attaches a second copy of a namespace at load time and hides which
# function came from where. The dependencies are declared in DESCRIPTION and NAMESPACE instead.
# A portable package holds only ASCII in its R code. The working folder keeps the characters
# as written, because they are easier to read; here a character inside a string becomes a
# \uXXXX escape, which produces exactly the same string, and one in a comment is transliterated,
# because a comment is for a reader and an escape would not be.
asciify <- function(line) {
  ch <- strsplit(line, "", fixed = TRUE)[[1]]
  if (!length(ch) || !any(utf8ToInt(line) > 127L, na.rm = TRUE)) return(line)
  out <- character(length(ch)); in_str <- ""; esc <- FALSE; comment <- FALSE
  for (i in seq_along(ch)) {
    c0 <- ch[i]; code <- utf8ToInt(c0)
    if (!comment) {
      if (esc) esc <- FALSE
      else if (nzchar(in_str)) { if (c0 == "\\") esc <- TRUE else if (c0 == in_str) in_str <- "" }
      else if (c0 == "\"" || c0 == "'") in_str <- c0
      else if (c0 == "#") comment <- TRUE
    }
    out[i] <- if (code <= 127L) c0
      else if (nzchar(in_str)) {
        # \u takes at most four hex digits, so anything above the basic plane needs \U and
        # eight: written as \u, an emoji silently becomes a different character.
        if (code > 0xFFFFL) sprintf("\\U%08x", code) else sprintf("\\u%04x", code)
      }
      else { a <- iconv(c0, "UTF-8", "ASCII//TRANSLIT"); if (is.na(a) || !nzchar(a)) "?" else a }
  }
  paste(out, collapse = "")
}

for (f in list.files("R", pattern = "[.]R$")) {
  txt <- readLines(file.path("R", f), warn = FALSE, encoding = "UTF-8")
  drop <- grepl("^\\s*suppressMessages\\(\\{?\\s*library\\(|^\\s*library\\(", txt)
  txt <- vapply(txt[!drop], asciify, character(1), USE.NAMES = FALSE)
  writeLines(txt, file.path(PKG, "R", f), useBytes = TRUE)
  if (any(drop)) say("  %-20s %d library() line(s) removed", f, sum(drop))
}

# Round-trip check: every string literal in the escaped copy must be identical to the one in
# the file it came from. An escape that silently changes a character is exactly the kind of
# thing nobody notices until it is on screen.
for (f in list.files("R", pattern = "[.]R$")) {
  a <- tryCatch(parse(file.path("R", f), keep.source = FALSE), error = function(e) NULL)
  b <- tryCatch(parse(file.path(PKG, "R", f), keep.source = FALSE), error = function(e) NULL)
  if (is.null(a) || is.null(b)) next
  strs <- function(e) { out <- character(0)
    rec <- function(x) { if (is.character(x)) out <<- c(out, x)
      else if (is.call(x) || is.pairlist(x) || is.expression(x)) for (i in seq_along(x)) rec(x[[i]]) }
    for (i in seq_along(e)) rec(e[[i]]); out }
  sa <- strs(a); sb <- strs(b)
  bad <- which(sa != sb)
  if (length(bad))
    stop("Escaping changed a string in ", f, ": ", sQuote(sa[bad[1]]), " became ", sQuote(sb[bad[1]]))
}
say("  R/: strings verified identical after escaping")

# ---- the entry point ------------------------------------------------------------------------
writeLines(c(
'#\' Run the D.R.I.analyseR application',
'#\'',
'#\' Opens the Shiny application in a browser. Every table is loaded from inside the app, so',
'#\' nothing has to be prepared beforehand; `data_dir` only saves the loading when a folder',
'#\' already holds the three tables under the names the app asks for.',
'#\'',
'#\' @param data_dir Optional folder holding "Tabla 1 - personas y diagnosticos",',
'#\'   "Tabla 2 - personas y tratamientos" and "Tabla 3 - covariables y causas de muerte"',
'#\'   (.rds or .csv). When given, a button in tab 1 loads all three at once. Defaults to the',
'#\'   environment variable DRID_DATA, and to none when that is unset.',
'#\' @param ... Passed to [shiny::runApp()], for example `port` or `launch.browser`.',
'#\' @return Called for its side effect; does not return while the application runs.',
'#\' @export',
'run_app <- function(data_dir = Sys.getenv("DRID_DATA", ""), ...) {',
'  for (p in c("shiny", "bslib", "readxl", "writexl", "reactable", "ggplot2"))',
'    if (!requireNamespace(p, quietly = TRUE))',
'      stop("The application needs the package \'", p, "\'. Install it with install.packages(\\"", p, "\\").")',
'  if (nzchar(data_dir) && !dir.exists(data_dir))',
'    stop("data_dir does not exist: ", data_dir)',
'  old <- Sys.getenv("DRID_DATA", unset = NA_character_)',
'  Sys.setenv(DRID_DATA = data_dir)',
'  on.exit(if (is.na(old)) Sys.unsetenv("DRID_DATA") else Sys.setenv(DRID_DATA = old), add = TRUE)',
'  app <- system.file("app", package = "D.R.I.analyseR")',
'  if (!nzchar(app)) stop("The application was not found in the installed package.")',
'  shiny::runApp(app, ...)',
'}',
'',
'#\' Folder holding the example definition and data tables',
'#\'',
'#\' The tables shipped with the package: the synthetic patients, and the definitions used in',
'#\' the drug-related infectious disease study they come from.',
'#\'',
'#\' @param file Optional file name. Without it, the folder itself is returned.',
'#\' @return A path.',
'#\' @export',
'#\' @examples',
'#\' head(list.files(drid_examples()))',
'drid_examples <- function(file = NULL) {',
'  d <- system.file("extdata", package = "D.R.I.analyseR")',
'  if (is.null(file)) d else file.path(d, file)',
'}'),
  file.path(PKG, "R", "run_app.R"))

writeLines(c(
'# data.table syntax inside a package needs this flag; without it every j expression is',
'# evaluated in the wrong frame when the package is installed rather than sourced.',
'.datatable.aware <- TRUE',
'',
'# Columns are referred to by name inside data.table calls, which R CMD check reads as',
'# undefined global variables. Declaring them keeps a real typo visible among the noise.',
'utils::globalVariables(c(',
'  "id", "event", "date", "cod", "code", "type", "system", "role", "rule", "use", "from", "to",',
'  "persons", "person_years", "events", "rate", "outcome", "competing", "censored", "status",',
'  "status_label", "time", "start", "end", "exit", "exited", "admin_end", "outcome_date",',
'  "competing_date", "censor_date", "group_date", "index_date", "group", "N", "V1", "pref",',
'  "f_dx", "f_drug", "f_any", "min_dispensations", "birth", "birth_year", "death", "death_code",',
'  "death_type", "last_contact", "sex", "age", "age_band", "era", "stratum", "year", "estimate",',
'  "with_index_event", "with_outcome", "without_outcome", "pct_with_outcome", "persons_at_risk",',
'  "ci_low", "ci_high", "term", "p", "model", "ev", "n", "has_dx", "has_drug", "roles", "N_",',
'  "Disease", "CodeType", "DxCode", "Use", "ValidFrom", "ValidTo", "Source", "Role", "ATC",',
'  "MinDispensations", "value", "variable", "cause", "disease", "step", "rows", "label",',
'  ".", ".terminal", "all_dating", "cell", "instance", "is_case", "name", "st", "state"))'),
  file.path(PKG, "R", "zzz.R"))

# ---- inst/ : the app and the example tables -------------------------------------------------
app <- readLines("app.R", warn = FALSE)
i_lib <- grep("^\\s*library\\(shiny\\)", app)[1]
i_src <- grep('^source\\("R/', app)
stopifnot(length(i_src) > 0)
app <- append(app[-i_src], c(
  '# The engine is the package. An installed copy is simply attached; a source checkout, where',
  '# the package is not installed, has the same files one folder up and they are sourced.',
  'if (requireNamespace("D.R.I.analyseR", quietly = TRUE)) {',
  '  library(D.R.I.analyseR)',
  '} else {',
  '  dev <- normalizePath(file.path("..", "..", "R"), mustWork = FALSE)',
  '  if (!dir.exists(dev)) dev <- "R"',
  '  for (f in list.files(dev, pattern = "[.]R$", full.names = TRUE)) source(f, local = TRUE)',
  '}'), after = min(i_src) - 1L)
writeLines(app, file.path(PKG, "inst", "app", "app.R"))
say("  app.R: %d source() line(s) replaced by the package", length(i_src))

ex <- list.files("examples", pattern = "[.](xlsx|csv)$", full.names = TRUE)
file.copy(ex, file.path(PKG, "inst", "extdata"), overwrite = TRUE)
say("  inst/extdata: %d example table(s)", length(ex))
# The suites source the engine from R/ when run in the working folder; inside the package the
# engine is already there, so the source() lines become an attach.
# Only the suites over synthetic cases. Anything else under tests/ is local validation against
# the real extraction: it would not run anywhere else and it names paths that must not travel.
for (f in list.files("tests", pattern = "^test_.*[.]R$")) {
  txt <- readLines(file.path("tests", f), warn = FALSE)
  src <- grepl('source\\("R/|for \\(f in c\\("engine"', txt)
  # The suites build their own data.tables, so they still need data.table attached; what they
  # no longer need is to source the engine, which the package provides.
  txt[src] <- 'suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)'
  txt <- txt[!duplicated(txt) | !grepl("library\\(D[.]R[.]I", txt)]
  writeLines(txt, file.path(PKG, "tests", f))
}
say("  tests: %d suite(s)", length(list.files(file.path(PKG, "tests"))))

# ---- DESCRIPTION, NAMESPACE and the rest -----------------------------------------------------
# NAMESPACE is generated from the sources: every function defined at top level is part of the
# analysis API, and a hand-written list would go stale the first time one is added.
# Only definitions in column 1: an indented one is a local helper inside another function and
# exporting it would name something that does not exist in the namespace.
# The exported names are read by parsing each file and keeping the top-level assignments whose
# value is a function. A regular expression over the text would also catch the helpers defined
# inside other functions, which exist nowhere in the namespace.
fns <- unlist(lapply(list.files(file.path(PKG, "R"), full.names = TRUE), function(f) {
  top <- parse(f, keep.source = FALSE)
  # Functions and the constants beside them alike: the coding systems, the reserved event
  # names and the table specifications are as much a part of the interface as any function,
  # and the test suites read them.
  nm <- vapply(as.list(top), function(x) {
    if (is.call(x) && length(x) >= 3L &&
        as.character(x[[1L]])[1L] %in% c("<-", "=", "<<-") && is.symbol(x[[2L]]))
      as.character(x[[2L]])[1L] else NA_character_
  }, character(1))
  nm[!is.na(nm)]
}))
fns <- sort(unique(fns[!grepl("^[.]", fns) & nzchar(fns)]))
esc <- function(x) ifelse(grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x), x, paste0("`", x, "`"))
writeLines(c(
  "# Generated by data-raw/build_package.R -- do not edit by hand.",
  paste0("export(", esc(fns), ")"),
  "import(data.table)",
  "importFrom(stats, as.formula, median, na.omit, qchisq, quantile, setNames)",
  "importFrom(survival, Surv, coxph, finegray, survfit)",
  "importFrom(utils, globalVariables, head, tail)"), file.path(PKG, "NAMESPACE"))
say("  NAMESPACE: %d exported function(s)", length(fns))

writeLines(c(
  "Package: D.R.I.analyseR",
  "Type: Package",
  "Title: Cohort, Infection, Treatment and Outcome Analysis for Drug-Related",
  "    Infectious Diseases",
  "Version: 0.1.0",
  "Authors@R: person(\"Ibai\", \"Tamayo\", email = \"ibai.tamayo.rodriguez@navarra.es\",",
  "    role = c(\"aut\", \"cre\"))",
  "Description: Finds, for each person, the first date of every disease, treatment",
  "    and outcome defined from diagnosis codes, drug dispensations and a patient",
  "    table, and takes them through the six phases of a drug-related infectious",
  "    disease study: who uses drugs, who has the infection, who was treated, what",
  "    happened, how it looks over calendar time, and what the competing risks say.",
  "    Coding systems never mix, and a code is compared only within its own:",
  "    hierarchical ones (ICD, ICPC, ATC) by prefix, flat identifiers (SNOMED,",
  "    RxNorm) by equality. A code may be declared as one that dates a disease",
  "    without diagnosing it, or as valid only within a range of dates. Definitions",
  "    travel with the data as tables, so what was measured can be read and changed",
  "    without touching the code. Both the cause-specific and the subdistribution",
  "    hazard ratio are reported, because they answer different questions.",
  "License: MIT + file LICENSE",
  "Encoding: UTF-8",
  "URL: https://github.com/ibaitamayo/D.R.I.analyseR",
  "BugReports: https://github.com/ibaitamayo/D.R.I.analyseR/issues",
  "Depends:", "    R (>= 4.2)",
  "Imports:", "    data.table,", "    survival,", "    stats,", "    utils",
  "Suggests:", "    shiny,", "    bslib,", "    readxl,", "    writexl,", "    reactable,",
  "    ggplot2,", "    jsonlite,", "    bupaR,", "    edeaR,", "    processmapR,", "    DiagrammeR",
  "RoxygenNote: 7.2.3"), file.path(PKG, "DESCRIPTION"))

yr <- format(Sys.Date(), "%Y")
writeLines(c(paste("YEAR:", yr), "COPYRIGHT HOLDER: Ibai Tamayo"), file.path(PKG, "LICENSE"))
writeLines(c(
  sprintf("MIT License"), "",
  sprintf("Copyright (c) %s Ibai Tamayo", yr), "",
  "Permission is hereby granted, free of charge, to any person obtaining a copy",
  "of this software and associated documentation files (the \"Software\"), to deal",
  "in the Software without restriction, including without limitation the rights",
  "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
  "copies of the Software, and to permit persons to whom the Software is",
  "furnished to do so, subject to the following conditions:", "",
  "The above copyright notice and this permission notice shall be included in all",
  "copies or substantial portions of the Software.", "",
  "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
  "SOFTWARE."), file.path(PKG, "LICENSE.md"))

writeLines(c("^data-raw$", "^.*\\.Rproj$", "^\\.Rproj\\.user$", "^\\.github$",
             "^README\\.Rmd$", "^LICENSE\\.md$", "^\\.DS_Store$"), file.path(PKG, ".Rbuildignore"))
writeLines(c(".Rproj.user", ".Rhistory", ".RData", ".Ruserdata", ".DS_Store",
             "*.tar.gz", "*.Rcheck/", "", "# Never: these hold patient-level rows.",
             "*.rds", "tablas_app/", "datos*/", "EUDA*/"), file.path(PKG, ".gitignore"))

# ---- man/ : grouped help pages ---------------------------------------------------------------
# Written here rather than with roxygen2, which is not a dependency of this project. Every
# exported name needs a help page or R CMD check reports it as missing; grouping them by what
# they are for gives a reader something better than eighty pages of one paragraph each. The
# groups are checked against NAMESPACE below, so a new export cannot be forgotten.
unlink(file.path(PKG, "man"), recursive = TRUE); dir.create(file.path(PKG, "man"))
rd <- function(name, title, desc, aliases, details = NULL, seealso = NULL, extra = NULL) {
  esc_rd <- function(x) gsub("([%{}\\\\])", "\\\\\\1", x)
  writeLines(c(
    "% Generated by data-raw/build_package.R -- do not edit by hand.",
    paste0("\\name{", name, "}"),
    paste0("\\alias{", aliases, "}"),
    paste0("\\title{", esc_rd(title), "}"),
    paste0("\\description{\n", esc_rd(desc), "\n}"),
    if (!is.null(details)) paste0("\\details{\n", esc_rd(details), "\n}"),
    extra,
    if (!is.null(seealso)) paste0("\\seealso{", seealso, "}"),
    "\\keyword{manip}"), file.path(PKG, "man", paste0(name, ".Rd")))
  aliases
}
covered <- character(0)
add <- function(...) covered <<- c(covered, rd(...))

add("reading-tables", "Reading the input tables",
  paste("Each function takes one table as it arrives and returns it in the shape the search",
        "expects, together with a summary of what was dropped and why. Nothing is discarded",
        "silently: a row without a date, a code in a system the definitions do not use or a",
        "repeated identifier is counted and reported."),
  c("reading-tables", "prepare_diagnoses", "prepare_drugs", "prepare_patients", "prepare_eras",
    "as_date", "detect_date_format", "normalise_code", "coding_system", "coding_version",
    "version_family", "is_icd10_dx_format", "DATE_FORMATS", "DRUG_SYSTEMS", "EXACT_SYSTEMS"),
  details = paste("A code is compared only within its own coding system. Hierarchical systems",
        "(ICD, ICPC, ATC) are compared by prefix, because truncating a code is how a definition",
        "says \"the whole category\"; flat identifiers (SNOMED, RxNorm) are compared whole, since",
        "a shorter one is not a parent but a different concept."))

add("definitions", "Definition tables: what is being measured",
  paste("A definition is a list of codes with a name. Beyond the name, the coding system and",
        "the code, a row may declare the role the event plays, whether the code can diagnose or",
        "only date, and the range of dates in which it applies."),
  c("definitions", "prepare_definition", "ROLES", "RULES", "USES", "ROLE_ALIASES",
    "RULE_ALIASES", "USE_ALIASES", "RESERVED_EVENTS", "reserved_event_warnings",
    "in_window", "table_specs"),
  details = paste("Roles are birth, cohort, disease, comorbidity, treatment, marker and mortality.",
        "A marker says which population the analysis is about and never starts a follow-up or",
        "acts as an outcome. A code whose use is \"date\" cannot establish the event but can",
        "supply its earliest date in a person who has it on other evidence; a definition made",
        "only of such codes is refused."))

add("finding-events", "Finding the first date of every event",
  paste("The search itself: codes are matched against the diagnosis and dispensation tables and",
        "reduced to one date per person and per event, the first occurrence."),
  c("finding-events", "find_events", "match_codes", "unmatched_warnings", "rows_by_system",
    "system_coverage_warnings", "version_warnings"))

add("patient-events", "Events that come from the patient table",
  paste("Birth, death, the cause of death and the end of follow-up are not searched for in the",
        "diagnosis table: they are read from the patient table and appended to the same long",
        "table, so every view downstream treats them like any other event."),
  c("patient-events", "patient_events", "cohort_entry", "event_roles", "order_terminal_last",
    "age_at", "age_band", "era_at", "add_months", "year_of"),
  details = paste("A death with no recorded cause becomes an event of its own rather than being",
        "folded into the others, so the extent of missing cause information stays visible."))

add("population", "Who the analysis is about",
  paste("The cohort as defined by the drug-use events, or a matched comparison with people who",
        "have no documented use."),
  c("population", "population_groups", "match_controls", "phase_funnel"))

add("calendar-views", "Uptake, rates and counts by calendar year",
  paste("Views over calendar time: how many people had reached each event by the end of each",
        "year, how many person-years were accumulated, and the incidence rate with exact",
        "Poisson confidence intervals."),
  c("calendar-views", "follow_up", "cumulative_uptake", "rates_by_year", "events_by_year",
    "person_years_by_year", "by_group", "group_from_event", "trim_empty_years",
    "empty_uptake", "empty_rates"),
  details = paste("Years before the first person and after the last are not data but the span of",
        "the oldest record in the table, so they are dropped. An empty year in the middle is",
        "kept, because a gap is a finding."))

add("competing-risks", "Time to event with competing risks",
  paste("The person-level table the models read, and the two summaries that do not need a model:",
        "events and rates per group, and the Aalen-Johansen cumulative incidence."),
  c("competing-risks", "time_to_event", "event_summary", "cumulative_incidence",
    "CENSORED", "OUTCOME", "COMPETING"),
  details = paste("A person who dies of another cause can no longer reach the outcome. That is not",
        "censoring, which means observation stopped: the outcome has become impossible, so the",
        "death is treated as a competing event. One minus Kaplan-Meier would overestimate the",
        "cumulative incidence, which is why Aalen-Johansen is used."))

add("models", "Cause-specific and subdistribution hazard ratios",
  paste("Both models are fitted and reported side by side, because they answer different",
        "questions and can point in different directions."),
  c("models", "model_data", "usable_covariates", "model_warnings", "fit_cause_specific",
    "fit_subdistribution", "fit_models", "tidy_cox"),
  details = paste("The cause-specific hazard ratio asks how much faster the outcome happens among",
        "those still at risk; the subdistribution (Fine-Gray) hazard ratio asks how much the",
        "cumulative incidence differs. A treatment that prolongs survival grants more time in",
        "which to reach the outcome, and only the second reflects that. Neither is called",
        "\"the\" hazard ratio."))

add("process-map", "The sequences people actually follow",
  paste("The event log and the process map built from it: which events each person passes",
        "through, in what order, and the median time between consecutive events."),
  c("process-map", "build_eventlog", "process_map_widget", "ROLE_COLOURS"))

add("app-ui", "Pieces the application uses",
  paste("Help text, table specifications and guided-tour steps shown by the Shiny application.",
        "They are part of the package so that what the application says about a table and what",
        "the engine does with it cannot drift apart."),
  c("app-ui", "DATE_HELP", "TYPE_HELP", "DRUG_TYPE_HELP", "GENERAL_HELP", "html_table",
    "spec_ui", "guide_steps", "guide_hints", "guide_head", "send_hint", "ghost_zone"))

invisible(rd("run_app", "Run the D.R.I.analyseR application",
  paste("Opens the Shiny application in a browser. Every table is loaded from inside the",
        "application, so nothing has to be prepared beforehand."),
  c("run_app", "drid_examples"),
  extra = c(
    "\\usage{",
    "run_app(data_dir = Sys.getenv(\"DRID_DATA\", \"\"), ...)",
    "",
    "drid_examples(file = NULL)",
    "}",
    "\\arguments{",
    "  \\item{data_dir}{Optional folder already holding the three tables under the names the",
    "    application asks for. A button in tab 1 then loads all three at once. Defaults to the",
    "    environment variable \\code{DRID_DATA}, and to none when that is unset.}",
    "  \\item{...}{Passed to \\code{\\link[shiny]{runApp}}, for example \\code{port}.}",
    "  \\item{file}{Optional file name. Without it, the folder itself is returned.}",
    "}",
    "\\value{\\code{run_app} is called for its side effect. \\code{drid_examples} returns a path.}",
    "\\examples{",
    "head(list.files(drid_examples()))",
    "}")))
covered <- c(covered, "run_app", "drid_examples")

missing <- setdiff(fns, covered)
if (length(missing))
  stop("These exported names have no help page; add them to a group in data-raw/build_package.R: ",
       paste(missing, collapse = ", "))
stray <- setdiff(setdiff(covered, fns), c("reading-tables", "definitions", "finding-events",
  "patient-events", "population", "calendar-views", "competing-risks", "models", "process-map", "app-ui"))
if (length(stray)) say("  man/: %d name(s) documented but not exported: %s", length(stray), paste(stray, collapse = ", "))
say("  man/: %d page(s) covering %d exported name(s)", length(list.files(file.path(PKG, "man"))), length(fns))

say("  DESCRIPTION, NAMESPACE, LICENSE, .Rbuildignore, .gitignore")
say("\nAssembled in %s", PKG)
