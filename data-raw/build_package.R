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
for (f in list.files("R", pattern = "[.]R$")) {
  txt <- readLines(file.path("R", f), warn = FALSE)
  drop <- grepl("^\\s*suppressMessages\\(\\{?\\s*library\\(|^\\s*library\\(", txt)
  writeLines(txt[!drop], file.path(PKG, "R", f))
  if (any(drop)) say("  %-20s %d library() line(s) removed", f, sum(drop))
}

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
'  "MinDispensations", "value", "variable", "cause", "disease", "step", "rows", "label"))'),
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
for (f in list.files("tests", pattern = "[.]R$")) {
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
  "importFrom(survival, Surv, coxph, finegray)",
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
say("  DESCRIPTION, NAMESPACE, LICENSE, .Rbuildignore, .gitignore")
say("\nAssembled in %s", PKG)
