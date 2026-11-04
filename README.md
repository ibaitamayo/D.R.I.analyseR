# D.R.I.analyseR

Cohort, infection, treatment and outcome analysis for drug-related infectious diseases.

The package answers one question for every person in a health-records extraction: **on what
date did each thing first happen to them?** From those dates it builds the six phases of a
drug-related infectious disease study — who uses drugs, who has the infection, who was treated,
what happened, how it looks over calendar time, and what the competing risks say.

What is measured lives in tables, not in code. A definition is a list of codes with a name, so
it can be read, audited and replaced without touching the package. That is what makes the same
analysis portable between registries that share nothing but the shape of their data.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ibaitamayo/D.R.I.analyseR")
```

The engine depends only on `data.table` and `survival`. The application needs `shiny`, `bslib`,
`readxl`, `writexl`, `reactable` and `ggplot2`; the process map additionally needs `bupaR`,
`edeaR`, `processmapR` and `DiagrammeR`.

## Running the application

```r
D.R.I.analyseR::run_app()
```

Everything is loaded from inside the application; nothing has to be prepared first. Tab 1 takes
the patient tables, tab 2 the definitions, and the **Load example tables** buttons fill both
with synthetic data so the whole thing can be tried without touching real records.

A folder that already holds the three tables under the names the application asks for can be
passed once:

```r
D.R.I.analyseR::run_app(data_dir = "~/my_project/tables")
```

## The three input tables

| Table | One row is | Required columns |
|---|---|---|
| Persons and diagnoses | one diagnosis for one patient on one date | patient id, code, code type, date |
| Persons and treatments | one dispensation to one patient on one date | patient id, drug code, date |
| Patient covariates | one person | patient id; sex, birth, death, cause, area and end of follow-up are optional |

Column names are free: the application asks which column is which. Codes are compared only
within their own coding system — ICD-9, ICD-10, ICPC and ATC by prefix, SNOMED and RxNorm by
equality — so an ICD-9 code can never match an ICD-10 one.

## The definition tables

One row is one code inside one definition. Beyond the name, the type and the code, three
columns express what a plain code list cannot:

| Column | Meaning |
|---|---|
| `Role` | `cohort`, `disease`, `comorbidity`, `treatment`, `marker` or `mortality`. A **marker** says the analysis is about these people — that they injected, that they were in prison — and never starts anyone's follow-up or acts as an outcome. |
| `Use` | `case` (default) diagnoses and dates. `date` can only date: an unspecified code in a person already diagnosed by other means is often the earliest record of the condition, but it cannot establish it. A definition made only of `date` codes is refused. |
| `ValidFrom` / `ValidTo` | the range in which the row applies, so the same code can belong to one disease up to a date and to another after it. |

The application has a **Definitions in use** tab that renders all of this from the tables
actually loaded, with the published source where one exists and the decision taken where it
does not.

## What it reports

Cumulative uptake and incidence rates by calendar year with exact Poisson intervals; a process
map of the sequences people actually follow; Aalen–Johansen cumulative incidence; and two
models side by side — the **cause-specific** hazard ratio and the **subdistribution**
(Fine–Gray) hazard ratio. They answer different questions and can point in different
directions, which is why both are shown and neither is called *the* hazard ratio.

## Tests

```r
for (f in list.files(system.file("tests", package = "D.R.I.analyseR"), full.names = TRUE)) source(f)
```

Six suites over synthetic cases whose answer is known, including one that submits the whole
pipeline to empty and near-empty cohorts: a cohort filter will produce them, so they must
return an answer or a sentence, never a stack trace.

## Data

No patient-level data is in this repository and none should be added to it. The example tables
under `inst/extdata` are synthetic. The scripts under `data-raw/` read a removable volume and
are kept out of the built package.

## Licence

MIT. See `LICENSE.md`.
