# D.R.I.analyseR

Cohort, infection, treatment and outcome analysis for drug-related infectious diseases.

The package answers one question for every person in a health-records extraction: **on what
date did each thing first happen to them?** From those dates it builds the six phases of a
drug-related infectious disease study — who uses drugs, who has the infection, who was treated,
what happened, how it looks over calendar time, and what the competing risks say.

What is measured lives in tables, not in code. A definition is a list of codes with a name, so
it can be read, audited and replaced without touching the package. **[Read the assumptions that
ship with it](ASSUMPTIONS.md)**: what counts as a drug user, how injecting is inferred, and what
the package decides that no table can change.

---

## Getting started from nothing

**1. Install R.** Download it from [cran.r-project.org](https://cran.r-project.org/) and install
it like any other program. Version 4.2 or newer.

**2. Install RStudio** (optional but much easier):
[posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/). It is free.
Everything below is typed into the pane called **Console**.

**3. Install the package.** Copy these two lines into the console and press Enter:

```r
install.packages("remotes")
remotes::install_github("ibaitamayo/D.R.I.analyseR")
```

The first line is needed once on a new computer. If R asks *"Do you want to install from
sources the package which needs compilation?"*, answer **no** (type `n` and Enter).

**4. Install what the application needs.** The engine itself needs almost nothing; the
application and its charts need a few more:

```r
install.packages(c("shiny", "bslib", "readxl", "writexl", "reactable", "ggplot2"))
```

And, only for the process map in tab 4:

```r
install.packages(c("bupaR", "edeaR", "processmapR", "DiagrammeR"))
```

This takes several minutes the first time and nothing afterwards.

**5. Run it:**

```r
D.R.I.analyseR::run_app()
```

A browser window opens. To stop, press the red stop button in RStudio, or Esc in the console.

### Trying it without any data

In the application: tab **1. Patient data** → *Load example patient tables*, then tab
**2. Definitions** → *Load the DRID definitions*, then tab **3. Event search** → *Find events*.
That should report around 20,000 persons and 99,000 person-event pairs in a second or two.

The people are synthetic, invented for this purpose. The definitions are not: they are the real
code lists described in [ASSUMPTIONS.md](ASSUMPTIONS.md), so what the demo shows is the actual
analysis running on made-up patients.

---

## Using your own tables

There are two ways in. Either is fine; the second only saves clicking.

### Loading them by hand

In tab 1 and tab 2, drop each file into its box, or browse to it. Accepted: `.csv`, `.txt`
(separator `;`, `,` or tab), Excel `.xlsx` / `.xls`, or `.rds`. **Column names are free**: after
loading, the application asks which column is which.

### Loading a whole folder at once

If a folder already holds the three patient tables, give it when starting:

```r
D.R.I.analyseR::run_app(data_dir = "C:/my_project/tables")
```

A button then appears in tab 1, **Load project tables**, that reads all three. For this the
files must be named **exactly** like this, in `.rds` or `.csv` (`.rds` is preferred when both
are present):

```
Tabla 1 - personas y diagnosticos.rds      (or .csv)
Tabla 2 - personas y tratamientos.rds      (or .csv)
Tabla 3 - covariables y causas de muerte.rds
```

The names carry no meaning beyond being what the button looks for. Anything else in the folder
is ignored. If one is missing, the application says which.

### What goes in each table

**Table 1 — one row per diagnosis, per patient, per date**

| Patient id | Code | Code type | Date |
|---|---|---|---|
| 100234 | F11.20 | 10ES | 2019-05-01 |
| 100234 | P19 | CIAP2 | 2018-01-15 |

Code type is the label of the coding system: `CIAP`, `CIAP2` or `ICPC`; `9`, `CIE9` or
`ICD-9-CM`; `10`, `10ES`, `CIE-10-ES`, `ICD-10-CM`…; or `SNOMED`. Rows with a label the
application does not recognise are not used, and it says how many.

**Table 2 — one row per dispensation**

| Patient id | Drug code | Code type | Date |
|---|---|---|---|
| 100234 | J05AP55 | ATC | 2018-01-10 |

Without a code-type column the whole table is read as ATC, and the application says so.

**Table 3 — one row per person.** Only the identifier is required; what is missing is simply
not derived.

| Patient id | Sex | Year of birth | Date of birth | Date of death | Cause code | Code type | Area | End of follow-up |
|---|---|---|---|---|---|---|---|---|
| 100234 | F | 1978 | 1978-05-02 | 2021-03-14 | B18.2 | CIE-10-ES | ZBS07 | 2021-03-14 |

Age is not a column: a stored age is the age at extraction, not at the time zero of the
analysis, so it is derived. A date of birth is used in preference to a year; with only a year,
the birth event is dated 1 July and the age carries that uncertainty.

Dates may be `YYYY-MM-DD` (best), `DD/MM/YYYY`, `DD-MM-YYYY`, `YYYY/MM/DD`, `DD.MM.YYYY`,
`YYYYMMDD` or Excel date cells. The format is detected and shown after loading.

**The identifier must be the same in all three tables.** Nothing else links them.

### The definition tables

Three more spreadsheets say what is being measured. They ship filled in, and
**[ASSUMPTIONS.md](ASSUMPTIONS.md)** explains every column and how to replace any of them. The
short version: one row per code, with the disease name, the coding system, the code, and
optionally the role it plays, whether it diagnoses or only dates, the dates between which it
applies, and where it comes from.

Nothing in this package is specific to hepatitis C or to drugs. Change the tables and the same
six phases run on a different disease.

---

## What it reports

Cumulative uptake and incidence rates by calendar year with exact Poisson intervals; a process
map of the sequences people actually follow; Aalen–Johansen cumulative incidence; and two
models side by side — the **cause-specific** hazard ratio and the **subdistribution**
(Fine–Gray) hazard ratio. They answer different questions and can point in different
directions, which is why both are shown and neither is called *the* hazard ratio.

The last tab, **Definitions in use**, documents what the loaded definitions actually say, in
seven steps and then code by code, with the source where one exists and the decision taken
where it does not.

## Data

No patient-level data is in this repository and none should be added to it. The example tables
under `inst/extdata` are synthetic. The application runs on your own machine and sends nothing
anywhere.

## Tests

```r
for (f in list.files(system.file("tests", package = "D.R.I.analyseR"), full.names = TRUE)) source(f)
```

Six suites over synthetic cases whose answer is known, including one that submits the whole
pipeline to empty and near-empty cohorts: a cohort filter will produce them, so they must
return an answer or a sentence, never a stack trace.

## Licence

MIT. See [LICENSE.md](LICENSE.md).
