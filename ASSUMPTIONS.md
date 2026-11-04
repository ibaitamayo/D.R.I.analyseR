# The assumptions

Every decision this package makes about *what counts as what* is written in three tables, not
in code. This page states the decisions that come with the package, where each one comes from,
and — the point of the exercise — how to replace any of them.

The application shows the same thing, generated from whatever tables are loaded, in its last
tab, **Definitions in use**.

---

## What counts as a drug user

A person enters the cohort on the **earliest** date on which any of nine definitions is met.
Seven are diagnosis codes, two are dispensations.

| Event | Where it comes from |
|---|---|
| `Drug_use` | Quan 2005, *Med Care* 43(11):1130-9 — Elixhauser drug abuse, ICD-10 and enhanced ICD-9-CM; plus the BDCAP ICPC-2 catalogue for primary care; Z86.4 from Ball 2017 |
| `Drug_induced_mental_disorders` | Quan 2005, same algorithm |
| `Nondependent_drug_abuse` | Quan 2005, same algorithm |
| `Poisoning_narcotics_psychostimulants` | EUDA definition for general mortality registers (until 2023 *Selection B*): T40.0–T40.9 and T43.6 |
| `Accidental_drug_poisoning` | INE reduced list of 102 causes of death, group 095, *Envenenamiento accidental por psicofármacos y drogas de abuso* |
| `Medication_abuse`, `Drug_counselling` | BDCAP ICPC-2 catalogue |
| `Opioid_agonist_treatment`, `Methadone_treatment` | ATC N07BC — a dispensation, used as a proxy for dependence |

**Two deviations worth knowing.** The EUDA definition is a *cause of death*: there the T code is
combined with an external cause (X42, X62, Y12, X44, X64, Y14). Applied to diagnosis records,
where no external cause exists, the T code is used alone. And the INE group 095 entry for E850
is printed as `E850.0-.9`, which reads either as a range or as two codes; E850.1 (methadone)
was kept by decision, and the definition table says so on that row.

Entering the cohort is not the same as being a current user. It is the first date on which the
records say the person used drugs, and nothing after it removes them.

## How we know someone injects

There is **no code for injecting** in ICD-9, ICD-10 or ICPC. `F11.20` is opioid dependence and
says nothing about route. Route of administration is collected in the treatment-demand registry
that feeds the EUDA indicator, not in clinical coding. So it has to be inferred, and the
inference is declared rather than assumed.

`Injecting_drug_use` is a **marker**: a person is marked if they have either

- an **opioid use disorder** — ICD-10 F11, T40.1; ICD-9 304.0, 305.5, 965.01, E850.0 — or
- **hepatitis C**, by a specific code or an eradicating treatment.

Hepatitis C is in the definition because in a cohort of diagnosed drug users it is acquired by
injecting; blood products have been screened since 1990.

Two further markers were tested and **rejected**, which is as much a part of the assumption as
what was kept. In the Navarre data, opioid agonist treatment *on its own* carries 3.8 % of
diagnosed hepatitis C and skin and soft-tissue infections *on their own* 6.9 %, against 3.5 %
in the rest of the cohort: they add members without adding injectors. Opioid use disorder on
its own carries 44.1 %, which is where the national figure for people who inject drugs sits
(OEDA 2023: 40.4 % among those injecting in the previous 12 months, 48.2 % among those who ever
injected).

**What this costs.** Because hepatitis C is part of the marker, the prevalence of hepatitis C
among marked people cannot be reported as an estimate — it is partly built in. Treatment uptake
is unaffected, since its denominator is marked people *with* hepatitis C. Mortality from a
hepatitis C-associated cause in this group has to be read knowing the group was formed partly
by having it.

A marker never starts anyone's follow-up and is never an outcome. It restricts the cohort, and
the restriction is applied — or not — with a checkbox when the results are read.

## Other definitions that ship

| Event | Basis |
|---|---|
| `HCV` | Specific hepatitis C codes, or an eradicating treatment. Unspecified viral hepatitis (ICPC-2 D72) from 1990 on **dates** a confirmed case but never diagnoses one |
| `HBV` | Specific hepatitis B codes, plus D72 **before** 1990: the hepatitis C virus was identified in 1989, so unspecified viral hepatitis recorded earlier cannot be it |
| `HIV`, `Endocarditis` | Ball 2017, validated algorithm for injection-drug-use-associated infective endocarditis |
| `HCV_treatment_DAA` / `_interferon_other` / `_all` | ATC J05AP and L03AB |
| `HCV_associated_death` | Hepatitis C, hepatocellular carcinoma and chronic liver disease codes, matching the definition used in the Norwegian register study. Explicitly alcoholic codes (K70; ICD-9 571.0–571.3) are excluded |

## What the package decides that a table cannot change

Being honest about the limits matters more than the list above, because these are the
assumptions nobody can edit out:

1. **One date per person per event: the first.** A second episode is not an event. Everything
   downstream is built on first dates.
2. **Entry into the cohort is the earliest of the `cohort` events.** Which events those are is
   yours to set; that it is the earliest of them is not.
3. **A code is compared only within its own coding system**, by prefix for ICD, ICPC and ATC,
   whole for SNOMED and RxNorm.
4. **Any death that is not one of the defined causes competes with the outcome**, rather than
   censoring it.
5. **There is no calendar left-truncation.** Follow-up starts at the index event, not at a date
   you choose. Where a data source begins later than the records — hospital pharmacy from 2012,
   say — person-time before it is counted with a numerator that could not have been anything
   but zero.

Everything else — which codes, in which systems, under which name, in which role, diagnosing or
only dating, within which window, and how many dispensations are needed — is in the tables.

## Replacing any of this

The definitions that ship are loaded from inside the package, so they cannot be edited in
place. To change one, use **Download the definitions now loaded** in tab 2: it writes out what
is loaded, exactly as it is, one sheet per table. Edit the rows and load the file back in. To
change who counts as a drug user, change which rows carry `Role = cohort`.

Three spreadsheets, one row per code:

**Disease – diagnosis code**

| Disease | CodeType | DxCode | Role | Use | ValidFrom | ValidTo | Source |
|---|---|---|---|---|---|---|---|
| `HCV` | CIE-10-ES | B18.2 | disease | case | | | Ball 2017 |
| `HCV` | CIAP-2 | D72 | disease | date | 1990-01-01 | | dates a confirmed case |
| `HBV` | CIAP-2 | D72 | disease | case | | 1989-12-31 | before 1990 it cannot be HCV |

- **Role** — `cohort`, `disease`, `comorbidity`, `treatment`, `marker` or `mortality`.
- **Use** — `case` (default) diagnoses and dates; `date` can only supply an earlier date to
  someone who has the event on other evidence. A definition made only of `date` codes is
  refused.
- **ValidFrom / ValidTo** — the range in which that row applies. Either end may be left empty.
- **Source** — free text, not used in the search. It is what the documentation tab prints, so
  it is where a citation, or the decision taken for want of one, belongs.

**Disease – drug code**: `Disease`, `ATC`, `Role`, `MinDispensations`, `Source`. Rows with the
same `Disease` as in the diagnosis table form **one** definition: diagnosed *or* treated.

**Cause of death – code**: `Disease`, `CodeType`, `DxCode`. Searched only against the cause of
death in the patient table, never against the diagnosis table — a hospital diagnosis of
hepatitis C is not a death from hepatitis C.

Column names are free: the application asks which column is which when the file is loaded.
