# Patient table, eras and death events (Rscript tests/test_covariates.R)
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date

# ---- age ------------------------------------------------------------------------------
ok(age_at(D("1980-06-15"), NA, D("2020-06-14")) == 39, "age: birthday not yet reached")
ok(age_at(D("1980-06-15"), NA, D("2020-06-15")) == 40, "age: birthday reached")
ok(age_at(D("1980-02-29"), NA, D("2021-02-28")) == 40, "age: 29 February against 28 February")
ok(age_at(NA, 1980, D("2020-01-01")) == 40, "age from the year alone")
ok(is.na(age_at(D("1750-01-01"), NA, D("2020-01-01"))), "sentinel birth year gives NA")
ok(is.na(age_at(D("2030-01-01"), NA, D("2020-01-01"))), "birth after the reference gives NA")
ok(is.na(age_at(D("2020-06-01"), NA, D("2020-01-01"))), "birth later in the same year gives NA")
ok(identical(age_at(c(D("1980-01-01"), NA), c(NA, 1990), c(D("2020-01-01"), D("2020-01-01"))), c(40, 30)),
   "vectorised over date and year")
ok(identical(age_band(c(10, 35, 80), c(18, 50, 65)), c("[-Inf,18)", "[18,50)", "[65, Inf)")) ||
   length(unique(age_band(c(10, 35, 80), c(18, 50, 65)))) == 3, "age bands split into distinct bands")
ok(all(is.na(age_band(c(10, 35), NULL))), "no cut points: no band")

# ---- eras -----------------------------------------------------------------------------
e <- prepare_eras(data.table(f = D(c("1900-01-01", "2017-01-01")), n = c("Before DAA", "DAA")), "f", "n")
ok(nrow(e$data) == 2 && !length(e$warnings), "two eras, no warning")
ok(is.na(e$data$end[2]) && e$data$end[1] == D("2016-12-31"), "the last era is open and the first closes the day before")
ok(era_at(D("2010-05-01"), e$data) == "Before DAA", "a date inside the first era")
ok(era_at(D("2017-01-01"), e$data) == "DAA", "the first day belongs to its era")
ok(era_at(D("2016-12-31"), e$data) == "Before DAA", "the last day of an era")
ok(is.na(era_at(as.Date(NA), e$data)), "a missing date has no era")
e2 <- prepare_eras(data.table(f = D("2017-01-01"), n = "DAA"), "f", "n")
ok(era_at(D("2000-01-01"), e2$data) == "Before DAA", "with one boundary the earlier era is named automatically")
ok(is.null(prepare_eras(data.table(f = D(character()), n = character()), "f", "n")$data),
   "an empty era table means no era variable")
ok(is.null(prepare_eras(NULL, "f", "n")$data), "no era table at all means no era variable")
mid <- prepare_eras(data.table(f = D("2017-03-01"), n = "DAA"), "f", "n")
ok(any(grepl("middle of a calendar year", mid$warnings)), "a boundary that is not 1 January is warned about")

# ---- patient table --------------------------------------------------------------------
pt <- data.table(
  pac  = c("p1", "p2", "p3", "p4", "p5", "p2"),
  sexo = c("F", "M", "F", "M", "F", "M"),
  nac  = D(c("1970-01-01", "1980-01-01", "1990-01-01", "1960-01-01", "1975-01-01", "1980-01-01")),
  fallec = D(c("2019-06-01", NA, "2020-03-01", "2018-01-01", NA, NA)),
  causa = c("B18.2", NA, "C22.0", "I21.9", NA, NA),
  tipo  = c("CIE-10-ES", NA, "CIE-10-ES", "CIE-10-ES", NA, NA))
p <- prepare_patients(pt, "pac", col_birth = "nac", col_sex = "sexo", col_death = "fallec",
                      col_death_code = "causa", col_death_type = "tipo")
ok(nrow(p$data) == 5, "one row per person: the repeated identifier is collapsed")
ok(p$summary[step == "Repeated identifier (first row kept)", rows] == 1, "and it is counted")
ok(!p$approximate_age, "with a full birth date the age is not flagged as approximate")
p_y <- prepare_patients(data.table(pac = "p1", anio = 1980), "pac", col_birth_year = "anio")
ok(p_y$approximate_age, "with only the birth year the age is flagged as approximate")
# plausibilidad del nacimiento, no de una edad en una fecha arbitraria
p_ok <- prepare_patients(data.table(pac = as.character(1:3), anio = c(1950, 2005, 2024)),
                         "pac", col_birth_year = "anio")
ok(p_ok$summary[grepl("Implausible birth", step), rows] == 0,
   "a recent year of birth is not implausible")
p_bad <- prepare_patients(data.table(pac = as.character(1:3), anio = c(1753, 1990, 2099)),
                          "pac", col_birth_year = "anio")
ok(p_bad$summary[grepl("Implausible birth", step), rows] == 2,
   "a sentinel year and a future year are counted")
p_nt <- prepare_patients(data.table(pac = "p1", f = D("2019-01-01"), c = "B18.2"),
                         "pac", col_death = "f", col_death_code = "c")
ok(is.na(p_nt$data$death_code) && p_nt$summary[step == "Cause code without a coding system (not used)", rows] == 1,
   "a cause code with no coding system is dropped and counted")

# ---- death as events ------------------------------------------------------------------
causes <- prepare_definition(
  data.table(e = c("HCV_death", "Hepatocellular_carcinoma_death"), t = "CIE-10-ES", c = c("B18.2", "C22.0")),
  "e", "t", "c")
ev <- patient_events(p$data, causes$codes)
ok(nrow(ev[event == "Death"]) == 3, "one Death per recorded death")
ok(ev[event == "HCV_death", id] == "p1", "the cause matches its definition")
ok(ev[event == "Hepatocellular_carcinoma_death", id] == "p3", "and the other cause too")
ok(ev[event == "Death_other_cause", id] == "p4", "a coded cause that matches nothing is its own node")
ok(nrow(ev[event == "Death_cause_unregistered"]) == 0, "nobody here died without a cause code")
ok(all(ev[!event %in% c("End_of_follow_up", "Birth"), date] %in% p$data$death),
   "every death event is dated at the death date")
ok(nrow(ev[event == "Birth"]) == nrow(p$data[!is.na(birth) | !is.na(birth_year)]),
   "birth is an event of its own, one per person who has a birth year")
year_only <- p$data[is.na(birth) & !is.na(birth_year), id]
ok(all(format(ev[event == "Birth" & id %in% year_only, date], "%m-%d") == "07-01"),
   "with only a year, birth is dated at the midpoint of that year")
# On its own fixture, because the table above carries years and not dates of birth.
ev_b <- patient_events(data.table(id = c("b1", "b2"), birth = D(c("1970-03-02", NA)),
                                  birth_year = c(NA, 1980), death = as.Date(c(NA, NA)),
                                  death_code = NA_character_, death_type = NA_character_))
ok(ev_b[id == "b1", date] == D("1970-03-02"),
   "a real date of birth is used as it comes, not replaced by the midpoint")
ok(ev_b[id == "b2", date] == D("1980-07-01"), "and the year-only one still falls on the midpoint")
# the three death nodes add up to the deaths
n3 <- nrow(ev[event %in% c("Death_other_cause", "Death_cause_unregistered")]) +
  uniqueN(ev[event %in% c("HCV_death", "Hepatocellular_carcinoma_death"), id])
ok(n3 == nrow(ev[event == "Death"]), "matched + other + unknown = all deaths")
# without cause definitions nothing is guessed
ev0 <- patient_events(p$data, NULL)
ok(nrow(ev0[event == "Death_other_cause"]) == 3 && nrow(ev0[event == "HCV_death"]) == 0,
   "with no cause definitions, a coded cause is 'other', never guessed")
no_code <- prepare_patients(data.table(pac = "p9", f = D("2019-01-01")), "pac", col_death = "f")
ok(patient_events(no_code$data, causes$codes)[event == "Death_cause_unregistered", .N] == 1,
   "a death with no cause code is Death_cause_unregistered")
# end of follow-up
p_lc <- prepare_patients(data.table(pac = "p1", uc = D("2022-12-31")), "pac", col_last_contact = "uc")
ok(patient_events(p_lc$data)[event == "End_of_follow_up", date] == D("2022-12-31"), "end of follow-up as an event")

# ---- reserved names and kinds ---------------------------------------------------------
ok(length(reserved_event_warnings(c("HCV", "Death"))) == 1, "a definition named Death is rejected")
ok(length(reserved_event_warnings(c("HCV", "Endocarditis"))) == 0, "ordinary names are fine")

# ---- roles, declared in the definition table ------------------------------------------
r <- event_roles(data.table(event = c("HCV", "Homelessness", "Opioid_agonist_treatment"),
                            role = c("disease", "comorbidity", "treatment")),
                 cause_events = "HCV_death")
ok(r[event == "Homelessness", role] == "comorbidity", "a comorbidity keeps its declared role")
ok(r[event == "HCV_death", role] == "mortality", "a cause-of-death event is mortality")
ok(all(r[event %in% setdiff(RESERVED_EVENTS, c("Birth", "Cohort_entry")), role] == "mortality"),
   "the reserved death events are mortality")
ok(r[event == "Birth", role] == "birth" && r[event == "Cohort_entry", role] == "cohort",
   "birth and entry into the cohort are not deaths")
ok(nrow(event_roles(NULL, "HCV_death")) == length(RESERVED_EVENTS) + 1,
   "with no definitions only the mortality events have a role")

# ---- death sorts last within its date -------------------------------------------------
same_day <- data.table(id = c("x", "x"), event = c("Death", "HCV"), date = D(c("2020-05-01", "2020-05-01")))
o <- order_terminal_last(same_day, r)
ok(identical(o$event, c("HCV", "Death")), "on the same date the death comes last, not alphabetically first")
two_days <- data.table(id = "x", event = c("Death", "HCV"), date = D(c("2020-05-02", "2020-05-01")))
ok(identical(order_terminal_last(two_days, r)$event, c("HCV", "Death")), "and dates still order first")

# ---- exact matching, 2 controls per case ---------------------------------------------
# seis celdas de 10 personas: (1970/1980/1990) x (F/M)
pop <- data.table(id = as.character(1:60),
                  birth_year = rep(c(1970, 1980, 1990), each = 20),
                  sex = rep(c("F", "M"), 30))
m <- match_controls(pop, cases = c("1", "3", "21"), by = c("birth_year", "sex"), ratio = 2)
ok(m$data[group == "case", .N] == 3, "every case found its controls in a cell with enough people")
ok(m$data[group == "control", .N] == 6, "two controls per case")
ok(!anyDuplicated(m$data$id), "nobody is used twice: no replacement")
ok(m$data[, uniqueN(stratum)] == 3 && all(m$data[, .N, by = stratum]$N == 3),
   "one stratum per case, with its two controls")
# the match is exact on the key
key <- merge(m$data, pop, by = "id")
ok(key[, uniqueN(paste(birth_year, sex)), by = stratum][, all(V1 == 1)], "exact on birth year and sex")
# a cell that cannot supply the controls drops the case
small <- data.table(id = c("a", "b", "c", "d"), birth_year = c(1950, 1950, 1960, 1960), sex = "F")
m2 <- match_controls(small, cases = c("a", "c"), ratio = 2)
ok(m2$data[group == "case", .N] == 0, "with one control per cell no case can be matched")
ok(m2$summary[grepl("dropped", step), rows] == 2, "and the loss is counted")
# an incomplete key cannot be matched exactly
na_pop <- rbind(pop, data.table(id = "99", birth_year = NA_real_, sex = "F"))
ok(match_controls(na_pop, cases = "99")$summary[grepl("complete matching key", step), rows] == 1,
   "a person without a complete key is set aside and counted")
# the seed makes it reproducible
ok(identical(match_controls(pop, c("1","3","21"), seed = 123)$data,
             match_controls(pop, c("1","3","21"), seed = 123)$data), "same seed, same controls")
ok(!identical(match_controls(pop, c("1","3","21"), seed = 123)$data$id,
              match_controls(pop, c("1","3","21"), seed = 7)$data$id), "a different seed draws differently")

# ---- the two modes --------------------------------------------------------------------
users_long <- data.table(id = c("1", "3", "11"), event = "Drug_use", date = D("2010-01-01"))
g_coh <- population_groups(pop, users_long, "Drug_use", mode = "cohort")
ok(all(g_coh$data$group == "drug user") && nrow(g_coh$data) == 60,
   "cohort mode: everyone in the data is a user, nobody is matched")
g_pop <- population_groups(pop, users_long, "Drug_use", mode = "population")
ok(identical(sort(unique(g_pop$data$group)), c("drug user", "no documented use")),
   "population mode: two groups")
ok(g_pop$data[group == "no documented use", .N] == 6, "with two controls per user")
ok(!any(g_pop$data[group == "no documented use", id] %in% g_pop$users),
   "a control never has a drug-use event")


# ---- one entry event for a cohort defined by several diagnoses -------------------------------
long_ce <- data.table(id = c("a", "a", "b", "c"),
                      event = c("Drug_use", "Poisoning", "Poisoning", "HCV"),
                      date = D(c("2010-03-01", "2008-05-01", "2015-01-01", "2012-01-01")))
ce <- cohort_entry(long_ce, c("Drug_use", "Poisoning"))
ok(nrow(ce) == 2 && all(ce$event == "Cohort_entry"), "one entry event per person, whatever found them")
ok(ce[id == "a", date] == D("2008-05-01"), "dated at the first of the cohort events, not the first listed")
ok(!"c" %in% ce$id, "somebody with only the disease does not enter the cohort")
ok(nrow(cohort_entry(long_ce, character(0))) == 0, "with no cohort definition there is no entry event")

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
