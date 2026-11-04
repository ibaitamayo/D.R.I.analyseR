# Outcome module tests with synthetic cases whose answer is known
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date
L <- function(...) rbindlist(list(...))
p <- function(id, event, date) data.table(id = id, event = event, date = D(date))

# Caso 1: 3 personas con VHC en 2015, una tratada en 2016, una muerta en 2017
d1 <- L(p("a","VHC","2015-03-01"), p("b","VHC","2015-06-01"), p("c","VHC","2015-09-01"),
        p("a","Tto","2016-05-01"), p("c","Muerte","2017-02-01"))
cu <- cumulative_uptake(d1, "VHC", "Tto", "Muerte", years = 2015:2018, end_date = D("2018-12-31"))
ok(cu[year == 2015, with_index_event] == 3 && cu[year == 2015, with_outcome] == 0, "2015: 3 with index event, 0 treated")
ok(cu[year == 2016, with_outcome] == 1 && cu[year == 2016, pct_with_outcome] == 33.3, "2016: 1 of 3 treated = 33.3%")
ok(cu[year == 2017, with_index_event] == 2, "2017: a person who dies leaves the denominator")
ok(cu[year == 2018, pct_with_outcome] == 50, "2018: 1 of 2 = 50%")

# Caso 2: personas-año. Una persona desde el 1-1-2015 hasta el 31-12-2016 = 2 años
d2 <- L(p("x","VHC","2015-01-01"))
fu <- follow_up(d2, "VHC", NULL, end_date = D("2016-12-31"))
py <- person_years_by_year(fu, 2015:2016)
ok(abs(sum(py$person_years) - 2) < 0.01, "Person-years: two full years add up to 2")
ok(abs(py[year == 2015, person_years] - 1) < 0.01, "Person-years: 1 per calendar year")

# Caso 3: media persona-año (entra el 1 de julio)
fu3 <- follow_up(L(p("y","VHC","2015-07-01")), "VHC", NULL, end_date = D("2015-12-31"))
ok(abs(person_years_by_year(fu3, 2015)$person_years - 0.504) < 0.01, "Entry mid-year ≈ 0.5 person-years")

# Caso 4: tasa. 1 muerte en 2016 con 2 personas-año = 500 por 1.000
d4 <- L(p("m","VHC","2015-01-01"), p("n","VHC","2015-01-01"), p("m","Muerte","2016-12-31"))
r <- rates_by_year(d4, "VHC", "Muerte", NULL, per = 1000, years = 2015:2016, end_date = D("2016-12-31"))
ok(r[year == 2015, events] == 0 && abs(r[year == 2015, person_years] - 2) < 0.02, "2015: 0 events, 2 person-years")
ok(r[year == 2016, events] == 1, "2016: 1 event")
ok(r[year == 2016, ci_low] < r[year == 2016, rate] & r[year == 2016, rate] < r[year == 2016, ci_high], "The confidence interval contains the rate")

# Caso 5: el desenlace detiene el seguimiento (muerte)
fu5 <- follow_up(L(p("z","VHC","2015-01-01")), "VHC", NULL, end_date = D("2016-12-31"))
py5 <- person_years_by_year(fu5, 2015:2016, data.table(id = "z", outcome_date = D("2015-12-31")), stop_at_outcome = TRUE)
ok(abs(py5[year == 2016, person_years]) < 0.01, "No follow-up accrues after death")

# Caso 6: eventos por año, con años sin eventos
d6 <- L(p("a","Vacuna","2015-01-01"), p("b","Vacuna","2017-01-01"))
e <- events_by_year(d6, years = 2015:2017)
ok(nrow(e) == 3 && e[year == 2016, persons] == 0, "Years with no events appear as 0")
ok(e[year == 2015, persons] == 1 && e[year == 2017, persons] == 1, "Correct count per year")

# Caso 7: sin personas con el evento índice no revienta
ok(nrow(cumulative_uptake(d6, "NoExiste", "Vacuna")) == 0, "A missing index event returns an empty table")

# ---- a cohort where nobody has the index event: empty, but still a table ----------------
# This is what a cohort filter produces: the event was chosen before the restriction and no
# longer happens to anybody. A table with no columns would fail further downstream, where the
# message no longer says what went wrong.
none <- data.table(id = c("1", "2"), event = "Other", date = as.Date("2015-01-01"))
u0 <- cumulative_uptake(none, "Nobody_has_this", "Other")
ok(nrow(u0) == 0 && all(c("year", "with_index_event", "with_outcome", "without_outcome",
                          "pct_with_outcome") %in% names(u0)),
   "cumulative_uptake with nobody at the index event returns an empty table that keeps its columns")
r0 <- rates_by_year(none, "Nobody_has_this", "Other")
ok(nrow(r0) == 0 && all(c("year", "person_years", "events", "rate") %in% names(r0)),
   "and so does rates_by_year")
ok(!inherits(try(melt(u0[, .(year, with_outcome, without_outcome)], id.vars = "year"), silent = TRUE), "try-error"),
   "so the chart that reshapes it does not fail either")

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
