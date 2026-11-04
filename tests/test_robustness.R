# What happens when there is almost nothing: one person, one event, a group with no events,
# a cohort a filter emptied (Rscript tests/test_robustness.R).
#
# None of these is a valid analysis, and that is the point: the package must say so and keep
# standing, because an empty cohort is what a filter produces and a user will produce it.
suppressMessages({library(data.table); library(survival)})
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date
# survives: runs without an error, whatever it returns
surv <- function(expr, txt) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { cat("     -> ", conditionMessage(e), "\n"); FALSE })
  ok(r, txt)
}
# speaks: fails, but with a sentence a person can read, not an assertion dump
speaks <- function(expr, txt) {
  r <- tryCatch({ force(expr); FALSE }, error = function(e) grepl("[a-z] [a-z]", conditionMessage(e)) &&
                   !grepl("is not TRUE|subscript|undefined columns|objeto|object '", conditionMessage(e)))
  ok(isTRUE(r), txt)
}

empty <- data.table(id = character(), event = character(), date = as.Date(character()))
one   <- data.table(id = "1", event = "A", date = D("2015-06-01"))
two   <- data.table(id = c("1", "1", "2"), event = c("A", "B", "A"),
                    date = D(c("2015-01-01", "2016-01-01", "2015-07-01")))

cat("\n-- an empty long table --\n")
surv(follow_up(empty, "A"), "follow_up")
surv(cumulative_uptake(empty, "A", "B"), "cumulative_uptake")
surv(rates_by_year(empty, "A", "B"), "rates_by_year")
surv(events_by_year(empty), "events_by_year")
surv(cohort_entry(empty, "A"), "cohort_entry")
surv(patient_events(data.table(id = character())), "patient_events")
speaks(time_to_event(empty, "A", "B"), "time_to_event says which event is missing")

cat("\n-- one person, one event --\n")
surv(cumulative_uptake(one, "A", "B"), "cumulative_uptake with an outcome nobody has")
surv(rates_by_year(one, "A", "B"), "rates_by_year with an outcome nobody has")
surv(events_by_year(one), "events_by_year")
surv(cohort_entry(one, "A"), "cohort_entry")
speaks(time_to_event(one, "A", "B"), "time_to_event with an outcome nobody has")
u <- cumulative_uptake(one, "A", "B")
ok(all(c("year", "with_index_event", "with_outcome", "without_outcome") %in% names(u)),
   "and the empty cascade still has its columns, so the chart can reshape it")

cat("\n-- two people, an outcome one of them reaches --\n")
tte <- time_to_event(two, "A", "B")
surv(event_summary(tte), "event_summary")
surv(cumulative_incidence(tte), "cumulative_incidence with one event and no competing event")
surv(fit_models(cbind(tte, sex = factor(c("F", "M")))[, status := c(1L, 0L)][], "sex"),
     "fit_models with two people")
surv(model_warnings(cbind(tte, g = factor(c("a", "b")))[, status := 0L][], "g", 1L),
     "model_warnings with no outcome at all")
ok(grepl("No outcome events", model_warnings(copy(tte)[, status := 0L][], character(0), 1L)[1]),
   "and it says there is nothing to estimate instead of estimating it")

cat("\n-- a group with nobody in it --\n")
g <- data.table(id = c("1", "2"), group = c("x", "y"))
surv(by_group(two, g[0], function(x) cumulative_uptake(x, "A", "B")), "by_group with an empty group table")
surv(by_group(two, g, function(x) cumulative_uptake(x, "A", "B")), "by_group with one person per group")
surv(by_group(two, g, function(x) rates_by_year(x, "A", "B")), "by_group of rates")

cat("\n-- the funnel and the roles --\n")
roles <- data.table(event = c("A", "B"), role = c("cohort", "disease"))
surv(phase_funnel(empty, roles), "phase_funnel on nothing")
surv(phase_funnel(one, roles, disease = "B"), "phase_funnel with a disease nobody has")
surv(phase_funnel(two, roles, disease = "B"), "phase_funnel with one case")
surv(event_roles(data.table(event = character(), rule = character(), role = character())),
     "event_roles with no definition")

cat("\n-- definitions that find nothing --\n")
dd <- data.frame(ev = "Z", ty = "CIE-10-ES", co = "Z99.9")
pd <- prepare_definition(dd, "ev", "ty", "co")
dxd <- data.table(id = "1", system = "ICD10", cod = "B182", date = D("2015-01-01"))
surv(find_events(dx = dxd, definition = pd), "find_events when no code matches anything")
r <- find_events(dx = dxd, definition = pd)
ok(nrow(r$long) == 0, "and it returns no events rather than inventing one")
surv(find_events(dx = dxd[0], definition = pd), "find_events on an empty diagnosis table")

cat("\n-- and none of it warns --\n")
w <- character(0)
withCallingHandlers({
  find_events(dx = dxd, definition = pd); find_events(dx = dxd[0], definition = pd)
  cumulative_uptake(one, "A", "B"); rates_by_year(one, "A", "B"); events_by_year(one)
}, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
ok(length(w) == 0, paste0("an empty search produces no warnings",
   if (length(w)) paste0(" (got: ", paste(unique(w), collapse = " | "), ")")))

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
