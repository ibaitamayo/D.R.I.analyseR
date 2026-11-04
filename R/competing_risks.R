# Time to event with competing risks, built on the events found by the engine.
#
# Nothing here is specific to hepatitis C or to antiviral treatment: you choose which
# event starts the clock, which event is the outcome, which one competes with it, which
# one censors, and (optionally) which event splits people into groups. Every label in
# the output carries the name of the event you chose, so the same code serves any
# intervention with the same data structure.
#
#   time_to_event()   one row per person: start, end, time, status (0 censored,
#                     1 outcome, 2 competing), group
#   event_summary()   counts, person-years and rates per status and group
#   cumulative_incidence()  Aalen-Johansen cumulative incidence for the outcome and the
#                     competing event, which is what must be read when events compete
#
# Landmark: follow-up can start a fixed time after the index event, classifying the
# group by what happened before that moment. It removes the immortal time of comparing
# treated and untreated from the index date.

CENSORED <- 0L; OUTCOME <- 1L; COMPETING <- 2L

# Adds n months to a date without extra packages (clamped to the last day of the month)
add_months <- function(date, n) {
  l <- as.POSIXlt(date); day <- l$mday
  l$mday <- 1L; l$mon <- l$mon + n
  first <- as.Date(l)
  last <- as.POSIXlt(first + 31); last$mday <- 0L       # last day of the target month
  as.Date(pmin(as.numeric(first + (day - 1)), as.numeric(as.Date(last))), origin = "1970-01-01")
}

# group_table: optional data.table (id, group) with groups worked out elsewhere, for
# exposures that are not "an event before the start" (for example, ever infected).
time_to_event <- function(long, index_event, outcome_event, competing_event = NULL,
                          censor_event = NULL, group_event = NULL, end_date = NULL,
                          landmark_months = 0, time_unit = c("years", "days"),
                          group_table = NULL, group_when = c("ever", "at_start")) {
  time_unit <- match.arg(time_unit); group_when <- match.arg(group_when)
  d <- as.data.table(long)
  # Named, because this is what happens when a cohort is restricted and the event chosen before
  # the restriction no longer happens to anybody in it.
  for (nm in c(index_event, outcome_event)) {
    lbl <- if (identical(nm, index_event)) "index event" else "outcome event"
    if (!nm %in% d$event) stop("Nobody in this cohort has the ", lbl, " (", nm, ").")
  }
  if (is.null(end_date)) end_date <- max(d$date, na.rm = TRUE) else end_date <- as.Date(end_date)

  date_of <- function(ev) if (!is.null(ev) && ev %in% d$event) d[event == ev, .(id, .fecha = date)] else NULL
  base <- d[event == index_event, .(id, index_date = date)]
  add_date <- function(b, ev, name) {
    f <- date_of(ev)
    if (is.null(f)) { b[, (name) := as.Date(NA)]; return(b) }
    setnames(f, ".fecha", name); merge(b, f, by = "id", all.x = TRUE)
  }
  base <- add_date(base, outcome_event, "outcome_date")
  base <- add_date(base, competing_event, "competing_date")
  base <- add_date(base, censor_event, "censor_date")
  base <- add_date(base, group_event, "group_date")

  # Landmark: the clock starts this many months after the index event
  base[, start := index_date]
  if (landmark_months > 0) {
    base[, start := add_months(index_date, landmark_months)]
    # whoever had the outcome, the competing event or the censoring before the landmark does not enter
    base <- base[(is.na(outcome_date) | outcome_date > start) &
                 (is.na(competing_date) | competing_date > start) &
                 (is.na(censor_date) | censor_date > start) & start <= end_date]
  }

  # How the group is assigned. "at_start" counts only what had already happened when follow-up
  # began, which is what keeps immortal time out; "ever" counts the event whenever it happened.
  # "ever" is the default because a treatment that always arrives years after the index event
  # would otherwise leave the treated group empty, but it buys that at a price: to be counted as
  # treated a person had to survive until the treatment, so the treated group cannot die before
  # it. Whoever reports an "ever" comparison has to say so.
  if (!is.null(group_table)) {
    g <- as.data.table(group_table)[, .(id, group = as.character(group))]
    base <- merge(base, g, by = "id", all.x = TRUE)
    base[is.na(group), group := "Unknown"]
  } else {
    has_group <- if (identical(group_when, "ever")) !is.na(base$group_date)
                 else !is.na(base$group_date) & base$group_date <= base$start
    base[, group := if (is.null(group_event)) "All"
                    else fifelse(has_group, paste0("With ", group_event), paste0("Without ", group_event))]
  }

  # End of follow-up: the first of outcome, competing event, censoring or end of period
  base[, admin_end := pmin(fifelse(is.na(censor_date), end_date, censor_date), end_date)]
  base[, `:=`(
    end = pmin(outcome_date, competing_date, admin_end, na.rm = TRUE),
    status = fcase(
      !is.na(outcome_date) & outcome_date <= pmin(competing_date, admin_end, na.rm = TRUE), OUTCOME,
      !is.na(competing_date) & competing_date <= admin_end, COMPETING,
      default = CENSORED))]
  base[end < start, end := start]
  base[, time := as.numeric(end - start) / if (time_unit == "years") 365.25 else 1]
  base[, .(id, group, start, end, time, status,
           status_label = factor(status, c(CENSORED, OUTCOME, COMPETING),
                                 c("Censored", outcome_event,
                                   if (is.null(competing_event)) "Competing" else competing_event)))]
}

event_summary <- function(tte, per = 1000) {
  s <- as.data.table(tte)[, .(persons = .N, person_years = sum(time),
                              outcome = sum(status == OUTCOME),
                              competing = sum(status == COMPETING),
                              censored = sum(status == CENSORED),
                              median_follow_up = median(time)), by = group]
  s[, `:=`(outcome_rate = per * outcome / person_years,
           competing_rate = per * competing / person_years)]
  s[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]
}

# Aalen-Johansen: the probability of having reached each event by each time, taking into
# account that the competing event prevents the outcome. One minus Kaplan-Meier would
# overestimate it.
cumulative_incidence <- function(tte) {
  d <- as.data.table(tte)
  # with a single group survfit returns no strata, so the group is repeated by hand
  group_of <- function(f, d) if (is.null(f$strata)) rep(unique(d$group), length(f$time))
                             else rep(sub("^group=", "", names(f$strata)), f$strata)
  if (!any(d$status == COMPETING)) {          # without competing events, Aalen-Johansen = 1 - Kaplan-Meier
    f <- survfit(Surv(time, status == OUTCOME) ~ group, data = d)
    out <- data.table(time = f$time, cif = 1 - f$surv, event = levels(d$status_label)[2],
                      group = group_of(f, d))
    return(out[order(group, time)])
  }
  d[, st := factor(status, c(CENSORED, OUTCOME, COMPETING), c("censor", "outcome", "competing"))]
  f <- survfit(Surv(time, st) ~ group, data = d)
  p <- as.data.table(f$pstate); setnames(p, f$states)
  p[, `:=`(time = f$time, group = group_of(f, d))]
  long_cif <- melt(p, id.vars = c("time", "group"), variable.name = "state", value.name = "cif")
  labels <- c(outcome = levels(d$status_label)[2], competing = levels(d$status_label)[3])
  long_cif <- long_cif[state %in% c("outcome", "competing")]
  long_cif[, event := labels[as.character(state)]][, state := NULL]
  long_cif[order(group, event, time)]
}
