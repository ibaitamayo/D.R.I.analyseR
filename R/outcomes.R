# Outcome measures over calendar time, from the events found by the engine.
#
# The engine returns one row per person and event, with the FIRST date of that event
# (id, event, date). Everything here is built on that, so repeated events (several
# vaccine doses, several admissions) are not counted: only the first one per person.
#
# Three views, matching what registry reports usually show:
#   cumulative_uptake()  cumulative people with an index event and how many of them
#                        reached an outcome, by year (cascade / "treatment uptake")
#   events_by_year()     number of people reaching each event, by year (calendar table)
#   rates_by_year()      events and rate per person-years at risk, by year, with
#                        exact Poisson confidence intervals
# Column names are English so the output can be used as is in any country.

year_of <- function(x) as.integer(format(x, "%Y"))

# Follow-up of each person: from the index event to the exit event (death, deregistration)
# or to the end of the period, whichever comes first.
follow_up <- function(long, index_event, exit_event = NULL, end_date = NULL) {
  d <- as.data.table(long)
  idx <- d[event == index_event, .(id, start = date)]
  if (!nrow(idx)) return(data.table(id = character(), start = as.Date(character()), end = as.Date(character()), exited = logical()))
  end_date <- if (is.null(end_date)) max(d$date, na.rm = TRUE) else as.Date(end_date)
  out <- if (!is.null(exit_event) && exit_event %in% d$event) d[event == exit_event, .(id, exit = date)] else NULL
  fu <- if (is.null(out)) copy(idx)[, exit := as.Date(NA)] else merge(idx, out, by = "id", all.x = TRUE)
  fu[, `:=`(end = pmin(fifelse(is.na(exit), end_date, exit), end_date), exited = !is.na(exit) & exit <= end_date)]
  fu[end < start, end := start]          # exit before index: zero-length follow-up
  fu[, .(id, start, end, exited)]
}

# Cascade by calendar year: people whose index event happened up to 31 December of that
# year and who have not left the cohort by then, and how many of those reached the outcome.
# Years before the first person and after the last one are not data, they are the span of the
# oldest record in the table. A chart that opens in 1910 with an empty axis says nothing, so the
# empty years at either end are dropped; empty years in the middle are kept, because a gap is a
# finding.
trim_empty_years <- function(d, denominator) {
  if (!nrow(d) || !denominator %in% names(d)) return(d)
  has <- which(!is.na(d[[denominator]]) & d[[denominator]] > 0)
  if (!length(has)) return(d[0])
  d[min(has):max(has)]
}

# An empty result is still a result with columns: nobody in this cohort reached the index event.
# Returning a table with no columns at all turns that into an error three calls away, where the
# message no longer says what happened.
empty_uptake <- data.table(year = integer(), with_index_event = integer(), with_outcome = integer(),
                           without_outcome = integer(), pct_with_outcome = numeric())
empty_rates <- data.table(year = integer(), person_years = numeric(), persons_at_risk = integer(),
                          events = integer(), rate = numeric(), ci_low = numeric(), ci_high = numeric())

cumulative_uptake <- function(long, index_event, outcome_event, exit_event = NULL,
                              years = NULL, end_date = NULL) {
  d <- as.data.table(long)
  fu <- follow_up(d, index_event, exit_event, end_date)
  if (!nrow(fu)) return(copy(empty_uptake))
  out <- d[event == outcome_event, .(id, outcome_date = date)]
  fu <- merge(fu, out, by = "id", all.x = TRUE)
  if (is.null(years)) years <- seq(min(year_of(fu$start)), max(year_of(fu$end)))
  rbindlist(lapply(years, function(y) {
    cut_off <- as.Date(paste0(y, "-12-31"))
    in_cohort <- fu[start <= cut_off & (!exited | end > cut_off)]
    n <- nrow(in_cohort); k <- in_cohort[!is.na(outcome_date) & outcome_date <= cut_off, .N]
    data.table(year = y, with_index_event = n, with_outcome = k, without_outcome = n - k,
               pct_with_outcome = if (n > 0) round(100 * k / n, 1) else NA_real_)
  })) |> trim_empty_years("with_index_event")
}

# Number of people reaching each event, by calendar year (first event per person).
events_by_year <- function(long, events = NULL, years = NULL) {
  d <- as.data.table(long)
  if (!is.null(events)) d <- d[event %in% events]
  if (!nrow(d)) return(data.table())
  d <- d[, .(persons = uniqueN(id)), by = .(year = year_of(date), event = event)]
  if (is.null(years)) years <- seq(min(d$year), max(d$year))
  grid <- CJ(year = years, event = unique(d$event), unique = TRUE)
  g <- merge(grid, d, by = c("year", "event"), all.x = TRUE)[is.na(persons), persons := 0L]
  # Trimmed over all the events together, so every line on the chart shares one axis.
  keep <- g[, .(n = sum(persons)), by = year][n > 0, range(year)]
  g[year >= keep[1] & year <= keep[2]][order(event, year)]
}

# Person-years in each calendar year, counting from the index event to the exit
# (and to the outcome, if the outcome stops follow-up, as with death).
person_years_by_year <- function(fu, years, outcome = NULL, stop_at_outcome = TRUE) {
  f <- copy(fu)
  if (!is.null(outcome) && stop_at_outcome) {
    f <- merge(f, outcome, by = "id", all.x = TRUE)
    f[!is.na(outcome_date) & outcome_date < end, end := outcome_date]
  }
  rbindlist(lapply(years, function(y) {
    from <- as.Date(paste0(y, "-01-01")); to <- as.Date(paste0(y, "-12-31"))
    slice <- f[start <= to & end >= from]
    py <- slice[, sum(as.numeric(pmin(end, to) - pmax(start, from)) + 1) / 365.25]
    data.table(year = y, person_years = py, persons_at_risk = nrow(slice))
  }))
}

# Events and rate per calendar year, with exact Poisson confidence intervals.
rates_by_year <- function(long, index_event, outcome_event, exit_event = NULL,
                          per = 1000, years = NULL, end_date = NULL, stop_at_outcome = TRUE) {
  d <- as.data.table(long)
  fu <- follow_up(d, index_event, exit_event, end_date)
  if (!nrow(fu)) return(copy(empty_rates))
  out <- d[event == outcome_event, .(id, outcome_date = date)][id %in% fu$id]
  if (is.null(years)) years <- seq(min(year_of(fu$start)), max(year_of(fu$end)))
  py <- person_years_by_year(fu, years, out, stop_at_outcome)
  ev <- out[, .(events = .N), by = .(year = year_of(outcome_date))]
  r <- merge(py, ev, by = "year", all.x = TRUE)[is.na(events), events := 0L]
  r[, `:=`(rate = per * events / person_years,
           ci_low = per * qchisq(0.025, 2 * events) / (2 * person_years),
           ci_high = per * qchisq(0.975, 2 * (events + 1)) / (2 * person_years))]
  r[person_years == 0, `:=`(rate = NA_real_, ci_low = NA_real_, ci_high = NA_real_)]
  r <- trim_empty_years(r, "person_years")
  r[, lapply(.SD, function(x) if (is.numeric(x) && !is.integer(x)) round(x, 2) else x)]
}

# Applies one of the views above to each group and binds the results with a group column.
# groups is a table (id, group); without it everything is one group, which keeps the callers
# identical whether or not a comparison was asked for.
by_group <- function(long, groups, f) {
  if (is.null(groups) || !nrow(groups)) return(f(long)[, group := "All"][])
  g <- as.data.table(groups)
  parts <- lapply(split(as.character(g$id), g$group), function(ids) f(long[as.character(id) %in% ids]))
  rbindlist(Map(function(x, nm) if (nrow(x)) x[, group := nm][] else NULL, parts, names(parts)),
            use.names = TRUE)
}

# Two groups from an event: those who ever reach it and those who do not, among the people who
# have the index event. "Ever", not "before time zero": for a treatment that is the question the
# cascade asks, and the landmark of tab 6 is where immortal time is dealt with.
group_from_event <- function(long, split_event, index_event) {
  l <- as.data.table(long)
  # Local copies: a parameter named like a column is masked inside i, and ..x is not allowed there.
  idx <- index_event; spl <- split_event
  ids <- unique(l[event == idx, as.character(id)])
  if (!length(ids)) return(NULL)
  with <- unique(l[event == spl, as.character(id)])
  data.table(id = ids, group = fifelse(ids %in% with, paste("With", spl), paste("Without", spl)))
}
