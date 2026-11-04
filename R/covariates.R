# Patient-level table (T3) and treatment eras.
#
# T3 has one row per person: sex, birth, death and the cause of death. Age is NOT a column of
# the table: a stored age would be the age at extraction, not at the time zero of the analysis,
# so what is asked for is the birth date (or year) and the age is derived at the reference date
# the analysis uses.
#
# Death reaches the rest of the app as ordinary events, so nothing downstream changes:
#   Death                 the fact, from the death date. Always, if there is a date.
#   <cause definitions>   named by whichever cause-of-death definition the cause code matches.
#   Death_other_cause     a cause code is present and matches no cause definition.
#   Death_cause_unregistered   the death is recorded and there is no cause code.
# The last two are structural, not clinical: in most sources the cause only exists for deaths
# during an admission, so "not that cause" and "cause not known" must not be the same node.

RESERVED_EVENTS <- c("Birth", "Cohort_entry", "Death", "Death_other_cause",
                     "Death_cause_unregistered", "End_of_follow_up")

# Age at a reference date. With a full birth date the age is exact; with only a year it is the
# difference of years, which is approximate by up to one year and must be declared as such.
# Returns NA, to be counted and not silently turned into a zero, when the birth is not plausible.
age_at <- function(birth, birth_year = NA, reference, min_birth_year = 1900) {
  n <- max(length(birth), length(birth_year), length(reference))
  fit <- function(x) if (length(x) == n) x else rep(x, length.out = n)
  birth <- fit(as.Date(birth)); birth_year <- fit(suppressWarnings(as.numeric(birth_year)))
  reference <- fit(as.Date(reference))
  ry <- as.integer(format(reference, "%Y"))
  has_date <- !is.na(birth)
  by <- ifelse(has_date, as.integer(format(birth, "%Y")), birth_year)
  age <- ry - by
  if (any(has_date, na.rm = TRUE)) {
    before <- format(reference, "%m%d") < format(birth, "%m%d")   # birthday not yet reached
    age[has_date] <- age[has_date] - as.integer(before[has_date])
  }
  bad <- is.na(by) | is.na(ry) | by < min_birth_year | by > ry |
    (has_date & !is.na(reference) & birth > reference)
  age[bad] <- NA
  as.numeric(age)
}

age_band <- function(age, breaks) {
  if (is.null(breaks) || !length(breaks)) return(rep(NA_character_, length(age)))
  b <- sort(unique(c(-Inf, breaks, Inf)))
  as.character(cut(age, breaks = b, right = FALSE, dig.lab = 4))
}

# Eras: one row per era, with the date it starts. Intervals are [start_i, start_i+1).
# An empty or absent table means there is no era variable; nothing is invented.
prepare_eras <- function(d, col_date, col_name) {
  if (is.null(d)) return(list(data = NULL, warnings = character(0)))
  d <- as.data.table(d)
  if (!nrow(d)) return(list(data = NULL, warnings = character(0)))
  e <- data.table(start = as_date(d[[col_date]]), name = trimws(as.character(d[[col_name]])))
  w <- character(0)
  bad <- e[is.na(start) | is.na(name) | name == "", .N]
  if (bad) {
    w <- c(w, paste(bad, "era row(s) without a readable date or without a name were ignored."))
    e <- e[!is.na(start) & !is.na(name) & name != ""]
  }
  if (!nrow(e)) return(list(data = NULL, warnings = w))
  setorder(e, start)
  if (anyDuplicated(e$start)) {
    w <- c(w, "Two eras start on the same date; only the first one is kept.")
    e <- unique(e, by = "start")
  }
  if (any(format(e$start, "%m%d") != "0101"))
    w <- c(w, paste("An era starts in the middle of a calendar year. Rates by year aggregate whole",
                    "years into eras, so a boundary that is not 1 January splits a year between two eras."))
  e[, end := c(start[-1] - 1, as.Date(NA))]
  list(data = e[, .(start, end, name)], warnings = w)
}

era_at <- function(reference, eras) {
  if (is.null(eras) || !nrow(eras)) return(rep(NA_character_, length(reference)))
  reference <- as.Date(reference)
  i <- findInterval(as.numeric(reference), as.numeric(eras$start))
  out <- ifelse(i == 0L, paste("Before", eras$name[1]), eras$name[pmax(i, 1L)])
  out[is.na(reference)] <- NA_character_
  out
}

# One row per person. Every field except the identifier is optional: what is absent is simply
# not derived, and the summary says how many rows were affected.
prepare_patients <- function(d, col_id, col_birth = NULL, col_birth_year = NULL, col_sex = NULL,
                             col_death = NULL, col_death_code = NULL, col_death_type = NULL,
                             col_last_contact = NULL, extra_cols = NULL, min_birth_year = 1900) {
  d <- as.data.table(d)
  pick <- function(col) if (!is.null(col) && length(col) == 1L && !is.na(col) && nzchar(col) && col %in% names(d)) d[[col]] else NULL
  n0 <- nrow(d)
  out <- data.table(id = as.character(d[[col_id]]))
  b <- pick(col_birth);        out[, birth := if (is.null(b)) as.Date(NA) else as_date(b)]
  by <- pick(col_birth_year);  out[, birth_year := if (is.null(by)) NA_real_ else suppressWarnings(as.numeric(by))]
  sx <- pick(col_sex);         out[, sex := if (is.null(sx)) NA_character_ else trimws(as.character(sx))]
  dd <- pick(col_death);       out[, death := if (is.null(dd)) as.Date(NA) else as_date(dd)]
  dc <- pick(col_death_code);  out[, death_code := if (is.null(dc)) NA_character_ else trimws(as.character(dc))]
  dt <- pick(col_death_type);  out[, death_type := if (is.null(dt)) NA_character_ else trimws(as.character(dt))]
  lc <- pick(col_last_contact); out[, last_contact := if (is.null(lc)) as.Date(NA) else as_date(lc)]
  for (col in intersect(extra_cols, names(d))) out[[col]] <- trimws(as.character(d[[col]]))

  no_id <- out[is.na(id) | id == "", .N]
  out <- out[!is.na(id) & id != ""]
  dup <- anyDuplicated(out$id)
  n_dup <- if (dup) sum(duplicated(out$id)) else 0L
  if (dup) out <- unique(out, by = "id")          # first row kept; never averaged or picked at random
  out[death_code == "", death_code := NA_character_]
  # A cause code without its coding system cannot be compared with anything, so it is dropped
  # here rather than compared against the wrong vocabulary later.
  no_cause_type <- out[!is.na(death_code) & (is.na(death_type) | death_type == ""), .N]
  out[!is.na(death_code) & (is.na(death_type) | death_type == ""), death_code := NA_character_]
  implausible <- out[!is.na(death) & !is.na(birth) & birth > death, .N]
  # Plausibility of the birth itself, not of an age at some arbitrary date: testing it against a
  # fixed reference year would flag everyone born after that year.
  by_all <- fifelse(!is.na(out$birth), as.numeric(format(out$birth, "%Y")), out$birth_year)
  bad_birth <- sum(!is.na(by_all) & (by_all < min_birth_year | by_all > as.numeric(format(Sys.Date(), "%Y"))))

  list(data = out,
       approximate_age = is.null(b) && !is.null(by),
       summary = data.table(
         step = c("Rows read", "Missing identifier", "Repeated identifier (first row kept)",
                  "Implausible birth (not used for age)", "Cause code without a coding system (not used)",
                  "Birth after death", "Persons used"),
         rows = c(n0, no_id, n_dup, bad_birth, no_cause_type, implausible, nrow(out))))
}

# Death as events, ready to be appended to the long table of the engine.
# cause_codes is the prepared cause-of-death definition (event, system, cod); without it there
# is no cause, so every recorded death becomes Death_cause_unregistered.
patient_events <- function(patients, cause_codes = NULL) {
  p <- as.data.table(patients)
  empty <- data.table(id = character(), event = character(), date = as.Date(character()))
  if (!nrow(p)) return(empty)
  out <- list()
  # Birth as an event, so a trajectory can start where the person starts. With only a year, the
  # first of July is used: the midpoint of the year is the least wrong single day for it, and
  # nothing downstream measures anything from birth with a precision this cannot carry.
  if (any(c("birth", "birth_year") %in% names(p))) {
    bd <- if ("birth" %in% names(p)) p$birth else as.Date(rep(NA, nrow(p)))
    if ("birth_year" %in% names(p)) {
      # Only a four-digit year can become a date. A sentinel year outside that range leaves the
      # person without a birth event, which is what it should do: it is not a birth.
      only_year <- is.na(bd) & !is.na(p$birth_year) & p$birth_year >= 1000 & p$birth_year <= 9999
      bd[only_year] <- as.Date(sprintf("%04d-07-01", as.integer(p$birth_year[only_year])))
    }
    out$birth <- data.table(id = p$id, event = "Birth", date = bd)[!is.na(date)]
  }
  if ("last_contact" %in% names(p))
    out$follow <- p[!is.na(last_contact), .(id, event = "End_of_follow_up", date = last_contact)]
  dead <- p[!is.na(death)]
  if (!nrow(dead)) return(if (length(out)) rbindlist(out, use.names = TRUE) else empty)
  out$death <- dead[, .(id, event = "Death", date = death)]
  coded <- dead[!is.na(death_code)]
  matched <- empty
  if (nrow(coded) && !is.null(cause_codes) && nrow(cause_codes)) {
    src <- coded[, .(id, system = coding_system(death_type), cod = normalise_code(death_code), date = death)]
    src <- src[!is.na(system)]
    # A cause of death is a single coded fact: only codes that establish it are used, and the
    # date is the death itself, so the dating codes have nothing to contribute here.
    if (nrow(src)) matched <- match_codes(src, cause_codes)[use == "case", .(id, event, date)]
  }
  out$cause <- matched
  # Coded but matching no cause definition, and recorded without a cause: different nodes,
  # because collapsing them would hide how much of the cause is simply missing.
  out$other <- coded[!id %in% matched$id, .(id, event = "Death_other_cause", date = death)]
  out$unknown <- dead[is.na(death_code), .(id, event = "Death_cause_unregistered", date = death)]
  res <- rbindlist(out[vapply(out, nrow, 0L) > 0L], use.names = TRUE)
  # Death_other_cause is the competing event: death from any cause not associated with the
  # disease under study. What follows is only its composition, for the methods section; nothing
  # is inferred from it and no code here is turned into an event of its own.
  setattr(res, "other_causes",
          coded[!id %in% matched$id, .(persons = .N), by = .(death_type, death_code)][order(-persons)])
  res
}

# Role per event, for colouring and for choosing what the process map shows. The role is read
# from the definition table, where it is declared; the events the patient table generates are
# mortality by construction.
event_roles <- function(rules = NULL, cause_events = character(0)) {
  fixed <- data.table(event = c("Birth", "Cohort_entry"), role = c("birth", "cohort"))
  deaths <- data.table(event = unique(c(setdiff(RESERVED_EVENTS, fixed$event), cause_events)),
                       role = "mortality")
  deaths <- rbind(fixed, deaths)
  base <- if (is.null(rules) || !nrow(rules)) data.table(event = character(), role = character())
          else as.data.table(rules)[, .(event, role)]
  unique(rbind(base[!event %in% deaths$event], deaths), by = "event")
}

# Death is absorbing: nothing follows it. The event log breaks ties within one date
# alphabetically, so without this a diagnosis recorded on the day of death could be drawn
# after the death. Returns the long table with a sort key that puts mortality last.
order_terminal_last <- function(long, roles) {
  d <- as.data.table(long)
  r <- as.data.table(roles)
  d[, .terminal := as.integer(event %in% r[role == "mortality", event])]
  setorder(d, id, date, .terminal, event)
  d[, .terminal := NULL][]
}

# Exact matching, 2 controls per case, without replacement: equivalent to MatchIt with an exact
# constraint and no distance, and much faster, because with no distance term the only thing that
# happens inside a cell is a random draw. The draw is random, so the seed is fixed and declared.
# A case whose cell cannot supply the full ratio is dropped: those are extreme cells, and keeping
# them with fewer controls would break the matching it is there to provide.
match_controls <- function(patients, cases, by = c("birth_year", "sex"), ratio = 2L, seed = 123L) {
  p <- as.data.table(patients)
  by <- intersect(by, names(p))
  stopifnot(length(by) > 0L, "id" %in% names(p), ratio >= 1L)
  d <- data.table(id = as.character(p$id))
  for (b in by) d[[b]] <- as.character(p[[b]])
  n_all <- nrow(d)
  incomplete <- d[, Reduce(`|`, lapply(.SD, function(x) is.na(x) | x == "")), .SDcols = by]
  d <- d[!incomplete]
  d[, cell := do.call(paste, c(.SD, sep = "\r")), .SDcols = by]
  d[, is_case := id %in% cases]
  take <- function(x, n) x[sample.int(length(x), n)]   # never sample(x, n): x of length 1 is a trap
  set.seed(seed)
  out <- d[, {
    cs <- id[is_case]; ct <- id[!is_case]
    k <- min(length(cs), length(ct) %/% ratio)
    if (k == 0L) .(id = character(), group = character(), stratum = character())
    else {
      pc <- if (length(cs) == k) cs else take(cs, k)
      pt <- take(ct, k * ratio)
      .(id = c(pc, pt), group = rep(c("case", "control"), c(k, k * ratio)),
        stratum = c(pc, rep(pc, each = ratio)))
    }
  }, by = cell][, .(id, group, stratum)]
  n_cases <- length(unique(cases))
  list(data = out,
       summary = data.table(
         step = c("Persons read", "Without a complete matching key", "Cases", "Cases matched",
                  "Cases dropped (cell could not supply the controls)", "Controls selected"),
         rows = c(n_all, sum(incomplete), n_cases, out[group == "case", .N],
                  n_cases - out[group == "case", .N], out[group == "control", .N])),
       keys = by, ratio = ratio, seed = seed)
}

# Who is a drug user, and the groups the analysis compares.
#   mode "cohort"      every person in the data is a user; the comparison is treated vs untreated
#   mode "population"  users are identified by the cohort-role events and matched to controls
population_groups <- function(patients, long, cohort_events, mode = c("cohort", "population"),
                              by = c("birth_year", "sex"), ratio = 2L, seed = 123L) {
  mode <- match.arg(mode)
  p <- as.data.table(patients); l <- as.data.table(long)
  users <- unique(l[event %in% cohort_events, as.character(id)])
  if (mode == "cohort")
    return(list(data = data.table(id = as.character(p$id), group = "drug user", stratum = NA_character_),
                summary = data.table(step = c("Persons read", "With a drug-use event"),
                                     rows = c(nrow(p), length(intersect(users, as.character(p$id))))),
                mode = mode, users = users))
  m <- match_controls(p, users, by = by, ratio = ratio, seed = seed)
  m$data[, group := fifelse(group == "case", "drug user", "no documented use")]
  c(m, list(mode = mode, users = users))
}

reserved_event_warnings <- function(definition_events) {
  clash <- intersect(definition_events, RESERVED_EVENTS)
  if (!length(clash)) return(character(0))
  paste0("These event names are reserved for the patient table and must be renamed in the ",
         "definitions: ", paste(clash, collapse = ", "), ".")
}

# The funnel of the six phases, as it stands after a search. Each step is a subset of the one
# above it, so the percentages read down the page and the table can be checked by eye. Nothing
# here is a new calculation: it is the same long table of events, counted by person.
#
# Treatments and causes are shown as factors with their levels, not as a flat list, because that
# is what they are: one eradicating treatment with two levels, one disease-associated death with
# three. A level is counted on the same base as its header, so the columns add up.
# One entry event for the whole population definition. A cohort found through seven different
# diagnoses is still one cohort, and every analysis asks when the person entered it, not through
# which code: that belongs in the funnel, which still breaks it down. The date is the first of
# them, which is the date the person became part of the population.
cohort_entry <- function(long, cohort_events, name = "Cohort_entry") {
  l <- as.data.table(long)
  empty <- data.table(id = character(), event = character(), date = as.Date(character()))
  if (!length(cohort_events) || !nrow(l)) return(empty)
  e <- l[event %in% cohort_events & !is.na(date), .(date = min(date)), by = id]
  if (!nrow(e)) return(empty)
  e[, .(id = as.character(id), event = name, date)]
}

phase_funnel <- function(long, roles, disease = NULL, n_persons = NULL, groups = NULL,
                         cause_events = character(0), rules = NULL) {
  l <- as.data.table(long); r <- as.data.table(roles)
  who <- function(ev) unique(l[event %in% ev, as.character(id)])
  rows <- list()
  add <- function(phase, step, ids, base = NULL, level = 0L) rows[[length(rows) + 1L]] <<- data.table(
    phase = phase, step = paste0(strrep("\u2003", level), step), persons = length(ids),
    of_previous = if (is.null(base) || !length(base)) NA_real_ else round(100 * length(ids) / length(base), 1))

  everyone <- if (!is.null(n_persons)) seq_len(n_persons) else unique(as.character(l$id))
  add("0 \u00b7 Data", "Persons in the data", everyone)

  # 1 - who is in the population, and how they were found
  coh_ev <- r[role == "cohort", event]
  cohort <- who(coh_ev)
  if (!is.null(groups) && identical(groups$mode, "population")) {
    g <- as.data.table(groups$data)
    add("1 \u00b7 Cohort", "People who use drugs (matched)", g[group == "drug user", id], everyone)
    add("1 \u00b7 Cohort", "Matched controls, no documented use", g[group == "no documented use", id], everyone)
    base <- g[group == "drug user", id]
  } else {
    add("1 \u00b7 Cohort", "People who use drugs", cohort, everyone)
    base <- if (length(cohort)) cohort else everyone
  }
  if (!is.null(rules) && length(coh_ev)) {
    ru <- as.data.table(rules)
    by_dx <- intersect(who(ru[event %in% coh_ev & has_dx, event]), base)
    by_rx <- intersect(who(ru[event %in% coh_ev & has_drug, event]), base)
    add("1 \u00b7 Cohort", "found by a diagnosis", setdiff(by_dx, by_rx), base, 1L)
    add("1 \u00b7 Cohort", "found by a dispensation only", setdiff(by_rx, by_dx), base, 1L)
    add("1 \u00b7 Cohort", "found by both", intersect(by_dx, by_rx), base, 1L)
  }

  if (is.null(disease) || !nzchar(disease)) return(rbindlist(rows))

  # 2 - the disease under study
  with_dis <- intersect(who(disease), base)
  add("2 \u00b7 Infection", disease, with_dis, base)

  # 3 - eradicating treatment: the union, then its levels. The union is found from the data
  # (the event whose people contain all the others), not from its name.
  tr <- r[role == "treatment", event]
  tr_ids <- lapply(stats::setNames(tr, tr), function(e) intersect(who(e), with_dis))
  if (length(tr)) {
    others <- unlist(tr_ids[setdiff(tr, names(tr_ids))], use.names = FALSE)
    is_union <- vapply(tr, function(e) all(unlist(tr_ids[setdiff(tr, e)], use.names = FALSE) %in% tr_ids[[e]]), TRUE)
    union_ev <- if (any(is_union)) tr[which(is_union)[1]] else NULL
    levels_ev <- setdiff(tr, union_ev)
    treated <- if (!is.null(union_ev)) tr_ids[[union_ev]] else unique(unlist(tr_ids, use.names = FALSE))
    add("3 \u00b7 Treatment", "Eradicating treatment", treated, with_dis)
    for (e in levels_ev) add("3 \u00b7 Treatment", e, tr_ids[[e]], treated, 1L)
  }

  # 4, 5, 6 - outcome, cause and competing event, all on the same base so the column adds up
  dead <- intersect(who("Death"), with_dis)
  add("4 \u00b7 Outcome", "Died", dead, with_dis)
  cause_ev <- intersect(cause_events, unique(l$event))
  if (length(cause_ev)) {
    assoc <- intersect(who(cause_ev), dead)
    add("5 \u00b7 Cause", "Death associated with the disease", assoc, dead)
    # The breakdown only when there is one: with a single cause event it would repeat the line
    # above with the same number.
    if (length(cause_ev) > 1L)
      for (e in cause_ev) add("5 \u00b7 Cause", e, intersect(who(e), dead), dead, 1L)
  }
  add("6 \u00b7 Competing", "Death from a different cause", intersect(who("Death_other_cause"), dead), dead)
  add("6 \u00b7 Competing", "Death cause unregistered", intersect(who("Death_cause_unregistered"), dead), dead)
  rbindlist(rows)
}
