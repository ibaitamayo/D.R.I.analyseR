# Models over the person-level table of competing_risks.R. Two of them, on purpose:
#
#   cause-specific hazard ratio (Cox, competing events censored)
#       answers: among those still at risk, how much faster does the outcome happen?
#   subdistribution hazard ratio (Fine-Gray)
#       answers: how much does the cumulative incidence of the outcome differ?
#
# They are different questions and they can point in different directions, which is why both
# are reported and neither is called "the" hazard ratio. A treatment that keeps people alive
# longer gives them more time to reach the outcome, and only the second one shows it.

# Covariates measured at the start of follow-up, not at extraction: age changes, and so does
# the era. The reference date is `start`, which is the index date or the landmark if one was used.
model_data <- function(tte, patients = NULL, eras = NULL, age_breaks = NULL, extra = character(0)) {
  d <- as.data.table(tte)[, id := as.character(id)]
  if (!is.null(patients)) {
    p <- as.data.table(patients)[, id := as.character(id)]
    keep <- intersect(c("id", "birth", "birth_year", "sex", extra), names(p))
    d <- merge(d, p[, keep, with = FALSE], by = "id", all.x = TRUE)
    if (all(c("birth", "birth_year") %in% names(d)))
      d[, age := age_at(birth, birth_year, start)]
    if (!is.null(age_breaks) && length(age_breaks)) d[, age_band := age_band(age, age_breaks)]
  }
  if (!is.null(eras) && nrow(eras)) d[, era := era_at(start, eras)]
  for (v in intersect(c("group", "sex", "age_band", "era", extra), names(d)))
    if (!is.factor(d[[v]])) d[, (v) := factor(get(v))]
  d[]
}

# Covariates that cannot go into a model, with the reason. Checked before fitting so the user
# is told instead of getting a fit that silently dropped them.
usable_covariates <- function(d, covariates) {
  out <- list(use = character(0), dropped = character(0))
  for (v in covariates) {
    if (!v %in% names(d)) { out$dropped <- c(out$dropped, paste0(v, " (not in the table)")); next }
    x <- d[[v]]
    n <- length(unique(x[!is.na(x)]))
    if (n < 2) out$dropped <- c(out$dropped, paste0(v, " (only one value)"))
    else if (anyNA(x)) {
      out$dropped <- c(out$dropped, paste0(v, " (", sum(is.na(x)), " missing values)"))
    } else out$use <- c(out$use, v)
  }
  out
}

# Warnings that change how a result should be read, never a reason to refuse the fit.
model_warnings <- function(d, covariates, n_terms) {
  w <- character(0)
  ev <- sum(d$status == 1L)
  if (ev == 0) return("No outcome events: nothing can be estimated.")
  per <- ev / max(1L, n_terms)
  if (per < 10)
    w <- c(w, paste0(ev, " outcome events for ", n_terms, " model terms (", round(per, 1),
                     " per term). Below about 10 the estimates are unstable."))
  for (v in covariates) {
    if (!is.factor(d[[v]])) next
    empty <- d[, .(ev = sum(status == 1L)), by = c(v)][ev == 0]
    if (nrow(empty))
      w <- c(w, paste0("No outcome events in ", v, " = ", paste(empty[[v]], collapse = ", "),
                       ": that ratio is not interpretable."))
  }
  w
}

tidy_cox <- function(fit, model) {
  s <- summary(fit)
  ci <- s$conf.int
  data.table(model = model, term = rownames(ci),
             estimate = as.numeric(ci[, 1]), ci_low = as.numeric(ci[, 3]), ci_high = as.numeric(ci[, 4]),
             p = as.numeric(s$coefficients[, ncol(s$coefficients)]))
}

# Cause-specific: the competing event is censoring, so this is the rate among those still at risk.
fit_cause_specific <- function(d, covariates) {
  d <- as.data.table(d)
  u <- usable_covariates(d, covariates)
  if (!length(u$use)) return(list(table = NULL, warnings = c("No usable covariate.", u$dropped), terms = 0L))
  f <- stats::as.formula(paste("Surv(time, status == 1L) ~", paste(u$use, collapse = " + ")))
  fit <- survival::coxph(f, data = d)
  tb <- tidy_cox(fit, "Cause-specific HR")
  list(table = tb, warnings = c(model_warnings(d, u$use, nrow(tb)),
                                if (length(u$dropped)) paste("Left out:", paste(u$dropped, collapse = "; "))),
       terms = nrow(tb), fit = fit)
}

# Fine-Gray: the competing event keeps the person in the risk set with a decreasing weight, so
# this is about the cumulative incidence. Needs status as a factor with the censored level first,
# which is what time_to_event() already returns in status_label.
fit_subdistribution <- function(d, covariates, outcome_label = NULL) {
  d <- as.data.table(d)
  u <- usable_covariates(d, covariates)
  if (!length(u$use)) return(list(table = NULL, warnings = c("No usable covariate.", u$dropped), terms = 0L))
  if (!"status_label" %in% names(d)) return(list(table = NULL, warnings = "No status_label: cannot fit Fine-Gray.", terms = 0L))
  lv <- levels(d$status_label)
  et <- outcome_label %||% lv[2]
  if (!sum(d$status_label == et)) return(list(table = NULL, warnings = "No outcome events: Fine-Gray not fitted.", terms = 0L))
  if (!sum(d$status == 2L))
    return(list(table = NULL, terms = 0L,
                warnings = paste("No competing events, so the subdistribution hazard ratio equals the",
                                 "cause-specific one. Only the first table is shown.")))
  keep <- c("time", "status_label", u$use)
  fg <- survival::finegray(stats::as.formula("Surv(time, status_label) ~ ."),
                           data = as.data.frame(d[, keep, with = FALSE]), etype = et)
  f <- stats::as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~", paste(u$use, collapse = " + ")))
  fit <- survival::coxph(f, data = fg, weights = fg$fgwt)
  tb <- tidy_cox(fit, "Subdistribution HR (Fine-Gray)")
  list(table = tb, warnings = if (length(u$dropped)) paste("Left out:", paste(u$dropped, collapse = "; ")) else character(0),
       terms = nrow(tb), fit = fit)
}

# Both, side by side, which is how they should be read.
fit_models <- function(d, covariates, outcome_label = NULL) {
  cs <- fit_cause_specific(d, covariates)
  sd <- fit_subdistribution(d, covariates, outcome_label)
  list(table = rbindlist(list(cs$table, sd$table), use.names = TRUE, fill = TRUE),
       warnings = unique(c(cs$warnings, sd$warnings)))
}
