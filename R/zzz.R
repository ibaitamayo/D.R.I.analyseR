# data.table syntax inside a package needs this flag; without it every j expression is
# evaluated in the wrong frame when the package is installed rather than sourced.
.datatable.aware <- TRUE

# Columns are referred to by name inside data.table calls, which R CMD check reads as
# undefined global variables. Declaring them keeps a real typo visible among the noise.
utils::globalVariables(c(
  "id", "event", "date", "cod", "code", "type", "system", "role", "rule", "use", "from", "to",
  "persons", "person_years", "events", "rate", "outcome", "competing", "censored", "status",
  "status_label", "time", "start", "end", "exit", "exited", "admin_end", "outcome_date",
  "competing_date", "censor_date", "group_date", "index_date", "group", "N", "V1", "pref",
  "f_dx", "f_drug", "f_any", "min_dispensations", "birth", "birth_year", "death", "death_code",
  "death_type", "last_contact", "sex", "age", "age_band", "era", "stratum", "year", "estimate",
  "with_index_event", "with_outcome", "without_outcome", "pct_with_outcome", "persons_at_risk",
  "ci_low", "ci_high", "term", "p", "model", "ev", "n", "has_dx", "has_drug", "roles", "N_",
  "Disease", "CodeType", "DxCode", "Use", "ValidFrom", "ValidTo", "Source", "Role", "ATC",
  "MinDispensations", "value", "variable", "cause", "disease", "step", "rows", "label"))
