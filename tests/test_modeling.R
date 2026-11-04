# Models with competing risks (Rscript tests/test_modeling.R)
suppressMessages({library(data.table); library(survival)})
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date

set.seed(42)
n <- 900
# A treated group with a lower rate of the outcome and the same rate of the competing event.
trt <- rep(c("Treated", "Untreated"), each = n / 2)
haz <- ifelse(trt == "Treated", 0.04, 0.12)
t_out <- rexp(n, haz); t_comp <- rexp(n, 0.05); t_cens <- runif(n, 1, 12)
time <- pmin(t_out, t_comp, t_cens)
status <- fifelse(t_out <= pmin(t_comp, t_cens), 1L, fifelse(t_comp <= t_cens, 2L, 0L))
d <- data.table(id = as.character(seq_len(n)), group = factor(trt, c("Untreated", "Treated")),
                sex = factor(sample(c("F", "M"), n, TRUE)), time = time, status = status,
                start = D("2015-01-01"),
                status_label = factor(status, 0:2, c("Censored", "Liver_death", "Other_death")))

ok(sum(d$status == 1L) > 50 && sum(d$status == 2L) > 50, "the synthetic set has both kinds of event")

cs <- fit_cause_specific(d, c("group", "sex"))
ok(!is.null(cs$table) && nrow(cs$table) == 2, "cause-specific: one row per term")
hr <- cs$table[grepl("group", term), estimate]
ok(hr < 1, "treatment lowers the cause-specific hazard, as it was simulated")
ok(cs$table[grepl("group", term), ci_high] < 1, "and the interval excludes 1")

sd <- fit_subdistribution(d, c("group", "sex"))
ok(!is.null(sd$table) && nrow(sd$table) == 2, "Fine-Gray: one row per term")
ok(sd$table[grepl("group", term), estimate] < 1, "the subdistribution hazard ratio also goes down")
ok(sd$table$model[1] == "Subdistribution HR (Fine-Gray)", "each table says which model it is")

both <- fit_models(d, c("group", "sex"))
ok(nrow(both$table) == 4 && uniqueN(both$table$model) == 2, "both models are reported together")

# the two answer different questions and need not agree: that is the point of reporting both
ok(abs(cs$table[grepl("group", term), estimate] - sd$table[grepl("group", term), estimate]) > 1e-8,
   "the two ratios are not the same number")

# ---- guards ---------------------------------------------------------------------------
u <- usable_covariates(d[, one := factor("a")][], c("group", "one", "nope"))
ok(identical(u$use, "group"), "a constant covariate and a missing one are left out")
ok(any(grepl("only one value", u$dropped)) && any(grepl("not in the table", u$dropped)),
   "and the reason is given for each")
d_na <- copy(d); d_na[1:10, sex := NA]
ok(any(grepl("missing values", usable_covariates(d_na, "sex")$dropped)),
   "a covariate with missing values is left out, not silently dropped by the fit")

few <- d[1:40]
ok(any(grepl("per term", model_warnings(few, "group", 1L))), "too few events per term is warned")
no_ev <- copy(d); no_ev[group == "Treated", status := 0L]
ok(any(grepl("No outcome events in group", model_warnings(no_ev, "group", 1L))),
   "a level with no events is named")

# no competing event: Fine-Gray equals the cause-specific one and says so instead of fitting
only1 <- copy(d)[status == 2L, `:=`(status = 0L, status_label = factor("Censored", levels(d$status_label)))]
r1 <- fit_subdistribution(only1, "group")
ok(is.null(r1$table) && any(grepl("equals the", r1$warnings)),
   "with no competing event it does not pretend to be a different model")

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
