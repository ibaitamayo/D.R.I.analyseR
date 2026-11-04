# Competing-risks module tests with synthetic cases whose answer is known
suppressMessages({library(data.table); library(survival)}); library(D.R.I.analyseR)
failures <- 0
ok <- function(cond, txt) { cat(if (isTRUE(cond)) "OK  " else "FAIL", txt, "\n"); if (!isTRUE(cond)) failures <<- failures + 1 }
D <- as.Date; p <- function(id, event, date) data.table(id = id, event = event, date = D(date))
L <- function(...) rbindlist(list(...))

# 4 people enter in 2010: a dies of the outcome, b of the competing event, c is censored
# by exit, d reaches the end of the period
d1 <- L(p("a","Dx","2010-01-01"), p("b","Dx","2010-01-01"), p("c","Dx","2010-01-01"), p("d","Dx","2010-01-01"),
        p("a","Liver_death","2012-01-01"), p("b","Other_death","2011-01-01"), p("c","Exit","2013-01-01"))
t1 <- time_to_event(d1, "Dx", "Liver_death", "Other_death", "Exit", end_date = D("2015-01-01"))
ok(t1[id=="a", status] == 1 && t1[id=="b", status] == 2 && t1[id=="c", status] == 0 && t1[id=="d", status] == 0,
   "Status: outcome, competing, censored by exit, censored at end of period")
ok(abs(t1[id=="a", time] - 2) < 0.01 && abs(t1[id=="d", time] - 5) < 0.02, "Time to event in years")
ok(all(levels(t1$status_label) == c("Censored", "Liver_death", "Other_death")), "Labels take the chosen event names")

# The outcome wins if it happens before the competing event
d2 <- L(p("x","Dx","2010-01-01"), p("x","Liver_death","2011-01-01"), p("x","Other_death","2012-01-01"))
ok(time_to_event(d2,"Dx","Liver_death","Other_death", end_date=D("2015-01-01"))[, status] == 1, "Whichever comes first wins")

# Summary: person-years and rates
s <- event_summary(t1, per = 1000)
ok(s$persons == 4 && s$outcome == 1 && s$competing == 1 && s$censored == 2, "Summary counts by status")
ok(abs(s$person_years - (2 + 1 + 3 + 5)) < 0.1, "Person-years add up to the follow-up of the four people")

# Aalen-Johansen vs 1 - Kaplan-Meier: with competing events the naive one is higher
d3 <- L(rbindlist(lapply(1:50, function(i) p(paste0("p",i),"Dx","2010-01-01"))),
        rbindlist(lapply(1:10, function(i) p(paste0("p",i),"Liver_death", D("2010-01-01") + 365*i/2))),
        rbindlist(lapply(11:35, function(i) p(paste0("p",i),"Other_death", D("2010-01-01") + 200))))
t3 <- time_to_event(d3, "Dx", "Liver_death", "Other_death", end_date = D("2020-01-01"))
cif <- cumulative_incidence(t3)
aj <- max(cif[event == "Liver_death", cif])
km <- 1 - min(survfit(Surv(time, status == 1) ~ 1, data = t3)$surv)
ok(aj < km, sprintf("Aalen-Johansen (%.3f) por debajo de 1 - Kaplan-Meier (%.3f) al haber competidor", aj, km))
ok(abs(aj - 10/50) < 0.001, "Con seguimiento completo, la incidencia acumulada = 10 de 50")
ok(all(cif[, .(suma = max(cif)), by = event][, suma] <= 1), "Ninguna incidencia acumulada supera 1")

# Landmark: classifies by what happened before, and excludes earlier events
d4 <- L(p("t","Dx","2010-01-01"), p("t","Treat","2010-03-01"), p("u","Dx","2010-01-01"),
        p("v","Dx","2010-01-01"), p("v","Liver_death","2010-02-01"))
t4 <- time_to_event(d4, "Dx", "Liver_death", NULL, NULL, "Treat", end_date = D("2015-01-01"), landmark_months = 6)
ok(nrow(t4) == 2 && !("v" %in% t4$id), "Landmark: whoever had the outcome before it does not enter")
ok(t4[id=="t", group] == "With Treat" && t4[id=="u", group] == "Without Treat", "Landmark: group by what happened before")
ok(abs(t4[id=="t", as.numeric(start - D("2010-01-01"))] - 181) <= 1, "Landmark: follow-up starts 6 months later")

# Without a competing event it still works (Aalen-Johansen = 1 - Kaplan-Meier)
t5 <- time_to_event(d1, "Dx", "Liver_death", NULL, NULL, end_date = D("2015-01-01"))
ok(nrow(cumulative_incidence(t5)) > 0 && all(t5$status != 2), "Without a competing event the module still runs")

# Groups given from outside (exposure that is not "an event before the start")
g <- data.table(id = c("a","b","c","d"), group = c("Ever","Ever","Never","Never"))
t6 <- time_to_event(d1, "Dx", "Liver_death", "Other_death", "Exit", end_date = D("2015-01-01"), group_table = g)
ok(all(sort(unique(t6$group)) == c("Ever","Never")) && t6[id=="a", group] == "Ever", "group_table: groups taken from outside")
ok(nrow(event_summary(t6)) == 2, "group_table: the summary splits into two groups")

# ---- an event the cohort no longer has is named, not asserted -----------------------------
e <- try(time_to_event(data.table(id = "1", event = "Other", date = as.Date("2015-01-01")),
                       "Nobody_has_this", "Other"), silent = TRUE)
ok(inherits(e, "try-error") && grepl("Nobody in this cohort has the index event", conditionMessage(attr(e, "condition"))),
   "a missing index event says so in words, instead of failing on an assertion")

cat(if (failures == 0) "\nAll tests passed\n" else paste("\nFailures:", failures, "\n"))
