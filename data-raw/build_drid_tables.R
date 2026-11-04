# Builds the three patient tables that D.R.I.analyseR reads, from the five delivered files.
#
# The delivered files are an extraction, not a data model: the same diagnosis arrives through
# several routes, one coding-system label is really a care setting, and death comes as a year.
# Everything this script decides is written down here, because each decision is about these
# data and not about the package: the package must receive tables it can use as they are.
#
# Output goes next to the input, on the same removable volume. Every output row carries
# Pac_Unif_Cod, so nothing written here may be copied into the repository.
#
# Usage:  Rscript data-raw/build_drid_tables.R
suppressMessages({library(readxl); library(data.table)})

ROOT    <- "/Volumes/NO NAME/datos drog"
OUT     <- file.path(ROOT, "tablas_app")
TODAY   <- Sys.Date()           # extraction horizon: no event can be later
ICD10_FROM <- 2016L             # Spain replaced CIE-9-MC with CIE-10-ES on 2016-01-01
dir.create(OUT, showWarnings = FALSE)

id_chr <- function(x) sub("\\.0+$", "", sprintf("%.0f", as.numeric(x)))
say    <- function(...) cat(sprintf(...), "\n", sep = "")
log    <- character(0)
note   <- function(...) { s <- sprintf(...); log <<- c(log, s); say("  %s", s) }

# ---- T1: persons and diagnoses -------------------------------------------------------------
say("T1  Persons and diagnoses")
f2 <- file.path(ROOT, "Punto II - Diagnosticos.xlsx")
# Two sheets, split by identifier only: one table that was cut in half to fit Excel's row limit.
dx <- rbindlist(lapply(excel_sheets(f2), function(s) as.data.table(read_excel(f2, sheet = s))),
                use.names = TRUE)
setnames(dx, c("id", "ini", "fin", "cod", "desc", "tipo", "caso"))
n0 <- nrow(dx)
dx <- dx[, .(id = id_chr(id), cod = trimws(cod), tipo = trimws(tipo), d = as.Date(ini))]

# URG is the care setting (hospital emergency), not a coding system: its codes are CIE-10-ES
# (F10.129, S06.0X9A, T14.8XXA) and all of them are dated after the 2016 switch. Left as "URG"
# the package would not recognise the label and would drop every one of these rows.
n_urg <- dx[tipo == "URG", .N]
dx[tipo == "URG", tipo := "10ES"]   # relabelled in full just below
note("%s rows labelled URG relabelled as CIE-10-ES (emergency department, CIE-10-ES codes)", format(n_urg, big.mark = " "))

# M (ICD-O morphology), QUIR (procedure codes) and ORPHA (Orphanet) are not diagnosis coding
# systems the package compares against; they are dropped here instead of arriving as rows it
# cannot use.
drop <- dx[!tipo %in% c("CIAP", "CIAP2", "9", "10ES"), .N, by = tipo]
dx <- dx[tipo %in% c("CIAP", "CIAP2", "9", "10ES")]
if (nrow(drop)) note("dropped non-diagnosis coding systems: %s",
                     paste(sprintf("%s (%s)", drop$tipo, format(drop$N, big.mark = " ")), collapse = ", "))

# The delivered labels are abbreviations ("9", "10ES"). The national version decides which
# codes exist (V02.62 is CIE-9-MC, B18.2 is CIE-10-ES), so the labels are written in full:
# the definitions shipped with the package name the version, and the package compares the two.
dx[, tipo := c("9" = "CIE-9-MC", "10ES" = "CIE-10-ES", "CIAP2" = "CIAP-2", "CIAP" = "CIAP")[tipo]]
note("coding-system labels written in full (CIE-9-MC, CIE-10-ES, CIAP-2, CIAP)")
note("  %s rows are CIAP (ICPC-1), compared against definitions written for CIAP-2 (ICPC-2)",
     format(dx[tipo == "CIAP", .N], big.mark = " "))

# The hospital discharge file carries one principal diagnosis per admission with its own date.
# 96,6 % of its person-code pairs are also in Punto II, but on other dates, so it is added
# rather than merged: the engine takes the first date of each event per person, and these dates
# can be earlier. It has no coding-system column, so the system comes from the discharge year;
# the data confirm the cut exactly (no code with a CIE-9 shape from 2016 on).
cm <- as.data.table(read_excel(file.path(ROOT, "Punto I - Ingresos hospitalarios CMBD.xlsx")))
setnames(cm, c("id", "ing", "alta", "cod", "desc", "alta_tipo", "asist"))
cmbd <- cm[, .(id = id_chr(id), cod = trimws(cod),
               tipo = fifelse(year(as.Date(alta)) >= ICD10_FROM, "CIE-10-ES", "CIE-9-MC"),
               d = as.Date(ing))]
note("added %s hospital principal diagnoses (coding system from the discharge year, cut at %d)",
     format(nrow(cmbd), big.mark = " "), ICD10_FROM)
dx <- rbind(dx, cmbd)

# A diagnosis with no date is not an event. A date after the extraction cannot be one either.
# Old dates are kept: in primary care the date is the onset of a condition, so 1985 is a
# plausible start for drug use and not an error.
n_na <- dx[is.na(d), .N]; n_fut <- dx[!is.na(d) & d > TODAY, .N]
dx <- dx[!is.na(d) & d <= TODAY]
note("dropped %d rows with no diagnosis date and %d dated after the extraction", n_na, n_fut)
n_dup <- nrow(dx) - nrow(unique(dx))
dx <- unique(dx)
note("dropped %s exactly duplicated rows", format(n_dup, big.mark = " "))
setnames(dx, c("Pac_Unif_Cod", "Diag_Cod", "Diag_Tipo", "Diag_Fecha"))
setcolorder(dx, c("Pac_Unif_Cod", "Diag_Cod", "Diag_Tipo", "Diag_Fecha"))
say("  %s rows, %s persons (from %s delivered)", format(nrow(dx), big.mark = " "),
    format(uniqueN(dx$Pac_Unif_Cod), big.mark = " "), format(n0 + nrow(cmbd), big.mark = " "))
print(dx[, .N, by = Diag_Tipo][order(-N)])

# ---- T2: persons and treatments ------------------------------------------------------------
say("\nT2  Persons and treatments")
read_med <- function(file, source) {
  m <- as.data.table(read_excel(file.path(ROOT, file)))
  setnames(m, c("id", "atc5", "d5", "atc7", "d7", "ini", "fin"))
  m[, .(Pac_Unif_Cod = id_chr(id), ATC_Cod = trimws(atc7), ATC_Tipo = "ATC",
        Disp_Fecha = as.Date(ini), fuente = source)]
}
# ATC7 is used, never ATC5: the level is complete in both files and the engine matches ATC by
# prefix, so a definition written at J05AP or N07BC still reaches these rows.
drg <- rbind(read_med("Punto IV - MedicacionFARHO.xlsx", "hospital"),
             read_med("Punto IV - MedicacionLAMIA.xlsx", "community"))
note("hospital pharmacy %s rows, community pharmacy %s rows",
     format(drg[fuente == "hospital", .N], big.mark = " "), format(drg[fuente == "community", .N], big.mark = " "))
n_bad <- drg[is.na(Disp_Fecha) | Disp_Fecha > TODAY, .N]
drg <- drg[!is.na(Disp_Fecha) & Disp_Fecha <= TODAY]
if (n_bad) note("dropped %d rows with no or impossible dispensation date", n_bad)
drg[, fuente := NULL]
drg <- unique(drg)
say("  %s rows, %s persons", format(nrow(drg), big.mark = " "), format(uniqueN(drg$Pac_Unif_Cod), big.mark = " "))
print(drg[, .(n = .N, personas = uniqueN(Pac_Unif_Cod)), by = .(ATC5 = substr(ATC_Cod, 1, 5))][order(ATC5)])

# ---- T3: patient covariates and death causes ------------------------------------------------
say("\nT3  Patient covariates and death causes")
dm <- as.data.table(read_excel(file.path(ROOT, "Punto III - DatosDemograficos.xlsx")))
setnames(dm, c("id", "nac", "sexo", "zbs", "fall", "ult"))
pt <- dm[, .(Pac_Unif_Cod = id_chr(id), Sexo = sexo, Anio_nac = as.integer(nac),
             Zona = zbs, ult = as.Date(ult), anio_fall = as.integer(fall))]

# Death arrives as a year, which cannot date an event. For a person recorded as dead the date
# of death is the date of last contact: after dying there are no further contacts, so the last
# one is the death itself.
pt[, Fecha_defuncion := fifelse(!is.na(anio_fall), ult, as.Date(NA))]
dead <- pt[!is.na(anio_fall), .N]
note("%s persons recorded as dead; the date of death is their date of last contact", format(dead, big.mark = " "))
note("  of these, %d have no date of last contact and so keep no date of death",
     pt[!is.na(anio_fall) & is.na(Fecha_defuncion), .N])
disc <- pt[!is.na(anio_fall) & !is.na(Fecha_defuncion) & year(Fecha_defuncion) != anio_fall]
note("  %d have a last contact in a different year from the recorded year of death (%d later, %d earlier)",
     nrow(disc), disc[year(Fecha_defuncion) > anio_fall, .N], disc[year(Fecha_defuncion) < anio_fall, .N])
# Impossible dates are removed, which is cleaning, not a choice: a death before birth or after
# the extraction is not a date.
imp <- pt[!is.na(Fecha_defuncion) & (year(Fecha_defuncion) < Anio_nac | Fecha_defuncion > TODAY), .N]
pt[!is.na(Fecha_defuncion) & (year(Fecha_defuncion) < Anio_nac | Fecha_defuncion > TODAY), Fecha_defuncion := NA]
if (imp) note("  %d impossible dates of death removed (before birth or after the extraction)", imp)

# Cause of death: only the hospital file holds a cause, as the principal diagnosis of the
# admission that ended in death. It exists for deaths in hospital and for nobody else, which is
# the limitation the analysis already declares. Choosing which diagnosis of which episode is
# the cause is a decision about these data, so it is taken here and not in the package.
ex <- cm[grepl("exitus", alta_tipo, ignore.case = TRUE)]
ex <- ex[order(as.Date(alta))][, .SD[.N], by = .(id = id_chr(id))]
ex <- ex[, .(Pac_Unif_Cod = id, Causa = trimws(cod),
             Causa_tipo = fifelse(year(as.Date(alta)) >= ICD10_FROM, "CIE-10-ES", "CIE-9-MC"))]
pt <- merge(pt, ex, by = "Pac_Unif_Cod", all.x = TRUE)
note("%d causes of death taken from the principal diagnosis of the admission ending in death",
     pt[!is.na(Causa), .N])
note("  %d of the %s dead have no cause: they did not die in hospital",
     pt[!is.na(anio_fall) & is.na(Causa), .N], format(dead, big.mark = " "))
# A cause with no death is not usable: the discharge says the person died, demographics does
# not record it. The cause is removed so the table never carries one without a date of death;
# the discrepancy itself is reported, because reconciling the two files is their call.
n_orph <- pt[is.na(anio_fall) & !is.na(Causa), .N]
pt[is.na(anio_fall), `:=`(Causa = NA_character_, Causa_tipo = NA_character_)]
note("  %d discharge(s) ending in death belong to a person demographics does not record as dead: cause removed", n_orph)

# End of follow-up: the last contact for everyone. For the dead it is the date of death, for
# the living it is where the data stop knowing about them. Without it a living person with no
# further records would count as time at risk until the end of the data.
pt[, Fin_seguimiento := ult]
note("%d living persons have no date of last contact and so no end of follow-up",
     pt[is.na(anio_fall) & is.na(Fin_seguimiento), .N])
pt <- pt[, .(Pac_Unif_Cod, Sexo, Anio_nac, Fecha_defuncion, Causa, Causa_tipo, Zona, Fin_seguimiento)]
say("  %s persons", format(nrow(pt), big.mark = " "))
print(pt[, .N, by = Sexo][order(-N)])
say("  zones: %d   birth years: %d-%d (%d missing)", uniqueN(pt$Zona),
    min(pt$Anio_nac, na.rm = TRUE), max(pt$Anio_nac, na.rm = TRUE), pt[is.na(Anio_nac), .N])

# ---- write -----------------------------------------------------------------------------------
# Two formats for each: rds loads instantly and keeps the dates typed, csv can be opened and
# checked by anyone. The app reads either. The names are the ones the app asks for in tab 1.
tables <- list("Tabla 1 - personas y diagnosticos"        = dx,
               "Tabla 2 - personas y tratamientos"        = drg,
               "Tabla 3 - covariables y causas de muerte" = pt)
for (nm in names(tables)) {
  saveRDS(tables[[nm]], file.path(OUT, paste0(nm, ".rds")))
  fwrite(tables[[nm]], file.path(OUT, paste0(nm, ".csv")), bom = TRUE)
  say("  %-42s %9s rows", nm, format(nrow(tables[[nm]]), big.mark = " "))
}
writeLines(c(paste("Built", format(Sys.time(), "%Y-%m-%d %H:%M"), "by data-raw/build_drid_tables.R"),
             "", "Decisions taken on these data (the package takes none of them):", paste("-", log)),
           file.path(OUT, "COMO_SE_CONSTRUYERON.txt"))
say("\nWritten to %s", OUT)
