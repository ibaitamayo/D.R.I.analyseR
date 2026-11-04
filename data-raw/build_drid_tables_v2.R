# Builds the three patient tables from the SECOND extraction (EUDA2) and then full-joins them
# with the tables built from the first one, so nothing either extraction holds is lost.
#
# The two extractions cover the same 16 910 persons but are not the same table: the second one
# is de-duplicated to one row per diagnosis episode, so it has fewer rows and, for some people,
# a later first date; and it carries a real date of birth and a real date of death, which the
# first one did not. Neither is a superset of the other, which is why the union is taken.
#
# Every output row carries Pac_Unif_Cod, so nothing written here may be copied into the repository.
#
# Usage:  Rscript data-raw/build_drid_tables_v2.R
suppressMessages({library(readxl); library(data.table)})

NEW     <- "/Volumes/NO NAME/EUDA2"
OLD_OUT <- "/Volumes/NO NAME/datos drog/tablas_app"
OUT_NEW <- file.path(NEW, "tablas_app")
OUT_UNI <- "/Volumes/NO NAME/tablas_app_union"
TODAY   <- Sys.Date()
ICD10_FROM <- 2016L
for (d in c(OUT_NEW, OUT_UNI)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

id_chr <- function(x) sub("\\.0+$", "", sprintf("%.0f", as.numeric(x)))
say    <- function(...) cat(sprintf(...), "\n", sep = "")
log    <- character(0)
note   <- function(...) { s <- sprintf(...); log <<- c(log, s); say("  %s", s) }

# The second extraction delivers dates three ways in the same file set: typed datetimes, ISO
# text with a time part, and Excel serial numbers as text (with "NULL" for missing). One parser
# for all three, so a format change cannot silently become a missing date.
as_date2 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x)); out <- as.Date(rep(NA_character_, length(x)))
  x[x %in% c("", "NULL", "NA")] <- NA_character_
  num <- suppressWarnings(as.numeric(x))
  ser <- !is.na(num) & num > 1000 & num < 100000
  out[ser] <- as.Date(floor(num[ser]), origin = "1899-12-30")
  iso <- is.na(out) & !is.na(x) & nchar(x) >= 10
  if (any(iso)) out[iso] <- suppressWarnings(as.Date(substr(x[iso], 1, 10)))
  out
}

# ---- T1 new: persons and diagnoses ---------------------------------------------------------
say("T1  Persons and diagnoses (EUDA2)")
f2 <- file.path(NEW, "Diagnosticos.xlsx")
dxn <- rbindlist(lapply(excel_sheets(f2), function(s) as.data.table(read_excel(f2, sheet = s))),
                 use.names = TRUE, fill = TRUE)
n0 <- nrow(dxn)
dxn <- dxn[, .(id = id_chr(Pac_Unif_Cod), cod = trimws(as.character(Diag_Cod)),
               tipo = trimws(as.character(Diag_Tipo)), d = as_date2(ResDiag_Inicio_Fecha))]
n_urg <- dxn[tipo == "URG", .N]
dxn[tipo == "URG", tipo := "10ES"]
note("%s rows labelled URG relabelled as CIE-10-ES", format(n_urg, big.mark = " "))
drop <- dxn[!tipo %in% c("CIAP", "CIAP2", "9", "10ES"), .N, by = tipo]
dxn <- dxn[tipo %in% c("CIAP", "CIAP2", "9", "10ES")]
if (nrow(drop)) note("dropped non-diagnosis coding systems: %s",
                     paste(sprintf("%s (%s)", drop$tipo, format(drop$N, big.mark = " ")), collapse = ", "))
dxn[, tipo := c("9" = "CIE-9-MC", "10ES" = "CIE-10-ES", "CIAP2" = "CIAP-2", "CIAP" = "CIAP")[tipo]]

cmn <- as.data.table(read_excel(file.path(NEW, "Ingresos hospitalarios.xls")))
cmn[, `:=`(ing = as_date2(CMBD_Fecha_Inicio_Contacto), alta = as_date2(CMBD_Fecha_Fin_Contacto))]
cmbdn <- cmn[, .(id = id_chr(Pac_Unif_Cod), cod = trimws(as.character(Diag_Cod)),
                 tipo = fifelse(year(alta) >= ICD10_FROM, "CIE-10-ES", "CIE-9-MC"), d = ing)]
note("added %s hospital principal diagnoses (coding system from the discharge year)",
     format(nrow(cmbdn), big.mark = " "))
dxn <- rbind(dxn, cmbdn)
n_na <- dxn[is.na(d) | is.na(cod) | cod == "", .N]; n_fut <- dxn[!is.na(d) & d > TODAY, .N]
dxn <- dxn[!is.na(d) & d <= TODAY & !is.na(cod) & cod != ""]
note("dropped %s rows with no date or no code and %d dated after the extraction",
     format(n_na, big.mark = " "), n_fut)
n_dup <- nrow(dxn) - nrow(unique(dxn)); dxn <- unique(dxn)
note("dropped %s exactly duplicated rows", format(n_dup, big.mark = " "))
setnames(dxn, c("Pac_Unif_Cod", "Diag_Cod", "Diag_Tipo", "Diag_Fecha"))
say("  %s rows, %s persons (from %s delivered)", format(nrow(dxn), big.mark = " "),
    format(uniqueN(dxn$Pac_Unif_Cod), big.mark = " "), format(n0 + nrow(cmbdn), big.mark = " "))

# ---- T2 new: persons and treatments --------------------------------------------------------
say("\nT2  Persons and treatments (EUDA2)")
fa <- as.data.table(read_excel(file.path(NEW, "Farho.xlsx")))
la <- as.data.table(read_excel(file.path(NEW, "Lamia.xlsx")))
drgn <- rbind(
  fa[, .(Pac_Unif_Cod = id_chr(Pac_Unif_Cod), ATC_Cod = trimws(as.character(ATC7_Cod)),
         ATC_Tipo = "ATC", Disp_Fecha = as_date2(Disp_Fecha), fuente = "hospital")],
  la[, .(Pac_Unif_Cod = id_chr(Pac_Unif_Cod), ATC_Cod = trimws(as.character(ATC7_Cod)),
         ATC_Tipo = "ATC", Disp_Fecha = as_date2(PresAle_Prescripcion_Inicio_Fecha),
         fuente = "community")])
note("hospital pharmacy %s rows, community pharmacy %s rows",
     format(drgn[fuente == "hospital", .N], big.mark = " "),
     format(drgn[fuente == "community", .N], big.mark = " "))
n_bad <- drgn[is.na(Disp_Fecha) | Disp_Fecha > TODAY | is.na(ATC_Cod) | ATC_Cod == "", .N]
drgn <- drgn[!is.na(Disp_Fecha) & Disp_Fecha <= TODAY & !is.na(ATC_Cod) & ATC_Cod != ""]
if (n_bad) note("dropped %d rows with no or impossible dispensation date, or no ATC code", n_bad)
drgn[, fuente := NULL]; drgn <- unique(drgn)
say("  %s rows, %s persons", format(nrow(drgn), big.mark = " "),
    format(uniqueN(drgn$Pac_Unif_Cod), big.mark = " "))

# ---- T3 new: patient covariates and death causes --------------------------------------------
say("\nT3  Patient covariates and death causes (EUDA2)")
dm <- as.data.table(read_excel(file.path(NEW, "Datos demograficos.xls"), guess_max = 20000))
ptn <- dm[, .(Pac_Unif_Cod = id_chr(Pac_Unif_Cod),
              Sexo = trimws(as.character(Sexo_Nom)),
              Fecha_nac = as_date2(Pac_Fecha_Nacimiento),
              Fecha_defuncion = as_date2(Pac_Fecha_Fallecimiento),
              Zona = trimws(as.character(ZBS_Nom)),
              Fin_seguimiento = as_date2(Pac_Fecha_ultimo_contacto))]
ptn[, Anio_nac := year(Fecha_nac)]
note("this extraction carries a real date of birth (%d of %d persons) and a real date of death",
     ptn[!is.na(Fecha_nac), .N], nrow(ptn))
note("%s persons with a recorded date of death", format(ptn[!is.na(Fecha_defuncion), .N], big.mark = " "))
imp <- ptn[!is.na(Fecha_defuncion) & (Fecha_defuncion > TODAY |
            (!is.na(Fecha_nac) & Fecha_defuncion < Fecha_nac)), .N]
ptn[!is.na(Fecha_defuncion) & (Fecha_defuncion > TODAY |
     (!is.na(Fecha_nac) & Fecha_defuncion < Fecha_nac)), Fecha_defuncion := NA]
if (imp) note("  %d impossible dates of death removed (before birth or after the extraction)", imp)
# End of follow-up: the last contact, and never earlier than the death it is supposed to contain.
ptn[!is.na(Fecha_defuncion) & (is.na(Fin_seguimiento) | Fin_seguimiento < Fecha_defuncion),
    Fin_seguimiento := Fecha_defuncion]
note("%d persons have no date of last contact and so no end of follow-up",
     ptn[is.na(Fin_seguimiento), .N])
# Cause of death: still only the hospital file, as the principal diagnosis of the admission
# ending in death. This extraction adds no cause-of-death field.
exn <- cmn[grepl("exitus", TipoCircAlt_Nom, ignore.case = TRUE)]
exn <- exn[order(alta)][, .SD[.N], by = .(id = id_chr(Pac_Unif_Cod))]
exn <- exn[, .(Pac_Unif_Cod = id, Causa = trimws(as.character(Diag_Cod)),
               Causa_tipo = fifelse(year(alta) >= ICD10_FROM, "CIE-10-ES", "CIE-9-MC"))]
ptn <- merge(ptn, exn, by = "Pac_Unif_Cod", all.x = TRUE)
note("%d causes of death taken from the admission ending in death", ptn[!is.na(Causa), .N])
n_orph <- ptn[is.na(Fecha_defuncion) & !is.na(Causa), .N]
ptn[is.na(Fecha_defuncion), `:=`(Causa = NA_character_, Causa_tipo = NA_character_)]
if (n_orph) note("  %d discharge(s) ending in death with no recorded death: cause removed", n_orph)
note("  %d of the dead have no cause: they did not die in hospital",
     ptn[!is.na(Fecha_defuncion) & is.na(Causa), .N])
ptn <- ptn[, .(Pac_Unif_Cod, Sexo, Anio_nac, Fecha_nac, Fecha_defuncion, Causa, Causa_tipo,
               Zona, Fin_seguimiento)]
say("  %s persons", format(nrow(ptn), big.mark = " "))

saveRDS(dxn, file.path(OUT_NEW, "Tabla 1 - personas y diagnosticos.rds"))
saveRDS(drgn, file.path(OUT_NEW, "Tabla 2 - personas y tratamientos.rds"))
saveRDS(ptn, file.path(OUT_NEW, "Tabla 3 - covariables y causas de muerte.rds"))

# ================= FULL JOIN with the first extraction =======================================
say("\n=== FULL JOIN with the tables from the first extraction ===")
o1 <- as.data.table(readRDS(file.path(OLD_OUT, "Tabla 1 - personas y diagnosticos.rds")))
o2 <- as.data.table(readRDS(file.path(OLD_OUT, "Tabla 2 - personas y tratamientos.rds")))
o3 <- as.data.table(readRDS(file.path(OLD_OUT, "Tabla 3 - covariables y causas de muerte.rds")))

# T1 and T2 are event tables: one row is one fact (this person, this code, this date). The union
# of the two extractions is therefore every fact either of them recorded, with exact repeats
# collapsed. A row only one of them has is kept: neither file is a superset of the other.
u1 <- unique(rbind(o1[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)],
                   dxn[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)]))
note("T1 union: %s rows (first %s, second %s, shared %s)", format(nrow(u1), big.mark = " "),
     format(nrow(o1), big.mark = " "), format(nrow(dxn), big.mark = " "),
     format(nrow(o1) + nrow(dxn) - nrow(u1), big.mark = " "))
note("  rows only in the first: %s | only in the second: %s",
     format(nrow(fsetdiff(o1[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)],
                          dxn[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)])), big.mark = " "),
     format(nrow(fsetdiff(dxn[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)],
                          o1[, .(Pac_Unif_Cod, Diag_Cod, Diag_Tipo, Diag_Fecha)])), big.mark = " "))
u2 <- unique(rbind(o2[, .(Pac_Unif_Cod, ATC_Cod, ATC_Tipo, Disp_Fecha)],
                   drgn[, .(Pac_Unif_Cod, ATC_Cod, ATC_Tipo, Disp_Fecha)]))
note("T2 union: %s rows (first %s, second %s)", format(nrow(u2), big.mark = " "),
     format(nrow(o2), big.mark = " "), format(nrow(drgn), big.mark = " "))

# T3 is one row per person, so the join is field by field. The second extraction wins wherever
# it carries a measured value that the first could only approximate: a real date of birth and a
# real date of death, against a year of birth and a date of last contact standing in for death.
# Everything else is filled from whichever file has it, and every disagreement is counted.
u3 <- merge(o3, ptn, by = "Pac_Unif_Cod", all = TRUE, suffixes = c("_1", "_2"))
cmp_death <- u3[!is.na(Fecha_defuncion_1) & !is.na(Fecha_defuncion_2)]
note("T3: %d persons; dead in both %d, only in the first %d, only in the second %d", nrow(u3),
     nrow(cmp_death), u3[!is.na(Fecha_defuncion_1) & is.na(Fecha_defuncion_2), .N],
     u3[is.na(Fecha_defuncion_1) & !is.na(Fecha_defuncion_2), .N])
if (nrow(cmp_death)) {
  dd <- as.numeric(cmp_death$Fecha_defuncion_2 - cmp_death$Fecha_defuncion_1)
  note("  of those dead in both, the real date equals the last-contact proxy in %d (%.1f%%); median difference %.0f days, IQR %.0f to %.0f",
       sum(dd == 0), 100 * mean(dd == 0), median(dd), quantile(dd, .25), quantile(dd, .75))
}
u3[, `:=`(
  Sexo            = fcoalesce(Sexo_2, Sexo_1),
  Fecha_nac       = Fecha_nac,
  Anio_nac        = fcoalesce(Anio_nac_2, Anio_nac_1),
  Fecha_defuncion = fcoalesce(Fecha_defuncion_2, Fecha_defuncion_1),
  Causa           = fcoalesce(Causa_2, Causa_1),
  Causa_tipo      = fcoalesce(Causa_tipo_2, Causa_tipo_1),
  Zona            = fcoalesce(Zona_2, Zona_1),
  Fin_seguimiento = pmax(Fin_seguimiento_1, Fin_seguimiento_2, na.rm = TRUE))]
note("  sex disagrees in %d persons; year of birth in %d; zone in %d",
     u3[!is.na(Sexo_1) & !is.na(Sexo_2) & Sexo_1 != Sexo_2, .N],
     u3[!is.na(Anio_nac_1) & !is.na(Anio_nac_2) & Anio_nac_1 != Anio_nac_2, .N],
     u3[!is.na(Zona_1) & !is.na(Zona_2) & Zona_1 != Zona_2, .N])
# End of follow-up can never precede the death it must contain.
u3[!is.na(Fecha_defuncion) & (is.na(Fin_seguimiento) | Fin_seguimiento < Fecha_defuncion),
   Fin_seguimiento := Fecha_defuncion]
u3 <- u3[, .(Pac_Unif_Cod, Sexo, Anio_nac, Fecha_nac, Fecha_defuncion, Causa, Causa_tipo,
             Zona, Fin_seguimiento)]
note("union: %s dead, %d of them with a cause", format(u3[!is.na(Fecha_defuncion), .N], big.mark = " "),
     u3[!is.na(Causa), .N])

tables <- list("Tabla 1 - personas y diagnosticos"        = u1,
               "Tabla 2 - personas y tratamientos"        = u2,
               "Tabla 3 - covariables y causas de muerte" = u3)
for (nm in names(tables)) {
  saveRDS(tables[[nm]], file.path(OUT_UNI, paste0(nm, ".rds")))
  fwrite(tables[[nm]], file.path(OUT_UNI, paste0(nm, ".csv")), bom = TRUE)
  say("  %-42s %9s rows", nm, format(nrow(tables[[nm]]), big.mark = " "))
}
writeLines(c(paste("Built", format(Sys.time(), "%Y-%m-%d %H:%M"), "by data-raw/build_drid_tables_v2.R"),
             "Union of the extraction in 'datos drog' (29 Oct) and 'EUDA2' (3 Nov).", "",
             "Decisions taken on these data (the package takes none of them):", paste("-", log)),
           file.path(OUT_UNI, "COMO_SE_CONSTRUYERON.txt"))
say("\nNew-extraction tables: %s\nUnion tables:          %s", OUT_NEW, OUT_UNI)
