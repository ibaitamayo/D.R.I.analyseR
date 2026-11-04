#' Run the D.R.I.analyseR application
#'
#' Opens the Shiny application in a browser. Every table is loaded from inside the app, so
#' nothing has to be prepared beforehand; `data_dir` only saves the loading when a folder
#' already holds the three tables under the names the app asks for.
#'
#' @param data_dir Optional folder holding "Tabla 1 - personas y diagnosticos",
#'   "Tabla 2 - personas y tratamientos" and "Tabla 3 - covariables y causas de muerte"
#'   (.rds or .csv). When given, a button in tab 1 loads all three at once. Defaults to the
#'   environment variable DRID_DATA, and to none when that is unset.
#' @param ... Passed to [shiny::runApp()], for example `port` or `launch.browser`.
#' @return Called for its side effect; does not return while the application runs.
#' @export
run_app <- function(data_dir = Sys.getenv("DRID_DATA", ""), ...) {
  for (p in c("shiny", "bslib", "readxl", "writexl", "reactable", "ggplot2"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("The application needs the package '", p, "'. Install it with install.packages(\"", p, "\").")
  if (nzchar(data_dir) && !dir.exists(data_dir))
    stop("data_dir does not exist: ", data_dir)
  old <- Sys.getenv("DRID_DATA", unset = NA_character_)
  Sys.setenv(DRID_DATA = data_dir)
  on.exit(if (is.na(old)) Sys.unsetenv("DRID_DATA") else Sys.setenv(DRID_DATA = old), add = TRUE)
  app <- system.file("app", package = "D.R.I.analyseR")
  if (!nzchar(app)) stop("The application was not found in the installed package.")
  shiny::runApp(app, ...)
}

#' Folder holding the example definition and data tables
#'
#' The tables shipped with the package: the synthetic patients, and the definitions used in
#' the drug-related infectious disease study they come from.
#'
#' @param file Optional file name. Without it, the folder itself is returned.
#' @return A path.
#' @export
#' @examples
#' head(list.files(drid_examples()))
drid_examples <- function(file = NULL) {
  d <- system.file("extdata", package = "D.R.I.analyseR")
  if (is.null(file)) d else file.path(d, file)
}
