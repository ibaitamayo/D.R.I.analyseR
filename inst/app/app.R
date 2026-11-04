# MorbfindR · event finder from diagnoses and drug dispensations
# Local app: data never leave the computer where it runs.
#   shiny::runApp("path/to/MorbfindR_app")
#
# Four tables, each with its key fields mapped when loaded:
#   1. patient–diagnosis   2. patient–drug   3. disease–diagnosis code   4. disease–ATC code
suppressMessages({
  library(shiny); library(bslib); library(data.table); library(readxl); library(writexl); library(reactable); library(ggplot2)
})
# Where the example and definition tables live: inside the installed package, or beside this
# file in the working folder it was developed in.
EX_DIR <- local({
  d <- system.file("extdata", package = "D.R.I.analyseR")
  if (nzchar(d) && dir.exists(d)) d else "examples"
})
ex <- function(f) file.path(EX_DIR, f)

# The engine is the package. An installed copy is simply attached; a source checkout, where
# the package is not installed, has the same files one folder up and they are sourced.
if (requireNamespace("D.R.I.analyseR", quietly = TRUE)) {
  library(D.R.I.analyseR)
} else {
  dev <- normalizePath(file.path("..", "..", "R"), mustWork = FALSE)
  if (!dir.exists(dev)) dev <- "R"
  for (f in list.files(dev, pattern = "[.]R$", full.names = TRUE)) source(f, local = TRUE)
}
options(shiny.maxRequestSize = 4 * 1024^3)

# Folder holding this project's own tables, if there is one. Real data live outside the app, so
# the path is given from outside too: DRID_DATA="/path/to/tables" R -e 'shiny::runApp()'.
PROJECT_DIR <- Sys.getenv("DRID_DATA", "")
if (nzchar(PROJECT_DIR) && !dir.exists(PROJECT_DIR)) {
  warning("DRID_DATA points to a folder that does not exist: ", PROJECT_DIR)
  PROJECT_DIR <- ""
}

NONE <- "(none)"
CHECK_SAMPLE <- 1e5
RULE_LABELS <- c("Diagnosis or drug" = "dx_or_drug", "Diagnosis and drug" = "dx_and_drug",
                 "Diagnosis only" = "dx", "Drug only" = "drug")

# ---- helpers --------------------------------------------------------------------
read_file <- function(path, name, sep = "auto", enc = "UTF-8") {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) return(as.data.table(read_excel(path, col_types = "text")))
  if (ext == "rds") return(as.data.table(readRDS(path)))
  fread(path, sep = if (sep == "auto") "auto" else if (sep == "tab") "\t" else sep,
        encoding = enc, colClasses = "character", showProgress = FALSE)
}
fmt_t <- function(s) {
  s <- max(0, round(s))
  if (s < 60) paste0(s, " s") else if (s < 3600) sprintf("%d min %02d s", s %/% 60, s %% 60)
  else sprintf("%d h %02d min", s %/% 3600, (s %% 3600) %/% 60)
}
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), " %")
sample_of <- function(d) if (nrow(d) > CHECK_SAMPLE) d[sample(.N, CHECK_SAMPLE)] else d
opt <- function(x) if (is.null(x) || x == NONE) NULL else x
fmt_label <- function(f) c("%Y-%m-%d" = "YYYY-MM-DD", "%d/%m/%Y" = "DD/MM/YYYY", "%d-%m-%Y" = "DD-MM-YYYY",
                           "%Y/%m/%d" = "YYYY/MM/DD", "%d.%m.%Y" = "DD.MM.YYYY", "%Y%m%d" = "YYYYMMDD")[[f]]
distinct_cols <- function(...) { x <- c(...); length(unique(x)) == length(x) }

# Suggests a column: tries patterns in order of preference, skipping columns already taken
guess <- function(cols, patterns, exclude = character(0), optional = FALSE) {
  free <- setdiff(cols, exclude)
  for (p in patterns) { i <- grep(p, free, ignore.case = TRUE); if (length(i)) return(free[i[1]]) }
  if (optional) NONE else if (length(free)) free[1] else cols[1]
}
PAT_ID    <- c("pac_unif", "^id$", "patient", "pac", "id")
PAT_DATE  <- c("fecha", "date")
PAT_EVENT <- c("^event", "disease", "evento", "enfermedad", "event")

# Progress with remaining-time estimate (from the fraction done so far)
new_progress <- function(p) {
  t0 <- Sys.time()
  function(f, text) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    left <- if (f > 0.03) el * (1 - f) / f else NA
    p$set(value = f, message = text,
          detail = paste0(round(100 * f), " % · ", fmt_t(el), " elapsed",
                          if (!is.na(left)) paste0(" · ~", fmt_t(left), " left") else " · estimating time left…"))
  }
}

templates <- list(
  disease_diagnosis = data.table(Disease = c("Drug_use", "Drug_use", "Drug_use", "HCV", "HCV"),
                                 CodeType = c("ICD-10-CM", "ICD-9-CM", "CIAP2", "ICD-10-CM", "ICD-9-CM"),
                                 DxCode = c("F11", "304.0", "P19", "B18.2", "070.54"),
                                 Source = c("Quan 2008", "Quan 2008", "BDCAP", "Ball 2017", "ICD-10 -> ICD-9 mapping")),
  disease_atc = data.table(Disease = c("HCV", "Opioid_agonist_treatment"), ATC = c("J05AP", "N07BC"),
                           MinDispensations = c(1, 2), Source = c("ATC", "ATC")),
  instructions = data.table(
    Topic = c("Column names", "Code type", "Codes and granularity", "ATC codes", "MinDispensations", "Rules"),
    Description = c(
      "Free: key fields are mapped in the app when each table is loaded.",
      "Coding system of each diagnosis code: CIAP/CIAP2/ICPC, CIE-9/ICD-9 (or 9), CIE-10/ICD-10 (or 10ES).",
      "No dot = whole category (E860 includes E860.0). With a dot = that level and its children (E86.0 includes E86.01, not E86.1).",
      "Prefix as well: J05AP includes every J05AP* drug.",
      "Optional: distinct dispensation dates required; the event date is that of the dispensation reaching it. Default 1.",
      "For diseases present in both definition tables, the app asks whether diagnosis OR drug, diagnosis AND drug, or only one of them defines the event. A disease can be defined as diagnosed OR treated, but a treatment event must be defined by the drug alone: a diagnosis never makes someone treated.")))

clock_js <- tags$script(HTML("
  var clocks = {};
  Shiny.addCustomMessageHandler('clock_start', function(id) {
    var t0 = Date.now(), el = document.getElementById(id);
    el.style.display = 'block';
    clearInterval(clocks[id]);
    clocks[id] = setInterval(function() {
      var s = Math.round((Date.now() - t0) / 1000);
      el.querySelector('.clock-t').textContent = (s < 60 ? s + ' s' : Math.floor(s/60) + ' min ' + (s%60) + ' s');
    }, 1000);
  });
  Shiny.addCustomMessageHandler('clock_stop', function(id) { clearInterval(clocks[id]); document.getElementById(id).style.display = 'none'; });
"))
clock_ui <- function(id, text) div(id = id, class = "alert alert-info", style = "display:none;",
  span(class = "spinner-border spinner-border-sm me-2", role = "status"),
  text, " · elapsed: ", strong(class = "clock-t", "0 s"))

# One card per table: upload, reading options, key fields and quick checks
table_card <- function(id, title, help, big = TRUE) {
  card(id = paste0("card_", id), card_header(title),
    p(class = "text-muted small mb-1 mf-help", help),
    accordion(open = FALSE, class = "mb-2",
      accordion_panel("What should this table contain?", icon = icon("circle-question"), spec_ui(id))),
    ghost_zone(id, fileInput(paste0("f_", id), NULL, accept = c(".csv", ".txt", ".xlsx", ".xls", ".rds"),
                             placeholder = "Drop or browse your file")),
    if (big) div(id = paste0("opts_", id), layout_columns(
      selectInput(paste0("sep_", id), "Separator", c("auto", ";", ",", "tab")),
      selectInput(paste0("enc_", id), "Encoding", c("UTF-8", "Latin-1")))),
    clock_ui(paste0("clock_", id), "Reading file"),
    uiOutput(paste0("keys_", id)), uiOutput(paste0("check_", id)))
}

# ---- UI ----------------------------------------------------------------------------
ui <- page_navbar(
  id = "nav",
  title = "MorbfindR · event finder",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # only the process map needs to fill the window; elsewhere the content keeps its natural height
  fillable = "4. Process map",
  header = tagList(clock_js, guide_head()),
  nav_panel("Welcome",
    layout_columns(col_widths = c(3, 6, 3), div(),
      card(card_header("Before you start"),
        p("This app finds, for each patient, the ", strong("first date"), " of each disease or event you define, ",
          "from diagnosis codes (CIAP / ICD-9 / ICD-10) and, optionally, drug dispensations (ATC)."),
        h6(class = "mt-3", "Do you want guidance?"),
        radioButtons("help_mode", NULL, selected = "on", width = "100%",
          choiceValues = c("on", "off"),
          choiceNames = list(
            HTML("<strong>Yes, guide me.</strong><br><span class='text-muted small'>Explanatory text in every tab, the faded example tables behind each upload box, and step-by-step bubbles the first time you open a tab.</span>"),
            HTML("<strong>No thanks.</strong><br><span class='text-muted small'>No standing explanations and no bubbles. The folded &lsquo;What should this table contain?&rsquo; panels and the Excel templates stay where they are.</span>"))),
        p(class = "text-muted small mb-0", "You can change this at any time, here."),
        div(class = "mt-3", actionButton("go_start", "Continue", class = "btn-primary", width = "160px"))),
      div())),
  nav_panel("Start here",
    layout_columns(col_widths = c(5, 7),
      card(id = "steps_card", card_header("What this app does"),
        p("It finds, for each patient, the ", strong("first date"), " of each disease or event you define, ",
          "using diagnosis codes (CIAP / ICD-9 / ICD-10) and, optionally, drug dispensations (ATC). ",
          "The result can be turned into an event log and a process map."),
        h6("Steps"),
        tags$ol(
          tags$li(strong("Prepare your tables"), " (three with patient data, two with definitions). What each must contain is described on the right, ",
                  "and again next to each upload box. Templates for the definitions can be downloaded in tab 2."),
          tags$li(strong("Tab 1 · Patient data:"), " upload the patient – diagnosis table and, if you use drugs, the patient – drug table. ",
                  "Tell the app which column is which (", em("key fields"), ") and check the preview and the quick checks."),
          tags$li(strong("Tab 2 · Definitions:"), " upload the disease – diagnosis code table and/or the disease – ATC table, map their key fields ",
                  "and choose, for diseases defined by both, whether diagnosis OR drug, or diagnosis AND drug, is required."),
          tags$li(strong("Tab 3 · Results:"), " press ", em("Find events"), ". Download the table with the first date per disease."),
          tags$li(strong("Tab 4 · Process map"), " (optional): draw the most frequent trajectories between diseases.")),
        p("To see how it works before using your own data, press ", strong("Load example tables"), " in tab 1."),
        h6("General rules"), GENERAL_HELP(),
        tags$button(class = "btn btn-outline-warning btn-sm", onclick = "mfResetGuide()", icon("rotate-left"), " Restart the step-by-step guide")),
      card(id = "four_tables", card_header("Your tables: three of patients, two of definitions"),
        navset_underline(
          nav_panel("1 · Persons and diagnoses", spec_ui("pdx")),
          nav_panel("2 · Persons and treatments", spec_ui("pdrug")),
          nav_panel("3 · Patient covariates and death causes", spec_ui("ppat")),
          nav_panel("4 · Disease – diagnosis code", spec_ui("ddx")),
          nav_panel("5 · Disease – drug code", spec_ui("datc")),
          nav_panel("6 · Cause of death – code", spec_ui("dcause")))))),
  nav_panel("1. Patient data",
    div(id = "example_box", class = "d-flex align-items-end gap-3 mb-2",
        actionButton("example", "Load example patient tables", class = "btn-outline-secondary mb-3"),
        span(class = "text-muted mb-3 mf-help",
             "Synthetic persons, diagnoses, dispensations and covariates, to try everything safely. ",
             "The definitions are chosen in tab 2.")),
    # Real data are never kept with the app. Point DRID_DATA at the folder that holds them and
    # the three tables can be loaded in one click; without it this button does not appear.
    if (nzchar(PROJECT_DIR)) div(class = "d-flex align-items-end gap-3 mb-2",
        actionButton("project", "Load project tables", class = "btn-outline-primary mb-3"),
        span(class = "text-muted mb-3 small", "From ", tags$code(PROJECT_DIR), ".")),
    layout_columns(col_widths = c(4, 4, 4),
      table_card("pdx", "1 · Persons and diagnoses", "Required.", big = FALSE),
      table_card("pdrug", "2 · Persons and treatments", "Optional: dispensations, for diseases or treatments defined by drugs.", big = FALSE),
      table_card("ppat", "3 · Patient covariates and death causes", "Optional: sex, year of birth, area, death and its cause. Needed for mortality and for age.", big = FALSE)),
    div(id = "drop_proc_wrap", checkboxInput("drop_proc", "Drop ICD-10 rows without diagnosis format (procedures, morphology)", TRUE)),
    card(id = "mode_card", card_header("Who is in the data?"),
      p(class = "text-muted small mf-help",
        "If the extraction is already a cohort of people who use drugs, there is nothing to select and the ",
        "comparison is treated against untreated. If it is the whole population, the app identifies the users ",
        "with the events whose role is 'cohort' and draws two matched controls per user, so the comparison is ",
        "users against people with no documented use."),
      radioButtons("pop_mode", NULL, inline = TRUE,
        c("The data are already a cohort of people who use drugs" = "cohort",
          "The data are the whole population: define the users and match controls" = "population"), "cohort"),
      conditionalPanel("input.pop_mode == 'population'",
        p(class = "small text-muted",
          "Exact matching, two controls per user, without replacement, on year of birth, sex and area if ",
          "there is one. Seed 123, so the draw is reproducible. A user whose cell cannot supply both controls ",
          "is left out."),
        uiOutput("match_info")))),
  nav_panel("2. Definitions",
    div(class = "d-flex align-items-end gap-3 mb-2",
        actionButton("example_def", "Load the DRID definitions", class = "btn-outline-secondary mb-3"),
        span(class = "text-muted mb-3 small", "Quan 2008, Ball 2017, INE and EUDA (revised)."),
        downloadButton("template", "Download templates", class = "btn-outline-secondary mb-3"),
        span(class = "text-muted mb-3 mf-help", "You can edit them, or load your own files below.")),
    layout_columns(col_widths = c(6, 6),
      table_card("ddx", "3 · Disease – diagnosis code", "Needed for diseases defined by diagnoses.", big = FALSE),
      table_card("datc", "4 · Disease – drug code", "Needed for diseases or treatments defined by drugs.", big = FALSE)),
    layout_columns(col_widths = c(6, 6),
      table_card("dcause", "5 · Cause of death – code", paste(
        "Optional, its own table so nothing has to be merged with the diagnosis definitions.",
        "These codes are compared only with the cause of death of the patient table, never with",
        "the diagnoses: a hospital diagnosis of hepatitis C is not a death from hepatitis C."), big = FALSE),
      div()),
    card(id = "rules_card", card_header("Rules per disease"),
      p(class = "text-muted small mf-help", "Diseases defined in both tables need a rule combining diagnoses and drugs. ",
        "For drug-based definitions, the minimum number of distinct dispensation dates can be adjusted."),
      uiOutput("rules_ui"), uiOutput("warn_def"))),
  nav_panel("3. Event search",
    card(card_header("Search"),
      checkboxInput("without_event", "Include persons without any event in the table", FALSE),
      actionButton("run", "Find events", class = "btn-primary", width = "220px"),
      clock_ui("clock_run", "Searching"),
      uiOutput("res_info"),
      uiOutput("restrict_ui")),
    card(id = "funnel_card", card_header("The six phases"),
      p(class = "text-muted small mf-help",
        "Each step is a subset of the one above, so the percentages read down the page. ",
        "Counts are people, never rows, and nothing here is recalculated: it is the same events ",
        "found above, counted by person."),
      selectInput("funnel_disease", "Disease under study", choices = NULL, width = "320px"),
      reactableOutput("tab_funnel")),
    layout_columns(col_widths = c(5, 7),
      card(id = "card_summary", card_header("Persons per disease"), reactableOutput("tab_summary"),
           h6("Diagnosis rows"), reactableOutput("tab_prep"), uiOutput("other_causes")),
      card(card_header("Analysis table (first 200 rows)"),
           p(class = "small text-muted mb-1",
             "One row per person: covariates, the group the cohort decision produced, and the first date ",
             "of every event. This is the table the later tabs use."),
           downloadButton("download", "Download the analysis table (csv)"), reactableOutput("tab_res")))),
  nav_panel("4. Process map",
    # fillable = FALSE: in a fillable card the body is a flex container, the grid of controls
    # computes to height 0 and the button lands on top of the slider.
    card(fillable = FALSE, card_header("Event log and process map"),
      div(class = "alert alert-light",
          "Nodes: persons going through each event; the fill shows how many. Arrows: median time between consecutive ",
          "events and number of persons, for the trajectories you keep with the slider \u2014 at 5%, the few most frequent ",
          "paths, which together cover 5% of people, not 5% of the paths. Death is drawn last within its date, because ",
          "nothing follows it. The border colour is the role of the event."),
      div(class = "small text-muted mb-2",
          lapply(seq_along(ROLE_COLOURS), function(i) span(class = "me-3",
            span(style = paste0("display:inline-block;width:12px;height:12px;border:3px solid ",
                                ROLE_COLOURS[i], ";margin-right:4px;vertical-align:middle")),
            names(ROLE_COLOURS)[i]))),
      # The event list is as tall as the number of events, so it gets a block of its own with a
      # ceiling and its own scrollbar: in a column beside the controls it grew under the button.
      div(class = "border rounded p-2 mb-3", style = "max-height:280px;overflow-y:auto",
          uiOutput("ev_map_ui")),
      # A plain Bootstrap row, not layout_columns: the slider's track is positioned absolutely,
      # so the row has to keep its own height for whatever comes after it.
      div(class = "row g-3 mb-3",
        div(class = "col-md-8", sliderInput("coverage", "Most frequent trajectories covering (% of persons)",
                                            1, 100, 5, step = 1, width = "100%")),
        div(class = "col-md-4", selectInput("units", "Time unit",
                                            c(months = "months", weeks = "weeks", days = "days"), width = "100%"))),
      div(class = "mt-4 mb-3", actionButton("make_map", "Draw map", class = "btn-primary", width = "220px")),
      clock_ui("clock_map", "Drawing the map"),
      # The widget sizes itself from its container. Outside a fillable card nothing gives it a
      # height, so it drew at zero pixels and the map was there but invisible.
      # "heigth" (sic): that is the argument name in processmapR.
      div(style = "height:700px;overflow:auto",
          processmapR::processMapOutput("map", heigth = "700px")))),
  nav_panel("5. Outcomes",
    card(card_header("Outcomes over calendar time"), card_body(fillable = FALSE,
      p(class = "text-muted small mf-help",
        "Three views over the events found in tab 3, in the style used by registry reports: a cascade ",
        "(how many people with an index event reached an outcome), a calendar table of events (vaccination, ",
        "death, treatment\u2026) and rates per person-years at risk."),
      layout_columns(col_widths = c(3, 3, 3, 3),
        selectInput("oc_index", "Index event (time zero)", choices = NULL),
        selectInput("oc_outcome", "Outcome event", choices = NULL),
        selectInput("oc_exit", "Exit event (optional)", choices = NULL),
        selectInput("oc_group", "Compare by (optional)", choices = NULL),
        div(numericInput("oc_per", "Rate per", 1000, min = 1, step = 1000),
            p(class = "text-muted small mb-0", "person-years"))),
      div(class = "row g-3 mt-1",
        div(class = "col-md-6",
            sliderInput("oc_years", "Calendar years", min = 2000, max = 2026, value = c(2000, 2026), sep = "", step = 1, width = "100%")),
        div(class = "col-md-6",
            checkboxInput("oc_stop", "The outcome ends follow-up (as with death)", TRUE),
            p(class = "text-muted small mb-0", "First event per person: repeated events are not counted."))),
      uiOutput("oc_aviso"))),
    navset_card_underline(
      nav_panel("Cascade", card_body(fillable = FALSE,
        plotOutput("oc_plot_cascade", height = "420px"), class = "pt-3",
        div(class = "mt-3 mb-2", downloadButton("oc_dl_cascade", "Download table (csv)")),
        reactableOutput("oc_tab_cascade"))),
      nav_panel("Events by year", card_body(fillable = FALSE,
        plotOutput("oc_plot_events", height = "420px"), class = "pt-3",
        selectizeInput("oc_events", "Events to show", choices = NULL, multiple = TRUE, width = "100%"),
        div(class = "mb-2", downloadButton("oc_dl_events", "Download table (csv)")),
        reactableOutput("oc_tab_events"))),
      nav_panel("Rates", card_body(fillable = FALSE,
        uiOutput("oc_rate_total"),
        plotOutput("oc_plot_rates", height = "420px"), class = "pt-3",
        div(class = "mt-3 mb-2", downloadButton("oc_dl_rates", "Download table (csv)")),
        reactableOutput("oc_tab_rates"))))),
  nav_panel("6. Outcome modeling",
    card(card_header("Time to event, with competing risks"), card_body(fillable = FALSE,
      p(class = "text-muted small mf-help",
        "Nothing here is specific to any disease: you choose which event starts the clock, which one is ",
        "the outcome, which one competes with it and which one censors. Every table and figure is named ",
        "after the events you pick, so the same analysis serves any intervention with this data structure."),
      layout_columns(col_widths = c(3, 3, 3, 3),
        selectInput("cr_index", "Index event (time zero)", choices = NULL),
        selectInput("cr_outcome", "Outcome event", choices = NULL),
        selectInput("cr_competing", "Competing event", choices = NULL),
        selectInput("cr_censor", "Censoring event (optional)", choices = NULL)),
      layout_columns(col_widths = c(4, 4, 4),
        selectInput("cr_group", "Intervention / group (optional)", choices = NULL),
        div(numericInput("cr_landmark", "Start follow-up this many months after time zero", 0, min = 0, step = 1),
            p(class = "small text-muted mb-0",
              "Zero means follow-up starts at the index event. A positive number delays the start and ",
              "classifies the group at that moment, which removes the immortal time of comparing treated ",
              "with untreated from day zero: someone treated in month 4 cannot count as treated from day 1.")),
        numericInput("cr_per", "Rates per", 1000, min = 1, step = 1000)),
      uiOutput("cr_note"))),
    navset_card_underline(
      nav_panel("Cumulative incidence", card_body(fillable = FALSE, class = "pt-3",
        plotOutput("cr_plot", height = "440px"),
        div(class = "mt-3 mb-2", downloadButton("cr_dl_cif", "Download curve (csv)")))),
      nav_panel("Summary", card_body(fillable = FALSE, class = "pt-3",
        reactableOutput("cr_tab_summary"),
        div(class = "mt-3 mb-2", downloadButton("cr_dl_summary", "Download table (csv)")))),
      nav_panel("Models", card_body(fillable = FALSE, class = "pt-3",
        p(class = "text-muted small mf-help",
          "Two models, because they answer different questions and can point in different directions. ",
          "The cause-specific hazard ratio is about the rate among those still at risk, with the competing ",
          "event censored. The subdistribution hazard ratio (Fine-Gray) is about the cumulative incidence, ",
          "keeping people who had the competing event in the risk set. Neither is 'the' hazard ratio."),
        uiOutput("mod_covars_ui"),
        uiOutput("mod_warn"),
        reactableOutput("mod_table"),
        div(class = "mt-3 mb-2", downloadButton("mod_dl", "Download the estimates (csv)")))),
      nav_panel("Follow-up per person", card_body(fillable = FALSE, class = "pt-3",
        p(class = "text-muted small mf-help",
          "One row per person: start and end of follow-up, time, and whether it ended in the outcome, ",
          "the competing event or censoring. This is the table the analysis is built on."),
        reactableOutput("cr_tab_tte"),
        div(class = "mt-3 mb-2", downloadButton("cr_dl_tte", "Download table (csv)")))))),
  nav_panel("Definitions in use",
    card(card_body(fillable = FALSE, class = "pt-3",
      uiOutput("defs_plain"),
      tags$details(class = "mt-4 border rounded p-3",
        tags$summary(class = "fw-semibold",
          "Every code, one by one, as the definition tables declare it"),
        div(class = "mt-3",
          p(class = "text-muted small",
            "The listing below is generated from the definition tables themselves, not written ",
            "alongside them, so it cannot drift from what the search applied. The last column ",
            "gives the published source where one exists and the decision taken on these data ",
            "where it does not."),
          downloadButton("dl_defs", "Download the full listing (csv)"),
          uiOutput("defs_doc")))))),
  nav_spacer(),
  nav_item(tags$a(class = "nav-link mf-help", href = "#", onclick = "mfStartTour(); return false;", icon("circle-question"), " Guide"))
)

# ---- server --------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(pdx = NULL, pdrug = NULL, ppat = NULL, ddx = NULL, datc = NULL, dcause = NULL, eras = NULL)

  # Guidance on or off, chosen in the Welcome tab. It hides the standing explanations
  # (class mf-help) and stops the step-by-step bubbles from opening at all.
  observeEvent(input$help_mode, ignoreInit = FALSE, {
    session$sendCustomMessage("mf_help", identical(input$help_mode, "on"))
  })
  observeEvent(input$go_start, {
    nav_select("nav", if (identical(input$help_mode, "on")) "Start here" else "1. Patient data")
  })
  load_table <- function(id) {
    f <- input[[paste0("f_", id)]]
    session$sendCustomMessage("clock_start", paste0("clock_", id))
    on.exit(session$sendCustomMessage("clock_stop", paste0("clock_", id)))
    rv[[id]] <- tryCatch(read_file(f$datapath, f$name, input[[paste0("sep_", id)]] %||% "auto", input[[paste0("enc_", id)]] %||% "UTF-8"),
                         error = function(e) { showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(rv[[id]])) { session$sendCustomMessage("mf_loaded", id); send_hint(session, id) }
  }
  for (id in c("pdx", "pdrug", "ppat", "ddx", "datc", "dcause")) local({ i <- id; observeEvent(input[[paste0("f_", i)]], load_table(i)) })
  observeEvent(input$example, {
    rv$pdx   <- fread(ex("diagnoses_example.csv"), colClasses = "character")
    rv$pdrug <- fread(ex("drugs_example.csv"), colClasses = "character")
    rv$ppat  <- fread(ex("patients_example.csv"), colClasses = "character")
    for (i in c("pdx", "pdrug", "ppat")) session$sendCustomMessage("mf_loaded", i)
    send_hint(session, "example")
  })
  # The three tables as they were generated for this project. rds is preferred over csv: the
  # dates are already dates, so nothing depends on guessing a format.
  observeEvent(input$project, {
    pick <- function(stem) {
      f <- file.path(PROJECT_DIR, paste0(stem, c(".rds", ".csv")))
      f[file.exists(f)][1]
    }
    files <- list(pdx = pick("Tabla 1 - personas y diagnosticos"),
                  pdrug = pick("Tabla 2 - personas y tratamientos"),
                  ppat = pick("Tabla 3 - covariables y causas de muerte"))
    missing <- names(files)[vapply(files, function(x) is.na(x) || !length(x), TRUE)]
    if (length(missing)) {
      showNotification(paste("Not found in", PROJECT_DIR, "-", paste(missing, collapse = ", ")), type = "error")
      return()
    }
    for (i in names(files)) {
      rv[[i]] <- tryCatch(read_file(files[[i]], basename(files[[i]]), "auto", "UTF-8"),
                          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (!is.null(rv[[i]])) session$sendCustomMessage("mf_loaded", i)
    }
    showNotification(sprintf("Loaded %s rows of diagnoses, %s dispensations and %s persons.",
                             fmt_n(nrow(rv$pdx)), fmt_n(nrow(rv$pdrug)), fmt_n(nrow(rv$ppat))),
                     duration = 8)
  })
  observeEvent(input$example_def, {
    suffix <- "DRID"
    rv$ddx   <- as.data.table(read_excel(ex(paste0("definition_dx_", suffix, ".xlsx")), col_types = "text"))
    rv$datc  <- as.data.table(read_excel(ex(paste0("definition_atc_", suffix, ".xlsx")), col_types = "text"))
    f_cause  <- ex(paste0("definition_death_cause_", suffix, ".xlsx"))
    rv$dcause <- if (file.exists(f_cause)) as.data.table(read_excel(f_cause, col_types = "text")) else NULL
    rv$eras <- if (file.exists(ex("eras_hcv_ES.xlsx")))
      prepare_eras(as.data.table(read_excel(ex("eras_hcv_ES.xlsx"))), "Start", "Name")$data else NULL
    for (i in c("ddx", "datc", "dcause")) session$sendCustomMessage("mf_loaded", i)
  })
  n_rows <- function(d) p(strong(fmt_n(nrow(d))), " rows read. ", strong("Key fields:"))

  # ---- key fields, linked as soon as each table is loaded ----
  output$keys_pdx <- renderUI({
    d <- rv$pdx; req(d); cols <- names(d)
    g_id <- guess(cols, PAT_ID); g_type <- guess(cols, c("tipo", "type", "codif", "system"), g_id)
    g_date <- guess(cols, PAT_DATE, c(g_id, g_type))
    g_code <- guess(cols, c("diag_cod", "^cod", "^code", "codigo", "code", "cod", "diag"), c(g_id, g_type, g_date))
    tagList(n_rows(d), layout_columns(
      selectInput("k_pdx_id", "Patient ID", cols, g_id), selectInput("k_pdx_code", "Diagnosis code", cols, g_code),
      selectInput("k_pdx_type", "Code type", cols, g_type), selectInput("k_pdx_date", "Diagnosis date", cols, g_date)))
  })
  output$keys_pdrug <- renderUI({
    d <- rv$pdrug; req(d); cols <- names(d)
    g_id <- guess(cols, PAT_ID); g_date <- guess(cols, PAT_DATE, g_id)
    g_type <- guess(cols, c("codetype", "code_type", "tipo", "type", "codif", "system", "vocab"),
                    c(g_id, g_date), optional = TRUE)
    g_atc <- guess(cols, c("atc", "rxcui", "^cod", "code", "drug", "farm"), c(g_id, g_date, g_type))
    tagList(n_rows(d), layout_columns(
      selectInput("k_pdrug_id", "Patient ID", cols, g_id), selectInput("k_pdrug_atc", "Drug code", cols, g_atc),
      selectInput("k_pdrug_type", "Code type (optional: ATC if absent)", c(NONE, cols), g_type),
      selectInput("k_pdrug_date", "Dispensation date", cols, g_date)))
  })
  output$keys_ppat <- renderUI({
    d <- rv$ppat; req(d); cols <- names(d)
    g_id <- guess(cols, PAT_ID)
    g_sex <- guess(cols, c("sexo", "sex", "gender"), g_id, optional = TRUE)
    g_by <- guess(cols, c("anio_nac", "ano_nac", "birth_year", "year_of_birth", "nac", "birth"), c(g_id, g_sex), optional = TRUE)
    g_bd <- guess(cols, c("fecha_nac", "fecha_nacimiento", "birth_date", "date_of_birth", "dob"), c(g_id, g_sex, g_by), optional = TRUE)
    g_death <- guess(cols, c("defunc", "fallec", "death", "exitus"), c(g_id, g_sex, g_by, g_bd), optional = TRUE)
    g_cause <- guess(cols, c("causa", "cause"), c(g_id, g_sex, g_by, g_bd, g_death), optional = TRUE)
    g_ctype <- guess(cols, c("causa_tipo", "cause_type", "tipo", "type", "codif"), c(g_id, g_sex, g_by, g_bd, g_death, g_cause), optional = TRUE)
    g_area <- guess(cols, c("zona", "zbs", "area", "zone", "district"), c(g_id, g_sex, g_by, g_bd, g_death, g_cause, g_ctype), optional = TRUE)
    g_last <- guess(cols, c("ultimo_contacto", "last_contact", "seguimiento", "follow"), c(g_id, g_sex, g_by, g_bd, g_death, g_cause, g_ctype, g_area), optional = TRUE)
    tagList(n_rows(d), layout_columns(
      selectInput("k_ppat_id", "Patient ID", cols, g_id),
      selectInput("k_ppat_sex", "Sex", c(NONE, cols), g_sex),
      selectInput("k_ppat_birth_year", "Year of birth", c(NONE, cols), g_by),
      selectInput("k_ppat_birth", "Date of birth (used instead of the year when present)", c(NONE, cols), g_bd),
      selectInput("k_ppat_death", "Date of death", c(NONE, cols), g_death)),
      layout_columns(
      selectInput("k_ppat_cause", "Cause-of-death code", c(NONE, cols), g_cause),
      selectInput("k_ppat_cause_type", "Code type of the cause", c(NONE, cols), g_ctype),
      selectInput("k_ppat_area", "Area or zone", c(NONE, cols), g_area),
      selectInput("k_ppat_last", "End of follow-up", c(NONE, cols), g_last)))
  })
  patients <- reactive({
    req(rv$ppat, input$k_ppat_id)
    prepare_patients(rv$ppat, input$k_ppat_id,
                     col_birth = opt(input$k_ppat_birth),
                     col_birth_year = opt(input$k_ppat_birth_year), col_sex = opt(input$k_ppat_sex),
                     col_death = opt(input$k_ppat_death), col_death_code = opt(input$k_ppat_cause),
                     col_death_type = opt(input$k_ppat_cause_type), col_last_contact = opt(input$k_ppat_last),
                     extra_cols = opt(input$k_ppat_area))
  })
  output$check_ppat <- renderUI({
    d <- rv$ppat; req(d, input$k_ppat_id)
    pt <- patients()
    dead <- pt$data[!is.na(death), .N]; coded <- pt$data[!is.na(death) & !is.na(death_code), .N]
    tagList(h6("Preview (one row per person)"),
      preview(head(pt$data[, .(id, sex, birth_year, death, death_code, death_type)], 10)),
      check_box(nrow(pt$data) > 0,
        strong(fmt_n(nrow(pt$data))), " persons · deaths: ", strong(fmt_n(dead)),
        " · of those, with a cause code: ", strong(if (dead) pct(coded / dead) else "\u2014"),
        if (pt$approximate_age) " · age derived from the year of birth, so it is approximate by up to one year" else ""),
      reactable(pt$summary, compact = TRUE, striped = TRUE, defaultPageSize = 7))
  })
  output$keys_ddx <- renderUI({
    d <- rv$ddx; req(d); cols <- names(d)
    g_ev <- guess(cols, PAT_EVENT); g_type <- guess(cols, c("codetype", "code_type", "type", "tipo", "system", "codif"), g_ev)
    g_code <- guess(cols, c("dxcode", "^code$", "codigo", "diag_cod", "^cod", "code"), c(g_ev, g_type))
    tagList(n_rows(d), layout_columns(
      selectInput("k_ddx_event", "Disease", cols, g_ev), selectInput("k_ddx_type", "Code type", cols, g_type),
      selectInput("k_ddx_code", "Diagnosis code", cols, g_code),
      selectInput("k_ddx_role", "Role (optional)", c(NONE, cols), guess(cols, c("^role$", "rol", "papel"), c(g_ev, g_type, g_code), optional = TRUE))),
      layout_columns(
      selectInput("k_ddx_use", "Use of the code: diagnoses or only dates (optional)", c(NONE, cols),
                  guess(cols, c("^use$", "^uso$", "supporting", "apoyo"), c(g_ev, g_type, g_code), optional = TRUE)),
      selectInput("k_ddx_from", "Code valid from (optional)", c(NONE, cols),
                  guess(cols, c("validfrom", "valid_from", "valido_desde", "desde"), c(g_ev, g_type, g_code), optional = TRUE)),
      selectInput("k_ddx_to", "Code valid to (optional)", c(NONE, cols),
                  guess(cols, c("validto", "valid_to", "valido_hasta", "hasta"), c(g_ev, g_type, g_code), optional = TRUE)),
      selectInput("k_ddx_src", "Source or decision (optional)", c(NONE, cols),
                  guess(cols, c("^source$", "fuente", "origen", "referencia", "decision"), c(g_ev, g_type, g_code), optional = TRUE))))
  })
  output$keys_dcause <- renderUI({
    d <- rv$dcause; req(d); cols <- names(d)
    g_ev <- guess(cols, c("causa", "cause", "disease", "evento", "event"))
    g_type <- guess(cols, c("codetype", "code_type", "type", "tipo", "system", "codif"), g_ev)
    g_code <- guess(cols, c("dxcode", "^code$", "codigo", "diag_cod", "^cod", "code"), c(g_ev, g_type))
    tagList(n_rows(d), layout_columns(
      selectInput("k_dcause_event", "Cause", cols, g_ev),
      selectInput("k_dcause_type", "Code type", cols, g_type),
      selectInput("k_dcause_code", "Code", cols, g_code)))
  })
  # Prepared on its own, with the role fixed by the slot: these are causes of death and nothing
  # else, so nobody has to declare it and nobody can declare it wrong.
  cause_definition <- reactive({
    req(rv$dcause, input$k_dcause_event)
    d <- rv$dcause[, .(event = trimws(get(input$k_dcause_event)), type = get(input$k_dcause_type),
                       code = get(input$k_dcause_code), role = "mortality")]
    prepare_definition(d, "event", "type", "code", col_role = "role")
  })
  output$keys_datc <- renderUI({
    d <- rv$datc; req(d); cols <- names(d)
    g_ev <- guess(cols, PAT_EVENT)
    g_type <- guess(cols, c("codetype", "code_type", "tipo", "type", "codif", "system", "vocab"), g_ev, optional = TRUE)
    g_atc <- guess(cols, c("atc", "rxcui", "^code$", "codigo", "code", "cod"), c(g_ev, g_type))
    g_min <- guess(cols, c("min"), c(g_ev, g_type, g_atc), optional = TRUE)
    tagList(n_rows(d), layout_columns(
      selectInput("k_datc_event", "Disease", cols, g_ev), selectInput("k_datc_atc", "Drug code", cols, g_atc),
      selectInput("k_datc_type", "Code type (optional: ATC if absent)", c(NONE, cols), g_type),
      selectInput("k_datc_min", "Min. dispensations (optional)", c(NONE, cols), g_min),
      selectInput("k_datc_role", "Role (optional)", c(NONE, cols), guess(cols, c("^role$", "rol", "papel"), c(g_ev, g_type, g_atc, g_min), optional = TRUE)),
      selectInput("k_datc_src", "Source or decision (optional)", c(NONE, cols),
                  guess(cols, c("^source$", "fuente", "origen", "referencia", "decision"), c(g_ev, g_type, g_atc, g_min), optional = TRUE))))
  })

  # ---- quick checks on linked fields (sample) ----
  preview <- function(dt) renderReactable(reactable(head(dt, 5), compact = TRUE))
  check_box <- function(ok, ...) div(class = if (ok) "alert alert-light mt-2" else "alert alert-warning mt-2", ...)
  sample_note <- function(d) if (nrow(d) > CHECK_SAMPLE) paste0(" (sample of ", fmt_n(CHECK_SAMPLE), " rows)")
  output$check_pdx <- renderUI({
    d <- rv$pdx; req(d, input$k_pdx_id)
    k <- c(input$k_pdx_id, input$k_pdx_code, input$k_pdx_type, input$k_pdx_date)
    validate(need(distinct_cols(k), "Each key field must be a different column."))
    s <- sample_of(d); fmt_d <- detect_date_format(s[[k[4]]]); dates <- as_date(s[[k[4]]], fmt_d); sys <- coding_system(s[[k[3]]])
    types <- s[, .N, by = .(type = get(k[3]))][, version := coding_version(type)][order(-N)]
    tagList(h6("Preview (linked fields)"),
      preview(s[, .(patient_id = get(k[1]), code = get(k[2]), code_type = get(k[3]), date = get(k[4]))]),
      check_box(mean(!is.na(dates)) >= 0.95 && mean(!is.na(sys)) >= 0.95,
        "Date format detected: ", strong(fmt_label(fmt_d)), " · readable dates: ", strong(pct(mean(!is.na(dates)))),
        " · recognised code types: ", strong(pct(mean(!is.na(sys)))),
        sample_note(d), br(), "Code types found: ",
        paste(types[, paste0(type, " → ", fifelse(is.na(version), "NOT RECOGNISED", version))], collapse = "; ")))
  })
  output$check_pdrug <- renderUI({
    d <- rv$pdrug; req(d, input$k_pdrug_id)
    k <- c(input$k_pdrug_id, input$k_pdrug_atc, input$k_pdrug_date)
    kt <- opt(input$k_pdrug_type)
    validate(need(distinct_cols(c(k, kt)), "Each key field must be a different column."))
    s <- sample_of(d); fmt_d <- detect_date_format(s[[k[3]]]); dates <- as_date(s[[k[3]]], fmt_d)
    sys <- if (is.null(kt)) rep("ATC", nrow(s)) else coding_system(s[[kt]])
    ok_sys <- !is.na(sys) & sys %in% DRUG_SYSTEMS
    tagList(h6("Preview (linked fields)"),
      preview(s[, c(.(patient_id = get(k[1]), drug_code = get(k[2])),
                    if (!is.null(kt)) .(code_type = get(kt)) else NULL, .(date = get(k[3])))]),
      check_box(mean(!is.na(dates)) >= 0.95 && mean(ok_sys) >= 0.95,
        "Date format detected: ", strong(fmt_label(fmt_d)), " · readable dates: ", strong(pct(mean(!is.na(dates)))),
        " · rows in a drug coding system: ", strong(pct(mean(ok_sys))),
        if (is.null(kt)) " (no code-type column: read as ATC)" else
          paste0(" · types found: ", paste(unique(stats::na.omit(paste0(s[[kt]], " → ", coding_version(s[[kt]])))), collapse = "; ")),
        sample_note(d)))
  })
  output$check_ddx <- renderUI({
    d <- rv$ddx; req(d, input$k_ddx_event)
    k <- c(input$k_ddx_event, input$k_ddx_type, input$k_ddx_code)
    validate(need(distinct_cols(k), "Each key field must be a different column."))
    sys <- coding_system(d[[k[2]]])
    tagList(h6("Preview (linked fields)"),
      preview(d[, .(disease = get(k[1]), code_type = get(k[2]), code = get(k[3]))]),
      check_box(all(!is.na(sys)), uniqueN(d[[k[1]]]), " diseases · recognised code types: ", strong(pct(mean(!is.na(sys)))),
                br(), "Versions: ", paste(unique(paste0(d[[k[2]]], " → ", fifelse(is.na(sys), "NOT RECOGNISED", coding_version(d[[k[2]]])))), collapse = "; ")))
  })
  output$check_datc <- renderUI({
    d <- rv$datc; req(d, input$k_datc_event)
    k <- c(input$k_datc_event, input$k_datc_atc)
    validate(need(distinct_cols(k, opt(input$k_datc_min)), "Each key field must be a different column."))
    atc_ok <- grepl("^[A-Z][0-9]{2}", toupper(trimws(d[[k[2]]])))
    tagList(h6("Preview (linked fields)"),
      preview(d[, .(disease = get(k[1]), atc = get(k[2]))]),
      check_box(all(atc_ok), uniqueN(d[[k[1]]]), " diseases · values with ATC format: ", strong(pct(mean(atc_ok)))))
  })

  # ---- definitions linked into one table for the engine ----
  def_linked <- reactive({
    parts <- list()
    if (!is.null(rv$ddx) && !is.null(input$k_ddx_event))
      parts$dx <- rv$ddx[, .(event = trimws(get(input$k_ddx_event)), type = get(input$k_ddx_type),
                             code = get(input$k_ddx_code), min_disp = NA_character_,
                             role = if (!is.null(opt(input$k_ddx_role))) as.character(get(opt(input$k_ddx_role))) else NA_character_,
                             use = if (!is.null(opt(input$k_ddx_use))) as.character(get(opt(input$k_ddx_use))) else NA_character_,
                             vfrom = if (!is.null(opt(input$k_ddx_from))) as.character(get(opt(input$k_ddx_from))) else NA_character_,
                             vto = if (!is.null(opt(input$k_ddx_to))) as.character(get(opt(input$k_ddx_to))) else NA_character_,
                             src = if (!is.null(opt(input$k_ddx_src))) as.character(get(opt(input$k_ddx_src))) else NA_character_)]
    if (!is.null(rv$datc) && !is.null(input$k_datc_event))
      parts$atc <- rv$datc[, .(event = trimws(get(input$k_datc_event)),
                               type = if (!is.null(opt(input$k_datc_type))) as.character(get(opt(input$k_datc_type))) else "ATC",
                               code = get(input$k_datc_atc),
                               min_disp = if (!is.null(opt(input$k_datc_min))) as.character(get(input$k_datc_min)) else NA_character_,
                               role = if (!is.null(opt(input$k_datc_role))) as.character(get(opt(input$k_datc_role))) else NA_character_,
                               use = NA_character_, vfrom = NA_character_, vto = NA_character_,
                               src = if (!is.null(opt(input$k_datc_src))) as.character(get(opt(input$k_datc_src))) else NA_character_)]
    validate(need(length(parts) > 0, "Load at least one definition table."))
    rbindlist(parts)
  })
  events_info <- reactive({
    d <- def_linked()
    d[, .(has_dx = any(!coding_system(type) %in% DRUG_SYSTEMS), has_drug = any(coding_system(type) %in% DRUG_SYSTEMS),
          min_disp = suppressWarnings(max(as.numeric(min_disp), na.rm = TRUE))), by = event][
      !is.finite(min_disp), min_disp := 1][order(event)]
  })
  output$rules_ui <- renderUI({
    e <- events_info()
    rows <- lapply(seq_len(nrow(e)), function(i) {
      layout_columns(col_widths = c(4, 5, 3), class = "align-items-center",
        strong(e$event[i]),
        if (e$has_dx[i] && e$has_drug[i]) selectInput(paste0("rule_", i), NULL, RULE_LABELS, "dx_or_drug")
        else span(class = "text-muted", if (e$has_drug[i]) "Drug only" else "Diagnosis only"),
        if (e$has_drug[i]) numericInput(paste0("min_", i), NULL, e$min_disp[i], min = 1, step = 1) else span())
    })
    tagList(layout_columns(col_widths = c(4, 5, 3), em("Disease"), em("Rule"), em("Min. dispensations")), rows)
  })
  definition <- reactive({
    d <- def_linked(); e <- events_info()
    e[, rule := vapply(seq_len(.N), function(i)
      if (has_dx[i] && has_drug[i]) input[[paste0("rule_", i)]] %||% "dx_or_drug" else if (has_drug[i]) "drug" else "dx", "")]
    e[, min_n := vapply(seq_len(.N), function(i) if (has_drug[i]) as.numeric(input[[paste0("min_", i)]] %||% min_disp[i]) else NA_real_, 0)]
    d <- merge(d[, .(event, type, code, role, use, vfrom, vto)], e[, .(event, rule, min_n)], by = "event")
    tryCatch(prepare_definition(d, "event", "type", "code", "rule", "min_n", "role",
                                col_use = "use", col_from = "vfrom", col_to = "vto"),
             error = function(err) validate(need(FALSE, conditionMessage(err))))
  })
  output$warn_def <- renderUI({
    req(!is.null(rv$ddx) || !is.null(rv$datc))   # the rules panel already says a table is missing
    w <- definition()$warnings
    if (!is.null(rv$pdx) && !is.null(input$k_pdx_type) && !is.null(rv$ddx) && !is.null(input$k_ddx_type))
      w <- c(w, version_warnings(unique(sample_of(rv$pdx)[[input$k_pdx_type]]), unique(rv$ddx[[input$k_ddx_type]])))
    # Coding systems present on each side. Counted on the whole column, not on a sample, because
    # the message gives the number of rows that cannot match.
    if (!is.null(rv$pdx) && !is.null(input$k_pdx_type)) {
      tt <- table(rv$pdx[[input$k_pdx_type]])
      rows <- rows_by_system(names(tt), as.numeric(tt))
      if (!is.null(rv$pdrug)) {
        kt <- opt(input$k_pdrug_type)
        dr <- if (is.null(kt)) stats::setNames(nrow(rv$pdrug), "ATC") else
          rows_by_system(names(table(rv$pdrug[[kt]])), as.numeric(table(rv$pdrug[[kt]])))
        rows <- c(rows[setdiff(names(rows), names(dr))], dr)
      }
      w <- c(w, system_coverage_warnings(rows, definition()$codes$system))
    }
    w <- c(w, reserved_event_warnings(definition()$rules$event))
    if (!length(w)) div(class = "alert alert-success mt-2", "Definitions linked without warnings.")
    else div(class = "alert alert-warning mt-2", tags$ul(lapply(w, tags$li)))
  })
  output$template <- downloadHandler("definition_templates.xlsx", function(file) write_xlsx(templates, file))

  # ---- search ----
  search_result <- eventReactive(input$run, {
    validate(need(!is.null(rv$pdx) || !is.null(rv$pdrug), "Load at least one patient table (diagnoses or drugs) in tab 1."),
             need(!is.null(rv$ddx) || !is.null(rv$datc), "Load at least one definition table in tab 2."),
             need(is.null(rv$pdx) || !is.null(input$k_pdx_id), "Check the key fields of the patient – diagnosis table in tab 1."),
             need(is.null(rv$ddx) || !is.null(input$k_ddx_event), "Check the key fields of the definition tables in tab 2."))
    session$sendCustomMessage("clock_start", "clock_run")
    on.exit(session$sendCustomMessage("clock_stop", "clock_run"))
    p <- Progress$new(session, min = 0, max = 1); on.exit(p$close(), add = TRUE)
    step <- new_progress(p)
    t0 <- Sys.time()
    tryCatch({
      prep_dx <- NULL; prep_drug <- NULL
      if (!is.null(rv$pdx)) {
        step(0.01, "Preparing diagnoses")
        prep_dx <- prepare_diagnoses(rv$pdx, input$k_pdx_id, input$k_pdx_code, input$k_pdx_type, input$k_pdx_date,
                                     drop_procedures = input$drop_proc)
      }
      if (!is.null(rv$pdrug)) {
        step(0.15, "Preparing dispensations")
        prep_drug <- prepare_drugs(rv$pdrug, input$k_pdrug_id, input$k_pdrug_atc, input$k_pdrug_date,
                                   opt(input$k_pdrug_type))
      }
      # Causes of death come in their own table and are never searched in the diagnosis table:
      # a hospital diagnosis of hepatitis C is not a death from hepatitis C.
      def <- definition()
      cd <- if (!is.null(rv$dcause) && !is.null(input$k_dcause_event)) cause_definition() else NULL
      cause_codes <- if (is.null(cd)) NULL else cd$codes
      mort <- if (is.null(cd)) character(0) else cd$rules$event
      r <- find_events(prep_dx$data, prep_drug$data, def, include_without_event = input$without_event,
                       progress = function(f, txt) step(0.2 + 0.8 * f, txt))
      # The patient table adds its own events (death, its cause, end of follow-up) to the same
      # long table, so every tab downstream sees them like any other event.
      pt <- NULL; groups <- NULL; other_causes <- NULL
      roles <- event_roles(def$rules, cause_events = mort)
      # One event for entry into the population, whatever definition found the person: every
      # selector asks when they entered, not through which code. The funnel still breaks it down.
      ce <- cohort_entry(r$long, roles[role == "cohort" & event != "Cohort_entry", event])
      if (nrow(ce)) r$long <- rbindlist(list(r$long, ce), use.names = TRUE)
      if (!is.null(rv$ppat) && !is.null(input$k_ppat_id)) {
        step(0.95, "Reading the patient table")
        pt <- patients()
        dev <- patient_events(pt$data, cause_codes = cause_codes)
        other_causes <- attr(dev, "other_causes")
        if (nrow(dev)) r$long <- rbindlist(list(r$long, dev), use.names = TRUE)
        cohort_events <- roles[role == "cohort", event]
        groups <- population_groups(pt$data, r$long, cohort_events, mode = input$pop_mode,
                                    by = c("birth_year", "sex", opt(input$k_ppat_area)))
      }
      # One summary over everything that was actually found, definitions and patient table
      # alike: the pickers of the map and of the outcome tabs are filled from this, so an event
      # that exists must be offered even when no definition file declared it.
      found <- r$long[, .(persons = uniqueN(id)), by = event]
      r$summary <- merge(found, r$summary[, .(event, rule)], by = "event", all.x = TRUE)
      r$summary <- merge(r$summary, roles, by = "event", all.x = TRUE)
      r$summary[is.na(rule), rule := "patient table"]
      r$summary[is.na(role), role := "disease"]
      setorder(r$summary, -persons)
      step(1, "Done")
      c(r, list(prep_dx = prep_dx$summary, patients = pt, groups = groups, roles = roles,
                other_causes = other_causes, cause_events = mort, rules = def$rules,
                secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }, error = function(e) { showNotification(paste("Search error:", conditionMessage(e)), type = "error", duration = NULL); NULL })
  })

  # ---- cohort filter ----
  # A marker says we saw something about the person -- that they injected, that they were in
  # prison -- and not that something happened to them. It never moves anybody's follow-up: the
  # cohort, its entry date and every other date stay exactly as the search found them. All it
  # decides is who the analysis is about, and that is chosen here, when the results are read,
  # so the same search can be looked at restricted and unrestricted without running again.
  # Several markers are required together: each one narrows the cohort further.
  result <- reactive({
    r <- search_result(); req(r)
    keep <- intersect(input$restrict %||% character(0), unique(r$long$event))
    if (!length(keep)) return(r)
    ids <- Reduce(intersect, lapply(keep, function(e) r$long[event == e, unique(id)]))
    r$long <- r$long[id %in% ids]
    if (!is.null(r$wide))     r$wide <- r$wide[id %in% ids]
    # Both of these are lists with a $data inside, not tables: subsetting the list itself would
    # quietly drop $data and leave every downstream filter matching nobody.
    if (!is.null(r$groups$data))   r$groups$data   <- r$groups$data[as.character(id) %in% ids]
    if (!is.null(r$patients$data)) r$patients$data <- r$patients$data[as.character(id) %in% ids]
    # copy(): the summary of the unrestricted search must survive untouched for the next filter.
    found <- r$long[, .(persons = uniqueN(id)), by = event]
    s <- merge(copy(r$summary)[, persons := NULL], found, by = "event", all.x = TRUE)
    s[is.na(persons), persons := 0L]
    setorder(s, -persons)
    r$summary <- s
    r$restricted <- keep
    r
  })

  output$restrict_ui <- renderUI({
    r <- search_result(); req(r)
    m <- r$summary[role == "marker" & persons > 0]
    if (!nrow(m)) return(NULL)
    tagList(
      hr(class = "my-2"),
      checkboxGroupInput("restrict", NULL,
        choiceNames = lapply(seq_len(nrow(m)), function(i)
          span(strong(m$event[i]), span(class = "text-muted", " · ", fmt_n(m$persons[i]), " persons"))),
        choiceValues = m$event, selected = isolate(input$restrict)),
      p(class = "small text-muted mb-0",
        "Restricts every tab below to these people. Nobody's follow-up changes: the dates, the ",
        "entry into the cohort and the events found are the same, only the cohort is smaller."))
  })
  observeEvent(result(), if (!is.null(result())) send_hint(session, "results"))
  output$res_info <- renderUI({
    r <- result()
    validate(need(!is.null(r), "The search could not be completed: see the message above or check tabs 1 and 2."))
    # Definition codes that matched nothing, only for systems compared by equality: there a
    # typo finds nothing and would otherwise be invisible.
    w <- unmatched_warnings(r$unmatched)
    tagList(
      div(class = "alert alert-success mt-3", fmt_n(uniqueN(r$long$id)), " persons with at least one event · ",
          fmt_n(nrow(r$long)), " person-event pairs · ", fmt_t(r$secs)),
      if (length(r$restricted))
        div(class = "alert alert-info py-2",
            "Everything below is restricted to ", strong(paste(r$restricted, collapse = " + ")),
            ": ", fmt_n(uniqueN(r$long$id)), " of ",
            fmt_n(uniqueN(search_result()$long$id)), " persons."),
      if (length(w)) div(class = "alert alert-warning", tags$ul(lapply(w, tags$li))))
  })
  output$match_info <- renderUI({
    r <- result()
    if (is.null(r) || is.null(r$groups)) return(p(class = "small text-muted",
      "Load the patient table and run the search to see how many users were matched."))
    g <- r$groups
    if (identical(g$mode, "cohort")) return(NULL)
    lost <- g$summary[grepl("dropped", step), rows]
    tagList(reactable(g$summary, compact = TRUE, striped = TRUE, defaultPageSize = 6),
      if (length(lost) && lost > 0) div(class = "alert alert-warning mt-2",
        fmt_n(lost), " people who use drugs were left out: their cell could not supply two controls."))
  })
  output$tab_summary <- renderReactable({ r <- result(); req(r); reactable(r$summary, compact = TRUE, striped = TRUE) })
  output$tab_prep <- renderReactable({ r <- result(); req(r, r$prep_dx); reactable(r$prep_dx, compact = TRUE) })
  output$ev_map_ui <- renderUI({
    r <- result()
    if (is.null(r)) return(p(class = "text-muted small", "Run the search in tab 3 first."))
    ev <- r$summary[event %in% analysis_events()][order(factor(role, levels = ROLES), -persons)]
    ch <- stats::setNames(ev$event, paste0(ev$event, "  (", ev$role, ")"))
    checkboxGroupInput("ev_map", "Events to include", choices = ch, selected = ev$event)
  })
  output$mod_covars_ui <- renderUI({
    d <- mod_data(); req(d)
    cand <- intersect(c("group", "sex", "age_band", "era"), names(d))
    cand <- c(cand, setdiff(names(d)[vapply(d, function(x) is.factor(x) || is.character(x), TRUE)],
                            c(cand, "id", "status_label")))
    checkboxGroupInput("mod_covars", "Adjust for", choices = cand, inline = TRUE,
                       selected = intersect(c("group", "sex", "age_band"), cand))
  })
  output$tab_funnel <- renderReactable({
    r <- result(); req(r)
    f <- phase_funnel(r$long, r$roles, disease = input$funnel_disease,
                      n_persons = if (!is.null(r$patients)) nrow(r$patients$data) else NULL,
                      groups = r$groups, cause_events = r$cause_events, rules = r$rules)
    reactable(f, compact = TRUE, striped = TRUE, groupBy = "phase", defaultPageSize = 20,
      columns = list(
        phase = colDef(name = "Phase"), step = colDef(name = "Step", minWidth = 180),
        persons = colDef(name = "People", format = colFormat(separators = TRUE)),
        of_previous = colDef(name = "% of the step above",
                             format = colFormat(suffix = "%", digits = 1))))
  })
  output$other_causes <- renderUI({
    r <- result(); req(r)
    oc <- r$other_causes
    if (is.null(oc) || !nrow(oc)) return(NULL)
    tagList(h6("What the competing event is made of"),
      p(class = "small text-muted",
        "Death_other_cause is death from any cause not associated with the disease under study, and ",
        "this is its composition. Nothing is inferred from these codes."),
      reactable(head(oc, 25), compact = TRUE, striped = TRUE, defaultPageSize = 8))
  })
  output$tab_res <- renderReactable({ req(result()); reactable(head(analysis_table(), 200), compact = TRUE, defaultPageSize = 10) })
  # One row per person with everything the later analyses use: the covariates, the group the
  # cohort decision produced, and the first date of every event. This is the analysis dataset.
  analysis_table <- reactive({
    r <- result(); req(r)
    d <- as.data.table(r$wide)[, id := as.character(id)]
    if (!is.null(r$patients)) {
      pt <- as.data.table(r$patients$data)[, id := as.character(id)]
      d <- merge(pt[, setdiff(names(pt), c("death_code", "death_type")), with = FALSE], d, by = "id", all.y = TRUE)
    }
    g <- analysis_groups()
    if (!is.null(g)) d <- merge(g, d, by = "id", all.x = TRUE)
    else if (!is.null(r$groups)) d <- d[id %in% as.character(r$groups$data$id)]
    d[]
  })
  output$download <- downloadHandler(function() paste0("analysis_table_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) fwrite(analysis_table(), file, sep = ";"))

  # An event cannot play two parts at once: the index event is not available as outcome,
  # competing or censoring, and the competing event cannot be the outcome. Done by taking the
  # choices away rather than by complaining afterwards.
  cr_choices <- function(exclude, optional = TRUE) {
    ev <- setdiff(analysis_events(), exclude)
    if (optional) c("(none)", ev) else ev
  }
  cr_keep <- function(id, choices) {
    cur <- input[[id]]
    updateSelectInput(session, id, choices = choices,
                      selected = if (!is.null(cur) && cur %in% choices) cur else choices[1])
  }
  observeEvent(input$cr_index, ignoreInit = TRUE, {
    req(result())
    cr_keep("cr_outcome", cr_choices(input$cr_index, optional = FALSE))
    for (i in c("cr_competing", "cr_censor", "cr_group")) cr_keep(i, cr_choices(input$cr_index))
  })
  observeEvent(input$cr_outcome, ignoreInit = TRUE, {
    req(result())
    for (i in c("cr_competing", "cr_censor")) cr_keep(i, cr_choices(c(input$cr_index, input$cr_outcome)))
  })

  # The population the analysis runs on, which is what the cohort decision selects: in cohort
  # mode everyone in the data, in population mode the matched users and their controls. Tabs 5
  # and 6 read this, never the raw event table, so what is analysed is what the funnel counted.
  analysis_long <- reactive({
    r <- result(); req(r)
    if (is.null(r$groups)) return(r$long)
    r$long[as.character(id) %in% as.character(r$groups$data$id)]
  })
  # What any analysis is about, and nothing else: birth, entry into the population, the
  # diseases, the treatments, and the two deaths that make a competing-risks pair. The
  # individual definitions that found the cohort, the comorbidities, crude death and death with
  # no cause are not offered anywhere: they are read in the funnel and in the tables, not
  # chosen. One list, so every picker in the app offers the same thing.
  analysis_events <- reactive({
    r <- result(); req(r)
    s <- r$summary[persons > 0]
    ev <- unique(c(s[role == "birth", event], s[event == "Cohort_entry", event],
                   s[role == "disease", event], s[role == "treatment", event],
                   s[role == "mortality" & !event %in% RESERVED_EVENTS, event],
                   s[event == "Death_other_cause", event]))
    if (length(ev)) ev else s$event
  })

  # The group the analysis compares: users against controls when the population was defined,
  # otherwise it is left to the treatment event chosen in each tab.
  analysis_groups <- reactive({
    r <- result()
    if (is.null(r) || is.null(r$groups) || !identical(r$groups$mode, "population")) return(NULL)
    as.data.table(r$groups$data)[, .(id = as.character(id), group)]
  })

  # ---- outcomes over calendar time ----
  oc_years <- reactive({ req(input$oc_years); seq(input$oc_years[1], input$oc_years[2]) })
  oc_exit <- reactive(if (is.null(input$oc_exit) || input$oc_exit == "(none)") NULL else input$oc_exit)
  oc_long <- reactive(analysis_long())
  # Restricting the cohort can empty an event that was chosen before the restriction. The
  # selectors are refreshed for the new cohort, but the outputs run once before that round-trip
  # finishes, so they are told plainly instead of failing on a stale choice.
  in_cohort <- function(events) {
    r <- result(); req(r)
    events <- events[!is.null(events)]; events <- events[nzchar(events)]
    gone <- setdiff(events, unique(r$long$event))
    validate(need(!length(gone),
      paste0("Nobody in this cohort has ", paste(gone, collapse = ", "),
             if (length(r$restricted)) paste0(" (restricted to ", paste(r$restricted, collapse = " + "), ")"),
             ". Choose another event.")))
  }
  oc_fin <- reactive(max(oc_long()$date, na.rm = TRUE))

  output$oc_aviso <- renderUI({
    r <- result()
    if (is.null(r)) return(div(class = "alert alert-warning mt-2", "Run the search in tab 3 first."))
    req(input$oc_index, input$oc_outcome); in_cohort(c(input$oc_index, input$oc_outcome))
    n_idx <- uniqueN(r$long[event == input$oc_index, id])
    div(class = "alert alert-light mt-2",
        strong(fmt_n(n_idx)), " people with the index event (", input$oc_index, "). ",
        "Follow-up runs to ", strong(format(oc_fin())),
        if (!is.null(oc_exit())) paste0(" or to the exit event (", oc_exit(), "), whichever comes first"), ".")
  })

  # The comparison of tab 5: the population group when the cohort was defined, or any event,
  # typically the treatment. Without one, everything is a single group and the views are as before.
  oc_groups <- reactive({
    g <- input$oc_group
    if (is.null(g) || g == "(none)") return(NULL)
    if (identical(g, "Cohort group")) return(analysis_groups())
    group_from_event(oc_long(), g, input$oc_index)
  })
  tabla_cascada <- reactive({
    req(input$oc_index, input$oc_outcome); in_cohort(c(input$oc_index, input$oc_outcome, oc_exit()))
    by_group(oc_long(), oc_groups(), function(x)
      cumulative_uptake(x, input$oc_index, input$oc_outcome, oc_exit(), oc_years(), oc_fin()))
  })
  tabla_eventos <- reactive({ req(input$oc_events); in_cohort(input$oc_events)
    events_by_year(oc_long(), input$oc_events, oc_years()) })
  tabla_tasas <- reactive({
    req(input$oc_index, input$oc_outcome); in_cohort(c(input$oc_index, input$oc_outcome, oc_exit()))
    by_group(oc_long(), oc_groups(), function(x)
      rates_by_year(x, input$oc_index, input$oc_outcome, oc_exit(), input$oc_per %||% 1000,
                    oc_years(), oc_fin(), input$oc_stop))
  })

  tema <- function(p) p + theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank())

  output$oc_plot_cascade <- renderPlot({
    d <- tabla_cascada(); validate(need(nrow(d) > 0, "No people with that index event."))
    barras <- melt(d[, .(year, with_outcome, without_outcome)], id.vars = "year",
                   variable.name = "estado", value.name = "personas")
    barras[, estado := factor(estado, c("without_outcome", "with_outcome"),
                              c("Without outcome", "With outcome"))]
    barras[, group := rep(d$group, 2)]
    escala <- max(d$with_index_event, na.rm = TRUE) / 100
    tema(ggplot(barras, aes(year, personas, fill = estado)) +
      geom_col(width = 0.8) +
      geom_line(data = d, aes(year, pct_with_outcome * escala), inherit.aes = FALSE, linewidth = 1, colour = "#2c3e50") +
      geom_point(data = d, aes(year, pct_with_outcome * escala), inherit.aes = FALSE, size = 1.8, colour = "#2c3e50") +
      scale_fill_manual(values = c("Without outcome" = "#b8c4cf", "With outcome" = "#18bc9c")) +
      (if (uniqueN(d$group) > 1) facet_wrap(~ group) else NULL) +
      scale_y_continuous(name = "People (cumulative)",
                         sec.axis = sec_axis(~ . / escala, name = "% with outcome")) +
      labs(x = NULL, title = paste0("Cumulative people with ", input$oc_index,
                                    " and proportion reaching ", input$oc_outcome)))
  })

  output$oc_plot_events <- renderPlot({
    d <- tabla_eventos(); validate(need(nrow(d) > 0, "Select at least one event."))
    tema(ggplot(d, aes(year, persons, colour = event)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
      labs(x = NULL, y = "People (first event)", title = "People reaching each event, by calendar year"))
  })

  # The headline number of this tab: one rate over the whole period, per group, with its exact
  # Poisson interval. The figure shows the shape; this is the result.
  output$oc_rate_total <- renderUI({
    d <- tabla_tasas(); req(nrow(d))
    per <- input$oc_per %||% 1000
    tot <- d[, .(events = sum(events), person_years = sum(person_years)), by = group]
    tot <- tot[person_years > 0]
    if (!nrow(tot)) return(NULL)
    tot[, rate := events / person_years * per]
    ci <- tot[, { k <- stats::poisson.test(events, person_years)
                  .(lo = k$conf.int[1] * per, hi = k$conf.int[2] * per) }, by = group]
    tot <- merge(tot, ci, by = "group")
    linea <- function(i) tags$li(
      if (tot$group[i] != "All") tags$b(paste0(tot$group[i], ": ")),
      tags$b(format(round(tot$rate[i], 2), nsmall = 2)), " per ", fmt_n(per), " person-years",
      " (95% CI ", format(round(tot$lo[i], 2), nsmall = 2), " to ", format(round(tot$hi[i], 2), nsmall = 2), ")",
      tags$span(class = "text-muted", paste0(" \u2014 ", fmt_n(tot$events[i]), " events in ",
                                             fmt_n(round(tot$person_years[i])), " person-years")))
    div(class = "alert alert-light",
        strong(paste0("Rate of ", input$oc_outcome, " among people with ", input$oc_index,
                      ", over ", min(oc_years()), "\u2013", max(oc_years()), ":")),
        tags$ul(class = "mb-0", lapply(seq_len(nrow(tot)), linea)))
  })

  output$oc_plot_rates <- renderPlot({
    d <- tabla_tasas(); validate(need(nrow(d) > 0, "No follow-up time for that index event."))
    varios <- uniqueN(d$group) > 1
    tema(ggplot(d, aes(year, rate, colour = group, fill = group)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, colour = NA) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
      (if (varios) list() else list(scale_colour_manual(values = c(All = "#18bc9c"), guide = "none"),
                                    scale_fill_manual(values = c(All = "#18bc9c"), guide = "none"))) +
      labs(x = NULL, y = paste0(input$oc_outcome, " per ", fmt_n(input$oc_per %||% 1000), " person-years"),
           title = paste0("Rate of ", input$oc_outcome, " among people with ", input$oc_index,
                          ", by calendar year"), subtitle = "Shaded area: 95% confidence interval"))
  })

  output$oc_tab_cascade <- renderReactable(reactable(tabla_cascada(), compact = TRUE, striped = TRUE, defaultPageSize = 12))
  output$oc_tab_events <- renderReactable(reactable(tabla_eventos(), compact = TRUE, striped = TRUE, defaultPageSize = 12))
  output$oc_tab_rates <- renderReactable(reactable(tabla_tasas(), compact = TRUE, striped = TRUE, defaultPageSize = 12))
  descarga <- function(nombre, datos) downloadHandler(
    function() paste0(nombre, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) fwrite(datos(), file, sep = ";"))
  output$oc_dl_cascade <- descarga("cascade", tabla_cascada)
  output$oc_dl_events <- descarga("events_by_year", tabla_eventos)
  output$oc_dl_rates <- descarga("rates_by_year", tabla_tasas)

  # ---- time to event with competing risks ----
  cr_opt <- function(x) if (is.null(x) || x == "(none)") NULL else x
  cr_tte <- reactive({
    r <- result(); req(r, input$cr_index, input$cr_outcome)
    validate(need(input$cr_index != input$cr_outcome, "The index event and the outcome must be different."))
    in_cohort(c(input$cr_index, input$cr_outcome, cr_opt(input$cr_competing),
                cr_opt(input$cr_censor), cr_opt(input$cr_group)))
    time_to_event(analysis_long(), input$cr_index, input$cr_outcome, cr_opt(input$cr_competing),
                  cr_opt(input$cr_censor), cr_opt(input$cr_group),
                  end_date = max(r$long$date, na.rm = TRUE),
                  landmark_months = input$cr_landmark %||% 0)
  })
  cr_summary <- reactive(event_summary(cr_tte(), input$cr_per %||% 1000))
  cr_cif <- reactive(cumulative_incidence(cr_tte()))

  output$cr_note <- renderUI({
    r <- result()
    if (is.null(r)) return(div(class = "alert alert-warning mt-2", "Run the search in tab 3 first."))
    t <- cr_tte(); s <- cr_summary()
    div(class = "alert alert-light mt-2",
        strong(fmt_n(nrow(t))), " people followed from ", strong(input$cr_index),
        if ((input$cr_landmark %||% 0) > 0) paste0(" + ", input$cr_landmark, " months (landmark)"), ". ",
        strong(fmt_n(sum(s$outcome))), " reached ", strong(input$cr_outcome),
        if (!is.null(cr_opt(input$cr_competing)))
          span(", ", strong(fmt_n(sum(s$competing))), " had the competing event (", input$cr_competing, ")"),
        ", ", strong(fmt_n(sum(s$censored))), " censored.",
        if (is.null(cr_opt(input$cr_competing)))
          span(class = "text-muted", br(), "Without a competing event the curve is 1 - Kaplan-Meier. ",
               "If a competing event can prevent the outcome, pick it: otherwise the risk is overestimated."),
        # Said here and not only in a method section: the group is "ever", so whoever reads the
        # ratio has to know what it cannot separate.
        if (!is.null(cr_opt(input$cr_group)))
          span(class = "text-muted", br(), "The group is whoever ever had ", strong(input$cr_group),
               ", whenever it happened, not only before time zero. A person had to survive until ",
               "then to count in that group, so part of any difference is that head start and not ",
               "the intervention. Report it as such."))
  })

  output$cr_plot <- renderPlot({
    d <- cr_cif(); validate(need(nrow(d) > 0, "No follow-up time with those events."))
    n_grupos <- uniqueN(d$group)
    p <- ggplot(d, aes(time, 100 * cif, colour = event)) +
      geom_step(linewidth = 0.9) +
      labs(x = "Years since time zero", y = "Cumulative incidence (%)",
           title = paste0("Cumulative incidence of ", input$cr_outcome, " since ", input$cr_index),
           subtitle = if (!is.null(cr_opt(input$cr_competing)))
             paste0("Aalen-Johansen, with ", input$cr_competing, " as competing event")
           else "Without a competing event (1 - Kaplan-Meier)")
    if (n_grupos > 1) p <- p + facet_wrap(~ group)
    tema(p)
  })

  # The person-level table with its covariates measured at the start of follow-up.
  mod_data <- reactive({
    r <- result(); req(r)
    model_data(cr_tte(), patients = if (!is.null(r$patients)) r$patients$data else NULL,
               eras = rv$eras, age_breaks = c(35, 50, 65))
  })
  mod_fit <- reactive({
    req(input$mod_covars)
    fit_models(mod_data(), input$mod_covars, outcome_label = input$cr_outcome)
  })
  output$mod_warn <- renderUI({
    w <- mod_fit()$warnings
    if (!length(w)) return(NULL)
    div(class = "alert alert-warning", tags$ul(lapply(w, tags$li)))
  })
  output$mod_table <- renderReactable({
    t <- mod_fit()$table
    validate(need(!is.null(t) && nrow(t), "No estimate could be produced: see the message above."))
    reactable(t, compact = TRUE, striped = TRUE, groupBy = "model", defaultPageSize = 20,
      columns = list(
        model = colDef(name = "Model"), term = colDef(name = "Term", minWidth = 170),
        estimate = colDef(name = "Ratio", format = colFormat(digits = 2)),
        ci_low = colDef(name = "95% CI low", format = colFormat(digits = 2)),
        ci_high = colDef(name = "95% CI high", format = colFormat(digits = 2)),
        p = colDef(name = "p", format = colFormat(digits = 4))))
  })
  output$mod_dl <- downloadHandler(function() paste0("model_estimates_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) fwrite(mod_fit()$table, file, sep = ";"))

  output$cr_tab_summary <- renderReactable({
    s <- cr_summary()
    setnames(s, c("outcome", "competing", "outcome_rate", "competing_rate"),
             c(input$cr_outcome, cr_opt(input$cr_competing) %||% "competing",
               paste0(input$cr_outcome, "_per_", fmt_n(input$cr_per %||% 1000), "_py"),
               paste0(cr_opt(input$cr_competing) %||% "competing", "_per_", fmt_n(input$cr_per %||% 1000), "_py")),
             skip_absent = TRUE)
    reactable(s, compact = TRUE, striped = TRUE)
  })
  output$cr_tab_tte <- renderReactable(reactable(head(cr_tte(), 200), compact = TRUE, defaultPageSize = 10))
  output$cr_dl_cif <- descarga("cumulative_incidence", cr_cif)
  output$cr_dl_summary <- descarga("event_summary", cr_summary)
  output$cr_dl_tte <- downloadHandler(
    function() paste0("time_to_event_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) fwrite(cr_tte(), file, sep = ";"))

  # ---- documentation of the definitions in use ----
  blank_na <- function(x) if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) "" else as.character(x)

  # Built from the linked definition table and the cause-of-death table, never from a text kept
  # beside them: a page that is written separately is a page that stops being true.
  defs_table <- reactive({
    parts <- list()
    d <- try(def_linked(), silent = TRUE)
    if (!inherits(d, "try-error") && !is.null(d))
      parts$def <- as.data.table(d)[, .(event, role = role, type, code,
                                        use = fifelse(is.na(use) | use == "", "case", use),
                                        valid_from = vfrom, valid_to = vto, source = src)]
    if (!is.null(rv$dcause) && !is.null(input$k_dcause_event))
      parts$cause <- rv$dcause[, .(event = trimws(get(input$k_dcause_event)), role = "mortality",
                                   type = as.character(get(input$k_dcause_type)),
                                   code = as.character(get(input$k_dcause_code)),
                                   use = "case", valid_from = NA_character_, valid_to = NA_character_,
                                   source = NA_character_)]
    validate(need(length(parts) > 0, "Load at least one definition table in tab 2."))
    out <- rbindlist(parts, use.names = TRUE)
    out[is.na(role) | role == "", role := "(derived)"]
    out[]
  })

  output$defs_doc <- renderUI({
    d <- defs_table()
    rules <- try(definition()$rules, silent = TRUE)
    evs <- unique(d$event)
    blocks <- lapply(evs, function(e) {
      x <- d[event == e]
      rl <- if (!inherits(rules, "try-error") && e %in% rules$event) rules[event == e] else NULL
      src <- unique(x$source[!is.na(x$source) & x$source != ""])
      # Rows are grouped by what makes them differ, so a definition of 222 codes reads as the
      # two or three decisions it really is and not as 222 lines.
      g <- x[, .(codes = paste(sort(unique(code)), collapse = ", "), n = .N),
             by = .(type, use, valid_from, valid_to, source)][order(type, use)]
      tags$details(class = "mb-2", open = NA,
        tags$summary(strong(e), span(class = "text-muted",
          " · ", x[1]$role, if (!is.null(rl)) paste0(" · ", rl$rule), " · ",
          nrow(x), if (nrow(x) == 1) " code" else " codes")),
        tags$table(class = "table table-sm table-bordered small mt-2",
          tags$thead(tags$tr(lapply(c("Coding system", "Use", "Valid from", "Valid to",
                                      "Codes", "Source or decision"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(g)), function(i) tags$tr(
            tags$td(g$type[i]),
            tags$td(if (identical(g$use[i], "date")) span(class = "text-warning-emphasis", "dates only") else "diagnoses"),
            tags$td(blank_na(g$valid_from[i])), tags$td(blank_na(g$valid_to[i])),
            tags$td(tags$code(class = "small", g$codes[i])),
            tags$td(if (is.na(g$source[i]) || !nzchar(g$source[i]))
                      em(class = "text-muted", "no source: decision taken on these data")
                    else g$source[i]))))))
    })
    tagList(p(class = "small text-muted mt-3", length(evs), " events · ", nrow(d), " code rows"), blocks)
  })

  # ---- the same thing in plain words, for someone who did not build it ----
  # Seven steps, in the order the package runs them. What is written here is read from the
  # definitions actually loaded, so a page that says "4 diseases" says it because there are four.
  step_box <- function(n, title, body, tone = "secondary") {
    div(class = "d-flex mb-3",
      div(class = paste0("flex-shrink-0 me-3 rounded-circle d-flex align-items-center ",
                         "justify-content-center fw-bold text-white bg-", tone),
          style = "width:2.2rem;height:2.2rem;", n),
      div(class = "flex-grow-1", h6(class = "mb-1", title), div(class = "small", body)))
  }
  chips <- function(x, tone = "light") if (!length(x)) em(class = "text-muted", "none") else
    lapply(sort(unique(x)), function(v) span(class = paste0("badge rounded-pill bg-", tone,
      " text-dark border me-1 mb-1"), v))

  # A source line is a sentence; a chip is a label. The citation is what makes it findable, so
  # that is what is kept: "Quan 2005", "Ball 2017", "EUDA", "INE". The full sentence stays one
  # sub-tab away, and arrives here as the chip's tooltip.
  short_src <- function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    # Under the "decisions" heading the word "Decision" on every chip is noise; what
    # distinguishes them is what was decided.
    x <- sub("^[Dd]ecision:\\s*", "", x)
    author <- regmatches(x, regexpr("^[A-Z][A-Za-z\u00c0-\u00ff]+ [12][0-9]{3}", x))
    out <- vapply(seq_along(x), function(i) {
      a <- regmatches(x[i], regexpr("^[A-Z][A-Za-z\u00c0-\u00ff]+ [12][0-9]{3}", x[i]))
      if (length(a)) return(a)
      acr <- regmatches(x[i], regexpr("^[A-Z]{3,6}\\b", x[i]))
      if (length(acr)) return(acr)
      w <- strsplit(x[i], "\\s+")[[1]]
      paste(w[seq_len(min(3L, length(w)))], collapse = " ")
    }, "")
    sub("[,;:.]$", "", out)
  }
  src_chips <- function(x, tone) {
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (!length(x)) return(em(class = "text-muted", "none"))
    lbl <- short_src(x)
    keep <- !duplicated(lbl)
    lapply(order(lbl[keep]), function(i) {
      j <- which(keep)[i]
      span(class = paste0("badge rounded-pill bg-", tone, " text-dark border me-1 mb-1"),
           title = x[j], lbl[keep][i])
    })
  }

  output$defs_plain <- renderUI({
    d <- defs_table()
    rl <- try(definition()$rules, silent = TRUE); if (inherits(rl, "try-error")) rl <- NULL
    role_of <- function(r) if (is.null(rl)) character(0) else sort(unique(rl[role == r, event]))
    sys <- sort(unique(d$type[!is.na(d$type)]))
    srcs <- unique(d$source[!is.na(d$source) & nzchar(d$source)])
    # A source that cites something and a source that records what we decided are not the same
    # kind of authority, so they are not shown as one list.
    is_dec <- grepl("^decision|decision|kept by|no source|standard codes", srcs, ignore.case = TRUE)
    lit <- srcs[!is_dec]; dec <- srcs[is_dec]
    marker <- role_of("marker"); dis <- role_of("disease"); tr <- role_of("treatment")
    coh <- role_of("cohort"); como <- role_of("comorbidity")
    cause <- if (!is.null(rv$dcause) && !is.null(input$k_dcause_event))
      sort(unique(trimws(as.character(rv$dcause[[input$k_dcause_event]])))) else character(0)
    n_dating <- d[use == "date", .N]; n_window <- d[!is.na(valid_from) | !is.na(valid_to), .N]

    tagList(
      div(class = "alert alert-light border",
        strong("In summary. "),
        "The package reads a list of codes for each disease, establishes for every person the ",
        "first date on which any of those codes appears, and derives from those dates who ",
        "entered the cohort, who acquired the infection, who was treated, who died and of what."),

      step_box(1, "Codes are searched in two input tables", tagList(
        p(class = "mb-1", "Diagnoses, from hospital and primary care, and drug dispensations. ",
          "No other source is read. A code is compared only within its own coding system, so an ",
          "ICD-9 code can never match an ICD-10 one. The systems present in this project are:"),
        div(chips(sys))), "primary"),

      step_box(2, "Each disease is a list of codes with a documented origin", tagList(
        p(class = "mb-1", nrow(d), " code rows across ", uniqueN(d$event), " events."),
        p(class = "mb-1 mt-2 text-muted", "Published literature or official lists (the full reference appears on hover):"),
        div(src_chips(lit, "info")),
        p(class = "mb-1 mt-2 text-muted", "Decisions taken on these data, where no published source covered the case:"),
        div(src_chips(dec, "warning")),
        p(class = "mb-0 mt-2", "Two properties a plain code list cannot express are recorded here. ",
          strong(n_dating), " code(s) may date a disease but never diagnose it, which is the case ",
          "of an unspecified code in a person already diagnosed by other means; and ", strong(n_window),
          " code(s) apply only within a range of dates, because the same code denoted a different ",
          "condition before it.")), "primary"),

      step_box(3, "A subset of those codes identifies injecting drug use", tagList(
        p(class = "mb-1", "These events carry the role of marker: they do not begin anyone's ",
          "follow-up and are never an outcome. They establish which population the analysis concerns."),
        div(chips(marker, "warning")),
        p(class = "mb-0", "A checkbox in tab 3 restricts every subsequent tab to those persons. ",
          "No date changes; the cohort is simply smaller.")), "warning"),

      step_box(4, "Every person becomes a sequence of first dates", tagList(
        p(class = "mb-1", "One date per person and per event: the first occurrence. That is the ",
          "entire data model. Entry into the cohort is the earliest of the drug-use definitions:"),
        div(chips(coh, "success")),
        p(class = "mb-1 mt-2", "Infections and other diseases:"), div(chips(dis, "info")),
        if (length(como)) tagList(p(class = "mb-1 mt-2", "Context, neither the population nor the disease under study:"),
                                  div(chips(como))),
        p(class = "mb-1 mt-2", "Treatments, which only a dispensation can establish, since a diagnosis never makes a person treated:"),
        div(chips(tr, "success")),
        p(class = "mb-0 mt-2", "Tab 4 renders those sequences as a process map: which events each ",
          "person passes through, in what order, and the median time between one and the next.")), "primary"),

      step_box(5, "Death is then added from the patient table", tagList(
        p(class = "mb-1", "The date of death and, where it exists, its cause. The cause has its own ",
          "list of codes and is never searched in the diagnosis table: a hospital diagnosis of ",
          "hepatitis C is not a death from hepatitis C. The causes defined here are:"),
        div(chips(cause, "danger")),
        p(class = "mb-0 mt-2", "A death with no recorded cause is kept as an event of its own rather ",
          "than merged into the others, so that the extent of missing cause information remains visible.")), "danger"),

      step_box(6, "Any other death competes with the outcome", tagList(
        p(class = "mb-1", "A person who dies of an overdose can no longer die of liver disease. ",
          "This is not censoring: censoring means observation stopped, whereas here the outcome ",
          "has become impossible."),
        p(class = "mb-0", "Every death that is not one of the causes above is therefore treated as ",
          "a competing event, rather than as a person who remains at risk.")), "danger"),

      step_box(7, "Two models are reported, because they answer different questions", tagList(
        tags$ul(class = "mb-1",
          tags$li(strong("Cause-specific hazard ratio"), " — among those still alive and at risk, ",
                  "how much faster does the outcome occur?"),
          tags$li(strong("Subdistribution hazard ratio (Fine–Gray)"), " — how much does the ",
                  "cumulative proportion of persons who ever reach the outcome differ?")),
        p(class = "mb-0", "The two may point in opposite directions, which is why both are reported ",
          "and neither is presented as ", em("the"), " hazard ratio. A treatment that prolongs ",
          "survival grants more time in which to reach the outcome, and only the second reflects that.")), "dark"),

      hr(),
      p(class = "small text-muted mb-0",
        "This page is generated from the definition tables loaded in tab 2 and the patient table ",
        "loaded in tab 1. A change to a definition changes this page with it."))
  })
  output$dl_defs <- downloadHandler(
    function() paste0("definitions_in_use_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) fwrite(defs_table(), file, sep = ";", bom = TRUE))

  # ---- process map ----
  observeEvent(result(), {
    r <- result(); req(r)
    dis <- r$summary[persons > 0 & role == "disease", event]
    updateSelectInput(session, "funnel_disease", choices = dis, selected = dis[1])
    ev <- analysis_events()
    # A sensible starting point: time zero is the disease under study, not whatever happens to
    # be the most frequent event. The user can still pick anything.
    first_dis <- r$summary[persons > 0 & role == "disease", event]
    idx <- if (length(first_dis)) first_dis[1] else ev[1]
    out1 <- setdiff(ev, idx)
    # The first event alphabetically is Birth, and "how many people with hepatitis C were born"
    # is not a question. The opening view is the one anybody wants: uptake of a treatment, or,
    # failing that, a cause of death.
    pick_out <- function(cands) {
      for (r0 in c("treatment", "mortality")) {
        v <- intersect(r$summary[persons > 0 & role == r0, event], cands)
        if (length(v)) return(v[1])
      }
      if (length(cands)) cands[1] else idx
    }
    # Grouped by role so the map picker reads as what it is: cohort, disease, treatment,
    # comorbidity and mortality, instead of one flat list.

    updateSelectInput(session, "oc_index", choices = ev, selected = idx)
    updateSelectInput(session, "oc_outcome", choices = ev, selected = pick_out(out1))
    updateSelectInput(session, "oc_exit", choices = c("(none)", ev), selected = "(none)")
    has_pop <- !is.null(r$groups) && identical(r$groups$mode, "population")
    # The comparison is almost always by treatment, so the treatments come first and under their
    # own heading instead of being three entries lost in a list of every event. Nothing is
    # removed: grouping by a comorbidity or by an infection is still one scroll away.
    treat <- r$summary[persons > 0 & role == "treatment", event]
    group_choices <- c(list("(none)" = "(none)"),
                       if (has_pop) list("Cohort group" = "Cohort group"),
                       if (length(treat)) list(Treatment = as.list(stats::setNames(treat, treat))),
                       list("Other events" = as.list(stats::setNames(setdiff(ev, treat), setdiff(ev, treat)))))
    updateSelectInput(session, "oc_group", choices = group_choices, selected = "(none)")
    updateSelectizeInput(session, "oc_events", choices = ev, selected = ev)
    rango <- range(year_of(r$long$date), na.rm = TRUE)
    updateSliderInput(session, "oc_years", min = rango[1], max = rango[2], value = rango)
    updateSelectInput(session, "cr_index", choices = ev, selected = idx)
    updateSelectInput(session, "cr_outcome", choices = out1, selected = pick_out(out1))
    updateSelectInput(session, "cr_competing", choices = c("(none)", ev), selected = "(none)")
    updateSelectInput(session, "cr_censor", choices = c("(none)", ev), selected = "(none)")
    updateSelectInput(session, "cr_group", choices = group_choices[names(group_choices) != "Cohort group"],
                      selected = "(none)")
  })
  map <- eventReactive(input$make_map, {
    r <- result()
    validate(need(!is.null(r), "Run the search first."), need(length(input$ev_map) >= 1, "Select at least one event."))
    session$sendCustomMessage("clock_start", "clock_map")
    on.exit(session$sendCustomMessage("clock_stop", "clock_map"))
    p <- Progress$new(session, min = 0, max = 1); on.exit(p$close(), add = TRUE)
    step <- new_progress(p)
    step(0.05, "Building the event log")
    lg <- suppressWarnings(build_eventlog(r$long, input$ev_map, roles = r$roles))
    step(0.5, "Filtering trajectories and drawing the map")
    w <- suppressWarnings(process_map_widget(lg, input$coverage / 100, input$units, roles = r$roles))
    step(1, "Done"); w
  })
  output$map <- processmapR::renderProcessMap(map())

  # key fields must exist even if their tab was never opened (e.g. load examples and go straight to Results)
  for (o in c("keys_pdx", "keys_pdrug", "keys_ppat", "keys_ddx", "keys_datc", "keys_dcause", "rules_ui")) outputOptions(output, o, suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
