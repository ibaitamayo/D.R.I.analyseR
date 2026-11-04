# Step-by-step guide (game-style coach marks) and "ghost" example tables behind upload zones.
# No extra packages: plain CSS + JavaScript.
#
# - A guided tour starts the first time each tab is opened (remembered in the browser);
#   the "Guide" button in the navbar replays the tour of the current tab.
# - Contextual hints appear after actions (e.g. "table loaded: now check the key fields").
# - Each upload zone shows, faded in the background, what the expected table looks like.

guide_steps <- list(
  "Start here" = list(
    list(title = "Welcome! 👋", text = "This app finds, for each patient, the first date of each disease you define, from diagnosis codes and drug dispensations. I'll guide you step by step."),
    list(target = "#four_tables", title = "Your tables",
         text = "You will need up to five tables: three with patient data and two with disease definitions. Click each tab here to see which columns they need and in what format."),
    list(target = "#steps_card", title = "The route", text = "These are the steps. Each tab of the app is one step, and I'll be there in each of them."),
    list(target = ".navbar .nav-link[data-guide='1. Patient data']", title = "Next stop: tab 1",
         text = "When your tables are ready (or if you just want to practise with example data), go to tab 1.")),
  "1. Patient data" = list(
    list(target = "#example_box", title = "Practice mode", text = "No data yet? Load example tables for all five slots and try everything safely. Choose the simple demo, or the DRID definitions from the literature (Quan 2008, Ball 2017, INE, EUDA)."),
    list(target = "#zone_pdx", title = "Slot 1 · Patient – diagnosis",
         text = "Drop your patient – diagnosis table here. The faded table behind shows what it should look like: one row per diagnosis, with patient, code, code type and date."),
    list(target = "#card_pdx .accordion", title = "Need the details?", text = "Open this to see every column, its format and accepted values."),
    list(target = "#opts_pdx", title = "Reading options",
         text = "Only for csv/txt: leave 'auto' unless all columns appear merged into one. If accents look broken, switch to Latin-1."),
    list(target = "#zone_pdrug", title = "Slot 2 · Patient – drug (optional)",
         text = "Only needed if some disease is defined by drugs: one row per dispensation, with patient, ATC code and date."),
    list(target = "#drop_proc_wrap", title = "Keep this ticked ✔", text = "It removes procedure codes stored among diagnoses, so they are not mistaken for diseases.")),
  "2. Definitions" = list(
    list(target = "#template", title = "Templates", text = "Download ready-to-fill Excel templates for both definition tables, with instructions."),
    list(target = "#zone_ddx", title = "Slot 3 · Disease – diagnosis code",
         text = "One row per code in each disease definition: disease name, code type and code. No dot = whole category; with a dot = that level and below."),
    list(target = "#zone_datc", title = "Slot 4 · Disease – ATC code",
         text = "One row per ATC code in each drug-based definition. Use exactly the same disease name as in slot 3 if a disease uses both."),
    list(target = "#rules_card", title = "Combine diagnoses and drugs",
         text = "For diseases present in both slots, choose here whether diagnosis OR drug is enough, or both are required. Careful with treatment events: a disease can be defined as diagnosed OR treated, but a treatment is only a treatment if the drug was dispensed \u2014 never define it with a diagnosis.")),
  "3. Results" = list(
    list(target = "#run", title = "Run the search", text = "When the slots are filled and checked, press here. A progress bar will tell you how long is left."),
    list(target = "#card_summary", title = "Persons per disease", text = "Number of persons found for each disease, and how many diagnosis rows were used or dropped."),
    list(target = "#download", title = "Download the results", text = "Download the full table: one row per patient, one column per disease with its first date.")),
  "4. Process map" = list(
    list(target = "#ev_map", title = "Choose events", text = "Pick the diseases to include in the map."),
    list(target = "#coverage", title = "Most frequent paths", text = "Keep the trajectories followed by this share of persons. Lower values give simpler maps."),
    list(target = "#make_map", title = "Draw the map", text = "This can take a while with many persons: a clock and a time estimate will keep you informed.")),
  "5. Outcomes" = list(
    list(target = "#oc_index", title = "Pick your starting point",
         text = "The index event is time zero: everyone who reached it enters the denominator, from the date they reached it."),
    list(target = "#oc_outcome", title = "And what you are counting",
         text = "The outcome event is what you are following: treatment, death, a vaccination\u2026"),
    list(target = "#oc_exit", title = "Who leaves the cohort",
         text = "Optional. If you pick an exit event (for example death or deregistration), those people stop counting in the denominator from that date on."),
    list(target = "#oc_aviso", title = "Check the follow-up",
         text = "This line tells you how many people have the index event and until when follow-up runs."),
    list(title = "Three views",
         text = "Cascade: cumulative people and proportion reaching the outcome, by year. Events by year: a calendar table of each event. Rates: events per person-years at risk, with confidence intervals. All three can be downloaded as a table.")),
  "6. Time to event" = list(
    list(target = "#cr_index", title = "The clock starts here",
         text = "Everyone who reached the index event enters, and their follow-up starts on that date."),
    list(target = "#cr_outcome", title = "What you are counting",
         text = "The outcome event. Follow-up ends when it happens."),
    list(target = "#cr_competing", title = "What competes with it",
         text = "An event that prevents the outcome from ever happening \u2014 typically death from another cause. If you leave it empty the curve is 1 - Kaplan-Meier, which overestimates the risk when such events exist."),
    list(target = "#cr_censor", title = "Who stops being observed",
         text = "Optional: leaving the system, moving away. Those people are censored, not counted as having the outcome."),
    list(target = "#cr_group", title = "The intervention",
         text = "Optional: splits people into those who had this event before follow-up starts and those who did not. Every table and figure is then named after your choice."),
    list(target = "#cr_landmark", title = "Landmark",
         text = "Starting follow-up some months after time zero, and classifying the intervention at that moment, removes the immortal time of comparing treated and untreated from day zero. Whoever had an event before the landmark does not enter.")),
  "Laboratory" = list(
    list(title = "Coming soon 🔧", text = "Diseases defined from laboratory results will be added here."))
)

# Contextual hints sent by the server after user actions
guide_hints <- list(
  pdx = list(target = "#keys_pdx", title = "Table loaded! ✅",
             text = "Now check that each key field points to the right column. The preview and the checks below show how the app reads your table; orange means something needs a look."),
  pdrug = list(target = "#keys_pdrug", title = "Dispensations loaded! ✅",
               text = "Check the key fields: patient ID, ATC code and dispensation date."),
  ddx = list(target = "#keys_ddx", title = "Definitions loaded! ✅",
             text = "Tell the app which column is the disease, the code type and the code."),
  datc = list(target = "#keys_datc", title = "Drug definitions loaded! ✅",
              text = "Map disease and ATC code; the minimum dispensations column is optional."),
  example = list(target = ".navbar .nav-link[data-guide='2. Definitions']", title = "Example data ready",
                 text = "All five slots are filled with example tables. Look at the key fields here, then continue to tab 2."),
  results = list(target = "#card_summary", title = "Results ready",
                 text = "Persons found per disease. You can download the table, or go to tab 4 to draw the process map."))

guide_head <- function() tags$head(
  tags$style(HTML("
    .ghost-zone { position: relative; border: 2px dashed #b8c4cf; border-radius: 10px; padding: 132px 12px 4px;
                  overflow: hidden; background: #fbfcfd; }
    .ghost-zone .ghost { position: absolute; top: 26px; left: 10px; right: 10px; height: 100px; opacity: .45;
                         pointer-events: none; filter: grayscale(30%); overflow: hidden; }
    .ghost-zone .ghost table { background: #fff; }
    .ghost-zone .ghost-label { position: absolute; top: 5px; left: 12px; font-size: .75rem; color: #7b8a97;
                               font-style: italic; pointer-events: none; }
    .ghost-zone .form-group { position: relative; z-index: 2; background: rgba(255,255,255,.85); border-radius: 6px; padding: 6px; }
    .ghost-zone.loaded { border-style: solid; border-color: #18bc9c; padding-top: 12px; }
    .ghost-zone.loaded .ghost, .ghost-zone.loaded .ghost-label { display: none; }
    #mf-spot { position: fixed; z-index: 2000; border-radius: 10px; pointer-events: none;
               box-shadow: 0 0 0 9999px rgba(15, 30, 45, .55); transition: all .35s ease; }
    #mf-spot::after { content: ''; position: absolute; inset: -6px; border: 3px solid #f39c12; border-radius: 12px;
                      animation: mfpulse 1.4s ease-in-out infinite; }
    @keyframes mfpulse { 0%,100% { opacity: 1; transform: scale(1); } 50% { opacity: .35; transform: scale(1.03); } }
    #mf-bubble { position: fixed; z-index: 2001; width: 340px; background: #fff; border-radius: 14px; padding: 14px 16px 12px;
                 box-shadow: 0 10px 30px rgba(0,0,0,.35); border: 3px solid #f39c12; animation: mfpop .35s cubic-bezier(.2,1.6,.4,1); }
    @keyframes mfpop { from { transform: scale(.6); opacity: 0; } to { transform: scale(1); opacity: 1; } }
    #mf-bubble h5 { margin: 0 0 6px; font-weight: 700; color: #2c3e50; }
    #mf-bubble p { margin: 0 0 10px; font-size: .92rem; }
    #mf-bubble .mf-foot { display: flex; align-items: center; justify-content: space-between; gap: 6px; }
    #mf-bubble .mf-dots span { display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #d5dbe0; margin-right: 3px; }
    #mf-bubble .mf-dots span.on { background: #f39c12; }
    #mf-bubble .mf-skip { font-size: .78rem; color: #7b8a97; cursor: pointer; text-decoration: underline; }
    /* help turned off from the Welcome tab: no standing explanations, no ghost tables, no bubbles */
    body.mf-nohelp .mf-help { display: none !important; }
    body.mf-nohelp .ghost-zone { border-style: solid; padding-top: 12px; }

  ")),
  tags$script(HTML(paste0("var MF_STEPS = ", jsonlite::toJSON(guide_steps, auto_unbox = TRUE), ";"))),
  tags$script(HTML("
  (function() {
    var tour = null, seen = {}, MF_HELP = true;
    // remembers finished guides; falls back to memory if the browser blocks localStorage
    function done(k) { if (seen[k]) return true; try { return !!localStorage.getItem(k); } catch (e) { return false; } }
    function mark(k) { if (!k) return; seen[k] = true; try { localStorage.setItem(k, '1'); } catch (e) {} }
    function removeAll() { ['mf-spot', 'mf-bubble'].forEach(function(id) { var e = document.getElementById(id); if (e) e.remove(); }); }
    function visible(el) { return el && el.offsetParent !== null && el.getBoundingClientRect().width > 0; }
    // Resolves a step target: a navbar tab hidden in a collapsed menu falls back to the menu button
    function targetOf(s) {
      if (!s.target) return null;
      var el = document.querySelector(s.target);
      if (visible(el)) return el;
      if (s.target.indexOf('.navbar') === 0) { var t = document.querySelector('.navbar-toggler, .navbar-toggle'); if (visible(t)) return t; }
      return null;
    }

    // Positions spot and bubble over the current target without re-creating them
    function place() {
      if (!tour) return;
      var s = tour.steps[tour.i], sp = document.getElementById('mf-spot'), b = document.getElementById('mf-bubble');
      if (!sp || !b) return;
      var vw = window.innerWidth, vh = window.innerHeight, bw = b.offsetWidth, bh = b.offsetHeight, g = 14, pad = 6;
      if (!s.target) {
        sp.style.cssText = 'left:50%;top:50%;width:0;height:0';
        b.style.left = Math.max(10, (vw - bw) / 2) + 'px'; b.style.top = Math.max(10, vh * 0.3) + 'px'; return;
      }
      var el = targetOf(s);
      if (!el) { sp.style.cssText = 'left:50%;top:50%;width:0;height:0';
                 b.style.left = Math.max(10, (vw - bw) / 2) + 'px'; b.style.top = Math.max(10, vh * 0.3) + 'px'; return; }
      var r = el.getBoundingClientRect();
      sp.style.left = (r.left - pad) + 'px'; sp.style.top = (r.top - pad) + 'px';
      sp.style.width = (r.width + 2 * pad) + 'px'; sp.style.height = (r.height + 2 * pad) + 'px';
      var cx = function(x) { return Math.min(Math.max(10, x), vw - bw - 10); };
      var cy = function(y) { return Math.min(Math.max(10, y), vh - bh - 10); };
      // below, above, right, left of the target; otherwise bottom-right corner
      var pos = [
        { ok: r.bottom + g + bh < vh - 10, x: cx(r.left), y: r.bottom + g },
        { ok: r.top - g - bh > 10,         x: cx(r.left), y: r.top - g - bh },
        { ok: r.right + g + bw < vw - 10,  x: r.right + g, y: cy(r.top) },
        { ok: r.left - g - bw > 10,        x: r.left - g - bw, y: cy(r.top) }
      ].filter(function(p) { return p.ok; })[0] || { x: vw - bw - 10, y: vh - bh - 10 };
      b.style.left = pos.x + 'px'; b.style.top = pos.y + 'px';
    }

    // Builds the bubble for the current step (once per step)
    function show() {
      removeAll();
      if (!tour) return;
      // tour steps whose target is not on screen are skipped; a single hint is shown centred instead
      while (!tour.hint && tour.i < tour.steps.length && tour.steps[tour.i].target && !targetOf(tour.steps[tour.i])) tour.i++;
      if (tour.i >= tour.steps.length) { finish(); return; }
      var s = tour.steps[tour.i], n = tour.steps.length;
      var dots = ''; for (var k = 0; k < n; k++) dots += '<span class=\"' + (k === tour.i ? 'on' : '') + '\"></span>';
      var sp = document.createElement('div'); sp.id = 'mf-spot'; document.body.appendChild(sp);
      var b = document.createElement('div'); b.id = 'mf-bubble';
      b.innerHTML = '<h5>' + s.title + '</h5><p>' + s.text + '</p><div class=\"mf-foot\"><div class=\"mf-dots\">' + dots + '</div><div>' +
        (n > 1 ? '<span class=\"mf-skip\">Skip guide</span> ' : '') +
        (tour.i > 0 ? '<button class=\"btn btn-sm btn-light\" data-mf=\"back\">◀</button> ' : '') +
        '<button class=\"btn btn-sm btn-warning\" data-mf=\"next\">' + (tour.i === n - 1 ? 'Got it' : 'Next ▶') + '</button></div></div>';
      document.body.appendChild(b);
      b.querySelector('[data-mf=next]').onclick = function() { tour.i++; tour.i >= n ? finish() : show(); };
      var bk = b.querySelector('[data-mf=back]'); if (bk) bk.onclick = function() { tour.i--; show(); };
      var sk = b.querySelector('.mf-skip'); if (sk) sk.onclick = finish;
      var tg = targetOf(s); if (tg) tg.scrollIntoView({ block: 'nearest' });
      place();
    }
    function finish() { if (tour) mark(tour.key); tour = null; removeAll(); }
    function start(steps, key, hint) { if (!steps || !steps.length || tour) return; tour = { steps: steps, i: 0, key: key, hint: !!hint }; show(); }
    function activeTab() { var a = document.querySelector('.navbar .nav-link.active'); return a ? a.getAttribute('data-guide') : null; }
    function autoTour() {
      if (!MF_HELP) return;
      var t = activeTab(), k = 'mf_tour_' + t;
      if (t && MF_STEPS[t] && !done(k)) setTimeout(function() { start(MF_STEPS[t], k); }, 500);
    }
    window.mfStartTour = function() { if (!MF_HELP) return; if (tour) finish(); var t = activeTab(); if (t) start(MF_STEPS[t], null); };
    window.mfResetGuide = function() {
      seen = {}; Object.keys(MF_STEPS).forEach(function(t) { try { localStorage.removeItem('mf_tour_' + t); } catch (e) {} });
      try { Object.keys(localStorage).forEach(function(k) { if (k.indexOf('mf_hint_') === 0) localStorage.removeItem(k); }); } catch (e) {}
      window.mfStartTour();
    };
    var raf = null;
    function reposition() { if (tour && !raf) raf = requestAnimationFrame(function() { raf = null; place(); }); }
    $(document).on('shiny:connected', function() {
      document.querySelectorAll('.navbar .nav-link').forEach(function(a) { a.setAttribute('data-guide', a.textContent.trim()); });
      autoTour();
    });
    $(document).on('shown.bs.tab', '.navbar .nav-link', function() { if (tour) { tour = null; removeAll(); } autoTour(); });
    window.addEventListener('resize', reposition);
    window.addEventListener('scroll', reposition, true);
    $(document).on('keydown', function(e) { if (tour && e.key === 'Escape') finish(); });
    Shiny.addCustomMessageHandler('mf_help', function(on) {
      MF_HELP = !!on;
      document.querySelectorAll('.navbar .nav-link').forEach(function(a) {
        if (!a.getAttribute('data-guide')) a.setAttribute('data-guide', a.textContent.trim()); });
      var st = null;
      document.querySelectorAll('.navbar .nav-link').forEach(function(a) {
        if (a.textContent.trim() === 'Start here') st = a; });
      if (st && st.parentElement) st.parentElement.classList.add('mf-help');
      document.body.classList.toggle('mf-nohelp', !MF_HELP);
      if (!MF_HELP && tour) finish();
    });
    Shiny.addCustomMessageHandler('mf_loaded', function(id) { var z = document.getElementById('zone_' + id); if (z) z.classList.add('loaded'); });
    Shiny.addCustomMessageHandler('mf_hint', function(h) {
      var k = 'mf_hint_' + h.key;
      if (!MF_HELP || done(k) || tour) return;
      mark(k);   // shown once, even if closed without pressing any button
      setTimeout(function() { start([h], k, true); }, 600);
    });
  })();
  ")))

# Upload zone with the expected table faded in the background
ghost_zone <- function(id, ...) div(id = paste0("zone_", id), class = "ghost-zone",
  div(class = "ghost-label mf-help", "Your table should look like this (column names are free):"),
  div(class = "ghost mf-help", html_table(table_specs[[id]]$example)),
  ...)

send_hint <- function(session, key) {
  h <- guide_hints[[key]]
  session$sendCustomMessage("mf_hint", c(h, key = key))
}
