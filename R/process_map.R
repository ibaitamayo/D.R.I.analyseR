# Converts results (id, event, date) into an event log and a process map:
# nodes = persons going through each event; edges = median time between consecutive
# events and number of persons, restricted to the most frequent trajectories.

# Colour of the border by role: the fill keeps encoding how many people go through each node,
# so nothing is traded away. Role is what the node IS; frequency is how many reach it.
ROLE_COLOURS <- c(birth = "#4F4F4F", cohort = "#7B5EA7", disease = "#1F6FB4",
                  treatment = "#2E8B57", comorbidity = "#B8860B", marker = "#5A7D7C", mortality = "#8B1A1A")

build_eventlog <- function(long, events = NULL, roles = NULL) {
  d <- as.data.table(long)
  if (!is.null(events)) d <- d[event %in% events]
  d <- d[, .(id = as.character(id), event, date = as.POSIXct(date), resource = "BARDENA", lifecycle = "complete")]
  # Death is absorbing. Ties within a date are broken alphabetically, so without this a
  # diagnosis coded on the day of death would be drawn after the death.
  if (!is.null(roles)) d <- order_terminal_last(d, roles) else setorder(d, id, date, event)
  d[, instance := as.character(.I)]
  # eventlog built directly: converting from activitylog drops the POSIXct type in bupaR 1.0.x
  bupaR::eventlog(as.data.frame(d), case_id = "id", activity_id = "event",
                  activity_instance_id = "instance", lifecycle_id = "lifecycle",
                  timestamp = "date", resource_id = "resource")
}

process_map_widget <- function(log, coverage = 0.8, units = "months", roles = NULL) {
  if (coverage < 1) log <- edeaR::filter_trace_frequency(log, percentage = coverage)
  g <- processmapR::process_map(log,
                           type_nodes = processmapR::frequency("absolute_case"),
                           type_edges = processmapR::performance(median, units),
                           sec_edges  = processmapR::frequency("absolute_case"),
                           rankdir = "TB", render = FALSE)
  if (!is.null(roles) && !is.null(g$nodes_df) && all(c("tooltip", "color") %in% names(g$nodes_df))) {
    r <- as.data.table(roles)
    name <- sub("\n.*$", "", g$nodes_df$tooltip)          # tooltip is "Event\nN"
    col <- unname(ROLE_COLOURS[r$role[match(name, r$event)]])
    g$nodes_df$color <- fifelse(is.na(col), g$nodes_df$color, col)
    g$nodes_df$penwidth <- fifelse(is.na(col), g$nodes_df$penwidth, 3)
  }
  g |>
    # grViz widget: what processmapR::renderProcessMap expects (via DiagrammeR::renderGrViz)
    DiagrammeR::render_graph()
}
