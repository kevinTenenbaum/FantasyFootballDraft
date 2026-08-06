#!/usr/bin/env Rscript

# Compare current-pick candidates across shared sampled opponent boards. Future
# picks use a fast no-lookahead policy: solve the fractional plan on the board
# visible at that pick and take its largest current player share.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "draft_optimizer.R"))
source(file.path(script_dir, "R", "draft_simulator.R"))
source(file.path(script_dir, "R", "scenario_evaluator.R"))

args <- commandArgs(trailingOnly = TRUE)
value_prefixes <- c(
  "--season=", "--draft-slot=", "--current-round=", "--drafted-ids=",
  "--roster-ids=", "--screen-scenarios=", "--final-scenarios=",
  "--finalists=", "--seed=", "--cores=", "--viable-rb-points=", "--downside-weight=",
  "--projections=", "--availability=", "--rankings=", "--scenarios=",
  "--rosters="
)
recognized <- vapply(
  args,
  function(argument) any(startsWith(argument, value_prefixes)),
  logical(1)
)
if (any(!recognized)) {
  stop("Unknown argument(s): ", paste(args[!recognized], collapse = ", "), call. = FALSE)
}

option_value <- function(prefix, default = NULL) {
  values <- args[startsWith(args, prefix)]
  if (!length(values)) return(default)
  if (length(values) > 1L) stop(prefix, " can only be supplied once.", call. = FALSE)
  value <- sub(paste0("^", prefix), "", values[[1L]])
  if (!nzchar(value)) stop(prefix, " cannot be empty.", call. = FALSE)
  value
}
integer_option <- function(prefix, default) {
  value <- suppressWarnings(as.integer(option_value(prefix, default)))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop(prefix, " must be a positive integer.", call. = FALSE)
  }
  value
}
numeric_option <- function(prefix, default) {
  value <- suppressWarnings(as.numeric(option_value(prefix, default)))
  if (length(value) != 1L || !is.finite(value)) {
    stop(prefix, " must be numeric.", call. = FALSE)
  }
  value
}
id_option <- function(prefix) {
  value <- option_value(prefix, NULL)
  if (is.null(value)) return(character())
  unique(strsplit(value, ",", fixed = TRUE)[[1L]])
}
resolve_project_path <- function(path) {
  if (grepl("^/", path)) path else file.path(script_dir, path)
}

season <- integer_option("--season=", 2026L)
draft_slot <- integer_option("--draft-slot=", 2L)
current_round <- integer_option("--current-round=", 1L)
screen_scenarios <- integer_option("--screen-scenarios=", 20L)
final_scenarios <- integer_option("--final-scenarios=", 500L)
finalists <- integer_option("--finalists=", 5L)
seed <- integer_option("--seed=", 310001L)
cores <- integer_option("--cores=", 4L)
viable_rb_points <- numeric_option("--viable-rb-points=", 150)
downside_weight <- numeric_option("--downside-weight=", 0.20)
if (downside_weight < 0 || downside_weight > 1) {
  stop("--downside-weight must be between 0 and 1.", call. = FALSE)
}
drafted_player_ids <- id_option("--drafted-ids=")
roster_player_ids <- id_option("--roster-ids=")

projection_path <- resolve_project_path(option_value(
  "--projections=",
  file.path("data", "derived", paste0("simple_linear_projections_", season, ".csv"))
))
availability_path <- resolve_project_path(option_value(
  "--availability=",
  file.path("data", "derived", paste0("player_availability_", season, ".csv"))
))
rankings_path <- resolve_project_path(option_value(
  "--rankings=",
  file.path("data", "derived", paste0("draft_scenario_rankings_", season, ".csv"))
))
scenarios_path <- resolve_project_path(option_value(
  "--scenarios=",
  file.path("data", "derived", paste0("draft_scenario_results_", season, ".csv"))
))
rosters_path <- resolve_project_path(option_value(
  "--rosters=",
  file.path("data", "derived", paste0("draft_scenario_rosters_", season, ".csv"))
))

projections <- utils::read.csv(projection_path, stringsAsFactors = FALSE)
availability <- utils::read.csv(availability_path, stringsAsFactors = FALSE)
config <- default_draft_config()

progress <- function(stage, completed, total) {
  cat(stage, " scenarios: ", completed, "/", total, " candidate-runs\n", sep = "")
}
results <- evaluate_candidates_in_shared_scenarios(
  projections = projections,
  availability = availability,
  current_round = current_round,
  draft_slot = draft_slot,
  drafted_player_ids = drafted_player_ids,
  roster_player_ids = roster_player_ids,
  screen_scenarios = screen_scenarios,
  final_scenarios = final_scenarios,
  finalists = finalists,
  seed = seed,
  viable_rb_points = viable_rb_points,
  downside_weight = downside_weight,
  cores = cores,
  config = config,
  progress = progress
)

for (path in c(rankings_path, scenarios_path, rosters_path)) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}
utils::write.csv(results$rankings, rankings_path, row.names = FALSE, na = "")
utils::write.csv(results$scenarios, scenarios_path, row.names = FALSE, na = "")
utils::write.csv(results$rosters, rosters_path, row.names = FALSE, na = "")

display <- results$rankings[results$rankings$is_finalist, c(
  "rank", "player_name", "position", "scenario_count", "mean_roster_score",
  "p10_roster_score", "cvar10_roster_score", "risk_adjusted_score",
  "mean_rb1_rb2_points", "probability_two_viable_rbs"
)]
numeric_columns <- vapply(display, is.numeric, logical(1))
display[numeric_columns] <- lapply(display[numeric_columns], round, 3)
cat("\nFinalist rankings\n")
print(display, row.names = FALSE, right = FALSE)
cat("\nRankings: ", normalizePath(rankings_path, mustWork = FALSE), "\n", sep = "")
cat("Scenarios: ", normalizePath(scenarios_path, mustWork = FALSE), "\n", sep = "")
cat("Rosters: ", normalizePath(rosters_path, mustWork = FALSE), "\n", sep = "")
