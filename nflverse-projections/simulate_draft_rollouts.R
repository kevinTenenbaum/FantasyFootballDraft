#!/usr/bin/env Rscript

# Experimental greedy rollouts: sample one concrete opponent board, reveal the
# survivors at each of our picks, select one real player, and re-optimize.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "draft_optimizer.R"))
source(file.path(script_dir, "R", "draft_simulator.R"))

args <- commandArgs(trailingOnly = TRUE)
value_prefixes <- c(
  "--season=", "--draft-slot=", "--simulations=", "--seed=",
  "--candidate-count=", "--minimum-availability=", "--bench-weights=",
  "--qb-bench-weights=", "--projections=", "--availability=", "--output=",
  "--summary="
)
recognized <- vapply(
  args,
  function(argument) any(startsWith(argument, value_prefixes)),
  logical(1)
)
if (any(!recognized)) {
  stop(
    "Unknown argument(s): ", paste(args[!recognized], collapse = ", "),
    call. = FALSE
  )
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

numeric_vector_option <- function(prefix, default) {
  values <- suppressWarnings(as.numeric(strsplit(
    option_value(prefix, default),
    ",",
    fixed = TRUE
  )[[1L]]))
  if (!length(values) || any(!is.finite(values))) {
    stop(prefix, " must be a comma-separated numeric list.", call. = FALSE)
  }
  values
}

resolve_project_path <- function(path) {
  if (grepl("^/", path)) path else file.path(script_dir, path)
}

target_season <- integer_option("--season=", 2026L)
draft_slot <- integer_option("--draft-slot=", 2L)
simulation_count <- integer_option("--simulations=", 3L)
initial_seed <- integer_option("--seed=", 202602L)
candidate_count <- integer_option("--candidate-count=", 5L)
minimum_availability <- suppressWarnings(as.numeric(option_value(
  "--minimum-availability=",
  0.02
)))
if (
  length(minimum_availability) != 1L ||
    !is.finite(minimum_availability)
) {
  stop("--minimum-availability must be numeric.", call. = FALSE)
}
bench_weights <- numeric_vector_option(
  "--bench-weights=",
  "0.45,0.25,0.10"
)
qb_bench_weights <- numeric_vector_option(
  "--qb-bench-weights=",
  "0.20,0.05,0.01"
)

config <- default_draft_config(
  team_count = 12L,
  offensive_rounds = 14L,
  candidate_count = candidate_count,
  minimum_future_availability = minimum_availability,
  bench_weights = bench_weights,
  qb_bench_weights = qb_bench_weights
)
if (draft_slot > config$team_count) {
  stop("--draft-slot must be between 1 and 12.", call. = FALSE)
}

projection_path <- resolve_project_path(option_value(
  "--projections=",
  file.path("data", "derived", paste0("simple_linear_projections_", target_season, ".csv"))
))
availability_path <- resolve_project_path(option_value(
  "--availability=",
  file.path("data", "derived", paste0("player_availability_", target_season, ".csv"))
))
output_path <- resolve_project_path(option_value(
  "--output=",
  file.path("data", "derived", paste0("greedy_draft_rollouts_", target_season, ".csv"))
))
summary_path <- resolve_project_path(option_value(
  "--summary=",
  file.path("data", "derived", paste0("greedy_draft_rollout_summary_", target_season, ".csv"))
))

if (!file.exists(projection_path)) {
  stop("Projection file does not exist: ", projection_path, call. = FALSE)
}
if (!file.exists(availability_path)) {
  stop("Availability file does not exist: ", availability_path, call. = FALSE)
}
projections <- utils::read.csv(projection_path, stringsAsFactors = FALSE)
availability <- utils::read.csv(availability_path, stringsAsFactors = FALSE)
if ("teams" %in% names(availability)) {
  availability_teams <- unique(availability$teams)
  availability_teams <- availability_teams[is.finite(availability_teams)]
  if (length(availability_teams) != 1L || availability_teams[[1L]] != 12L) {
    stop("Availability data must be built for exactly 12 teams.", call. = FALSE)
  }
}

results <- run_greedy_draft_rollouts(
  projections = projections,
  availability = availability,
  draft_slot = draft_slot,
  simulations = simulation_count,
  seed = initial_seed,
  config = config
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(results$picks, output_path, row.names = FALSE, na = "")
utils::write.csv(results$summary, summary_path, row.names = FALSE, na = "")

for (simulation in seq_len(simulation_count)) {
  rows <- results$picks[results$picks$simulation == simulation, ]
  display <- rows[c(
    "round", "overall_pick", "player_name", "position", "public_adp",
    "projected_fantasy_points"
  )]
  display$public_adp <- ifelse(
    is.na(display$public_adp),
    "unmatched",
    format(round(as.numeric(display$public_adp), 1), nsmall = 1, trim = TRUE)
  )
  cat(
    "\nSimulation ", simulation, " (seed ", rows$seed[[1L]], ")\n",
    sep = ""
  )
  print(display, row.names = FALSE, right = FALSE)
}
cat("\nSimulation summaries\n")
print(results$summary, row.names = FALSE, right = FALSE)
cat("\nPicks: ", normalizePath(output_path, mustWork = FALSE), "\n", sep = "")
cat("Summary: ", normalizePath(summary_path, mustWork = FALSE), "\n", sep = "")
