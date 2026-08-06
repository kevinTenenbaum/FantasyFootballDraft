#!/usr/bin/env Rscript

# Recommend the current pick by fixing each top candidate in turn and solving
# the remaining 12-team draft as a fractional linear program.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "draft_optimizer.R"))

args <- commandArgs(trailingOnly = TRUE)
value_prefixes <- c(
  "--season=", "--draft-slot=", "--current-round=", "--candidate-count=",
  "--minimum-availability=", "--bench-weights=", "--qb-bench-weights=", "--projections=",
  "--availability=", "--roster=", "--drafted=", "--output=",
  "--selection-plan=", "--role-plan="
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

integer_option <- function(prefix, default = NULL) {
  raw_value <- option_value(prefix, default)
  if (is.null(raw_value)) return(NULL)
  value <- suppressWarnings(as.integer(raw_value))
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

resolve_project_path <- function(path) {
  if (grepl("^/", path)) path else file.path(script_dir, path)
}

read_player_ids <- function(path, label) {
  if (is.null(path)) return(character())
  path <- resolve_project_path(path)
  if (!file.exists(path)) stop(label, " file does not exist: ", path, call. = FALSE)
  rows <- utils::read.csv(path, stringsAsFactors = FALSE)
  validate_draft_columns(rows, "player_id", label)
  unique(as.character(rows$player_id[nzchar(as.character(rows$player_id))]))
}

target_season <- integer_option("--season=", 2026L)
draft_slot <- integer_option("--draft-slot=", 1L)
candidate_count <- integer_option("--candidate-count=", 5L)
minimum_availability <- numeric_option("--minimum-availability=", 0.02)
bench_weights <- suppressWarnings(as.numeric(strsplit(
  option_value("--bench-weights=", "0.45,0.25,0.10"),
  ",",
  fixed = TRUE
)[[1L]]))
if (!length(bench_weights) || any(!is.finite(bench_weights))) {
  stop("--bench-weights must be a comma-separated numeric list.", call. = FALSE)
}
qb_bench_weights <- suppressWarnings(as.numeric(strsplit(
  option_value("--qb-bench-weights=", "0.20,0.05,0.01"),
  ",",
  fixed = TRUE
)[[1L]]))
if (!length(qb_bench_weights) || any(!is.finite(qb_bench_weights))) {
  stop("--qb-bench-weights must be a comma-separated numeric list.", call. = FALSE)
}

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
  file.path("data", "derived", paste0("draft_recommendations_", target_season, ".csv"))
))
selection_plan_path <- resolve_project_path(option_value(
  "--selection-plan=",
  file.path("data", "derived", paste0("fractional_selection_plan_", target_season, ".csv"))
))
role_plan_path <- resolve_project_path(option_value(
  "--role-plan=",
  file.path("data", "derived", paste0("fractional_role_plan_", target_season, ".csv"))
))

if (!file.exists(projection_path)) {
  stop("Projection file does not exist: ", projection_path, call. = FALSE)
}
if (!file.exists(availability_path)) {
  stop("Availability file does not exist: ", availability_path, call. = FALSE)
}
roster_player_ids <- read_player_ids(option_value("--roster=", NULL), "Roster")
drafted_player_ids <- read_player_ids(option_value("--drafted=", NULL), "Drafted-player")
current_round <- integer_option("--current-round=", length(roster_player_ids) + 1L)

projections <- utils::read.csv(projection_path, stringsAsFactors = FALSE)
availability <- utils::read.csv(availability_path, stringsAsFactors = FALSE)
if ("teams" %in% names(availability)) {
  availability_teams <- unique(availability$teams)
  availability_teams <- availability_teams[is.finite(availability_teams)]
  if (length(availability_teams) != 1L || availability_teams[[1L]] != 12L) {
    stop("Availability data must be built for exactly 12 teams.", call. = FALSE)
  }
}

candidate_data <- build_pick_candidate_sets(
  projections = projections,
  availability = availability,
  current_round = current_round,
  draft_slot = draft_slot,
  drafted_player_ids = drafted_player_ids,
  roster_player_ids = roster_player_ids,
  config = config
)
recommendation <- recommend_draft_pick(candidate_data)

for (path in c(output_path, selection_plan_path, role_plan_path)) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}
utils::write.csv(recommendation$rankings, output_path, row.names = FALSE, na = "")
utils::write.csv(
  recommendation$selection_plan,
  selection_plan_path,
  row.names = FALSE,
  na = ""
)
utils::write.csv(recommendation$role_plan, role_plan_path, row.names = FALSE, na = "")

best <- recommendation$rankings[recommendation$rankings$rank == 1L, ]
cat(
  "Recommended pick: ", best$player_name, " (", best$position, ", ",
  round(best$projected_fantasy_points, 2), " projected points)\n",
  sep = ""
)
cat(
  "Draft slot ", draft_slot, ", round ", current_round,
  ", overall pick ", candidate_data$picks[[1L]], ".\n",
  sep = ""
)
cat("Candidate rankings: ", normalizePath(output_path, mustWork = FALSE), "\n", sep = "")
cat(
  "Fractional selection plan: ",
  normalizePath(selection_plan_path, mustWork = FALSE), "\n",
  sep = ""
)
cat("Fractional role plan: ", normalizePath(role_plan_path, mustWork = FALSE), "\n", sep = "")
