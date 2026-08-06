#!/usr/bin/env Rscript

# Compare the full candidate-fixing fractional policy with the faster
# largest-current-fraction rollout on identical sampled opponent boards.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "draft_optimizer.R"))
source(file.path(script_dir, "R", "draft_simulator.R"))
source(file.path(script_dir, "R", "scenario_evaluator.R"))
source(file.path(script_dir, "R", "policy_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
value_prefixes <- c(
  "--season=", "--draft-slot=", "--current-round=", "--candidate-id=",
  "--drafted-ids=", "--roster-ids=", "--simulations=", "--seed=",
  "--cores=", "--viable-rb-points=", "--projections=", "--availability=",
  "--summary=", "--scenarios=", "--rosters=", "--paired=", "--agreement="
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
candidate_player_id <- option_value("--candidate-id=", "00-0039075")
drafted_player_ids <- id_option("--drafted-ids=")
roster_player_ids <- id_option("--roster-ids=")
simulations <- integer_option("--simulations=", 50L)
seed <- integer_option("--seed=", 410001L)
cores <- integer_option("--cores=", 4L)
viable_rb_points <- numeric_option("--viable-rb-points=", 150)

projection_path <- resolve_project_path(option_value(
  "--projections=",
  file.path("data", "derived", paste0("simple_linear_projections_", season, ".csv"))
))
availability_path <- resolve_project_path(option_value(
  "--availability=",
  file.path("data", "derived", paste0("player_availability_", season, ".csv"))
))
paths <- list(
  summary = resolve_project_path(option_value(
    "--summary=", file.path("data", "derived", paste0("draft_policy_summary_", season, ".csv"))
  )),
  scenarios = resolve_project_path(option_value(
    "--scenarios=", file.path("data", "derived", paste0("draft_policy_scenarios_", season, ".csv"))
  )),
  rosters = resolve_project_path(option_value(
    "--rosters=", file.path("data", "derived", paste0("draft_policy_rosters_", season, ".csv"))
  )),
  paired = resolve_project_path(option_value(
    "--paired=", file.path("data", "derived", paste0("draft_policy_paired_scores_", season, ".csv"))
  )),
  agreement = resolve_project_path(option_value(
    "--agreement=", file.path("data", "derived", paste0("draft_policy_pick_agreement_", season, ".csv"))
  ))
)

projections <- utils::read.csv(projection_path, stringsAsFactors = FALSE)
availability <- utils::read.csv(availability_path, stringsAsFactors = FALSE)
progress <- function(completed, total) {
  cat("policy comparison: ", completed, "/", total, " rollouts\n", sep = "")
}
results <- run_paired_policy_comparison(
  projections = projections,
  availability = availability,
  candidate_player_id = candidate_player_id,
  current_round = current_round,
  draft_slot = draft_slot,
  drafted_player_ids = drafted_player_ids,
  roster_player_ids = roster_player_ids,
  simulations = simulations,
  seed = seed,
  viable_rb_points = viable_rb_points,
  cores = cores,
  config = default_draft_config(),
  progress = progress
)
for (path in paths) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(results$summary, paths$summary, row.names = FALSE, na = "")
utils::write.csv(results$scenarios, paths$scenarios, row.names = FALSE, na = "")
utils::write.csv(results$rosters, paths$rosters, row.names = FALSE, na = "")
utils::write.csv(results$paired_scores, paths$paired, row.names = FALSE, na = "")
utils::write.csv(
  results$pick_agreement_by_round,
  paths$agreement,
  row.names = FALSE,
  na = ""
)

display <- results$summary
numeric_columns <- vapply(display, is.numeric, logical(1))
display[numeric_columns] <- lapply(display[numeric_columns], round, 3)
cat("\nPolicy summary\n")
print(display, row.names = FALSE, right = FALSE)
cat("\nOverall pick agreement: ", round(results$pick_agreement, 3), "\n", sep = "")
cat(
  "Mean paired score advantage for candidate-fixing: ",
  round(mean(results$paired_scores$score_difference), 3), "\n",
  sep = ""
)
cat("Summary: ", normalizePath(paths$summary, mustWork = FALSE), "\n", sep = "")
cat("Rosters: ", normalizePath(paths$rosters, mustWork = FALSE), "\n", sep = "")
