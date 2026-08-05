#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}

source(file.path(script_dir, "R", "collect_nflverse_data.R"))

# Edit these defaults when starting a new projection season. Command-line
# arguments can still override the season window and collection tiers.
collection_defaults <- list(
  target_season = 2026L,
  history_start = 2016L,
  include_play_by_play = TRUE,
  include_participation = TRUE,
  include_expected_points = TRUE,
  include_injuries = TRUE,
  force = FALSE,
  fail_on_optional = FALSE
)

config <- default_collection_config(
  project_dir = script_dir,
  overrides = collection_defaults
)
config <- apply_command_line_args(config, commandArgs(trailingOnly = TRUE))

manifest <- collect_nflverse_data(config)
print_collection_summary(manifest, config)
