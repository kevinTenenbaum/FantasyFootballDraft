#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
project_dir <- dirname(test_dir)
source(file.path(project_dir, "R", "collect_nflverse_data.R"))

legacy_depth <- data.frame(
  season = 2024L, club_code = "BUF", week = 1L, depth_team = "2",
  gsis_id = "00-0000001", position = "RB", depth_position = "RB",
  full_name = "Legacy Player", formation = "Offense"
)
modern_depth <- data.frame(
  dt = "2025-08-01T12:00:00Z", team = "BUF", player_name = "Modern Player",
  gsis_id = "00-0000002", pos_abb = "RB", pos_rank = 1L, pos_grp = "3WR 1TE"
)
legacy_normalized <- normalize_depth_charts(legacy_depth, 2024L)
modern_normalized <- normalize_depth_charts(modern_depth, 2025L)
stopifnot(identical(names(legacy_normalized), canonical_depth_chart_columns()))
stopifnot(identical(names(modern_normalized), canonical_depth_chart_columns()))
stopifnot(legacy_normalized$depth_rank == 2L)
stopifnot(legacy_normalized$source == "gsis_weekly")
stopifnot(modern_normalized$season == 2025L)
stopifnot(modern_normalized$depth_rank == 1L)
stopifnot(modern_normalized$source == "espn_snapshot")

fake_data <- list(
  player_stats = data.frame(
    player_id = "00-0000001", position = "QB", season = 2024L, week = 1L,
    season_type = "REG", team = "BUF", attempts = 30L, carries = 4L,
    targets = 0L, receptions = 0L
  ),
  team_stats = data.frame(
    team = "BUF", season = 2024L, week = 1L, season_type = "REG",
    attempts = 30L, carries = 25L
  ),
  rosters = data.frame(season = 2024L, team = "BUF", position = "QB"),
  weekly_rosters = data.frame(
    season = 2024L, week = 1L, team = "BUF", position = "QB"
  ),
  depth_charts = modern_normalized,
  snap_counts = data.frame(
    season = 2024L, week = 1L, team = "BUF", position = "QB", offense_snaps = 60L
  ),
  players = data.frame(
    gsis_id = "00-0000001", display_name = "Test Player", position = "QB",
    birth_date = as.Date("1995-01-01")
  ),
  player_ids = data.frame(
    gsis_id = "00-0000001", name = "Test Player", position = "QB"
  ),
  schedules = data.frame(
    season = 2024L, week = 1L, home_team = "BUF", away_team = "MIA"
  ),
  draft_picks = data.frame(
    season = 2024L, round = 1L, pick = 1L, team = "BUF", position = "QB"
  ),
  combine = data.frame(season = 2024L, player_name = "Test Player", pos = "QB")
)

loaders <- lapply(fake_data, function(data) {
  force(data)
  function(seasons) data
})

temporary_project <- tempfile("nflverse-collector-test-")
dir.create(temporary_project)
config <- default_collection_config(
  project_dir = temporary_project,
  overrides = list(
    target_season = 2025L,
    history_start = 2024L,
    include_play_by_play = FALSE,
    include_participation = FALSE,
    include_expected_points = FALSE,
    include_injuries = FALSE
  )
)

manifest <- collect_nflverse_data(config, loaders = loaders)
stopifnot(nrow(manifest) == 14L)
stopifnot(all(manifest$status == "ok"))
stopifnot(file.exists(file.path(config$manifest_dir, "latest.csv")))
stopifnot(all(file.exists(manifest$path)))

cached <- collect_nflverse_data(config, loaders = loaders)
stopifnot(all(cached$status == "cached"))

cat("Offline collector smoke test passed.\n")
