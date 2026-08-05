#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
projection_path <- if (length(args) >= 1) args[[1]] else "public/projections.json"
stats_dir <- if (length(args) >= 2) args[[2]] else "../nflverse-projections/data/raw/player_stats"
output_path <- if (length(args) >= 3) args[[3]] else "public/player-history.json"

source("../nflverse-projections/R/fantasy_points.R")

history_seasons <- 2023:2025
latest_season <- max(history_seasons)
players <- jsonlite::fromJSON(projection_path, simplifyDataFrame = TRUE)
skill_player_ids <- players$id[players$position %in% skill_positions]

read_season <- function(season) {
  path <- file.path(stats_dir, paste0("player_stats_", season, ".parquet"))
  stats <- arrow::read_parquet(path) |>
    as.data.frame()
  stats[
    stats$season == season &
      stats$season_type == "REG" &
      stats$position %in% skill_positions &
      stats$player_id %in% skill_player_ids,
    ,
    drop = FALSE
  ]
}

weekly_stats <- bind_rows(lapply(history_seasons, read_season))
weekly_stats$scored_fantasy_points <- score_player_weeks(
  weekly_stats,
  standard_ppr_scoring()
)

sum_stat <- function(group, column) {
  if (!column %in% names(group)) return(0)
  round(sum(suppressWarnings(as.numeric(group[[column]])), na.rm = TRUE), 2)
}

collapse_value <- function(value) {
  values <- unique(as.character(value[!is.na(value) & nzchar(value)]))
  if (length(values)) paste(values, collapse = "/") else "—"
}

stat_values <- function(group) {
  list(
    completions = sum_stat(group, "completions"),
    attempts = sum_stat(group, "attempts"),
    passingYards = sum_stat(group, "passing_yards"),
    passingTds = sum_stat(group, "passing_tds"),
    interceptions = sum_stat(group, "passing_interceptions"),
    carries = sum_stat(group, "carries"),
    rushingYards = sum_stat(group, "rushing_yards"),
    rushingTds = sum_stat(group, "rushing_tds"),
    targets = sum_stat(group, "targets"),
    receptions = sum_stat(group, "receptions"),
    receivingYards = sum_stat(group, "receiving_yards"),
    receivingTds = sum_stat(group, "receiving_tds"),
    fantasyPoints = round(sum(group$scored_fantasy_points, na.rm = TRUE), 2)
  )
}

season_groups <- split(
  weekly_stats,
  paste(weekly_stats$player_id, weekly_stats$season, sep = "::"),
  drop = TRUE
)

season_rows <- lapply(season_groups, function(group) {
  games <- if ("game_id" %in% names(group)) {
    length(unique(group$game_id[!is.na(group$game_id) & nzchar(group$game_id)]))
  } else {
    length(unique(group$week[!is.na(group$week)]))
  }
  values <- stat_values(group)
  c(
    list(
      playerId = as.character(group$player_id[[1]]),
      season = as.integer(group$season[[1]]),
      team = collapse_value(group$team),
      games = games
    ),
    values,
    list(pointsPerGame = if (games > 0) round(values$fantasyPoints / games, 2) else 0)
  )
})

latest_stats <- weekly_stats[weekly_stats$season == latest_season, , drop = FALSE]
game_groups <- split(
  latest_stats,
  paste(latest_stats$player_id, latest_stats$week, sep = "::"),
  drop = TRUE
)

game_rows <- lapply(game_groups, function(group) {
  c(
    list(
      playerId = as.character(group$player_id[[1]]),
      week = as.integer(group$week[[1]]),
      team = collapse_value(group$team),
      opponent = collapse_value(group$opponent_team)
    ),
    stat_values(group)
  )
})

rows_for_player <- function(rows, player_id, sort_field, decreasing = FALSE) {
  matched <- rows[vapply(rows, function(row) identical(row$playerId, player_id), logical(1))]
  if (!length(matched)) return(list())
  order_values <- vapply(matched, function(row) as.numeric(row[[sort_field]]), numeric(1))
  unname(matched[order(order_values, decreasing = decreasing)])
}

history <- lapply(skill_player_ids, function(player_id) {
  seasons <- rows_for_player(season_rows, player_id, "season", decreasing = TRUE)
  games <- rows_for_player(game_rows, player_id, "week")
  seasons <- lapply(seasons, function(row) row[names(row) != "playerId"])
  games <- lapply(games, function(row) row[names(row) != "playerId"])
  list(seasons = seasons, gameLog = games)
})
names(history) <- skill_player_ids

output <- list(
  meta = list(
    seasons = history_seasons,
    gameLogSeason = latest_season,
    seasonType = "REG",
    scoring = "PPR"
  ),
  players = history
)

jsonlite::write_json(output, output_path, auto_unbox = TRUE, pretty = FALSE)
message("Wrote player history for ", length(history), " players to ", output_path)
