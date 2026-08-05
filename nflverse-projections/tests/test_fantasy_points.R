#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "fantasy_points.R"))

base_row <- function(player_id, name, position, week, team = "BUF") {
  data.frame(
    player_id = player_id,
    player_display_name = name,
    position = position,
    season = 2024L,
    week = week,
    game_id = paste0("2024_", sprintf("%02d", week), "_TEST"),
    season_type = "REG",
    team = team,
    passing_yards = 0, passing_tds = 0, passing_interceptions = 0,
    passing_2pt_conversions = 0, rushing_yards = 0, rushing_tds = 0,
    rushing_2pt_conversions = 0, receptions = 0, receiving_yards = 0,
    receiving_tds = 0, receiving_2pt_conversions = 0,
    fumbles_lost_total = 0, special_teams_tds = 0,
    stringsAsFactors = FALSE
  )
}

qb1 <- base_row("qb1", "Example QB", "QB", 1L)
qb1$passing_yards <- 250
qb1$passing_tds <- 2
qb1$passing_interceptions <- 1
qb1$rushing_yards <- 20

qb2 <- base_row("qb1", "Example QB", "QB", 2L)
qb2$passing_yards <- 300
qb2$passing_tds <- 3
qb2$rushing_tds <- 1
qb2$fumbles_lost_total <- 1

rb <- base_row("rb1", "Example RB", "RB", 1L)
rb$rushing_yards <- 100
rb$rushing_tds <- 1
rb$receptions <- 5
rb$receiving_yards <- 40

kicker <- base_row("k1", "Ignored Kicker", "K", 1L)
defense <- base_row("dst1", "Ignored Defense", "DST", 1L)
postseason <- base_row("rb1", "Example RB", "RB", 19L)
postseason$season_type <- "POST"
postseason$rushing_yards <- 500

stats <- rbind(qb1, qb2, rb, kicker, defense, postseason)
scored <- calculate_season_fantasy_points(stats, 2024L)

stopifnot(setequal(scored$player_id, c("qb1", "rb1")))
stopifnot(scored$games[scored$player_id == "qb1"] == 2L)
stopifnot(all(scored$season == 2024L))
stopifnot(scored$fantasy_points[scored$player_id == "qb1"] == 46)
stopifnot(scored$fantasy_points[scored$player_id == "rb1"] == 25)
stopifnot(scored$position_rank[scored$player_id == "qb1"] == 1)
stopifnot(scored$position_rank[scored$player_id == "rb1"] == 1)

half_ppr <- standard_ppr_scoring()
half_ppr[["receptions"]] <- 0.5
half_scored <- calculate_season_fantasy_points(stats, 2024L, half_ppr)
stopifnot(half_scored$fantasy_points[half_scored$player_id == "rb1"] == 22.5)

cache_dir <- tempfile("fantasy-points-cache-")
dir.create(file.path(cache_dir, "player_stats"), recursive = TRUE)
arrow::write_parquet(
  stats,
  file.path(cache_dir, "player_stats", "player_stats_2024.parquet")
)
stats_2023 <- stats
stats_2023$season <- 2023L
arrow::write_parquet(
  stats_2023,
  file.path(cache_dir, "player_stats", "player_stats_2023.parquet")
)
stopifnot(identical(discover_collected_player_stat_seasons(cache_dir), 2023:2024))
all_seasons <- calculate_cached_fantasy_points(data_dir = cache_dir)
stopifnot(setequal(unique(all_seasons$season), 2023:2024))
stopifnot(nrow(all_seasons) == 4L)

cat("Fantasy-points tests passed.\n")
