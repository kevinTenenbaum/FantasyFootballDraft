#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "fantasy_points.R"))

# Edit the point values here for your league. Values are fantasy points per unit
# of the corresponding nflverse statistic. By default, every collected season is
# scored and combined into one flat file.
scoring_rules <- c(
  passing_yards = 0.04,            # 1 point per 25 passing yards
  passing_tds = 4,
  passing_interceptions = -2,
  passing_2pt_conversions = 2,
  rushing_yards = 0.10,            # 1 point per 10 rushing yards
  rushing_tds = 6,
  rushing_2pt_conversions = 2,
  receptions = 1,                  # set to 0.5 for half-PPR or 0 for standard
  receiving_yards = 0.10,
  receiving_tds = 6,
  receiving_2pt_conversions = 2,
  fumbles_lost_total = -2,
  special_teams_tds = 6
)

args <- commandArgs(trailingOnly = TRUE)
start_arg <- args[startsWith(args, "--start=")]
end_arg <- args[startsWith(args, "--end=")]
data_arg <- args[startsWith(args, "--data=")]
output_arg <- args[startsWith(args, "--output=")]
known <- startsWith(args, "--start=") | startsWith(args, "--end=") |
  startsWith(args, "--data=") | startsWith(args, "--output=")
if (any(!known)) {
  stop("Unknown argument(s): ", paste(args[!known], collapse = ", "), call. = FALSE)
}

data_dir <- file.path(script_dir, "data", "raw")
if (length(data_arg)) {
  data_dir <- sub("^--data=", "", data_arg[[1]])
  if (!grepl("^/", data_dir)) data_dir <- file.path(script_dir, data_dir)
}

available_seasons <- discover_collected_player_stat_seasons(data_dir)
start_season <- min(available_seasons)
end_season <- max(available_seasons)
if (length(start_arg)) {
  start_season <- suppressWarnings(as.integer(sub("^--start=", "", start_arg[[1]])))
  if (is.na(start_season)) stop("--start must be an integer.", call. = FALSE)
}
if (length(end_arg)) {
  end_season <- suppressWarnings(as.integer(sub("^--end=", "", end_arg[[1]])))
  if (is.na(end_season)) stop("--end must be an integer.", call. = FALSE)
}
if (start_season > end_season) {
  stop("--start cannot be later than --end.", call. = FALSE)
}
seasons <- available_seasons[
  available_seasons >= start_season & available_seasons <= end_season
]
if (!length(seasons)) {
  stop("No collected seasons fall between ", start_season, " and ", end_season, ".",
       call. = FALSE)
}

output_path <- file.path(
  script_dir, "data", "derived", "fantasy_points_all_seasons.csv"
)
if (length(output_arg)) {
  output_path <- sub("^--output=", "", output_arg[[1]])
  if (!grepl("^/", output_path)) output_path <- file.path(script_dir, output_path)
}

points <- calculate_cached_fantasy_points(
  scoring_rules = scoring_rules,
  data_dir = data_dir,
  seasons = seasons
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(points, output_path, row.names = FALSE, na = "")

season_word <- if (length(seasons) == 1L) "season" else "seasons"
cat("Scored", nrow(points), "player-seasons across", length(seasons), season_word, "(",
    min(seasons), "-", max(seasons), ")\n")
cat("Output:", normalizePath(output_path, mustWork = FALSE), "\n\n")
print(utils::head(points[c(
  "season", "player_name", "position", "position_rank", "teams", "games",
  "fantasy_points"
)], 20L), row.names = FALSE)
