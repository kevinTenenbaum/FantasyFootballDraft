# Convert nflverse weekly player statistics into season fantasy-point totals for
# arbitrary linear scoring rules. Defense and kickers are intentionally excluded.

skill_positions <- c("QB", "RB", "WR", "TE")

standard_ppr_scoring <- function() {
  c(
    passing_yards = 0.04,
    passing_tds = 4,
    passing_interceptions = -2,
    passing_2pt_conversions = 2,
    rushing_yards = 0.10,
    rushing_tds = 6,
    rushing_2pt_conversions = 2,
    receptions = 1,
    receiving_yards = 0.10,
    receiving_tds = 6,
    receiving_2pt_conversions = 2,
    fumbles_lost_total = -2,
    special_teams_tds = 6
  )
}

validate_scoring_rules <- function(scoring_rules) {
  if (is.list(scoring_rules)) scoring_rules <- unlist(scoring_rules, use.names = TRUE)
  if (!is.numeric(scoring_rules) || is.null(names(scoring_rules)) ||
      any(!nzchar(names(scoring_rules)))) {
    stop("scoring_rules must be a named numeric vector or named list.", call. = FALSE)
  }
  if (anyDuplicated(names(scoring_rules))) {
    stop("scoring_rules contains duplicate stat names.", call. = FALSE)
  }
  if (any(!is.finite(scoring_rules))) {
    stop("All scoring-rule values must be finite numbers.", call. = FALSE)
  }
  scoring_rules
}

validate_player_stats <- function(player_stats, season, scoring_rules) {
  if (!is.data.frame(player_stats)) {
    stop("player_stats must be a data frame.", call. = FALSE)
  }
  identity_columns <- c("player_id", "position", "season", "season_type", "team")
  missing_identity <- setdiff(identity_columns, names(player_stats))
  if (length(missing_identity)) {
    stop("player_stats is missing identity column(s): ",
         paste(missing_identity, collapse = ", "), call. = FALSE)
  }
  missing_stats <- setdiff(names(scoring_rules)[scoring_rules != 0], names(player_stats))
  if (length(missing_stats)) {
    stop("player_stats is missing scored stat column(s): ",
         paste(missing_stats, collapse = ", "), call. = FALSE)
  }
  if (length(season) != 1L || is.na(season) || season != as.integer(season)) {
    stop("season must be one integer.", call. = FALSE)
  }
  invisible(TRUE)
}

player_display_names <- function(player_stats) {
  if ("player_display_name" %in% names(player_stats)) {
    display <- as.character(player_stats$player_display_name)
  } else {
    display <- rep(NA_character_, nrow(player_stats))
  }
  if ("player_name" %in% names(player_stats)) {
    fallback <- as.character(player_stats$player_name)
    use_fallback <- is.na(display) | !nzchar(display)
    display[use_fallback] <- fallback[use_fallback]
  }
  display
}

score_player_weeks <- function(player_stats, scoring_rules) {
  scoring_rules <- validate_scoring_rules(scoring_rules)
  points <- numeric(nrow(player_stats))
  for (stat in names(scoring_rules)) {
    if (!stat %in% names(player_stats)) next
    values <- suppressWarnings(as.numeric(player_stats[[stat]]))
    values[is.na(values)] <- 0
    points <- points + values * unname(scoring_rules[[stat]])
  }
  points
}

collapse_teams <- function(team) {
  team <- unique(as.character(team[!is.na(team) & nzchar(team)]))
  if (length(team)) paste(team, collapse = "/") else NA_character_
}

count_games <- function(group) {
  if ("game_id" %in% names(group)) {
    game_ids <- unique(group$game_id[!is.na(group$game_id) & nzchar(group$game_id)])
    if (length(game_ids)) return(length(game_ids))
  }
  if ("week" %in% names(group)) return(length(unique(group$week[!is.na(group$week)])))
  nrow(group)
}

calculate_season_fantasy_points <- function(
    player_stats,
    season,
    scoring_rules = standard_ppr_scoring(),
    season_types = "REG") {
  scoring_rules <- validate_scoring_rules(scoring_rules)
  validate_player_stats(player_stats, season, scoring_rules)
  season <- as.integer(season)

  keep <- player_stats$season == season &
    player_stats$season_type %in% season_types &
    player_stats$position %in% skill_positions
  stats <- player_stats[keep, , drop = FALSE]
  if (!nrow(stats)) {
    stop("No QB/RB/WR/TE rows found for season ", season,
         " and season type(s) ", paste(season_types, collapse = ", "), ".",
         call. = FALSE)
  }

  stats$player_name_for_output <- player_display_names(stats)
  stats$fantasy_points_for_output <- score_player_weeks(stats, scoring_rules)

  stat_columns <- intersect(names(scoring_rules), names(stats))
  split_rows <- split(stats, stats$player_id, drop = TRUE)
  results <- lapply(split_rows, function(group) {
    summed_stats <- vapply(stat_columns, function(stat) {
      sum(suppressWarnings(as.numeric(group[[stat]])), na.rm = TRUE)
    }, numeric(1))
    names(summed_stats) <- stat_columns

    name_values <- group$player_name_for_output
    name_values <- name_values[!is.na(name_values) & nzchar(name_values)]
    player_name <- if (length(name_values)) tail(name_values, 1L) else NA_character_

    identity <- data.frame(
      player_id = as.character(group$player_id[[1]]),
      player_name = player_name,
      position = as.character(group$position[[1]]),
      season = season,
      teams = collapse_teams(group$team),
      games = count_games(group),
      fantasy_points = sum(group$fantasy_points_for_output, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    cbind(identity, as.data.frame(as.list(summed_stats), check.names = FALSE))
  })

  output <- do.call(rbind, results)
  rownames(output) <- NULL
  output <- output[order(output$position, -output$fantasy_points, output$player_name), ]
  output$position_rank <- ave(
    output$fantasy_points,
    output$position,
    FUN = function(points) rank(-points, ties.method = "min")
  )
  output <- output[order(-output$fantasy_points, output$position, output$player_name), ]
  output$fantasy_points <- round(output$fantasy_points, 2)
  rownames(output) <- NULL

  leading <- c(
    "player_id", "player_name", "position", "position_rank", "season", "teams",
    "games", "fantasy_points"
  )
  output[c(leading, setdiff(names(output), leading))]
}

read_collected_player_stats <- function(season, data_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The arrow package is required to read collected parquet files.",
         call. = FALSE)
  }
  path <- file.path(
    data_dir,
    "player_stats",
    paste0("player_stats_", as.integer(season), ".parquet")
  )
  if (!file.exists(path)) {
    stop("Collected player-stat file does not exist: ", path,
         ". Run collect_data.R first.", call. = FALSE)
  }
  arrow::read_parquet(path)
}

calculate_cached_season_fantasy_points <- function(
    season,
    scoring_rules = standard_ppr_scoring(),
    data_dir,
    season_types = "REG") {
  player_stats <- read_collected_player_stats(season, data_dir)
  calculate_season_fantasy_points(
    player_stats = player_stats,
    season = season,
    scoring_rules = scoring_rules,
    season_types = season_types
  )
}

discover_collected_player_stat_seasons <- function(data_dir) {
  directory <- file.path(data_dir, "player_stats")
  if (!dir.exists(directory)) {
    stop("Collected player-stat directory does not exist: ", directory,
         ". Run collect_data.R first.", call. = FALSE)
  }
  files <- list.files(
    directory,
    pattern = "^player_stats_[0-9]{4}\\.parquet$",
    full.names = FALSE
  )
  seasons <- suppressWarnings(as.integer(sub(
    "^player_stats_([0-9]{4})\\.parquet$", "\\1", files
  )))
  seasons <- sort(unique(seasons[!is.na(seasons)]))
  if (!length(seasons)) {
    stop("No collected player_stats_YEAR.parquet files found in ", directory, ".",
         call. = FALSE)
  }
  seasons
}

calculate_cached_fantasy_points <- function(
    scoring_rules = standard_ppr_scoring(),
    data_dir,
    seasons = NULL,
    season_types = "REG") {
  available <- discover_collected_player_stat_seasons(data_dir)
  if (is.null(seasons)) seasons <- available
  seasons <- sort(unique(as.integer(seasons)))
  missing <- setdiff(seasons, available)
  if (length(missing)) {
    stop("No collected player-stat file for season(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  results <- lapply(seasons, function(season) {
    message("Scoring season ", season, " ...")
    calculate_cached_season_fantasy_points(
      season = season,
      scoring_rules = scoring_rules,
      data_dir = data_dir,
      season_types = season_types
    )
  })
  output <- do.call(rbind, results)
  rownames(output) <- NULL
  output[order(output$season, -output$fantasy_points, output$position), ]
}
