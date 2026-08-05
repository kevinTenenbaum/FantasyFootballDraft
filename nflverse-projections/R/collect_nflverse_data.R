# Download and cache the nflverse inputs needed by a season-long player
# projection model. Data is stored as one parquet file per dataset/season so a
# failed or interrupted run can resume without downloading completed seasons.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

default_target_season <- function(today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  month <- as.integer(format(today, "%m"))
  if (month <= 2L) year - 1L else year
}

default_collection_config <- function(project_dir = getwd(), overrides = list()) {
  defaults <- list(
    project_dir = normalizePath(project_dir, mustWork = FALSE),
    output_dir = file.path(project_dir, "data", "raw"),
    manifest_dir = file.path(project_dir, "data", "manifests"),
    target_season = default_target_season(),
    history_start = 2016L,
    include_play_by_play = TRUE,
    include_participation = TRUE,
    include_expected_points = TRUE,
    include_injuries = TRUE,
    force = FALSE,
    fail_on_optional = FALSE
  )

  unknown <- setdiff(names(overrides), names(defaults))
  if (length(unknown)) {
    stop("Unknown collection config field(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  defaults[names(overrides)] <- overrides
  validate_collection_config(defaults)
}

validate_collection_config <- function(config) {
  integer_fields <- c("target_season", "history_start")
  for (field in integer_fields) {
    value <- config[[field]]
    if (length(value) != 1L || is.na(value) || value != as.integer(value)) {
      stop(field, " must be one integer.", call. = FALSE)
    }
    config[[field]] <- as.integer(value)
  }

  if (config$history_start < 1999L) {
    stop("history_start must be 1999 or later for nflverse statistics.", call. = FALSE)
  }
  if (config$history_start >= config$target_season) {
    stop("history_start must be earlier than target_season.", call. = FALSE)
  }

  logical_fields <- c(
    "include_play_by_play", "include_participation",
    "include_expected_points", "include_injuries", "force",
    "fail_on_optional"
  )
  for (field in logical_fields) {
    if (!is.logical(config[[field]]) || length(config[[field]]) != 1L ||
        is.na(config[[field]])) {
      stop(field, " must be TRUE or FALSE.", call. = FALSE)
    }
  }

  config$history_seasons <- seq.int(
    config$history_start,
    config$target_season - 1L
  )
  config$context_seasons <- seq.int(
    config$history_start,
    config$target_season
  )
  config
}

parse_integer_option <- function(argument, prefix) {
  value <- sub(paste0("^", prefix, "="), "", argument)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) stop(prefix, " must be an integer.", call. = FALSE)
  parsed
}

apply_command_line_args <- function(config, args) {
  known_flags <- c(
    "--skip-pbp", "--skip-participation", "--skip-expected-points",
    "--skip-injuries", "--force", "--fail-on-optional"
  )
  value_options <- c("--start=", "--target=", "--output=")
  recognized <- args %in% known_flags |
    vapply(args, function(x) any(startsWith(x, value_options)), logical(1))
  if (any(!recognized)) {
    stop("Unknown argument(s): ", paste(args[!recognized], collapse = ", "),
         call. = FALSE)
  }

  start_arg <- args[startsWith(args, "--start=")]
  target_arg <- args[startsWith(args, "--target=")]
  output_arg <- args[startsWith(args, "--output=")]
  if (length(start_arg)) config$history_start <- parse_integer_option(start_arg[[1]], "--start")
  if (length(target_arg)) config$target_season <- parse_integer_option(target_arg[[1]], "--target")
  if (length(output_arg)) {
    output <- sub("^--output=", "", output_arg[[1]])
    if (!nzchar(output)) stop("--output cannot be empty.", call. = FALSE)
    if (!grepl("^/", output)) output <- file.path(config$project_dir, output)
    config$output_dir <- normalizePath(output, mustWork = FALSE)
    config$manifest_dir <- file.path(dirname(config$output_dir), "manifests")
  }

  if ("--skip-pbp" %in% args) config$include_play_by_play <- FALSE
  if ("--skip-participation" %in% args) config$include_participation <- FALSE
  if ("--skip-expected-points" %in% args) config$include_expected_points <- FALSE
  if ("--skip-injuries" %in% args) config$include_injuries <- FALSE
  if ("--force" %in% args) config$force <- TRUE
  if ("--fail-on-optional" %in% args) config$fail_on_optional <- TRUE

  validate_collection_config(config)
}

assert_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing required R package(s): ", paste(missing, collapse = ", "),
      ". Install them with install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), ")).",
      call. = FALSE
    )
  }
}

canonical_depth_chart_columns <- function() {
  c(
    "season", "week", "snapshot_date", "team", "player_id", "player_name",
    "position", "depth_position", "depth_rank", "formation", "source"
  )
}

normalize_depth_charts <- function(data, season) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  legacy_columns <- c(
    "season", "club_code", "week", "depth_team", "gsis_id", "position",
    "depth_position", "full_name"
  )
  modern_columns <- c(
    "dt", "team", "player_name", "gsis_id", "pos_abb", "pos_rank"
  )

  if (all(legacy_columns %in% names(data))) {
    normalized <- data.frame(
      season = as.integer(data$season),
      week = as.integer(data$week),
      snapshot_date = NA_character_,
      team = as.character(data$club_code),
      player_id = as.character(data$gsis_id),
      player_name = as.character(data$full_name),
      position = as.character(data$position),
      depth_position = as.character(data$depth_position),
      depth_rank = suppressWarnings(as.integer(data$depth_team)),
      formation = if ("formation" %in% names(data)) {
        as.character(data$formation)
      } else {
        NA_character_
      },
      source = "gsis_weekly",
      stringsAsFactors = FALSE
    )
  } else if (all(modern_columns %in% names(data))) {
    normalized <- data.frame(
      # The 2025 file includes snapshots from early 2026, so the requested NFL
      # season is more reliable than the calendar year embedded in dt.
      season = rep(as.integer(season), nrow(data)),
      week = rep(NA_integer_, nrow(data)),
      snapshot_date = as.character(data$dt),
      team = as.character(data$team),
      player_id = as.character(data$gsis_id),
      player_name = as.character(data$player_name),
      position = as.character(data$pos_abb),
      depth_position = as.character(data$pos_abb),
      depth_rank = suppressWarnings(as.integer(data$pos_rank)),
      formation = if ("pos_grp" %in% names(data)) {
        as.character(data$pos_grp)
      } else {
        NA_character_
      },
      source = "espn_snapshot",
      stringsAsFactors = FALSE
    )
  } else {
    stop(
      "Unrecognized nflverse depth-chart schema. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  normalized[canonical_depth_chart_columns()]
}

make_nflreadr_loaders <- function() {
  assert_packages(c("nflreadr", "arrow"))
  list(
    player_stats = function(season) nflreadr::load_player_stats(
      seasons = season, summary_level = "week", file_type = "parquet"
    ),
    team_stats = function(season) nflreadr::load_team_stats(
      seasons = season, summary_level = "week", file_type = "parquet"
    ),
    rosters = function(season) nflreadr::load_rosters(
      seasons = season, file_type = "parquet"
    ),
    weekly_rosters = function(season) nflreadr::load_rosters_weekly(
      seasons = season, file_type = "parquet"
    ),
    depth_charts = function(season) normalize_depth_charts(
      nflreadr::load_depth_charts(seasons = season), season
    ),
    snap_counts = function(season) nflreadr::load_snap_counts(
      seasons = season, file_type = "parquet"
    ),
    injuries = function(season) nflreadr::load_injuries(
      seasons = season, file_type = "parquet"
    ),
    play_by_play = function(season) nflreadr::load_pbp(
      seasons = season, file_type = "parquet"
    ),
    participation = function(season) nflreadr::load_participation(
      seasons = season, include_pbp = FALSE, file_type = "parquet"
    ),
    expected_points = function(season) nflreadr::load_ff_opportunity(
      seasons = season, stat_type = "weekly", model_version = "latest"
    ),
    players = function(seasons) nflreadr::load_players(file_type = "parquet"),
    player_ids = function(seasons) nflreadr::load_ff_playerids(),
    schedules = function(seasons) nflreadr::load_schedules(seasons = seasons),
    draft_picks = function(seasons) nflreadr::load_draft_picks(
      seasons = seasons, file_type = "parquet"
    ),
    combine = function(seasons) nflreadr::load_combine(
      seasons = seasons, file_type = "parquet"
    )
  )
}

dataset_contracts <- function(config) {
  historical <- config$history_seasons
  context <- config$context_seasons

  specs <- list(
    list(
      name = "player_stats", seasons = historical, by_season = TRUE,
      required = TRUE,
      columns = c("player_id", "position", "season", "week", "season_type",
                  "team", "attempts", "carries", "targets", "receptions")
    ),
    list(
      name = "team_stats", seasons = historical, by_season = TRUE,
      required = TRUE,
      columns = c("team", "season", "week", "season_type", "attempts", "carries")
    ),
    list(
      name = "rosters", seasons = context, by_season = TRUE,
      required = TRUE,
      columns = c("season", "team", "position")
    ),
    list(
      name = "weekly_rosters", seasons = context, by_season = TRUE,
      required = FALSE,
      columns = c("season", "week", "team", "position")
    ),
    list(
      name = "depth_charts", seasons = context, by_season = TRUE,
      required = FALSE,
      columns = canonical_depth_chart_columns()
    ),
    list(
      name = "snap_counts", seasons = historical, by_season = TRUE,
      required = FALSE,
      columns = c("season", "week", "team", "position", "offense_snaps")
    ),
    list(
      name = "players", seasons = context, by_season = FALSE,
      required = TRUE,
      columns = c("gsis_id", "display_name", "position", "birth_date")
    ),
    list(
      name = "player_ids", seasons = context, by_season = FALSE,
      required = TRUE,
      columns = c("gsis_id", "name", "position")
    ),
    list(
      name = "schedules", seasons = context, by_season = FALSE,
      required = TRUE,
      columns = c("season", "week", "home_team", "away_team")
    ),
    list(
      name = "draft_picks", seasons = seq.int(max(2000L, config$history_start - 10L),
                                                config$target_season),
      by_season = FALSE, required = TRUE,
      columns = c("season", "round", "pick", "team", "position")
    ),
    list(
      name = "combine", seasons = seq.int(max(2000L, config$history_start - 10L),
                                            config$target_season),
      by_season = FALSE, required = FALSE,
      columns = c("season", "player_name", "pos")
    )
  )

  if (config$include_injuries) {
    specs[[length(specs) + 1L]] <- list(
      name = "injuries", seasons = historical, by_season = TRUE,
      required = FALSE,
      columns = c("season", "week", "team", "position")
    )
  }
  if (config$include_play_by_play) {
    specs[[length(specs) + 1L]] <- list(
      name = "play_by_play", seasons = historical, by_season = TRUE,
      required = TRUE,
      columns = c("season", "week", "game_id", "posteam", "play_type",
                  "pass_attempt", "rush_attempt", "yardline_100")
    )
  }
  if (config$include_participation) {
    specs[[length(specs) + 1L]] <- list(
      name = "participation", seasons = historical, by_season = TRUE,
      required = FALSE,
      # Participation files are keyed to play-by-play; season and week are
      # intentionally added later by joining nflverse_game_id + play_id.
      columns = c("nflverse_game_id", "play_id", "offense_players",
                  "defense_players")
    )
  }
  if (config$include_expected_points) {
    specs[[length(specs) + 1L]] <- list(
      name = "expected_points", seasons = historical, by_season = TRUE,
      required = FALSE,
      columns = c("season", "week")
    )
  }
  specs
}

prepare_for_parquet <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  names(data) <- make.unique(names(data), sep = "_")

  list_columns <- vapply(data, is.list, logical(1))
  if (any(list_columns)) {
    assert_packages("jsonlite")
    data[list_columns] <- lapply(data[list_columns], function(column) {
      vapply(column, function(value) {
        if (is.null(value) || length(value) == 0L) return(NA_character_)
        jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
      }, character(1))
    })
  }
  data
}

validate_dataset <- function(data, required_columns, dataset, season = NA_integer_) {
  if (!is.data.frame(data)) {
    stop(dataset, " did not return a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop(dataset, " returned zero rows", if (!is.na(season)) paste0(" for ", season),
         ".", call. = FALSE)
  }
  missing <- setdiff(required_columns, names(data))
  if (length(missing)) {
    stop(dataset, " is missing required column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(data)
}

dataset_path <- function(config, dataset, season = NA_integer_) {
  directory <- file.path(config$output_dir, dataset)
  filename <- if (is.na(season)) {
    paste0(dataset, ".parquet")
  } else {
    paste0(dataset, "_", season, ".parquet")
  }
  file.path(directory, filename)
}

write_parquet_atomic <- function(data, path) {
  assert_packages("arrow")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) file.remove(temporary), add = TRUE)
  arrow::write_parquet(prepare_for_parquet(data), temporary, compression = "zstd")
  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace existing file: ", path, call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop("Could not move completed parquet file into place: ", path, call. = FALSE)
  }
  invisible(path)
}

new_manifest_row <- function(dataset, season, required, status, path,
                             rows = NA_integer_, columns = NA_integer_,
                             message = "") {
  data.frame(
    dataset = dataset,
    season = if (is.na(season)) NA_integer_ else as.integer(season),
    required = required,
    status = status,
    rows = as.integer(rows),
    columns = as.integer(columns),
    bytes = if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_,
    path = normalizePath(path, mustWork = FALSE),
    collected_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    message = message,
    stringsAsFactors = FALSE
  )
}

collect_one_dataset <- function(spec, season, loader, config) {
  path <- dataset_path(config, spec$name, season)
  if (!config$force && file.exists(path) && file.info(path)$size > 0) {
    cached_columns <- tryCatch(
      names(arrow::read_parquet(path, as_data_frame = FALSE)),
      error = function(error) character()
    )
    if (all(spec$columns %in% cached_columns)) {
      return(new_manifest_row(
        spec$name, season, spec$required, "cached", path,
        message = "Reused existing parquet file with a valid schema."
      ))
    }
    message("Refreshing ", spec$name, if (!is.na(season)) paste0(" ", season) else "",
            " because its cached schema is outdated.")
  }

  label <- paste0(spec$name, if (!is.na(season)) paste0(" ", season) else "")
  message("Collecting ", label, " ...")
  tryCatch({
    argument <- if (is.na(season)) spec$seasons else season
    data <- loader(argument)
    validate_dataset(data, spec$columns, spec$name, season)
    write_parquet_atomic(data, path)
    new_manifest_row(
      spec$name, season, spec$required, "ok", path,
      rows = nrow(data), columns = ncol(data)
    )
  }, error = function(error) {
    new_manifest_row(
      spec$name, season, spec$required, "error", path,
      message = conditionMessage(error)
    )
  })
}

write_manifest <- function(manifest, config) {
  dir.create(config$manifest_dir, recursive = TRUE, showWarnings = FALSE)
  latest_path <- file.path(config$manifest_dir, "latest.csv")
  timestamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
  archive_path <- file.path(config$manifest_dir, paste0("collection_", timestamp, ".csv"))
  utils::write.csv(manifest, latest_path, row.names = FALSE, na = "")
  utils::write.csv(manifest, archive_path, row.names = FALSE, na = "")
  invisible(c(latest = latest_path, archive = archive_path))
}

collect_nflverse_data <- function(config = default_collection_config(), loaders = NULL) {
  config <- validate_collection_config(config)
  assert_packages("arrow")
  if (is.null(loaders)) loaders <- make_nflreadr_loaders()

  specs <- dataset_contracts(config)
  missing_loaders <- setdiff(vapply(specs, `[[`, character(1), "name"), names(loaders))
  if (length(missing_loaders)) {
    stop("No loader supplied for: ", paste(missing_loaders, collapse = ", "),
         call. = FALSE)
  }

  rows <- list()
  for (spec in specs) {
    seasons <- if (isTRUE(spec$by_season)) spec$seasons else NA_integer_
    for (season in seasons) {
      rows[[length(rows) + 1L]] <- collect_one_dataset(
        spec, season, loaders[[spec$name]], config
      )
    }
  }
  manifest <- do.call(rbind, rows)
  write_manifest(manifest, config)

  required_failures <- manifest$status == "error" & manifest$required
  optional_failures <- manifest$status == "error" & !manifest$required
  if (any(required_failures) || (config$fail_on_optional && any(optional_failures))) {
    failures <- manifest[required_failures | (config$fail_on_optional & optional_failures), ]
    stop(
      "Collection completed with blocking failures: ",
      paste(paste0(failures$dataset, " ", failures$season, ": ", failures$message),
            collapse = "; "),
      ". See data/manifests/latest.csv for the full report.",
      call. = FALSE
    )
  }
  manifest
}

print_collection_summary <- function(manifest, config) {
  counts <- table(factor(manifest$status, levels = c("ok", "cached", "error")))
  cat("\nNFLVERSE COLLECTION SUMMARY\n")
  cat("Target season:", config$target_season, "\n")
  cat("Historical seasons:", min(config$history_seasons), "-",
      max(config$history_seasons), "\n")
  cat("Downloaded:", counts[["ok"]], " Cached:", counts[["cached"]],
      " Errors:", counts[["error"]], "\n")
  cat("Manifest:", file.path(config$manifest_dir, "latest.csv"), "\n")

  failures <- manifest[manifest$status == "error", ]
  if (nrow(failures)) {
    cat("\nNon-blocking source gaps/errors:\n")
    for (i in seq_len(nrow(failures))) {
      cat("-", failures$dataset[[i]], failures$season[[i]], ":",
          failures$message[[i]], "\n")
    }
  }
  invisible(manifest)
}
