#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0) {
    return(default)
  }
  sub(prefix, "", match[[length(match)]], fixed = TRUE)
}

start_season <- as.integer(arg_value("start-season", "2010"))
end_season <- as.integer(
  arg_value("end-season", as.character(as.integer(format(Sys.Date(), "%Y")) - 1L))
)
scoring <- tolower(arg_value("scoring", "ppr"))
teams <- as.integer(arg_value("teams", "12"))
raw_dir <- arg_value("raw-dir", "data/raw/public_adp")
output_path <- arg_value(
  "output",
  sprintf("data/derived/public_adp_history_%d_team_%s.csv", teams, scoring)
)
summary_path <- arg_value(
  "summary",
  sprintf("data/derived/public_adp_history_%d_team_%s_summary.csv", teams, scoring)
)

if (is.na(start_season) || is.na(end_season) || start_season > end_season) {
  stop("Provide a valid season range with --start-season=YEAR and --end-season=YEAR.")
}
if (is.na(teams) || teams < 2L) {
  stop("--teams must be an integer of at least 2.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required.")
}

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)

scalar_character <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  as.character(value[[1L]])
}

scalar_numeric <- function(value, default = NA_real_) {
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  as.numeric(value[[1L]])
}

normalize_position <- function(position) {
  position <- toupper(scalar_character(position))
  if (position %in% c("DEF", "D/ST", "DST")) {
    return("DST")
  }
  if (position %in% c("PK", "K")) {
    return("K")
  }
  position
}

download_season <- function(season) {
  source_url <- sprintf(
    "https://fantasyfootballcalculator.com/api/v1/adp/%s?position=all&teams=%d&year=%d",
    utils::URLencode(scoring, reserved = TRUE),
    teams,
    season
  )
  downloaded_path <- tempfile(fileext = ".json")
  on.exit(unlink(downloaded_path), add = TRUE)

  downloaded <- tryCatch(
    {
      suppressWarnings(utils::download.file(
        source_url,
        downloaded_path,
        mode = "wb",
        quiet = TRUE
      ))
      TRUE
    },
    error = function(error) {
      warning(sprintf("Season %d download failed: %s", season, conditionMessage(error)))
      FALSE
    }
  )
  if (!downloaded) {
    return(NULL)
  }

  response <- tryCatch(
    jsonlite::fromJSON(downloaded_path, simplifyVector = FALSE),
    error = function(error) {
      warning(sprintf("Season %d response was not valid JSON: %s", season, conditionMessage(error)))
      NULL
    }
  )
  if (is.null(response)) {
    return(NULL)
  }

  status <- scalar_character(response$status, "Error")
  if (!identical(tolower(status), "success") || length(response$players) == 0L) {
    warning(sprintf("Season %d has no archived ADP data (status: %s).", season, status))
    return(NULL)
  }

  end_date <- scalar_character(response$meta$end_date, "unknown-date")
  safe_end_date <- gsub("[^0-9-]", "", end_date)
  if (!nzchar(safe_end_date)) {
    safe_end_date <- "unknown-date"
  }
  raw_path <- file.path(
    raw_dir,
    sprintf(
      "fantasy_football_calculator_%s_%d_team_%d_%s.json",
      scoring,
      teams,
      season,
      safe_end_date
    )
  )
  if (!file.copy(downloaded_path, raw_path, overwrite = TRUE)) {
    stop(sprintf("Could not preserve the raw response at %s.", raw_path))
  }

  meta <- response$meta
  player_rows <- lapply(response$players, function(player) {
    data.frame(
      season = season,
      scoring_format = scoring,
      teams = teams,
      source = "Fantasy Football Calculator",
      source_url = source_url,
      source_total_drafts = scalar_numeric(meta$total_drafts),
      source_start_date = scalar_character(meta$start_date),
      source_end_date = scalar_character(meta$end_date),
      source_rounds = scalar_numeric(meta$rounds),
      source_player_id = scalar_character(player$player_id),
      player_name = scalar_character(player$name),
      position = normalize_position(player$position),
      team = scalar_character(player$team),
      adp = scalar_numeric(player$adp),
      adp_formatted = scalar_character(player$adp_formatted),
      adp_sd = scalar_numeric(player$stdev),
      times_drafted = scalar_numeric(player$times_drafted),
      observed_earliest_pick = scalar_numeric(player$high),
      observed_latest_pick = scalar_numeric(player$low),
      bye = scalar_numeric(player$bye),
      stringsAsFactors = FALSE
    )
  })
  players <- do.call(rbind, player_rows)

  list(
    players = players,
    summary = data.frame(
      season = season,
      source_start_date = scalar_character(meta$start_date),
      source_end_date = scalar_character(meta$end_date),
      source_total_drafts = scalar_numeric(meta$total_drafts),
      source_rounds = scalar_numeric(meta$rounds),
      player_count = nrow(players),
      qb_count = sum(players$position == "QB", na.rm = TRUE),
      rb_count = sum(players$position == "RB", na.rm = TRUE),
      wr_count = sum(players$position == "WR", na.rm = TRUE),
      te_count = sum(players$position == "TE", na.rm = TRUE),
      dst_count = sum(players$position == "DST", na.rm = TRUE),
      kicker_count = sum(players$position == "K", na.rm = TRUE),
      raw_file = raw_path,
      stringsAsFactors = FALSE
    )
  )
}

results <- vector("list", end_season - start_season + 1L)
names(results) <- as.character(seq.int(start_season, end_season))

for (season in seq.int(start_season, end_season)) {
  message(sprintf("Downloading %d %d-team %s ADP...", season, teams, toupper(scoring)))
  result <- download_season(season)
  if (!is.null(result)) {
    results[[as.character(season)]] <- result
    message(sprintf(
      "  %d players from %s drafts (%s through %s)",
      nrow(result$players),
      format(result$summary$source_total_drafts, big.mark = ",", scientific = FALSE),
      result$summary$source_start_date,
      result$summary$source_end_date
    ))
  }
}

results <- Filter(Negate(is.null), results)
if (length(results) == 0L) {
  stop("No archived ADP seasons were available for the requested range.")
}

historical_adp <- do.call(rbind, lapply(results, `[[`, "players"))
historical_adp <- historical_adp[order(historical_adp$season, historical_adp$adp), ]
row.names(historical_adp) <- NULL

coverage <- do.call(rbind, lapply(results, `[[`, "summary"))
coverage <- coverage[order(coverage$season), ]
row.names(coverage) <- NULL

utils::write.csv(historical_adp, output_path, row.names = FALSE, na = "")
utils::write.csv(coverage, summary_path, row.names = FALSE, na = "")

message(sprintf(
  "Wrote %s player-season rows across %d seasons to %s",
  format(nrow(historical_adp), big.mark = ",", scientific = FALSE),
  nrow(coverage),
  output_path
))
message(sprintf("Wrote the coverage audit to %s", summary_path))
