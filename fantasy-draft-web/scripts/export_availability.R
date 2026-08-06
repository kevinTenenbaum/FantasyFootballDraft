args <- commandArgs(trailingOnly = TRUE)

input_path <- if (length(args) >= 1) args[[1]] else "../nflverse-projections/data/derived/player_availability_12_team_2026.csv"
output_path <- if (length(args) >= 2) args[[2]] else "public/availability.json"

availability <- read.csv(input_path, stringsAsFactors = FALSE)
required_columns <- c(
  "season", "scoring_format", "teams", "rounds", "source",
  "source_total_drafts", "source_start_date", "source_end_date", "player_id",
  "source_rounds", "calibration_method", "calibration_pool_size",
  "match_method", "adp", "pick", "availability_probability"
)
missing_columns <- setdiff(required_columns, names(availability))
if (length(missing_columns)) {
  stop(
    "Availability data is missing required column(s): ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

model_rows <- unique(availability[c(
  "season", "scoring_format", "teams", "rounds", "source",
  "source_total_drafts", "source_start_date", "source_end_date",
  "source_rounds", "calibration_method", "calibration_pool_size"
)])
if (nrow(model_rows) != 1L) {
  stop("Availability data must contain exactly one model configuration.", call. = FALSE)
}

matched <- availability[availability$match_method != "unmatched", , drop = FALSE]
matched <- matched[order(matched$player_id, matched$pick), , drop = FALSE]
player_rows <- split(matched, matched$player_id)
players <- lapply(player_rows, function(rows) {
  expected_picks <- seq_len(max(rows$pick))
  if (!identical(as.integer(rows$pick), expected_picks)) {
    stop("Availability curve has missing or duplicate picks for ", rows$player_id[[1L]], ".", call. = FALSE)
  }
  list(
    matchMethod = rows$match_method[[1L]],
    adp = round(as.numeric(rows$adp[[1L]]), 1L),
    probabilities = unname(round(rows$availability_probability, 6L))
  )
})

model <- model_rows[1L, ]
payload <- list(
  model = list(
    season = as.integer(model$season),
    scoringFormat = model$scoring_format,
    teamCount = as.integer(model$teams),
    rounds = as.integer(model$rounds),
    source = model$source,
    totalDrafts = as.integer(model$source_total_drafts),
    startDate = model$source_start_date,
    endDate = model$source_end_date,
    sourceRounds = as.integer(model$source_rounds),
    calibrationMethod = model$calibration_method,
    calibrationPoolSize = as.integer(model$calibration_pool_size),
    maxPick = as.integer(max(availability$pick))
  ),
  players = players
)

jsonlite::write_json(payload, output_path, auto_unbox = TRUE, digits = 6L, pretty = FALSE)
cat("Wrote", length(players), "matched player availability curves to", output_path, "\n")
