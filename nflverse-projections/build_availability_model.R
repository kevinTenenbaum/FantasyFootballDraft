#!/usr/bin/env Rscript

# Estimate the probability that every projected player remains available at
# every pick using Fantasy Football Calculator aggregate ADP distributions.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "availability.R"))

target_season <- 2026L
scoring_format <- "ppr"
team_count <- 12L
draft_rounds <- 15L
source_name <- "Fantasy Football Calculator"
source_base_url <- "https://fantasyfootballcalculator.com"

args <- commandArgs(trailingOnly = TRUE)
value_prefixes <- c(
  "--season=", "--scoring=", "--teams=", "--rounds=", "--adp-json=",
  "--projections=", "--output="
)
recognized <- vapply(
  args,
  function(argument) any(startsWith(argument, value_prefixes)),
  logical(1)
)
if (any(!recognized)) {
  stop(
    "Unknown argument(s): ", paste(args[!recognized], collapse = ", "),
    call. = FALSE
  )
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

target_season <- integer_option("--season=", target_season)
team_count <- integer_option("--teams=", team_count)
draft_rounds <- integer_option("--rounds=", draft_rounds)
scoring_format <- tolower(option_value("--scoring=", scoring_format))
valid_scoring_formats <- c("ppr", "half-ppr", "standard", "2qb")
if (!scoring_format %in% valid_scoring_formats) {
  stop(
    "--scoring must be one of: ",
    paste(valid_scoring_formats, collapse = ", "),
    call. = FALSE
  )
}

resolve_project_path <- function(path) {
  if (grepl("^/", path)) path else file.path(script_dir, path)
}

projection_path <- resolve_project_path(option_value(
  "--projections=",
  file.path(
    "data", "derived", paste0("simple_linear_projections_", target_season, ".csv")
  )
))
output_path <- resolve_project_path(option_value(
  "--output=",
  file.path(
    "data", "derived", paste0("player_availability_", target_season, ".csv")
  )
))
adp_json_path <- option_value("--adp-json=", NULL)
if (!is.null(adp_json_path)) adp_json_path <- resolve_project_path(adp_json_path)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Package 'jsonlite' is required. Install it with install.packages('jsonlite').",
    call. = FALSE
  )
}
if (!file.exists(projection_path)) {
  stop("Projection file does not exist: ", projection_path, call. = FALSE)
}

request_url <- paste0(
  source_base_url, "/api/v1/adp/", scoring_format,
  "?position=all&teams=", team_count,
  "&year=", target_season
)

downloaded_path <- NULL
if (is.null(adp_json_path)) {
  downloaded_path <- tempfile("public-adp-", fileext = ".json")
  on.exit(if (file.exists(downloaded_path)) file.remove(downloaded_path), add = TRUE)
  download_status <- tryCatch(
    utils::download.file(request_url, downloaded_path, mode = "wb", quiet = TRUE),
    error = function(error) error
  )
  if (inherits(download_status, "error") || download_status != 0L) {
    message <- if (inherits(download_status, "error")) {
      conditionMessage(download_status)
    } else {
      paste0("download.file returned status ", download_status)
    }
    stop("Could not download public ADP data: ", message, call. = FALSE)
  }
  adp_json_path <- downloaded_path
}

payload <- jsonlite::fromJSON(adp_json_path, simplifyDataFrame = TRUE)
if (is.null(payload$status) || payload$status != "Success") {
  stop("Public ADP response did not report a successful status.", call. = FALSE)
}
if (is.null(payload$meta) || is.null(payload$players)) {
  stop("Public ADP response is missing meta or players data.", call. = FALSE)
}

meta <- as.list(payload$meta)
if (as.integer(meta$teams) != team_count) {
  stop(
    "Public ADP settings do not match the requested league: received ",
    meta$teams, " teams instead of ", team_count, ".",
    call. = FALSE
  )
}

if (!is.null(downloaded_path)) {
  snapshot_dir <- file.path(script_dir, "data", "raw", "public_adp")
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  snapshot_path <- file.path(
    snapshot_dir,
    paste0(
      "fantasy_football_calculator_", scoring_format, "_", team_count,
      "_team_", target_season, "_", meta$end_date, ".json"
    )
  )
  if (!file.copy(downloaded_path, snapshot_path, overwrite = TRUE)) {
    stop("Could not save public ADP snapshot: ", snapshot_path, call. = FALSE)
  }
}

projections <- utils::read.csv(projection_path, stringsAsFactors = FALSE)
max_pick <- team_count * draft_rounds
availability <- build_availability_table(
  projections = projections,
  public_adp = payload$players,
  max_pick = max_pick
)

availability$season <- target_season
availability$scoring_format <- scoring_format
availability$teams <- team_count
availability$rounds <- draft_rounds
availability$source <- source_name
availability$source_url <- request_url
availability$source_total_drafts <- as.integer(meta$total_drafts)
availability$source_start_date <- as.character(meta$start_date)
availability$source_end_date <- as.character(meta$end_date)
availability$source_rounds <- as.integer(meta$rounds)
availability$calibration_method <- "slot_position_frequency"
availability$calibration_pool_size <- nrow(prepare_public_adp(payload$players))
availability$availability_probability <- round(
  availability$availability_probability,
  6L
)

leading_columns <- c(
  "season", "scoring_format", "teams", "rounds", "source",
  "source_url", "source_total_drafts", "source_start_date", "source_end_date",
  "source_rounds", "calibration_method", "calibration_pool_size"
)
availability <- availability[c(
  leading_columns,
  setdiff(names(availability), leading_columns)
)]

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(availability, output_path, row.names = FALSE, na = "")

player_rows <- availability[availability$pick == 1L, , drop = FALSE]
matched_players <- sum(player_rows$match_method != "unmatched")
cat("Built public ADP availability curves for ", nrow(player_rows),
    " projected players across picks 1-", max_pick, ".\n", sep = "")
cat("Matched ", matched_players, " players to ", source_name,
    "; ", nrow(player_rows) - matched_players,
    " players absent from public ADP remain at probability 1.\n", sep = "")
cat("Source window: ", meta$start_date, " through ", meta$end_date,
    " (", meta$total_drafts, " drafts).\n", sep = "")
cat("Modeled league: ", team_count, " teams and ", draft_rounds,
    " rounds; source sample uses ", meta$rounds, " rounds.\n", sep = "")
cat("Output: ", normalizePath(output_path, mustWork = FALSE), "\n", sep = "")
