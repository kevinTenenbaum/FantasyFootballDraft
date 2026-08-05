#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "availability.R"))

curve <- bounded_draft_survival(adp = 5, adp_sd = 2, max_pick = 10L)
stopifnot(length(curve) == 10L)
stopifnot(curve[[1L]] == 1)
stopifnot(all(diff(curve) <= 0))
stopifnot(all(curve >= 0 & curve <= 1))

point_curve <- bounded_draft_survival(adp = 4, adp_sd = 0, max_pick = 10L)
stopifnot(identical(point_curve, c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0)))

projections <- data.frame(
  player_id = c("gsis-1", "gsis-2", "gsis-3", "gsis-4"),
  player_name = c(
    "D.J. Example", "Traded Player Jr.", "Kenneth Example", "Unlisted Player"
  ),
  team = c("LA", "BUF", "TB", "CHI"),
  position = c("WR", "RB", "RB", "TE"),
  stringsAsFactors = FALSE
)
public_adp <- data.frame(
  player_id = c(101, 102, 103),
  name = c("DJ Example", "Traded Player", "Kenny Example"),
  position = c("WR", "RB", "RB"),
  team = c("LAR", "MIA", "TB"),
  adp = c(5, 8, 9),
  times_drafted = c(100, 80, 60),
  high = c(1, 3, 4),
  low = c(10, 14, 15),
  stdev = c(2, 3, 3),
  stringsAsFactors = FALSE
)

availability <- build_availability_table(
  projections,
  public_adp,
  max_pick = 10L
)
stopifnot(nrow(availability) == 40L)
first_rows <- availability[availability$pick == 1L, ]
stopifnot(first_rows$match_method[[1L]] == "name_position_team")
stopifnot(first_rows$match_method[[2L]] == "name_position")
stopifnot(first_rows$match_method[[3L]] == "fuzzy_name_position_team")
stopifnot(first_rows$match_method[[4L]] == "unmatched")
stopifnot(all(
  availability$availability_probability[
    availability$player_id == "gsis-4"
  ] == 1
))
for (player_id in unique(availability$player_id)) {
  player_curve <- availability$availability_probability[
    availability$player_id == player_id
  ]
  stopifnot(all(diff(player_curve) <= 0))
}

cat("Availability-model tests passed.\n")
