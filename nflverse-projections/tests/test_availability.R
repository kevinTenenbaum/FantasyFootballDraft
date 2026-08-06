#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "availability.R"))

curve <- lower_bounded_draft_survival(adp = 5, adp_sd = 2, max_pick = 10L)
long_curve <- lower_bounded_draft_survival(adp = 5, adp_sd = 2, max_pick = 20L)
stopifnot(length(curve) == 10L)
stopifnot(curve[[1L]] == 1)
stopifnot(all(diff(curve) <= 0))
stopifnot(all(curve >= 0 & curve <= 1))
stopifnot(isTRUE(all.equal(curve, long_curve[seq_along(curve)])))
stopifnot(curve[[10L]] > 0)

point_curve <- lower_bounded_draft_survival(adp = 12, adp_sd = 0, max_pick = 10L)
stopifnot(identical(point_curve, rep(1, 10L)))

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
  player_id = 101:112,
  name = c(
    "DJ Example", "Traded Player", "Kenny Example", "Quarterback One",
    "Quarterback Two", "Receiver Two", "Receiver Three", "Tight End One",
    "Tight End Two", "Example Defense", "Example Kicker", "Runner Three"
  ),
  position = c("WR", "RB", "RB", "QB", "QB", "WR", "WR", "TE", "TE", "DEF", "PK", "RB"),
  team = c("LAR", "MIA", "TB", "BUF", "KC", "PHI", "DAL", "SF", "DET", "DEN", "BAL", "CHI"),
  adp = seq(1.5, 8.5, length.out = 12),
  times_drafted = rep(100, 12),
  high = 1:12,
  low = 4:15,
  stdev = rep(2, 12),
  stringsAsFactors = FALSE
)

prepared_adp <- prepare_public_adp(public_adp)
calibration <- calibrate_public_availability(prepared_adp, max_pick = 10L)
expected_drafted <- colSums(1 - calibration$curves)
stopifnot(max(abs(expected_drafted - 0:9)) < 1e-5)
stopifnot(all(apply(calibration$curves, 1L, function(player_curve) {
  all(diff(player_curve) <= 1e-8)
})))
rb_share <- sum(prepared_adp$times_drafted[prepared_adp$position == "RB"]) /
  sum(prepared_adp$times_drafted)
rb_drafted_before_pick_10 <- sum(
  1 - calibration$curves[prepared_adp$position == "RB", 10L]
)
stopifnot(abs(rb_drafted_before_pick_10 - 9 * rb_share) < 0.75)

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
