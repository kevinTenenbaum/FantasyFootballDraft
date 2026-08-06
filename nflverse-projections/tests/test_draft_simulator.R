#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "draft_optimizer.R"))
source(file.path(dirname(test_dir), "R", "draft_simulator.R"))

availability <- data.frame(
  player_id = rep(paste0("p", 1:8), each = 2L),
  pick = rep(1:2, times = 8L),
  match_method = rep(c(
    "name_position", "name_position", "name_position", "name_position",
    "name_position", "name_position", "unmatched", "unmatched"
  ), each = 2L),
  adp = rep(c(1, 2, 3, 4, 5, 6, NA, NA), each = 2L),
  adp_sd = rep(c(1, 1, 2, 2, 3, 3, NA, NA), each = 2L),
  stringsAsFactors = FALSE
)
adp <- prepare_rollout_adp(availability)
first <- sample_opponent_order(adp, seed = 42L)
second <- sample_opponent_order(adp, seed = 42L)
stopifnot(
  nrow(adp) == 6L,
  identical(first, second),
  length(first) == 6L,
  !anyDuplicated(first),
  all(first %in% paste0("p", 1:6))
)

fake_rollout <- list(
  picks = data.frame(
    seed = rep(42L, 14L),
    position = c("QB", rep("RB", 4L), rep("WR", 6L), rep("TE", 3L)),
    projected_fantasy_points = rep(100, 14L)
  ),
  role_plan = data.frame(
    role_type = c(rep("starter", 7L), rep("bench", 7L)),
    weighted_points = c(rep(100, 7L), rep(20, 7L))
  )
)
summary <- summarize_greedy_rollout(fake_rollout, simulation = 1L)
stopifnot(
  summary$QB == 1L,
  summary$RB == 4L,
  summary$WR == 6L,
  summary$TE == 3L,
  summary$raw_projected_points == 1400,
  summary$weighted_roster_score == 840
)

cat("Draft simulator tests passed.\n")
