#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "draft_optimizer.R"))
source(file.path(dirname(test_dir), "R", "draft_simulator.R"))
source(file.path(dirname(test_dir), "R", "scenario_evaluator.R"))

positions <- c("QB", rep("RB", 4L), rep("WR", 6L), rep("TE", 3L))
projections <- data.frame(
  player_id = paste0("p", seq_along(positions)),
  player_name = paste("Player", seq_along(positions)),
  team = "FA",
  position = positions,
  projected_fantasy_points = seq(280, 150, length.out = length(positions)),
  stringsAsFactors = FALSE
)
scored <- score_concrete_roster(
  projections$player_id,
  projections,
  default_draft_config()
)
stopifnot(
  is.finite(scored$score),
  abs(sum(scored$assignments$fraction) - 14) < 1e-6,
  abs(sum(
    scored$assignments$fraction[
      scored$assignments$role_type == "starter"
    ]
  ) - 7) < 1e-6
)

scenario_results <- data.frame(
  candidate_player_id = rep("p1", 10L),
  roster_score = 1:10 * 100,
  rb1_rb2_points = rep(350, 10L),
  two_viable_rbs = c(rep(TRUE, 8L), FALSE, FALSE),
  QB = 2,
  RB = 4,
  WR = 5,
  TE = 3
)
summary <- candidate_scenario_summary(
  scenario_results,
  "p1",
  projections,
  downside_probability = 0.10,
  downside_weight = 0.20
)
stopifnot(
  summary$scenario_count == 10L,
  summary$mean_roster_score == 550,
  summary$cvar10_roster_score == 100,
  summary$risk_adjusted_score == 460,
  summary$probability_two_viable_rbs == 0.8
)

cat("Scenario evaluator tests passed.\n")
