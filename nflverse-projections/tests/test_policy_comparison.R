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
source(file.path(dirname(test_dir), "R", "policy_comparison.R"))

rows <- data.frame(
  policy = rep(c("a", "b"), each = 10L),
  roster_score = c(1:10, 2:11) * 100,
  raw_points = 2000,
  rb1_rb2_points = rep(c(350, 400), each = 10L),
  two_viable_rbs = rep(c(TRUE, FALSE), each = 10L),
  rbs_through_round_6 = rep(c(2, 1), each = 10L),
  first_rb_round = rep(c(3, 4), each = 10L),
  second_rb_round = rep(c(5, 7), each = 10L),
  QB = 2,
  RB = 4,
  WR = 5,
  TE = 3
)
summary <- summarize_policy_scenarios(rows)
stopifnot(
  nrow(summary) == 2L,
  summary$mean_roster_score[summary$policy == "a"] == 550,
  summary$cvar10_roster_score[summary$policy == "a"] == 100,
  summary$probability_two_rbs_through_round_6[summary$policy == "a"] == 1
)

rosters <- expand.grid(
  policy = c("a", "b"),
  scenario = 1:2,
  round = 1:3,
  stringsAsFactors = FALSE
)
rosters$player_id <- paste0("p", rosters$round)
rosters$player_id[
  rosters$policy == "b" & rosters$scenario == 2 & rosters$round == 3
] <- "different"
agreement <- compare_policy_selections(rosters)
stopifnot(
  abs(agreement$overall - 5 / 6) < 1e-8,
  agreement$by_round$pick_agreement_probability[
    agreement$by_round$round == 3
  ] == 0.5
)

cat("Policy-comparison tests passed.\n")
