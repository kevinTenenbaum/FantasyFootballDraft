#!/usr/bin/env Rscript

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1L]])))
} else {
  normalizePath(getwd())
}
source(file.path(dirname(test_dir), "R", "draft_optimizer.R"))

stopifnot(identical(
  snake_pick_numbers(3L, 12L, 1:4),
  c(3L, 22L, 27L, 46L)
))

config <- default_draft_config(candidate_count = 3L)
roles <- draft_roster_roles(config)
stopifnot(
  sum(roles$role_type == "starter") == 7L,
  sum(roles$role_type == "bench") == sum(config$position_maximums),
  roles$weight[roles$role_id == "QB_BENCH_1"] == 0.20,
  roles$weight[roles$role_id == "QB_BENCH_2"] == 0.05,
  roles$weight[roles$role_id == "QB_BENCH_3"] == 0.01,
  roles$weight[roles$role_id == "RB_BENCH_1"] == 0.45,
  roles$weight[roles$role_id == "RB_BENCH_2"] == 0.25,
  roles$weight[roles$role_id == "RB_BENCH_3"] == 0.10
)

positions <- rep(c("QB", "RB", "WR", "TE"), each = 10L)
position_index <- ave(seq_along(positions), positions, FUN = seq_along)
projections <- data.frame(
  player_id = paste0(tolower(positions), position_index),
  player_name = paste(positions, position_index),
  team = "FA",
  position = positions,
  projected_fantasy_points = c(
    seq(300, 210, length.out = 10),
    seq(280, 190, length.out = 10),
    seq(270, 180, length.out = 10),
    seq(240, 150, length.out = 10)
  ),
  stringsAsFactors = FALSE
)

picks <- seq_len(168L)
availability <- do.call(rbind, lapply(
  seq_len(nrow(projections)),
  function(player_index) {
    # Better players disappear sooner, while every position retains deep,
    # high-survival fallback options.
    midpoint <- 8 + position_index[[player_index]] * 16
    probability <- stats::plogis((midpoint - picks) / 10)
    data.frame(
      player_id = projections$player_id[[player_index]],
      pick = picks,
      availability_probability = probability,
      stringsAsFactors = FALSE
    )
  }
))

candidate_data <- build_pick_candidate_sets(
  projections = projections,
  availability = availability,
  current_round = 1L,
  draft_slot = 3L,
  config = config
)
stopifnot(
  length(candidate_data$current_candidates) == 12L,
  all(colSums(candidate_data$allowed) >= 4L),
  all(candidate_data$conditional_availability[, 1L] == 1)
)

recommendation <- recommend_draft_pick(candidate_data)
stopifnot(
  nrow(recommendation$rankings) == 12L,
  recommendation$rankings$status[[1L]] == "optimal",
  abs(sum(
    recommendation$selection_plan$fraction[
      recommendation$selection_plan$round == 1L
    ]
  ) - 1) < 1e-6,
  all(abs(
    tapply(
      recommendation$selection_plan$fraction,
      recommendation$selection_plan$round,
      sum
    ) - 1
  ) < 1e-6),
  abs(sum(
    recommendation$role_plan$fraction[
      recommendation$role_plan$role_type == "starter"
    ]
  ) - 7) < 1e-6,
  abs(sum(
    recommendation$role_plan$fraction[
      recommendation$role_plan$role_type == "bench"
    ]
  ) - 7) < 1e-6
)

position_totals <- tapply(
  recommendation$role_plan$fraction,
  recommendation$role_plan$position,
  sum
)
stopifnot(all(
  position_totals <= config$position_maximums[names(position_totals)] + 1e-6
))

# Every selected tail must remain within the conditional availability bound.
selection <- recommendation$selection_plan
for (player_id in unique(selection$player_id)) {
  player_rows <- selection[selection$player_id == player_id, ]
  for (pick_index in seq_along(candidate_data$picks)) {
    tail_mass <- sum(player_rows$fraction[player_rows$pick_index >= pick_index])
    stopifnot(
      tail_mass <=
        candidate_data$conditional_availability[player_id, pick_index] + 1e-6
    )
  }
}

cat("Draft optimizer tests passed.\n")
