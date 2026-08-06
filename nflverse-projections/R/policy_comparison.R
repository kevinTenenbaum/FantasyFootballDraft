# Paired comparison of future draft-selection policies on shared opponent boards.

summarize_policy_scenarios <- function(rows, downside_probability = 0.10) {
  policies <- unique(rows$policy)
  do.call(rbind, lapply(policies, function(policy) {
    policy_rows <- rows[rows$policy == policy, , drop = FALSE]
    scores <- policy_rows$roster_score
    tail_count <- max(1L, ceiling(length(scores) * downside_probability))
    data.frame(
      policy = policy,
      scenarios = nrow(policy_rows),
      mean_roster_score = mean(scores),
      median_roster_score = stats::median(scores),
      p10_roster_score = as.numeric(stats::quantile(scores, downside_probability)),
      cvar10_roster_score = mean(sort(scores)[seq_len(tail_count)]),
      standard_error = stats::sd(scores) / sqrt(length(scores)),
      mean_raw_points = mean(policy_rows$raw_points),
      mean_rb1_rb2_points = mean(policy_rows$rb1_rb2_points),
      probability_two_viable_rbs = mean(policy_rows$two_viable_rbs),
      mean_rbs_through_round_6 = mean(policy_rows$rbs_through_round_6),
      probability_two_rbs_through_round_6 = mean(
        policy_rows$rbs_through_round_6 >= 2L
      ),
      mean_first_rb_round = mean(policy_rows$first_rb_round),
      mean_second_rb_round = mean(policy_rows$second_rb_round),
      mean_QB = mean(policy_rows$QB),
      mean_RB = mean(policy_rows$RB),
      mean_WR = mean(policy_rows$WR),
      mean_TE = mean(policy_rows$TE),
      stringsAsFactors = FALSE
    )
  }))
}

compare_policy_selections <- function(rosters) {
  policies <- unique(rosters$policy)
  if (length(policies) != 2L) {
    stop("Selection comparison requires exactly two policies.", call. = FALSE)
  }
  first <- rosters[rosters$policy == policies[[1L]], c(
    "scenario", "round", "player_id"
  )]
  second <- rosters[rosters$policy == policies[[2L]], c(
    "scenario", "round", "player_id"
  )]
  paired <- merge(
    first,
    second,
    by = c("scenario", "round"),
    suffixes = c("_first", "_second")
  )
  paired$same_player <- paired$player_id_first == paired$player_id_second
  by_round <- aggregate(same_player ~ round, paired, mean)
  names(by_round)[[2L]] <- "pick_agreement_probability"
  list(overall = mean(paired$same_player), by_round = by_round)
}

run_paired_policy_comparison <- function(
    projections,
    availability,
    candidate_player_id,
    current_round,
    draft_slot,
    drafted_player_ids = character(),
    roster_player_ids = character(),
    simulations = 50L,
    seed = 410001L,
    viable_rb_points = 150,
    cores = 1L,
    config = default_draft_config(),
    progress = NULL) {
  projections <- prepare_draft_projections(projections, config)
  simulations <- as.integer(simulations)
  cores <- as.integer(cores)
  if (simulations < 1L) stop("simulations must be positive.", call. = FALSE)
  if (cores < 1L) stop("cores must be positive.", call. = FALSE)
  if (.Platform$OS.type == "windows") cores <- 1L

  adp_players <- prepare_rollout_adp(availability)
  scenario_seeds <- as.integer(seed) + seq_len(simulations) - 1L
  opponent_orders <- lapply(
    scenario_seeds,
    function(scenario_seed) sample_opponent_order(
      adp_players,
      seed = scenario_seed,
      max_pick = config$team_count * 15L
    )
  )
  policies <- c("candidate_fixing", "largest_fraction")
  tasks <- expand.grid(
    policy = policies,
    scenario = seq_len(simulations),
    stringsAsFactors = FALSE
  )
  if (!is.null(progress)) progress(0L, nrow(tasks))

  worker <- function(task_index) {
    task <- tasks[task_index, ]
    rollout <- run_candidate_scenario(
      candidate_player_id = candidate_player_id,
      opponent_order = opponent_orders[[task$scenario]],
      projections = projections,
      availability = availability,
      current_round = current_round,
      draft_slot = draft_slot,
      drafted_player_ids = drafted_player_ids,
      roster_player_ids = roster_player_ids,
      config = config,
      viable_rb_points = viable_rb_points,
      future_policy = task$policy
    )
    rb_rounds <- sort(rollout$picks$round[rollout$picks$position == "RB"])
    position_counts <- rollout$position_counts
    scenario_row <- data.frame(
      policy = task$policy,
      scenario = task$scenario,
      seed = scenario_seeds[[task$scenario]],
      roster_score = rollout$roster_score,
      starter_score = rollout$starter_score,
      bench_score = rollout$bench_score,
      raw_points = rollout$raw_points,
      rb1_rb2_points = rollout$rb1_rb2_points,
      two_viable_rbs = rollout$two_viable_rbs,
      rbs_through_round_6 = sum(rb_rounds <= 6L),
      first_rb_round = rb_rounds[[1L]],
      second_rb_round = rb_rounds[[2L]],
      QB = unname(position_counts[["QB"]]),
      RB = unname(position_counts[["RB"]]),
      WR = unname(position_counts[["WR"]]),
      TE = unname(position_counts[["TE"]]),
      stringsAsFactors = FALSE
    )
    roster_rows <- rollout$picks
    roster_rows$policy <- task$policy
    roster_rows$scenario <- task$scenario
    roster_rows$seed <- scenario_seeds[[task$scenario]]
    list(scenario = scenario_row, roster = roster_rows)
  }

  task_indexes <- seq_len(nrow(tasks))
  results <- if (cores > 1L) {
    parallel::mclapply(
      task_indexes,
      worker,
      mc.cores = cores,
      mc.preschedule = TRUE
    )
  } else {
    lapply(task_indexes, worker)
  }
  if (!is.null(progress)) progress(nrow(tasks), nrow(tasks))

  scenario_rows <- do.call(rbind, lapply(results, `[[`, "scenario"))
  roster_rows <- do.call(rbind, lapply(results, `[[`, "roster"))
  adp_by_id <- setNames(adp_players$adp, adp_players$player_id)
  roster_rows$public_adp <- adp_by_id[roster_rows$player_id]
  summary <- summarize_policy_scenarios(scenario_rows)
  selection_comparison <- compare_policy_selections(roster_rows)

  first_scores <- scenario_rows[
    scenario_rows$policy == policies[[1L]],
    c("scenario", "roster_score")
  ]
  second_scores <- scenario_rows[
    scenario_rows$policy == policies[[2L]],
    c("scenario", "roster_score")
  ]
  paired <- merge(
    first_scores,
    second_scores,
    by = "scenario",
    suffixes = c("_candidate_fixing", "_largest_fraction")
  )
  paired$score_difference <-
    paired$roster_score_candidate_fixing - paired$roster_score_largest_fraction

  list(
    summary = summary,
    scenarios = scenario_rows,
    rosters = roster_rows,
    paired_scores = paired,
    pick_agreement = selection_comparison$overall,
    pick_agreement_by_round = selection_comparison$by_round
  )
}
