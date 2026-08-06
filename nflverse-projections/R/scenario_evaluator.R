# Shared-scenario counterfactual evaluation for the current draft pick.

choose_fractional_policy_pick <- function(candidate_data) {
  model <- build_fractional_draft_lp(candidate_data)
  solved <- solve_fractional_draft_lp(model)
  if (solved$status != "optimal") {
    stop("The fractional rollout policy was not feasible.", call. = FALSE)
  }
  variables <- model$variables
  current_rows <- which(
    variables$variable_type == "selection" & variables$pick_index == 1L
  )
  if (!length(current_rows)) stop("No current selection variables exist.", call. = FALSE)
  shares <- solved$solution[current_rows]
  projections <- setNames(
    candidate_data$pool$projected_fantasy_points,
    candidate_data$pool$player_id
  )
  ordering <- order(
    -shares,
    -projections[variables$player_id[current_rows]],
    variables$player_id[current_rows]
  )
  selected_row <- current_rows[ordering[[1L]]]
  list(
    player_id = variables$player_id[[selected_row]],
    fractional_share = shares[ordering[[1L]]],
    solution = solved
  )
}

score_concrete_roster <- function(
    roster_player_ids,
    projections,
    config = default_draft_config()) {
  projections <- prepare_draft_projections(projections, config)
  roster_player_ids <- as.character(roster_player_ids)
  if (length(roster_player_ids) != config$offensive_rounds) {
    stop(
      "A completed offensive roster must contain exactly ",
      config$offensive_rounds, " players.",
      call. = FALSE
    )
  }
  if (anyDuplicated(roster_player_ids)) {
    stop("A concrete roster cannot contain duplicate players.", call. = FALSE)
  }
  unknown <- setdiff(roster_player_ids, projections$player_id)
  if (length(unknown)) {
    stop("Roster players are absent from projections: ", paste(unknown, collapse = ", "))
  }
  roster <- projections[roster_player_ids, , drop = FALSE]
  position_counts <- table(factor(roster$position, levels = config$positions))
  if (any(position_counts > config$position_maximums[names(position_counts)])) {
    stop("The completed roster exceeds a position maximum.", call. = FALSE)
  }

  roles <- draft_roster_roles(config)
  variables <- do.call(rbind, lapply(seq_len(nrow(roster)), function(player_index) {
    eligible <- vapply(
      roles$position_group,
      function(group) role_accepts_position(roster$position[[player_index]], group),
      logical(1)
    )
    data.frame(
      player_id = roster$player_id[[player_index]],
      role_id = roles$role_id[eligible],
      stringsAsFactors = FALSE
    )
  }))
  variables$variable_index <- seq_len(nrow(variables))
  projection_by_id <- setNames(
    roster$projected_fantasy_points,
    roster$player_id
  )
  weight_by_role <- setNames(roles$weight, roles$role_id)
  objective <-
    projection_by_id[variables$player_id] * weight_by_role[variables$role_id]

  constraints <- list()
  for (player_id in roster$player_id) {
    indexes <- variables$variable_index[variables$player_id == player_id]
    constraints <- append_lp_constraint(constraints, indexes, 1, "=", 1)
  }
  for (role_index in seq_len(nrow(roles))) {
    indexes <- variables$variable_index[
      variables$role_id == roles$role_id[[role_index]]
    ]
    direction <- if (roles$role_type[[role_index]] == "starter") "=" else "<="
    constraints <- append_lp_constraint(constraints, indexes, 1, direction, 1)
  }
  bench_ids <- roles$role_id[roles$role_type == "bench"]
  constraints <- append_lp_constraint(
    constraints,
    variables$variable_index[variables$role_id %in% bench_ids],
    1,
    "=",
    config$bench_slots
  )

  constraint_matrix <- matrix(
    0,
    nrow = length(constraints),
    ncol = nrow(variables)
  )
  directions <- character(length(constraints))
  right_hand_sides <- numeric(length(constraints))
  for (constraint_index in seq_along(constraints)) {
    constraint <- constraints[[constraint_index]]
    if (length(constraint$indexes)) {
      constraint_matrix[constraint_index, constraint$indexes] <- constraint$values
    }
    directions[[constraint_index]] <- constraint$direction
    right_hand_sides[[constraint_index]] <- constraint$rhs
  }
  solution <- lpSolve::lp(
    direction = "max",
    objective.in = objective,
    const.mat = constraint_matrix,
    const.dir = directions,
    const.rhs = right_hand_sides,
    all.int = FALSE
  )
  if (solution$status != 0L) {
    stop("The completed roster cannot fill the configured roles.", call. = FALSE)
  }

  positive <- which(solution$solution > 1e-7)
  assignments <- variables[positive, c("player_id", "role_id"), drop = FALSE]
  assignments$fraction <- solution$solution[positive]
  assignments <- merge(
    assignments,
    roster[c(
      "player_id", "player_name", "team", "position",
      "projected_fantasy_points"
    )],
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  assignments <- merge(
    assignments,
    roles[c("role_id", "role_type", "depth", "weight")],
    by = "role_id",
    all.x = TRUE,
    sort = FALSE
  )
  assignments$weighted_points <-
    assignments$fraction * assignments$projected_fantasy_points * assignments$weight
  list(score = solution$objval, assignments = assignments)
}

run_candidate_scenario <- function(
    candidate_player_id,
    opponent_order,
    projections,
    availability,
    current_round,
    draft_slot,
    drafted_player_ids = character(),
    roster_player_ids = character(),
    config = default_draft_config(),
    viable_rb_points = 150,
    future_policy = c("largest_fraction", "candidate_fixing")) {
  future_policy <- match.arg(future_policy)
  projections <- prepare_draft_projections(projections, config)
  candidate_player_id <- as.character(candidate_player_id)
  if (!candidate_player_id %in% projections$player_id) {
    stop("Candidate is absent from projections: ", candidate_player_id, call. = FALSE)
  }
  if (candidate_player_id %in% union(drafted_player_ids, roster_player_ids)) {
    stop("Candidate is not available on the current board.", call. = FALSE)
  }

  rounds <- seq.int(current_round, config$offensive_rounds)
  own_picks <- snake_pick_numbers(draft_slot, config$team_count, rounds)
  roster <- c(as.character(roster_player_ids), candidate_player_id)
  drafted <- unique(c(as.character(drafted_player_ids), roster))
  pick_rows <- list(data.frame(
    round = current_round,
    overall_pick = own_picks[[1L]],
    player_id = candidate_player_id,
    policy_share = 1,
    stringsAsFactors = FALSE
  ))

  if (length(own_picks) > 1L) {
    next_own_index <- 2L
    for (overall_pick in seq.int(own_picks[[1L]] + 1L, max(own_picks))) {
      if (overall_pick == own_picks[[next_own_index]]) {
        round <- rounds[[next_own_index]]
        candidate_data <- build_pick_candidate_sets(
          projections = projections,
          availability = availability,
          current_round = round,
          draft_slot = draft_slot,
          drafted_player_ids = drafted,
          roster_player_ids = roster,
          config = config
        )
        selected <- if (future_policy == "candidate_fixing") {
          recommendation <- recommend_draft_pick(candidate_data)
          list(
            player_id = recommendation$best_player_id,
            fractional_share = 1
          )
        } else {
          choose_fractional_policy_pick(candidate_data)
        }
        roster <- c(roster, selected$player_id)
        drafted <- unique(c(drafted, selected$player_id))
        pick_rows[[length(pick_rows) + 1L]] <- data.frame(
          round = round,
          overall_pick = overall_pick,
          player_id = selected$player_id,
          policy_share = selected$fractional_share,
          stringsAsFactors = FALSE
        )
        next_own_index <- next_own_index + 1L
        if (next_own_index > length(own_picks)) break
      } else {
        remaining <- opponent_order[!opponent_order %in% drafted]
        if (!length(remaining)) {
          stop("The sampled opponent order was exhausted.", call. = FALSE)
        }
        drafted <- c(drafted, remaining[[1L]])
      }
    }
  }

  picks <- do.call(rbind, pick_rows)
  pick_details <- projections[
    match(picks$player_id, projections$player_id),
    c(
      "player_id", "player_name", "team", "position",
      "projected_fantasy_points"
    ),
    drop = FALSE
  ]
  picks <- cbind(
    picks[c("round", "overall_pick", "policy_share")],
    pick_details
  )
  roster_score <- score_concrete_roster(roster, projections, config)
  rb_points <- sort(
    projections$projected_fantasy_points[
      match(roster, projections$player_id)[
        projections$position[match(roster, projections$player_id)] == "RB"
      ]
    ],
    decreasing = TRUE
  )
  position_counts <- table(factor(
    projections$position[match(roster, projections$player_id)],
    levels = config$positions
  ))
  list(
    picks = picks,
    roster_player_ids = roster,
    roster_score = roster_score$score,
    starter_score = sum(
      roster_score$assignments$weighted_points[
        roster_score$assignments$role_type == "starter"
      ]
    ),
    bench_score = sum(
      roster_score$assignments$weighted_points[
        roster_score$assignments$role_type == "bench"
      ]
    ),
    raw_points = sum(picks$projected_fantasy_points),
    rb1_rb2_points = if (length(rb_points) >= 2L) sum(rb_points[1:2]) else 0,
    two_viable_rbs = sum(rb_points >= viable_rb_points) >= 2L,
    position_counts = position_counts
  )
}

candidate_scenario_summary <- function(
    scenario_results,
    candidate_player_id,
    projections,
    downside_probability = 0.10,
    downside_weight = 0.20) {
  rows <- scenario_results[
    scenario_results$candidate_player_id == candidate_player_id,
    ,
    drop = FALSE
  ]
  scores <- rows$roster_score
  tail_count <- max(1L, ceiling(length(scores) * downside_probability))
  cvar <- mean(sort(scores)[seq_len(tail_count)])
  projection_row <- projections[projections$player_id == candidate_player_id, ]
  data.frame(
    candidate_player_id = candidate_player_id,
    player_name = projection_row$player_name[[1L]],
    position = projection_row$position[[1L]],
    projected_fantasy_points = projection_row$projected_fantasy_points[[1L]],
    scenario_count = nrow(rows),
    mean_roster_score = mean(scores),
    median_roster_score = stats::median(scores),
    p10_roster_score = as.numeric(stats::quantile(scores, downside_probability)),
    cvar10_roster_score = cvar,
    standard_error = stats::sd(scores) / sqrt(length(scores)),
    risk_adjusted_score =
      (1 - downside_weight) * mean(scores) + downside_weight * cvar,
    mean_rb1_rb2_points = mean(rows$rb1_rb2_points),
    probability_two_viable_rbs = mean(rows$two_viable_rbs),
    mean_QB = mean(rows$QB),
    mean_RB = mean(rows$RB),
    mean_WR = mean(rows$WR),
    mean_TE = mean(rows$TE),
    stringsAsFactors = FALSE
  )
}

evaluate_candidates_in_shared_scenarios <- function(
    projections,
    availability,
    current_round,
    draft_slot,
    drafted_player_ids = character(),
    roster_player_ids = character(),
    screen_scenarios = 20L,
    final_scenarios = 500L,
    finalists = 5L,
    seed = 310001L,
    viable_rb_points = 150,
    downside_probability = 0.10,
    downside_weight = 0.20,
    cores = 1L,
    config = default_draft_config(),
    progress = NULL) {
  projections <- prepare_draft_projections(projections, config)
  screen_scenarios <- as.integer(screen_scenarios)
  final_scenarios <- as.integer(final_scenarios)
  finalists <- as.integer(finalists)
  cores <- as.integer(cores)
  if (screen_scenarios < 1L || final_scenarios < screen_scenarios) {
    stop("Scenario counts must satisfy 1 <= screen <= final.", call. = FALSE)
  }
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    stop("cores must be a positive integer.", call. = FALSE)
  }
  if (.Platform$OS.type == "windows") cores <- 1L

  current_data <- build_pick_candidate_sets(
    projections = projections,
    availability = availability,
    current_round = current_round,
    draft_slot = draft_slot,
    drafted_player_ids = drafted_player_ids,
    roster_player_ids = roster_player_ids,
    config = config
  )
  candidates <- current_data$current_candidates
  finalists <- min(finalists, length(candidates))
  adp_players <- prepare_rollout_adp(availability)
  scenario_seeds <- as.integer(seed) + seq_len(final_scenarios) - 1L
  opponent_orders <- lapply(
    scenario_seeds,
    function(scenario_seed) sample_opponent_order(
      adp_players,
      seed = scenario_seed,
      max_pick = config$team_count * 15L
    )
  )

  run_one <- function(candidate_player_id, scenario_index) {
    rollout <- run_candidate_scenario(
      candidate_player_id = candidate_player_id,
      opponent_order = opponent_orders[[scenario_index]],
      projections = projections,
      availability = availability,
      current_round = current_round,
      draft_slot = draft_slot,
      drafted_player_ids = drafted_player_ids,
      roster_player_ids = roster_player_ids,
      config = config,
      viable_rb_points = viable_rb_points
    )
    position_counts <- rollout$position_counts
    scenario_row <- data.frame(
      candidate_player_id = candidate_player_id,
      scenario = scenario_index,
      seed = scenario_seeds[[scenario_index]],
      roster_score = rollout$roster_score,
      starter_score = rollout$starter_score,
      bench_score = rollout$bench_score,
      raw_points = rollout$raw_points,
      rb1_rb2_points = rollout$rb1_rb2_points,
      two_viable_rbs = rollout$two_viable_rbs,
      QB = unname(position_counts[["QB"]]),
      RB = unname(position_counts[["RB"]]),
      WR = unname(position_counts[["WR"]]),
      TE = unname(position_counts[["TE"]]),
      stringsAsFactors = FALSE
    )
    picks <- rollout$picks
    picks$candidate_player_id <- candidate_player_id
    picks$scenario <- scenario_index
    picks$seed <- scenario_seeds[[scenario_index]]
    roster_row <- picks[c(
      "candidate_player_id", "scenario", "seed", "round", "overall_pick",
      "player_id", "player_name", "team", "position",
      "projected_fantasy_points", "policy_share"
    )]
    list(scenario = scenario_row, roster = roster_row)
  }

  run_tasks <- function(candidate_ids, scenario_indexes, stage) {
    tasks <- expand.grid(
      candidate_player_id = candidate_ids,
      scenario_index = scenario_indexes,
      stringsAsFactors = FALSE
    )
    if (!is.null(progress)) progress(stage, 0L, nrow(tasks))
    task_indexes <- seq_len(nrow(tasks))
    worker <- function(task_index) {
      run_one(
        tasks$candidate_player_id[[task_index]],
        tasks$scenario_index[[task_index]]
      )
    }
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
    if (!is.null(progress)) progress(stage, nrow(tasks), nrow(tasks))
    list(
      scenarios = do.call(rbind, lapply(results, `[[`, "scenario")),
      rosters = do.call(rbind, lapply(results, `[[`, "roster"))
    )
  }

  screen <- run_tasks(candidates, seq_len(screen_scenarios), "screen")
  screen_results <- screen$scenarios
  screen_summary <- do.call(rbind, lapply(
    candidates,
    function(candidate_player_id) candidate_scenario_summary(
      screen_results,
      candidate_player_id,
      projections,
      downside_probability,
      downside_weight
    )
  ))
  finalist_ids <- screen_summary$candidate_player_id[
    order(-screen_summary$risk_adjusted_score)
  ][seq_len(finalists)]

  final <- NULL
  if (final_scenarios > screen_scenarios) {
    extra_indexes <- seq.int(screen_scenarios + 1L, final_scenarios)
    final <- run_tasks(finalist_ids, extra_indexes, "final")
  }

  scenario_results <- if (is.null(final)) {
    screen$scenarios
  } else {
    rbind(screen$scenarios, final$scenarios)
  }
  roster_results <- if (is.null(final)) {
    screen$rosters
  } else {
    rbind(screen$rosters, final$rosters)
  }
  final_summary <- do.call(rbind, lapply(
    candidates,
    function(candidate_player_id) candidate_scenario_summary(
      scenario_results,
      candidate_player_id,
      projections,
      downside_probability,
      downside_weight
    )
  ))
  final_summary$is_finalist <- final_summary$candidate_player_id %in% finalist_ids
  final_summary <- final_summary[order(
    -final_summary$is_finalist,
    -final_summary$risk_adjusted_score
  ), ]
  final_summary$rank <- seq_len(nrow(final_summary))
  final_summary <- final_summary[c(
    "rank", "is_finalist", "candidate_player_id", "player_name", "position",
    "projected_fantasy_points", "scenario_count", "mean_roster_score",
    "median_roster_score", "p10_roster_score", "cvar10_roster_score",
    "standard_error", "risk_adjusted_score", "mean_rb1_rb2_points",
    "probability_two_viable_rbs", "mean_QB", "mean_RB", "mean_WR", "mean_TE"
  )]
  list(
    rankings = final_summary,
    scenarios = scenario_results,
    rosters = roster_results,
    finalists = finalist_ids,
    screen_summary = screen_summary
  )
}
