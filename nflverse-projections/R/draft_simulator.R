# Greedy draft rollouts on concrete opponent boards. This module deliberately
# stays separate from the fractional optimizer so the experimental simulation
# policy can evolve without changing the production recommendation model.

prepare_rollout_adp <- function(availability) {
  availability <- as.data.frame(availability, stringsAsFactors = FALSE)
  validate_draft_columns(
    availability,
    c("player_id", "pick", "match_method", "adp", "adp_sd"),
    "Availability data"
  )
  rows <- availability[availability$pick == 1L, , drop = FALSE]
  rows$player_id <- as.character(rows$player_id)
  rows$adp <- suppressWarnings(as.numeric(rows$adp))
  rows$adp_sd <- suppressWarnings(as.numeric(rows$adp_sd))
  rows <- rows[
    rows$match_method != "unmatched" &
      is.finite(rows$adp) &
      is.finite(rows$adp_sd),
    ,
    drop = FALSE
  ]
  if (anyDuplicated(rows$player_id)) {
    stop("ADP rows contain duplicate player_id values.", call. = FALSE)
  }
  rows
}

sample_opponent_order <- function(adp_players, seed, max_pick = 180L) {
  if (!nrow(adp_players)) stop("No matched ADP players are available.", call. = FALSE)
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    stop("seed must be one integer.", call. = FALSE)
  }
  set.seed(seed)
  sampled_pick <- stats::rnorm(
    nrow(adp_players),
    mean = adp_players$adp,
    sd = pmax(adp_players$adp_sd, 0.5)
  )
  sampled_pick <- pmin(max_pick, pmax(1, sampled_pick))

  # The small jitter breaks ties after truncation without materially changing
  # the public-ADP draw. Sorting creates one coherent, duplicate-free board.
  sampled_pick <- sampled_pick + stats::runif(nrow(adp_players), 0, 0.001)
  adp_players$player_id[order(sampled_pick)]
}

run_greedy_draft_rollout <- function(
    projections,
    availability,
    draft_slot,
    seed,
    config = default_draft_config()) {
  projections <- prepare_draft_projections(projections, config)
  adp_players <- prepare_rollout_adp(availability)
  opponent_order <- sample_opponent_order(
    adp_players,
    seed = seed,
    max_pick = config$team_count * 15L
  )
  own_picks <- snake_pick_numbers(
    draft_slot,
    config$team_count,
    seq_len(config$offensive_rounds)
  )

  drafted_player_ids <- character()
  roster_player_ids <- character()
  pick_rows <- vector("list", config$offensive_rounds)
  final_role_plan <- NULL

  for (overall_pick in seq_len(max(own_picks))) {
    current_round <- match(overall_pick, own_picks)
    if (!is.na(current_round)) {
      candidate_data <- build_pick_candidate_sets(
        projections = projections,
        availability = availability,
        current_round = current_round,
        draft_slot = draft_slot,
        drafted_player_ids = drafted_player_ids,
        roster_player_ids = roster_player_ids,
        config = config
      )
      recommendation <- recommend_draft_pick(candidate_data)
      selected_id <- recommendation$best_player_id
      selected <- projections[selected_id, , drop = FALSE]
      public_adp <- adp_players$adp[match(selected_id, adp_players$player_id)]
      runner_up_gap <- if (nrow(recommendation$rankings) >= 2L) {
        recommendation$rankings$points_behind_best[[2L]]
      } else {
        NA_real_
      }

      pick_rows[[current_round]] <- data.frame(
        seed = as.integer(seed),
        round = current_round,
        overall_pick = overall_pick,
        player_id = selected_id,
        player_name = selected$player_name[[1L]],
        position = selected$position[[1L]],
        public_adp = public_adp,
        projected_fantasy_points = selected$projected_fantasy_points[[1L]],
        advantage_over_runner_up = runner_up_gap,
        stringsAsFactors = FALSE
      )
      roster_player_ids <- c(roster_player_ids, selected_id)
      drafted_player_ids <- unique(c(drafted_player_ids, selected_id))
      final_role_plan <- recommendation$role_plan
    } else {
      remaining <- opponent_order[!opponent_order %in% drafted_player_ids]
      if (!length(remaining)) {
        stop("The sampled opponent order was exhausted.", call. = FALSE)
      }
      drafted_player_ids <- c(drafted_player_ids, remaining[[1L]])
    }
  }

  picks <- do.call(rbind, pick_rows)
  rownames(picks) <- NULL
  list(
    picks = picks,
    role_plan = final_role_plan,
    opponent_order = opponent_order
  )
}

summarize_greedy_rollout <- function(rollout, simulation) {
  positions <- table(factor(
    rollout$picks$position,
    levels = c("QB", "RB", "WR", "TE")
  ))
  roles <- rollout$role_plan
  data.frame(
    simulation = as.integer(simulation),
    seed = rollout$picks$seed[[1L]],
    QB = unname(positions[["QB"]]),
    RB = unname(positions[["RB"]]),
    WR = unname(positions[["WR"]]),
    TE = unname(positions[["TE"]]),
    raw_projected_points = sum(rollout$picks$projected_fantasy_points),
    weighted_roster_score = sum(roles$weighted_points),
    weighted_starter_score = sum(
      roles$weighted_points[roles$role_type == "starter"]
    ),
    weighted_bench_score = sum(
      roles$weighted_points[roles$role_type == "bench"]
    ),
    stringsAsFactors = FALSE
  )
}

run_greedy_draft_rollouts <- function(
    projections,
    availability,
    draft_slot,
    simulations = 3L,
    seed = 202602L,
    config = default_draft_config()) {
  simulations <- as.integer(simulations)
  seed <- as.integer(seed)
  if (length(simulations) != 1L || is.na(simulations) || simulations < 1L) {
    stop("simulations must be a positive integer.", call. = FALSE)
  }
  seeds <- seed + seq_len(simulations) - 1L
  rollouts <- lapply(
    seeds,
    function(simulation_seed) run_greedy_draft_rollout(
      projections = projections,
      availability = availability,
      draft_slot = draft_slot,
      seed = simulation_seed,
      config = config
    )
  )

  picks <- do.call(rbind, lapply(seq_along(rollouts), function(index) {
    rows <- rollouts[[index]]$picks
    rows$simulation <- index
    rows[c(
      "simulation", "seed", "round", "overall_pick", "player_id",
      "player_name", "position", "public_adp", "projected_fantasy_points",
      "advantage_over_runner_up"
    )]
  }))
  summary <- do.call(rbind, lapply(
    seq_along(rollouts),
    function(index) summarize_greedy_rollout(rollouts[[index]], index)
  ))
  rownames(picks) <- NULL
  rownames(summary) <- NULL
  list(picks = picks, summary = summary, rollouts = rollouts)
}
