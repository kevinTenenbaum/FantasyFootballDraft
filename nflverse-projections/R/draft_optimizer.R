# Fractional draft optimization using player projections and conditional
# per-pick availability probabilities.

validate_draft_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      label, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

default_draft_config <- function(
    team_count = 12L,
    offensive_rounds = 14L,
    candidate_count = 5L,
    minimum_future_availability = 0.02,
    bench_weights = c(0.45, 0.25, 0.10),
    qb_bench_weights = c(0.20, 0.05, 0.01)) {
  team_count <- as.integer(team_count)
  offensive_rounds <- as.integer(offensive_rounds)
  candidate_count <- as.integer(candidate_count)
  minimum_future_availability <- as.numeric(minimum_future_availability)
  bench_weights <- as.numeric(bench_weights)
  qb_bench_weights <- as.numeric(qb_bench_weights)

  if (team_count < 2L) stop("team_count must be at least 2.", call. = FALSE)
  if (offensive_rounds != 14L) {
    stop(
      "This league configuration requires 14 offensive rounds before K and D/ST.",
      call. = FALSE
    )
  }
  if (candidate_count < 3L || candidate_count > 5L) {
    stop("candidate_count must be between 3 and 5.", call. = FALSE)
  }
  if (
    length(minimum_future_availability) != 1L ||
      !is.finite(minimum_future_availability) ||
      minimum_future_availability < 0 ||
      minimum_future_availability > 1
  ) {
    stop(
      "minimum_future_availability must be between 0 and 1.",
      call. = FALSE
    )
  }
  if (!length(bench_weights) || any(!is.finite(bench_weights))) {
    stop("bench_weights must contain finite values.", call. = FALSE)
  }
  if (any(bench_weights < 0 | bench_weights > 1)) {
    stop("bench_weights must be between 0 and 1.", call. = FALSE)
  }
  if (!length(qb_bench_weights) || any(!is.finite(qb_bench_weights))) {
    stop("qb_bench_weights must contain finite values.", call. = FALSE)
  }
  if (any(qb_bench_weights < 0 | qb_bench_weights > 1)) {
    stop("qb_bench_weights must be between 0 and 1.", call. = FALSE)
  }

  list(
    team_count = team_count,
    offensive_rounds = offensive_rounds,
    candidate_count = candidate_count,
    minimum_future_availability = minimum_future_availability,
    bench_slots = 7L,
    bench_weights = bench_weights,
    qb_bench_weights = qb_bench_weights,
    positions = c("QB", "RB", "WR", "TE"),
    position_maximums = c(QB = 4, RB = 8, WR = 8, TE = 4)
  )
}

draft_roster_roles <- function(config = default_draft_config()) {
  starter_roles <- data.frame(
    role_id = c(
      "QB_START",
      "RB_START_1", "RB_START_2",
      "WRT_START_1", "WRT_START_2", "WRT_START_3",
      "FLEX_START"
    ),
    role_type = "starter",
    position_group = c("QB", "RB", "RB", "WR_TE", "WR_TE", "WR_TE", "FLEX"),
    depth = 1L,
    weight = 1,
    stringsAsFactors = FALSE
  )

  bench_roles <- do.call(rbind, lapply(
    config$positions,
    function(position) {
      maximum <- unname(config$position_maximums[[position]])
      depth <- seq_len(maximum)
      position_weights <- if (position == "QB") {
        config$qb_bench_weights
      } else {
        config$bench_weights
      }
      weight_index <- pmin(depth, length(position_weights))
      data.frame(
        role_id = paste0(position, "_BENCH_", depth),
        role_type = "bench",
        position_group = position,
        depth = depth,
        weight = position_weights[weight_index],
        stringsAsFactors = FALSE
      )
    }
  ))

  output <- rbind(starter_roles, bench_roles)
  rownames(output) <- NULL
  output
}

role_accepts_position <- function(position, position_group) {
  position <- toupper(as.character(position))
  switch(
    position_group,
    QB = position == "QB",
    RB = position == "RB",
    WR = position == "WR",
    TE = position == "TE",
    WR_TE = position %in% c("WR", "TE"),
    FLEX = position %in% c("RB", "WR", "TE"),
    FALSE
  )
}

snake_pick_numbers <- function(
    draft_slot,
    team_count = 12L,
    rounds = seq_len(14L)) {
  draft_slot <- as.integer(draft_slot)
  team_count <- as.integer(team_count)
  rounds <- as.integer(rounds)
  if (
    length(draft_slot) != 1L || is.na(draft_slot) ||
      draft_slot < 1L || draft_slot > team_count
  ) {
    stop("draft_slot must be between 1 and team_count.", call. = FALSE)
  }
  if (!length(rounds) || any(is.na(rounds)) || any(rounds < 1L)) {
    stop("rounds must contain positive integers.", call. = FALSE)
  }

  offset <- ifelse(rounds %% 2L == 1L, draft_slot, team_count - draft_slot + 1L)
  as.integer((rounds - 1L) * team_count + offset)
}

prepare_draft_projections <- function(projections, config) {
  projections <- as.data.frame(projections, stringsAsFactors = FALSE)
  validate_draft_columns(
    projections,
    c(
      "player_id", "player_name", "team", "position",
      "projected_fantasy_points"
    ),
    "Projection data"
  )
  projections$player_id <- as.character(projections$player_id)
  projections$position <- toupper(as.character(projections$position))
  projections$projected_fantasy_points <- suppressWarnings(as.numeric(
    projections$projected_fantasy_points
  ))
  projections <- projections[
    nzchar(projections$player_id) &
      projections$position %in% config$positions &
      is.finite(projections$projected_fantasy_points),
    ,
    drop = FALSE
  ]
  if (anyDuplicated(projections$player_id)) {
    duplicated_ids <- unique(projections$player_id[duplicated(projections$player_id)])
    stop(
      "Projection data has duplicate player_id values: ",
      paste(utils::head(duplicated_ids, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  rownames(projections) <- projections$player_id
  projections
}

conditional_availability_matrix <- function(
    availability,
    player_ids,
    current_pick,
    future_picks) {
  availability <- as.data.frame(availability, stringsAsFactors = FALSE)
  validate_draft_columns(
    availability,
    c("player_id", "pick", "availability_probability"),
    "Availability data"
  )
  availability$player_id <- as.character(availability$player_id)
  availability$pick <- suppressWarnings(as.integer(availability$pick))
  availability$availability_probability <- suppressWarnings(as.numeric(
    availability$availability_probability
  ))
  target_picks <- unique(c(as.integer(current_pick), as.integer(future_picks)))
  rows <- availability[
    availability$player_id %in% player_ids & availability$pick %in% target_picks,
    c("player_id", "pick", "availability_probability"),
    drop = FALSE
  ]
  if (anyDuplicated(rows[c("player_id", "pick")])) {
    stop(
      "Availability data has duplicate player_id/pick combinations.",
      call. = FALSE
    )
  }

  raw <- matrix(
    NA_real_,
    nrow = length(player_ids),
    ncol = length(target_picks),
    dimnames = list(player_ids, as.character(target_picks))
  )
  if (nrow(rows)) {
    row_index <- match(rows$player_id, player_ids)
    column_index <- match(rows$pick, target_picks)
    raw[cbind(row_index, column_index)] <- rows$availability_probability
  }
  if (anyNA(raw)) {
    missing_index <- which(is.na(raw), arr.ind = TRUE)[1L, ]
    stop(
      "Availability data is missing player ", rownames(raw)[missing_index[[1L]]],
      " at pick ", colnames(raw)[missing_index[[2L]]], ".",
      call. = FALSE
    )
  }
  if (any(!is.finite(raw)) || any(raw < 0 | raw > 1)) {
    stop("Availability probabilities must be finite values from 0 to 1.", call. = FALSE)
  }

  current <- raw[, as.character(current_pick)]
  output <- matrix(
    0,
    nrow = length(player_ids),
    ncol = length(future_picks),
    dimnames = list(player_ids, as.character(future_picks))
  )
  for (column_index in seq_along(future_picks)) {
    pick <- future_picks[[column_index]]
    if (pick == current_pick) {
      output[, column_index] <- 1
    } else {
      future <- raw[, as.character(pick)]
      positive_current <- current > 0
      output[positive_current, column_index] <- pmin(
        1,
        pmax(0, future[positive_current] / current[positive_current])
      )
    }
  }
  output
}

top_projected_ids <- function(players, count) {
  if (!nrow(players)) return(character())
  order_index <- order(
    -players$projected_fantasy_points,
    players$player_name,
    players$player_id
  )
  utils::head(players$player_id[order_index], count)
}

build_pick_candidate_sets <- function(
    projections,
    availability,
    current_round,
    draft_slot,
    drafted_player_ids = character(),
    roster_player_ids = character(),
    config = default_draft_config()) {
  projections <- prepare_draft_projections(projections, config)
  current_round <- as.integer(current_round)
  if (
    length(current_round) != 1L || is.na(current_round) ||
      current_round < 1L || current_round > config$offensive_rounds
  ) {
    stop(
      "current_round must be between 1 and ", config$offensive_rounds, ".",
      call. = FALSE
    )
  }

  drafted_player_ids <- unique(as.character(drafted_player_ids))
  roster_player_ids <- unique(as.character(roster_player_ids))
  unknown_roster <- setdiff(roster_player_ids, projections$player_id)
  if (length(unknown_roster)) {
    stop(
      "Roster player_id values are absent from the projection data: ",
      paste(unknown_roster, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(roster_player_ids) != current_round - 1L) {
    stop(
      "The roster must contain exactly one prior offensive selection per round: ",
      current_round - 1L, " player(s) before round ", current_round, ".",
      call. = FALSE
    )
  }

  unavailable_ids <- union(drafted_player_ids, roster_player_ids)
  available <- projections[!projections$player_id %in% unavailable_ids, , drop = FALSE]
  if (!nrow(available)) stop("No projected players remain available.", call. = FALSE)

  rounds <- seq.int(current_round, config$offensive_rounds)
  picks <- snake_pick_numbers(draft_slot, config$team_count, rounds)
  conditional <- conditional_availability_matrix(
    availability = availability,
    player_ids = available$player_id,
    current_pick = picks[[1L]],
    future_picks = picks
  )

  candidate_sets <- vector("list", length(picks))
  names(candidate_sets) <- as.character(picks)
  for (pick_index in seq_along(picks)) {
    pick_candidates <- character()
    for (position in config$positions) {
      position_players <- available[available$position == position, , drop = FALSE]
      if (pick_index == 1L) {
        position_ids <- top_projected_ids(position_players, config$candidate_count)
      } else {
        position_probability <- conditional[position_players$player_id, pick_index]
        likely <- position_players[
          position_probability >= config$minimum_future_availability,
          ,
          drop = FALSE
        ]
        position_ids <- top_projected_ids(likely, config$candidate_count)

        # Keep one high-survival fallback at every position and future pick so
        # pruning does not make an otherwise valid roster infeasible.
        fallback_order <- order(
          -position_probability,
          -position_players$projected_fantasy_points,
          position_players$player_name
        )
        if (length(fallback_order)) {
          position_ids <- unique(c(
            position_ids,
            position_players$player_id[fallback_order[[1L]]]
          ))
        }
      }
      pick_candidates <- c(pick_candidates, position_ids)
    }
    candidate_sets[[pick_index]] <- unique(pick_candidates)
  }

  pool_ids <- unique(c(unlist(candidate_sets, use.names = FALSE), roster_player_ids))
  pool <- projections[match(pool_ids, projections$player_id), , drop = FALSE]
  selectable_ids <- setdiff(pool_ids, roster_player_ids)
  allowed <- matrix(
    FALSE,
    nrow = length(selectable_ids),
    ncol = length(picks),
    dimnames = list(selectable_ids, as.character(picks))
  )
  for (pick_index in seq_along(picks)) {
    allowed[, pick_index] <- selectable_ids %in% candidate_sets[[pick_index]]
  }

  conditional_pool <- conditional[selectable_ids, , drop = FALSE]
  list(
    projections = projections,
    pool = pool,
    rounds = rounds,
    picks = picks,
    candidate_sets = candidate_sets,
    current_candidates = candidate_sets[[1L]],
    allowed = allowed,
    conditional_availability = conditional_pool,
    roster_player_ids = roster_player_ids,
    config = config
  )
}

append_lp_constraint <- function(constraints, indexes, values, direction, rhs) {
  constraints[[length(constraints) + 1L]] <- list(
    indexes = as.integer(indexes),
    values = as.numeric(values),
    direction = direction,
    rhs = as.numeric(rhs)
  )
  constraints
}

build_fractional_draft_lp <- function(candidate_data) {
  pool <- candidate_data$pool
  allowed <- candidate_data$allowed
  conditional <- candidate_data$conditional_availability
  config <- candidate_data$config
  roles <- draft_roster_roles(config)
  owned <- pool$player_id %in% candidate_data$roster_player_ids

  x_rows <- which(allowed, arr.ind = TRUE)
  x_variables <- data.frame(
    variable_type = "selection",
    player_id = rownames(allowed)[x_rows[, "row"]],
    pick_index = x_rows[, "col"],
    role_id = NA_character_,
    stringsAsFactors = FALSE
  )

  y_parts <- vector("list", nrow(pool))
  for (player_index in seq_len(nrow(pool))) {
    eligible <- vapply(
      roles$position_group,
      function(group) role_accepts_position(pool$position[[player_index]], group),
      logical(1)
    )
    y_parts[[player_index]] <- data.frame(
      variable_type = "role",
      player_id = pool$player_id[[player_index]],
      pick_index = NA_integer_,
      role_id = roles$role_id[eligible],
      stringsAsFactors = FALSE
    )
  }
  y_variables <- do.call(rbind, y_parts)
  variables <- rbind(x_variables, y_variables)
  variables$variable_index <- seq_len(nrow(variables))

  projection_by_id <- setNames(pool$projected_fantasy_points, pool$player_id)
  weight_by_role <- setNames(roles$weight, roles$role_id)
  objective <- rep(0, nrow(variables))
  y_index <- variables$variable_type == "role"
  objective[y_index] <-
    projection_by_id[variables$player_id[y_index]] *
    weight_by_role[variables$role_id[y_index]]

  constraints <- list()
  x_variable_rows <- which(variables$variable_type == "selection")
  y_variable_rows <- which(variables$variable_type == "role")

  # Exactly one fractional selection at every remaining offensive pick.
  for (pick_index in seq_along(candidate_data$picks)) {
    indexes <- x_variable_rows[
      variables$pick_index[x_variable_rows] == pick_index
    ]
    constraints <- append_lp_constraint(constraints, indexes, 1, "=", 1)
  }

  # A player can be selected at most once. Nested tail constraints ensure that
  # all selection mass at or after a pick fits inside survival to that pick.
  selectable_ids <- rownames(allowed)
  for (player_id in selectable_ids) {
    player_x <- x_variable_rows[variables$player_id[x_variable_rows] == player_id]
    constraints <- append_lp_constraint(constraints, player_x, 1, "<=", 1)
    for (pick_index in seq_along(candidate_data$picks)) {
      tail_indexes <- player_x[
        variables$pick_index[player_x] >= pick_index
      ]
      if (length(tail_indexes)) {
        constraints <- append_lp_constraint(
          constraints,
          tail_indexes,
          1,
          "<=",
          conditional[player_id, pick_index]
        )
      }
    }
  }

  # Every selected or already-owned player share must be assigned to one
  # eligible starter or bench role.
  for (player_index in seq_len(nrow(pool))) {
    player_id <- pool$player_id[[player_index]]
    player_y <- y_variable_rows[variables$player_id[y_variable_rows] == player_id]
    player_x <- x_variable_rows[variables$player_id[x_variable_rows] == player_id]
    indexes <- c(player_y, player_x)
    values <- c(rep(1, length(player_y)), rep(-1, length(player_x)))
    constraints <- append_lp_constraint(
      constraints,
      indexes,
      values,
      "=",
      as.numeric(owned[[player_index]])
    )
  }

  # All seven starting slots are filled exactly. Position-specific bench roles
  # have unit capacity, while the shared bench total is exactly seven.
  for (role_index in seq_len(nrow(roles))) {
    role_y <- y_variable_rows[
      variables$role_id[y_variable_rows] == roles$role_id[[role_index]]
    ]
    direction <- if (roles$role_type[[role_index]] == "starter") "=" else "<="
    constraints <- append_lp_constraint(constraints, role_y, 1, direction, 1)
  }
  bench_role_ids <- roles$role_id[roles$role_type == "bench"]
  bench_y <- y_variable_rows[
    variables$role_id[y_variable_rows] %in% bench_role_ids
  ]
  constraints <- append_lp_constraint(
    constraints,
    bench_y,
    1,
    "=",
    config$bench_slots
  )

  # Enforce league maximums across both starters and bench.
  position_by_id <- setNames(pool$position, pool$player_id)
  for (position in config$positions) {
    position_y <- y_variable_rows[
      position_by_id[variables$player_id[y_variable_rows]] == position
    ]
    constraints <- append_lp_constraint(
      constraints,
      position_y,
      1,
      "<=",
      unname(config$position_maximums[[position]])
    )
  }

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

  list(
    objective = objective,
    constraint_matrix = constraint_matrix,
    directions = directions,
    right_hand_sides = right_hand_sides,
    variables = variables,
    roles = roles,
    candidate_data = candidate_data
  )
}

solve_fractional_draft_lp <- function(lp_model, fixed_player_id = NULL) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop(
      "Package 'lpSolve' is required. Install it with install.packages('lpSolve').",
      call. = FALSE
    )
  }

  constraint_matrix <- lp_model$constraint_matrix
  directions <- lp_model$directions
  right_hand_sides <- lp_model$right_hand_sides
  if (!is.null(fixed_player_id)) {
    variables <- lp_model$variables
    fixed_index <- which(
      variables$variable_type == "selection" &
        variables$pick_index == 1L &
        variables$player_id == fixed_player_id
    )
    if (length(fixed_index) != 1L) {
      stop(
        "fixed_player_id is not eligible at the current pick: ",
        fixed_player_id,
        call. = FALSE
      )
    }
    fixed_row <- numeric(length(lp_model$objective))
    fixed_row[fixed_index] <- 1
    constraint_matrix <- rbind(constraint_matrix, fixed_row)
    directions <- c(directions, "=")
    right_hand_sides <- c(right_hand_sides, 1)
  }

  solution <- lpSolve::lp(
    direction = "max",
    objective.in = lp_model$objective,
    const.mat = constraint_matrix,
    const.dir = directions,
    const.rhs = right_hand_sides,
    all.int = FALSE
  )
  status_labels <- c(
    `0` = "optimal",
    `1` = "suboptimal",
    `2` = "infeasible",
    `3` = "unbounded",
    `5` = "numeric_failure"
  )
  status <- status_labels[[as.character(solution$status)]]
  if (is.null(status)) status <- paste0("solver_status_", solution$status)

  list(
    status = status,
    solver_status = solution$status,
    objective = if (solution$status == 0L) solution$objval else NA_real_,
    solution = if (solution$status == 0L) solution$solution else NULL
  )
}

solution_role_summary <- function(lp_model, solved, tolerance = 1e-7) {
  if (solved$status != "optimal") return(data.frame())
  variables <- lp_model$variables
  role_rows <- which(
    variables$variable_type == "role" & solved$solution > tolerance
  )
  if (!length(role_rows)) return(data.frame())
  pool <- lp_model$candidate_data$pool
  roles <- lp_model$roles
  output <- variables[role_rows, c("player_id", "role_id"), drop = FALSE]
  output$fraction <- solved$solution[role_rows]
  output <- merge(
    output,
    pool[c("player_id", "player_name", "team", "position", "projected_fantasy_points")],
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  output <- merge(
    output,
    roles[c("role_id", "role_type", "depth", "weight")],
    by = "role_id",
    all.x = TRUE,
    sort = FALSE
  )
  output$weighted_points <-
    output$fraction * output$projected_fantasy_points * output$weight
  output[order(output$role_type, output$role_id, -output$fraction), ]
}

solution_selection_summary <- function(lp_model, solved, tolerance = 1e-7) {
  if (solved$status != "optimal") return(data.frame())
  variables <- lp_model$variables
  selection_rows <- which(
    variables$variable_type == "selection" & solved$solution > tolerance
  )
  if (!length(selection_rows)) return(data.frame())
  pool <- lp_model$candidate_data$pool
  output <- variables[
    selection_rows,
    c("player_id", "pick_index"),
    drop = FALSE
  ]
  output$fraction <- solved$solution[selection_rows]
  output$round <- lp_model$candidate_data$rounds[output$pick_index]
  output$overall_pick <- lp_model$candidate_data$picks[output$pick_index]
  availability <- lp_model$candidate_data$conditional_availability
  output$conditional_availability <- mapply(
    function(player_id, pick_index) availability[player_id, pick_index],
    output$player_id,
    output$pick_index
  )
  output <- merge(
    output,
    pool[c("player_id", "player_name", "team", "position", "projected_fantasy_points")],
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  output[order(output$round, -output$fraction, -output$projected_fantasy_points), ]
}

recommend_draft_pick <- function(candidate_data) {
  lp_model <- build_fractional_draft_lp(candidate_data)
  candidates <- candidate_data$pool[
    match(candidate_data$current_candidates, candidate_data$pool$player_id),
    ,
    drop = FALSE
  ]
  results <- vector("list", nrow(candidates))
  solutions <- vector("list", nrow(candidates))
  names(solutions) <- candidates$player_id

  for (candidate_index in seq_len(nrow(candidates))) {
    player_id <- candidates$player_id[[candidate_index]]
    solved <- solve_fractional_draft_lp(lp_model, fixed_player_id = player_id)
    solutions[[player_id]] <- solved
    roles <- solution_role_summary(lp_model, solved)
    candidate_roles <- roles[roles$player_id == player_id, , drop = FALSE]
    starter_value <- if (nrow(roles)) {
      sum(roles$weighted_points[roles$role_type == "starter"])
    } else {
      NA_real_
    }
    bench_value <- if (nrow(roles)) {
      sum(roles$weighted_points[roles$role_type == "bench"])
    } else {
      NA_real_
    }
    role_description <- if (nrow(candidate_roles)) {
      paste0(
        candidate_roles$role_id,
        " ",
        format(round(candidate_roles$fraction, 3), trim = TRUE),
        collapse = "; "
      )
    } else {
      NA_character_
    }
    results[[candidate_index]] <- data.frame(
      player_id = player_id,
      player_name = candidates$player_name[[candidate_index]],
      team = candidates$team[[candidate_index]],
      position = candidates$position[[candidate_index]],
      projected_fantasy_points = candidates$projected_fantasy_points[[candidate_index]],
      status = solved$status,
      roster_objective = solved$objective,
      starter_value = starter_value,
      bench_value = bench_value,
      candidate_roles = role_description,
      stringsAsFactors = FALSE
    )
  }

  rankings <- do.call(rbind, results)
  rankings <- rankings[order(-rankings$roster_objective, -rankings$projected_fantasy_points), ]
  rankings$rank <- seq_len(nrow(rankings))
  best_objective <- if (any(is.finite(rankings$roster_objective))) {
    max(rankings$roster_objective, na.rm = TRUE)
  } else {
    NA_real_
  }
  rankings$points_behind_best <- best_objective - rankings$roster_objective
  rankings <- rankings[c(
    "rank", "player_id", "player_name", "team", "position",
    "projected_fantasy_points", "status", "roster_objective",
    "points_behind_best", "starter_value", "bench_value", "candidate_roles"
  )]

  optimal_rankings <- rankings[rankings$status == "optimal", , drop = FALSE]
  if (!nrow(optimal_rankings)) {
    stop("No current candidate produced a feasible fractional roster.", call. = FALSE)
  }
  best_player_id <- optimal_rankings$player_id[[1L]]
  best_solution <- solutions[[best_player_id]]
  list(
    rankings = rankings,
    best_player_id = best_player_id,
    best_solution = best_solution,
    selection_plan = solution_selection_summary(lp_model, best_solution),
    role_plan = solution_role_summary(lp_model, best_solution),
    lp_model = lp_model
  )
}
