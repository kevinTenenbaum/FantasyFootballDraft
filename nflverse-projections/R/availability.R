# Build slot- and position-calibrated per-pick player availability curves from
# public aggregate ADP data.

normalize_availability_name <- function(name) {
  normalized <- iconv(as.character(name), to = "ASCII//TRANSLIT")
  normalized[is.na(normalized)] <- as.character(name)[is.na(normalized)]
  normalized <- tolower(normalized)
  normalized <- gsub("[^a-z0-9]+", " ", normalized)
  normalized <- trimws(gsub("\\s+", " ", normalized))
  normalized <- gsub("\\s+(jr|sr|ii|iii|iv)$", "", normalized)
  gsub(" ", "", normalized)
}

normalize_availability_team <- function(team) {
  normalized <- toupper(trimws(as.character(team)))
  aliases <- c(
    LA = "LAR", STL = "LAR",
    JAC = "JAX",
    OAK = "LV",
    SD = "LAC",
    WSH = "WAS"
  )
  replace <- normalized %in% names(aliases)
  normalized[replace] <- unname(aliases[normalized[replace]])
  normalized
}

validate_availability_columns <- function(data, required, label) {
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

prepare_public_adp <- function(players) {
  players <- as.data.frame(players, stringsAsFactors = FALSE)
  validate_availability_columns(
    players,
    c(
      "player_id", "name", "position", "team", "adp", "times_drafted",
      "high", "low", "stdev"
    ),
    "Public ADP data"
  )

  numeric_columns <- c(
    "player_id", "adp", "times_drafted", "high", "low", "stdev"
  )
  players[numeric_columns] <- lapply(
    players[numeric_columns],
    function(value) suppressWarnings(as.numeric(value))
  )
  players$position <- toupper(as.character(players$position))
  players$position[players$position == "DEF"] <- "DST"
  players$position[players$position == "PK"] <- "K"
  players$team <- as.character(players$team)
  players <- players[
    players$position %in% c("QB", "RB", "WR", "TE", "DST", "K") &
      is.finite(players$adp),
    ,
    drop = FALSE
  ]
  players$name_key <- normalize_availability_name(players$name)
  players$team_key <- normalize_availability_team(players$team)
  players$player_key <- paste(players$name_key, players$position, sep = "|")
  players
}

match_public_adp <- function(projections, public_adp) {
  projections <- as.data.frame(projections, stringsAsFactors = FALSE)
  validate_availability_columns(
    projections,
    c("player_id", "player_name", "team", "position"),
    "Projection data"
  )

  projections$position <- toupper(as.character(projections$position))
  projections$name_key <- normalize_availability_name(projections$player_name)
  projections$team_key <- normalize_availability_team(projections$team)
  projections$player_key <- paste(
    projections$name_key,
    projections$position,
    sep = "|"
  )

  adp_index <- rep(NA_integer_, nrow(projections))
  match_method <- rep("unmatched", nrow(projections))
  adp_key_counts <- table(public_adp$player_key)

  for (row_index in seq_len(nrow(projections))) {
    key_candidates <- which(
      public_adp$player_key == projections$player_key[[row_index]]
    )
    if (!length(key_candidates)) next

    team_candidates <- key_candidates[
      public_adp$team_key[key_candidates] == projections$team_key[[row_index]]
    ]
    if (length(team_candidates) == 1L) {
      adp_index[[row_index]] <- team_candidates[[1L]]
      match_method[[row_index]] <- "name_position_team"
    } else if (
      length(key_candidates) == 1L &&
        adp_key_counts[[projections$player_key[[row_index]]]] == 1L
    ) {
      adp_index[[row_index]] <- key_candidates[[1L]]
      match_method[[row_index]] <- "name_position"
    }
  }

  # Resolve conservative nickname/spelling variants only when team and
  # position agree, the public player has not already been used, and one
  # candidate has a uniquely small edit distance.
  used_adp_indexes <- unique(adp_index[!is.na(adp_index)])
  for (row_index in which(is.na(adp_index))) {
    candidates <- which(
      public_adp$position == projections$position[[row_index]] &
        public_adp$team_key == projections$team_key[[row_index]] &
        !seq_len(nrow(public_adp)) %in% used_adp_indexes
    )
    if (!length(candidates)) next

    distances <- as.numeric(utils::adist(
      projections$name_key[[row_index]],
      public_adp$name_key[candidates]
    ))
    lengths <- pmax(
      nchar(projections$name_key[[row_index]]),
      nchar(public_adp$name_key[candidates])
    )
    normalized_distances <- distances / lengths
    best_distance <- min(normalized_distances)
    best_candidates <- candidates[normalized_distances == best_distance]
    if (length(best_candidates) == 1L && best_distance <= 0.25) {
      adp_index[[row_index]] <- best_candidates[[1L]]
      match_method[[row_index]] <- "fuzzy_name_position_team"
      used_adp_indexes <- c(used_adp_indexes, best_candidates[[1L]])
    }
  }

  adp_columns <- c(
    "player_id", "name", "team", "adp", "times_drafted", "high", "low",
    "stdev"
  )
  matched_adp <- public_adp[adp_index, adp_columns, drop = FALSE]
  names(matched_adp) <- paste0("public_adp_", names(matched_adp))

  output <- cbind(
    projections[c("player_id", "player_name", "team", "position")],
    match_method = match_method,
    matched_adp
  )
  output$public_adp_player_id <- as.character(output$public_adp_player_id)
  output
}

lower_bounded_draft_survival <- function(adp, adp_sd, max_pick) {
  if (
    length(adp) != 1L || !is.finite(adp) ||
      length(max_pick) != 1L || !is.finite(max_pick) || max_pick < 1L
  ) {
    stop("adp and max_pick must be finite positive values.", call. = FALSE)
  }
  max_pick <- as.integer(max_pick)
  support <- seq_len(max_pick)

  if (length(adp_sd) != 1L || !is.finite(adp_sd) || adp_sd <= 0) {
    selected_pick <- max(1L, as.integer(round(adp)))
    return(as.numeric(support <= selected_pick))
  }

  lower_tail <- stats::pnorm(
    0.5,
    mean = adp,
    sd = adp_sd,
    lower.tail = FALSE
  )
  survival <- stats::pnorm(
    support - 0.5,
    mean = adp,
    sd = adp_sd,
    lower.tail = FALSE
  ) / lower_tail
  pmin(1, pmax(0, survival))
}

public_pick_cdf <- function(threshold, adp, adp_sd) {
  adp <- as.numeric(adp)
  adp_sd <- as.numeric(adp_sd)
  output <- numeric(length(adp))
  deterministic <- !is.finite(adp_sd) | adp_sd <= 0

  if (any(deterministic)) {
    selected_pick <- pmax(1L, as.integer(round(adp[deterministic])))
    output[deterministic] <- as.numeric(threshold > selected_pick)
  }
  if (any(!deterministic)) {
    indexes <- which(!deterministic)
    lower_tail <- stats::pnorm(
      0.5,
      mean = adp[indexes],
      sd = adp_sd[indexes],
      lower.tail = FALSE
    )
    remaining_tail <- stats::pnorm(
      threshold,
      mean = adp[indexes],
      sd = adp_sd[indexes],
      lower.tail = FALSE
    )
    output[indexes] <- 1 - remaining_tail / lower_tail
  }

  pmin(1, pmax(0, output))
}

capped_position_targets <- function(shares, capacities, total) {
  shares <- pmax(0, as.numeric(shares))
  capacities <- pmax(0, as.numeric(capacities))
  if (!length(shares) || length(shares) != length(capacities)) {
    stop("shares and capacities must have the same positive length.", call. = FALSE)
  }
  if (sum(capacities) + 1e-8 < total) {
    stop("Public ADP does not contain enough players for the draft horizon.", call. = FALSE)
  }
  if (sum(shares) <= 0) shares[] <- 1

  targets <- numeric(length(shares))
  active <- capacities > 0
  remaining <- as.numeric(total)
  while (remaining > 1e-8 && any(active)) {
    active_shares <- shares[active]
    if (sum(active_shares) <= 0) active_shares[] <- 1
    proposed <- remaining * active_shares / sum(active_shares)
    room <- capacities[active] - targets[active]
    accepted <- pmin(proposed, room)
    targets[active] <- targets[active] + accepted
    remaining <- total - sum(targets)
    newly_full <- room - accepted <= 1e-8
    active[which(active)[newly_full]] <- FALSE
    if (!any(newly_full)) break
  }

  if (remaining > 1e-6) {
    room <- pmax(0, capacities - targets)
    targets <- targets + remaining * room / sum(room)
  }
  targets
}

solve_participation <- function(base_weight, end_cdf, target) {
  if (target <= 0) return(rep(0, length(base_weight)))
  capacity <- sum(end_cdf)
  if (target >= capacity - 1e-8) return(rep(1, length(base_weight)))

  objective <- function(log_multiplier) {
    participation <- pmin(1, exp(log_multiplier) * base_weight)
    sum(participation * end_cdf) - target
  }
  lower <- -20
  upper <- 20
  while (objective(lower) > 0) lower <- lower - 10
  while (objective(upper) < 0) upper <- upper + 10
  multiplier <- exp(stats::uniroot(
    objective,
    interval = c(lower, upper),
    tol = 1e-10
  )$root)
  pmin(1, multiplier * base_weight)
}

calibrate_public_availability <- function(public_adp, max_pick, participation_prior = 5) {
  max_pick <- as.integer(max_pick)
  if (length(max_pick) != 1L || is.na(max_pick) || max_pick < 1L) {
    stop("max_pick must be a positive integer.", call. = FALSE)
  }
  if (nrow(public_adp) < max_pick) {
    stop("Public ADP must contain at least one player per modeled pick.", call. = FALSE)
  }

  positions <- c("QB", "RB", "WR", "TE", "DST", "K")
  end_threshold <- max_pick + 0.5
  end_cdf <- public_pick_cdf(
    end_threshold,
    public_adp$adp,
    public_adp$stdev
  )
  drafted_counts <- pmax(0, public_adp$times_drafted)
  position_frequency <- vapply(
    positions,
    function(position) sum(drafted_counts[public_adp$position == position]),
    numeric(1)
  )
  position_capacity <- vapply(
    positions,
    function(position) sum(end_cdf[public_adp$position == position]),
    numeric(1)
  )
  position_targets <- capped_position_targets(
    position_frequency,
    position_capacity,
    max_pick
  )
  names(position_targets) <- positions

  participation <- numeric(nrow(public_adp))
  for (position in positions) {
    indexes <- which(public_adp$position == position)
    if (!length(indexes)) next
    participation[indexes] <- solve_participation(
      drafted_counts[indexes] + participation_prior,
      end_cdf[indexes],
      position_targets[[position]]
    )
  }

  expected_drafted <- function(threshold) {
    sum(participation * public_pick_cdf(
      threshold,
      public_adp$adp,
      public_adp$stdev
    ))
  }
  source_clock <- numeric(max_pick)
  curves <- matrix(
    1,
    nrow = nrow(public_adp),
    ncol = max_pick,
    dimnames = list(as.character(public_adp$player_id), as.character(seq_len(max_pick)))
  )
  for (pick in seq_len(max_pick)) {
    target <- pick - 1
    if (target == 0) {
      source_clock[[pick]] <- 0.5
      next
    }
    objective <- function(threshold) expected_drafted(threshold) - target
    threshold <- stats::uniroot(
      objective,
      interval = c(0.5, end_threshold),
      tol = 1e-10
    )$root
    source_clock[[pick]] <- threshold
    curves[, pick] <- 1 - participation * public_pick_cdf(
      threshold,
      public_adp$adp,
      public_adp$stdev
    )
  }

  curves[] <- pmin(1, pmax(0, curves))
  expected_before_pick <- colSums(1 - curves)
  if (max(abs(expected_before_pick - (seq_len(max_pick) - 1))) > 1e-5) {
    stop("Availability calibration failed to conserve draft slots.", call. = FALSE)
  }
  if (any(apply(curves, 1L, function(curve) any(diff(curve) > 1e-8)))) {
    stop("Availability calibration produced a non-monotone curve.", call. = FALSE)
  }

  list(
    curves = curves,
    participation = participation,
    position_targets = position_targets,
    source_clock = source_clock
  )
}

build_availability_table <- function(projections, public_adp, max_pick) {
  public_adp <- prepare_public_adp(public_adp)
  matched <- match_public_adp(projections, public_adp)
  calibration <- calibrate_public_availability(public_adp, max_pick)
  picks <- seq_len(as.integer(max_pick))
  output <- vector("list", nrow(matched))

  for (row_index in seq_len(nrow(matched))) {
    is_matched <- matched$match_method[[row_index]] != "unmatched"
    availability <- if (is_matched) {
      public_index <- match(
        matched$public_adp_player_id[[row_index]],
        as.character(public_adp$player_id)
      )
      unname(calibration$curves[public_index, ])
    } else {
      rep(1, length(picks))
    }

    output[[row_index]] <- data.frame(
      player_id = matched$player_id[[row_index]],
      player_name = matched$player_name[[row_index]],
      team = matched$team[[row_index]],
      position = matched$position[[row_index]],
      match_method = matched$match_method[[row_index]],
      public_adp_player_id = matched$public_adp_player_id[[row_index]],
      public_adp_name = matched$public_adp_name[[row_index]],
      public_adp_team = matched$public_adp_team[[row_index]],
      adp = matched$public_adp_adp[[row_index]],
      adp_sd = matched$public_adp_stdev[[row_index]],
      times_drafted = matched$public_adp_times_drafted[[row_index]],
      observed_earliest_pick = matched$public_adp_high[[row_index]],
      observed_latest_pick = matched$public_adp_low[[row_index]],
      pick = picks,
      availability_probability = availability,
      stringsAsFactors = FALSE
    )
  }

  output <- do.call(rbind, output)
  rownames(output) <- NULL
  output
}
