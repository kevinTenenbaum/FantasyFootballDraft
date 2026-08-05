# Build per-pick player availability curves from public aggregate ADP data.

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
  players$team <- as.character(players$team)
  players <- players[
    players$position %in% c("QB", "RB", "WR", "TE") &
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

bounded_draft_survival <- function(adp, adp_sd, max_pick) {
  if (
    length(adp) != 1L || !is.finite(adp) ||
      length(max_pick) != 1L || !is.finite(max_pick) || max_pick < 1L
  ) {
    stop("adp and max_pick must be finite positive values.", call. = FALSE)
  }
  max_pick <- as.integer(max_pick)
  support <- seq_len(max_pick)

  if (length(adp_sd) != 1L || !is.finite(adp_sd) || adp_sd <= 0) {
    selected_pick <- min(max(1L, as.integer(round(adp))), max_pick)
    return(as.numeric(support <= selected_pick))
  }

  weights <- stats::dnorm(support, mean = adp, sd = adp_sd)
  if (!all(is.finite(weights)) || sum(weights) <= 0) {
    selected_pick <- min(max(1L, as.integer(round(adp))), max_pick)
    weights <- as.numeric(support == selected_pick)
  }
  weights <- weights / sum(weights)
  survival <- rev(cumsum(rev(weights)))
  pmin(1, pmax(0, survival))
}

build_availability_table <- function(projections, public_adp, max_pick) {
  public_adp <- prepare_public_adp(public_adp)
  matched <- match_public_adp(projections, public_adp)
  picks <- seq_len(as.integer(max_pick))
  output <- vector("list", nrow(matched))

  for (row_index in seq_len(nrow(matched))) {
    is_matched <- matched$match_method[[row_index]] != "unmatched"
    availability <- if (is_matched) {
      bounded_draft_survival(
        matched$public_adp_adp[[row_index]],
        matched$public_adp_stdev[[row_index]],
        max_pick
      )
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
