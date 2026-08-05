#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(jsonlite)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
projection_path <- if (length(args) >= 1) args[[1]] else "public/projections.json"
injury_path <- if (length(args) >= 2) args[[2]] else "../nflverse-projections/data/raw/injuries/injuries_2025.parquet"
output_path <- if (length(args) >= 3) args[[3]] else "public/player-summaries.json"

source("../ff.R")

players <- jsonlite::fromJSON(projection_path, simplifyDataFrame = TRUE)
injuries <- arrow::read_parquet(injury_path) |>
  as.data.frame()

rankings <- tryCatch(
  fetchFantasyProsRankings(1),
  error = function(error) {
    warning("FantasyPros rankings were unavailable: ", conditionMessage(error))
    data.frame()
  }
)

normalize_name <- function(value) {
  value |>
    iconv(to = "ASCII//TRANSLIT") |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]", "")
}

normalize_team <- function(value) {
  dplyr::recode(
    toupper(value),
    "LA" = "LAR",
    "WSH" = "WAS",
    "JAC" = "JAX",
    "OAK" = "LV",
    "SD" = "LAC",
    .default = toupper(value)
  )
}

normalize_position <- function(value) {
  value <- toupper(value)
  ifelse(value %in% c("D/ST", "DEF"), "DST", value)
}

players$name_key <- normalize_name(players$name)
players$position_key <- normalize_position(players$position)
players$team_key <- normalize_team(players$nflTeam)

if (nrow(rankings) > 0) {
  rankings$name_key <- normalize_name(rankings$PlayerName)
  rankings$position_key <- normalize_position(rankings$Pos)
  rankings$team_key <- normalize_team(rankings$Team)
  rankings <- rankings |>
    arrange(ECR) |>
    distinct(name_key, position_key, .keep_all = TRUE)
} else {
  rankings <- data.frame(
    name_key = character(), position_key = character(), team_key = character(),
    ECR = numeric(), PositionRank = numeric(), Best = numeric(), Worst = numeric(),
    stringsAsFactors = FALSE
  )
}

rank_key <- paste(rankings$name_key, rankings$position_key, sep = "::")
player_key <- paste(players$name_key, players$position_key, sep = "::")
rank_match <- match(player_key, rank_key)

# Team defenses use different display names across data providers, so match those
# rows by team and position when a name match is not available.
dst_rows <- which(players$position_key == "DST" & is.na(rank_match))
if (length(dst_rows) > 0 && nrow(rankings) > 0) {
  dst_rankings <- which(rankings$position_key == "DST")
  rank_match[dst_rows] <- dst_rankings[
    match(players$team_key[dst_rows], rankings$team_key[dst_rankings])
  ]
}

ranking_for <- function(index) {
  match_index <- rank_match[[index]]
  if (is.na(match_index)) return(NULL)
  rankings[match_index, , drop = FALSE]
}

position_label <- function(position, rank) {
  if (is.na(rank)) return(position)
  paste0(position, round(rank))
}

model_context <- function(player) {
  if (!is.finite(player$projectedPoints) || player$projectedPoints <= 0) return(NULL)

  role <- if (!is.na(player$depthRole) && nzchar(player$depthRole)) {
    tolower(player$depthRole)
  } else {
    "depth"
  }

  sprintf(
    "Our model projects %.0f points (%.0f at the 90th percentile) and treats him as a %s %s option.",
    player$projectedPoints,
    player$projectedP90,
    role,
    player$position
  )
}

performance_summary <- function(player, ranking) {
  model_text <- model_context(player)

  if (is.null(ranking)) {
    if (player$position_key == "DST") {
      return(sprintf(
        "Current consensus coverage is limited for the %s defense. Treat the unit as a matchup-driven streaming option unless its preseason role and personnel improve.",
        player$nflTeam
      ))
    }

    if (player$position_key == "K") {
      return(sprintf(
        "Current consensus coverage is limited for %s. Kicker value is driven mostly by job security, team scoring environment, and weekly matchup.",
        player$name
      ))
    }

    return(paste(
      sprintf("Current expert rankings provide limited coverage for %s, which usually signals a deep-league or watch-list player.", player$name),
      model_text
    ))
  }

  ecr <- suppressWarnings(as.numeric(ranking$ECR[[1]]))
  position_rank <- suppressWarnings(as.numeric(ranking$PositionRank[[1]]))
  best <- suppressWarnings(as.numeric(ranking$Best[[1]]))
  worst <- suppressWarnings(as.numeric(ranking$Worst[[1]]))

  tier <- dplyr::case_when(
    ecr <= 12 ~ "an elite first-round option",
    ecr <= 36 ~ "an early-round cornerstone",
    ecr <= 100 ~ "a core fantasy starter",
    ecr <= 200 ~ "a later-round depth or upside target",
    TRUE ~ "a deep-league option"
  )

  first_sentence <- sprintf(
    "Consensus analysts view %s as %s (ECR %.0f, %s).",
    player$name,
    tier,
    ecr,
    position_label(player$position_key, position_rank)
  )

  if (is.finite(best) && is.finite(worst) && worst - best >= 40) {
    second_sentence <- sprintf(
      "The expert range of %.0f-%.0f shows meaningful disagreement, so his role and preseason usage matter more than the average rank alone.",
      best,
      worst
    )
  } else if (!is.null(model_text)) {
    second_sentence <- model_text
  } else {
    second_sentence <- sprintf(
      "The expert range is %.0f-%.0f, indicating relatively stable agreement around that price.",
      best,
      worst
    )
  }

  paste(first_sentence, second_sentence)
}

clean_injury <- function(value) {
  value <- value[!is.na(value) & nzchar(value)]
  value <- value[!stringr::str_detect(
    tolower(value),
    "illness|(^|[^a-z])rest([^a-z]|$)|personal|not injury[ -]related"
  )]
  unique(value)
}

injury_summary <- function(player) {
  if (player$position_key == "DST") {
    return("This is a unit-level projection; injuries to individual defenders can materially change the outlook, so recheck the depth chart near Week 1.")
  }

  if (isTRUE(player$rookie)) {
    return("As a rookie, he has no NFL injury-report history in this dataset; current camp reports should drive the health assessment.")
  }

  player_injuries <- injuries[injuries$gsis_id == player$id, , drop = FALSE]
  if (nrow(player_injuries) == 0) {
    return("No material 2025 NFL injury listing is available in the local nflverse reports; check current camp news before drafting.")
  }

  labels <- clean_injury(c(
    player_injuries$report_primary_injury,
    player_injuries$report_secondary_injury,
    player_injuries$practice_primary_injury,
    player_injuries$practice_secondary_injury
  ))

  out_weeks <- length(unique(player_injuries$week[
    !is.na(player_injuries$report_status) &
      tolower(player_injuries$report_status) %in% c("out", "doubtful")
  ]))

  if (length(labels) == 0) {
    return("No recurring physical injury appears in his 2025 nflverse reports; this is historical context, not a current medical clearance.")
  }

  injury_text <- paste(head(tolower(labels), 3), collapse = ", ")
  if (out_weeks > 0) {
    sprintf(
      "His 2025 reports included %s, with an Out/Doubtful designation in %d week%s. Treat this as historical risk and verify his current camp status.",
      injury_text,
      out_weeks,
      ifelse(out_weeks == 1, "", "s")
    )
  } else {
    sprintf(
      "His 2025 reports included %s but no Out/Doubtful week in this dataset. Treat this as historical context and verify his current camp status.",
      injury_text
    )
  }
}

curated <- list(
  "Josh Allen" = list(
    bullets = c(
      "Still the consensus QB1: his rushing production keeps the weekly floor and ceiling elite even after a quieter 2025 passing season.",
      "He had January surgery for a right-foot fracture, then returned to Bills practice by June; the latest team update did not indicate a restriction."
    ),
    sourceLabel = "Current fantasy analysis and Buffalo Bills updates"
  ),
  "Puka Nacua" = list(
    bullets = c(
      "Coming off 129 catches, 1,715 yards, and 10 touchdowns, he is being valued as a top-five overall PPR pick and the centerpiece of the Rams passing game.",
      "He was running routes and catching passes during May OTAs, with no active limitation noted in the latest Rams update."
    ),
    sourceLabel = "Current fantasy analysis and Los Angeles Rams updates"
  ),
  "Bijan Robinson" = list(
    bullets = c(
      "Analysts are treating him as an elite three-down back after 2,298 scrimmage yards and 79 catches; the receiving workload gives him one of fantasy's safest ceilings.",
      "There is no prominent current injury concern in the latest team outlook, though Atlanta may manage his enormous workload with Brian Robinson Jr. behind him."
    ),
    sourceLabel = "Current fantasy analysis and Atlanta Falcons updates"
  ),
  "Christian McCaffrey" = list(
    bullets = c(
      "He again delivered league-winning usage in 2025, but 450 total touches and his age make workload durability the central risk in his price.",
      "The 2024 Achilles, calf, and knee problems remain relevant history; he is currently active in 49ers camp, with no new limitation highlighted."
    ),
    sourceLabel = "Current fantasy analysis and San Francisco 49ers updates"
  )
)

summaries <- lapply(seq_len(nrow(players)), function(index) {
  player <- players[index, , drop = FALSE]
  override <- curated[[player$name[[1]]]]

  if (!is.null(override)) {
    return(list(
      bullets = override$bullets,
      updated = "2026-08-05",
      sourceLabel = override$sourceLabel
    ))
  }

  list(
    bullets = c(
      performance_summary(player, ranking_for(index)),
      injury_summary(player)
    ),
    updated = "2026-08-05",
    sourceLabel = "FantasyPros consensus and 2025 nflverse injury reports"
  )
})

names(summaries) <- players$id
jsonlite::write_json(summaries, output_path, auto_unbox = TRUE, pretty = TRUE)

message("Wrote ", length(summaries), " player summaries to ", output_path)
