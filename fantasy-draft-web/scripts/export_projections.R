args <- commandArgs(trailingOnly = TRUE)

input_path <- if (length(args) >= 1) args[[1]] else "../nflverse-projections/data/derived/simple_linear_projections_2026.csv"
output_path <- if (length(args) >= 2) args[[2]] else "public/projections.json"

projections <- read.csv(input_path, stringsAsFactors = FALSE)
projections <- projections[
  !is.na(projections$player_id) & projections$player_id != "" &
    !is.na(projections$player_name) & projections$player_name != "" &
    is.finite(projections$projected_fantasy_points),
]

web_projections <- data.frame(
  id = projections$player_id,
  name = projections$player_name,
  nflTeam = projections$team,
  position = projections$position,
  positionRank = projections$position_rank,
  depthRole = projections$depth_role,
  projectedPoints = round(projections$projected_fantasy_points, 2),
  projectedP10 = round(projections$projected_fantasy_points_p10, 2),
  projectedP50 = round(projections$projected_fantasy_points_p50, 2),
  projectedP90 = round(projections$projected_fantasy_points_p90, 2),
  priorPoints = round(projections$prior_points, 2),
  availability = round(projections$availability_1yr, 3),
  rookie = projections$rookie == 1,
  stringsAsFactors = FALSE
)

roster_path <- file.path(
  dirname(dirname(input_path)),
  "raw", "rosters", "rosters_2026.parquet"
)
rosters <- as.data.frame(arrow::read_parquet(roster_path))

kickers <- rosters[
  rosters$position == "K" &
    rosters$status == "ACT" &
    !is.na(rosters$gsis_id) &
    !is.na(rosters$full_name) &
    !is.na(rosters$team),
]
kickers <- kickers[order(kickers$full_name),]

kicker_rows <- data.frame(
  id = kickers$gsis_id,
  name = kickers$full_name,
  nflTeam = kickers$team,
  position = "K",
  positionRank = seq_len(nrow(kickers)),
  depthRole = "kicker",
  projectedPoints = 0,
  projectedP10 = 0,
  projectedP50 = 0,
  projectedP90 = 0,
  priorPoints = NA_real_,
  availability = NA_real_,
  rookie = FALSE,
  stringsAsFactors = FALSE
)

nfl_teams <- sort(unique(rosters$team[!is.na(rosters$team)]))
defense_rows <- data.frame(
  id = paste0("DST-", nfl_teams),
  name = paste(nfl_teams, "D/ST"),
  nflTeam = nfl_teams,
  position = "DST",
  positionRank = seq_along(nfl_teams),
  depthRole = "team defense",
  projectedPoints = 0,
  projectedP10 = 0,
  projectedP50 = 0,
  projectedP90 = 0,
  priorPoints = NA_real_,
  availability = NA_real_,
  rookie = FALSE,
  stringsAsFactors = FALSE
)

web_projections <- rbind(web_projections, defense_rows, kicker_rows)

jsonlite::write_json(web_projections, output_path, dataframe = "rows", na = "null", pretty = FALSE)
cat("Wrote", nrow(web_projections), "players to", output_path, "\n")
