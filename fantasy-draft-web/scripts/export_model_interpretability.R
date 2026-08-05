#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
projection_path <- if (length(args) >= 1) args[[1]] else "../nflverse-projections/data/derived/simple_linear_projections_2026.csv"
model_path <- if (length(args) >= 2) args[[2]] else "../nflverse-projections/data/models/simple_linear_models.rds"
history_path <- if (length(args) >= 3) args[[3]] else "../nflverse-projections/data/derived/fantasy_points_all_seasons.csv"
output_path <- if (length(args) >= 4) args[[4]] else "public/player-interpretability.json"

projections <- read.csv(projection_path, stringsAsFactors = FALSE)
projections <- projections[
  !is.na(projections$player_id) & projections$player_id != "" &
    !is.na(projections$player_name) & projections$player_name != "" &
    is.finite(projections$projected_fantasy_points),
]
models <- readRDS(model_path)
history <- read.csv(history_path, stringsAsFactors = FALSE)

prior2 <- history[history$season == 2024, c("player_id", "fantasy_points")]
prior2 <- prior2[!duplicated(prior2$player_id), ]
projections$prior2_points <- prior2$fantasy_points[
  match(projections$player_id, prior2$player_id)
]
projections$prior2_points[is.na(projections$prior2_points)] <- 0
projections$prior_two_year_points <-
  0.65 * projections$prior_points + 0.35 * projections$prior2_points
projections$log_draft_pick <- log1p(projections$draft_pick)

coefficient <- function(coefficients, name) {
  if (!name %in% names(coefficients)) return(0)
  value <- unname(coefficients[[name]])
  if (is.null(value) || !is.finite(value)) 0 else value
}

contribution <- function(key, label, points, input) {
  list(
    key = key,
    label = label,
    points = round(unname(points), 4),
    input = input
  )
}

percent_label <- function(value) paste0(round(value * 100), "%")
number_label <- function(value, digits = 1) format(round(value, digits), nsmall = digits, trim = TRUE)

explain_player <- function(row) {
  position <- row$position[[1]]
  model <- models[[position]]
  coefficients <- stats::coef(model)
  role <- row$depth_role[[1]]
  role_coefficient <- if (role == "secondary") {
    "depth_rolesecondary"
  } else if (role == "depth") {
    "depth_roledepth"
  } else {
    NULL
  }
  role_interaction <- if (role == "secondary") {
    "shrunk_prior_ppg:depth_rolesecondary"
  } else if (role == "depth") {
    "shrunk_prior_ppg:depth_roledepth"
  } else {
    NULL
  }

  role_points <- if (is.null(role_coefficient)) 0 else coefficient(coefficients, role_coefficient)
  role_interaction_points <- if (is.null(role_interaction)) {
    0
  } else {
    coefficient(coefficients, role_interaction) * row$shrunk_prior_ppg[[1]]
  }

  contributions <- list(
    contribution("baseline", "Position baseline", coefficient(coefficients, "(Intercept)"), position),
    contribution("prior_points", "Prior-season points", coefficient(coefficients, "prior_points") * row$prior_points[[1]], paste0(number_label(row$prior_points[[1]]), " pts")),
    contribution("two_year_points", "Two-year weighted form", coefficient(coefficients, "prior_two_year_points") * row$prior_two_year_points[[1]], paste0(number_label(row$prior_two_year_points[[1]]), " weighted pts")),
    contribution("shrunk_ppg", "Shrunk points per game", coefficient(coefficients, "shrunk_prior_ppg") * row$shrunk_prior_ppg[[1]], paste0(number_label(row$shrunk_prior_ppg[[1]], 2), " PPG")),
    contribution("availability_1yr", "One-year availability", coefficient(coefficients, "availability_1yr") * row$availability_1yr[[1]], percent_label(row$availability_1yr[[1]])),
    contribution("availability_2yr", "Two-year availability", coefficient(coefficients, "availability_2yr") * row$availability_2yr[[1]], percent_label(row$availability_2yr[[1]])),
    contribution("depth_rank", "Depth-chart rank", coefficient(coefficients, "depth_rank") * row$depth_rank[[1]], paste0("No. ", row$depth_rank[[1]])),
    contribution("depth_role", "Depth-chart role", role_points, tools::toTitleCase(role)),
    contribution("draft_pick", "Draft capital", coefficient(coefficients, "log_draft_pick") * row$log_draft_pick[[1]], paste0("Pick ", row$draft_pick[[1]])),
    contribution("age", "Age", coefficient(coefficients, "age") * row$age[[1]], paste0(number_label(row$age[[1]]), " years")),
    contribution("experience", "NFL experience", coefficient(coefficients, "years_exp") * row$years_exp[[1]], paste0(row$years_exp[[1]], " years")),
    contribution("rookie", "Rookie status", coefficient(coefficients, "rookie") * row$rookie[[1]], if (row$rookie[[1]] == 1) "Rookie" else "Veteran"),
    contribution("rookie_draft", "Rookie × draft capital", coefficient(coefficients, "log_draft_pick:rookie") * row$log_draft_pick[[1]] * row$rookie[[1]], if (row$rookie[[1]] == 1) paste0("Rookie · pick ", row$draft_pick[[1]]) else "Not active"),
    contribution("role_ppg", "Role × shrunk PPG", role_interaction_points, paste0(tools::toTitleCase(role), " · ", number_label(row$shrunk_prior_ppg[[1]], 2), " PPG"))
  )

  raw_prediction <- sum(vapply(contributions, function(item) item$points, numeric(1)))
  # The stored point values are rounded to four decimals, so compute the exact
  # prediction independently for the reconciliation check and display total.
  newdata <- row
  newdata$depth_role <- factor(newdata$depth_role, levels = model$xlevels$depth_role)
  exact_prediction <- unname(stats::predict(model, newdata = newdata))
  if (abs(raw_prediction - exact_prediction) > 0.01) {
    stop("Contribution reconciliation failed for ", row$player_name[[1]], call. = FALSE)
  }

  projected_points <- max(0, exact_prediction)
  if (exact_prediction < 0) {
    contributions[[length(contributions) + 1L]] <- contribution(
      "projection_floor", "Zero-point projection floor", -exact_prediction,
      "Negative raw prediction floored to zero"
    )
  }

  list(
    position = position,
    rawPrediction = round(exact_prediction, 4),
    projectedPoints = round(projected_points, 4),
    contributions = contributions
  )
}

explanations <- lapply(seq_len(nrow(projections)), function(index) {
  explain_player(projections[index, , drop = FALSE])
})
names(explanations) <- projections$player_id

output <- list(
  meta = list(
    model = "Position-specific linear regression",
    season = 2026,
    note = "Each bar is the player's input value multiplied by the fitted coefficient. Bars sum to the mean projection."
  ),
  players = explanations
)

jsonlite::write_json(output, output_path, auto_unbox = TRUE, pretty = FALSE)
message("Wrote model explanations for ", length(explanations), " players to ", output_path)
