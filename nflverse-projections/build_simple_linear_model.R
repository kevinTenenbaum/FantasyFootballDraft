#!/usr/bin/env Rscript

# Simple position-specific linear model for next-season total fantasy points.
# Edit the target season and scoring rules below, then run this file after the
# nflverse collector has downloaded the historical inputs.

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
rstudio_file <- ""
if (!length(script_file) &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  rstudio_file <- tryCatch(
    rstudioapi::getSourceEditorContext()$path,
    error = function(error) ""
  )
}

script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]])))
} else if (nzchar(rstudio_file)) {
  dirname(normalizePath(rstudio_file))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "R", "fantasy_points.R"))

target_season <- 2026L
scoring_rules <- c(
  passing_yards = 0.04,
  passing_tds = 4,
  passing_interceptions = -2,
  passing_2pt_conversions = 2,
  rushing_yards = 0.10,
  rushing_tds = 6,
  rushing_2pt_conversions = 2,
  receptions = 1,
  receiving_yards = 0.10,
  receiving_tds = 6,
  receiving_2pt_conversions = 2,
  fumbles_lost_total = -2,
  special_teams_tds = 6
)

data_dir <- file.path(script_dir, "data", "raw")
derived_dir <- file.path(script_dir, "data", "derived")
model_dir <- file.path(script_dir, "data", "models")
positions <- c("QB", "RB", "WR", "TE")
role_levels <- c("starter", "secondary", "depth")
role_shrink_games <- 4
role_prior_position_weight <- 10
cv_folds <- 5L
cv_seed <- 20260805L
quantile_levels <- c(p10 = 0.10, p50 = 0.50, p90 = 0.90)
quantile_prediction_columns <- paste0(
  "projected_fantasy_points_",
  names(quantile_levels)
)

if (!requireNamespace("quantreg", quietly = TRUE)) {
  stop(
    "Package 'quantreg' is required for uncertainty projections. ",
    "Install it with install.packages('quantreg').",
    call. = FALSE
  )
}

read_dataset_files <- function(dataset) {
  paths <- list.files(
    file.path(data_dir, dataset),
    pattern = "\\.parquet$",
    full.names = TRUE
  )
  data <- lapply(paths, arrow::read_parquet)
  do.call(rbind, data)
}

select_opening_depth_chart <- function(depth) {
  depth <- depth[
    depth$position %in% positions & depth$depth_position %in% positions,
  ]
  depth$sort_key <- ifelse(
    is.na(depth$week),
    depth$snapshot_date,
    sprintf("W%02d", depth$week)
  )
  depth <- depth[order(
    depth$season, depth$player_id, depth$sort_key, depth$depth_rank
  ), ]
  depth <- depth[!duplicated(paste(depth$season, depth$player_id)), ]

  modern_wr <- depth$source == "espn_snapshot" & depth$position == "WR"
  depth$depth_role <- "depth"
  depth$depth_role[!modern_wr & depth$depth_rank == 1] <- "starter"
  depth$depth_role[!modern_wr & depth$depth_rank == 2] <- "secondary"
  depth$depth_role[modern_wr & depth$depth_rank <= 3] <- "starter"
  depth$depth_role[modern_wr & depth$depth_rank >= 4 & depth$depth_rank <= 6] <-
    "secondary"
  depth$depth_role <- factor(depth$depth_role, levels = role_levels)

  depth[c(
    "season", "team", "player_id", "player_name", "position",
    "depth_rank", "depth_role"
  )]
}

collapse_rosters <- function(rosters) {
  rosters$week[is.na(rosters$week)] <- 0
  rosters <- rosters[order(rosters$season, rosters$gsis_id, -rosters$week), ]
  rosters <- rosters[!duplicated(paste(rosters$season, rosters$gsis_id)), ]
  rosters[c("season", "gsis_id", "birth_date", "years_exp")]
}

collapse_draft_picks <- function(draft_picks) {
  draft_picks <- draft_picks[order(draft_picks$season, draft_picks$pick), ]
  draft_picks <- draft_picks[!duplicated(draft_picks$gsis_id), ]
  draft_picks[c("gsis_id", "season", "pick")]
}

fantasy_slice <- function(fantasy, season, prefix) {
  values <- fantasy[fantasy$season == season, c(
    "player_id", "games", "fantasy_points"
  )]
  names(values)[names(values) == "games"] <- paste0(prefix, "games")
  names(values)[names(values) == "fantasy_points"] <- paste0(prefix, "points")
  values
}

make_model_rows <- function(season, fantasy, depth, rosters, draft_picks) {
  rows <- depth[depth$season == season, ]
  rows <- merge(
    rows,
    fantasy_slice(fantasy, season - 1L, "prior_"),
    by = "player_id",
    all.x = TRUE
  )
  rows <- merge(
    rows,
    fantasy_slice(fantasy, season - 2L, "prior2_"),
    by = "player_id",
    all.x = TRUE
  )
  target <- fantasy_slice(fantasy, season, "target_")
  target <- target[c("player_id", "target_points")]
  rows <- merge(rows, target, by = "player_id", all.x = TRUE)

  roster <- rosters[rosters$season == season, ]
  names(roster)[names(roster) == "gsis_id"] <- "player_id"
  rows <- merge(rows, roster[c(
    "player_id", "birth_date", "years_exp"
  )], by = "player_id", all.x = TRUE)

  names(draft_picks) <- c("player_id", "draft_year", "draft_pick")
  rows <- merge(rows, draft_picks, by = "player_id", all.x = TRUE)

  zero_columns <- c(
    "prior_games", "prior_points", "prior2_games", "prior2_points",
    "target_points"
  )
  for (column in zero_columns) rows[[column]][is.na(rows[[column]])] <- 0

  rows$season <- season
  rows$prior_ppg <- ifelse(
    rows$prior_games > 0,
    rows$prior_points / rows$prior_games,
    0
  )
  prior_games_possible <- ifelse(season - 1L <= 2020L, 16, 17)
  prior2_games_possible <- ifelse(season - 2L <= 2020L, 16, 17)
  rows$availability_1yr <- rows$prior_games / prior_games_possible
  rows$availability_2yr <-
    (rows$prior_games + rows$prior2_games) /
    (prior_games_possible + prior2_games_possible)
  rows$prior_two_year_points <- 0.65 * rows$prior_points + 0.35 * rows$prior2_points
  rows$draft_pick[is.na(rows$draft_pick)] <- 300
  rows$log_draft_pick <- log1p(rows$draft_pick)
  rows$rookie <- as.integer(rows$draft_year == season)
  rows$rookie[is.na(rows$rookie)] <- 0
  rows$years_exp[is.na(rows$years_exp)] <- 0
  rows$age <- as.numeric(
    as.Date(paste0(season, "-09-01")) - as.Date(rows$birth_date)
  ) / 365.25
  rows
}

add_role_shrinkage <- function(training, projection) {
  position_means <- aggregate(prior_ppg ~ position, training, mean)
  role_means <- aggregate(
    prior_ppg ~ position + depth_role,
    training,
    function(x) c(mean = mean(x), n = length(x))
  )
  role_means <- data.frame(
    position = role_means$position,
    depth_role = role_means$depth_role,
    role_mean = role_means$prior_ppg[, "mean"],
    role_n = role_means$prior_ppg[, "n"]
  )
  names(position_means)[2] <- "position_mean"
  role_means <- merge(role_means, position_means, by = "position")
  role_means$role_prior <-
    (role_means$role_n * role_means$role_mean +
       role_prior_position_weight * role_means$position_mean) /
    (role_means$role_n + role_prior_position_weight)

  add_prior <- function(rows) {
    rows <- merge(
      rows,
      role_means[c("position", "depth_role", "role_prior")],
      by = c("position", "depth_role"),
      all.x = TRUE
    )
    rows <- merge(rows, position_means, by = "position", all.x = TRUE)
    rows$role_prior[is.na(rows$role_prior)] <-
      rows$position_mean[is.na(rows$role_prior)]
    rows$shrunk_prior_ppg <-
      (rows$prior_games * rows$prior_ppg +
         role_shrink_games * rows$role_prior) /
      (rows$prior_games + role_shrink_games)
    rows
  }

  list(
    training = add_prior(training),
    projection = add_prior(projection),
    role_priors = role_means
  )
}

fantasy <- calculate_cached_fantasy_points(
  scoring_rules = scoring_rules,
  data_dir = data_dir
)
depth <- select_opening_depth_chart(read_dataset_files("depth_charts"))
rosters <- collapse_rosters(read_dataset_files("rosters"))
draft_picks <- collapse_draft_picks(read_dataset_files("draft_picks"))

training_seasons <- sort(intersect(
  unique(depth$season),
  unique(fantasy$season)
))
training_seasons <- training_seasons[
  training_seasons < target_season &
    (training_seasons - 1L) %in% unique(fantasy$season)
]

training_base <- do.call(rbind, lapply(training_seasons, function(season) {
  make_model_rows(season, fantasy, depth, rosters, draft_picks)
}))
training_base$training_row_id <- seq_len(nrow(training_base))
projection_base <- make_model_rows(
  target_season, fantasy, depth, rosters, draft_picks
)

model_formula <- target_points ~
  prior_points +
  prior_two_year_points +
  shrunk_prior_ppg +
  availability_1yr +
  availability_2yr +
  depth_rank +
  depth_role +
  log_draft_pick +
  age +
  years_exp +
  rookie +
  rookie:log_draft_pick +
  depth_role:shrunk_prior_ppg

impute_ages <- function(training_rows, projection_rows) {
  for (position_name in positions) {
    median_age <- median(
      training_rows$age[training_rows$position == position_name],
      na.rm = TRUE
    )
    missing_training_age <-
      training_rows$position == position_name & is.na(training_rows$age)
    missing_projection_age <-
      projection_rows$position == position_name & is.na(projection_rows$age)
    training_rows$age[missing_training_age] <- median_age
    projection_rows$age[missing_projection_age] <- median_age
  }

  list(training = training_rows, projection = projection_rows)
}

fit_position_models <- function(training_rows) {
  fitted_models <- setNames(vector("list", length(positions)), positions)
  for (position_name in positions) {
    position_training <- training_rows[
      training_rows$position == position_name,
      ,
      drop = FALSE
    ]
    fitted_models[[position_name]] <- lm(
      model_formula,
      data = position_training
    )
  }
  fitted_models
}

predict_position_models <- function(fitted_models, rows) {
  predictions <- rep(NA_real_, nrow(rows))
  for (position_name in positions) {
    position_rows <- rows$position == position_name
    predictions[position_rows] <- pmax(
      0,
      predict(
        fitted_models[[position_name]],
        newdata = rows[position_rows, , drop = FALSE]
      )
    )
  }
  predictions
}

fit_position_quantile_models <- function(training_rows) {
  fitted_models <- setNames(vector("list", length(positions)), positions)

  for (position_name in positions) {
    position_training <- training_rows[
      training_rows$position == position_name,
      ,
      drop = FALSE
    ]
    position_models <- setNames(
      vector("list", length(quantile_levels)),
      names(quantile_levels)
    )
    for (quantile_name in names(quantile_levels)) {
      position_models[[quantile_name]] <- quantreg::rq(
        model_formula,
        tau = unname(quantile_levels[[quantile_name]]),
        data = position_training,
        method = "fn"
      )
    }
    fitted_models[[position_name]] <- position_models
  }

  fitted_models
}

predict_position_quantile_models <- function(fitted_models, rows) {
  predictions <- matrix(
    NA_real_,
    nrow = nrow(rows),
    ncol = length(quantile_levels),
    dimnames = list(NULL, quantile_prediction_columns)
  )

  for (position_name in positions) {
    position_rows <- rows$position == position_name
    for (quantile_name in names(quantile_levels)) {
      prediction_column <- paste0(
        "projected_fantasy_points_",
        quantile_name
      )
      predictions[position_rows, prediction_column] <- pmax(
        0,
        predict(
          fitted_models[[position_name]][[quantile_name]],
          newdata = rows[position_rows, , drop = FALSE]
        )
      )
    }
  }

  if (any(!is.finite(predictions))) {
    stop("Quantile models produced non-finite predictions.", call. = FALSE)
  }

  crossed <- predictions[, 1L] > predictions[, 2L] |
    predictions[, 2L] > predictions[, 3L]
  predictions <- t(apply(predictions, 1L, sort))
  colnames(predictions) <- quantile_prediction_columns

  list(
    predictions = predictions,
    crossing_count = sum(crossed)
  )
}

set.seed(cv_seed)
player_folds <- training_base[
  !duplicated(training_base$player_id),
  c("player_id", "position")
]
player_folds$cv_fold <- NA_integer_
for (position_name in positions) {
  position_players <- which(player_folds$position == position_name)
  fold_numbers <- rep(seq_len(cv_folds), length.out = length(position_players))
  player_folds$cv_fold[position_players] <- sample(fold_numbers)
}
training_base$cv_fold <- player_folds$cv_fold[
  match(training_base$player_id, player_folds$player_id)
]

training <- training_base
training$role_prior <- NA_real_
training$position_mean <- NA_real_
training$shrunk_prior_ppg <- NA_real_
training$projected_fantasy_points <- NA_real_
for (prediction_column in quantile_prediction_columns) {
  training[[prediction_column]] <- NA_real_
}
oof_quantile_crossings <- 0L

for (fold in seq_len(cv_folds)) {
  fold_training_base <- training_base[training_base$cv_fold != fold, ]
  fold_validation_base <- training_base[training_base$cv_fold == fold, ]

  fold_shrunk <- add_role_shrinkage(
    fold_training_base,
    fold_validation_base
  )
  fold_data <- impute_ages(
    fold_shrunk$training,
    fold_shrunk$projection
  )
  fold_training <- fold_data$training
  fold_validation <- fold_data$projection
  fold_models <- fit_position_models(fold_training)
  fold_predictions <- predict_position_models(fold_models, fold_validation)
  fold_quantile_models <- fit_position_quantile_models(fold_training)
  fold_quantile_result <- predict_position_quantile_models(
    fold_quantile_models,
    fold_validation
  )
  oof_quantile_crossings <- oof_quantile_crossings +
    fold_quantile_result$crossing_count

  output_rows <- match(
    fold_validation$training_row_id,
    training$training_row_id
  )
  training$age[output_rows] <- fold_validation$age
  training$role_prior[output_rows] <- fold_validation$role_prior
  training$position_mean[output_rows] <- fold_validation$position_mean
  training$shrunk_prior_ppg[output_rows] <- fold_validation$shrunk_prior_ppg
  training$projected_fantasy_points[output_rows] <- fold_predictions
  for (prediction_column in quantile_prediction_columns) {
    training[[prediction_column]][output_rows] <-
      fold_quantile_result$predictions[, prediction_column]
  }
}

full_shrunk <- add_role_shrinkage(training_base, projection_base)
full_data <- impute_ages(full_shrunk$training, full_shrunk$projection)
model_training <- full_data$training
projection <- full_data$projection
models <- fit_position_models(model_training)
projection$projected_fantasy_points <- predict_position_models(models, projection)
quantile_models <- fit_position_quantile_models(model_training)
projection_quantile_result <- predict_position_quantile_models(
  quantile_models,
  projection
)
for (prediction_column in quantile_prediction_columns) {
  projection[[prediction_column]] <-
    projection_quantile_result$predictions[, prediction_column]
}

projection$position_rank <- ave(
  projection$projected_fantasy_points,
  projection$position,
  FUN = function(points) rank(-points, ties.method = "min")
)

projection_output <- projection[order(-projection$projected_fantasy_points), c(
  "season", "player_id", "player_name", "team", "position", "position_rank",
  "depth_rank", "depth_role", "prior_points", "prior_games", "prior_ppg",
  "role_prior", "shrunk_prior_ppg", "availability_1yr", "availability_2yr",
  "draft_pick", "age", "years_exp", "rookie", "projected_fantasy_points",
  quantile_prediction_columns
)]
rounded_prediction_columns <- c(
  "projected_fantasy_points",
  quantile_prediction_columns
)
projection_output[rounded_prediction_columns] <- lapply(
  projection_output[rounded_prediction_columns],
  round,
  digits = 2
)

dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  training[names(training) != "training_row_id"],
  file.path(derived_dir, "simple_linear_training_data.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  full_shrunk$role_priors,
  file.path(derived_dir, "simple_linear_role_priors.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  projection_output,
  file.path(
    derived_dir,
    paste0("simple_linear_projections_", target_season, ".csv")
  ),
  row.names = FALSE,
  na = ""
)
saveRDS(models, file.path(model_dir, "simple_linear_models.rds"))
saveRDS(
  quantile_models,
  file.path(model_dir, "simple_linear_quantile_models.rds")
)

cat("Built QB/RB/WR/TE linear models using seasons ",
    min(training_seasons), "-", max(training_seasons), ".\n", sep = "")
cat("Added grouped ", cv_folds,
    "-fold out-of-fold mean and quantile predictions for the training rows.\n",
    sep = "")
cat("Rearranged ", oof_quantile_crossings,
    " crossing out-of-fold quantile rows and ",
    projection_quantile_result$crossing_count,
    " crossing projection rows.\n", sep = "")
cat("Wrote ", nrow(projection_output), " projections for ", target_season, ".\n\n",
    sep = "")
print(utils::head(projection_output[c(
  "player_name", "position", "position_rank", "depth_role",
  "projected_fantasy_points", quantile_prediction_columns
)], 30L), row.names = FALSE)
