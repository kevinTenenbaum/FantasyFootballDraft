#!/usr/bin/env Rscript

# Summarize calibration and accuracy for the grouped out-of-fold predictions
# created by build_simple_linear_model.R.

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

training_path <- file.path(
  script_dir,
  "data",
  "derived",
  "simple_linear_training_data.csv"
)
output_dir <- file.path(script_dir, "data", "derived")
quantile_levels <- c(p10 = 0.10, p50 = 0.50, p90 = 0.90)
quantile_prediction_columns <- paste0(
  "projected_fantasy_points_",
  names(quantile_levels)
)

metric_row <- function(rows, group_type, group) {
  calibration_model <- lm(
    target_points ~ projected_fantasy_points,
    data = rows
  )
  errors <- rows$projected_fantasy_points - rows$target_points

  data.frame(
    group_type = group_type,
    group = as.character(group),
    n = nrow(rows),
    mean_actual = mean(rows$target_points),
    mean_predicted = mean(rows$projected_fantasy_points),
    mean_error = mean(errors),
    mae = mean(abs(errors)),
    rmse = sqrt(mean(errors^2)),
    pearson_correlation = cor(
      rows$target_points,
      rows$projected_fantasy_points,
      method = "pearson"
    ),
    spearman_correlation = cor(
      rows$target_points,
      rows$projected_fantasy_points,
      method = "spearman"
    ),
    calibration_intercept = unname(coef(calibration_model)[[1]]),
    calibration_slope = unname(coef(calibration_model)[[2]]),
    r_squared = summary(calibration_model)$r.squared,
    stringsAsFactors = FALSE
  )
}

summarize_groups <- function(rows, column, group_type = column) {
  groups <- sort(unique(rows[[column]]))
  output <- vector("list", length(groups))
  for (index in seq_along(groups)) {
    group <- groups[[index]]
    output[[index]] <- metric_row(
      rows[rows[[column]] == group, , drop = FALSE],
      group_type,
      group
    )
  }
  do.call(rbind, output)
}

pinball_loss <- function(actual, predicted, probability) {
  residual <- actual - predicted
  mean(residual * (probability - as.integer(residual < 0)))
}

uncertainty_metric_row <- function(rows, group_type, group) {
  actual <- rows$target_points
  p10 <- rows$projected_fantasy_points_p10
  p50 <- rows$projected_fantasy_points_p50
  p90 <- rows$projected_fantasy_points_p90

  data.frame(
    group_type = group_type,
    group = as.character(group),
    n = nrow(rows),
    p10_empirical_probability = mean(actual <= p10),
    p50_empirical_probability = mean(actual <= p50),
    p90_empirical_probability = mean(actual <= p90),
    p10_pinball_loss = pinball_loss(actual, p10, quantile_levels[["p10"]]),
    p50_pinball_loss = pinball_loss(actual, p50, quantile_levels[["p50"]]),
    p90_pinball_loss = pinball_loss(actual, p90, quantile_levels[["p90"]]),
    interval_80_coverage = mean(actual >= p10 & actual <= p90),
    below_p10_rate = mean(actual < p10),
    above_p90_rate = mean(actual > p90),
    mean_interval_width = mean(p90 - p10),
    median_interval_width = median(p90 - p10),
    stringsAsFactors = FALSE
  )
}

summarize_uncertainty_groups <- function(rows, column, group_type = column) {
  groups <- sort(unique(rows[[column]]))
  output <- vector("list", length(groups))
  for (index in seq_along(groups)) {
    group <- groups[[index]]
    output[[index]] <- uncertainty_metric_row(
      rows[rows[[column]] == group, , drop = FALSE],
      group_type,
      group
    )
  }
  do.call(rbind, output)
}

calibration_bins <- function(rows, position_label) {
  ordered_rows <- order(rows$projected_fantasy_points, rows$target_points)
  rows$calibration_decile <- NA_integer_
  rows$calibration_decile[ordered_rows] <- pmin(
    10L,
    ceiling(seq_along(ordered_rows) * 10 / length(ordered_rows))
  )

  output <- vector("list", 10L)
  for (decile in seq_len(10L)) {
    bin <- rows[rows$calibration_decile == decile, , drop = FALSE]
    errors <- bin$projected_fantasy_points - bin$target_points
    output[[decile]] <- data.frame(
      position = position_label,
      calibration_decile = decile,
      n = nrow(bin),
      min_predicted = min(bin$projected_fantasy_points),
      max_predicted = max(bin$projected_fantasy_points),
      mean_predicted = mean(bin$projected_fantasy_points),
      mean_actual = mean(bin$target_points),
      mean_error = mean(errors),
      mae = mean(abs(errors)),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

evaluate_predictions <- function(path = training_path) {
  training <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing_quantile_columns <- setdiff(
    quantile_prediction_columns,
    names(training)
  )
  if (length(missing_quantile_columns)) {
    stop(
      "Missing quantile predictions: ",
      paste(missing_quantile_columns, collapse = ", "),
      ". Run build_simple_linear_model.R first.",
      call. = FALSE
    )
  }
  quantiles_out_of_order <-
    training$projected_fantasy_points_p10 >
      training$projected_fantasy_points_p50 |
    training$projected_fantasy_points_p50 >
      training$projected_fantasy_points_p90
  if (any(quantiles_out_of_order)) {
    stop("Quantile predictions are not monotonically ordered.", call. = FALSE)
  }

  accuracy_summary <- rbind(
    metric_row(training, "overall", "all_players"),
    summarize_groups(training, "position"),
    summarize_groups(training, "season"),
    summarize_groups(training, "depth_role"),
    summarize_groups(training, "cv_fold")
  )

  uncertainty_summary <- rbind(
    uncertainty_metric_row(training, "overall", "all_players"),
    summarize_uncertainty_groups(training, "position"),
    summarize_uncertainty_groups(training, "season"),
    summarize_uncertainty_groups(training, "depth_role"),
    summarize_uncertainty_groups(training, "cv_fold")
  )

  calibration_summary <- calibration_bins(training, "ALL")
  for (position_name in sort(unique(training$position))) {
    position_rows <- training[
      training$position == position_name,
      ,
      drop = FALSE
    ]
    calibration_summary <- rbind(
      calibration_summary,
      calibration_bins(position_rows, position_name)
    )
  }

  training$prediction_error <-
    training$projected_fantasy_points - training$target_points
  miss_columns <- c(
    "season", "player_id", "player_name", "team", "position",
    "depth_role", "cv_fold", "target_points", "projected_fantasy_points",
    quantile_prediction_columns, "prediction_error"
  )
  underpredictions <- head(
    training[order(training$prediction_error), miss_columns],
    25L
  )
  underpredictions$miss_direction <- "underpredicted"
  overpredictions <- head(
    training[order(-training$prediction_error), miss_columns],
    25L
  )
  overpredictions$miss_direction <- "overpredicted"
  largest_misses <- rbind(underpredictions, overpredictions)

  utils::write.csv(
    accuracy_summary,
    file.path(output_dir, "simple_linear_accuracy_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    calibration_summary,
    file.path(output_dir, "simple_linear_calibration_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    uncertainty_summary,
    file.path(output_dir, "simple_linear_uncertainty_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    largest_misses,
    file.path(output_dir, "simple_linear_largest_misses.csv"),
    row.names = FALSE
  )

  list(
    accuracy = accuracy_summary,
    calibration = calibration_summary,
    uncertainty = uncertainty_summary,
    largest_misses = largest_misses
  )
}

evaluation_results <- evaluate_predictions()

cat("Grouped out-of-fold accuracy and calibration summary\n\n")
print(
  evaluation_results$accuracy[
    evaluation_results$accuracy$group_type %in% c("overall", "position"),
  ],
  row.names = FALSE,
  digits = 3
)
cat("\nQuantile calibration and 80% interval coverage\n\n")
print(
  evaluation_results$uncertainty[
    evaluation_results$uncertainty$group_type %in% c("overall", "position"),
  ],
  row.names = FALSE,
  digits = 3
)
cat("\nCalibration by predicted-point decile\n\n")
print(
  evaluation_results$calibration[
    evaluation_results$calibration$position == "ALL",
  ],
  row.names = FALSE,
  digits = 3
)

invisible(evaluation_results)
