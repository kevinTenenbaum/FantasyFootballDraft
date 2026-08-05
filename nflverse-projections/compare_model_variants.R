#!/usr/bin/env Rscript

# Compare candidate nonlinear terms and interactions using the same grouped
# five-fold assignments created by build_simple_linear_model.R.

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
positions <- c("QB", "RB", "WR", "TE")
role_levels <- c("starter", "secondary", "depth")
role_shrink_games <- 4
role_prior_position_weight <- 10

model_formulas <- list(
  baseline = target_points ~
    prior_points + prior_two_year_points + shrunk_prior_ppg +
    availability_1yr + availability_2yr + depth_rank + depth_role +
    log_draft_pick + age + years_exp + rookie +
    rookie:log_draft_pick + depth_role:shrunk_prior_ppg,

  nonlinear_age = target_points ~
    prior_points + prior_two_year_points + shrunk_prior_ppg +
    availability_1yr + availability_2yr + depth_rank + depth_role +
    log_draft_pick + splines::ns(age, df = 3) + years_exp + rookie +
    rookie:log_draft_pick + depth_role:shrunk_prior_ppg,

  nonlinear_ppg = target_points ~
    prior_points + prior_two_year_points +
    splines::ns(shrunk_prior_ppg, df = 3) +
    availability_1yr + availability_2yr + depth_rank + depth_role +
    log_draft_pick + age + years_exp + rookie +
    rookie:log_draft_pick + depth_role:shrunk_prior_ppg,

  nonlinear_age_ppg = target_points ~
    prior_points + prior_two_year_points +
    splines::ns(shrunk_prior_ppg, df = 3) +
    availability_1yr + availability_2yr + depth_rank + depth_role +
    log_draft_pick + splines::ns(age, df = 3) + years_exp + rookie +
    rookie:log_draft_pick + depth_role:shrunk_prior_ppg,

  nonlinear_availability = target_points ~
    prior_points + prior_two_year_points + shrunk_prior_ppg +
    splines::ns(availability_1yr, df = 3) + availability_2yr +
    depth_rank + depth_role + log_draft_pick + age + years_exp + rookie +
    rookie:log_draft_pick + depth_role:shrunk_prior_ppg,

  combined = target_points ~
    prior_points + prior_two_year_points +
    splines::ns(shrunk_prior_ppg, df = 3) +
    splines::ns(availability_1yr, df = 3) + availability_2yr +
    depth_rank + depth_role +
    depth_role:splines::ns(shrunk_prior_ppg, df = 3) +
    rookie * log_draft_pick +
    splines::ns(age, df = 3) + years_exp
)

add_role_shrinkage <- function(training_rows, validation_rows) {
  position_means <- aggregate(prior_ppg ~ position, training_rows, mean)
  role_means <- aggregate(
    prior_ppg ~ position + depth_role,
    training_rows,
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
    training = add_prior(training_rows),
    validation = add_prior(validation_rows)
  )
}

impute_ages <- function(training_rows, validation_rows) {
  for (position_name in positions) {
    median_age <- median(
      training_rows$age[training_rows$position == position_name],
      na.rm = TRUE
    )
    training_rows$age[
      training_rows$position == position_name & is.na(training_rows$age)
    ] <- median_age
    validation_rows$age[
      validation_rows$position == position_name & is.na(validation_rows$age)
    ] <- median_age
  }
  list(training = training_rows, validation = validation_rows)
}

fit_position_models <- function(training_rows, model_formula) {
  models <- setNames(vector("list", length(positions)), positions)
  for (position_name in positions) {
    position_training <- training_rows[
      training_rows$position == position_name,
      ,
      drop = FALSE
    ]
    models[[position_name]] <- lm(
      model_formula,
      data = position_training
    )
  }
  models
}

predict_position_models <- function(models, rows) {
  predictions <- rep(NA_real_, nrow(rows))
  for (position_name in positions) {
    position_rows <- rows$position == position_name
    predictions[position_rows] <- pmax(
      0,
      predict(
        models[[position_name]],
        newdata = rows[position_rows, , drop = FALSE]
      )
    )
  }
  predictions
}

metric_row <- function(rows, prediction_column, variant, position = "ALL") {
  predicted <- rows[[prediction_column]]
  actual <- rows$target_points
  errors <- predicted - actual
  calibration_model <- lm(actual ~ predicted)
  ordered_rows <- order(predicted)
  top_decile_rows <- tail(
    ordered_rows,
    ceiling(length(ordered_rows) / 10)
  )

  data.frame(
    variant = variant,
    position = position,
    n = nrow(rows),
    mean_actual = mean(actual),
    mean_predicted = mean(predicted),
    mean_error = mean(errors),
    mae = mean(abs(errors)),
    rmse = sqrt(mean(errors^2)),
    pearson_correlation = cor(actual, predicted, method = "pearson"),
    spearman_correlation = cor(actual, predicted, method = "spearman"),
    calibration_intercept = unname(coef(calibration_model)[[1]]),
    calibration_slope = unname(coef(calibration_model)[[2]]),
    r_squared = summary(calibration_model)$r.squared,
    top_decile_mean_error = mean(errors[top_decile_rows]),
    stringsAsFactors = FALSE
  )
}

make_calibration_bins <- function(rows, prediction_column, variant, position) {
  predicted <- rows[[prediction_column]]
  ordered_rows <- order(predicted, rows$target_points)
  deciles <- rep(NA_integer_, nrow(rows))
  deciles[ordered_rows] <- pmin(
    10L,
    ceiling(seq_along(ordered_rows) * 10 / length(ordered_rows))
  )

  output <- vector("list", 10L)
  for (decile in seq_len(10L)) {
    bin_rows <- deciles == decile
    errors <- predicted[bin_rows] - rows$target_points[bin_rows]
    output[[decile]] <- data.frame(
      variant = variant,
      position = position,
      calibration_decile = decile,
      n = sum(bin_rows),
      min_predicted = min(predicted[bin_rows]),
      max_predicted = max(predicted[bin_rows]),
      mean_predicted = mean(predicted[bin_rows]),
      mean_actual = mean(rows$target_points[bin_rows]),
      mean_error = mean(errors),
      mae = mean(abs(errors)),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

training <- utils::read.csv(training_path, stringsAsFactors = FALSE)
training$role_prior <- NULL
training$position_mean <- NULL
training$shrunk_prior_ppg <- NULL
training$training_row_id <- seq_len(nrow(training))
training$depth_role <- factor(training$depth_role, levels = role_levels)
training$age <- as.numeric(
  as.Date(paste0(training$season, "-09-01")) -
    as.Date(training$birth_date)
) / 365.25

prediction_columns <- paste0("prediction_", names(model_formulas))
for (prediction_column in prediction_columns) {
  training[[prediction_column]] <- NA_real_
}

for (fold in sort(unique(training$cv_fold))) {
  fold_training <- training[training$cv_fold != fold, ]
  fold_validation <- training[training$cv_fold == fold, ]
  fold_shrunk <- add_role_shrinkage(fold_training, fold_validation)
  fold_data <- impute_ages(
    fold_shrunk$training,
    fold_shrunk$validation
  )
  fold_training <- fold_data$training
  fold_validation <- fold_data$validation
  output_rows <- match(
    fold_validation$training_row_id,
    training$training_row_id
  )

  for (variant in names(model_formulas)) {
    models <- fit_position_models(
      fold_training,
      model_formulas[[variant]]
    )
    training[[paste0("prediction_", variant)]][output_rows] <-
      predict_position_models(models, fold_validation)
  }
}

prediction_output <- training[c(
  "season", "player_id", "player_name", "team", "position", "depth_role",
  "cv_fold", "target_points", prediction_columns
)]

accuracy_rows <- list()
calibration_rows <- list()
row_index <- 1L
calibration_index <- 1L
comparison_variants <- names(model_formulas)
for (variant in comparison_variants) {
  prediction_column <- paste0("prediction_", variant)
  accuracy_rows[[row_index]] <- metric_row(
    training,
    prediction_column,
    variant
  )
  row_index <- row_index + 1L
  calibration_rows[[calibration_index]] <- make_calibration_bins(
    training,
    prediction_column,
    variant,
    "ALL"
  )
  calibration_index <- calibration_index + 1L

  for (position_name in positions) {
    position_rows <- training[
      training$position == position_name,
      ,
      drop = FALSE
    ]
    accuracy_rows[[row_index]] <- metric_row(
      position_rows,
      prediction_column,
      variant,
      position_name
    )
    row_index <- row_index + 1L
    calibration_rows[[calibration_index]] <- make_calibration_bins(
      position_rows,
      prediction_column,
      variant,
      position_name
    )
    calibration_index <- calibration_index + 1L
  }
}

accuracy_summary <- do.call(rbind, accuracy_rows)
calibration_summary <- do.call(rbind, calibration_rows)

utils::write.csv(
  prediction_output,
  file.path(output_dir, "simple_linear_variant_predictions.csv"),
  row.names = FALSE
)
utils::write.csv(
  accuracy_summary,
  file.path(output_dir, "simple_linear_variant_accuracy.csv"),
  row.names = FALSE
)
utils::write.csv(
  calibration_summary,
  file.path(output_dir, "simple_linear_variant_calibration.csv"),
  row.names = FALSE
)
overall_summary <- accuracy_summary[accuracy_summary$position == "ALL", ]
overall_summary <- overall_summary[order(overall_summary$rmse), ]

cat("Grouped five-fold model comparison\n\n")
print(overall_summary, row.names = FALSE, digits = 4)

invisible(list(
  predictions = prediction_output,
  accuracy = accuracy_summary,
  calibration = calibration_summary
))
