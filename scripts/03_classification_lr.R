library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

# ===========================================================================
# 4. Classification
# ===========================================================================
# Target variable: category_name (multiclass classification)
# Predictors: view_count, likes, comment_count, duration_seconds,
#             like_rate, comment_rate
# ===========================================================================

# ---------------------------------------------------------------------------
# Data preparation for classification
# ---------------------------------------------------------------------------

# Removing categories with too few samples
min_samples <- 50
category_counts <- youtube_data %>%
  count(category_name) %>%
  filter(n >= min_samples) %>%
  collect()

valid_categories <- category_counts$category_name

ml_data <- youtube_data %>%
  filter(category_name %in% valid_categories) %>%
  filter(
    !is.na(category_name),
    !is.na(view_count),
    !is.na(likes),
    !is.na(comment_count),
    !is.na(duration_seconds),
    !is.na(like_rate),
    !is.na(comment_rate)
  ) %>%
  select(
    category_name,
    view_count, likes, comment_count, duration_seconds,
    like_rate, comment_rate
  )

print("Class distribution:")
ml_data %>%
  count(category_name, sort = TRUE) %>%
  print()

print(paste("Total number of records:", sdf_nrow(ml_data)))

# ---------------------------------------------------------------------------
# 80/20 train/test split
# ---------------------------------------------------------------------------
splits <- sdf_random_split(ml_data, training = 0.8, test = 0.2, seed = 42L)
train_data <- sdf_persist(splits$training, storage.level = "MEMORY_AND_DISK")
test_data  <- sdf_persist(splits$test,     storage.level = "MEMORY_AND_DISK")

print(paste("Train set:", sdf_nrow(train_data), "records"))
print(paste("Test set:", sdf_nrow(test_data), "records"))

# ---------------------------------------------------------------------------
# Spark ML Pipeline — shared preprocessing steps
#
# Data flow:
#   [Spark DataFrame]
#       → ft_string_indexer   (category_name → label)
#       → ft_vector_assembler (features → feature vector)
#       → ft_standard_scaler  (feature scaling)
#       → ml_logistic_regression
#       → prediction on test set
# ---------------------------------------------------------------------------

predictors <- c(
  "view_count", "likes", "comment_count",
  "duration_seconds", "like_rate", "comment_rate"
)

# ---------------------------------------------------------------------------
# Helper function for computing metrics from Spark predictions
# ---------------------------------------------------------------------------
compute_metrics_spark <- function(predictions_df) {
  pred_local <- predictions_df
  
  classes <- sort(unique(pred_local$category_name))
  
  # Confusion matrix
  cm_table <- table(
    Prediction = pred_local$predicted_label,
    Reference  = pred_local$category_name
  )
  
  # Per-class metrics
  per_class <- lapply(classes, function(cls) {
    if (!(cls %in% rownames(cm_table))) {
      return(data.frame(class = cls, precision = 0, recall = 0, f1 = 0))
    }
    tp <- ifelse(cls %in% rownames(cm_table) & cls %in% colnames(cm_table),
                 cm_table[cls, cls], 0
    )
    fp <- sum(cm_table[cls, ]) - tp
    fn <- ifelse(cls %in% colnames(cm_table),
                 sum(cm_table[, cls]), 0
    ) - tp
    precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1 <- ifelse((precision + recall) == 0, 0,
                 2 * precision * recall / (precision + recall)
    )
    data.frame(class = cls, precision = precision, recall = recall, f1 = f1)
  })
  
  per_class_df <- do.call(rbind, per_class)
  
  accuracy <- mean(pred_local$category_name == pred_local$predicted_label)
  macro_prec <- mean(per_class_df$precision)
  macro_rec <- mean(per_class_df$recall)
  macro_f1 <- mean(per_class_df$f1)
  
  list(
    per_class       = per_class_df,
    cm_table        = cm_table,
    accuracy        = accuracy,
    macro_precision = macro_prec,
    macro_recall    = macro_rec,
    macro_f1        = macro_f1
  )
}

print_metrics <- function(metrics, scenario_name, cv_accuracy = NULL) {
  cat("\n=================================================\n")
  cat(scenario_name, "\n")
  cat("=================================================\n")
  cat(sprintf("Accuracy:         %.4f\n", metrics$accuracy))
  cat(sprintf("Macro Precision:  %.4f\n", metrics$macro_precision))
  cat(sprintf("Macro Recall:     %.4f\n", metrics$macro_recall))
  cat(sprintf("Macro F1:         %.4f\n", metrics$macro_f1))
  if (!is.null(cv_accuracy)) {
    cat(sprintf("CV Accuracy:      %.4f\n", cv_accuracy))
  }
  cat("\nPer-class metrics:\n")
  print(metrics$per_class)
}

# ---------------------------------------------------------------------------
# Helper function to build and evaluate Spark ML pipeline
# ---------------------------------------------------------------------------
build_and_evaluate <- function(train_sdf, test_sdf, alpha, lambda, scenario_name) {
  pipeline <- ml_pipeline(sc) %>%
    ft_string_indexer(
      input_col  = "category_name",
      output_col = "label"
    ) %>%
    ft_vector_assembler(
      input_cols = predictors,
      output_col = "features_raw"
    ) %>%
    ft_standard_scaler(
      input_col     = "features_raw",
      output_col    = "features",
      with_mean     = TRUE,
      with_std      = TRUE
    ) %>%
    ml_logistic_regression(
      features_col      = "features",
      label_col         = "label",
      elastic_net_param = alpha,
      reg_param         = lambda,
      max_iter          = 30,
      family            = "multinomial"
    )
  
  fitted_pipeline <- ml_fit(pipeline, train_sdf)
  
  predictions <- ml_transform(fitted_pipeline, test_sdf)
  
  string_indexer_model <- ml_stages(fitted_pipeline)[[1]]
  labels_metadata <- ml_labels(string_indexer_model)
  
  predictions_local <- predictions %>%
    select(category_name, prediction) %>%
    collect() %>%
    mutate(predicted_label = labels_metadata[as.integer(prediction) + 1])
  
  list(
    pipeline = fitted_pipeline,
    predictions = predictions_local
  )
}

# ===========================================================================
# 4.1 Multinomial Logistic Regression (Spark ML)
#
# Three scenarios:
#   Scenario 1: L2 regularization (alpha=0), lambda=0.001 (weak regularization)
#   Scenario 2: L2 regularization (alpha=0), lambda=0.1   (strong regularization)
#   Scenario 3: L1 regularization (alpha=1), lambda=0.01  (Lasso)
# ===========================================================================

lr_scenarios <- list(
  list(name = "Scenario 1: L2 (Ridge), lambda=0.001", alpha = 0, lambda = 0.001),
  list(name = "Scenario 2: L2 (Ridge), lambda=0.1", alpha = 0, lambda = 0.1),
  list(name = "Scenario 3: L1 (Lasso), lambda=0.01", alpha = 1, lambda = 0.01)
)

# ---------------------------------------------------------------------------
# k-fold cross-validation (manual implementation in Spark ML)
# ---------------------------------------------------------------------------
build_cv_pipeline <- function(alpha, lambda, train_sdf, k = 3) {
  fold_weights <- setNames(rep(1 / k, k), paste0("fold", seq_len(k)))
  folds <- sdf_random_split(train_sdf, weights = fold_weights, seed = 42L)
  
  fold_accuracies <- numeric(k)
  
  for (i in seq_len(k)) {
    val_fold <- folds[[i]]
    train_fold <- do.call(sdf_bind_rows, folds[-i])
    
    pipeline <- ml_pipeline(sc) %>%
      ft_string_indexer(input_col = "category_name", output_col = "label") %>%
      ft_vector_assembler(input_cols = predictors, output_col = "features_raw") %>%
      ft_standard_scaler(
        input_col = "features_raw", output_col = "features",
        with_mean = TRUE, with_std = TRUE
      ) %>%
      ml_logistic_regression(
        features_col      = "features",
        label_col         = "label",
        elastic_net_param = alpha,
        reg_param         = lambda,
        max_iter          = 30,
        family            = "multinomial"
      )
    
    fitted <- ml_fit(pipeline, train_fold)
    preds <- ml_transform(fitted, val_fold) %>%
      select(label, prediction) %>%
      collect()
    
    fold_accuracies[i] <- mean(preds$label == preds$prediction)
    cat(sprintf("  Fold %d accuracy: %.4f\n", i, fold_accuracies[i]))
    
    rm(fitted, preds)
    gc()
  }
  
  mean(fold_accuracies)
}

lr_results <- list()

for (sc_params in lr_scenarios) {
  cat("\n--- Training:", sc_params$name, "---\n")
  
  cv_accuracy <- build_cv_pipeline(sc_params$alpha, sc_params$lambda, train_data, k = 3)
  cat(sprintf("CV Accuracy (k-fold): %.4f\n", cv_accuracy))
  
  result <- build_and_evaluate(
    train_data, test_data,
    sc_params$alpha, sc_params$lambda,
    sc_params$name
  )
  
  metrics <- compute_metrics_spark(result$predictions)
  print_metrics(metrics, sc_params$name, cv_accuracy)
  
  lr_results[[sc_params$name]] <- list(
    pipeline    = result$pipeline,
    predictions = result$predictions,
    metrics     = metrics,
    cv_accuracy = cv_accuracy,
    params      = sc_params
  )
}

# ---------------------------------------------------------------------------
# Comparison of scenarios and best model selection
# ---------------------------------------------------------------------------
lr_comparison <- do.call(rbind, lapply(names(lr_results), function(nm) {
  m <- lr_results[[nm]]$metrics
  data.frame(
    scenario        = nm,
    accuracy        = round(m$accuracy, 4),
    macro_f1        = round(m$macro_f1, 4),
    macro_precision = round(m$macro_precision, 4),
    macro_recall    = round(m$macro_recall, 4),
    cv_accuracy     = round(lr_results[[nm]]$cv_accuracy, 4)
  )
}))

cat("\n=== Logistic Regression Scenario Comparison ===\n")
print(lr_comparison)

best_lr_name <- lr_comparison$scenario[which.max(lr_comparison$macro_f1)]
best_lr <- lr_results[[best_lr_name]]
cat(sprintf(
  "\nBest LR scenario: %s (Macro F1 = %.4f)\n",
  best_lr_name, max(lr_comparison$macro_f1)
))

# ---------------------------------------------------------------------------
# Visualization of logistic regression results
# ---------------------------------------------------------------------------

lr_plot_data <- melt(
  lr_comparison[, c("scenario", "accuracy", "macro_f1", "macro_precision", "macro_recall")],
  id.vars = "scenario",
  variable.name = "metric",
  value.name = "value"
)

lr_plot_1 <- ggplot(lr_plot_data, aes(x = scenario, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Logistic Regression scenario comparison",
    x = "Scenario", y = "Metric Value", fill = "Metric"
  ) +
  theme_minimal()

best_cm_df <- as.data.frame(best_lr$metrics$cm_table)

lr_plot_2 <- ggplot(best_cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 2.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = paste("Confusion Matrix ", best_lr_name),
    x = "True Class", y = "Predicted Class"
  )

lr_plot_3 <- ggplot(best_lr$metrics$per_class, aes(x = reorder(class, f1), y = f1)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = paste("F1 Score per Category ", best_lr_name),
    x = "Category", y = "F1 Score"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(lr_plot_1, "lr_plot_1")
save_plot(lr_plot_2, "lr_plot_2")
save_plot(lr_plot_3, "lr_plot_3")

# ---------------------------------------------------------------------------
# Save best model results to disk
# ---------------------------------------------------------------------------

saveRDS(
  list(
    metrics     = best_lr$metrics,
    predictions = best_lr$predictions,
    cv_accuracy = best_lr$cv_accuracy,
    scenario    = best_lr_name
  ),
  file = "models/lr_best_results.rds"
)

cat(sprintf("Saved results: models/lr_best_results.rds\n"))

spark_disconnect(sc)