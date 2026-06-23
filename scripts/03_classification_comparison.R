library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# ===========================================================================
# 4.5 Model Comparison — comparison of the best scenario
#     from each classification algorithm
#
# This script loads saved results from the models/ folder instead of retraining
# models. It assumes that the following scripts were previously executed:
#   - 03_classification_lr.R
#   - 03_classification_svc.R
#   - 03_classification_rf.R
# which save results into models/lr_best_results.rds, etc.
# ===========================================================================

# ---------------------------------------------------------------------------
# Load saved results
# ---------------------------------------------------------------------------
lr_saved  <- readRDS("models/lr_best_results.rds")
svm_saved <- readRDS("models/svm_best_results.rds")
rf_saved  <- readRDS("models/rf_best_results.rds")

best_lr_name  <- lr_saved$scenario
best_svm_name <- svm_saved$scenario
best_rf_name  <- rf_saved$scenario

cat("Loaded models:\n")
cat(sprintf("  LR:  %s\n", best_lr_name))
cat(sprintf("  SVM: %s\n", best_svm_name))
cat(sprintf("  RF:  %s\n", best_rf_name))

# ---------------------------------------------------------------------------
# Comparison table — one row per algorithm (best scenario per model)
# ---------------------------------------------------------------------------
comparison_table <- data.frame(
  algorithm      = c("Logistic Regression", "SVM (One-vs-Rest)", "Random Forest"),
  scenario       = c(best_lr_name, best_svm_name, best_rf_name),
  accuracy       = round(c(
    lr_saved$metrics$accuracy,
    svm_saved$metrics$accuracy,
    rf_saved$metrics$accuracy
  ), 4),
  macro_precision = round(c(
    lr_saved$metrics$macro_precision,
    svm_saved$metrics$macro_precision,
    rf_saved$metrics$macro_precision
  ), 4),
  macro_recall    = round(c(
    lr_saved$metrics$macro_recall,
    svm_saved$metrics$macro_recall,
    rf_saved$metrics$macro_recall
  ), 4),
  macro_f1        = round(c(
    lr_saved$metrics$macro_f1,
    svm_saved$metrics$macro_f1,
    rf_saved$metrics$macro_f1
  ), 4),
  cv_accuracy     = round(c(
    lr_saved$cv_accuracy,
    svm_saved$cv_accuracy,
    rf_saved$cv_accuracy
  ), 4)
)

cat("\n=== Comparison of best scenario per algorithm ===\n")
print(comparison_table)

# ---------------------------------------------------------------------------
# Identify overall best model (by Macro F1)
# ---------------------------------------------------------------------------
best_overall_idx  <- which.max(comparison_table$macro_f1)
best_overall_name <- comparison_table$algorithm[best_overall_idx]

cat(sprintf(
  "\nOverall best model: %s (Macro F1 = %.4f, Accuracy = %.4f)\n",
  best_overall_name,
  comparison_table$macro_f1[best_overall_idx],
  comparison_table$accuracy[best_overall_idx]
))

# ---------------------------------------------------------------------------
# Stability analysis: difference between CV accuracy and test accuracy
#
# Large gap (CV >> test) indicates overfitting.
# Small gap indicates good generalization.
# ---------------------------------------------------------------------------
comparison_table$generalization <- round(
  comparison_table$cv_accuracy - comparison_table$accuracy, 4
)

cat("\nGeneralization analysis (CV accuracy - test accuracy):\n")
print(comparison_table[, c("algorithm", "cv_accuracy", "accuracy", "generalization")])

# ---------------------------------------------------------------------------
# Visualization 1: Metric comparison by algorithm
# ---------------------------------------------------------------------------
cmp_plot_data <- melt(
  comparison_table[, c(
    "algorithm", "accuracy", "macro_f1",
    "macro_precision", "macro_recall"
  )],
  id.vars = "algorithm",
  variable.name = "metric",
  value.name = "value"
)

cmp_plot_1 <- ggplot(
  cmp_plot_data,
  aes(x = algorithm, y = value, fill = metric)
) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  labs(
    title = "Algorithm Comparison - Key Metrics (Best Scenario)",
    x     = "Algorithm",
    y     = "Metric Value",
    fill  = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# ---------------------------------------------------------------------------
# Visualization 2: CV accuracy vs test accuracy (model stability)
# ---------------------------------------------------------------------------
stability_data <- melt(
  comparison_table[, c("algorithm", "cv_accuracy", "accuracy")],
  id.vars = "algorithm",
  variable.name = "type",
  value.name = "value"
)

stability_data$type <- ifelse(
  stability_data$type == "cv_accuracy", "CV Accuracy", "Test Accuracy"
)

cmp_plot_2 <- ggplot(
  stability_data,
  aes(x = algorithm, y = value, fill = type)
) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  labs(
    title = "Model Stability - CV Accuracy vs Test Accuracy",
    x     = "Algorithm",
    y     = "Accuracy",
    fill  = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# ---------------------------------------------------------------------------
# Visualization 3: F1 per category — all three algorithms compared
#
# Combine per_class data.frames from all best models to see which algorithm
# performs better for each category.
# ---------------------------------------------------------------------------
per_class_combined <- rbind(
  cbind(lr_saved$metrics$per_class,  algorithm = "Logistic Regression"),
  cbind(svm_saved$metrics$per_class, algorithm = "SVM (One-vs-Rest)"),
  cbind(rf_saved$metrics$per_class,  algorithm = "Random Forest")
)

cmp_plot_3 <- ggplot(
  per_class_combined,
  aes(x = reorder(class, f1), y = f1, fill = algorithm)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "F1 Score per Category - Algorithm Comparison",
    x     = "Category",
    y     = "F1 Score",
    fill  = "Algorithm"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(cmp_plot_1, "cmp_plot_1")
save_plot(cmp_plot_2, "cmp_plot_2")
save_plot(cmp_plot_3, "cmp_plot_3", width = 10, height = 7)