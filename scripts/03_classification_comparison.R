library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# ===========================================================================
# 4.5 Poređenje modela — međusobno poređenje najboljeg scenarija
#     svakog klasifikacionog algoritma
#
# Ovaj fajl učitava sačuvane rezultate iz models/ foldera umesto da ponovo
# trenira modele. Uslov je da su prethodno pokrenuti:
#   - 03_classification_lr.R
#   - 03_classification_svc.R
#   - 03_classification_rf.R
# koji čuvaju rezultate u models/lr_best_results.rds,...
# ===========================================================================

# ---------------------------------------------------------------------------
# Učitavanje sačuvanih rezultata
# ---------------------------------------------------------------------------
lr_saved  <- readRDS("models/lr_best_results.rds")
svm_saved <- readRDS("models/svm_best_results.rds")
rf_saved  <- readRDS("models/rf_best_results.rds")

best_lr_name  <- lr_saved$scenario
best_svm_name <- svm_saved$scenario
best_rf_name  <- rf_saved$scenario

cat("Učitani modeli:\n")
cat(sprintf("  LR:  %s\n", best_lr_name))
cat(sprintf("  SVM: %s\n", best_svm_name))
cat(sprintf("  RF:  %s\n", best_rf_name))

# ---------------------------------------------------------------------------
# Tabela poređenja — jedan red po algoritmu (najbolji scenario svakog)
# ---------------------------------------------------------------------------
comparison_table <- data.frame(
  algoritam       = c("Logistička regresija", "SVM (One-vs-Rest)", "Random Forest"),
  scenario        = c(best_lr_name, best_svm_name, best_rf_name),
  accuracy        = round(c(
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

cat("\n=== Poređenje najboljeg scenarija svakog algoritma ===\n")
print(comparison_table)

# ---------------------------------------------------------------------------
# Identifikacija ukupno najboljeg modela (po Macro F1)
# ---------------------------------------------------------------------------
best_overall_idx  <- which.max(comparison_table$macro_f1)
best_overall_name <- comparison_table$algoritam[best_overall_idx]

cat(sprintf(
  "\nUkupno najbolji model: %s (Macro F1 = %.4f, Accuracy = %.4f)\n",
  best_overall_name,
  comparison_table$macro_f1[best_overall_idx],
  comparison_table$accuracy[best_overall_idx]
))

# ---------------------------------------------------------------------------
# Analiza stabilnosti: razlika između CV accuracy i test accuracy
#
# Velika razlika (CV >> test) signalizuje overfit.
# Mala razlika potvrđuje da model dobro generalizuje.
# ---------------------------------------------------------------------------
comparison_table$generalizacija <- round(
  comparison_table$cv_accuracy - comparison_table$accuracy, 4
)

cat("\nAnaliza generalizacije (CV accuracy − test accuracy):\n")
print(comparison_table[, c("algoritam", "cv_accuracy", "accuracy", "generalizacija")])

# ---------------------------------------------------------------------------
# Vizualizacija 1: Poređenje metrika po algoritmu
# ---------------------------------------------------------------------------
cmp_plot_data <- melt(
  comparison_table[, c(
    "algoritam", "accuracy", "macro_f1",
    "macro_precision", "macro_recall"
  )],
  id.vars      = "algoritam",
  variable.name = "metrika",
  value.name    = "vrednost"
)

cmp_plot_1 <- ggplot(
  cmp_plot_data,
  aes(x = algoritam, y = vrednost, fill = metrika)
) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  labs(
    title = "Poređenje algoritama — ključne metrike (najbolji scenario)",
    x     = "Algoritam",
    y     = "Vrednost metrike",
    fill  = "Metrika"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# ---------------------------------------------------------------------------
# Vizualizacija 2: CV accuracy vs. test accuracy (stabilnost modela)
# ---------------------------------------------------------------------------
stability_data <- melt(
  comparison_table[, c("algoritam", "cv_accuracy", "accuracy")],
  id.vars      = "algoritam",
  variable.name = "tip",
  value.name    = "vrednost"
)
stability_data$tip <- ifelse(
  stability_data$tip == "cv_accuracy", "CV Accuracy", "Test Accuracy"
)

cmp_plot_2 <- ggplot(
  stability_data,
  aes(x = algoritam, y = vrednost, fill = tip)
) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  labs(
    title = "Stabilnost modela — CV accuracy vs. test accuracy",
    x     = "Algoritam",
    y     = "Accuracy",
    fill  = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# ---------------------------------------------------------------------------
# Vizualizacija 3: F1 po kategoriji — sva tri algoritma uporedo
#
# Spajaju se per_class data.frame-ovi sva tri najbolja scenarija kako bi se
# videlo za koje kategorije koji algoritam bolje radi.
# ---------------------------------------------------------------------------
per_class_combined <- rbind(
  cbind(lr_saved$metrics$per_class,  algoritam = "Logistička regresija"),
  cbind(svm_saved$metrics$per_class, algoritam = "SVM (One-vs-Rest)"),
  cbind(rf_saved$metrics$per_class,  algoritam = "Random Forest")
)

cmp_plot_3 <- ggplot(
  per_class_combined,
  aes(x = reorder(class, f1), y = f1, fill = algoritam)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "F1 po kategoriji — poređenje algoritama",
    x     = "Kategorija",
    y     = "F1 skor",
    fill  = "Algoritam"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(cmp_plot_1, "cmp_plot_1")
save_plot(cmp_plot_2, "cmp_plot_2")
save_plot(cmp_plot_3, "cmp_plot_3", width = 10, height = 7)