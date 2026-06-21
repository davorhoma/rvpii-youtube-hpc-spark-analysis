library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

# ===========================================================================
# 4.2 Klasifikacija - Support Vector Machine (One-vs-Rest)
#
# Ciljno obeležje: category_name
# Prediktori: view_count, likes, comment_count, duration_seconds,
#             like_rate, comment_rate
# ===========================================================================

# ---------------------------------------------------------------------------
# Priprema podataka
# ---------------------------------------------------------------------------
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

print("Distribucija klasa:")
ml_data %>%
  count(category_name, sort = TRUE) %>%
  print()
print(paste("Ukupan broj zapisa:", sdf_nrow(ml_data)))

# ---------------------------------------------------------------------------
# 80/20 train/test split sa istim seed kao u LR fajlu radi konzistentnosti
# ---------------------------------------------------------------------------
splits <- sdf_random_split(ml_data, training = 0.8, test = 0.2, seed = 42L)
train_data <- splits$training
test_data <- splits$test

train_data <- sdf_persist(train_data, storage.level = "MEMORY_AND_DISK")
test_data <- sdf_persist(test_data, storage.level = "MEMORY_AND_DISK")

print(paste("Train skup:", sdf_nrow(train_data), "zapisa"))
print(paste("Test skup: ", sdf_nrow(test_data), "zapisa"))

predictors <- c(
  "view_count", "likes", "comment_count",
  "duration_seconds", "like_rate", "comment_rate"
)

# ---------------------------------------------------------------------------
# Pomocna funkcija za racunanje metrika (radi nad lokalnim data.frame-om)
# ---------------------------------------------------------------------------
compute_metrics <- function(predictions_local) {
  classes <- sort(unique(predictions_local$category_name))

  cm_table <- table(
    Prediction = predictions_local$predicted_label,
    Reference  = predictions_local$category_name
  )

  per_class <- lapply(classes, function(cls) {
    if (!(cls %in% rownames(cm_table))) {
      return(data.frame(class = cls, precision = 0, recall = 0, f1 = 0))
    }
    tp <- ifelse(cls %in% colnames(cm_table), cm_table[cls, cls], 0)
    fp <- sum(cm_table[cls, ]) - tp
    fn <- ifelse(cls %in% colnames(cm_table), sum(cm_table[, cls]), 0) - tp
    precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1 <- ifelse((precision + recall) == 0, 0,
      2 * precision * recall / (precision + recall)
    )
    data.frame(class = cls, precision = precision, recall = recall, f1 = f1)
  })
  per_class_df <- do.call(rbind, per_class)

  list(
    per_class = per_class_df,
    cm_table = cm_table,
    accuracy = mean(predictions_local$category_name ==
      predictions_local$predicted_label),
    macro_precision = mean(per_class_df$precision),
    macro_recall = mean(per_class_df$recall),
    macro_f1 = mean(per_class_df$f1)
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
  cat("\nMetrike po klasama:\n")
  print(metrics$per_class)
}

# ---------------------------------------------------------------------------
# Pomocna funkcija za izgradnju i evaluaciju SVM pipeline-a
#
# Pipeline:
#   ft_string_indexer  → category_name u numerički label
#   ft_vector_assembler → prediktori u features vektor
#   ft_standard_scaler  → standardizacija (SVM je osetljiv na skalu!)
#   ml_one_vs_rest      → OvR omotač oko ml_linear_svc
# ---------------------------------------------------------------------------
build_and_evaluate_svm <- function(train_sdf, test_sdf,
                                   reg_param, max_iter, scenario_name) {
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
      input_col = "features_raw",
      output_col = "features",
      with_mean = TRUE,
      with_std = TRUE
    ) %>%
    ml_one_vs_rest(
      classifier = ml_linear_svc(
        sc,
        features_col = "features",
        label_col    = "label",
        reg_param    = reg_param,
        max_iter     = max_iter
      ),
      features_col = "features",
      label_col = "label"
    )

  cat(sprintf("  Treniranje pipeline-a: %s ...\n", scenario_name))
  fitted_pipeline <- ml_fit(pipeline, train_sdf)

  predictions <- ml_transform(fitted_pipeline, test_sdf)

  # Mapiranje numeričkog labela → naziv kategorije
  # StringIndexerModel je prvi stage u pipeline-u
  string_indexer_model <- ml_stages(fitted_pipeline)[[1]]
  labels_metadata <- ml_labels(string_indexer_model)

  predictions_local <- predictions %>%
    select(category_name, prediction) %>%
    collect() %>%
    mutate(predicted_label = labels_metadata[as.integer(prediction) + 1])

  list(
    pipeline    = fitted_pipeline,
    predictions = predictions_local
  )
}

# ---------------------------------------------------------------------------
# Ručna k-fold unakrsna validacija za SVM
# ---------------------------------------------------------------------------
build_cv_svm <- function(reg_param, max_iter, train_sdf, k = 3) {
  fold_weights <- setNames(rep(1 / k, k), paste0("fold", seq_len(k)))
  folds <- sdf_random_split(train_sdf, weights = fold_weights, seed = 42L)
  folds <- lapply(
    folds,
    function(x) sdf_persist(x, storage.level = "MEMORY_AND_DISK")
  )

  invisible(lapply(folds, sdf_nrow))

  fold_accuracies <- numeric(k)

  for (i in seq_len(k)) {
    val_fold <- folds[[i]]
    train_fold <- do.call(sdf_bind_rows, folds[-i])

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
        input_col  = "features_raw",
        output_col = "features",
        with_mean  = TRUE,
        with_std   = TRUE
      ) %>%
      ml_one_vs_rest(
        classifier = ml_linear_svc(
          sc,
          features_col = "features",
          label_col    = "label",
          reg_param    = reg_param,
          max_iter     = max_iter
        ),
        features_col = "features",
        label_col = "label"
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

# ===========================================================================
# Scenariji hiperparametara
#
# reg_param kontroliše L2 regularizaciju težina SVM-a:
#   - manja vrednost → slabija regularizacija → model može bolje da se fituje,
#     ali je podložniji overfittingu
#   - veća vrednost → jača regularizacija → generalizacija, ali moguć underfitting
#
# max_iter kontroliše broj iteracija optimizacije:
#   - veći broj → potencijalno bolja konvergencija, ali sporije treniranje
#
# Scenario 1: slaba regularizacija (reg_param=0.001), standardan broj iteracija
# Scenario 2: jača regularizacija (reg_param=0.1),   standardan broj iteracija
# Scenario 3: slaba regularizacija (reg_param=0.001), više iteracija
# ===========================================================================
svm_scenarios <- list(
  list(name = "Scenario 1: reg=0.001, max_iter=5", reg_param = 0.001, max_iter = 5),
  list(name = "Scenario 2: reg=0.1,   max_iter=5", reg_param = 0.1, max_iter = 5),
  list(name = "Scenario 3: reg=0.001, max_iter=10", reg_param = 0.001, max_iter = 10)
)

svm_results <- list()

for (sc_params in svm_scenarios) {
  cat("\n--- Treniranje:", sc_params$name, "---\n")

  # 3-fold unakrsna validacija
  cv_accuracy <- build_cv_svm(
    sc_params$reg_param, sc_params$max_iter, train_data,
    k = 3
  )
  cat(sprintf("CV Accuracy (3-fold): %.4f\n", cv_accuracy))

  # Treniranje i evaluacija na test skupu
  result <- build_and_evaluate_svm(
    train_data, test_data,
    sc_params$reg_param, sc_params$max_iter,
    sc_params$name
  )

  metrics <- compute_metrics(result$predictions)
  print_metrics(metrics, sc_params$name, cv_accuracy)

  svm_results[[sc_params$name]] <- list(
    pipeline    = result$pipeline,
    predictions = result$predictions,
    metrics     = metrics,
    cv_accuracy = cv_accuracy,
    params      = sc_params
  )
}

# ---------------------------------------------------------------------------
# Poređenje scenarija i izbor najboljeg SVM modela
# ---------------------------------------------------------------------------
svm_comparison <- do.call(rbind, lapply(names(svm_results), function(nm) {
  m <- svm_results[[nm]]$metrics
  data.frame(
    scenario        = nm,
    accuracy        = round(m$accuracy, 4),
    macro_f1        = round(m$macro_f1, 4),
    macro_precision = round(m$macro_precision, 4),
    macro_recall    = round(m$macro_recall, 4),
    cv_accuracy     = round(svm_results[[nm]]$cv_accuracy, 4)
  )
}))

cat("\n=== Poređenje scenarija — SVM (One-vs-Rest) ===\n")
print(svm_comparison)

best_svm_name <- svm_comparison$scenario[which.max(svm_comparison$macro_f1)]
best_svm <- svm_results[[best_svm_name]]
cat(sprintf(
  "\nNajbolji scenario SVM: %s (Macro F1 = %.4f)\n",
  best_svm_name, max(svm_comparison$macro_f1)
))

# ---------------------------------------------------------------------------
# Vizualizacija rezultata
# ---------------------------------------------------------------------------

# Poređenje metrika po scenarijima
svm_plot_data <- melt(
  svm_comparison[, c(
    "scenario", "accuracy", "macro_f1",
    "macro_precision", "macro_recall"
  )],
  id.vars = "scenario",
  variable.name = "metrika",
  value.name = "vrednost"
)

svm_plot_1 <- ggplot(
  svm_plot_data,
  aes(x = scenario, y = vrednost, fill = metrika)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "SVM (One-vs-Rest) — poređenje scenarija",
    x     = "Scenario",
    y     = "Vrednost metrike",
    fill  = "Metrika"
  ) +
  theme_minimal()

# Matrica konfuzije najboljeg scenarija
best_cm_df <- as.data.frame(best_svm$metrics$cm_table)

svm_plot_2 <- ggplot(
  best_cm_df,
  aes(x = Reference, y = Prediction, fill = Freq)
) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 2.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = paste("Matrica konfuzije —", best_svm_name),
    x     = "Stvarna klasa",
    y     = "Predviđena klasa"
  )

# F1 po kategorijama za najbolji scenario
svm_plot_3 <- ggplot(
  best_svm$metrics$per_class,
  aes(x = reorder(class, f1), y = f1)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = paste("F1 po kategoriji —", best_svm_name),
    x     = "Kategorija",
    y     = "F1 skor"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(svm_plot_1, "svm_plot_1")
save_plot(svm_plot_2, "svm_plot_2")
save_plot(svm_plot_3, "svm_plot_3")

# ---------------------------------------------------------------------------
# Čuvanje najboljeg modela na disk
#
# saveRDS  → čuva R objekte (metrike, predikcije, naziv scenarija) kako bi
#             bili dostupni u comparison fajlu bez ponovnog treniranja
# ---------------------------------------------------------------------------

saveRDS(
  list(
    metrics     = best_svm$metrics,
    predictions = best_svm$predictions,
    cv_accuracy = best_svm$cv_accuracy,
    scenario    = best_svm_name
  ),
  file = "models/svm_best_results.rds"
)

cat(sprintf("Rezultati sačuvani: models/svm_best_results.rds\n"))