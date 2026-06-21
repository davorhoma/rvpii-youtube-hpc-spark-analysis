library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

# ===========================================================================
# 4.3 Klasifikacija — Random Forest
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
# 80/20 train/test split sa istim seed kao u LR i SVM fajlovima radi konzistentnosti
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
    accuracy = mean(predictions_local$category_name == predictions_local$predicted_label),
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
# Pomocna funkcija za izgradnju i evaluaciju RF pipeline-a
#
# Pipeline:
#   ft_string_indexer   → category_name u numerički label
#   ft_vector_assembler → prediktori u features vektor
#   ml_random_forest_classifier → RF (skaliranje NIJE potrebno za stabla)
#
# Hiperparametri:
#   num_trees   — broj stabala u ansamblu; više stabala → stabilnija predikcija,
#                 ali sporije treniranje
#   max_depth   — maksimalna dubina svakog stabla; veća dubina → kompleksniji model,
#                 podložniji overfittingu
#   min_instances_per_node — minimalan broj instanci u listu; veća vrednost →
#                 generalizacija, smanjuje rizik od overfittinga
# ---------------------------------------------------------------------------
build_and_evaluate_rf <- function(train_sdf, test_sdf,
                                  num_trees, max_depth,
                                  min_instances_per_node,
                                  max_bins = 32,
                                  scenario_name) {
  pipeline <- ml_pipeline(sc) %>%
    ft_string_indexer(
      input_col  = "category_name",
      output_col = "label"
    ) %>%
    ft_vector_assembler(
      input_cols = predictors,
      output_col = "features"
    ) %>%
    ml_random_forest_classifier(
      features_col            = "features",
      label_col               = "label",
      num_trees               = num_trees,
      max_depth               = max_depth,
      min_instances_per_node  = min_instances_per_node,
      max_bins                = max_bins,
      seed                    = 42L
    )

  cat(sprintf("  Treniranje pipeline-a: %s ...\n", scenario_name))
  fitted_pipeline <- ml_fit(pipeline, train_sdf)

  predictions <- ml_transform(fitted_pipeline, test_sdf)

  # Mapiranje numeričkog labela → naziv kategorije
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
# Ručna k-fold unakrsna validacija za Random Forest
# ---------------------------------------------------------------------------
build_cv_rf <- function(num_trees, max_depth, min_instances_per_node,
                        train_sdf, k = 3, max_bins = 32) {
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
        output_col = "features"
      ) %>%
      ml_random_forest_classifier(
        features_col           = "features",
        label_col              = "label",
        num_trees              = num_trees,
        max_depth              = max_depth,
        min_instances_per_node = min_instances_per_node,
        max_bins               = max_bins,
        seed                   = 42L
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
# num_trees kontroliše veličinu ansambla:
#   - manji broj → brže treniranje, veća varijansa predikcija
#   - veći broj → stabilnija predikcija, sporije treniranje
#
# max_depth kontroliše kompleksnost individualnih stabala:
#   - manja dubina → jednostavniji model, manji rizik od overfittinga
#   - veća dubina → kompleksniji model, veća sposobnost fitovanja, ali
#                   podložniji overfittingu
#
# min_instances_per_node kontroliše granularnost listova:
#   - veća vrednost → listoviobuhvataju više instanci → generalizacija
#   - manja vrednost → finiji listovi → potencijalni overfit
#
# Scenario 1: manji ansambl, srednja dubina           → osnovna linija
# Scenario 2: veći ansambl, srednja dubina            → više stabala, stabilnija predikcija
# Scenario 3: veći ansambl, veća dubina, manji listovi → kompleksniji model
# ===========================================================================
rf_scenarios <- list(
  list(
    name                   = "Scenario 1: num_trees=50,  max_depth=5, min_inst=1",
    num_trees              = 50,
    max_depth              = 5,
    min_instances_per_node = 1,
    max_bins               = 32
  ),
  list(
    name                   = "Scenario 2: num_trees=100, max_depth=5, min_inst=1",
    num_trees              = 100,
    max_depth              = 5,
    min_instances_per_node = 1,
    max_bins               = 32
  ),
  list(
    name                   = "Scenario 3: num_trees=100, max_depth=8, min_inst=5, max_bins=16",
    num_trees              = 50,
    max_depth              = 8,
    min_instances_per_node = 5,
    max_bins               = 16
  )
)

rf_results <- list()

for (sc_params in rf_scenarios) {
  cat("\n--- Treniranje:", sc_params$name, "---\n")

  # 3-fold unakrsna validacija
  cv_accuracy <- build_cv_rf(
    sc_params$num_trees,
    sc_params$max_depth,
    sc_params$min_instances_per_node,
    train_data,
    k = 3,
    max_bins = sc_params$max_bins
  )
  cat(sprintf("CV Accuracy (3-fold): %.4f\n", cv_accuracy))

  # Treniranje i evaluacija na test skupu
  result <- build_and_evaluate_rf(
    train_data, test_data,
    sc_params$num_trees,
    sc_params$max_depth,
    sc_params$min_instances_per_node,
    max_bins = sc_params$max_bins,
    scenario_name = sc_params$name
  )

  metrics <- compute_metrics(result$predictions)
  print_metrics(metrics, sc_params$name, cv_accuracy)

  rf_results[[sc_params$name]] <- list(
    pipeline    = result$pipeline,
    predictions = result$predictions,
    metrics     = metrics,
    cv_accuracy = cv_accuracy,
    params      = sc_params
  )
}

# ---------------------------------------------------------------------------
# Poređenje scenarija i izbor najboljeg RF modela
# ---------------------------------------------------------------------------
rf_comparison <- do.call(rbind, lapply(names(rf_results), function(nm) {
  m <- rf_results[[nm]]$metrics
  data.frame(
    scenario        = nm,
    accuracy        = round(m$accuracy, 4),
    macro_f1        = round(m$macro_f1, 4),
    macro_precision = round(m$macro_precision, 4),
    macro_recall    = round(m$macro_recall, 4),
    cv_accuracy     = round(rf_results[[nm]]$cv_accuracy, 4)
  )
}))

cat("\n=== Poređenje scenarija — Random Forest ===\n")
print(rf_comparison)

best_rf_name <- rf_comparison$scenario[which.max(rf_comparison$macro_f1)]
best_rf <- rf_results[[best_rf_name]]
cat(sprintf(
  "\nNajbolji scenario RF: %s (Macro F1 = %.4f)\n",
  best_rf_name, max(rf_comparison$macro_f1)
))

# ---------------------------------------------------------------------------
# Važnost obeležja (feature importance) najboljeg modela
# ---------------------------------------------------------------------------
best_rf_model <- ml_stages(best_rf$pipeline)[[3]] # treći stage: RF klasifikator
feature_importances <- ml_feature_importances(best_rf_model)

predictors <- c(
  "view_count",
  "likes",
  "comment_count",
  "duration_seconds",
  "like_rate",
  "comment_rate"
)

fi_df <- data.frame(
  feature = predictors,
  importance = feature_importances
) %>%
  arrange(desc(importance))

cat("\nVažnost obeležja (najbolji scenario):\n")
print(fi_df)

# ---------------------------------------------------------------------------
# Vizualizacija rezultata
# ---------------------------------------------------------------------------

# Poređenje metrika po scenarijima
rf_plot_data <- melt(
  rf_comparison[, c(
    "scenario", "accuracy", "macro_f1",
    "macro_precision", "macro_recall"
  )],
  id.vars = "scenario",
  variable.name = "metrika",
  value.name = "vrednost"
)

rf_plot_1 <- ggplot(
  rf_plot_data,
  aes(x = scenario, y = vrednost, fill = metrika)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Random Forest — poređenje scenarija",
    x     = "Scenario",
    y     = "Vrednost metrike",
    fill  = "Metrika"
  ) +
  theme_minimal()

# Matrica konfuzije najboljeg scenarija
best_cm_df <- as.data.frame(best_rf$metrics$cm_table)

rf_plot_2 <- ggplot(
  best_cm_df,
  aes(x = Reference, y = Prediction, fill = Freq)
) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 2.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = paste("Matrica konfuzije —", best_rf_name),
    x     = "Stvarna klasa",
    y     = "Predviđena klasa"
  )

# F1 po kategorijama za najbolji scenario
rf_plot_3 <- ggplot(
  best_rf$metrics$per_class,
  aes(x = reorder(class, f1), y = f1)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = paste("F1 po kategoriji —", best_rf_name),
    x     = "Kategorija",
    y     = "F1 skor"
  ) +
  theme_minimal()

# Važnost obeležja
rf_plot_4 <- ggplot(
  fi_df,
  aes(x = reorder(feature, importance), y = importance)
) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = paste("Važnost obeležja —", best_rf_name),
    x     = "Obeležje",
    y     = "Važnost (Gini)"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(rf_plot_1, "rf_plot_1")
save_plot(rf_plot_2, "rf_plot_2")
save_plot(rf_plot_3, "rf_plot_3")
save_plot(rf_plot_4, "rf_plot_4", width = 10)

# ---------------------------------------------------------------------------
# Čuvanje rezultata najboljeg modela na disk
#
# saveRDS čuva R objekte (metrike, predikcije, naziv scenarija) kako bi
# bili dostupni u comparison fajlu bez ponovnog treniranja.
#
# ---------------------------------------------------------------------------
saveRDS(
  list(
    metrics     = best_rf$metrics,
    predictions = best_rf$predictions,
    cv_accuracy = best_rf$cv_accuracy,
    scenario    = best_rf_name
  ),
  file = "models/rf_best_results.rds"
)

cat(sprintf("Rezultati sačuvani: models/rf_best_results.rds\n"))
