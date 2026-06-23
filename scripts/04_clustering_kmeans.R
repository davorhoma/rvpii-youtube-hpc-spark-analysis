library(sparklyr)
library(dplyr)
library(ggplot2)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

# ===========================================================================
# 5. Clustering — K-means
# ===========================================================================

# ---------------------------------------------------------------------------
# Data preparation
# ---------------------------------------------------------------------------
cluster_data <- youtube_data %>%
  filter(
    !is.na(view_count),
    !is.na(likes),
    !is.na(comment_count),
    !is.na(duration_seconds),
    !is.na(like_rate),
    !is.na(comment_rate),
    !is.na(trending_count)
  )

cluster_data <- sdf_persist(cluster_data, storage.level = "MEMORY_AND_DISK")

print(paste("Total number of records for clustering:", sdf_nrow(cluster_data)))

# Two feature sets:
#   predictors_basic    - same feature set as in classification
#   predictors_extended - adds trending_count, which measures how long
#                         a video remained popular and may separate clusters
#                         of short-term and long-term trending videos
predictors_basic <- c(
  "view_count", "likes", "comment_count",
  "duration_seconds", "like_rate", "comment_rate"
)
predictors_extended <- c(
  "view_count", "likes", "comment_count",
  "duration_seconds", "like_rate", "comment_rate",
  "trending_count"
)

# ---------------------------------------------------------------------------
# Scenario definitions
#
# k=4  — smaller number of clusters; expected coarse groups by popularity
#         (e.g. viral, popular, average, niche videos)
# k=6  — larger number of clusters; finer partitioning that may reveal
#         differences within popular categories (short vs. long content,
#         high vs. low comment rate, ...)
# ---------------------------------------------------------------------------
kmeans_scenarios <- list(
  list(
    name       = "Scenario A: k=4, basic features",
    k          = 4,
    predictors = predictors_basic,
    pred_label = "basic"
  ),
  list(
    name       = "Scenario B: k=4, extended features",
    k          = 4,
    predictors = predictors_extended,
    pred_label = "extended"
  ),
  list(
    name       = "Scenario C: k=6, basic features",
    k          = 6,
    predictors = predictors_basic,
    pred_label = "basic"
  ),
  list(
    name       = "Scenario D: k=6, extended features",
    k          = 6,
    predictors = predictors_extended,
    pred_label = "extended"
  )
)

# ---------------------------------------------------------------------------
# Helper function: build and evaluate K-means model
#
# Pipeline:
#   ft_vector_assembler → combines attributes into a feature vector
#   ft_standard_scaler  → standardization is required for K-means because
#                         the algorithm relies on Euclidean distance.
#                         Features with larger ranges (view_count ~ 10^6)
#                         would dominate features with smaller ranges
#                         (like_rate ~ 0.01) without standardization
#   ml_kmeans           → K-means clustering
#
# Returns: fitted model, predictions, within-cluster sum of squares (WCSS)
# ---------------------------------------------------------------------------
build_kmeans <- function(train_sdf, predictors, k, seed = 42L) {
  # Step 1: assembler + scaler
  prep_pipeline <- ml_pipeline(sc) %>%
    ft_vector_assembler(
      input_cols = predictors,
      output_col = "features_raw"
    ) %>%
    ft_standard_scaler(
      input_col  = "features_raw",
      output_col = "features_scaled",
      with_mean  = TRUE,
      with_std   = TRUE
    )
  # ft_min_max_scaler(
  #   input_col  = "features_raw",
  #   output_col = "features_scaled"
  # )

  prep_fitted <- ml_fit(prep_pipeline, train_sdf)
  scaled_sdf <- ml_transform(prep_fitted, train_sdf)

  # Step 2: K-means on standardized features
  kmeans_model <- ml_kmeans(scaled_sdf,
    formula    = ~features_scaled,
    k          = k,
    seed       = seed,
    max_iter   = 100L,
    init_steps = 5L
  )

  predictions <- ml_predict(kmeans_model, scaled_sdf)

  wcss <- ml_summary(kmeans_model)$training_cost

  list(
    pipeline    = kmeans_model,
    predictions = predictions,
    wcss        = wcss,
    k           = k
  )
}

# ---------------------------------------------------------------------------
# Helper function: cluster structure analysis
# ---------------------------------------------------------------------------
analyze_clusters <- function(predictions_sdf, predictors, k) {
  # Number of instances per cluster
  cluster_sizes <- predictions_sdf %>%
    count(prediction) %>%
    arrange(prediction) %>%
    collect()

  # Mean values of original features per cluster
  cluster_means <- predictions_sdf %>%
    group_by(prediction) %>%
    summarise(
      across(all_of(predictors), mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(prediction) %>%
    collect()

  list(
    cluster_sizes = cluster_sizes,
    cluster_means = cluster_means
  )
}

# ---------------------------------------------------------------------------
# Elbow method — determining the optimal k for the basic feature set
# ---------------------------------------------------------------------------
cat("\n=== Elbow method (basic feature set) ===\n")

elbow_k_values <- 2:10
elbow_wcss <- numeric(length(elbow_k_values))

for (i in seq_along(elbow_k_values)) {
  k_val <- elbow_k_values[i]
  cat(sprintf("  Training K-means for k=%d ...\n", k_val))

  result <- build_kmeans(cluster_data, predictors_basic, k = k_val)
  elbow_wcss[i] <- result$wcss
  cat(sprintf("  k=%d, WCSS=%.2f\n", k_val, result$wcss))

  rm(result)
  gc()
}

elbow_df <- data.frame(k = elbow_k_values, wcss = elbow_wcss)
cat("\nElbow table:\n")
print(elbow_df)

elbow_plot <- ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = elbow_k_values) +
  labs(
    title = "Elbow Method - Determining Optimal k",
    x     = "Number of clusters (k)",
    y     = "WCSS (within-cluster sum of squares)"
  ) +
  theme_minimal()

source("scripts/save_plot.R")
save_plot(elbow_plot, "kmeans_elbow_standard_scaler_k_4_6")

# ===========================================================================
# Run all scenarios
# ===========================================================================
kmeans_results <- list()

for (sc_params in kmeans_scenarios) {
  result_key <- paste0(sc_params$pred_label, "_k", sc_params$k)

  cat(sprintf("\n--- %s ---\n", sc_params$name))

  result <- build_kmeans(
    cluster_data,
    sc_params$predictors,
    k = sc_params$k
  )

  analysis <- analyze_clusters(
    result$predictions,
    sc_params$predictors,
    sc_params$k
  )

  cat(sprintf("WCSS: %.2f\n", result$wcss))
  cat("Cluster sizes:\n")
  print(analysis$cluster_sizes)
  cat("Cluster mean values:\n")
  print(analysis$cluster_means)

  kmeans_results[[result_key]] <- list(
    params    = sc_params,
    full_name = sc_params$name,
    result    = result,
    analysis  = analysis
  )

  rm(result)
  gc()
}

# ---------------------------------------------------------------------------
# 5.3 Overview of WCSS values for all combinations
# ---------------------------------------------------------------------------
wcss_summary <- do.call(rbind, lapply(names(kmeans_results), function(nm) {
  r <- kmeans_results[[nm]]
  data.frame(
    kljuc    = nm,
    scenario = r$params$name,
    k        = r$params$k,
    atributi = r$params$pred_label,
    wcss     = round(r$result$wcss, 2)
  )
}))

cat("\n=== WCSS overview across all scenarios ===\n")
print(wcss_summary)

# ===========================================================================
# 5.4 Visualizations
# ===========================================================================

# ---------------------------------------------------------------------------
# Visualization 1: WCSS comparison across scenarios
# ---------------------------------------------------------------------------
wcss_plot <- ggplot(
  wcss_summary,
  aes(x = scenario, y = wcss, fill = atributi)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "WCSS Comparison Across Scenarios",
    x     = "Scenario",
    y     = "WCSS",
    fill  = "Feature set"
  ) +
  theme_minimal()

save_plot(wcss_plot, "kmeans_wcss_comparison_standard_scaler_k_4_6", width = 10, height = 5)

# ---------------------------------------------------------------------------
# Visualization 2–5: Cluster scatter plots in 2D space
#
# Sampling is used for performance reasons
# (scatter plots with millions of points are impractical).
# ---------------------------------------------------------------------------
plot_clusters_2d <- function(result_obj, x_col, y_col,
                             x_label, y_label, title, filename) {
  sample_local <- result_obj$result$predictions %>%
    select(all_of(c(x_col, y_col, "prediction"))) %>%
    sdf_sample(fraction = 0.05, seed = 42L) %>%
    collect() %>%
    mutate(cluster = factor(prediction))

  p <- ggplot(
    sample_local,
    aes(x = .data[[x_col]], y = .data[[y_col]], color = cluster)
  ) +
    geom_point(alpha = 0.4, size = 1) +
    labs(
      title = title,
      x     = x_label,
      y     = y_label,
      color = "Cluster"
    ) +
    theme_minimal()

  save_plot(p, filename)
  p
}

for (sc_params in kmeans_scenarios) {
  key <- paste0(sc_params$pred_label, "_k", sc_params$k)
  filename <- paste0("kmeans_scatter_", key, "_sc")
  title <- paste(
    "Clusters", sc_params$name,
    "\n(view_count vs. like_rate, 5% sample)"
  )

  plot_clusters_2d(
    kmeans_results[[key]],
    x_col = "view_count",
    y_col = "like_rate",
    x_label = "View count",
    y_label = "Like rate",
    title = title,
    filename = filename
  )
}

# ---------------------------------------------------------------------------
# Visualization 6–9: Mean feature values per cluster (heatmap)
#
# Heatmaps of normalized means help interpret clusters:
# which clusters exhibit high/low values for specific features.
# ---------------------------------------------------------------------------
for (sc_params in kmeans_scenarios) {
  key <- paste0(sc_params$pred_label, "_k", sc_params$k)
  means_df <- kmeans_results[[key]]$analysis$cluster_means

  # Column-wise normalization to enable comparison between features
  means_scaled <- means_df
  for (col in sc_params$predictors) {
    col_vals <- means_df[[col]]
    col_range <- max(col_vals) - min(col_vals)
    means_scaled[[col]] <- if (col_range > 0) {
      (col_vals - min(col_vals)) / col_range
    } else {
      rep(0, length(col_vals))
    }
  }

  heatmap_data <- melt(
    means_scaled[, c("prediction", sc_params$predictors)],
    id.vars       = "prediction",
    variable.name = "feature",
    value.name    = "value"
  )
  heatmap_data$cluster <- factor(heatmap_data$prediction)

  p <- ggplot(
    heatmap_data,
    aes(x = feature, y = cluster, fill = value)
  ) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
    labs(
      title = paste("Cluster profile", sc_params$name),
      x     = "Feature",
      y     = "Cluster",
      fill  = "Normalized value"
    )

  save_plot(p, paste0("kmeans_heatmap_", key, "_sc"), width = 9, height = 5)
}

# ---------------------------------------------------------------------------
# Save analysis results to disk
# ---------------------------------------------------------------------------
saveRDS(
  list(
    elbow_df     = elbow_df,
    wcss_summary = wcss_summary,
    analyses     = lapply(kmeans_results, function(r) r$analysis)
  ),
  file = "models/kmeans_results.rds"
)

cat("\nClustering results saved: models/kmeans_results.rds\n")

spark_disconnect(sc)

rm(list = ls())
gc()