library(dplyr)
library(ggplot2)
library(sparklyr)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

print(colnames(youtube_data))

# ---------------------------------------------------------------------------
# 3. Exploratory Data Analysis
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 3.1 Descriptive Statistics
# Applied to: view_count, likes, comment_count, duration_seconds
# and derived features: like_rate, comment_rate
# ---------------------------------------------------------------------------

num_stats <- youtube_data %>%
  summarise(
    # view_count
    view_mean = mean(view_count, na.rm = TRUE),
    view_median = percentile_approx(view_count, 0.5),
    view_sd = sd(view_count, na.rm = TRUE),
    view_min = min(view_count, na.rm = TRUE),
    view_max = max(view_count, na.rm = TRUE),
    view_q1 = percentile_approx(view_count, 0.25),
    view_q3 = percentile_approx(view_count, 0.75),

    # likes
    likes_mean = mean(likes, na.rm = TRUE),
    likes_median = percentile_approx(likes, 0.5),
    likes_sd = sd(likes, na.rm = TRUE),
    likes_min = min(likes, na.rm = TRUE),
    likes_max = max(likes, na.rm = TRUE),
    likes_q1 = percentile_approx(likes, 0.25),
    likes_q3 = percentile_approx(likes, 0.75),

    # comment_count
    comments_mean = mean(comment_count, na.rm = TRUE),
    comments_median = percentile_approx(comment_count, 0.5),
    comments_sd = sd(comment_count, na.rm = TRUE),
    comments_min = min(comment_count, na.rm = TRUE),
    comments_max = max(comment_count, na.rm = TRUE),
    comments_q1 = percentile_approx(comment_count, 0.25),
    comments_q3 = percentile_approx(comment_count, 0.75),

    # duration_seconds
    duration_mean = mean(duration_seconds, na.rm = TRUE),
    duration_median = percentile_approx(duration_seconds, 0.5),
    duration_sd = sd(duration_seconds, na.rm = TRUE),
    duration_min = min(duration_seconds, na.rm = TRUE),
    duration_max = max(duration_seconds, na.rm = TRUE),
    duration_q1 = percentile_approx(duration_seconds, 0.25),
    duration_q3 = percentile_approx(duration_seconds, 0.75),

    # like_rate
    like_rate_mean = mean(like_rate, na.rm = TRUE),
    like_rate_median = percentile_approx(like_rate, 0.5),
    like_rate_sd = sd(like_rate, na.rm = TRUE),
    like_rate_min = min(like_rate, na.rm = TRUE),
    like_rate_max = max(like_rate, na.rm = TRUE),
    like_rate_q1 = percentile_approx(like_rate, 0.25),
    like_rate_q3 = percentile_approx(like_rate, 0.75),

    # comment_rate
    comment_rate_mean = mean(comment_rate, na.rm = TRUE),
    comment_rate_median = percentile_approx(comment_rate, 0.5),
    comment_rate_sd = sd(comment_rate, na.rm = TRUE),
    comment_rate_min = min(comment_rate, na.rm = TRUE),
    comment_rate_max = max(comment_rate, na.rm = TRUE),
    comment_rate_q1 = percentile_approx(comment_rate, 0.25),
    comment_rate_q3 = percentile_approx(comment_rate, 0.75)
  ) %>%
  collect()

print(num_stats)

# ---------------------------------------------------------------------------
# Collect data for local visualizations
# ---------------------------------------------------------------------------
youtube_sample <- youtube_data %>%
  select(
    view_count, likes, comment_count, duration_seconds,
    like_rate, comment_rate,
    category_name
  ) %>%
  collect()

# ---------------------------------------------------------------------------
# 3.2 Distribution Visualization
# ---------------------------------------------------------------------------

# --- Histograms (log scale) ------------------------------------------------

hist_1 <- ggplot(youtube_sample, aes(x = view_count)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of View Counts (log scale)",
    x = "View Count", y = "Number of Videos"
  )

hist_2 <- ggplot(youtube_sample, aes(x = likes)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of Likes (log scale)",
    x = "Like Count", y = "Number of Videos"
  )

hist_3 <- ggplot(youtube_sample, aes(x = comment_count)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of Comments (log scale)",
    x = "Comment Count", y = "Number of Videos"
  )

hist_4 <- ggplot(youtube_sample, aes(x = duration_seconds)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of Video Duration (log scale)",
    x = "Duration (seconds)", y = "Number of Videos"
  )

hist_5 <- ggplot(youtube_sample, aes(x = like_rate)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  scale_x_log10() +
  labs(
    title = "Distribution of Like Rate (log scale)",
    x = "Like Rate", y = "Number of Videos"
  )

hist_6 <- ggplot(youtube_sample, aes(x = comment_rate)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  scale_x_log10() +
  labs(
    title = "Distribution of Comment Rate (log scale)",
    x = "Comment rate", y = "Number of Videos"
  )

# --- Boxplots ---

boxplot_1 <- ggplot(youtube_sample, aes(y = view_count)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "View Count Boxplot", y = "View Count")

boxplot_2 <- ggplot(youtube_sample, aes(y = likes)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Likes Boxplot", y = "Like Count")

boxplot_3 <- ggplot(youtube_sample, aes(y = comment_count)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Comment Count Boxplot", y = "Comment Count")

boxplot_4 <- ggplot(youtube_sample, aes(y = duration_seconds)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10() +
  labs(title = "Video Duration Boxplot", y = "Duration (seconds)")

boxplot_5 <- ggplot(youtube_sample, aes(y = like_rate)) +
  geom_boxplot(fill = "darkorange") +
  scale_y_log10() +
  labs(title = "Like Rate Boxplot", y = "Like Rate")

boxplot_6 <- ggplot(youtube_sample, aes(y = comment_rate)) +
  geom_boxplot(fill = "darkorange") +
  scale_y_log10() +
  labs(title = "Comment Rate Boxplot", y = "Comment rate")

# --- Bar Plot - Category Distribution -------------------------------

category_counts <- youtube_data %>%
  count(category_name, sort = TRUE) %>%
  collect()

barplot_1 <- ggplot(category_counts, aes(x = reorder(category_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Videos by Category",
    x = "Category", y = "Number of Videos"
  )

# ---------------------------------------------------------------------------
# 3.3 Feature Relationship Analysis
# ---------------------------------------------------------------------------

# --- 3.3.1 Correlation Analysis of Numerical Variables ---

numeric_data <- youtube_sample %>%
  select(
    view_count, likes, comment_count, duration_seconds,
    like_rate, comment_rate
  )

cor_matrix <- cor(numeric_data, use = "complete.obs")

options(width = 200)
print(cor_matrix)

# Correlation matrix visualization
cor_long <- melt(cor_matrix)

cor_matrix_plot_1 <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0,
    limits = c(-1, 1), name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Numerical Variables", x = "", y = "")

# --- 3.3.2 Scatter Plot Analysis of Relationships Between Numerical Variables ---

scatter_plot_1 <- ggplot(numeric_data, aes(x = view_count, y = likes)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "View Count vs. Likes (log-log scale)",
    x = "View Count", y = "Like Count"
  )

scatter_plot_2 <- ggplot(numeric_data, aes(x = view_count, y = comment_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "View Count vs. Comment Count (log-log scale)",
    x = "View Count", y = "Comment Count"
  )

scatter_plot_3 <- ggplot(numeric_data, aes(x = likes, y = comment_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Likes vs. Comment Count (log-log scale)",
    x = "Like Count", y = "Comment Count"
  )

scatter_plot_4 <- ggplot(numeric_data, aes(x = duration_seconds, y = view_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Duration vs. View Count (log-log scale)",
    x = "Duration (seconds)", y = "View Count"
  )

# Scatter plots for derived features
scatter_plot_5 <- ggplot(numeric_data, aes(x = like_rate, y = comment_rate)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Like Rate vs. Comment Rate (log-log scale)",
    x = "Like Rate", y = "Comment Rate"
  )

# --- 3.3.3 Relationship Between Categorical and Numerical Variables ---

ratio_plot_1 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, view_count, median),
  y = view_count
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "View Count by Category",
    x = "Category", y = "View Count (log scale)"
  )

ratio_plot_2 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, likes, median),
  y = likes
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Likes by Category",
    x = "Category", y = "Like Count (log scale)"
  )

ratio_plot_3 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, comment_count, median),
  y = comment_count
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Comment Count by Category",
    x = "Category", y = "Comment Count (log scale)"
  )

ratio_plot_4 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, like_rate, median),
  y = like_rate
)) +
  geom_boxplot(fill = "darkorange", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Like Rate by Category",
    x = "Category", y = "Like Rate (log scale)"
  )

ratio_plot_5 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, comment_rate, median),
  y = comment_rate
)) +
  geom_boxplot(fill = "darkorange", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Comment Rate by Category",
    x = "Category", y = "Comment Rate (log scale)"
  )

ratio_plot_6 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, duration_seconds, median),
  y = duration_seconds
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Video Duration by Category",
    x = "Category", y = "Duration (seconds, log scale)"
  )

source("scripts/save_plot.R")
save_plot(hist_1, "hist_1")
save_plot(hist_2, "hist_2")
save_plot(hist_3, "hist_3")
save_plot(hist_4, "hist_4")
save_plot(hist_5, "hist_5")
save_plot(hist_6, "hist_6")

save_plot(boxplot_1, "boxplot_1")
save_plot(boxplot_2, "boxplot_2")
save_plot(boxplot_3, "boxplot_3")
save_plot(boxplot_4, "boxplot_4")
save_plot(boxplot_5, "boxplot_5")
save_plot(boxplot_6, "boxplot_6")

save_plot(barplot_1, "barplot_1")

save_plot(cor_matrix_plot_1, "cor_matrix_plot_1")

save_plot(scatter_plot_1, "scatter_plot_1")
save_plot(scatter_plot_2, "scatter_plot_2")
save_plot(scatter_plot_3, "scatter_plot_3")
save_plot(scatter_plot_4, "scatter_plot_4")
save_plot(scatter_plot_5, "scatter_plot_5")

save_plot(ratio_plot_1, "ratio_plot_1")
save_plot(ratio_plot_2, "ratio_plot_2")
save_plot(ratio_plot_3, "ratio_plot_3")
save_plot(ratio_plot_4, "ratio_plot_4")
save_plot(ratio_plot_5, "ratio_plot_5")
save_plot(ratio_plot_6, "ratio_plot_6")

spark_disconnect(sc)