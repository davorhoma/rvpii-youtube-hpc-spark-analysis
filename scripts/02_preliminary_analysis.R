library(dplyr)
library(ggplot2)
library(sparklyr)
library(reshape2)

source("scripts/01_prepare_data.R")

sc <- spark_connect(master = "local[*]")
youtube_data <- load_youtube_data(sc)

print(colnames(youtube_data))

# ---------------------------------------------------------------------------
# 3. Preliminarna analiza podataka
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 3.1 Deskriptivna statistika
# Primenjuje se na: view_count, likes, comment_count, duration_seconds
# i izvedene atribute: like_rate, comment_rate
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
# Prikupljanje podataka za lokalne vizualizacije
# ---------------------------------------------------------------------------
youtube_sample <- youtube_data %>%
  select(
    view_count, likes, comment_count, duration_seconds,
    like_rate, comment_rate,
    category_name
  ) %>%
  collect()

# ---------------------------------------------------------------------------
# 3.2 Vizualizacija distribucija
# ---------------------------------------------------------------------------

# --- Histogrami (log skala) ------------------------------------------------

hist_1 <- ggplot(youtube_sample, aes(x = view_count)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Raspodela broja pregleda (log skala)",
    x = "Broj pregleda", y = "Broj video zapisa"
  )

hist_2 <- ggplot(youtube_sample, aes(x = likes)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Raspodela broja lajkova (log skala)",
    x = "Broj lajkova", y = "Broj video zapisa"
  )

hist_3 <- ggplot(youtube_sample, aes(x = comment_count)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Raspodela broja komentara (log skala)",
    x = "Broj komentara", y = "Broj video zapisa"
  )

hist_4 <- ggplot(youtube_sample, aes(x = duration_seconds)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Raspodela trajanja video zapisa (log skala)",
    x = "Trajanje (sekunde)", y = "Broj video zapisa"
  )

hist_5 <- ggplot(youtube_sample, aes(x = like_rate)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  scale_x_log10() +
  labs(
    title = "Raspodela stope lajkova (log skala)",
    x = "Like rate", y = "Broj video zapisa"
  )

hist_6 <- ggplot(youtube_sample, aes(x = comment_rate)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  scale_x_log10() +
  labs(
    title = "Raspodela stope komentara (log skala)",
    x = "Comment rate", y = "Broj video zapisa"
  )

# --- Boxplotovi ---

boxplot_1 <- ggplot(youtube_sample, aes(y = view_count)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot broja pregleda", y = "Broj pregleda")

boxplot_2 <- ggplot(youtube_sample, aes(y = likes)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot broja lajkova", y = "Broj lajkova")

boxplot_3 <- ggplot(youtube_sample, aes(y = comment_count)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot broja komentara", y = "Broj komentara")

boxplot_4 <- ggplot(youtube_sample, aes(y = duration_seconds)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10() +
  labs(title = "Boxplot trajanja video zapisa", y = "Trajanje (sekunde)")

boxplot_5 <- ggplot(youtube_sample, aes(y = like_rate)) +
  geom_boxplot(fill = "darkorange") +
  scale_y_log10() +
  labs(title = "Boxplot stope lajkova", y = "Like rate")

boxplot_6 <- ggplot(youtube_sample, aes(y = comment_rate)) +
  geom_boxplot(fill = "darkorange") +
  scale_y_log10() +
  labs(title = "Boxplot stope komentara", y = "Comment rate")

# --- Bar plot - distribucija po kategorijama -------------------------------

category_counts <- youtube_data %>%
  count(category_name, sort = TRUE) %>%
  collect()

barplot_1 <- ggplot(category_counts, aes(x = reorder(category_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Broj video zapisa po kategoriji",
    x = "Kategorija", y = "Broj video zapisa"
  )

# ---------------------------------------------------------------------------
# 3.3 Analiza odnosa između obeležja
# ---------------------------------------------------------------------------

# --- 3.3.1 Korelaciona analiza numeričkih varijabli ---

numeric_data <- youtube_sample %>%
  select(
    view_count, likes, comment_count, duration_seconds,
    like_rate, comment_rate
  )

cor_matrix <- cor(numeric_data, use = "complete.obs")

options(width = 200)
print(cor_matrix)

# Vizualizacija korelacione matrice
cor_long <- melt(cor_matrix)

cor_matrix_plot_1 <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0,
    limits = c(-1, 1), name = "Korelacija"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Korelaciona matrica numeričkih varijabli", x = "", y = "")

# --- 3.3.2 Scatter plot analiza odnosa između numeričkih varijabli ---

scatter_plot_1 <- ggplot(numeric_data, aes(x = view_count, y = likes)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Broj pregleda vs. broj lajkova (log-log skala)",
    x = "Broj pregleda", y = "Broj lajkova"
  )

scatter_plot_2 <- ggplot(numeric_data, aes(x = view_count, y = comment_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Broj pregleda vs. broj komentara (log-log skala)",
    x = "Broj pregleda", y = "Broj komentara"
  )

scatter_plot_3 <- ggplot(numeric_data, aes(x = likes, y = comment_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Broj lajkova vs. broj komentara (log-log skala)",
    x = "Broj lajkova", y = "Broj komentara"
  )

scatter_plot_4 <- ggplot(numeric_data, aes(x = duration_seconds, y = view_count)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Trajanje vs. broj pregleda (log-log skala)",
    x = "Trajanje (sekunde)", y = "Broj pregleda"
  )

# Scatter plotovi za izvedena obeležja
scatter_plot_5 <- ggplot(numeric_data, aes(x = like_rate, y = comment_rate)) +
  geom_point(alpha = 0.2, size = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Stopa lajkova vs. stopa komentara (log-log skala)",
    x = "Like rate", y = "Comment rate"
  )

# --- 3.3.3 Odnos kategorijskih i numeričkih varijabli ---

ratio_plot_1 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, view_count, median),
  y = view_count
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Broj pregleda po kategoriji",
    x = "Kategorija", y = "Broj pregleda (log skala)"
  )

ratio_plot_2 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, likes, median),
  y = likes
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Broj lajkova po kategoriji",
    x = "Kategorija", y = "Broj lajkova (log skala)"
  )

ratio_plot_3 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, comment_count, median),
  y = comment_count
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Broj komentara po kategoriji",
    x = "Kategorija", y = "Broj komentara (log skala)"
  )

ratio_plot_4 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, like_rate, median),
  y = like_rate
)) +
  geom_boxplot(fill = "darkorange", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Stopa lajkova po kategoriji",
    x = "Kategorija", y = "Like rate (log skala)"
  )

ratio_plot_5 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, comment_rate, median),
  y = comment_rate
)) +
  geom_boxplot(fill = "darkorange", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Stopa komentara po kategoriji",
    x = "Kategorija", y = "Comment rate (log skala)"
  )

ratio_plot_6 <- ggplot(youtube_sample, aes(
  x = reorder(category_name, duration_seconds, median),
  y = duration_seconds
)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Trajanje video zapisa po kategoriji",
    x = "Kategorija", y = "Trajanje (sekunde, log skala)"
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