library(sparklyr)
library(dplyr)
library(ggplot2)
library(janitor)

load_youtube_data <- function(sc) {
  dataset <- spark_read_csv(
    sc,
    name = "youtube",
    path = "data/all_regions_youtube_trending_data_with_duration.csv",
    header = TRUE,
    infer_schema = TRUE
  )

  dataset <- dataset %>%
    clean_names() %>%
    mutate(
      view_count = as.numeric(view_count),
      likes = as.numeric(likes),
      dislikes = as.numeric(dislikes),
      comment_count = as.numeric(comment_count),
      published_at = to_date(published_at),
      trending_date = to_date(trending_date),
      category_id = as.numeric(category_id),
      duration = as.character(duration)
    )

  print(colnames(dataset))
  print(sdf_num_partitions(dataset))
  print(sdf_nrow(dataset))

  youtube_clean <- dataset %>%
    distinct(video_id, trending_date, .keep_all = TRUE)

  # youtube_clean <- sdf_drop_duplicates(
  #   dataset,
  #   cols = c("video_id", "trending_date")
  # )

  print(sdf_nrow(youtube_clean))

  # Obrada nedostajucih vrednosti

  print("Missing values (na_counts):")
  na_counts <- youtube_clean %>%
    summarise(
      video_id_na = sum(ifelse(is.na(video_id), 1, 0)),
      title_na = sum(ifelse(is.na(title), 1, 0)),
      channel_id_na = sum(ifelse(is.na(channel_id), 1, 0)),
      channel_title_na = sum(ifelse(is.na(channel_title), 1, 0)),
      category_id_na = sum(ifelse(is.na(category_id), 1, 0)),
      view_count_na = sum(ifelse(is.na(view_count), 1, 0)),
      likes_na = sum(ifelse(is.na(likes), 1, 0)),
      dislikes_na = sum(ifelse(is.na(dislikes), 1, 0)),
      comment_count_na = sum(ifelse(is.na(comment_count), 1, 0)),
      published_at_na = sum(ifelse(is.na(published_at), 1, 0)),
      trending_date_na = sum(ifelse(is.na(trending_date), 1, 0)),
      duration_na = sum(ifelse(is.na(duration), 1, 0)),
    )
  print(na_counts)

  duration_parts <- youtube_clean %>%
    summarise(
      ukupno = n(),
      dislikes_postoji = sum(ifelse(is.na(dislikes), 0, 1)),
      dislikes_nedostaje = sum(ifelse(is.na(dislikes), 1, 0)),
    ) %>%
    mutate(
      procenat_nedostaje = 100 * dislikes_nedostaje / ukupno
    )

  print(duration_parts)

  youtube_clean <- youtube_clean %>%
    filter(
      !is.na(video_id),
      !is.na(trending_date),
      !is.na(view_count)
    )

  medians <- youtube_clean %>%
    summarise(
      view_med = percentile_approx(view_count, 0.5),
      likes_med = percentile_approx(likes, 0.5),
      dislikes_med = percentile_approx(dislikes, 0.5),
      comments_med = percentile_approx(comment_count, 0.5)
    ) %>%
    collect()

  youtube_clean <- youtube_clean %>%
    mutate(
      view_count = ifelse(is.na(view_count), medians$view_med, view_count),
      likes = ifelse(is.na(likes), medians$likes_med, likes),
      dislikes = ifelse(is.na(dislikes), medians$dislikes_med, dislikes),
      comment_count = ifelse(is.na(comment_count), medians$comments_med, comment_count)
    )

  youtube_clean <- youtube_clean %>%
    mutate(
      duration = coalesce(duration, "PT0S"),

      h = as.integer(regexp_extract(duration, "([0-9]+)H", 1)),
      m = as.integer(regexp_extract(duration, "([0-9]+)M", 1)),
      s = as.integer(regexp_extract(duration, "([0-9]+)S", 1)),

      duration_seconds =
        coalesce(h, 0) * 3600 +
        coalesce(m, 0) * 60 +
        coalesce(s, 0)
    )

  youtube_clean %>%
    select(duration, duration_seconds) %>%
    sdf_sample(fraction = 0.001) %>%
    sdf_collect() %>%
    head(20)

  youtube_clean <- youtube_clean %>%
    mutate(
      duration_seconds = as.integer(duration_seconds)
    )

  youtube_clean <- youtube_clean %>%
    mutate(
      channel_title = ifelse(is.na(channel_title), "", channel_title),
      title = ifelse(is.na(title), "", title),
      description = ifelse(is.na(description), "", description),
      category_id = as.integer(coalesce(category_id, -1))
    )

  youtube_clean <- youtube_clean %>%
    select(
      video_id,
      title,
      channel_id,
      channel_title,
      category_id,
      view_count,
      likes,
      dislikes,
      comment_count,
      duration,
      duration_seconds,
      published_at,
      trending_date
    )

  category_map <- data.frame(
    category_id = c(
      1, 2, 10, 15, 17,
      18, 19, 20, 21, 22,
      23, 24, 25, 26, 27,
      28, 29, 30, 31, 32,
      33, 34, 35, 36, 37,
      38, 39, 40, 41, 42,
      43, 44, -1
    ),
    category_name = c(
      "Film & Animation",
      "Autos & Vehicles",
      "Music",
      "Pets & Animals",
      "Sports",
      "Short Movies",
      "Travel & Events",
      "Gaming",
      "Videoblogging",
      "People & Blogs",
      "Comedy",
      "Entertainment",
      "News & Politics",
      "Howto & Style",
      "Education",
      "Science & Technology",
      "Nonprofits & Activism",
      "Movies",
      "Anime/Animation",
      "Action/Adventure",
      "Classics",
      "Comedy",
      "Documentary",
      "Drama",
      "Family",
      "Foreign",
      "Horror",
      "Sci-Fi/Fantasy",
      "Thriller",
      "Shorts",
      "Shows",
      "Trailers",
      "Unknown"
    )
  )

  category_map$category_id <- as.integer(category_map$category_id)
  category_map_spark <- copy_to(sc, category_map, overwrite = TRUE)

  youtube_clean <- youtube_clean %>%
    left_join(category_map_spark, by = "category_id")

  print(
    youtube_clean %>%
      # filter(category_id == -1) %>%
      select(category_id, category_name) %>%
      distinct() %>%
      collect()
  )

  return(youtube_clean)

  # Normalizacija
  # assembler <- ft_vector_assembler(
  #   sc,
  #   input_cols = c("view_count", "likes", "dislikes", "comment_count"),
  #   output_col = "features"
  # )

  # scaler <- ft_standard_scaler(
  #   sc,
  #   input_col = "features",
  #   output_col = "scaled_features",
  #   with_mean = TRUE,
  #   with_std = TRUE
  # )

  # pipeline <- ml_pipeline(assembler, scaler)
  # model <- ml_fit(pipeline, youtube_clean)
  # youtube_scaled <- ml_transform(model, youtube_clean)

  # print(head(youtube_scaled))

  # print(colnames(youtube_scaled))

  # return(youtube_scaled)
}