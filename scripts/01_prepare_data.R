library(sparklyr)
library(dplyr)
library(ggplot2)
library(janitor)

load_youtube_data <- function(sc) {
  # ---------------------------------------------------------------------------
  # 2.1 Data loading
  # ---------------------------------------------------------------------------
  dataset <- spark_read_csv(
    sc,
    name = "youtube",
    path = "data/all_regions_youtube_trending_data_with_duration.csv",
    header = TRUE,
    infer_schema = TRUE
  )
  
  # print(sdf_schema(dataset))
  
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
  
  # ---------------------------------------------------------------------------
  # 2.3 Data cleaning — removing duplicates by (video_id, trending_date)
  # ---------------------------------------------------------------------------
  youtube_clean <- dataset %>%
    distinct(video_id, trending_date, .keep_all = TRUE)
  
  print(sdf_nrow(youtube_clean))
  
  # ---------------------------------------------------------------------------
  # 2.4 Missing value handling
  # ---------------------------------------------------------------------------
  print("Missing values (na_counts):")
  na_counts <- youtube_clean %>%
    summarise(
      video_id_na = sum(ifelse(is.na(video_id), 1, 0)),
      title_na = sum(ifelse(is.na(title), 1, 0)),
      channel_id_na = sum(ifelse(is.na(channel_id), 1, 0)),
      channel_title_na = sum(ifelse(is.na(channel_title), 1, 0)),
      category_id_na = sum(ifelse(is.na(category_id), 1, 0)),
      view_count_na = sum(ifelse(is.na(view_count) | view_count == 0, 1, 0)),
      likes_na = sum(ifelse(is.na(likes), 1, 0)),
      dislikes_na = sum(ifelse(is.na(dislikes) | dislikes == 0, 1, 0)),
      comment_count_na = sum(ifelse(is.na(comment_count), 1, 0)),
      published_at_na = sum(ifelse(is.na(published_at), 1, 0)),
      trending_date_na = sum(ifelse(is.na(trending_date), 1, 0)),
      duration_na = sum(ifelse(is.na(duration), 1, 0))
    )
  print(na_counts)
  
  # Note on dislikes: since 2021 YouTube removed public dislike counts,
  # resulting in ~65% missing values. This feature is excluded from modeling.
  dislikes_analysis <- youtube_clean %>%
    summarise(
      total = n(),
      dislikes_present = sum(ifelse(!is.na(dislikes) & dislikes != 0, 1, 0)),
      dislikes_missing = sum(ifelse(is.na(dislikes) | dislikes == 0, 1, 0))
    ) %>%
    mutate(missing_percentage = 100 * dislikes_missing / total)
  print(dislikes_analysis)
  
  # Filtering required fields
  youtube_clean <- youtube_clean %>%
    filter(
      !is.na(video_id),
      !is.na(trending_date),
      !is.na(view_count) | view_count == 0
    )
  
  # Median imputation for numerical attributes (excluding dislikes)
  medians <- youtube_clean %>%
    summarise(
      view_med     = percentile_approx(view_count, 0.5),
      likes_med    = percentile_approx(likes, 0.5),
      comments_med = percentile_approx(comment_count, 0.5)
    ) %>%
    collect()
  
  youtube_clean <- youtube_clean %>%
    mutate(
      view_count    = ifelse(is.na(view_count) | view_count == 0, medians$view_med, view_count),
      likes         = ifelse(is.na(likes), medians$likes_med, likes),
      comment_count = ifelse(is.na(comment_count), medians$comments_med, comment_count)
    )
  
  # ---------------------------------------------------------------------------
  # 2.6 Feature transformation — duration_seconds from ISO 8601 format
  # ---------------------------------------------------------------------------
  youtube_clean <- youtube_clean %>%
    mutate(
      duration = coalesce(duration, "PT0S"),
      h = as.integer(regexp_extract(duration, "([0-9]+)H", 1)),
      m = as.integer(regexp_extract(duration, "([0-9]+)M", 1)),
      s = as.integer(regexp_extract(duration, "([0-9]+)S", 1)),
      duration_seconds = as.integer(
        coalesce(h, 0L) * 3600L +
          coalesce(m, 0L) * 60L +
          coalesce(s, 0L)
      )
    )
  
  # Validation sample
  youtube_clean %>%
    select(duration, duration_seconds) %>%
    sdf_sample(fraction = 0.001) %>%
    sdf_collect() %>%
    head(20) %>%
    print()
  
  # Handling categorical missing values
  youtube_clean <- youtube_clean %>%
    mutate(
      category_id = as.integer(coalesce(category_id, -1))
    )
  
  # Selecting relevant columns
  youtube_clean <- youtube_clean %>%
    select(
      video_id,
      category_id,
      view_count,
      likes,
      comment_count,
      duration,
      duration_seconds,
      published_at,
      trending_date
    )
  
  # Mapping category_id to category names
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
      select(category_id, category_name) %>%
      distinct() %>%
      collect()
  )
  
  # ---------------------------------------------------------------------------
  # 2.6 Addressing denormalization — aggregation by video_id
  #
  # The original dataset contains multiple rows per (video_id, trending_date)
  # meaning each video appears multiple times over time. For classification
  # and clustering, we aggregate to one row per video_id using maximum values
  # of numerical attributes (peak popularity).
  # ---------------------------------------------------------------------------
  print("Number of rows before aggregation:")
  print(sdf_nrow(youtube_clean))
  
  youtube_aggregated <- youtube_clean %>%
    group_by(video_id, category_id, category_name) %>%
    summarise(
      view_count = max(view_count, na.rm = TRUE),
      likes = max(likes, na.rm = TRUE),
      comment_count = max(comment_count, na.rm = TRUE),
      duration_seconds = max(duration_seconds, na.rm = TRUE),
      trending_count = n(),
      .groups = "drop"
    )
  
  print("Number of rows after aggregation by video_id:")
  print(sdf_nrow(youtube_aggregated))
  
  # ---------------------------------------------------------------------------
  # 2.7 Feature engineering — engagement rates
  #
  # Like rate:
  #   like_rate = likes / view_count
  #
  # Comment rate:
  #   comment_rate = comment_count / view_count
  # ---------------------------------------------------------------------------
  youtube_aggregated <- youtube_aggregated %>%
    mutate(
      like_rate    = likes / view_count,
      comment_rate = comment_count / view_count
    ) %>%
    filter(
      view_count > 0,
      !is.na(like_rate),
      !is.na(comment_rate)
    )
  
  print("Sample of engineered features:")
  youtube_aggregated %>%
    select(
      video_id, view_count, likes, comment_count,
      like_rate, comment_rate
    ) %>%
    sdf_sample(fraction = 0.001) %>%
    sdf_collect() %>%
    head(10) %>%
    print()
  
  print("Final number of records (aggregated dataset):")
  print(sdf_nrow(youtube_aggregated))
  
  return(youtube_aggregated)
}