library(sparklyr)
library(dplyr)
library(ggplot2)
library(janitor)

sc <- spark_connect(master = "local[*]")

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
# 2.3 Čišćenje podataka — uklanjanje duplikata po (video_id, trending_date)
# ---------------------------------------------------------------------------
youtube_clean <- dataset %>%
  distinct(video_id, trending_date, .keep_all = TRUE)

print(sdf_nrow(youtube_clean))

# ---------------------------------------------------------------------------
# 2.4 Obrada nedostajućih vrednosti
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

# Napomena o dislikes: od 2021. YouTube je uklonio javni prikaz broja
# negativnih reakcija, što rezultuje ~65% nedostajućih vrednosti.
# Atribut se isključuje iz prediktorskog skupa.
dislikes_analysis <- youtube_clean %>%
  summarise(
    ukupno = n(),
    dislikes_postoji = sum(ifelse(!is.na(dislikes) & dislikes != 0, 1, 0)),
    dislikes_nedostaje = sum(ifelse(is.na(dislikes) | dislikes == 0, 1, 0))
  ) %>%
  mutate(procenat_nedostaje = 100 * dislikes_nedostaje / ukupno)
print(dislikes_analysis)

# Filtriranje obaveznih polja
youtube_clean <- youtube_clean %>%
  filter(
    !is.na(video_id),
    !is.na(trending_date),
    !is.na(view_count) | view_count == 0
  )

# Imputacija medijanom za numeričke atribute (bez dislikes)
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
# 2.6 Transformacija atributa — duration_seconds iz ISO 8601 formata
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

# Provera ispravnosti konverzije na uzorku
youtube_clean %>%
  select(duration, duration_seconds) %>%
  sdf_sample(fraction = 0.001) %>%
  sdf_collect() %>%
  head(20) %>%
  print()

# Zamena nedostajućih tekstualnih vrednosti
youtube_clean <- youtube_clean %>%
  mutate(
    channel_title = ifelse(is.na(channel_title), "", channel_title),
    title         = ifelse(is.na(title), "", title),
    description   = ifelse(is.na(description), "", description),
    category_id   = as.integer(coalesce(category_id, -1))
  )

# Selekcija relevantnih kolona (tekstualna polja title, tags, description
# zadržavaju se u datasetu, ali NEĆE biti korišćena kao prediktori
# u klasifikaciji i klasterizaciji)
youtube_clean <- youtube_clean %>%
  select(
    video_id,
    title,
    channel_id,
    channel_title,
    category_id,
    view_count,
    likes,
    comment_count,
    duration,
    duration_seconds,
    published_at,
    trending_date
  )

# Mapiranje category_id na naziv kategorije
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
# 2.2 Adresiranje denormalizacije — agregacija po video_id
#
# Originalni dataset sadrži po jedan red za svaki (video_id, trending_date)
# par, tj. isti video se može pojaviti više puta. Za klasifikaciju i
# klasterizaciju kreira se agregirani skup sa jednim redom po video_id,
# koristeći maksimalne vrednosti numeričkih atributa (peak popularnosti).
# ---------------------------------------------------------------------------
print("Broj redova pre agregacije:")
print(sdf_nrow(youtube_clean))

youtube_aggregated <- youtube_clean %>%
  group_by(video_id, category_id, category_name) %>%
  summarise(
    view_count = max(view_count, na.rm = TRUE),
    likes = max(likes, na.rm = TRUE),
    comment_count = max(comment_count, na.rm = TRUE),
    duration_seconds = max(duration_seconds, na.rm = TRUE),
    trending_count = n(), # koliko puta je video bio trending
    .groups = "drop"
  )

print("Broj redova nakon agregacije po video_id:")
print(sdf_nrow(youtube_aggregated))

# ---------------------------------------------------------------------------
# 2.7 Izvođenje novih atributa — stope angažmana
#
# Stopa pozitivnih reakcija:
#   like_rate = likes / view_count
#   
#
# Stopa komentara:
#   comment_rate = comment_count / view_count
# ---------------------------------------------------------------------------
youtube_aggregated <- youtube_aggregated %>%
  mutate(
    like_rate    = likes / view_count,
    comment_rate = comment_count / view_count
  ) %>%
  # Uklanjaju se zapisi gde su izvedeni atributi nevažeći
  # (view_count == 0 bi davao Inf/NaN)
  filter(
    view_count > 0,
    !is.na(like_rate),
    !is.na(comment_rate)
  )

print("Provera izvedenih atributa (uzorak):")
youtube_aggregated %>%
  select(
    video_id, view_count, likes, comment_count,
    like_rate, comment_rate
  ) %>%
  sdf_sample(fraction = 0.001) %>%
  sdf_collect() %>%
  head(10) %>%
  print()

print("Finalni broj zapisa (agregirani skup):")
print(sdf_nrow(youtube_aggregated))

spark_disconnect(sc)
