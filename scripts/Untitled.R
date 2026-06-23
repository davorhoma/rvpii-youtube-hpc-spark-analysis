library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local[*]")

df <- spark_read_csv(sc, "data", "putanja/do/fajla.csv")

df %>%
  summarise(n_unique = n_distinct(category_id)) %>%
  collect()