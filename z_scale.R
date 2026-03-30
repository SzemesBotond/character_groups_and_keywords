# load data from "drama_chargroup_and_keywords.R"
# Cluster data from a list of dataframes: shake_list 
# Steps:
# 1) select columns: title, name, numOfWords, numOfSpeechActs, betweenness
# 2) normnalize the count-based metrics to length, and z-score all the three 
# 3) bind all dataframes into one
# 4) Clusterization: k-means cluster into 4 clusters, or deciles or quartiles

# 1-3) select + normalize within each play + z-score + join metadata
all_shake <- shake_list %>%
  map(~ {
    df <- .x %>%
      select(title, name, numOfWords, numOfSpeechActs, betweenness)
    
    total_words_play <- sum(df$numOfWords, na.rm = TRUE)
    
    df %>%
      mutate(
        numOfWords_rel = numOfWords / total_words_play,
        numOfSpeechActs_rel = numOfSpeechActs / total_words_play,
        betweenness_rel = betweenness 
      ) %>%
      mutate(
        across(
          c(numOfWords_rel, numOfSpeechActs_rel, betweenness_rel),
          ~ as.numeric(scale(.x)),
          .names = "{.col}_z"
        )
      )
  }) %>%
  bind_rows(.id = "playName") %>%
  left_join(
    shake_meta %>% select(playName, Genre, Comedy_Other),
    by = "playName"
  )


# 4) differnt types of clusterization: 
# with or wo genre separtation, k-means, or based on Q or D rank
# k-means to cluster 1 to 4/10

## K-means

relabel_kmeans <- function(km) {
  centers <- as.numeric(km$centers)
  order_map <- order(centers, decreasing = TRUE)
  new_labels <- match(km$cluster, order_map)
  factor(new_labels, levels = 1:4)
}

cluster_one_metric <- function(df, col, k = 4) {
  x <- df[[col]]
  ok <- is.finite(x)
  out <- rep(NA_integer_, nrow(df))
  if (sum(ok) >= k) {
    km <- kmeans(x[ok], centers = k, nstart = 25)
    out[ok] <- as.integer(relabel_kmeans(km))
  }
  factor(out, levels = 1:k)
}

# Independent of genre
set.seed(123)

all_shake_independent_clusters <- all_shake %>%
  mutate(
    cluster_numOfWords      = cluster_one_metric(., "numOfWords_rel_z", 4),
    cluster_numOfSpeechActs = cluster_one_metric(., "numOfSpeechActs_rel_z", 4),
    cluster_betweenness     = cluster_one_metric(., "betweenness_rel_z", 4)
  )

# Genre-level clusters
set.seed(123)

all_shake_clustered_by_group <- all_shake %>%
  mutate(group = if_else(Comedy_Other == "Comedy", "Comedy", "Non-Comedy")) %>%
  group_by(group) %>%
  group_modify(~ .x %>%
                 mutate(
                   cluster_numOfWords      = cluster_one_metric(.x, "numOfWords_rel_z", 4),
                   cluster_numOfSpeechActs = cluster_one_metric(.x, "numOfSpeechActs_rel_z", 4),
                   cluster_betweenness     = cluster_one_metric(.x, "betweenness_rel_z", 4)
                 )
  ) %>%
  ungroup()

z_connector_genre <- all_shake_clustered_by_group %>%
  filter(cluster_betweenness == 1,
         cluster_numOfWords != 1,
         cluster_numOfSpeechActs != 1)

z_speaker_genre <- all_shake_clustered_by_group %>%
  filter(cluster_betweenness != 1,
         cluster_numOfWords == 1,
         cluster_numOfSpeechActs == 1)


## Quartile or Decile (range-based, not rank-based)

library(dplyr)

# Helper: quartiles based on equal value ranges
quartile_range <- function(x) {
  rng <- range(x, na.rm = TRUE)
  
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(factor(rep(NA_integer_, length(x)), levels = 1:4))
  }
  
  q <- cut(
    x,
    breaks = seq(rng[1], rng[2], length.out = 5),
    include.lowest = TRUE,
    labels = 4:1   # highest = 1, lowest = 4
  )
  
  factor(as.integer(as.character(q)), levels = 1:4)
}

# Helper: deciles based on equal value ranges
decile_range <- function(x) {
  rng <- range(x, na.rm = TRUE)
  
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(factor(rep(NA_integer_, length(x)), levels = 1:10))
  }
  
  d <- cut(
    x,
    breaks = seq(rng[1], rng[2], length.out = 11),
    include.lowest = TRUE,
    labels = 10:1   # highest = 1, lowest = 10
  )
  
  factor(as.integer(as.character(d)), levels = 1:10)
}

# Independent of genre: quartiles
all_shake_independent_clustersQ <- all_shake %>%
  mutate(
    cluster_numOfWords      = quartile_range(numOfWords_rel_z),
    cluster_numOfSpeechActs = quartile_range(numOfSpeechActs_rel_z),
    cluster_betweenness     = quartile_range(betweenness_rel_z)
  )

# Genre-level clusters: quartiles
all_shake_clustered_by_groupQ <- all_shake %>%
  mutate(group = if_else(Comedy_Other == "Comedy", "Comedy", "Non-Comedy")) %>%
  group_by(group) %>%
  group_modify(~ .x %>%
                 mutate(
                   cluster_numOfWords      = quartile_range(numOfWords_rel_z),
                   cluster_numOfSpeechActs = quartile_range(numOfSpeechActs_rel_z),
                   cluster_betweenness     = quartile_range(betweenness_rel_z)
                 )
  ) %>%
  ungroup()

# Independent of genre: deciles
all_shake_independent_clustersD <- all_shake %>%
  mutate(
    cluster_numOfWords      = decile_range(numOfWords_rel_z),
    cluster_numOfSpeechActs = decile_range(numOfSpeechActs_rel_z),
    cluster_betweenness     = decile_range(betweenness_rel_z)
  )

# Genre-level clusters: deciles
all_shake_clustered_by_groupD <- all_shake %>%
  mutate(group = if_else(Comedy_Other == "Comedy", "Comedy", "Non-Comedy")) %>%
  group_by(group) %>%
  group_modify(~ .x %>%
                 mutate(
                   cluster_numOfWords      = decile_range(numOfWords_rel_z),
                   cluster_numOfSpeechActs = decile_range(numOfSpeechActs_rel_z),
                   cluster_betweenness     = decile_range(betweenness_rel_z)
                 )
  ) %>%
  ungroup()

# within genre groups Q or D or independent
z_dominant_genreQ <- all_shake_clustered_by_groupQ %>%
  filter(
    cluster_numOfWords == 1,
    cluster_numOfSpeechActs == 1,
    cluster_betweenness == 1
  )

z_speaker_genreQ <- all_shake_clustered_by_groupQ %>%
  filter(
    cluster_numOfWords == 1,
    cluster_numOfSpeechActs == 1,
    cluster_betweenness != 1
  )

z_connector_genreQ <- all_shake_clustered_by_groupQ %>%
  filter(
    cluster_betweenness == 1,
    cluster_numOfWords != 1,
    cluster_numOfSpeechActs != 1
  )



### Visualization

all_shake <- all_shake %>%
  mutate(
    speech = rowMeans(
      select(., numOfWords_rel_z, numOfSpeechActs_rel_z),
      na.rm = TRUE
    )
  )

ggplot(all_shake, aes(x = speech, y = betweenness_rel_z, label = name)) +
  geom_point(alpha = 0.7) +
  geom_text(check_overlap = TRUE, vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    x = "Speech (mean z-score of normalized numOfWords and numOfSpeechActs)",
    y = "Betweenness (normalized z-score)",
    title = ""
  )

## Nicer label adjustment
all_shake <- all_shake %>%
  mutate(
    important = dplyr::percent_rank(numOfWords_rel_z) >= 0.95 |
      dplyr::percent_rank(numOfSpeechActs_rel_z) >= 0.95 |
      dplyr::percent_rank(betweenness_rel_z) >= 0.95
  )


library(ggrepel)

plot_to_save <- ggplot(all_shake, aes(x = speech, y = betweenness_rel_z)) +
  geom_point(alpha = 0.4) +
  geom_text_repel(
    data = filter(all_shake, important),
    aes(label = name),
    size = 3,
    max.overlaps = Inf,
    force = 0.1,
    force_pull = 2,
    box.padding = 0.15,
    point.padding = 0.05,
    min.segment.length = Inf,
    max.time = 2,
    max.iter = 10000,
    direction = "both"
  ) +
  theme_minimal() +
  labs(
    x = "Speech (mean z-score of normalized numOfWords and numOfSpeechActs)",
    y = "Betweenness (normalized z-score)",
    title = ""
  )

ggsave( "PATH",
        plot_to_save,
        width = 28,
        height = 28,
        units = "cm",
        dpi = 300)
