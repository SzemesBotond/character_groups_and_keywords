library(dplyr)
library(stringr)
library(purrr)

#using data from z-scale.R and drama_chargroup_and_keywords.R
# -------------------------
# 1. Helper: range-based bins
# -------------------------
make_bins <- function(x, n, prefix) {
  rng <- range(x, na.rm = TRUE)
  
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(NA_character_, length(x)))
  }
  
  cut(
    x,
    breaks = seq(rng[1], rng[2], length.out = n + 1),
    include.lowest = TRUE,
    labels = paste0(prefix, n:1)
  )
}

# -------------------------
# 2. Base data (one play)
# -------------------------
romeo_base <- shake_list_processed_power[["romeo-and-juliet"]] %>%
  mutate(
    # deciles
    across(
      c(numOfWords, numOfSpeechActs, betweenness),
      ~ make_bins(.x, 10, "D"),
      .names = "decile_{.col}"
    ),
    # quartiles
    across(
      c(numOfWords, numOfSpeechActs, betweenness),
      ~ make_bins(.x, 4, "Q"),
      .names = "quartile_{.col}"
    )
  )

# -------------------------
# 3. Select + filter (P1–P3 or Q1–Q3 == just "major" chars)
# -------------------------
romeo_filtered <- romeo_base %>%
  select(
    name,
    numOfWords, `P-numOfWords`, quartile_numOfWords, decile_numOfWords,
    numOfSpeechActs, `P-numOfSpeechActs`, quartile_numOfSpeechActs, decile_numOfSpeechActs,
    betweenness, `P-betweenness`, quartile_betweenness, decile_betweenness
  ) %>%
  filter(
    str_detect(`P-numOfWords`, "P[1-3]") |
      str_detect(`P-numOfSpeechActs`, "P[1-3]") |
      str_detect(`P-betweenness`, "P[1-3]") |
      str_detect(quartile_numOfWords, "Q[1-3]") |
      str_detect(quartile_numOfSpeechActs, "Q[1-3]") |
      str_detect(quartile_betweenness, "Q[1-3]")
  )

# -------------------------
# 4. Clean labels
# -------------------------
romeo_clean <- romeo_filtered %>%
  mutate(
    across(-name, ~ if (is.character(.x)) str_remove_all(.x, "[PQD]") else .x)
  ) %>%
  rename(
    power_numOfWords = `P-numOfWords`,
    power_numOfSpeechActs = `P-numOfSpeechActs`,
    power_betweenness = `P-betweenness`
  )

# -------------------------
# 5. Helper: extract clusters with unique final names
# -------------------------
get_clusters <- function(df, play, method = c("quartile", "decile", "kmeans"), scope = c("all", "genre")) {
  method <- match.arg(method)
  scope <- match.arg(scope)
  
  out <- df %>%
    filter(playName == play) %>%
    select(name, cluster_numOfWords, cluster_numOfSpeechActs, cluster_betweenness)
  
  if (method == "quartile" && scope == "all") {
    out %>%
      rename(
        quartile_numOfWords_rel_zscore = cluster_numOfWords,
        quartile_numOfSpeechActs_rel_zscore = cluster_numOfSpeechActs,
        quartile_betweenness_rel_zscore = cluster_betweenness
      )
  } else if (method == "decile" && scope == "all") {
    out %>%
      rename(
        decile_numOfWords_rel_zscore = cluster_numOfWords,
        decile_numOfSpeechActs_rel_zscore = cluster_numOfSpeechActs,
        decile_betweenness_rel_zscore = cluster_betweenness
      )
  } else if (method == "kmeans" && scope == "all") {
    out %>%
      rename(
        z_kmean_numOfWords = cluster_numOfWords,
        z_kmean_numOfSpeechActs = cluster_numOfSpeechActs,
        z_kmean_betweenness = cluster_betweenness
      )
  } else if (method == "quartile" && scope == "genre") {
    out %>%
      rename(
        quartile_numOfWords_rel_zscore_genre = cluster_numOfWords,
        quartile_numOfSpeechActs_rel_zscore_genre = cluster_numOfSpeechActs,
        quartile_betweenness_rel_zscore_genre = cluster_betweenness
      )
  } else if (method == "decile" && scope == "genre") {
    out %>%
      rename(
        decile_numOfWords_rel_zscore_genre = cluster_numOfWords,
        decile_numOfSpeechActs_rel_zscore_genre = cluster_numOfSpeechActs,
        decile_betweenness_rel_zscore_genre = cluster_betweenness
      )
  } else if (method == "kmeans" && scope == "genre") {
    out %>%
      rename(
        z_kmean_numOfWords_genre = cluster_numOfWords,
        z_kmean_numOfSpeechActs_genre = cluster_numOfSpeechActs,
        z_kmean_betweenness_genre = cluster_betweenness
      )
  }
}

# -------------------------
# 6. Collect all cluster sources
# -------------------------
clusters <- list(
  quartile_all   = get_clusters(all_shake_independent_clustersQ, "romeo-and-juliet", "quartile", "all"),
  decile_all     = get_clusters(all_shake_independent_clustersD, "romeo-and-juliet", "decile", "all"),
  kmeans_all     = get_clusters(all_shake_independent_clusters,  "romeo-and-juliet", "kmeans", "all"),
  quartile_genre = get_clusters(all_shake_clustered_by_groupQ,   "romeo-and-juliet", "quartile", "genre"),
  decile_genre   = get_clusters(all_shake_clustered_by_groupD,   "romeo-and-juliet", "decile", "genre"),
  kmeans_genre   = get_clusters(all_shake_clustered_by_group,    "romeo-and-juliet", "kmeans", "genre")
)

# -------------------------
# 7. Join everything
# -------------------------
romeo_groups <- reduce(
  clusters,
  ~ left_join(.x, .y, by = "name"),
  .init = romeo_clean
)

romeo_groups <- romeo_groups %>%
  mutate(
    across(
      -c(name, numOfWords, numOfSpeechActs, betweenness),
      ~ if (is.character(.x) || is.factor(.x)) {
        as.integer(str_remove_all(as.character(.x), "[PQD]"))
      } else {
        .x
      }
    )
  ) %>%
  select(
    name,
    numOfWords,
    power_numOfWords,
    quartile_numOfWords,
    decile_numOfWords,
    quartile_numOfWords_rel_zscore,
    decile_numOfWords_rel_zscore,
    quartile_numOfWords_rel_zscore_genre,
    decile_numOfWords_rel_zscore_genre,
    z_kmean_numOfWords,
    z_kmean_numOfWords_genre,
    numOfSpeechActs,
    power_numOfSpeechActs,
    quartile_numOfSpeechActs,
    decile_numOfSpeechActs,
    quartile_numOfSpeechActs_rel_zscore,
    decile_numOfSpeechActs_rel_zscore,
    quartile_numOfSpeechActs_rel_zscore_genre,
    decile_numOfSpeechActs_rel_zscore_genre,
    z_kmean_numOfSpeechActs,
    z_kmean_numOfSpeechActs_genre,
    betweenness,
    power_betweenness,
    quartile_betweenness,
    decile_betweenness,
    quartile_betweenness_rel_zscore,
    decile_betweenness_rel_zscore,
    quartile_betweenness_rel_zscore_genre,
    decile_betweenness_rel_zscore_genre,
    z_kmean_betweenness,
    z_kmean_betweenness_genre
  )


## Nicer output

romeo_groups_long <- bind_rows(
  romeo_groups %>%
    transmute(
      Character = name,
      Metric = "Words",
      Raw = numOfWords,
      Power = power_numOfWords,
      Q = quartile_numOfWords,
      D = decile_numOfWords,
      zQ_genre = quartile_numOfWords_rel_zscore_genre,
      zD_genre = decile_numOfWords_rel_zscore_genre,
      zK_genre = z_kmean_numOfWords_genre,
      zQ_all = quartile_numOfWords_rel_zscore,
      zD_all = decile_numOfWords_rel_zscore,
      zK_all = z_kmean_numOfWords
    ),
  
  romeo_groups %>%
    transmute(
      Character = name,
      Metric = "SpeechAct",
      Raw = numOfSpeechActs,
      Power = power_numOfSpeechActs,
      Q = quartile_numOfSpeechActs,
      D = decile_numOfSpeechActs,
      zQ_genre = quartile_numOfSpeechActs_rel_zscore_genre,
      zD_genre = decile_numOfSpeechActs_rel_zscore_genre,
      zK_genre = z_kmean_numOfSpeechActs_genre,
      zQ_all = quartile_numOfSpeechActs_rel_zscore,
      zD_all = decile_numOfSpeechActs_rel_zscore,
      zK_all = z_kmean_numOfSpeechActs
    ),
  
  romeo_groups %>%
    transmute(
      Character = name,
      Metric = "Betweenness",
      Raw = betweenness,
      Power = power_betweenness,
      Q = quartile_betweenness,
      D = decile_betweenness,
      zQ_genre = quartile_betweenness_rel_zscore_genre,
      zD_genre = decile_betweenness_rel_zscore_genre,
      zK_genre = z_kmean_betweenness_genre,
      zQ_all = quartile_betweenness_rel_zscore,
      zD_all = decile_betweenness_rel_zscore,
      zK_all = z_kmean_betweenness
    )
) %>%
  mutate(
    Metric = factor(Metric, levels = c("Words", "SpeechAct", "Betweenness"))
  ) %>%
  arrange(Character, Metric)

romeo_groups_long
