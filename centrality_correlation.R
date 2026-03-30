library(rdracor)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)

# Get all plays in ShakeDraCor
shake_meta <- get_dracor(corpus = "shake")

# Pull character-level data for every play
shake_chars <- map_dfr(
  shake_meta$playName,
  \(p) {
    message("Downloading: ", p)
    get_play_characters(play = p, corpus = "shake") %>%
      mutate(play = p)
  }
)

#the big function
compute_correlations <- function(n_char) {
  
  #just major chars in terms of numOfWords
  top_chars <- shake_chars %>%
    group_by(play) %>%
    slice_max(numOfWords, n = n_char, with_ties = FALSE) %>%
    ungroup()
  
  dat <- top_chars %>%
    transmute(
      play,
      weightedDegree,
      closeness,
      betweenness,
      eigenvector,
      numOfWords,
      numOfSpeechActs
    )
  
  per_play <- dat %>%
    group_by(play) %>%
    summarise(
      wd_words = cor(weightedDegree, numOfWords, use = "pairwise.complete.obs"),
      wd_speechacts = cor(weightedDegree, numOfSpeechActs, use = "pairwise.complete.obs"),
      
      closeness_words = cor(closeness, numOfWords, use = "pairwise.complete.obs"),
      closeness_speechacts = cor(closeness, numOfSpeechActs, use = "pairwise.complete.obs"),
      
      betweenness_words = cor(betweenness, numOfWords, use = "pairwise.complete.obs"),
      betweenness_speechacts = cor(betweenness, numOfSpeechActs, use = "pairwise.complete.obs"),
      
      eigenvector_words = cor(eigenvector, numOfWords, use = "pairwise.complete.obs"),
      eigenvector_speechacts = cor(eigenvector, numOfSpeechActs, use = "pairwise.complete.obs"),
      .groups = "drop"
    )
  
  agg <- tibble(
    threshold = n_char,
    centrality = c("Weigthed\nDegree", "Closeness", "Betweenness", "Eigenvector"),
    numOfWords = c(
      mean(per_play$wd_words, na.rm = TRUE),
      mean(per_play$closeness_words, na.rm = TRUE),
      mean(per_play$betweenness_words, na.rm = TRUE),
      mean(per_play$eigenvector_words, na.rm = TRUE)
    ),
    numOfSpeechActs = c(
      mean(per_play$wd_speechacts, na.rm = TRUE),
      mean(per_play$closeness_speechacts, na.rm = TRUE),
      mean(per_play$betweenness_speechacts, na.rm = TRUE),
      mean(per_play$eigenvector_speechacts, na.rm = TRUE)
    )
  )
  
  return(agg)
}

# number of top characters
threshold_results <- map_dfr(c(5,7), compute_correlations)

plot_dat <- threshold_results %>%
  pivot_longer(
    cols = c(numOfWords, numOfSpeechActs),
    names_to = "measure",
    values_to = "correlation"
  ) 

plot_dat$threshold <- c(rep("5 char", 8),rep("7 char", 8) )

plot_to_save <- ggplot(plot_dat,
                       aes(x = centrality, y = correlation, fill = measure)) +
  geom_col(position = "dodge") +
  facet_wrap(~threshold) +
  geom_hline(yintercept = 0) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Mean correlation",
    fill = NULL
  ) +
  scale_fill_manual(values = 
                      c("#889E19", "pink"))+
  theme_minimal()

