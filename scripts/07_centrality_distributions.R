# Figure 2: centrality distributions for Shakespeare characters with >500 words.
# Data are retrieved from the ShakeDraCor API, as in the paper.

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(rdracor)

shake_meta <- get_dracor(corpus = "shake")

shake_chars <- map_dfr(
  shake_meta$playName,
  \(play) {
    message("Downloading: ", play)
    get_play_characters(play = play, corpus = "shake") %>%
      mutate(play = play)
  }
)

centrality_distributions <- shake_chars %>%
  filter(numOfWords > 500) %>%
  select(closeness, betweenness, eigenvector) %>%
  pivot_longer(
    cols = c(closeness, betweenness, eigenvector),
    names_to = "centrality",
    values_to = "score"
  ) %>%
  mutate(
    centrality = factor(
      centrality,
      levels = c("closeness", "betweenness", "eigenvector")
    )
  )

figure_2 <- ggplot(
  centrality_distributions,
  aes(x = score, colour = centrality, fill = centrality)
) +
  geom_density(alpha = 0.2, na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Centrality score", y = "Density", title = NULL)

output_dir <- file.path("figures", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(output_dir, "figure_2_centrality_distributions.png"),
  figure_2,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)
