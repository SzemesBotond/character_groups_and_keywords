# Play-level character groups from ShakeDraCor.
# Run from the repository root. This script uses rdracor's default DraCor API.

library(dplyr)
library(purrr)
library(tidyr)
library(rdracor)

shake_meta <- get_dracor(corpus = "shake", full_metadata = TRUE)

genre_manual <- tibble::tribble(
  ~playName, ~Genre,
  "alls-well-that-ends-well", "Comedy",
  "as-you-like-it", "Comedy",
  "the-comedy-of-errors", "Comedy",
  "loves-labors-lost", "Comedy",
  "measure-for-measure", "Comedy",
  "the-merchant-of-venice", "Comedy",
  "the-merry-wives-of-windsor", "Comedy",
  "a-midsummer-nights-dream", "Comedy",
  "much-ado-about-nothing", "Comedy",
  "the-taming-of-the-shrew", "Comedy",
  "the-tempest", "Comedy",
  "twelfth-night", "Comedy",
  "two-gentlemen-of-verona", "Comedy",
  "the-winters-tale", "Comedy",
  "henry-iv-part-1", "History",
  "henry-iv-part-2", "History",
  "henry-v", "History",
  "henry-vi-part-1", "History",
  "henry-vi-part-2", "History",
  "henry-vi-part-3", "History",
  "henry-viii", "History",
  "king-john", "History",
  "richard-ii", "History",
  "richard-iii", "History",
  "antony-and-cleopatra", "Tragedy",
  "coriolanus", "Tragedy",
  "cymbeline", "Tragedy",
  "hamlet", "Tragedy",
  "julius-caesar", "Tragedy",
  "king-lear", "Tragedy",
  "macbeth", "Tragedy",
  "othello", "Tragedy",
  "pericles", "Tragedy",
  "romeo-and-juliet", "Tragedy",
  "timon-of-athens", "Tragedy",
  "titus-andronicus", "Tragedy",
  "troilus-and-cressida", "Tragedy"
)

shake_meta <- shake_meta %>%
  select(-any_of(c("Genre", "Comedy_Other"))) %>%
  left_join(genre_manual, by = "playName") %>%
  mutate(Comedy_Other = recode(Genre, Comedy = "Comedy",
                               History = "Other", Tragedy = "Other"))

stopifnot(
  !anyDuplicated(shake_meta$playName),
  !anyNA(shake_meta$Genre),
  nrow(shake_meta) == nrow(genre_manual)
)
print(count(shake_meta, Genre))

shake_characters <- shake_meta %>%
  select(playName, title) %>%
  mutate(characters = map(playName, ~ get_play_characters(
    play = .x, corpus = "shake"
  ))) %>%
  unnest(characters)

shake_list <- split(shake_characters, shake_characters$playName)

# Divide each metric's observed range into four unequal intervals.
# P1 is the interval nearest the maximum, not a fixed cast-size proportion.
process_df_powerlaw <- function(df, n_groups = 4, k = 2) {
  metrics <- c(
    "numOfScenes", "numOfSpeechActs", "numOfWords", "weightedDegree",
    "betweenness", "closeness", "eigenvector"
  )

  for (metric in metrics) {
    x <- df[[metric]]
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    steps <- seq(0, 1, length.out = n_groups + 1)
    breaks <- xmin + (xmax - xmin) * (1 - (1 - steps)^k)
    breaks <- cummax(breaks)
    labels <- paste0("P", rev(seq_len(n_groups)))

    df[[paste0("P-", metric)]] <- cut(
      x,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
  }
  df
}

shake_list_processed_power <- map(shake_list, process_df_powerlaw)
all_characters_power <- bind_rows(shake_list_processed_power, .id = "source_file")

all_characters_power1 <- all_characters_power %>%
  select(
    playName, id, name, P_numOfSpeechActs = `P-numOfSpeechActs`,
    P_numOfWords = `P-numOfWords`, P_betweenness = `P-betweenness`,
    P_closeness = `P-closeness`, P_eigenvector = `P-eigenvector`, numOfWords
  ) %>%
  left_join(
    shake_meta %>% select(playName, Genre, Comedy_Other),
    by = "playName"
  )

connectors <- all_characters_power1 %>%
  filter(P_betweenness == "P1", P_numOfSpeechActs != "P1", P_numOfWords != "P1")
speakers <- all_characters_power1 %>%
  filter(P_betweenness != "P1", P_numOfSpeechActs == "P1", P_numOfWords == "P1")
dominant <- all_characters_power1 %>%
  filter(P_betweenness == "P1", P_numOfSpeechActs == "P1", P_numOfWords == "P1")

speakers_com <- filter(speakers, Genre == "Comedy")
speakers_trag <- filter(speakers, Genre != "Comedy")
connectors_com <- filter(connectors, Genre == "Comedy")
connectors_trag <- filter(connectors, Genre != "Comedy")

intermediate <- all_characters_power1 %>%
  filter(
    P_betweenness %in% c("P2", "P3"),
    P_numOfSpeechActs %in% c("P2", "P3"),
    P_numOfWords %in% c("P2", "P3")
  )
orator <- filter(all_characters_power1, P_numOfSpeechActs != "P1", P_numOfWords == "P1")
interactor <- filter(all_characters_power1, P_numOfSpeechActs == "P1", P_numOfWords != "P1")

stopifnot(all(c("id", "playName", "Comedy_Other") %in% names(all_characters_power1)))

data_dir <- file.path("data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    shake_meta = shake_meta,
    shake_list = shake_list,
    shake_list_processed_power = shake_list_processed_power,
    all_characters_power1 = all_characters_power1,
    connectors = connectors,
    speakers = speakers,
    dominant = dominant,
    intermediate = intermediate,
    orator = orator,
    interactor = interactor
  ),
  file.path(data_dir, "character-groups-results.rds")
)
