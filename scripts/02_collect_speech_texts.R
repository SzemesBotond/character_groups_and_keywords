# Fetch and clean the selected character speeches.
# Requires data/character-groups-results.rds from script 01.

library(dplyr)
library(purrr)
library(tidyr)
library(tidytext)
library(readr)
library(stringr)
library(rdracor)

group_results <- readRDS(file.path("data", "character-groups-results.rds"))
connectors <- group_results$connectors
speakers <- group_results$speakers
dominant <- group_results$dominant
selected_plays <- unique(c(connectors$playName, speakers$playName, dominant$playName))

# Download each selected play once and match characters by stable ID.
speech_by_play <- setNames(
  map(selected_plays, ~ get_text_chr_spoken_bych(
    play = .x,
    corpus = "shake",
    as_data_frame = TRUE,
    split_text = FALSE
  )),
  selected_plays
)

get_character_speech <- function(play, character_id) {
  speech_by_play[[play]] %>%
    filter(.data$id == character_id) %>%
    select(label, text)
}

extract_group_speech <- function(group_data, group_name) {
  group_data %>%
    mutate(speeches = map2(playName, id, get_character_speech)) %>%
    unnest(speeches) %>%
    mutate(version = group_name)
}

char_speeches_connector <- extract_group_speech(connectors, "connector")
char_speeches_speaker <- extract_group_speech(speakers, "speaker")
char_speeches_dominant <- extract_group_speech(dominant, "dominant")
all_speech_power1 <- bind_rows(
  char_speeches_connector,
  char_speeches_speaker,
  char_speeches_dominant
)

stopifnot(
  nrow(char_speeches_connector) == nrow(connectors),
  nrow(char_speeches_speaker) == nrow(speakers),
  nrow(char_speeches_dominant) == nrow(dominant),
  is.character(all_speech_power1$text),
  !anyNA(all_speech_power1$text)
)
print(count(all_speech_power1, Comedy_Other, version))

# Normalization: lower-case sentence starts, remove
# remaining capitalized forms, then collapse whitespace.
all_speech_power1 <- all_speech_power1 %>%
  mutate(
    text = str_replace_all(
      text,
      '(?<=\\(|"|^|[.!?]\\s)([A-Z])',
      function(match) tolower(match)
    ),
    text = str_remove_all(text, "\\b\\w*[A-Z]+\\w*\\b"),
    text = str_squish(text)
  )

tidy_speech <- all_speech_power1 %>%
  unnest_tokens(word, text, to_lower = FALSE)

version_texts <- tidy_speech %>%
  mutate(document = paste(Comedy_Other, version, sep = "_")) %>%
  group_by(document) %>%
  summarise(text = paste(word, collapse = " "), .groups = "drop") %>%
  mutate(file_name = paste0("version_", document, ".txt"))

output_dir <- file.path("data", "keyword-texts")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
walk2(version_texts$text, file.path(output_dir, version_texts$file_name), write_file)

saveRDS(
  list(
    speech_by_play = speech_by_play,
    speech = all_speech_power1,
    tidy_speech = tidy_speech,
    pooled_texts = version_texts
  ),
  file.path("data", "speech-text-results.rds")
)
