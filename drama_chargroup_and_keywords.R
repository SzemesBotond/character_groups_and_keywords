library(dplyr)
library(purrr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(readr)
library(rdracor)
library(stringr)
library(stylo)

### STEP 1: Character groups

# load corpus level metadata and add Genre label
shake_meta <- get_dracor(corpus = "shake", full_metadata = TRUE)
Genre <- c(rep("Comedy", 14), rep("History", 10),rep("Tragedy", 13))
Comedy_Other <- c(rep("Comedy", 14), rep("Other",23))
shake_meta <- cbind(shake_meta, Genre,Comedy_Other)

# Extract character-level metadata for each play
# e.g. get_play_characters(play = "hamlet", corpus = "shake")
# dataframe: every row represents a character
shake_characters <- shake_meta %>%
  select(playName, title) %>%
  mutate(
    characters = map(playName, ~ get_play_characters(play = .x, corpus = "shake"))
  ) %>%
  unnest(characters)


# Split by playName
shake_list <- split(shake_characters, shake_characters$playName)

# Function to calculate rankings and cluster groups
# based on power-law distribution of range of the metric
process_df_powerlaw <- function(df, n_groups = 4, k = 2) {
  
  metrics <- c("numOfScenes", "numOfSpeechActs", "numOfWords",
               "weightedDegree", "betweenness", "closeness")
  
  for (metric in metrics) {
    
    x <- df[[metric]]
    
    # sorted range
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    
    # generate power-law cutpoints
    steps <- seq(0, 1, length.out = n_groups + 1)
    breaks <- xmin + (xmax - xmin) * (1 - (1 - steps)^k)  # power-law spacing
    
    # ensure strictly increasing
    breaks <- cummax(breaks)
    
    # label highest as P1
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


# Combine all dataframes into one
all_characters_power <- bind_rows(shake_list_processed_power, .id = "source_file")


# select important columns
all_characters_power1 <- all_characters_power %>% 
  select("playName", "id", "name", 
         "P-numOfSpeechActs",
         "P-numOfWords",
         "P-betweenness",
         "numOfWords")%>% # numOfWords for keyword anaylsis stats
  left_join(
    shake_meta %>% select(playName, Genre, Comedy_Other),
    by = "playName"
  )


connectors <- all_characters_power1 %>%  
  filter(`P-betweenness` == "P1" & `P-numOfSpeechActs`!= "P1" & `P-numOfWords`!= "P1")

speakers <- all_characters_power1 %>% 
  filter(`P-betweenness` != "P1" & `P-numOfSpeechActs`== "P1"& `P-numOfWords`== "P1")

dominant <- all_characters_power1 %>%  
  filter(`P-betweenness` == "P1" & `P-numOfSpeechActs`== "P1" & `P-numOfWords`== "P1")

speakers_com <- speakers %>% filter(Genre == "Comedy")
speakers_trag <- speakers %>% filter(Genre != "Comedy")

conncetors_com <- connectors %>% filter(Genre == "Comedy")
conncetors_trag <- connectors %>% filter(Genre != "Comedy")

##  STEP 2 - Get spoken text for each group
library(rdracor)
library(dplyr)
library(purrr)


# Function to fetch text for one play
get_character_speech <- function(play, character) {
  spoken <- get_text_chr_spoken_bych(play = play, corpus = "shake", 
                                     as_data_frame = TRUE, split_text = TRUE)
  
  spoken %>% 
    filter(label == character) %>%
    select(label, text)   # no playName here
}


# Apply across your tibble of characters
# do it for connectors, speakers and dominant chars separately
char_speeches_connector <- connectors %>%
  mutate(
    speeches = map2(playName, name, get_character_speech)
  ) %>%
  unnest(speeches)

char_speeches_connector$version <- "conncetor"
char_speeches_speaker$version <- "speaker"
char_speeches_dominant$version <- "dominant"

# Combine into one
all_speech_power <- bind_rows(char_speeches_connector, 
                        char_speeches_speaker, 
                        char_speeches_dominant)%>%
  mutate(text = as.character(text))

### STEP 3 - Oppose

# tokenize and remove uppercase
tidy_speech <- all_speech_power %>%
  unnest_tokens(word, text, to_lower = FALSE) #%>%

#anti_join(stop_words, by = "word")

#exclude dominant chars
tidy_speech <- tidy_speech %>% 
  filter(version != "dominant")

# group words by version and collapse into text
version_texts <- tidy_speech %>%
  mutate(document = paste(Comedy_Other, version, sep = "_")) %>% 
  group_by(document) %>%
  summarise(text = paste(word, collapse = " "), .groups = "drop") %>%
  mutate(file_name = paste0("version_", document, ".txt"))

# Write each file and put it in primairly and secondary corpus folder
setwd("YOUR DIR")
walk2(version_texts$text, version_texts$file_name, write_file)

# text.slice.length = 700
# text.slice.overlap = 0
# rare.occurrences.threshold = 3
# zeta.filter.threshold = 0.1
# oppose.method = "craig.zeta"

res  <- oppose()
