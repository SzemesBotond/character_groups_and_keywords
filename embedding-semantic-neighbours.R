library(rstatix)
library(rdracor)
library(word2vec)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)
library(tokenizers)

# Get all Shakespeare plays

shake_meta <- get_dracor(corpus = "shake")

play_ids <- shake_meta$name

#  Download spoken text only
# Collapse each play into one long string, but keep sentence boundaries for tokenisation.
shake_texts <- map_chr(
  play_ids,
  \(p) {
    txt <- get_text_chr_spoken(play = p, corpus = "shake")
    paste(txt, collapse = " ")
  }
)

names(shake_texts) <- play_ids

#  Minimal normalization
clean_text <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[[:digit:]]+", " ") |>
    str_replace_all("[-—–]", " ") |>
    str_replace_all("[^[:alpha:]'[:space:].!?;:]", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

shake_texts_clean <- vapply(shake_texts, clean_text, character(1))

# Split into sentence-level token lists
sentence_lists <- tokenize_sentences(shake_texts_clean)

sentence_token_lists <- sentence_lists |>
  map(function(sentences) {
    tokenize_words(
      sentences,
      lowercase = TRUE,
      strip_punct = TRUE,
      strip_numeric = TRUE
    )
  }) |>
  flatten()

# Remove empty token vectors
sentence_token_lists <- keep(sentence_token_lists, ~ length(.x) > 1)

# Train a static word2vec model
# Settings:
# - skip-gram: better for semantic neighbors
# - dim = 100: reasonable for a corpus this size
# - window = 8: broad enough for drama
# - min_count = 5: remove very rare words
# - iter = 30: stable enough for a smaller corpus
set.seed(1234)

w2v_model <- word2vec(
  x         = sentence_token_lists,
  type      = "skip-gram",
  dim       = 100,
  window    = 8,
  iter      = 30,
  min_count = 5,
  negative  = 10,
  sample    = 0.001,
  threads   = 1
)

'''
# Inspect vocabulary size
summary(w2v_model, type = "vocabulary")

# OPTIONAL: Get embedding matrix if you want it
emb <- as.matrix(w2v_model)
'''
# Query nearest neighbors

# Helper: return neighbors only for words that are in the model
nearest_terms <- function(model, seed_term, top_n = 25) {
  nn <- predict(model, seed_term, type = "nearest", top_n = top_n)
  
  # unwrap the one-element list
  nn <- nn[[1]]
  
  # turn into ordinary data.frame
  nn <- as.data.frame(nn, stringsAsFactors = FALSE)
  
  # build a clean long table
  data.frame(
    seed = seed_term,
    term = nn[[2]],         # neighbor word = term2
    similarity = nn[[3]],   # similarity column
    rank = nn[[4]],         # rank column
    stringsAsFactors = FALSE
  )
}

# Build lexicons based on seed_terms and nearest neighbor list manually

seed_terms <- c("say", "tell", "speak", "ask", "answer", "pray", "write", "read")

neighbors_raw <- bind_rows(lapply(seed_terms, function(s) {
  nearest_terms(w2v_model, s, top_n = 25)
}))

neighbors_raw$term


communication_lexicon <- c(
  "say", "said", "says",
  "tell", "told",
  "speak", "spoke",
  "ask", "answer",
  "hear",
  "call", "bid",
  "pray", "prithee",
  "beseech", "entreat",
  "request", "demand",
  "beg",
  "word", "words", "question",
  "write", "written", "writing", "writ", "read"
)

# Others after manual creation of wordlist from semantic neighbours
seed_terms <- c("send", "bring", "deliver", "message", "letter", "return")

mediation_words <- c(
  "send", "sent",
  "deliver", "delivered",
  "message", "messenger",
  "letter", "letters",
  "errand", "command",
  
  "bring", "brought",
  "return", "returned", "returns",
  "follow",
  "convey"
  )

seed_terms <- c("think", "know", "believe", "remember", "forget")
cognitive_words <- c(
  "think", "know", "believe", "remember", "forget",
  "knows", "remembered", "forgot",
  "perceive", "mean", "learn", "trust", "understand",
  "why", "how"
)

seed_terms <- c("lord", "master", "sir", "madam", "king", "lady")
hierarchy_words <- c( "king", "qeen", "prince", "princess", "noble", "royal",   
                      #no duke and eral because of names
                      "lord", "lords", "lordship", "lady", "ladyship", "sir",     
                      "madam","master", "mistress",
                      "liege", "grace", "highness", "majesty",
                      "servant", "knight",  "doctor",
                      "please","pleaseth", "pardon", "thank", "thanks",
                      "humbly","welcome", "gramercy")


seed_terms <- c("ho", "ha", "oh", "ay")
discourse_words <- c(
  "ha", "ho", "oh", "o",
  "ay", "nay",
  "fie", "foh",
  "hey", "heigh",
  "hark"
)

seed_terms <- c("therefore", "how", "why", "because")

# ANALYIS - per char and group - from drama_chargroup_and_keywords.R

df <- all_speech_power1 %>%
  mutate(
    text_collapsed = map_chr(text, ~ paste(.x, collapse = " "))
  )

speaker_connector_counts <- df %>%
  mutate(
    tokens = strsplit(tolower(text_collapsed), "\\s+"),
    n_tokens = lengths(tokens),
    
    # here the category
    n_lex = map_int(tokens, ~ sum(.x %in% possibility_words)), 
    freq_per_1000 = 1000 * n_lex / pmax(n_tokens, 1)
  )


speaker_connector_counts <- speaker_connector_counts %>% 
  filter(version != "dominant")

speaker_connector_com_counts <- speaker_connector_counts %>% 
  filter(Comedy_Other == "Comedy")

speaker_connector_other_counts <- speaker_connector_counts %>% 
  filter(Comedy_Other != "Comedy")


speaker_connector_counts   %>% 
  group_by(version) |>
  summarise(
    mean_freq = mean(freq_per_1000),
    median_freq = median(freq_per_1000),
    .groups = "drop"
  )
  
wilcox.test(
  freq_per_1000 ~ version,
  data = speaker_connector_counts
)

wilcox_effsize(
  speaker_connector_counts,
  freq_per_1000 ~ version
)

## SELF and OTHER reference combined effect

self_words <- c("i", "me", "my", "myself", "we", "us", "our", "ours", "ourselves")
others_words <- c("he", "him", "his","himself", "she", "her", "hers", "herself",
                  'they', "them", "their", "themselves")



speaker_connector_counts <- df %>%
  mutate(
    tokens = strsplit(tolower(text_collapsed), "\\s+"),
    n_tokens = lengths(tokens),
    
    n_self  = map_int(tokens, ~ sum(.x %in% self_words)),
    n_other = map_int(tokens, ~ sum(.x %in% others_words)),
    
    self_per_1000  = 1000 * n_self  / pmax(n_tokens, 1),
    other_per_1000 = 1000 * n_other / pmax(n_tokens, 1),
    
    self_other_score = self_per_1000 - other_per_1000
  )



speaker_connector_counts <- speaker_connector_counts %>% 
  filter(version != "dominant")

speaker_connector_com_counts <- speaker_connector_counts %>% 
  filter(Comedy_Other == "Comedy")

speaker_connector_other_counts <- speaker_connector_counts %>% 
  filter(Comedy_Other != "Comedy")



speaker_connector_counts   %>% 
  group_by(version) |>
  summarise(
    mean_freq = mean(self_other_score),
    median_freq = median(self_other_score),
    .groups = "drop"
  )


wilcox.test(
  self_other_score ~ version,
  data = speaker_connector_com_counts
)

wilcox_effsize(
  speaker_connector_com_counts,
  self_other_score ~ version
)

