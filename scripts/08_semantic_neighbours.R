# Word2vec semantic neighbours and lexical-category analyses.
# Requires data/speech-text-results.rds from script 02.

library(rstatix)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(rdracor)
library(word2vec)
library(tokenizers)

speech_results <- readRDS(file.path("data", "speech-text-results.rds"))
all_speech_power1 <- speech_results$all_speech_power1
if (is.null(all_speech_power1)) {
  stop(
    "The speech cache does not contain all_speech_power1. ",
    "Run scripts/02_collect_speech_texts.R again."
  )
}
all_speech_power <- all_speech_power1 %>%
  mutate(version = dplyr::recode(
    as.character(version),
    conncetor = "connector"
  ))

# Lexical categories defined in Appendix C of the manuscript.
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

cognitive_words <- c(
  "think", "know", "believe", "remember", "forget",
  "knows", "remembered", "forgot",
  "perceive", "mean", "learn", "trust", "understand",
  "why", "how"
)

hierarchy_words <- c( "king", "queen", "prince", "princess", "noble", "royal",   
                      #no duke and eral because of names
                      "lord", "lords", "lordship", "lady", "ladyship", "sir",     
                      "madam","master", "mistress",
                      "liege", "grace", "highness", "majesty",
                      "servant", "knight",  "doctor",
                      "please","pleaseth", "pardon", "thank", "thanks",
                      "humbly","welcome", "gramercy")

discourse_words <- c(
  "ha", "ho", "oh", "o",
  "ay", "nay",
  "fie", "foh",
  "hey", "heigh",
  "hark"
)

# Table 3: character-level frequencies for the five lexical categories.
# Frequencies are per 1,000 words. Self-Other is first-person minus
# third-person pronoun frequency, matching the manuscript's definition.
self_words <- c("i", "me", "my", "myself", "we", "us", "our", "ours", "ourselves")
others_words <- c(
  "he", "him", "his", "himself", "she", "her", "hers", "herself",
  "they", "them", "their", "themselves"
)

Table3_character_scores <- all_speech_power %>%
  filter(version %in% c("connector", "speaker")) %>%
  mutate(
    text_collapsed = map_chr(text, ~ paste(.x, collapse = " ")),
    text_collapsed = stringr::str_remove(text_collapsed, "^c\\("),
    tokens = tokenizers::tokenize_words(
      text_collapsed,
      lowercase = TRUE,
      strip_punct = TRUE,
      strip_numeric = TRUE
    ),
    total_words = lengths(tokens),
    n_Communication = map_int(tokens, ~ sum(.x %in% communication_lexicon)),
    n_Discourse = map_int(tokens, ~ sum(.x %in% discourse_words)),
    n_Cognition = map_int(tokens, ~ sum(.x %in% cognitive_words)),
    n_Hierarchy = map_int(tokens, ~ sum(.x %in% hierarchy_words)),
    n_self = map_int(tokens, ~ sum(.x %in% self_words)),
    n_other = map_int(tokens, ~ sum(.x %in% others_words))
  ) %>%
  mutate(
    Communication = 1000 * n_Communication / pmax(total_words, 1),
    Discourse = 1000 * n_Discourse / pmax(total_words, 1),
    Cognition = 1000 * n_Cognition / pmax(total_words, 1),
    Hierarchy = 1000 * n_Hierarchy / pmax(total_words, 1),
    `Self-Other` = 1000 * (n_self - n_other) / pmax(total_words, 1)
  ) %>%
  select(-text_collapsed, -tokens)

format_p <- function(p) {
  if (p < 0.001) {
    "<0.001"
  } else {
    formatted <- sub("0+$", "", sprintf("%.3f", p))
    sub("\\.$", "", formatted)
  }
}

summarise_category_scope <- function(scores, category, scope) {
  scope_data <- if (scope == "Comedy") {
    filter(scores, Comedy_Other == "Comedy")
  } else if (scope == "Other") {
    filter(scores, Comedy_Other != "Comedy")
  } else {
    scores
  }

  category_data <- scope_data %>%
    select(version, score = all_of(category)) %>%
    filter(is.finite(score))
  group_sizes <- count(category_data, version)
  if (!all(c("connector", "speaker") %in% group_sizes$version) ||
      any(group_sizes$n < 2)) {
    stop("Not enough connector and speaker characters for ", category, " in ", scope, ".")
  }

  means <- category_data %>%
    group_by(version) %>%
    summarise(mean_score = mean(score), .groups = "drop")
  connector_mean <- means$mean_score[means$version == "connector"]
  speaker_mean <- means$mean_score[means$version == "speaker"]
  test_result <- wilcox.test(score ~ version, data = category_data)
  effect <- rstatix::wilcox_effsize(category_data, score ~ version)$effsize[[1]]

  tibble(
    category = category,
    scope = scope,
    connector_mean = connector_mean,
    speaker_mean = speaker_mean,
    p_value = test_result$p.value,
    r = effect
  )
}

category_names <- c(
  "Communication", "Discourse", "Self-Other", "Cognition", "Hierarchy"
)
scopes <- c("All", "Comedy", "Other")
Table3_long <- purrr::map_dfr(category_names, function(category) {
  purrr::map_dfr(scopes, ~ summarise_category_scope(
    Table3_character_scores, category, .x
  ))
})

Table3 <- Table3_long %>%
  group_by(category) %>%
  summarise(
    Direction = if_else(
      first(connector_mean) > first(speaker_mean), "C>S", "S>C"
    ),
    `ALL Mean (C/S)` = sprintf("%.2f / %.2f", first(connector_mean), first(speaker_mean)),
    `ALL p (r)` = paste0(format_p(p_value[scope == "All"]),
                         " (", sprintf("%.2f", r[scope == "All"]), ")"),
    `Comedy p (r)` = paste0(format_p(p_value[scope == "Comedy"]),
                            " (", sprintf("%.2f", r[scope == "Comedy"]), ")"),
    `Other p (r)` = paste0(format_p(p_value[scope == "Other"]),
                           " (", sprintf("%.2f", r[scope == "Other"]), ")"),
    .groups = "drop"
  ) %>%
  rename(Category = category) %>%
  select(Category, Direction, `ALL Mean (C/S)`, `ALL p (r)`, `Comedy p (r)`, `Other p (r)`) %>%
  arrange(match(Category, category_names))

print(Table3)
dir.create("results", showWarnings = FALSE)
write.csv(Table3, file.path("results", "table3_semantic_category_tests.csv"), row.names = FALSE)
saveRDS(
  list(character_scores = Table3_character_scores, details = Table3_long, table = Table3),
  file.path("data", "table3_semantic_category_tests.rds")
)


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

# Inspect vocabulary size (optional)
# summary(w2v_model, type = "vocabulary")

# OPTIONAL: Get embedding matrix if you want it
# emb <- as.matrix(w2v_model)
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


seed_terms <- c("think", "know", "believe", "remember", "forget")


seed_terms <- c("lord", "master", "sir", "madam", "king", "lady")


seed_terms <- c("ho", "ha", "oh", "ay")


seed_terms <- c("therefore", "how", "why", "because")
