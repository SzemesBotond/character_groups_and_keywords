# Craig's Zeta comparisons and keyword figures.
# Requires data/speech-text-results.rds from script 02.
# This script uses each scope's project-local oppose_config.txt; no manual file
# moving is needed. Execute from the repository root.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(tidytext)
library(stylo)

speech_results <- readRDS(file.path("data", "speech-text-results.rds"))
tidy_speech <- speech_results$tidy_speech

scope_rows <- function(data, scope) {
  if (scope == "comedy") {
    filter(data, Comedy_Other == "Comedy")
  } else if (scope == "other") {
    filter(data, Comedy_Other != "Comedy")
  } else {
    data
  }
}

prepare_zeta_input <- function(data, scope, shuffled = FALSE) {
  selected <- scope_rows(data, scope) %>%
    filter(version %in% c("connector", "speaker")) %>%
    group_by(version) %>%
    summarise(
      text = if (shuffled) {
        paste(sample(word, size = length(word), replace = FALSE), collapse = " ")
      } else {
        paste(word, collapse = " ")
      },
      .groups = "drop"
    )

  if (!all(c("connector", "speaker") %in% selected$version)) {
    stop("The ", scope, " comparison needs both connector and speaker texts.")
  }

  comparison_dir <- file.path(
    "data", "zeta-input", scope,
    if (shuffled) "shuffled" else "original"
  )
  primary_dir <- file.path(comparison_dir, "primary_set")
  secondary_dir <- file.path(comparison_dir, "secondary_set")
  dir.create(primary_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(secondary_dir, recursive = TRUE, showWarnings = FALSE)

  write_file(
    selected$text[selected$version == "connector"],
    file.path(primary_dir, paste0(scope, "_connectors.txt"))
  )
  write_file(
    selected$text[selected$version == "speaker"],
    file.path(secondary_dir, paste0(scope, "_speakers.txt"))
  )

  config_scope <- if (scope == "comedy") "comedy" else "other"
  file.copy(
    file.path("keyword-results", config_scope, "oppose_config.txt"),
    file.path(comparison_dir, "oppose_config.txt"),
    overwrite = TRUE
  )
  comparison_dir
}

run_oppose <- function(comparison_dir) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(comparison_dir)
  stylo::oppose()
}

keyword_table <- function(result) {
  as_tibble(result$comparison, rownames = "feature") %>%
    filter(feature != "c") %>%
    mutate(
      diff = comparison.secondary - comparison.primary,
      group = case_when(
        diff < 0 ~ "Connectors",
        diff > 0 ~ "Speakers",
        TRUE ~ NA_character_
      ),
      plot_value = abs(diff)
    ) %>%
    filter(diff != 0, !is.na(group)) %>%
    group_by(group) %>%
    arrange(desc(plot_value), .by_group = TRUE) %>%
    mutate(Rank = row_number()) %>%
    ungroup()
}

plot_keyword_results <- function(result, shuffled_result, scope) {
  main_keywords <- keyword_table(result)
  shuffled_keywords <- keyword_table(shuffled_result) %>%
    filter(Rank <= 100) %>%
    select(feature, group, Shuffled_Rank = Rank)

  raw_counts <- bind_rows(
    tibble(
      group = "Connectors",
      feature = unlist(result$primary.slices, use.names = FALSE)
    ),
    tibble(
      group = "Speakers",
      feature = unlist(result$secondary.slices, use.names = FALSE)
    )
  ) %>%
    count(group, feature, name = "shown_freq")

  plot_tbl <- main_keywords %>%
    filter(Rank <= 40) %>%
    left_join(shuffled_keywords, by = c("feature", "group")) %>%
    left_join(raw_counts, by = c("feature", "group")) %>%
    mutate(
      shown_freq = coalesce(shown_freq, 0L),
      marker = case_when(
        Shuffled_Rank <= 40 ~ "*",
        Shuffled_Rank <= 100 ~ "+",
        TRUE ~ ""
      ),
      feature_label = str_trim(paste(feature, marker)),
      group = factor(group, levels = c("Connectors", "Speakers"))
    )

  keyword_plot <- ggplot(
    plot_tbl,
    aes(
      x = plot_value,
      y = tidytext::reorder_within(feature_label, plot_value, group),
      fill = group
    )
  ) +
    geom_col(width = 0.95) +
    geom_text(aes(label = shown_freq), hjust = -0.1, size = 3) +
    tidytext::scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
    facet_wrap(vars(group), scales = "free_y", nrow = 1) +
    theme_minimal() +
    labs(x = "Zeta score", y = NULL) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(5.5, 15, 5.5, 5.5)
    )

  file_stem <- switch(
    scope,
    other = "figure_4a_keywords_other",
    comedy = "figure_4b_keywords_comedy",
    all = "figure_5_keywords_all"
  )
  output_dir <- file.path("figures", "generated")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    file.path(output_dir, paste0(file_stem, ".png")),
    plot = keyword_plot,
    width = 15,
    height = 20,
    units = "cm",
    dpi = 300,
    bg = "white"
  )

  list(
    original = result,
    shuffled = shuffled_result,
    keywords = main_keywords,
    plotted = plot_tbl,
    plot = keyword_plot
  )
}

set.seed(123)
scopes <- c("comedy", "other", "all")
zeta_results <- setNames(vector("list", length(scopes)), scopes)

for (scope in scopes) {
  message("Running Zeta comparisons for: ", scope)
  original_dir <- prepare_zeta_input(tidy_speech, scope, shuffled = FALSE)
  shuffled_dir <- prepare_zeta_input(tidy_speech, scope, shuffled = TRUE)
  original_result <- run_oppose(original_dir)
  shuffled_result <- run_oppose(shuffled_dir)
  zeta_results[[scope]] <- plot_keyword_results(
    original_result,
    shuffled_result,
    scope
  )
}

results_dir <- file.path("data", "zeta-results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(zeta_results, file.path(results_dir, "zeta-results.rds"))
