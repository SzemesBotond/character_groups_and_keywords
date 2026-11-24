# Repository for paper Contrasting Verbal Prominence and Network Centrality: A Typology of Dramatic Characters

Code and keyword results for Botond Szemes (2026), *Journal of Computational Literary Studies* 5(1), DOI: 10.48694/jcls.4744.

## Project layout

- `scripts/`: numbered analysis stages.
- `data/`: generated RDS files, pooled texts, and the input corpora prepared for `stylo::oppose()`.
- `keyword-results/`: keyword lists and Zeta settings supplied with the project.
- `figures/paper/`: figures extracted from the manuscript proof.
- `figures/generated/`: figures written by the scripts.
- `results/`: generated tables.

Run scripts from the project root. The first three stages write intermediate RDS files so later scripts can be run in a fresh R session.

## Run order

1. `scripts/01_character_groups.R` retrieves ShakeDraCor character data, joins the manually coded genres by play ID, assigns play-level groups, checks the mapping, and saves `data/character-groups-results.rds`.
2. `scripts/02_collect_speech_texts.R` downloads each selected play once, matches speeches by character ID, prepares pooled speech texts, and saves `data/speech-text-results.rds` plus `data/keyword-texts/`.
3. `scripts/03_zeta_keywords.R` prepares the `primary_set/` and `secondary_set/` inputs under `data/zeta-input/`, runs original and shuffled Craig’s Zeta comparisons for Comedy, Other, and all plays, and saves results under `data/zeta-results/`. It writes Figures 4a, 4b, and 5 under `figures/generated/`.
4. `scripts/04_corpus_level_clustering.R` performs corpus-level clustering and writes Figure 3 plus `data/corpus-clustering-results.rds`.
5. `scripts/05_romeo_comparison.R` reads the saved play- and corpus-level results, compares cluster methods for *Romeo and Juliet*, and writes a CSV under `results/`.

The following scripts can run independently:

- `scripts/06_centrality_correlations.R` writes Figure 1.
- `scripts/07_centrality_distributions.R` writes Figure 2.

`scripts/08_semantic_neighbours.R` trains the word2vec model and compares the communication and first-person/other-person word categories. It reads `data/speech-text-results.rds` from step 2.

For example, from an R session opened in the project root:

```r
source("scripts/01_character_groups.R")
source("scripts/02_collect_speech_texts.R")
source("scripts/03_zeta_keywords.R")
source("scripts/04_corpus_level_clustering.R")
source("scripts/05_romeo_comparison.R")
```

The DraCor scripts use `rdracor`'s default API endpoint.  The paper identifies ShakeDraCor commit `781dd85` as the version used for the study.

## Keyword-analysis settings

The supplied `keyword-results/*/oppose_config.txt` files preserve the paper settings: 700-word slices, no overlap, rare-occurrence threshold 3, Zeta filter threshold 0.1, and `craig.zeta`. The Zeta script copies the matching config into each prepared comparison folder and temporarily runs `oppose()` there, so the user does not need to move input files by hand. It creates original and shuffled comparisons for comedy, other genres, and all plays.

## Figures

The five paper figures are included in `figures/paper/`; Figure 4 is also preserved as its two printed panels. Script output names and regeneration commands are listed in [figures/README.md](figures/README.md). 

## Requirements and versions

Use R 4.1 or later. The scripts use `dplyr`, `purrr`, `tidyr`, `tidytext`, `ggplot2`, `readr`, `rdracor`, `stringr`, `tibble`, `stylo`, `rstatix`, `word2vec`, `tokenizers`, and `ggrepel`.


See [DEPENDENCIES.md](DEPENDENCIES.md) for package roles and version notes.
