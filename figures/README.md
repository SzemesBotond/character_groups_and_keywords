# Figure inventory and reproduction



| Paper figure | Description | Regeneration |
| --- | --- | --- |
| 1 | Correlation of centrality scores with number of words and speech acts | `source("scripts/06_centrality_correlations.R")`; writes `generated/figure_1_centrality_correlations.png`. |
| 2 | Centrality distributions among characters speaking more than 500 words | `source("scripts/07_centrality_distributions.R")`; writes `generated/figure_2_centrality_distributions.png`. |
| 3 | Z-scaled verbal prominence and betweenness, with labels for top 5% on any measure | Run `scripts/01_character_groups.R`, then `scripts/04_corpus_level_clustering.R`; writes `generated/figure_3_character_positions.png`. |
| 4a | Top 40 Connector and Speaker keywords for histories and tragedies | Run scripts 01–03; writes `generated/figure_4a_keywords_other.png`. |
| 4b | Top 40 Connector and Speaker keywords for comedies | Run scripts 01–03; writes `generated/figure_4b_keywords_comedy.png`. |
| 5 | Top 40 keywords across the full corpus | Run scripts 01–03; writes `generated/figure_5_keywords_all.png`. |

The Zeta script also creates shuffled-text comparisons, marks keywords that recur among the shuffled top 40 or top 100, and includes raw slice counts in the bar labels. It stores the `stylo::oppose()` objects and plot data in `data/zeta-results/zeta-results.rds`.

