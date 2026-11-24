# R dependencies

R 4.1 or later is required (`\` anonymous functions and the native pipe are used). The project depends on these packages:

| Package | Use |
| --- | --- |
| `dplyr` | Data manipulation |
| `purrr` | Iterating over plays and character data |
| `tidyr` | Reshaping and unnesting |
| `tidytext` | Tokenization and plot labels |
| `ggplot2` | Figures |
| `readr` | Writing text corpora |
| `rdracor` | ShakeDraCor API access |
| `stringr` | Text cleaning |
| `tibble` | Genre mapping and result tables |
| `stylo` | Craig's Zeta / `oppose()` analysis |
| `rstatix` | Effect-size calculation in the semantic analysis |
| `word2vec` | Word-embedding model |
| `tokenizers` | Sentence and word tokenization |
| `ggrepel` | Labels in the corpus-level figure |

Exact historical package versions were not recorded in the supplied repository or manuscript. Do not interpret a newly generated `renv.lock` as the original environment unless it is created from the original R library. The manuscript's ShakeDraCor commit is `781dd85`; the live API does not itself pin the returned corpus to that revision.
