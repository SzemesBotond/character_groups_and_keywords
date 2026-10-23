# Contrasting Verbal Prominence and Network Centrality: A Typology of Dramatic Characters
Codes and data for the paper in JCLS 2026

The script _drama_chargroup_and_keywords.R_ analyzes a DraCor corpus and groups characters according to network-based and count-based metrics. By default, it analyzes the ShakeDraCor and uses betweenness centrality together with the number of words and speech acts per character. Both the corpus and the set of metrics can be easily modified or extended.

In a subsequent step, the script collects the speech acts of characters within each group via the DraCor API and contrasts their word usage using the _stylo_ package’s _oppose_ function (an implementation of Craig’s Zeta method). The folder keyword-results contains the extended keyword lists produced by this analysis.

_centrality_correlation.R_ measures the correlation between centrality metrics and number of words/speech acts for major characters (by default 5 or 7 chars with highest number of words)

_z_scale.R_ performs character clusterization at corpus-level (the method proposed in _drama_chargroup_and_keywords.R_ works at play level). _romeo-comparison.R_ compares different clusterization methods of a play's characters (by default _Romeo and Juliet_)

_embedding-semantic-neighbours.R_ uses word2vec to create semantic space of the corpus, then searches for top 25 semantic neighbours of seed terms, creating word categories. Then also analyses the distribution of word categories in character groups.

See ShakeDraCor: https://github.com/dracor-org/shakedracor/tree/main/.github (the version used in this study corresponds to Git commit 781dd85)
see also: dracor.org
stylo documentation: https://cran.r-project.org/web/packages/stylo/stylo.pdf



