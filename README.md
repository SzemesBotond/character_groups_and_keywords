# A Bottom-Up Analysis of Character Function and Language
Codes and data for the paper "Rethinking Protagonists. A Bottom-Up Analysis of Character Function and Language." JCLS 2026

The script _drama_chargroup_and_keywords.R_ analyzes a DraCor corpus and groups characters according to network-based and count-based metrics. By default, it analyzes the ShakeDraCor corpus and uses betweenness centrality together with the number of words and speech acts per character. Both the corpus and the set of metrics can be easily modified or extended.

In a subsequent step, the script collects the speech acts of characters within each group via the DraCor API and contrasts their word usage using the _stylo_ package’s _oppose_ function (an implementation of Craig’s Zeta method). The folder keyword-results contains the extended keyword lists produced by this analysis.

See ShakeDraCor: https://github.com/dracor-org/shakedracor/tree/main/.github (the version used in this study corresponds to Git commit 781dd85)
see also: dracor.org
stylo documentation: https://cran.r-project.org/web/packages/stylo/stylo.pdf



