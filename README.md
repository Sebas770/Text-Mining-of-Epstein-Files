# Text Mining and Linguistic Network Analysis of Court Filings in the Epstein Case

Data and code for the article *Text Mining and Linguistic Network Analysis of Court Filings in the Epstein Case*, by Sebastián Romero Papagayo and Juan Camilo Sosa Martínez (Department of Statistics, Universidad Nacional de Colombia).

The study collects, cleans, and analyzes ten U.S. court filings (civil complaints and federal indictments, 2015–2022) related to the Epstein case that mention prominent individuals or institutions. Each filing is analyzed through word frequency, sentiment and emotion analysis, and bigram and skipgram networks; the ten filings are then analyzed jointly through latent Dirichlet allocation, the Jaccard index, and Spearman correlation.

**Supplementary Material S1** (complete analysis of each filing): <https://sebas770.github.io/Text-Mining-of-Epstein-Files/>

> **Disclaimer.** All documents in this repository are publicly available court filings. Complaints and indictments contain **allegations**, not findings of fact. The appearance of a person or institution in a filing, or in any result derived from it, does not imply guilt or civil liability, which only the competent courts can determine. The results describe the **language** of each filing, not the truth of its content.

## Corpus

| Date | Case | Type | Words |
|:-----------|:-------------------------------------|:-----------|-------:|
| 2015-09-21 | *Giuffre v. Maxwell* | Complaint | 2,554 |
| 2016-04-26 | *Johnson v. Trump and Epstein* | Complaint | 1,577 |
| 2019-04-16 | *Giuffre v. Dershowitz* | Complaint | 7,201 |
| 2019-07-02 | *United States v. Epstein* | Indictment | 2,706 |
| 2020-06-29 | *United States v. Maxwell* | Indictment | 3,171 |
| 2021-08-09 | *Giuffre v. Prince Andrew* | Complaint | 3,390 |
| 2022-11-24 | *Doe v. Deutsche Bank* | Complaint | 23,105 |
| 2022-11-24 | *Doe v. JP Morgan* | Complaint | 16,366 |
| 2022-11-28 | *Pierson v. Black* | Complaint | 7,856 |
| 2022-12-27 | *Government of the USVI v. JP Morgan* | Complaint | 6,067 |
| | **Total** | | **73,993** |

*Words*: number of word tokens in the cleaned text of each filing. USVI: United States Virgin Islands.

## Repository structure

Each filing is identified by a name of the form `date-type-case` (for example, `2015-09-21-Complaint-Giuffre-v.-Maxwell`), where the date is the filing date and the type is either `Complaint` or `Indictment`.

The two folders needed to reproduce the study are:

```
Pre-Processed-and-Meta-Data/      # one folder per filing
    <filing>/
        <original>.pdf             # original document
        <filing>.txt               # raw text extracted from the PDF
        <filing>-Cleaning.R        # cleaning script
        <filing>-C.txt             # cleaned text used in the analyses
        <filing>-info.txt          # source and processing notes

Data-Analysis/
    <case>/                        # one folder per filing
        <filing>-C.txt             # cleaned text
        <case>-Analysis.R          # individual analysis
    Joint-Analysis/
        *-C.txt                    # the ten cleaned texts
        joint-analysis.R           # word counts, LDA, Jaccard index, Spearman correlation
```

The `-info.txt` files indicate where each document was obtained (CourtListener, the U.S. Department of Justice, or the New York State Courts Electronic Filing system) and whether it required special processing: *Johnson v. Trump and Epstein* and *United States v. Maxwell* were transcribed manually because their PDFs are scanned images, and the redacted passages of *Government of the USVI v. JP Morgan* appear as blanks.

The remaining files and folders (`_quarto.yml`, `index.qmd`, `cases/`, `_freeze/`, and `docs/`) contain the source and the rendered version of the Supplementary Material website, and are **not** needed to reproduce the analyses.

## Reproducing the analyses

### Requirements

The analyses were carried out in R. Install the required packages with:

```r
install.packages(c(
  "tidyverse", "readr", "magrittr", "pdftools",
  "tidytext", "textdata", "ngram", "scales",
  "wordcloud", "ggwordcloud", "RColorBrewer", "reshape2",
  "igraph", "ggraph", "tidygraph",
  "topicmodels"
))
```

The AFINN and NRC sentiment lexicons are downloaded by the `textdata` package the first time they are used. Run the following lines once in the R console and accept the download (type `1` or `yes` when asked):

```r
tidytext::get_sentiments("afinn")
tidytext::get_sentiments("nrc")
```

### Steps

The scripts read and write files using relative names, so each script must be run with its own folder as the working directory (in RStudio: *Session → Set Working Directory → To Source File Location*).

1. **Cleaning (optional).** The cleaned texts (`-C.txt`) are already included. To regenerate them, run the `-Cleaning.R` script of each filing in `Pre-Processed-and-Meta-Data/`.
2. **Individual analyses.** Run the `-Analysis.R` script of each filing in `Data-Analysis/`. It produces the word frequency charts, sentiment and emotion analysis, bigram and skipgram networks, network statistics, eigenvector centrality, and clusters.
3. **Pooled analysis.** Run `Data-Analysis/Joint-Analysis/joint-analysis.R`. It computes the number of words per filing, selects the number of topics through perplexity, fits the final latent Dirichlet allocation model (`k = 6`, collapsed Gibbs sampling with seed `777`), and computes the Jaccard index and Spearman correlation for each pair of filings.

**Note on the topic model.** With only ten documents, the assignment of some filings to topics depends on the random initialization of the Gibbs sampler. The seed is fixed for reproducibility; results obtained with other seeds or package versions may group some filings differently (see the Discussion section of the article).

## Supplementary Material website

The website was written in [Quarto](https://quarto.org/). Each page runs the same code as the analysis scripts when the site is rendered, so all figures and tables are generated from the cleaned texts; the code is hidden from the reader.

To render the site locally, install [Quarto](https://quarto.org/docs/get-started/) and the R packages `quarto`, `knitr`, and `rmarkdown`, open R in the root of the repository, and run:

```r
quarto::quarto_render()
```

The rendered site is written to `docs/`, which is published through GitHub Pages. Thanks to the `freeze` option, only the pages whose source has changed are re-executed.

## Use of AI tools

During the preparation of this work the authors used Claude Opus 5.5 (Anthropic) to translate the original Spanish text and code comments into English, improve language and readability, and assist with the formatting of the manuscript and the supplementary material. The authors reviewed and edited the content as needed and take full responsibility for it.
