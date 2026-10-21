#### Joint analysis of the ten court filings:
#### corpus size, LDA, Jaccard index, and Spearman correlation

# 0. Load libraries
library(tidyverse)
library(magrittr)
library(tidytext)
library(topicmodels)
library(scales)

data(stop_words)


#### Load data ----------

# File names of the ten cleaned documents (*-C.txt)
files <- list.files(pattern = "\\.txt$")

# Load all files into a tibble (one row per document)
full_texts <- tibble(
  document = files,
  text     = map_chr(files, read_file)
)


#### Words per document ----------

words_per_document <- full_texts %>%
  unnest_tokens(word, text) %>%                          # one word per row
  count(document, name = "total_words") %>%              # all words
  left_join(
    full_texts %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%             # drop stop words
      count(document, name = "words_no_stopwords"),
    by = "document"
  ) %>%
  arrange(document)

words_per_document

# Corpus total
words_per_document %>%
  summarise(across(where(is.numeric), sum))


####### LDA ---------------

# Tokenization (one word per row)
text_tidy <- full_texts %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!is.na(word))

# Token cleaning: remove tokens containing numbers and stop words
text_tidy %<>%
  filter(!grepl(pattern = "[0-9]", x = word))

text_tidy %<>%
  anti_join(x = ., y = stop_words, by = "word")

# Word counts by document
word_counts <- text_tidy %>%
  count(document, word, sort = TRUE)

# Document-term matrix
dtm <- word_counts %>%
  cast_dtm(document = document, term = word, value = n)

# Selection of the number of topics k through perplexity
k_values     <- 2:11
perplexities <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k_current <- k_values[i]
  
  # Fit a quick model for each k
  model_temp <- LDA(dtm, k = k_current, method = "Gibbs",
                    control = list(seed = 1234, iter = 1000, burnin = 500))
  
  # Store the perplexity computed by the package
  perplexities[i] <- perplexity(model_temp, dtm)
}

# Elbow plot: perplexity vs. number of topics
par(mfrow = c(1, 1), mar = c(3, 3, 2, 0.5), mgp = c(1.7, 0.7, 0))

plot(k_values, perplexities,
     type = "b",
     pch  = 21,
     col  = "black",
     bg   = adjustcolor("darkred", 0.8),
     lwd  = 2,
     cex  = 1.4,
     xlab = "k",
     ylab = "Perplexity")

# Optimal value: k = 6

# Gibbs sampler settings
control_gibbs <- list(
  seed   = 1234,
  burnin = 5000,
  iter   = 20000,
  keep   = 10      # keep the log-likelihood every 10 iterations
)

# Fit the final model
lda_model <- LDA(dtm, k = 6, method = "Gibbs", control = control_gibbs)
lda_model

# Log-likelihood trace (convergence check)
ll <- lda_model@logLiks

par(mfrow = c(1, 1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))

plot(ll[-c(1:10)], type = "p", pch = 16, col = adjustcolor(1, 0.8), cex = 1.2,
     xlab = "Iteration", ylab = "Log-likelihood")

# Extract the beta matrix (word probabilities per topic)
topics_beta <- tidy(lda_model, matrix = "beta")

# Top 10 words per topic
top_terms <- topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Bar chart: most probable words per topic
ggplot(top_terms, aes(x = beta,
                      y = reorder_within(term, beta, topic),
                      fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "turbo", alpha = 0.85) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    strip.text    = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),   # remove horizontal grid lines
    axis.text.y   = element_text(color = "black")
  ) +
  labs(
    x = "Probability (beta)",
    y = NULL
  )

# Distinctive terms per topic: log2 ratio between the topic and the mean of the other topics
exclusive_terms <- topics_beta %>%
  group_by(term) %>%
  filter(sum(beta) > 1e-4) %>%
  mutate(
    beta_mean_others = (sum(beta) - beta) / (n() - 1),
    log_ratio        = log2(beta / (beta_mean_others + 1e-10))
  ) %>%
  ungroup() %>%
  group_by(topic) %>%
  slice_max(log_ratio, n = 10) %>%
  ungroup() %>%
  arrange(topic, -log_ratio)

# Bar chart: distinctive terms per topic
ggplot(exclusive_terms, aes(x = log_ratio,
                            y = reorder_within(term, log_ratio, topic),
                            fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "plasma", alpha = 0.85) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    strip.text    = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    axis.text.y   = element_text(color = "black")
  ) +
  labs(
    x = "Exclusivity (log2 ratio)",
    y = NULL
  )

# Theta: topic proportions of each document
doc_theta <- tidy(lda_model, matrix = "gamma") %>%
  mutate(
    topic    = factor(topic, levels = 1:6, labels = paste("Topic", 1:6)),
    document = document %>%
      str_remove("^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-") %>%   # remove date and document type
      str_remove("-C\\.txt$") %>%
      str_replace_all("-", " ")
  )

# Stacked bar chart: topic proportions per document
ggplot(doc_theta, aes(x = gamma, y = document, fill = topic)) +
  geom_col(position = "fill", alpha = 0.9) +
  scale_fill_brewer(palette = "Set1", name = "Topics") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    axis.text.y   = element_text(color = "black", face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x = "Document proportion (theta)",
    y = "Documents"
  )


######### Jaccard index (lexical overlap) ----------

# Short document names (e.g., "Doe v. JP Morgan")
text_tidy_clean <- text_tidy %>%
  mutate(
    document = str_remove(document, "^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-"),
    document = str_remove(document, "-C\\.txt$"),
    document = str_replace_all(document, "-", " ")
  )

# Relative frequency of each word in each document (one column per document)
freq <- text_tidy_clean %>%
  count(document, word) %>%
  group_by(document) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(document, proportion, fill = 0)

doc_names <- setdiff(names(freq), "word")
n_docs    <- length(doc_names)

# Empty matrix to store the results
jaccard_matrix <- matrix(0, nrow = n_docs, ncol = n_docs,
                         dimnames = list(doc_names, doc_names))

# Jaccard index for each pair of documents:
# words in both documents / words in at least one of them
for (i in 1:n_docs) {
  for (j in 1:n_docs) {
    
    if (i == j) {
      jaccard_matrix[i, j] <- NA   # skip the document compared with itself
    } else {
      doc_A <- doc_names[i]
      doc_B <- doc_names[j]
      
      # Words appearing in at least one of the two documents (union)
      freq_pair <- freq %>%
        select(word, all_of(doc_A), all_of(doc_B)) %>%
        filter(.data[[doc_A]] != 0 | .data[[doc_B]] != 0)
      
      # Words appearing in both documents (intersection)
      freq_common <- freq_pair %>%
        filter(.data[[doc_A]] != 0, .data[[doc_B]] != 0)
      
      jaccard <- dim(freq_common)[1] / dim(freq_pair)[1]
      
      jaccard_matrix[i, j] <- jaccard
    }
    
  }
}

# Long format for plotting
jaccard_long <- as.data.frame(jaccard_matrix) %>%
  rownames_to_column(var = "Doc1") %>%
  pivot_longer(cols = -Doc1, names_to = "Doc2", values_to = "Jaccard") %>%
  filter(!is.na(Jaccard))

# Heat map of the Jaccard index
ggplot(jaccard_long, aes(x = Doc1, y = Doc2, fill = Jaccard)) +
  geom_tile(color = "white", linewidth = 0.4) +
  
  # Labels in black bold text
  geom_text(aes(label = scales::percent(Jaccard, accuracy = 0.1)),
            color = "black",
            size = 3.2, fontface = "bold") +
  
  # Purple palette: light lilac (low) to medium amethyst (high)
  scale_fill_gradient(low = "#f4ebf8", high = "#c39bd3",
                      labels = scales::percent_format(), name = "Jaccard index") +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", face = "bold"),
    axis.text.y   = element_text(color = "black", face = "bold"),
    panel.grid    = element_blank(),
    aspect.ratio  = 1
  ) +
  labs(
    x = NULL, y = NULL
  )

# Shared terms for selected pairs of documents
top_shared_terms <- function(doc_A, doc_B, data = freq, n_words = 10) {
  data %>%
    select(word, all_of(doc_A), all_of(doc_B)) %>%
    filter(.data[[doc_A]] != 0, .data[[doc_B]] != 0) %>%
    # Sort by the sum of the relative frequencies in both documents
    mutate(combined_share = .data[[doc_A]] + .data[[doc_B]]) %>%
    arrange(desc(combined_share)) %>%
    head(n = n_words)
}

# Pair 1: Doe v. JP Morgan & Doe v. Deutsche Bank
top_pair_1 <- top_shared_terms("Doe v. JP Morgan", "Doe v. Deutsche Bank")

# Pair 2: Giuffre v. Maxwell & Giuffre v. Dershowitz
top_pair_2 <- top_shared_terms("Giuffre v. Maxwell", "Giuffre v. Dershowitz")

# Pair 3: Government v. JP Morgan & Doe v. JP Morgan
top_pair_3 <- top_shared_terms("Government v. JP Morgan", "Doe v. JP Morgan")

# Pair 4: USA v. Maxwell & USA v. Epstein
top_pair_4 <- top_shared_terms("USA v. Maxwell", "USA v. Epstein")

print(top_pair_1)
print(top_pair_2)
print(top_pair_3)
print(top_pair_4)


###### Spearman correlation ---------------

# Term-document matrix (one column per document)
freq_matrix <- t(as.matrix(dtm))

colnames(freq_matrix) <- colnames(freq_matrix) %>%
  str_remove("^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-") %>%
  str_remove("-C\\.txt$") %>%
  str_replace_all("-", " ")

# Spearman correlation between the word frequencies of each pair of documents
cor_matrix <- cor(freq_matrix, method = "spearman")

cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Doc1") %>%
  pivot_longer(cols = -Doc1, names_to = "Doc2", values_to = "Correlation")

# Remove the diagonal (document compared with itself)
cor_long_clean <- cor_long %>%
  filter(as.character(Doc1) != as.character(Doc2))

# Heat map of the Spearman correlation
ggplot(cor_long_clean, aes(x = Doc1, y = Doc2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  # White labels on dark cells (correlation > 0.7), black otherwise
  geom_text(aes(label = sprintf("%.2f", Correlation),
                color = Correlation > 0.7),
            size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  
  scale_fill_gradient(low = "#f4f4f6", high = "#003f5c",
                      limits = c(0, 1), name = "Spearman") +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", face = "bold"),
    axis.text.y   = element_text(color = "black", face = "bold"),
    panel.grid    = element_blank(),
    aspect.ratio  = 1
  ) +
  labs(
    x = NULL,
    y = NULL
  )