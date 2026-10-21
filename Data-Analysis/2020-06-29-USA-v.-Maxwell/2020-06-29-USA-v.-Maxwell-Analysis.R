#### Analysis: 2020-06-29-Indictment-USA-v.-Maxwell

# 0. Load libraries
library(textdata)
library(ggwordcloud)
library(scales)
library(ngram)
library(readr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
library(igraph)
library(ggraph)
library(tidygraph)

data(stop_words)

##### Import text ----------

raw_text <- read_file("2020-06-29-Indictment-USA-v.-Maxwell-C.txt")

# Convert to tidy format
text <- tibble(
  text = raw_text
)

# Tokenization (one word per row)
text %<>%
  unnest_tokens(input = text, output = word) %>%
  filter(!is.na(word))

# Token cleaning
# Remove tokens containing numbers
text %<>%
  filter(!grepl(pattern = "[0-9]", x = word))

# Remove stop words
text %<>%
  anti_join(x = ., y = stop_words)

####### Word frequency --------------

# Table: top words with their relative frequency (%)
text %>%
  count(word, sort = TRUE) %>%
  mutate(percent = round((n / sum(n)) * 100, 2)) %>%
  head(n = 5)

# Lollipop chart: top 20 words
text %>%
  count(word, sort = TRUE) %>%
  mutate(
    percent = (n / sum(n)) * 100,
    label = paste0(n, " (", round(percent, 2), "%)"),
    word = reorder(word, n)
  ) %>%
  slice_max(n, n = 20, with_ties = FALSE) %>%
  ggplot(aes(x = word, y = n)) +
  geom_segment(aes(xend = word, yend = 0), color = "gray80", linewidth = 0.8) +
  geom_point(color = "#C0392B", size = 3.5) +
  geom_text(
    aes(label = label),
    hjust = -0.15,
    size = 3.5,
    color = "gray20"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(color = "gray40"),
    plot.title = element_text(face = "bold", size = 13)
  )

# Word cloud: top 20 words
set.seed(1243)
text %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(
    words = word,
    freq = n,
    max.words = 20,
    ordered.colors = FALSE,
    colors = brewer.pal(9, "Greys")[7:9],
    scale = c(3.5, 0.6)
  ))

######## Sentiment analysis ----------

afinn <- get_sentiments("afinn")

# Bar chart: AFINN score (frequency x value) per word
text %>%
  inner_join(afinn, by = "word") %>%
  count(word, value, sort = TRUE) %>%
  mutate(score = n * value) %>%
  filter(abs(score) > 2) %>%
  mutate(sentiment = ifelse(value > 0, "Positive", "Negative")) %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(word, score, fill = sentiment)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    name = "Sentiment",
    values = c("Negative" = "#d73027", "Positive" = "#4575b4")
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 20)
  ) +
  labs(
    y = "Score",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "plain", color = "black", size = 11),
    legend.position = "bottom"
  )
# Comparison word cloud: negative vs. positive words, sized by impact (frequency x |value|)
set.seed(123)
text %>%
  inner_join(afinn, by = "word") %>%
  count(word, value, sort = TRUE) %>%
  mutate(sentiment = ifelse(value > 0, "Positive", "Negative")) %>%
  mutate(impact = n * abs(value)) %>%
  acast(word ~ sentiment, value.var = "impact", fill = 0) %>%
  comparison.cloud(
    colors  = c("firebrick", "steelblue"),   # Negative, Positive
    max.words = 50,
    title.size = 1.5
  )

### NRC emotions

nrc <- get_sentiments("nrc")

emotions <- text %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("negative", "positive")) %>%
  count(sentiment) %>%
  mutate(
    percent = (n / sum(n)) * 100,
    sentiment = str_to_title(sentiment),   # e.g., "anger" -> "Anger"
    label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")
  )

pastel_colors <- c(
  "Anger"        = "#FF9999",
  "Sadness"      = "#4A90E2",
  "Joy"          = "#FDFD96",
  "Fear"         = "#CDB4DB",
  "Disgust"      = "#66CC66",
  "Anticipation" = "#FFB347",
  "Surprise"     = "#FFD1DC",
  "Trust"        = "#AEEEEE"
)

# Pie chart of emotions
ggplot(emotions, aes(x = "", y = percent, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1.25, label = label),
    position = position_stack(vjust = 0.5),
    size = 4.5,
    fontface = "bold",
    color = "gray20"
  ) +
  scale_fill_manual(
    values = pastel_colors,
    name = "Emotions"
  ) +
  theme_void()

######### Bigrams ----------

# Load the raw text in tidy format
text <- tibble(
  text = raw_text
)

# Tokenization by bigrams
text %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) -> text_bi

# Remove bigrams containing numbers or stop words
text_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Remove bigrams containing numbers
  filter(!grepl(pattern = "[0-9]", x = word1)) %>%
  filter(!grepl(pattern = "[0-9]", x = word2)) %>%
  # Remove stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  # Remove NAs
  filter(!is.na(word1)) %>%
  filter(!is.na(word2)) %>%
  # Count bigrams
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> text_bi_counts

# Build the network with bigrams that appear 3 or more times
g <- text_bi_counts %>%
  filter(weight > 2) %>%
  graph_from_data_frame(directed = FALSE)

summary(g)
# Identify connected components
V(g)$cluster <- components(graph = g)$membership

# Extract the largest connected component
gcc <- induced_subgraph(
  graph = g,
  vids = which(V(g)$cluster == which.max(components(graph = g)$csize))
)

# Plot settings (two plots side by side)
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1), mgp = c(1, 1, 1))

# Plot 1: connected component without weights
set.seed(9)
plot(
  gcc,
  layout = layout_with_lgl,
  vertex.color = 1,
  vertex.frame.color = 1,
  vertex.size = 3,
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = 1
)

# Plot 2: connected component with weights (vertex size = strength)
set.seed(9)
plot(
  gcc,
  layout = layout_with_lgl,
  vertex.color = adjustcolor("darkolivegreen4", 0.1),
  vertex.frame.color = "darkolivegreen4",
  vertex.size = 0.5 * strength(gcc),
  vertex.label.color = "black",
  vertex.label.cex = 1,
  vertex.label.dist = 1,
  edge.width = 3 * E(gcc)$weight / max(E(gcc)$weight)
)

####### Skipgrams ---------------

text <- tibble(
  text = raw_text
)

# Tokenization by skipgrams
text %>%
  unnest_tokens(input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
  filter(!is.na(skipgram)) -> text_skip

# Count the words in each skipgram
text_skip$num_words <- text_skip$skipgram %>%
  map_int(.f = ~ wordcount(.x))

# Remove unigrams
text_skip %<>%
  filter(num_words == 2) %>%
  select(-num_words)

# Remove skipgrams containing numbers or stop words
text_skip %>%
  separate(skipgram, c("word1", "word2"), sep = " ") %>%
  # Remove words containing numbers
  filter(!grepl(pattern = "[0-9]", x = word1)) %>%
  filter(!grepl(pattern = "[0-9]", x = word2)) %>%
  # Remove stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  # Remove NAs
  filter(!is.na(word1)) %>%
  filter(!is.na(word2)) %>%
  # Frequency
  count(word1, word2, sort = TRUE) %>%
  # Relative frequency (%)
  mutate(rel_freq = round((n / sum(n)) * 100,2)) %>%
  # Rename the count column to "weight"
  rename(weight = n) -> text_skip_counts

# Table: top 10 skipgrams
head(text_skip_counts, n = 10)

# Build the network and extract the largest connected component
g <- text_skip_counts %>%
  filter(weight > 0) %>%
  graph_from_data_frame(directed = FALSE)

g <- igraph::simplify(g)

V(g)$cluster <- components(graph = g)$membership

gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(components(graph = g)$csize)))

# Circular layout plot
set.seed(123)
ggraph(gcc, layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(edge_width = weight, edge_alpha = weight),
                color = "darkolivegreen4", show.legend = FALSE) +
  geom_node_point(aes(size = strength(gcc)), color = "gray20", alpha = 0.8, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.2, 1.5)) +
  scale_edge_alpha_continuous(range = c(0.1, 0.5)) +
  scale_size_continuous(range = c(1, 4)) +
  theme_void() +
  coord_fixed()

# Network statistics
metrics <- round(c(
  mean_distance(gcc),
  mean(degree(gcc)),
  sd(degree(gcc)),
  clique_num(gcc),
  edge_density(gcc),
  transitivity(gcc),
  assortativity_degree(gcc)
), 4)

metric_names <- c(
  "Mean distance", "Mean degree", "SD of degree",
  "Clique number", "Density", "Transitivity", "Assortativity"
)

network_table <- data.frame(
  Statistic = metric_names,
  Value     = round(metrics, 2)
)

network_table

# Top 10 most important words by eigenvector centrality
centrality <- tibble(
  word = V(gcc)$name,
  eigen = eigen_centrality(gcc)$vector
)

centrality %>%
  arrange(desc(eigen)) %>%
  slice_head(n = 10) %>%
  mutate(eigen = round(eigen, 2))

# Clustering (fast greedy algorithm)
set.seed(123)
kc <- cluster_fast_greedy(gcc)

# Plot: vertices colored by community
cols <- c(
  brewer.pal(9, "Set1")[1:9],
  brewer.pal(8, "Set2")[1:7],
  brewer.pal(8, "Set2")[1:7],
  brewer.pal(12, "Set3")[1:3]
)

gcc_tidy <- as_tbl_graph(gcc) %>%
  activate(nodes) %>%
  mutate(
    strength  = strength(gcc),
    community = as.factor(membership(kc))
  ) %>%
  arrange(community)

set.seed(123)

ggraph(gcc_tidy, layout = "linear", circular = TRUE) +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight),
    color = "gray70",
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(color = community, size = strength),
    show.legend = FALSE
  ) +
  scale_color_manual(values = cols) +
  scale_size_continuous(range = c(1, 5)) +
  scale_edge_width_continuous(range = c(0.5, 2)) +
  scale_edge_alpha_continuous(range = c(0.4, 0.9)) +
  theme_void() +
  coord_fixed()

# Cluster sizes
sizes(kc)

# Table: three strongest words in each of the five largest clusters
cluster_data <- data.frame(
  Word     = V(gcc)$name,
  Cluster  = kc$membership,
  Strength = strength(gcc)
)

top_cluster_words <- cluster_data %>%
  group_by(Cluster) %>%
  mutate(Cluster_size = n()) %>%
  ungroup() %>%
  arrange(desc(Cluster_size)) %>%
  filter(Cluster %in% head(unique(Cluster), 5)) %>%
  group_by(Cluster) %>%
  arrange(desc(Strength), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  select(Cluster, Cluster_size, Word, Strength) %>%
  mutate(Strength = round(Strength, 2))

print(top_cluster_words)

