## Data cleaning: 2019-07-02-Indictment-USA-v.-Epstein
# Note: the plain-text version of the indictment was downloaded from the
# Epstein Archive (https://www.epsteinarchive.org/docs/federal-indictment/).

# 0. Load libraries
library(tidyverse)
library(readr)

# 1. Read the text file as a single string
raw_text <- read_file("2019-07-02-Indictment-USA-v.-Epstein.txt")

# 2. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Remove page markers (e.g., "--- PAGE 1 ---")
  str_remove_all("--- PAGE [0-9]+ ---") %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 3. Save the cleaned text as a .txt file
write_file(text_clean, "2019-07-02-Indictment-USA-v.-Epstein-C.txt")