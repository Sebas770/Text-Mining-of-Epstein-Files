## Data cleaning: 2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein
# Note: the original PDF is a scanned image, so its text was transcribed
# manually into the .txt file loaded below.

# 0. Load libraries
library(tidyverse)
library(readr)

# 1. Read the manually transcribed text file as a single string
raw_text <- read_file("2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein.txt")

# 2. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 3. Save the cleaned text as a .txt file
write_file(text_clean, "2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein-C.txt")