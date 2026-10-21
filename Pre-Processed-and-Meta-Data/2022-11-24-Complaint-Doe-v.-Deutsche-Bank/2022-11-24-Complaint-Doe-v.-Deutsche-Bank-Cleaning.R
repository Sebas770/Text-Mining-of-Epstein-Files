## Data cleaning: 2022-11-24-Complaint-Doe-v.-Deutsche-Bank

# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the PDF file
raw_text <- pdf_text("gov.uscourts.nysd.590047.1.0.pdf")

# 2. Remove the last two pages
raw_text <- raw_text[1:102]

# 3. Join all pages into a single text
raw_text <- paste(raw_text, collapse = "\n")

# 4. Save the raw text as a .txt file
write_file(raw_text, "2022-11-24-Complaint-Doe-v.-Deutsche-Bank.txt")

# 5. Load the .txt file
raw_text <- read_file("2022-11-24-Complaint-Doe-v.-Deutsche-Bank.txt")

# 6. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Remove page headers (e.g., "Case 1:22-cv-10018 Document 1 Filed 11/24/22 Page 2 of 104")
  str_remove_all("Case 1:22-cv-10018 Document 1 Filed 11/24/22 Page [0-9]+ of 104") %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 7. Save the cleaned text as a .txt file
write_file(text_clean, "2022-11-24-Complaint-Doe-v.-Deutsche-Bank-C.txt")