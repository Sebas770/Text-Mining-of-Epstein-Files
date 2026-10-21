## Data cleaning: 2015-09-21-Complaint-Giuffre-v.-Maxwell

# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the PDF file
raw_text <- pdf_text("gov.uscourts.nysd.447706.1.0_8.pdf")

# 2. Remove the first and last pages
raw_text <- raw_text[2:11]

# 3. Join all pages into a single text
raw_text <- paste(raw_text, collapse = "\n")

# 4. Save the raw text as a .txt file
write_file(raw_text, "2015-09-21-Complaint-Giuffre-v.-Maxwell.txt")

# 5. Load the .txt file
raw_text <- read_file("2015-09-21-Complaint-Giuffre-v.-Maxwell.txt")

# 6. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Remove page headers (e.g., "Case 1:15-cv-07433-LAP Document 1 Filed 09/21/15 Page 2 of 12")
  str_remove_all("Case 1:15-cv-07433-LAP Document 1 Filed 09/21/15 Page [0-9]+ of 12") %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 7. Save the cleaned text as a .txt file
write_file(text_clean, "2015-09-21-Complaint-Giuffre-v.-Maxwell-C.txt")