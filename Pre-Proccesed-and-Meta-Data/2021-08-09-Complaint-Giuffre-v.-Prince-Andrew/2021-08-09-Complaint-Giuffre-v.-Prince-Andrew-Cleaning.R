## Data cleaning: 2021-08-09-Complaint-Giuffre-v.-Prince-Andrew

# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the PDF file
raw_text <- pdf_text("gov.uscourts.nysd.564713.1.0_5.pdf")

# 2. Remove the first and last pages
raw_text <- raw_text[2:14]

# 3. Join all pages into a single text
raw_text <- paste(raw_text, collapse = "\n")

# 4. Save the raw text as a .txt file
write_file(raw_text, "2021-08-09-Complaint-Giuffre-v.-Prince-Andrew.txt")

# 5. Load the .txt file
raw_text <- read_file("2021-08-09-Complaint-Giuffre-v.-Prince-Andrew.txt")

# 6. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Remove page headers (e.g., "Case 1:21-cv-06702-LAK Document 1 Filed 08/09/21 Page 2 of 15")
  str_remove_all("Case 1:21-cv-06702-LAK Document 1 Filed 08/09/21 Page [0-9]+ of 15") %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 7. Save the cleaned text as a .txt file
write_file(text_clean, "2021-08-09-Complaint-Giuffre-v.-Prince-Andrew-C.txt")