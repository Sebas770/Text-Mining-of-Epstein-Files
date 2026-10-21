## Data cleaning: 2022-11-28-Complaint-Pierson-v.-Black

# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the PDF file
raw_text <- pdf_text("952002_2022_Cheri_Pierson_v_Leon_Black_et_al_SUMMONS___COMPLAINT_1.pdf")

# 2. Remove the summons (first two pages) and the last page
raw_text <- raw_text[3:29]

# 3. Join all pages into a single text
raw_text <- paste(raw_text, collapse = "\n")

# 4. Save the raw text as a .txt file
write_file(raw_text, "2022-11-28-Complaint-Pierson-v.-Black.txt")

# 5. Load the .txt file
raw_text <- read_file("2022-11-28-Complaint-Pierson-v.-Black.txt")

# 6. Clean the text using regular expressions (regex)
text_clean <- raw_text %>%
  
  # Remove the elements of the NYSCEF page header
  # Page numbers (e.g., "3 of 30")
  str_remove_all("[0-9]+ of 30") %>%
  
  # Filing stamp
  str_remove_all("FILED: NEW YORK COUNTY CLERK 11/28/2022 11:29 AM") %>%
  
  # Document number
  str_remove_all("NYSCEF DOC. NO. 1") %>%
  
  # Index number
  str_remove_all("INDEX NO. 952002/2022") %>%
  
  # Date of receipt
  str_remove_all("RECEIVED NYSCEF: 11/28/2022") %>%
  
  # Replace line breaks with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra whitespace
  str_squish()

# 7. Save the cleaned text as a .txt file
write_file(text_clean, "2022-11-28-Complaint-Pierson-v.-Black-C.txt")