library(tidyverse)
library(rvest)

url <- "https://doughboys.fandom.com/wiki/Episodes"

wiki_page <- read_html(url) 

episodes <- wiki_page %>%
  html_nodes("table") %>%
  html_table() %>%
  first()

colnames(episodes) <- c("number", "title", "rating", "release_date", "accolades", "notes")

# Separate out the guests into a separate column
episodes <- episodes %>%
  separate(col = title, into = c("title", "guests"), 
           sep = " with (?!Joe)", # After Dough with Joe with mess this up
           remove = FALSE, fill = "right", extra = "merge") %>%
  # Convert release date to a Date
  mutate(release_date = lubridate::mdy(release_date)) %>%
  # Handle the cases where it's ToC and there's no rating, just a winner
  mutate(winner = case_when(
    is.na(as.numeric(rating)) | rating == "" ~ rating,
    TRUE ~ as.character(NA)
  ), rating = case_when(
    is.na(winner) | winner == "" ~ as.numeric(rating),
    TRUE ~ as.numeric(NA)
  ))


# Mark the live epsiodes
episodes$live <- grepl("\\(LIVE\\)|(Live)", episodes$guests, ignore.case = TRUE)

# Take the (LIVE) out of the guest field
episodes$guests <- gsub(" \\(LIVE\\)", "", episodes$guests, ignore.case = TRUE)

# Split the episodes with multiple guests
# Be more consistent in your Oxford commas, fellas
episodes$guests <- str_split(episodes$guests, pattern = "(, and )|( & )|( with )|( and )|(, )")

episodes %>%
  rowwise() %>%
  mutate(guests = paste0(guests, collapse = ", ")) %>% 
  write_csv(path = "data/doughboys-episodes.csv", na = "")

episodes %>%
  ggplot(aes(x = live, y = rating)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom()