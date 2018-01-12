library(rvest)
library(tidyverse)

### Should we re-collect all of the data from the start episode (TRUE)
### or just amend the existing data (FALSE)? 
startFresh <- FALSE

# Scrape Earwolf ----------------------------------------------------------

if (startFresh == TRUE) { 
  ### Make an empty dataframe
  episode <- data_frame()
  
  ### The first episode
  next_ep <- "http://www.earwolf.com/episode/welcome-to-comedy-bang-bang/"
} else if (startFresh == FALSE) {
  
  ### Read in the previously-exported CSV
  episode <- read_csv("data/cbb_earwolf_scrape.csv")
  
  ### Split the guests column into vectors
  episode$guests <- strsplit(episode$guests, ", ")
  
  ### Get the URL of the last episode processed
  last_ep <- tail(episode$url,1)
  
  ### Scrape the HTML from the last episode
  html_scrape <- read_html(last_ep)
  
  ### Find the next episode URL
  next_ep <- html_scrape %>% 
    html_nodes(".nextepisodelink") %>% 
    html_attr('href')
} 

while (length(next_ep) > 0) { # The next episode will be character(0) once it gets to the final episode
 ### Scrape the HTML  
  html_scrape <- read_html(next_ep)
  
  ### Get the episode date
  date <- html_scrape %>%
    html_node(".epidate") %>%
    html_text() %>%
    as.Date(format = "%b %d, %Y")
  
  ### Get the episode number, drop the first character because its a number sign
  number <- html_scrape %>%
    html_node(".epititle") %>%
    html_text() %>%
    str_extract("#.+") %>%
    substring(2)
  
  ### Get the episode title
  title <- html_scrape %>%
    html_node(".showtitle") %>%
    html_text() 
  
  ### Get the episode description
  desc <- html_scrape %>%
    html_node(".episodeshowdesc") %>%
    html_text()
  desc <- str_trim(gsub("\\n", "", desc))
  
  ### Get the episode guests (there can be multiple)
  guest <- html_scrape %>%
    html_nodes(".clearfix") %>%
    html_text() 

  ### Make a new row with all of the data
  newrow <- data_frame(date, number, title, url=next_ep, desc)
  
  ### Add the guests as a list
  newrow$guests <- list(guest)
  
  ### Add the new row to the data frame
  episode <- rbind(episode, newrow)
  
  ### Remove everything you just made, just in case you mess something up
  rm(newrow, title, desc, guest, number, date)
  
  ### Find the next episode URL
  next_ep <- html_scrape %>% 
    html_nodes(".nextepisodelink") %>% 
    html_attr('href')
}

episode_output <- episode

### Convert the guest column to a string (from a list)
episode_output$guests <- sapply(episode$guests, toString)

### Output the dataframe as a CSV
write_csv(episode_output, path = "data/cbb_earwolf_scrape.csv")
rm(episode_output)


