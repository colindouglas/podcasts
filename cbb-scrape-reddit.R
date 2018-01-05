# Scrape r/earwolf --------------------------------------------------------
library(httr)
library(rvest)
library(tidyverse)
### Init dataframe
reddit <- data_frame()

search_page <- "https://www.reddit.com/r/Earwolf/search"
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:57.0) Gecko/20100101 Firefox/57.0"

session <- html_session(search_page, user_agent(user_agent))

form <- html_form(session)[[3]]

form <- set_values(form, q = "comedy+bang+bang site:earwolf.com", restrict_sr = T, sort = "new")

results <- submit_form(session, form)

reddit_scrape <- read_html(results)

title <- reddit_scrape %>%
  html_nodes(".search-result-header") %>%
  html_text()














### The first page
next_page <- "https://www.reddit.com/r/Earwolf/search?q=comedy%2Bbang%2Bbang+site%3Aearwolf.com&restrict_sr=on&sort=new&t=all?test"

### Set a dummy title so the loop starts
title <- " "

while (length(title) > 0 ) {
  
  ### Start loop
  
#  download.file(next_page, destfile = 'reddit_scrape.html') #debug
#  reddit_scrape <- read_html('reddit_scrape.html') #debug
  
  reddit_scrape <- read_html(next_page)
  
  title <- reddit_scrape %>%
    html_nodes(".search-result-header") %>%
    html_text()
  
  number <- title %>% 
    str_extract("#[BO0-9.]+|# *[BO0-9.]+|episode [BO0-9.]+|Episode [BO0-9.]+") 
  
  url <- reddit_scrape %>%
    html_nodes(".search-link") %>%
    html_attr("href")
  
  upvotes <- reddit_scrape %>%
    html_nodes(".search-score") %>%
    html_text() %>%
    str_extract("[0-9]+")
  
  comments <- reddit_scrape %>%
    html_nodes(".search-comments") %>%
    html_text() %>%
    str_extract("[0-9]+")
  
  search_page <- reddit_scrape %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  
  next_page <- search_page %>% 
    subset(grepl("after", search_page)) %>%
    tail(n = 1)
  
  newrows <- data_frame(title, upvotes, comments, url)
  
  reddit <- reddit %>% rbind(newrows)
  
}

episode_reddit <- left_join(episode, reddit[2:4])

