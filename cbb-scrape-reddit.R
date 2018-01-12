library(RedditExtractoR)
library(tidyverse)

### Should we re-collect all of the data (TRUE)
### or just collect the most recent data (FALSE)? 
startFresh <- FALSE

if (startFresh == FALSE) {
  ### Read in the previous data scraped from reddit
  oldRedditEpisodes <- read_csv("cbb_reddit_scrape.csv") %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"))
  
  ### Find the URL for the most recent post
  lastURL <- oldRedditEpisodes$URL[oldRedditEpisodes$date == max(oldRedditEpisodes$date)]
  
  pageThreshold <- 0
  allURLs <- c()
  
  ### Keep expanding page threshold of the search until you find the last URL, then stop
  while (!(lastURL %in% allURLs)) {
    ### Increase the number of search pages you go back by one
    pageThreshold <- pageThreshold + 1
    ### For debug
    print(paste("Page Threshold:", pageThreshold))
    
    cbbThreads <- reddit_urls(
      search_terms = "Comedy Bang Bang site:earwolf.com",
      subreddit = "Earwolf",
      page_threshold = pageThreshold,
      sort_by = "new")
    
    cbbThreads2 <- reddit_urls(
      search_terms = "CBB site:earwolf.com",
      subreddit = "Earwolf",
      page_threshold = pageThreshold,
      sort_by = "new")
    
    ### Combine the list of URLs from both search terms, only keep the unique ones
    allURLs <- unique(c(cbbThreads$URL,cbbThreads2$URL))
  }
} else if (startFresh == TRUE) {
  ### Clear out any previously scraped data
  oldRedditEpisodes <- data_frame()
  
  ### Search 100 pages deep, that should about do it
    cbbThreads <- reddit_urls(
    search_terms = "Comedy Bang Bang site:earwolf.com",
    subreddit = "Earwolf",
    page_threshold = 100,
    sort_by = "new")
  
  cbbThreads2 <- reddit_urls(
    search_terms = "CBB site:earwolf.com",
    subreddit = "Earwolf",
    page_threshold = 100,
    sort_by = "new")
  
  ### Combine the list of URLs from both search terms, only keep the unique ones
  allURLs <- unique(c(cbbThreads$URL,cbbThreads2$URL))
}

### Get the comment data for all of the URLs you found above
redditComments <- reddit_content(allURLs)

if(startFresh == TRUE) write_csv(redditComments, "reddit_comments_scape.csv")

### Summarize the data
redditEpisodes <- redditComments %>%
  ### Only use the data where URL points to earwolf
  filter(domain == "earwolf.com") %>%
  ### Interpret the post_date column as a date
  mutate(post_date = as.Date(post_date, format = "%d-%m-%y")) %>%
  ### Cut off any of the URL that is after a question mark to improve matching
  mutate(URL = sub("\\?.*", "", URL)) %>%
  ### Group by all of the columns that are constant across a given thread
  group_by(post_date, title, num_comments, upvote_prop, post_score, author, link, URL) %>%
  ### Summarize, calculate the sum of all of the comment scores
  summarize(totalCommentScore = sum(comment_score)) %>%
  ### Make sure its a tibble
  as_data_frame()

### Rename the columns
names(redditEpisodes) <- c(
  post_date = "date", 
  title = "title", 
  num_comments = "comments", 
  upvote_prop= "ppvoteProp", 
  post_score = "score", 
  author = "author", 
  link = "link", 
  URL = "URL", 
  totalCommentScore = "totalCommentScore")

### Combine the newly scraped data with the old data
completeRedditEpisodes <- redditEpisodes %>% 
  bind_rows(oldRedditEpisodes) %>%
  ### Keep only the rows with distinct URLs 
  distinct(URL, .keep_all = TRUE) %>%
  ### Sort by date
  arrange(date)

### Write the episode data to a CSV
write_csv(completeRedditEpisodes, "cbb_reddit_scrape.csv")
