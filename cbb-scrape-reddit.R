library(RedditExtractoR)
library(tidyverse)

### Should we re-collect all of the data (TRUE)
### or just collect the most recent data (FALSE)? 
startFresh <- TRUE

if (startFresh == FALSE) {
  ### Read in the previous data scraped from reddit
  oldRedditEpisodes <- read_csv("cbb_reddit_scrape.csv")
  ### Find the URL for the most recent post
  lastURL <- oldRedditEpisodes$rURL[oldRedditEpisodes$rDate == max(oldRedditEpisodes$rDate)]
  
  pageThreshold <- 0
  allURLs <- c()
  
  ### Keep expanding page threshold of the search until you find the last URL, then stop
  while (!(lastURL %in% allURLs)) {
    ### Increase the number of search pages you go back by one
    pageThreshold <- pageThreshold + 1
    
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
  
  ### Just search 100 pages deep
  oldRedditEpisodes <- data_frame()
  cbbThreads <- reddit_urls(
    search_terms = "Comedy Bang Bang site:earwolf.com",
    subreddit = "Earwolf",
    page_threshold = 200,
    sort_by = "new")
  
  cbbThreads2 <- reddit_urls(
    search_terms = "CBB site:earwolf.com",
    subreddit = "Earwolf",
    page_threshold = 200,
    sort_by = "new")
  
  ### Combine the list of URLs from both search terms, only keep the unique ones
  allURLs <- unique(c(cbbThreads$URL,cbbThreads2$URL))
}

### Get the comment data for all of the URLs you found above
redditComments <- reddit_content(allURLs)

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
  ### Arrange in order of date
  arrange(post_date) %>% 
  ### Make sure its a tibble
  as_data_frame()

### Rename the columns, prefix an "r" to clarify that its from reddit data
names(redditEpisodes) <- c(
  post_date = "rDate", 
  title = "rTitle", 
  num_comments = "rComments", 
  upvote_prop= "rUpvoteProp", 
  post_score = "rScore", 
  author = "rAuthor", 
  link = "link", 
  URL = "rURL", 
  totalCommentScore = "rTotalCommentScore")

### Combine the newly scraped data with the old data
completeRedditEpisodes <- redditEpisodes %>% 
  bind_rows(oldRedditEpisodes) %>%
  ### Keep only the rows with distinct URLs 
  distinct(rURL, .keep_all = TRUE) %>%
  ### Sort by date
  arrange(rDate)

### Write the episode data to a CSV
write_csv(completeRedditEpisodes, "cbb_reddit_scrape.csv")
