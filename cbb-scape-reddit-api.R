library(screddr)
library(tidyverse)

oauth <- read_csv("oauth_keys.csv")

RedditAuth(oauth$key, oauth$secret)

### Get all of the threads that have "CBB" or "comedy bang bang" in the title
searchTerms <- c("comedy+bang+bang", "cBB")

redditThreads <- map_dfr(searchTerms, ~ SearchSubreddit(., subreddit = "Earwolf")) %>%
  distinct(name, .keep_all = TRUE) 

### Output the reddit thread data to a CSV
write_csv(redditThreads, path = "data/cbb_reddit_threads.csv")

### Get all of the comments on all of the reddit threads you just scraped
allComments <- map_dfr(redditThreads$name, ~ GetComments(., subreddit = "Earwolf"))

### Output the comment data to a CSV
write_csv(allComments, path = "data/cbb_reddit_comments.csv")

?SearchSubreddit
?RedditAuth
