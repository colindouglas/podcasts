library(screddr)
library(tidyverse)

### Read in OAuth2 keys from external CSV
oauth <- read_csv("oauth_keys.csv")

### Get a token
RedditAuth(oauth$key, oauth$secret)

### Define which search terms we're looking for
searchTerms <- c("comedy+bang+bang+site%3Aearwolf.com", "CBB+site%3Aearwolf.com")

### List of columns from the reddit results that are actually useful
usefulVars <- c("domain", "subreddit_id", "subreddit", "selftext", "link_flair_text",
                "id", "view_count", "author", "num_crossposts", "score", "over_18",
                "gilded", "author_flair_text", "stickied", "name", "permalink", "created",
                "url", "title", "ups", "num_comments", "is_self")



### Call "screddr::SearchSubreddit" for each of the terms in the Earwolf subreddit, keep only the write ones
scrapedThreads <- map_dfr(searchTerms, ~ SearchSubreddit(., subreddit = "Earwolf")) %>%
  distinct(name, .keep_all = TRUE)


### Read in the previously saved data, combine it with the new data, and calculate each thread's age
allThreads <- read_csv("data/cbb_reddit_threads.csv", col_types = RedditColTypes("thread")) %>%
  bind_rows(scrapedThreads) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(age = Sys.Date() - as.Date(anytime::anytime(created)))

### Load in previously cached comments
cachedComments <- read_csv("data/cbb_reddit_comments.csv", col_types = RedditColTypes("comment"))
uniqueThreads <- unique(cachedComments$link_id)

### Find all of the thread names that aren't in the "link_ids" column of the cached comments
unscrapedThreads <- allThreads$name[!(allThreads$name %in% uniqueThreads)]

### Find all of the threads IDs that are less than two weeks old, they probably need to be refreshed
refreshThreads <- allThreads %>%
  filter(age < 14) %>%
  pull(name)

### The threads that need updating are the threads that have never been scraped plus the newer threads
threadList <- c(unscrapedThreads, refreshThreads)

### Get all the comments on the threads that need to be scraped in "threadList"
newComments <- map_dfr(threadList, ~ GetComments(., subreddit = "Earwolf"))

### Combine the cached comments with the newly scraped comments
allComments <- newComments %>%
  bind_rows(cachedComments) %>%
  distinct(name, .keep_all = TRUE) 

### Output all of the data for quicker access in the future
write_csv(allThreads, path = "data/cbb_reddit_threads.csv")
write_csv(allComments, path = "data/cbb_reddit_comments.csv")
