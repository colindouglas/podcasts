library(screddr)
library(tidyverse)

### Read in OAuth2 keys from external CSV
oauth <- read_csv("oauth_keys.csv")

### Get a token
RedditAuth(oauth$key, oauth$secret)

### Define which search terms we're looking for
searchTerms <- c("comedy+bang+bang", "CBB")

### List of columns from the reddit results that are actually useful
usefulVars <- c("domain", "subreddit_id", "subreddit", "selftext", "link_flair_text",
                "id", "view_count", "author", "num_crossposts", "score", "over_18",
                "gilded", "author_flair_text", "stickied", "name", "permalink", "created",
                "url", "title", "ups", "num_comments", "is_self", "crosspost_parent" )


### Call "screddr::SearchSubreddit" for each of the terms in the Earwolf subreddit, keep only the write ones
scrapedThreads <- map_dfr(searchTerms, ~ SearchSubreddit(., subreddit = "Earwolf")) %>%
  distinct(name, .keep_all = TRUE)  %>%
  select(usefulVars)


### Read in the previously saved data, combine it with the new data, and calculate each thread's age
allThreads <- read_csv("data/cbb_reddit_threads.csv") %>%
  select(usefulVars) %>%
  bind_rows(scrapedThreads) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(age = as.numeric(Sys.Date()) - as.numeric(created)/60/60/24)

### Load in previously cached comments
cachedComments <- read_csv("data/cbb_reddit_comments.csv", col_types = cols(.default = "c"))
uniqueThreads <- unique(cachedComments$link_id)

### Find all of the thread names that aren't in the "link_ids" column of the cached comments
unscrapedThreads <- allThreads$name[!(allThreads$name %in% uniqueThreads)]

### Find all of the threads IDs that are less than a week old, they probably need to be refreshed
refreshThreads <- allThreads %>%
  filter(age < 7) %>%
  pull(name)

### The threads that need updating are the threads that have never been scraped plus the newer threads
threadList <- c(unscrapedThreads, refreshThreads)

newComments <- map_dfr(threadList, ~ GetComments(., subreddit = "Earwolf"))


allComments <- newComments %>%
  bind_rows(cachedComments) %>%
  distinct(name, .keep_all = TRUE) %>%
  type_convert()

### Output all of the data for quicker access in the future
write_csv(allThreads, path = "data/cbb_reddit_threads.csv")
write_csv(allComments, path = "data/cbb_reddit_comments.csv")
