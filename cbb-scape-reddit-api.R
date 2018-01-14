library(tidyverse)

# Set up httr for reddit API calls ----------------------------------------
### httr bugfix from https://github.com/r-lib/httr/pull/485
library(devtools)
devtools::install_github("r-lib/httr#485")
library(httr)

oauth <- read_csv("oauth_keys.csv")

# 1. Find OAuth settings for reddit:
#    https://github.com/reddit/reddit/wiki/OAuth2
endpoint <- oauth_endpoint(
  authorize = "https://www.reddit.com/api/v1/authorize",
  access =    "https://www.reddit.com/api/v1/access_token"
)

# 2. Register an application at https://www.reddit.com/prefs/apps
app <- oauth_app("EarwolfScrape", oauth$key, oauth$secret)

# 3b. If get 429 too many requests, the default user_agent is overloaded.
# If you have an application on Reddit then you can pass that using:
token <- oauth2.0_token(
  endpoint = endpoint, 
  app=app,
  scope = c("read", "modposts"),
  use_basic_auth = TRUE,
  cache = F,
  config_init = user_agent("Earwolf Comment Scraper")
)




SearchEarwolfSubreddit <- function(searchTerm) {
  
  ### The initial API call to get search results
  initialURL <- paste0("https://oauth.reddit.com/r/Earwolf/search.json?q=", searchTerm, "+site%3Aearwolf.com&sort=new&type=link&restrict_sr=TRUE&t=all&raw_json=1&limit=100")
  
  ### Perform the search API call
  response <- GET(initialURL,
                  user_agent("Earwolf Comment Scraper"),
                  config(token = token)) %>% content()
  
  ### Extract the data from the parsed JSON response into a dataframe
  threads <- data_frame(
    title = map(response$data$children, ~ .x$data$title),
    link = map(response$data$children, ~ .x$data$url),
    fullname = map(response$data$children, ~ .x$data$name),
    gilded = map(response$data$children, ~ .x$data$gilded),
    poster = map(response$data$children, ~ .x$data$author),
    score = map(response$data$children, ~ .x$data$score),
    permalink = map(response$data$children, ~ .x$data$permalink),
    numComments = map(response$data$children, ~ .x$data$num_comments)
  )
  
  ### Print status update
  searchPage <- 1
  print(paste("Finished page", searchPage, "of results for", searchTerm))
  
  ### Init newThreads for the while loop.
  newThreads <- data_frame()
  
  ### Keep searching until you don't get the max number of search results (100)
  while((nrow(newThreads) == 100 | nrow(threads) == 100) & searchPage < 10) {
    ### Count how many pages deep we're going
    searchPage <- searchPage + 1
    print(paste("Getting page", searchPage, "of results for", searchTerm))
    # Pause to respect API rules.
    Sys.sleep(2) 
    
    ### Get the fullname of the last thread in the search results
    lastThread <- tail(threads$fullname, n = 1L)
    
    ### Append the last thread fullname to the search URL
    nextURL <- paste0(initialURL, "&after=", lastThread)
    
    ### Get the next set of search results
    response <- GET(nextURL,
                    user_agent("Earwolf Comment Scraper"),
                    config(token = token)) %>% content()
    
    ### Extract the data
    newThreads <- data_frame(
      title = map(response$data$children, ~ .x$data$title),
      link = map(response$data$children, ~ .x$data$url),
      fullname = map(response$data$children, ~ .x$data$name),
      gilded = map(response$data$children, ~ .x$data$gilded),
      poster = map(response$data$children, ~ .x$data$author),
      score = map(response$data$children, ~ .x$data$score),
      permalink = map(response$data$children, ~ .x$data$permalink),
      numComments = map(response$data$children, ~ .x$data$num_comments)
    )
    
    ### Combine the old data with the new
    threads <- bind_rows(threads, newThreads)
  }
  print(paste("Done searching for", searchTerm, "after", searchPage, "pages"))
  return(threads)
}

redditThreads <- SearchEarwolfSubreddit("comedy+bang+bang") %>%
  bind_rows(SearchEarwolfSubreddit("CBB"))
