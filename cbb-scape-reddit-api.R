library(tidyverse)

# Set up httr for reddit API calls ----------------------------------------
### httr bugfix from https://github.com/r-lib/httr/pull/485
library(devtools)
devtools::install_github("r-lib/httr#485")
library(httr)

userAgent <- "Earwolf Comment Scraper (contact colindouglas@gmail.com)"
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
  config_init = user_agent(userAgent)
)


# End of API setup --------------------------------------------------------

### Function to make search calls to the API to find all of the relevant threads
SearchEarwolfSubreddit <- function(searchTerm) {
#  searchTerm <- "comedy+bang+bang" #debug
  ### The initial API call to get search results
  initialURL <- paste0("https://oauth.reddit.com/r/Earwolf/search.json?q=", searchTerm, "+site%3Aearwolf.com&sort=new&type=link&restrict_sr=TRUE&t=all&raw_json=1&limit=100")

  ### Perform the search API call
  response <- GET(initialURL,
                  user_agent("Earwolf Comment Scraper"),
                  config(token = token)) %>% content()

  threads <- map_dfr(response$data$children, ~flatten(.$data))

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
    Sys.sleep(1.1)

    ### Get the fullname of the last thread in the search results
    lastThread <- tail(threads$name, n = 1L)

    ### Append the last thread fullname to the search URL
    nextURL <- paste0(initialURL, "&after=", lastThread)

    ### Get the next set of search results
    response <- GET(nextURL,
                    user_agent("Earwolf Comment Scraper"),
                    config(token = token)) %>% content()

    ### Extract the data
    newThreads <- map_dfr(response$data$children, ~flatten(.$data))
    
    ### Combine the old data with the new
    threads <- bind_rows(threads, newThreads)
  }
  print(paste("Done searching for", searchTerm, "after", searchPage, "pages"))
  return(threads)
}

### Function to get all of the comments given a thread ID
GetComments <- function(threadID) {
  threadURL <- paste0("https://oauth.reddit.com/r/Earwolf/comments/", substring(threadID,4), ".json")
  print(paste("Fetching thread", threadID))
  
  ### Pause to respect API rules
  Sys.sleep(1.1)

  ### Get all of the information on the thread
  threadResponse <- GET(threadURL,
                        user_agent("Earwolf Comment Scraper"),
                        config(token = token)) %>% content()
  
    CountReplies <- function(x) { 
    if (length(x$data$replies) == 1) {
      return(0)
    } else {
      return(length(x$data$replies$data$children))
    }
  }
  
  WithoutReplies <- function(x) {
    output <- x[-which(names(x) == "replies")]
    output[lengths(output) == 0] <- NA
    output <- lapply(output, as.character)
    return(output)
  }
  
  InspectReplies <- function(y) {
    allComments <- data_frame()
    
    rInspectReplies <- function(endsInIndex) {
      if (CountReplies(endsInIndex) > 0) {
        allComments <- bind_rows(allComments, map_dfr(endsInIndex$data$replies$data$children, ~ WithoutReplies(.$data)))
        allComments <- bind_rows(allComments, map_dfr(endsInIndex$data$replies$data$children, ~ rInspectReplies(.)))
      }
      return(as.data.frame(allComments))
    }
    return(rInspectReplies(y))
  }
  
  childComments <- map_dfr(threadResponse[[2]]$data$children, ~ InspectReplies(.))    # Get all of the child(second-or-deeper) comments
  topComments <- map_dfr(threadResponse[[2]]$data$children, ~ WithoutReplies(.$data)) # Get all the top levels
  
  allComments <- bind_rows(topComments, childComments) # Join the top and child comments
  return(allComments)
} 

### Get all of the threads that have "CBB" or "comedy bang bang" in the title
searchTerms <- c("comedy+bang+bang", "cBB")

redditThreads <- map_dfr(searchTerms, ~ SearchEarwolfSubreddit(.)) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(-images)

### Output the reddit thread data to a CSV
write_csv(redditThreads, path = "data/cbb_reddit_scrape.csv")

### Get all of the comments on all of the reddit threads you just scraped
allComments <- map_dfr(redditThreads$name, ~ GetComments(.))

### Output the comment data to a CSV
write_csv(allComments, path = "data/cbb_reddit_comments_scrape.csv")
