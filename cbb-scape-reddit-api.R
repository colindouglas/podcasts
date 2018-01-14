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

SearchEarwolfSubreddit <- function(searchTerm) {
  
  ### The initial API call to get search results
  initialURL <- paste0("https://oauth.reddit.com/r/Earwolf/search.json?q=", searchTerm, "+site%3Aearwolf.com&sort=new&type=link&restrict_sr=TRUE&t=all&raw_json=1&limit=100")

  ### Perform the search API call
  response <- GET(initialURL,
                  user_agent("Earwolf Comment Scraper"),
                  config(token = token)) %>% content()

  ### Extract the data from the parsed JSON response into a dataframe
  threads <- data_frame(
    datetime = anytime::anytime(map_dbl(response$data$children, ~ .x$data$created)),
    title = map(response$data$children, ~ .x$data$title),
    link = map(response$data$children, ~ .x$data$url),
    fullname = map(response$data$children, ~ .x$data$name),
    gilded = map(response$data$children, ~ .x$data$gilded),
    poster = map(response$data$children, ~ .x$data$author),
    score = map(response$data$children, ~ .x$data$score),
    upvotes = map(response$data$children, ~ .x$data$ups),
    downvotes = map(response$data$children, ~ .x$data$downs),
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
    Sys.sleep(1.1)

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
      datetime = anytime::anytime(map_dbl(response$data$children, ~ .x$data$created)),
      title = map(response$data$children, ~ .x$data$title),
      link = map(response$data$children, ~ .x$data$url),
      fullname = map(response$data$children, ~ .x$data$name),
      gilded = map(response$data$children, ~ .x$data$gilded),
      poster = map(response$data$children, ~ .x$data$author),
      score = map(response$data$children, ~ .x$data$score),
      upvotes = map(response$data$children, ~ .x$data$ups),
      downvotes = map(response$data$children, ~ .x$data$downs),
      permalink = map(response$data$children, ~ .x$data$permalink),
      numComments = map(response$data$children, ~ .x$data$num_comments)
    )
    

    ### Combine the old data with the new
    threads <- bind_rows(threads, newThreads)
  }
  print(paste("Done searching for", searchTerm, "after", searchPage, "pages"))
  return(threads)
}
GetComments <- function(threadID) {
  
  ### Construct the API request to get all of the comment IDs of a thread
  threadURL <- paste0("https://oauth.reddit.com/r/Earwolf/comments/", substring(threadID,4), ".json")
  
  ### Make an API call to get all of the info in the thread
  threadResponse <- GET(threadURL,
                        user_agent("Earwolf Comment Scraper"),
                        config(token = token)) %>% content() %>% unlist()
  
  ### A hacky way to find all of the comments in the thread
  allCommentIDs <- threadResponse[grepl("data\\.id", names(threadResponse))]
  
  redditComments <- data_frame()
  ### Get the info on all of the comment IDs we just found
 # for (i in 2:length(allCommentIDs)) {
  for (i in 2:5) {
    
    print(paste("Getting comment", allCommentIDs[i], i-1, "of", length(allCommentIDs)-1))
    ### Wait to response the API results
    Sys.sleep(1.1)
    
    ### Construct the API call using the comment ID
    commentURL <- paste0("https://oauth.reddit.com/r/Earwolf/api/info.json?id=t1_", allCommentIDs[i])
    
    ### Make the API call to get info on the comment
    commentResponse <- GET(commentURL,
                           user_agent("Earwolf Comment Scraper"),
                           config(token = token)) %>% content()
    
    ### Parse the data out of the response
    newComment <- data_frame(
      datetime = anytime::anytime(commentResponse$data$children[[1]]$data$created),
      commentID = commentResponse$data$children[[1]]$data$name,
      threadID = commentResponse$data$children[[1]]$data$link_id,
      parentID = commentResponse$data$children[[1]]$data$parent_id,
      author = commentResponse$data$children[[1]]$data$author,
      score = commentResponse$data$children[[1]]$data$score,
      text = commentResponse$data$children[[1]]$data$body,
      gilded = commentResponse$data$children[[1]]$data$gilded
    )

    redditComments <- bind_rows(redditComments, newComment)
  }
  return(redditComments)
}
PossLength <- possibly(length, otherwise = 0)

### Get all of the threads that have "CBB" or "comedy bang bang" in the title
redditThreads <- SearchEarwolfSubreddit("comedy+bang+bang") %>%
  bind_rows(SearchEarwolfSubreddit("CBB")) %>%
  distinct(fullname, .keep_all = TRUE)


### Start comment scraping

redditComments <- GetComments(redditThreads$fullname[1])

threadID <- redditThreads$fullname[1]

### Construct the API request to get all of the comment IDs of a thread
threadURL <- paste0("https://oauth.reddit.com/r/Earwolf/comments/", substring(threadID,4), ".json")

### Make an API call to get all of the info in the thread
threadResponse <- GET(threadURL,
                      user_agent("Earwolf Comment Scraper"),
                      config(token = token)) %>% content()

topLevelComments <- data_frame(
  commentID = map(threadResponse[[2]]$data$children, ~ .$data$name),
  level = 1,
  levelNumber = 1:length(map(threadResponse[[2]]$data$children, ~ .$data$name)),
  datetime = anytime::anytime(map_dbl(threadResponse[[2]]$data$children, ~ .$data$created)),
  linkID = map(threadResponse[[2]]$data$children, ~ .$data$link_id),
  parentID = map(threadResponse[[2]]$data$children, ~ .$data$parent_id),
  replies = map(threadResponse[[2]]$data$children, ~ PossLength(.$data$replies$data$children)),
  author = map(threadResponse[[2]]$data$children, ~ .$data$author),
  gilded = map(threadResponse[[2]]$data$children, ~ .$data$gilded),
  text = map(threadResponse[[2]]$data$children, ~ .$data$body),
  score = map(threadResponse[[2]]$data$children, ~ .$data$score),
  permalink = map(threadResponse[[2]]$data$children, ~ .$data$permalink)
)


modify_depth(threadResponse, 3)




map2(commentsWithReplies$levelNumber

str(threadResponse[[2]]$data$children[[3]]$data$replies$data$children[[1]]$data, max.level = 2) ## Second level

str(threadResponse[[2]]$data$children[[3]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data, max.level = 2) ## third level

str(threadResponse[[2]]$data$children[[3]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data, max.level = 2) ## fourth level

