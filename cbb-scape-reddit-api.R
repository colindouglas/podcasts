library(tidyverse)

# httr bugfix from https://github.com/r-lib/httr/pull/485 -----------------

library(devtools)
devtools::install_github("r-lib/httr#485", force = TRUE)
library(httr)

oauth <- read_csv("oauth_keys.csv")

# 1. Find OAuth settings for reddit:
#    https://github.com/reddit/reddit/wiki/OAuth2
endpoint <- oauth_endpoint(
  authorize = "https://www.reddit.com/api/v1/authorize",
  access =    "https://www.reddit.com/api/v1/access_token"
)

# 2. Register an application at https://www.reddit.com/prefs/apps
app <- oauth_app("Test_App", oauth$key, oauth$secret)

# 3b. If get 429 too many requests, the default user_agent is overloaded.
# If you have an application on Reddit then you can pass that using:
token <- oauth2.0_token(
  endpoint = endpoint, 
  app=app,
  scope = c("read", "modposts"),
  use_basic_auth = TRUE,
  #  cache = F,
  config_init = user_agent("Earwolf Comment Scraper")
)

#trying to make a call
#Important! Make sure to put oauth.reddit.com instad of reddit.com!
request_url <- "https://oauth.reddit.com/r/Earwolf/search?q=Comedy+Bang+Bang"

response <- GET(request_url,
                user_agent("Testing Oauth with httr"),
                config(token = token)
)

response


