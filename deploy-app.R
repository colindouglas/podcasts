library(tidyverse)
library(rsconnect)

secrets <- read_csv("../oauth_keys.csv")

shiny_token <- secrets %>%
  filter(website == "shinyapps") %>%
  pull(token)

shiny_secret <- secrets %>%
  filter(website == "shinyapps") %>%
  pull(secret)

setAccountInfo(name = "colindouglas",
                          token = shiny_token,
                          secret = shiny_secret)

deployApp(appDir = "./", 
          upload = TRUE,
          appFiles = c("./data/earwolf_podcasts.Rda", "app.R"),
          appTitle = "podcasts")
