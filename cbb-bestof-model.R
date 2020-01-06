library(tidyverse)
library(tidymodels)
library(lubridate)

load(file = "data/earwolf_podcasts.Rda")

### Episode numbers of the Best Of'd episodes
BOs <- read_csv("data/cbb_bestof-episodes.csv")

# Clean up the Earwolf podcasts so there are only CBB podcasts
cbb <- earwolf_podcasts %>%
  filter(podcast == "Comedy Bang Bang", 
         !grepl("vote", title, ignore.case = TRUE) # Remove the entries that are just calls to vote in the Best Of's
  ) %>%
  mutate(year = year(date),
         guest_count = lengths(guests),
         BO = number %in% BOs,
         BO_year = year(date + days(40)), # If it's after Thanksgiving, it counts in nexy year's best of's
         thanksgiving = as_date(325, origin = paste0(BO_year - 1, "-01-01")),
         since_thanksgiving = date - thanksgiving) %>% 
  left_join(BOs, by = c("BO_year" = "year", "number")) %>%
  rename(BO_rank = rank)
