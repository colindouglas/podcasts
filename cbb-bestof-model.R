library(tidyverse)
library(lubridate)

load(file = "data/earwolf_podcasts.Rda")

# Episode numbers of the Best Of'd episodes
BOs <- read_csv("data/cbb_bestof-episodes.csv")

# Clean up the Earwolf podcasts so there are only CBB podcasts
cbb <- earwolf_podcasts %>%
  filter(podcast == "Comedy Bang Bang", 
         !grepl("vote", title, ignore.case = TRUE), # Remove the entries that are just calls to vote in the Best Of's
         !grepl("BO", number, ignore.case = TRUE), # Remove the Best of Episodes themselves, since they're (usually) not eligible
         !grepl("10th Anniversary", title, ignore.case = TRUE) # Remove 10th Anniversary Special, which wasn't eligible
  ) %>% mutate(year = year(date),
             guest_count = lengths(guests),
             BO = number %in% BOs,
             year = year(date + days(40)) # If it's after Thanksgiving, it counts in nexy year's best of's
  )

# How frequently must a guest appear in the training set to be relevant?
frequent_enough <- function(x) {
  if (!is.logical(x)) return(TRUE)
  sum(x) >= 3
}

# What is the range of years that we want to test?
#current_era <- 2020:2017
current_era <- 2019:2015

# Number of appearance by each guest
appearances <- cbb %>%
  filter(year %in% current_era, year != year(now())) %>%
  unnest_longer(col = guests) %>%
  count(guests, name = "appearances") %>%
  rename(guest = guests)


# Reshape column into very wide, ugly dataframe that plays will with our model
cbb_wide <- cbb %>%
  select(number, guests, year) %>%
  unnest_longer(col = guests) %>%
  reshape2::dcast(., number ~ guests, fun.aggregate = { function(x) as.logical(length(x))}, value.var = "guests") %>%
  left_join(select(BOs, number, rank), by = c("number")) %>%
  left_join(select(cbb, number, year), by = "number") %>%
  select(number, year, rank, everything()) 

# Make rank an ordered factor, when unranked episodes are ranked one more than the last ranking
last_rank <- max(cbb_wide$rank, na.rm = TRUE)
#cbb_wide$rank[is.na(cbb_wide$rank)] <- last_rank + 1
cbb_wide$rank[is.na(cbb_wide$rank)] <- NA
cbb_wide$rank <- factor(cbb_wide$rank, ordered = TRUE, levels = (last_rank):1)

# Fit using the "recent era" of Comedy Bang Bang, 2017 - 2019
cbb_recent <- cbb_wide %>%
  filter(year %in% current_era) %>%
  select_if(frequent_enough)

# This training set, everything except this year
cbb_train <- cbb_recent %>%
  filter(year != max(current_era))

# The test set, episodes from this year
cbb_test <- cbb_recent %>%
  filter(year == max(current_era)) 

# Fit the model
model <- glm(rank ~ ., data = select(cbb_train, -number, -year), family = binomial)

# Coefficients
coeffs <- broom::tidy(model) %>%
  arrange(-estimate) %>%
  mutate(term = gsub("(`)|(`TRUE)", "", term)) %>%
  select(guest = term, estimate, std.error) %>%
  left_join(appearances, by = c("guest")) %>%
  select(guest, appearances, everything())

# Make predictions for the current year episodes
predictions <- cbb_test %>%
  select(number) %>%
  mutate(score = predict(model, newdata = cbb_test)) %>%
  left_join(cbb, by = "number") %>%
  select(score, number, title, date, guests) %>%
  mutate(rank_estimate = rank(-score, ties.method = "first")) %>%
  left_join(BOs, by = "number") %>%
  mutate(diff = rank_estimate - rank)

        