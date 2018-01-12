library(tidyverse)
library(tidytext)

redditComments <- read_csv("reddit_comments_scape.csv") %>%
  select(post_date, comment_score, comment, link, structure, post_score, upvote_prop) %>%
  mutate(post_date = as.Date(post_date, format = "%d-%m-%y")) %>%
  unnest_tokens(word, comment) %>%
  anti_join(stop_words) %>%
  left_join(get_sentiments("bing")) %>%
  mutate(sentiment = case_when(
    sentiment == "negative" ~ -1,
    sentiment == "positive" ~  1,
    TRUE ~ 0)) %>%
  group_by(post_date, link, post_score, upvote_prop) %>%
  summarize(sentiment = sum(sentiment, na.rm = TRUE), words = n()) %>%
  mutate(sentimentRate = sentiment / words) %>%
  arrange(desc(sentimentRate))

redditComments %>%
  filter(upvote_prop > 0.6) %>%
  ggplot(aes(x = upvote_prop, y = sentimentRate)) + 
  geom_point() + labs(x = "", y = "")

model <- lm(data = redditComments, formula = post_score ~ sentiment)

redditComments %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


