library(tidyverse)
library(tidytext)

### Read in the collection of reddit comments on CBB threads
redditComments <- read_csv("reddit_comments_scape.csv") %>%
  
  ### Only keep the important columns
  select(post_date, comment_score, comment, link, structure, post_score, upvote_prop) %>%
  
  ### Set the post_date column to date format
  mutate(post_date = as.Date(post_date, format = "%d-%m-%y")) %>%
  
  ### Split the comment field into separate rows for each row
  unnest_tokens(word, comment) %>%
  
  ### Find the sentiment of each word using the "bing" unigram list
  left_join(get_sentiments("bing")) %>%
  
  ### Weight the negative words twice as strongly as the positive
  mutate(sentiment = case_when(
    sentiment == "negative" ~ -2,
    sentiment == "positive" ~  1,
    TRUE ~ 0)) %>%
  
  ### Group by the fields that are unique to each comment
  group_by(post_date, link, post_score, upvote_prop, structure) %>%
  
  ### Attempt to classify each comment as positive (+1) or negative (-1), and count the words in each
  summarize(sentiment = case_when(
    sum(sentiment, na.rm = TRUE) > 0 ~  1,
    sum(sentiment, na.rm = TRUE) < 0 ~ -1,
    TRUE ~ 0), words = n()) %>%
  
  ### Group by the fields that are unique to each thread
  group_by(post_date, link, post_score, upvote_prop) %>%
  
  ### Calculate the total sentiment of each thread, the average comment length, and the number of comments
  summarize(sentiment = sum(sentiment), avgCommentLength = mean(words), commentCount = n()) %>%
  
  ### Calculate the sentiment per comment, and a z-score for sentiment
  mutate(rSentiment = (sentiment / commentCount)) %>%
  mutate(zSentiment = (rSentiment - mean(.$rSentiment))/sd(.$rSentiment)) %>%
  
  ### Drop the rSentiment column
  select(-rSentiment) %>%
  
  ### Arrange by sentiment z-score
  arrange(desc(zSentiment))

### Output the data
write_csv(redditComments, "cbb_reddit_sentiment.csv")

redditComments %>%
  ggplot(aes(x = post_score, y = zSentiment)) + 
  geom_point() + geom_smooth(method = "lm")

redditComments %>%
  group_by() %>%
  select_if(is.numeric) %>%
  cor() %>% corrplot(method = "ellipse")
