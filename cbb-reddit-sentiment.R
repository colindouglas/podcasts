library(tidyverse)
library(tidytext)
library(corrplot)

### Read in the collection of reddit comments on CBB threads
comments <- read_csv("data/cbb_reddit_comments.csv", col_types = RedditColTypes("comment")) %>%
  
  ### Only keep the important columns
  select(name, link_id, created, body, upvotes = score) %>%

  ### Set the post_date column to date format
  mutate(created = as.Date(as.POSIXct(as.numeric(created), origin="1970-01-01")), comment = body) %>%

  ### Split the comment field into separate rows for each word
  unnest_tokens(word, body) %>%
  
  ### Find the sentiment of each word using the "afinn" unigram list
  left_join(get_sentiments("afinn")) %>%

  ### Group by the fields that are unique to each comment
  group_by(name, link_id, created, upvotes, comment) %>%
  
  ### Calculate the sentiment of each comment through the sum of the sentiment of all the words, also count the number of words in the comment
  summarize(sentiment = sum(score, na.rm = TRUE), words = n()) %>%
  mutate(rSentiment = sentiment/words)
  
  ### Group by the fields that are unique to each thread
thread_sentiment <- comments %>%
  group_by(link_id) %>%
  summarize(wrSent = mean(rSentiment), 
            words = sum(words), 
            sentiment = sum(sentiment), 
            upvotes = sum(upvotes),
            avgCommentLength = mean(words),
            totalComments = n(),
            positiveComments = sum(sentiment > 0),
            negativeComments = sum(sentiment < 0),
            neutralComments  = sum(sentiment == 0),
            last_comment_date = max(created)) %>%
  mutate(rSent = sentiment / words) %>%
  arrange(desc(wrSent)) 
  

thread_info <- read_csv("data/cbb_reddit_threads.csv", col_types = RedditColTypes("thread")) %>%
  inner_join(thread_sentiment, by = c("name" = "link_id")) %>%
  mutate(created = as.Date(anytime::anytime(created))) %>%
  mutate(tail = as.numeric(last_comment_date - created))

write_csv(thread_info, "data/cbb_reddit_sentiment.csv")

### Scatter plot to check for correlations
thread_info %>%
  ggplot(aes(
    x = created, 
    y = wrSent)) + 
  geom_point() + geom_smooth(method = "lm")

### Correlation plot to see what correlates
thread_info %>%
  group_by() %>%
  select(-view_count, -num_crossposts, -gilded, -num_comments, -ups) %>%
  select_if(is.numeric) %>%
  cor() %>% corrplot(method = "ellipse", type = "upper")