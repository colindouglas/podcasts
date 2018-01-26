# Invoke some libraries ---------------------------------------------------

library(tidyverse)
library(lubridate)

# Define some data --------------------------------------------------------

### Define a function that calculates the first Monday of the year
FirstMonday <- function(year) {
  day <- as.Date(paste0(year, "-01-01", format = "%Y-%m-%d"))
  while (weekdays(day) != "Monday") day <- day + 1
  return(as.Date(day)) 
}

### Define a function to check if a given value is in a vector of lists
### Example: episode$number[InList("Lauren Lapkus", episode$guests)]
### returns all of the episode numbers with Lauren Lapkus
in_list <- function(value, list) {
  sapply(list, is.element, el = value)
}

### A function that checks if a URL redirects to another address
CheckRedirect <- function(url) {
  return(httr::GET(url)$url)
}

flatten_time <- function(x = created, y) {
  x <- as.numeric(x)
  lm <- lm(y ~ x)
  b <- summary(lm)$coefficients[1]
  m <- summary(lm)$coefficients[2]
  normed_y <- y - (x * m + b)
  return(normed_y)
}

### Read in some data
earwolf_data <- read_csv("data/cbb_earwolf_scrape.csv") %>%
  mutate(guests = strsplit(guests, ", "))
bestof_data <- read_csv("data/cbb_bestof_episodes.csv")
sentiment_data <- read_csv("data/cbb_reddit_sentiment.csv")

unmatched_links <- sentiment_data %>%
  anti_join(earwolf_data, by=c("url" = "url"))

unmatched_links <- unmatched_links %>%
  mutate(url = gsub("http://earwolf.com", "http://www.earwolf.com", unmatched_links$url)) %>%
  rowwise %>%
  mutate(url = CheckRedirect(url))

### Check if any of the URLs are redirects, get the URL it redirects to instead

combined_data <- sentiment_data %>%
  bind_rows(unmatched_links) %>%
  left_join(earwolf_data, ., by = c("url" = "url")) %>%
  left_join(bestof_data, by = c("number" = "number")) %>%
  mutate(BOrank = rank) %>%
  select(-year, rank) %>%
  mutate(BO = number %in% unique(bestof_data$number))

### Make a vector of all of the guests
all_guests <- unlist(combined_data$guests)
 
### Find the top 20 guests (keeping ties)
top_guests <- all_guests %>%
  data_frame(guest = .) %>%
  group_by(guest) %>%
  tally() %>%
  top_n(20) %>%
  arrange(desc(n)) %>%
  pull(guest)

### Select the data that we will feed into the model
predict_data <- combined_data %>%
  rowwise() %>%
  mutate(nScore = flatten_time(created, score)) %>%
  select(date, number, guests, num_comments, 
         tail, wrSent, words, sentiment, upvotes, 
         avgCommentLength, totalComments, positiveComments, 
         negativeComments, neutralComments, rSent, BOrank, BO)



## Make a logical column in the data frame for a given guests's appearance in an episode
for (i in 1:length(top_guests)) predict_data[[top_guests[i]]] <- in_list(top_guests[i], predict_data$guests)
rm(i)

### Remove some more columns
predict_data <- select(predict_data, -guests, -date, -number, -BOrank, -avgCommentLength, -neutralComments)

### Make the model
library(rpart)

fit <- rpart(BO ~ ., data = predict_data, method = "class")

plot(fit)
text(fit)








# Make the plot -----------------------------------------------------------

### How big are the dots in the plot
dot_size <- 2

### How far away are the dots from each other on the y-axis (in units of a year)
offset <- 0.1

### The colors of the dots, top to bottom
colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#8dd3c7", "#a65628", "#f781bf", "#999999")

### A dummy table to hack together a legend
dummy_table <- data.frame(x=1:9, y=2008)

### Find the top 9 guests and how many episodes they've appeared in
top_guests <- names(sort(table(all_guests), decreasing = T)[1:9])
top_guests_table <- data.frame(sort(table(all_guests), decreasing=T)[1:9])
names(top_guests_table) <- c("name", "count")

ggplot(cbb) +
  theme_few() +
  ### Draw a filled rectangle around each episode in the Best Of lists
  geom_rect(data = filter(cbb, BO == T), alpha = 0.3, aes(
    xmin = SinceMonday - 2, 
    xmax = SinceMonday + 2, 
    ymin = year - (5*offset), 
    ymax = year + (5*offset)
  )) +
  
  ### Set the transparency of the dots for T/F
  scale_alpha_manual(values = c(0.05,0.8), guide=F) + 
  
  ### Flip the y axis around so earlier is at the top
  scale_y_reverse(breaks=2009:2017, name = "Year", limits=c(max(cbb$year)+5*offset,min(cbb$year)-6*offset)) +
  
  scale_x_continuous(name = "Day (Mondays Aligned)", breaks=seq(0, 365, 30)) +
  scale_color_manual("Guests", labels = paste0(top_guests_table$name, " (", top_guests_table$count, ")"), values = colors, guide="legend") +
  
  ### Draw points for each episode, and adjust the transparency based on whether the 
  geom_point(size = dot_size, color=colors[1], aes(x = SinceMonday, y = year-4*offset, alpha=get(top_guests[1]))) +
  geom_point(size = dot_size, color=colors[2], aes(x = SinceMonday, y = year-3*offset, alpha=get(top_guests[2]))) +
  geom_point(size = dot_size, color=colors[3], aes(x = SinceMonday, y = year-2*offset, alpha=get(top_guests[3]))) +
  geom_point(size = dot_size, color=colors[4], aes(x = SinceMonday, y = year-1*offset, alpha=get(top_guests[4]))) +
  geom_point(size = dot_size, color=colors[5], aes(x = SinceMonday, y = year, alpha=get(top_guests[5]))) +
  geom_point(size = dot_size, color=colors[6], aes(x = SinceMonday, y = year + 1*offset, alpha=get(top_guests[6]))) +
  geom_point(size = dot_size, color=colors[7], aes(x = SinceMonday, y = year + 2*offset, alpha=get(top_guests[7]))) +
  geom_point(size = dot_size, color=colors[8], aes(x = SinceMonday, y = year + 3*offset, alpha=get(top_guests[8]))) +
  geom_point(size = dot_size, color=colors[9], aes(x = SinceMonday, y = year + 4*offset, alpha=get(top_guests[9]))) +
  geom_point(data=dummy_table, alpha=1, aes(x=x, y=y, color=colors)) +
  
  ### Draw a vertical line at Thanksgiving (Day 325) and label it
  annotate("text", x = 325, y = 2009-6*offset, label = "Thanksgiving Cut-Off", size = 2.5, color="darkgrey", vjust=0) +
  geom_segment(color="darkgrey", lty=2, aes(x = 325, y = 2008.5, xend = 325, yend = 2017.5)) +
  
  ### Add a title and a subtitle.
  ggtitle("Top Comedy Bang Bang Guests by Episode", subtitle = "Best Of'd Episodes Highlighted")

ggsave(filename = "cbb-bestof-plot.png", width = 12, height = 6, dpi = 500)

