# Invoke some libraries ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggthemes)


### Define a function that calculates the first Monday of the year
FirstMonday <- function(year) {
  day <- as.Date(paste0(year, "-01-01", format = "%Y-%m-%d"))
  while (weekdays(day) != "Monday") day <- day + 1
  return(as.Date(day)) 
}

### Define a function to check if a given element is in a column of lists
### Example: episode$number[in.list("Lauren Lapkus", episode$guests)]
### returns all of the episode numbers with Lauren Lapkus
in.list <- function(element, list) {
  sapply(list, is.element, el = element)
}



# Import the data ---------------------------------------------------------

### Episode numbers of the Best Of'd episodes
BOs <- read_csv("data/cbb_bestof_episodes.csv") %>%
  pull(number)

cbb <- episode %>%
  mutate(year = year(date),
         sinceMonday = date - as.Date(sapply(year, FirstMonday), origin="1970-01-01"),
         guest_count = lengths(guests),
         BO = number %in% BOs
  )

### Fix the year if the Best Of episodes leaked into the following year
BO_numbers <- as.character(cbb$number[grepl("BO", cbb$number)])
cbb$year[cbb$number %in% BO_numbers] <- as.numeric(substring(BO_numbers,3,6))

### Make a vector of all of the guests
all_guests <- c()
for (i in 1:nrow(cbb)) all_guests <- c(all_guests, cbb$guests[[i]])

### Make a vector of all of the guests in the bestof eps
bo_guests <- c()
cbb_bo <- subset(cbb, BO == T)
for (i in 1:nrow(cbb_bo)) bo_guests <- c(bo_guests, cbb_bo$guests[[i]])

### Make a vector of only the unique guests
unique_guests <- unique(all_guests)

### Make a logical column in the data frame for a given guests's appearance in an episode
### This code does the same as before but much faster
for (i in 1:length(unique_guests)) {
      cbb[[unique_guests[i]]] <- in.list(unique_guests[i], cbb$guests)
  }
rm(i)


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
    xmin = sinceMonday - 2, 
    xmax = sinceMonday + 2, 
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
  geom_point(size = dot_size, color=colors[1], aes(x = sinceMonday, y = year-4*offset, alpha=get(top_guests[1]))) +
  geom_point(size = dot_size, color=colors[2], aes(x = sinceMonday, y = year-3*offset, alpha=get(top_guests[2]))) +
  geom_point(size = dot_size, color=colors[3], aes(x = sinceMonday, y = year-2*offset, alpha=get(top_guests[3]))) +
  geom_point(size = dot_size, color=colors[4], aes(x = sinceMonday, y = year-1*offset, alpha=get(top_guests[4]))) +
  geom_point(size = dot_size, color=colors[5], aes(x = sinceMonday, y = year, alpha=get(top_guests[5]))) +
  geom_point(size = dot_size, color=colors[6], aes(x = sinceMonday, y = year + 1*offset, alpha=get(top_guests[6]))) +
  geom_point(size = dot_size, color=colors[7], aes(x = sinceMonday, y = year + 2*offset, alpha=get(top_guests[7]))) +
  geom_point(size = dot_size, color=colors[8], aes(x = sinceMonday, y = year + 3*offset, alpha=get(top_guests[8]))) +
  geom_point(size = dot_size, color=colors[9], aes(x = sinceMonday, y = year + 4*offset, alpha=get(top_guests[9]))) +
  geom_point(data=dummy_table, alpha=1, aes(x=x, y=y, color=colors)) +
  
  ### Draw a vertical line at Thanksgiving (Day 325) and label it
  annotate("text", x = 325, y = 2009-6*offset, label = "Thanksgiving Cut-Off", size = 2.5, color="darkgrey", vjust=0) +
  geom_segment(color="darkgrey", lty=2, aes(x = 325, y = 2008.5, xend = 325, yend = 2017.5)) +
  
  ### Add a title and a subtitle.
  ggtitle("Top Comedy Bang Bang Guests by Episode", subtitle = "Best Of'd Episodes Highlighted")

ggsave(filename = "cbb-bestof-plot.png", width = 12, height = 6, dpi = 500)

# Make a linear model -----------------------------------------------------

### This is bad and hacked together and is probably bad stats but it's fun.
### Don't take it too seriously
bo_lm <- lm(data = (cbb %>% 
                      filter(!grepl("BO", cbb$number)) %>%
                      select(which(colnames(cbb) == "BO"):ncol(cbb))), 
            BO ~ .)

### Remake the top list with the right number
top_guests <- names(sort(table(all_guests), decreasing = T))
top_guests_table <- data.frame(sort(table(all_guests), decreasing=T)) %>%
  filter(Freq > 4)

names(top_guests_table) <- c("name", "count")

### Put the LM weights into a data frame
lm_table <- data.frame(name = c("Intercept", top_guests), lm_weight = bo_lm[[1]])

### Make a vector of all the guests in Best Of episopdes
cbb_bo <- subset(cbb, BO == T)
bo_guests <- c()
for (i in 1:nrow(cbb_bo)) bo_guests <- c(bo_guests, cbb_bo$guests[[i]])

### Make a summary table of all of the guests in the best ofs
df_boguests <- data.frame(table(bo_guests))

### Make a pretty data frame with summaries
summary <- left_join(top_guests_table, df_boguests, by=c("name" = "bo_guests"))
names(summary) <- c("guest", "ep", "bo")
summary <- mutate(summary, BO_rate = round(bo/ep, 3))
summary <- rbind(c("Intercept", 0,0,0), summary)
summary <- left_join(summary, lm_table, by=c("guest" = "name"))
summary$lm_weight <- round(summary$lm_weight, 3)
lm_df <- data_frame(coef(summary(bo_lm)[,2:4]))

### Add significance symbols for quick reference
summary$sig <- ""
summary$sig[summary$p < 0.1] <- "."
summary$sig[summary$p < 0.05] <- "*"
summary$sig[summary$p < 0.01] <- "**"
summary$sig[summary$p < 0.001] <- "***"

write_csv(summary, path="cbb-bestof-lm-summary.csv")


