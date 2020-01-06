# Invoke some libraries ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggthemes)

### Define a function to check if a given element is in a column of lists
### Example: episode$number[in.list("Lauren Lapkus", episode$guests)]
### returns all of the episode numbers with Lauren Lapkus
in.list <- function(element, list) {
  sapply(list, is.element, el = element)
}

# Import the data ---------------------------------------------------------

### Episode numbers of the Best Of'd episodes
BOs <- read_csv("data/cbb_bestof-episodes.csv") %>%
  pull(number)

load(file = "data/earwolf_podcasts.Rda")

cbb <- earwolf_podcasts %>%
  filter(podcast == "Comedy Bang Bang", 
         !grepl("vote", title, ignore.case = TRUE) # Remove the entries that are just calls to vote in the Best Of's
         ) %>%
  mutate(year = year(date),
         guest_count = lengths(guests),
         BO = number %in% BOs,
         BO_year = year(date + days(40)), # If it's after Thanksgiving, it counts in nexy year's best of's
         thanksgiving = as_date(325, origin = paste0(BO_year - 1, "-01-01")),
         since_thanksgiving = date - thanksgiving
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

# Make the plot -----------------------------------------------------------

# The start of each month relative to day 325 (Thanksgiving-ish)

month_starts <- paste0(1:12, "-01-2018") %>% mdy() %>% yday()
month_starts[month_starts < 325] <- month_starts[month_starts < 325] + 365
month_starts <- month_starts - 325



### How big are the dots in the plot
dot_size <- 2

### How far away are the dots from each other on the y-axis (in units of a year)
offset <- 0.1

### The colors of the dots, top to bottom
colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#8dd3c7", "#a65628", "#f781bf", "#999999")

### A dummy table to hack together a legend
dummy_table <- data.frame(x=1:9, y=2008)

### Find the top 9 guests and how many episodes they've appeared in
top_guests <- names(sort(table(bo_guests), decreasing = T)[1:9])
top_guests_table <- data.frame(sort(table(bo_guests), decreasing=T)[1:9])
names(top_guests_table) <- c("name", "count")

cbb_bestof_plot <- ggplot(cbb) +
  theme_few() +
  ### Draw a filled rectangle around each episode in the Best Of lists
  geom_rect(data = filter(cbb, BO == T), alpha = 0.3, aes(
    xmin = since_thanksgiving - 2, 
    xmax = since_thanksgiving + 2, 
    ymin = BO_year - (5*offset), 
    ymax = BO_year + (5*offset)
  )) +
  
  ### Set the transparency of the dots for T/F
  scale_alpha_manual(values = c(0.05,0.8), guide=F) + 
  
  ### Flip the y axis around so earlier is at the top
  scale_y_reverse(breaks = min(cbb$BO_year):max(cbb$BO_year), name = "Year", limits=c(max(cbb$BO_year)+5*offset,min(cbb$BO_year)-6*offset)) +
  
  scale_x_continuous(breaks = c(month_starts), labels = month.abb, name = "") +
  scale_color_manual("Guest (Best Of'd Eps)", labels = paste0(top_guests_table$name, " (", top_guests_table$count, ")"), values = colors, guide="legend") +
  
  ### Draw points for each episode, and adjust the transparency based on whether the 
  geom_point(size = dot_size, color=colors[1], aes(x = since_thanksgiving, y = BO_year - 4*offset, alpha=grepl(top_guests[[1]], guests))) +
  geom_point(size = dot_size, color=colors[2], aes(x = since_thanksgiving, y = BO_year - 3*offset, alpha=grepl(top_guests[[2]], guests))) +
  geom_point(size = dot_size, color=colors[3], aes(x = since_thanksgiving, y = BO_year - 2*offset, alpha=grepl(top_guests[[3]], guests))) +
  geom_point(size = dot_size, color=colors[4], aes(x = since_thanksgiving, y = BO_year - 1*offset, alpha=grepl(top_guests[[4]], guests))) +
  geom_point(size = dot_size, color=colors[5], aes(x = since_thanksgiving, y = BO_year,            alpha=grepl(top_guests[[5]], guests))) +
  geom_point(size = dot_size, color=colors[6], aes(x = since_thanksgiving, y = BO_year + 1*offset, alpha=grepl(top_guests[[6]], guests))) +
  geom_point(size = dot_size, color=colors[7], aes(x = since_thanksgiving, y = BO_year + 2*offset, alpha=grepl(top_guests[[7]], guests))) +
  geom_point(size = dot_size, color=colors[8], aes(x = since_thanksgiving, y = BO_year + 3*offset, alpha=grepl(top_guests[[8]], guests))) +
  geom_point(size = dot_size, color=colors[9], aes(x = since_thanksgiving, y = BO_year + 4*offset, alpha=grepl(top_guests[[9]], guests))) +
  geom_point(data=dummy_table, alpha=1, aes(x=x, y=y, color=colors)) +
  
  ### Draw a vertical line at Thanksgiving (Day 325) and label it
  annotate("text", x = 0, y = 2009-6*offset, label = "Thanksgiving", size = 2.5, color="darkgrey", vjust=0) +
  geom_segment(color="darkgrey", lty=2, aes(x = 0, y = 2008.5, xend = 0, yend = 2020.5)) +
  
  ### Add a title and a subtitle.
  ggtitle("Top Comedy Bang Bang Guests by Episode", subtitle = str_glue("Updated {Sys.Date()} // Best Of'd Episodes Highlighted"))



 