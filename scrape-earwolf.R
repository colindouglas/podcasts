library(rvest)
library(tidyverse)



# Function for reading HTML and tolerating errors
# Records any URLs that didn't work in a vector called "failures" so they can be retried at the end
safe_read_html <- function(next_ep) {
  out <- tryCatch(
    {
      read_html(next_ep)
    },
    error = function(cond) {
      failures <<- c(failures, next_ep)
      message("Failed: ", next_ep)
      message("Original error message: ", cond)
      return(NA)
    }
  )
  return(out)
}
failures <- c()

# Generalized Earwolf scraper
# Give it a URL and it will scrape all of the episodes from that episode onwards
scrape_earwolf <- function(url) {
    episode <- tibble()
    next_ep <- url
    
  while (length(next_ep) > 0) { # The next episode will be character(0) once it gets to the final episode
    #Sys.sleep(2)
    
    ### Scrape the HTML  
    html_scrape <- safe_read_html(next_ep)
    
    # If the request failed, moved on
    if (is.na(html_scrape)) {
      next 
    } else {
    # If the request succeeded, remove the URL from the vector of failures
      if (next_ep %in% failures) {
        failures <<- failures[failures != next_ep]
      } 
    }
    
    ### Get the episode date
    date <- html_scrape %>%
      html_node(".epidate") %>%
      html_text() %>%
      as.Date(format = "%b %d, %Y")
    
    ### Get the episode number, drop the first character because its a number sign
    title_number <- html_scrape %>%
      html_node(".epititle") %>%
      html_text() 
    
    podcast <- title_number %>%
      str_replace(pattern = "#.+", replacement = "") %>%
      str_trim()
    
    number <- title_number %>%
      str_extract("#.+") %>%
      substring(2)
    
    ### Get the episode title
    title <- html_scrape %>%
      html_node(".showtitle") %>%
      html_text() 
    
    message(podcast, " ", number, " - ", title)
    
    ### Get the episode description
    desc <- html_scrape %>%
      html_node(".episodeshowdesc") %>%
      html_text() %>%
      str_replace("\\n", "") %>%
      str_trim()
    
    ### Get the episode guests (there can be multiple)
    guest <- html_scrape %>%
      html_nodes(".clearfix") %>%
      html_text() 
    
    ### Get the episode photos if they exist
    photo_urls <- html_scrape %>%
      html_nodes(".epimgbox") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Cache the episode photos and get the exif data
    exif_dates <- map_chr(photo_urls, possibly(function(url) {
      dir.create(paste0("data/photos/", podcast), showWarnings = FALSE, recursive = TRUE)
      filename <- tail(str_split(url, pattern = "/")[[1]], 1)
      destination <- paste0("data/photos/", podcast, "/", number, " - ", filename)
      if (!file.exists(destination)) {
        download.file(url, destfile = destination)
      }
      
      exif_df <- exifr::read_exif(destination, tags = c("CreateDate"))
      
      if ("CreateDate" %in% colnames(exif_df)) {
        exif_df$CreateDate
      } else {
        as.character(NA)
      }
    }, otherwise = NA))
    
    # If there are multiple exif dates, take the earliest one
    if (sum(!is.na(exif_dates)) > 0) {
      exif_date <- min(lubridate::ymd_hms(exif_dates, quiet = TRUE), na.rm = TRUE)
    } else {
      exif_date <- NA
    }
    
    ### Make a new row with all of the data
    newrow <- tibble(podcast, number, title, date, exif_date, url = next_ep,
                     desc, guests = list(guest))
    
    ### Add the new row to the data frame
    episode <- bind_rows(episode, newrow)
    
    ### Find the next episode URL
    next_ep <- html_scrape %>% 
      html_nodes(".nextepisodelink") %>% 
      html_attr('href')
  }
    return(episode)
}

# URLS to start off with, if the podcast has never been checked before
starting_urls <- c(
  "https://www.earwolf.com/episode/icelandic-meat/", # Urgent Care
  "https://www.earwolf.com/episode/firsts-with-bill-hader/", # In Bed with Nick and Megan
  "https://www.earwolf.com/episode/donating-plasma/", # Get Rich Nick
  "https://www.earwolf.com/episode/ron-pauls-baby/", # Beautiful / Anonymous
  "https://www.earwolf.com/episode/nicole-was-gonna-ride-an-alligator/", # Best Friends
  "https://www.earwolf.com/episode/theyre-pine-nuts-with-klaus-kendall-paul-f-tompkins/", # Teacher's Lounge
  "https://www.earwolf.com/episode/lifes-a-pitch-2/", # Bitch Sesh
  "https://www.earwolf.com/episode/welcome-to-comedy-bang-bang/", # CBB
  "https://www.earwolf.com/episode/beyonce-for-vice-president/", 
  "https://www.earwolf.com/episode/will-ferrell-2/", # CONAF
  "https://www.earwolf.com/episode/generation-gaps/", # Cracked
  "https://www.earwolf.com/episode/analyze-phish/", # Earwolf Presents
  "https://www.earwolf.com/episode/guns-corporations-the-constitution-w-adam-winkler/", # Factually
  "https://www.earwolf.com/episode/beyonce-for-vice-president/", # Fake the Nation
  "https://www.earwolf.com/episode/donating-plasma/", # Get Rich Nick
  "https://www.earwolf.com/episode/whats-the-difference-between-sunni-and-shia-muslims-why-dont-they-love-each-other-with-dr-james-gelvin-professor-of-islamic-studies/", # Getting Curious
  "https://www.earwolf.com/episode/hello-from-the-magic-tavern/", # HFTMT
  "https://www.earwolf.com/episode/our-close-friend-jake-johnson/", #HoHa,
  "https://www.earwolf.com/episode/break-ups-baseball-boys-in-books/", #Homophilia
  "https://www.earwolf.com/episode/sonic-06-with-jordan-morris/", # HDTGP
  "https://www.earwolf.com/episode/burlesque/", # HDTGM
  "https://www.earwolf.com/episode/9641/", # I4H
  "https://www.earwolf.com/episode/ep-1-shrugging-destiny-w-paul-f-tompkins/", # Off Book
  "https://www.earwolf.com/episode/the-pilot/", # Office Ladies
  "https://www.earwolf.com/episode/rhea-butcher/", # Queery
  "https://www.earwolf.com/episode/from-boy-to-under-a-blood-red-sky/", #UTU2TM
  "https://www.earwolf.com/episode/all-things-latinx-w-curly-velasquez/", # Spanish Aqui Presents
  "https://www.earwolf.com/episode/scam-heiress-anna-sorokin-with-paul-f-tompkins/", # Scam Goddess
  "https://www.earwolf.com/episode/three-questions-episode-1/", # Andy Richter
  "https://www.earwolf.com/episode/this-was-a-mistake-3/", # Threedom
  "https://www.earwolf.com/episode/ts223-oj-simpsonkeshaleviticusguest-ross-mathews/", # Throwing Shade
  "https://www.earwolf.com/episode/to-space-but-further/", # Voyage to the Stars
  "https://www.earwolf.com/episode/jimmies/", # Yo Is This Racist
  "https://www.earwolf.com/episode/citizen-kane/" # Unspooled
  )

# Randomize the order
starting_urls <- sample(starting_urls)

# Load previously-scraped data
if (file.exists("data/earwolf_podcasts.Rda")) {
  load(file = "data/earwolf_podcasts.Rda")
} else {
  # If the RDA file doesn't exist, make it an empty dataframe
  earwolf_podcasts <- tibble()
  save(earwolf_podcasts, file = "data/earwolf_podcasts.Rda")
}

# A function to check if a URL has been scraped before
# If it hasn't been scraped, it starts from there
# If it has been scraped, it starts from the most recent un-scraped episodes
walk_over_podcasts <- function(urls) {
  walk(urls, function(start_url) {
    
    # If we've already scraped the URL, find if there's a new episode
    if (start_url %in% earwolf_podcasts$url) {
      this_podcast <- earwolf_podcasts %>%
        filter(url == start_url) %>%
        slice(1) %>%
        pull(podcast)
      
      last_url <- earwolf_podcasts %>%
        filter(podcast == this_podcast) %>%
        mutate(date = lubridate::ymd(date)) %>%
        arrange(desc(date)) %>%
        slice(1) %>%
        pull(url)
      
      # Just in case of 500s
       html_scrape <- safe_read_html(last_url) 

       if (!is.na(html_scrape)) {
         start_url <- html_scrape %>%
           html_nodes(".nextepisodelink") %>% 
           html_attr('href')
       } else {
         failures <<- c(failures, last_url)
       }
    }
    
    # If there's no new episodes, return nothing and print a message
    if (length(start_url) == 0) {
      message("No new episodes of ", this_podcast)
      return()
    }
    
    # Otherwise, scrape new episodes starting from 'start_url'
    new_data <- scrape_earwolf(start_url)
    
    # Append to Rda file to keep list-columns in tact
    load(file = "data/earwolf_podcasts.Rda")
    earwolf_podcasts <- bind_rows(earwolf_podcasts, new_data) %>%
      distinct(podcast, number, title, date, .keep_all = TRUE)
    save(earwolf_podcasts, file = "data/earwolf_podcasts.Rda")
    
    # Also write to CSV for compatibility
    earwolf_podcasts %>%
      rowwise() %>%
      mutate(guests = paste(guests, collapse = ", ")) %>% # Lists don't play well in CSVs
      write_csv("data/earwolf_podcasts.csv", na = "")
  })
}

# Update each of the podcasts in the start_url vector
walk_over_podcasts(starting_urls)

# Re-try any URLs that failed
if (length(failures) > 0) walk_over_podcasts(failures)