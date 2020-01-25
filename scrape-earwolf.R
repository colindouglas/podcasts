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
      html_text() %>%
      head(1)
    
    # If there's no guests, return NA instead of list(character(0))
    guest <- ifelse(length(guest) == 0, as.character(NA), guest)
    
    ### Get the episode photos if they exist
    photo_urls <- html_scrape %>%
      html_nodes(".epimgbox") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Cache the episode photos and get the exif data
    exif_dates <- map_chr(photo_urls, possibly(function(url) {
      
      # If it's the attachment_id format, ignore it, it frequently hangs
      if (grepl("attachment_id", url, ignore.case = TRUE)) return(NA) 
      
      # Make a folder for each podcast
      dir.create(paste0("data/photos/", podcast), showWarnings = FALSE, recursive = TRUE)
      filename <- tail(str_split(url, pattern = "/")[[1]], 1)
      
      # Name the file EP# - Original Filename
      destination <- paste0("data/photos/", podcast, "/", number, " - ", filename)
      tryCatch(expr = { download.file(url, destfile = destination) },
               error = function(x) { return(NA) },
               warning = function(x) { return(NA) }
      )
      
      exif_df <- exifr::read_exif(destination, tags = c("CreateDate"))
      
      if ("CreateDate" %in% colnames(exif_df)) {
        exif_df$CreateDate
      } else {
        as.character(NA)
      }
    }, otherwise = as.character(NA)))
    
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
      html_attr('href') %>%
      head(1)
    
    # Test for loops due to Earwolf's broken website
    # They don't work if the next_ep is character(0), so check for that first
    if (length(next_ep) != 0) {
      if (next_ep == "https://www.earwolf.com/episode/" | next_ep %in% episode$url | length(next_ep) > 1) {
        warning("Aborted from loop: ", next_ep)
        break
      } 
    }
  }
  return(episode)
}

# URLS to start off with, if the podcast has never been checked before
starting_current <- c(
  # Current Podcasts
  "https://www.earwolf.com/episode/icelandic-meat/", # Urgent Care
  "https://www.earwolf.com/episode/firsts-with-bill-hader/", # In Bed with Nick and Megan
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
  "https://www.earwolf.com/episode/citizen-kane/") # Unspooled
# Archived Podcasts

starting_archive <- c(
  "https://www.earwolf.com/episode/hand-putty/", # Affirmative Nation
  "https://www.earwolf.com/episode/phish-101/", # Analyze Phish
  "https://www.earwolf.com/episode/the-wit-and-wisdom-of-the-west-with-dalton-wilcox/", # ADPPP
  "https://www.earwolf.com/episode/larry-david/", # By the Way
  "https://www.earwolf.com/episode/1-goodfellas/", # The Canon
  "https://www.earwolf.com/episode/christopher-guest-2/", # Crybabies
  "https://www.earwolf.com/episode/0-getting-into-the-denzelishness/", # Denzel Washington
  "https://www.earwolf.com/episode/first-is-the-worst-w-chris-gethard-anna-drezen/", # Dr Gameshow
  "https://www.earwolf.com/episode/the-fogelnest-files-1/", # Fogelnest Files
  "https://www.earwolf.com/episode/163-w-mark-hoppus/", # Get Up On This
  "https://www.earwolf.com/episode/5-marty-allen/", # Gilbert Gottfried
  "https://www.earwolf.com/episode/will-forte/", # Happy Sad Confused
  "https://www.earwolf.com/episode/kattany-and-zooey/", # Hooray Show
  "https://www.earwolf.com/episode/there-will-be-blood-with-paul-f-tompkins/", # I Was There Too
  "https://www.earwolf.com/episode/episode-i/", # In Voorhees We Trust
  "https://www.earwolf.com/episode/episode-1-the-richardsonian-method-of-dream-analysis/", # In Your Dreams
  "https://www.earwolf.com/episode/conan-obrien-ben-sinclair-jordan-schlansky-jose-arroyo/", # Inside Conan
  "https://www.earwolf.com/episode/episode-001-dr-no-with-paul-f-tompkins/", # James Bonding
  "https://www.earwolf.com/episode/the-stump/", # John Levenstein
  "https://www.earwolf.com/episode/levar-burton/", # Kevin Pollak
  "https://www.earwolf.com/episode/a-new-day-in-brown-america/", # Kondabolu Brothers
  "https://www.earwolf.com/episode/18-robot-films/", # Malton on Movies
  "https://www.earwolf.com/episode/self-driving-me-to-divorce/", # My Dead Wife
  "https://www.earwolf.com/episode/let-the-game-begin/", # Nerd Poker
  "https://www.earwolf.com/episode/jon-hamm-2/", # Never Not Funny
  "https://www.earwolf.com/episode/nocturnal-emotions-1/", # Nocturnal Emotions
  "https://www.earwolf.com/episode/obscure-episode-1/", # Obscure
  "https://www.earwolf.com/episode/pistol-shrimps-4-21-15/", # Pistol Shrimps Radio
  "https://www.earwolf.com/episode/patrisse-khan-cullors-on-black-lives-matter-resistance-under-45/", # Politically Re-active
  "https://www.earwolf.com/episode/emma-willmann-liz-miele-devon-walker-and-hosts-cameron-esposito-and-rhea-butcher/", # Put Your Hands Together
  "https://www.earwolf.com/episode/welcome-to-qod-why-work-a-nine-to-six-job/", # Question of the Day
  "https://www.earwolf.com/episode/lets-get-going/", # Questions for Lennon
  "https://www.earwolf.com/episode/the-first-one/", # Raised by TV
  "https://www.earwolf.com/episode/julie-klausner/", # Ronna and Beverly
  "https://www.earwolf.com/episode/freelancer-or-entrepreneur/", # Startup School
  "https://www.earwolf.com/episode/welcome-to-sklarbro-country/", # Sklarbro Country
  "https://www.earwolf.com/episode/a-dennys-parking-lot/", # Spont
  "https://www.earwolf.com/episode/1-p-kat-w-paul-f-tompkins-and-katelyn-tarver/", # Supergroup
  "https://www.earwolf.com/episode/ep-1-home-is-where-the-wife-is/", # The Complete Woman
  "https://www.earwolf.com/episode/the-problem-with-louie-anderson/", # The Problem With
  "https://www.earwolf.com/episode/intro-topics/", # Topics
  "https://www.earwolf.com/episode/and-so-it-begins/", # Totally Laime
  "https://www.earwolf.com/episode/welcome-to-who-charted/", # Who Charted
  "https://www.earwolf.com/episode/public-domain-with-paul-f-tompkins/", # With Special Guest
  "https://www.earwolf.com/episode/who-makes-up-earwolf/", # The Wolf Den
  "https://www.earwolf.com/episode/jason-mantzoukas-seth-morris-spotlight-on-dr-lionel-drioche/" # WOMP It Up
)

starting_urls <- c(starting_current) #, starting_archive)

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