library(shiny)
library(tidyverse)

# Load, process data
load("./data/earwolf_podcasts.Rda")

guests <- earwolf_podcasts %>%
  unnest_longer(guests)

# Podcasts in order of release date
podcasts <- guests %>%
  group_by(podcast) %>%
  filter(sum(!is.na(guests)) > 50) %>% # Only podcasts with more than 50 guest credits
  arrange(date) %>%
  pull(podcast) %>% 
  unique()

# Calculate the frequency of each guest
guest_freq <- guests %>%
  filter(!is.na(guests)) %>%
  count(guests) %>%
  arrange(-n)

most <- log2(max(guest_freq$n))
least <- log2(min(guest_freq$n))

guest_freq <- guest_freq %>%
  mutate(color = viridis::viridis_pal(begin = 0, end = 0.9, option = "D", direction = -1)(20)[floor(log2(guest_freq$n)/most * 19) + 1],
         color = ifelse(n == 1, "#696969", color))

guest_colors <- guest_freq$color
names(guest_colors) <- guest_freq$guests



ui <- fluidPage(
  
  # App title ----
  titlePanel("Podcast Guests"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("podcast", "Podcast:", podcasts),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("guest_plot")
      
    )
  )
)

server <- function(input, output) {
  
  # Compute the formula text ----
  formulaText <- reactive({
    paste("Guest Appearances on ", input$podcast)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a bar plot
  output$guest_plot <- renderPlot({
    guests %>%
      filter(podcast == input$podcast) %>%
      filter(!is.na(guests)) %>%
      count(guests) %>%
      arrange(-n) %>%
      head(20) %>%
      mutate(guests = factor(guests, levels = guests)) %>%
      ggplot(aes(x = guests)) +
      geom_col(aes(y = n, fill = guests)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = guest_colors, guide = FALSE) +
      labs(x = "Podcast", y = "Count")
  })
  
  
}
shinyApp(ui, server)
