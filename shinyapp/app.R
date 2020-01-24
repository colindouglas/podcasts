library(shiny)
library(tidyverse)

# Load, process data
load("../data/earwolf_podcasts.Rda")

guests <- earwolf_podcasts %>%
  unnest_longer(guests)

# Podcasts in order of release date
podcasts <- guests %>%
  filter(!is.na(guests)) %>%
  arrange(date) %>%
  pull(podcast) %>% 
  unique()

# Calculate the frequency of each guest
guest_freq <- guests %>%
  filter(!is.na(guests)) %>%
  count(guests) %>%
  arrange(-n) %>%
  mutate(color = rep_len(viridis::plasma(n = 20), length.out = n()),
         color = ifelse(n >= 5, color, "#696969"))

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
