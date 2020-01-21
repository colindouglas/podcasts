library(shiny)
library(tidyverse)


# Load, process data
load("../data/earwolf_podcasts.Rda")

# Podcasts in order of release date
podcasts <- earwolf_podcasts %>%
  group_by(podcast) %>%
  summarize(date = min(date)) %>%
  arrange(date) %>%
  pull(podcast)


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
  output$guest_plot <- renderPlot({earwolf_podcasts %>%
      filter(podcast == input$podcast) %>%
      unnest_longer(guests) %>%
      filter(!is.na(guests)) %>%
      count(guests) %>%
      arrange(-n) %>%
      head(20) %>%
      ggplot(aes(x = guests)) +
      geom_col(aes(y = n)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
}
shinyApp(ui, server)
