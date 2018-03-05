
# README ------------------------------------------------------------------

# Sifan Liu

# KEY FUNCTIONS to be developed

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# 1. ( ) File upload: https://shiny.rstudio.com/gallery/file-upload.html
# 2. (/) Choose the variable to map for state color: https://github.com/daattali/colourpicker
# 3. (/) Choose from the color template: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# 4. ( ) Save the plot to local

# Read Data ---------------------------------------------------------------


library('dplyr')
library('maps')
library('mapproj')
library('ggplot2')
library('scales')
library('ggthemes')
library('RColorBrewer')
library('plotly')
library('fiftystater')

states <- map_data("state")
subsidy.summary <- read.csv("summary.csv")
subsidy.summary$ID <- tolower(subsidy.summary$State)



# Shiny R -----------------------------------------------------------------

library(shiny)
library(colourpicker)


ui <- fluidPage(
  titlePanel("Subsidy Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create state maps with 
               information from GJF Subsidy Tracker"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Share of foreign investment", 
                              "Share of foreign mega deals"),
                  selected = "Share of foreign investment"),
    
      colourInput("low", "Choose a color for low value","#deebf7"),
      colourInput("high", "Choose a color for high value", "#08519c")
    ),
    mainPanel(plotOutput("map"))
  )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    
    data <- switch(input$var, 
                   "Share of foreign investment" = subsidy.summary$foreign.share, 
                   "Share of foreign mega deals" = subsidy.summary$mega.foreign.share)
    ggplot() + 
      geom_map(data = states, map = fifty_states, aes(x = long, y = lat, map_id = region),fill = "white", color = "grey") +
      geom_map(data = subsidy.summary, map = fifty_states, aes(fill = data, map_id = ID)) +
      scale_fill_gradient(low = input$low, high = input$high) + 
      labs(x=NULL, y=NULL) + 
      coord_map("albers", lat0 = 39, lat1 = 45) + 
      theme(panel.border = element_blank()) + 
      theme(panel.background = element_blank()) + 
      theme(axis.ticks = element_blank()) + 
      theme(axis.text = element_blank())
    
  })
}

# Run app ----
shinyApp(ui, server)



