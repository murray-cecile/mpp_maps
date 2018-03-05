
# README ------------------------------------------------------------------

# Sifan Liu

# KEY FUNCTIONS to be developed

# 1. ( ) File upload: https://shiny.rstudio.com/gallery/file-upload.html
# 2. ( ) Choose the variable to map for bubble size
# 3. (/) Choose the variable to map for bubble color
# 4. ( ) Adjust the size 
# 5. ( ) Choose from the color template: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2



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
subsidy.summary <- read.csv("V:/Sifan/Subsidy-Tracker/summary.csv")
#subsidy.summary <- read.csv('../summary.csv')
subsidy.summary$ID <- tolower(subsidy.summary$State)




# Shiny R -----------------------------------------------------------------

library(shiny)


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
                  selected = "Share of foreign investment")
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
      geom_map(data = states, map = fifty_states, aes(x = long, y = lat, map_id = region),fill = "white", color = "black") +
      geom_map(data = subsidy.summary, map = fifty_states, aes(fill = data, map_id = ID)) +
      #  scale_fill_continuous(name = var,low='#deebf7', high='#08306b', guide='colorbar') + 
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



