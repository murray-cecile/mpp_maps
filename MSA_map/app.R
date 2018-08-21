
# 5. ( ) MSA bubble maps


# Author: Sifan Liu
# Date: Thu Jul 26 09:13:31 2018
# --------------


# Read Data ---------------------------------------------------------------


library('maps')
library('mapproj')
library('ggplot2')
library('scales')
library('ggthemes')
library('RColorBrewer')
library('fiftystater')
library('shiny')
library('colourpicker')
library('dplyr')
library('tigris')
library("shinythemes")

cbsa = core_based_statistical_areas()
cbsa_data = cbsa@data
HI_AK <- c("46520", "27980","11260", "21820")


# Shiny R -----------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("US MSA Mapping Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      helpText("If you have any questions or comments, please contact Sifan Liu (sliu@brookings.edu)"),
      
      fileInput('file1',"Choose CSV File",
                accept = c(".csv")),
      
      actionButton("choice", "Show data"),
      
      tags$hr(),
      
      selectInput("var", "Choose a variable for color", choices = NULL),
      selectInput("var2", "Choose a variable for size", choices = NULL),
      
      colourInput("low", "Choose a color for low value","#fee0d2"),
      colourInput("high", "Choose a color for high value", "#de2d26"),
      
      radioButtons("filetype", "File type:", choices = c("png", "pdf")),
      downloadButton("plot", label = "Download the plot")
    ),
    
    mainPanel(
      tableOutput("contents"),
      plotOutput("map"))
    
  )
)

# Server logic ----
server <- function(input, output,session) {
  
  info <- eventReactive(input$choice,{
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    vars <- names(df[-1])
    
    df$CBSAFP <- as.character(df$CBSA)
    df <- left_join(df, cbsa_data[c("CBSAFP", "INTPTLON", "INTPTLAT")], by = c('CBSAFP'))
    df$long = -as.numeric(substring(df$INTPTLON,2))
    df$lat = as.numeric(df$INTPTLAT)
    
    df <- df %>%
      mutate(long = case_when(
        CBSAFP == "46520" ~ -108.5,
        CBSAFP == "27980" ~ -104.5,
        CBSAFP == "11260" ~ -117.5,
        CBSAFP == "21820" ~ -117,
        TRUE ~ .$long),
        lat = case_when(
          CBSAFP == "46520" ~ 27,
          CBSAFP == "27980" ~ 24.5,
          CBSAFP == "11260" ~ 27,
          CBSAFP == "21820" ~ 27.5,
          TRUE ~ .$lat
        )
      )
    
    updateSelectInput(session,"var", "Choose a variable for color", choices = vars)
    updateSelectInput(session, "var2", "Choose a variable for size", choices = vars)
    
    df
  })
  
  output$contents <- renderTable({
    
    input_data <- info()
    names(input_data) <- gsub("\\."," ", names(input_data))
    input_data[-1] <- comma(input_data[-1],digits = 1)
    head(input_data)
    
  })
  
  plotInput <- function(){
    
    input_data <- info()
    
    map_wrapper <- ggplot() + 
      geom_map(data = fifty_states, map = fifty_states, aes(map_id = id),
               fill = "light grey", color = "white", size = 1) + 
      geom_point(data = input_data, 
                 aes_string(x = "long", y = "lat", color = input$var, size = input$var2)) +
      scale_size_continuous(range = c(1,10)) +
      coord_map("albers", lat0=39, lat1=45) +
      theme_map()
    
    if (is.discrete(input_data[[input$var]])){
      map_wrapper +
        scale_color_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(input_data[[input$var]]))),
                          labels = comma,
                          name = gsub("\\."," ",input$var))
    }
    else {
      map_wrapper +
        scale_color_continuous(labels = comma, name = gsub("\\."," ",input$var),
                              low = input$low, high = input$high, space = "Lab",guide = "colourbar")
    }
  }
  output$plot <- downloadHandler(
    filename = function(){
      paste("plot", input$filetype, sep = ".")
    },
    content = function(file){
      ggsave(file, plotInput(), device = input$filetype, width = 12, height = 6, bg = "transparent")
      
    }) 
  
  output$map <- renderPlot({
    print(plotInput())
  })
  
  
}

# Run app ----
shinyApp(ui, server)



