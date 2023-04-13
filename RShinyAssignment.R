library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

# Load the data
meteorite_landings <- read.csv("~/Downloads/Meteorite_Landings.csv")

# Define UI
ui <- fluidPage(
  
  # Set the app title
  titlePanel("Meteorite Landings"),
  
  # Create a sidebar with input options
  sidebarLayout(
    sidebarPanel(
      selectInput("type", label = "Meteorite Type",
                  choices = c("Fell", "Found", "Both"), selected = "Both"),
      numericInput("start_year", label = "Start Year", value = 1900, min = 860, max = 2013),
      numericInput("end_year", label = "End Year", value = 2013, min = 860, max = 2013),
      sliderInput("bins", label = "Number of bins",
                  min = 1, max = 50, value = 10)
    ),
    
    # Create a tabset panel with two tabs: Map and Histogram
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Histogram", plotlyOutput("histogram"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter the data based on user input
  filtered_data <- reactive({
    meteorite_landings %>% filter((year >= input$start_year & year <= input$end_year) & 
                                    (fall == input$type | input$type == "Both"))
  })
  
  
  # Create the Leaflet map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~reclong, 
        lat = ~reclat, 
        radius = 5, 
        stroke = FALSE,
        fillOpacity = 0.8,
        fillColor = cut(filtered_data()$mass_g, 
                        breaks = c(0, 500, 2000, Inf), 
                        labels = c("#FFEDA0", "#F03B20", "#BD0026"))
        ,
        popup = paste0("<strong>Name:</strong>", filtered_data()$name,
                       "<br><strong>Year:</strong>", filtered_data()$year,
                       "<br><strong>Mass:</strong>", filtered_data()$mass_g,
                       "<br><strong>Class:</strong>", filtered_data()$recclass)
      )
  })
  
  # Create the histogram
  output$histogram <- renderPlotly({
    ggplot(filtered_data(), aes(x = year)) + geom_histogram(bins = input$bins, fill = "steelblue",
                                                            color = "white") +
      labs(title = "Number of Meteorite Landings per Year",
           x = "Year", y = "Number of Meteorite Landings") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the app
shinyApp(ui, server)
