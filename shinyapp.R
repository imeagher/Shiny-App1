# Load necessary libraries
library(shiny)
library(bslib)  # Ensure bslib is loaded
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(memoise)
library(dplyr)
library(leaflet)

# Load real estate dataset
project1_data <- read.csv("/Users/isabella/Desktop/Chapman/Xtra Semester/MGSC 410/midtermdata.csv")

# Define the UI
ui <- fluidPage(  
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    primary = "#ff66b2",
    secondary = "#d9a1d1",
    success = "#f4c2c2",
    danger = "#ff4d94"
  ),
  
  # Custom CSS for consistent styling with the pink theme
  tags$head(
    tags$style(HTML("
    body { font-family: 'Roboto', sans-serif; background-color: #f9f9f9; }
    .btn, .form-control {
      border-radius: 4px;
      background-color: #ff66b2;
      color: white;
      border: 1px solid #ff66b2;
    }
    .btn:hover, .form-control:focus {
      background-color: #ff4d94;
      border-color: #ff4d94;
    }
    .sidebar {
      background-color: #f4c2c2;
      color: #333;
    }
    .metric-box {
      padding: 20px;
      border-radius: 12px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      margin: 10px;
      background: #ffffff;
      border: 1px solid #f4c2c2;
      position: relative;
    }
    .metric-title {
      color: black;
      font-size: 14px;
      margin-bottom: 8px;
    }
    .value {
      font-size: 28px;
      font-weight: bold;
      display: flex;
      align-items: center;
      gap: 8px;
      color: black;
    }
  "))
  ),
  
  # Title of the app
  titlePanel("Project 1 Deployment - LotwiZe Case Study"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("bedrooms_select", "Select Number of Bedrooms:",
                  choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
                              "12", "14", "32"),
                  selected = "1"),
      helpText("Choose a bedroom amount to analyze."),
      sliderInput("lotSize_range", "Select Lot Size (in square feet):",
                  min = 87, max = 363421080, value = c(87, 363421080), step = 500),
      helpText("Adjust the lot size range for property analysis."),
      downloadButton("download_data", "Download Home Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h4("Home Summary"),
                 fluidRow(
                   column(4, div(class = "metric-box", h4(class = "metric-title", "Price"), uiOutput("price"))),
                   column(4, div(class = "metric-box", h4(class = "metric-title", "Matching Homes"), uiOutput("matchingHomes"))),
                   column(4, div(class = "metric-box", h4(class = "metric-title", "State"), uiOutput("state")))
                 ),
                 h4("Distribution of Homes Based on Selected Criteria"),
                 plotOutput("histogram_plot")
        ),
        tabPanel("Details",
                 h4("Detailed Home Information"),
                 # Metric box for average price
                 fluidRow(
                   column(4,
                          div(class = "metric-box",
                              h4(class = "metric-title", "Average Price"),
                              uiOutput("average_price_details")
                          )
                   )
                 ),
                 # Interactive map for location of homes
                 h4("Interactive Map of Homes"),
                 leafletOutput("map")
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected bedrooms, lot size, and zip code
  filtered_home_data <- reactive({
    data <- project1_data %>%
      filter(bedrooms %in% as.numeric(input$bedrooms_select) &
               lotSize >= input$lotSize_range[1] & lotSize <= input$lotSize_range[2])
    
    # Filter based on the selected zip code if it's chosen
    if (!is.null(input$zipcode_select) && input$zipcode_select != "") {
      data <- data %>% filter(zipcode == input$zipcode_select)
    }
    
    data
  })
  
  # Populate the zipcode dropdown dynamically based on filtered data
  observe({
    zipcodes <- unique(filtered_home_data()$zipcode)
    updateSelectInput(session, "zipcode_select", choices = zipcodes)
  })
  
  # Render price output
  output$price <- renderUI({
    data <- filtered_home_data()
    avg_price <- data %>% summarise(avg_price = mean(price, na.rm = TRUE)) %>% pull(avg_price)
    HTML(paste("<span style='font-size: 24px; color: #e75480; font-weight: bold;'>$",
               formatC(avg_price, format = "f", big.mark = ",", digits = 0), "</span>"))
  })
  
  # Render matching homes count
  output$matchingHomes <- renderUI({
    num_homes <- nrow(filtered_home_data())
    HTML(paste("<span style='font-size: 24px; color: #e75480; font-weight: bold;'>", num_homes, "</span>"))
  })
  
  # Render state output
  output$state <- renderUI({
    states <- filtered_home_data() %>% summarise(states = paste(unique(state), collapse = ", ")) %>% pull(states)
    HTML(paste("<span style='font-size: 24px; color: #e75480; font-weight: bold;'>", states, "</span>"))
  })
  
  # Render histogram
  output$histogram_plot <- renderPlot({
    data <- filtered_home_data()
    ggplot(data, aes(x = price)) +
      geom_histogram(binwidth = 50000, fill = "#ff66b2", color = "#e75480", alpha = 0.7) +
      labs(title = "Distribution of Home Prices", x = "Price ($)", y = "Number of Homes") +
      theme_minimal()
  })
  
  # Render the average price for the "Details" tab
  output$average_price_details <- renderUI({
    data <- filtered_home_data()
    
    if (nrow(data) == 0) {
      return(HTML("<span style='font-size: 24px; color: #e75480; font-weight: bold;'>No data available</span>"))
    }
    
    avg_price <- data %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      pull(avg_price)
    
    HTML(paste("<span style='font-size: 24px; color: #e75480; font-weight: bold;'>$",
               formatC(avg_price, format = "f", big.mark = ",", digits = 0), "</span>"))
  })
  
  # Render the interactive map
  output$map <- renderLeaflet({
    data <- filtered_home_data()
    
    # Ensure we have latitude and longitude columns in your dataset
    if ("latitude" %in% colnames(data) && "longitude" %in% colnames(data)) {
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                         radius = 6, color = '#ff66b2', fillOpacity = 0.7,
                         popup = ~paste("Price: $", price, "<br>Zipcode: ", zipcode))
    } else {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = -118.25, lat = 34.05, popup = "No location data available.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
