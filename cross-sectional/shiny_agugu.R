library(shiny)

##Overall plot

# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Agugu per EA"),
  leafletOutput("map", width = "100%", height = "1000px")
)

# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create the map
    leaflet() %>%
      addTiles() %>%
      # setView(data = hh_list_a,
      #   lng = Longitude, lat = Latitude, zoom = 80) %>%
      addPolygons(
        data = df_ib_a,
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_a,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~Cluster_Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


##Selecting Clusters to be plotted

hh_list_aa_20 <- hh_list_a %>% dplyr::filter(Cluster_Number == 20)


# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Agugu per Cluster"),
  leafletOutput("map", width = "100%", height = "1000px")
)

# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create the map
    leaflet() %>%
      addTiles() %>%
      # setView(data = hh_list_a,
      #   lng = Longitude, lat = Latitude, zoom = 80) %>%
      addPolygons(
        data = df_ib_a,
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_aa_20,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~X_001_Serial_Number_of_Structure,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


# Load necessary libraries (if not already loaded)
if (!require(leaflet)) install.packages("leaflet")
if (!require(shiny)) install.packages("shiny")
library(leaflet)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Agugu per EA"),

  # Add the numeric input widget
  numericInput(
    inputId = "cluster_input",   # Unique ID for the numeric input
    label = "Enter Cluster Number:",   # Label for the numeric input
    min = 1,   # Minimum value for the numeric input
    max = 37,   # Maximum value for the numeric input
    value = 37   # Default value for the numeric input
  ),
  
  # Add the slider input widget
  sliderInput(
    inputId = "cluster_slider",   # Unique ID for the slider
    label = "Select Cluster Number:",   # Label for the slider
    min = 1,   # Minimum value for the slider
    max = 37,   # Maximum value for the slider
    value = 37   # Default value for the slider
  ),
  
  leafletOutput("map", width = "100%", height = "1000px")
)

# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- hh_list_a[hh_list_a$Cluster_Number <= input$cluster_slider, ]
    
    # Create the map
    leaflet() %>%
      addTiles() %>%
      # setView(data = hh_list_a,
      #   lng = Longitude, lat = Latitude, zoom = 80) %>%
      addPolygons(
        data = df_ib_a,
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = filtered_data,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~Cluster_Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)


# Load necessary libraries (if not already loaded)
if (!require(leaflet)) install.packages("leaflet")
if (!require(shiny)) install.packages("shiny")
library(leaflet)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Agugu per EA"),
  
  # Add the numeric input widget
  numericInput(
    inputId = "cluster_input",   # Unique ID for the numeric input
    label = "Enter Cluster Number:",   # Label for the numeric input
    min = 1,   # Minimum value for the numeric input
    max = 37,   # Maximum value for the numeric input
    value = 37   # Default value for the numeric input
  ),
  # Add the numeric input widget for Zoom Level
  numericInput(
    inputId = "zoom_input",   # Unique ID for the numeric input
    label = "Enter Zoom Level:",   # Label for the numeric input
    min = 1,   # Minimum value for the numeric input
    max = 18,  # Maximum value for the numeric input (adjust as needed)
    value = 10  # Default value for the numeric input
  ),
  
  leafletOutput("map", width = "100%", height = "1000px")
)

# Define server
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- hh_list_a[hh_list_a$Cluster_Number == input$cluster_input, ]
    
    # Create the map
    leaflet() %>%
      addTiles() %>%
      # setView(data = hh_list_a,
      #   lng = Longitude, lat = Latitude, zoom = 80) %>%
      addPolygons(
        data = df_ib_a,
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = filtered_data,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~Cluster_Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)

