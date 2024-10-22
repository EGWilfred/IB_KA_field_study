library(shiny)
library(shinyMobile)

# Define UI
ui <- shinyMobileUI(
  title = "My Shiny App",
  navbar = NULL,  # Remove the navbar for mobile app
  page = tabset(
    tabPanel("Tab 1",
             titlePanel("Households Listed in Agugu per EA"),
             leafletOutput("map", width = "100%", height = "1000px")
    ),
    tabPanel("Tab 2",
             # Place your UI elements here
    )
  )
)

# Define server
server <- function(input, output, session) {
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
 # Server logic goes here
}

# Run the Shiny app using shinyMobile
shinyMobile::runApp(
  appDir = getwd(),  # Path to your Shiny app directory
  display.mode = "showcase",
  server = function(input, output, session) {
    shinyMobileServer(server)
  }
)

Please note that converting a Shiny app into an Android app using shinyMobile may have certain limitations and may not support all features or dependencies used in your original Shiny app.

For more details and advanced customization options, refer to the official documentation and examples of the shinyMobile package.

Please let me know if you need any further assistance!
  
  
  
  
  
  
  Regenerate response
