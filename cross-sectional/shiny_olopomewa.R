# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Olopomewa per EA"),
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
        data = df_ib_o,
        fillColor = "pink",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_o,
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
