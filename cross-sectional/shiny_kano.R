rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
NuCDir <- file.path(Drive, "my_stuff")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')

#=======
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  NuDPDir <- file.path(Drive, "Desktop")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "EA_data")
  EpiDir <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "Shiny data")
  HFDir <- file.path(EpiDir, "Health_Facility")
  HHDir <- file.path(EpiDir, "Household")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}


library(shiny)
library(leaflet)
library(rgdal)
library(rsconnect)

##Household Listed Plots

hh_list_k <- read.csv(file.path(NuDPDir, "kn_hh_list_0109.csv"))

hh_list_k <- read.csv(file.path(HHDir, "kn_hh_list_0109.csv"))


# Extract the 'x' and 'y' columns

hh_list_x <- hh_list_k[, c("Ward", "EA.Serial.Number", "X_Enter.GPS.Location_latitude",
                           "X_Enter.GPS.Location_longitude", "X001..Serial.Number.of.Structure")]

##Extraction of individual sheets and shapefile
names(hh_list_x)[names(hh_list_x) == "X_Enter.GPS.Location_latitude"] <- "Latitude"
names(hh_list_x)[names(hh_list_x) == "X_Enter.GPS.Location_longitude"] <- "Longitude"


##Faggae Shiny

hh_list_f <- hh_list_x%>% dplyr::filter(Ward == "Fagge")


# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Fagge per EA"),
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
        data = df_kn_f,
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_f,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~EA.Serial.Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

##Gobirawa Shiny

hh_list_g <- hh_list_x%>% dplyr::filter(Ward == "Gobirawa")


# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Gobirawa per EA"),
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
        data = df_kn_g,
        fillColor = "light green",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_g,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "red",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~EA.Serial.Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


##Dorayi Shiny

hh_list_d <- hh_list_x%>% dplyr::filter(Ward == "Dorayi")


# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Dorayi per EA"),
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
        data = df_kn_d,
        fillColor = "tomato",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_d,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~EA.Serial.Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

##Zango Shiny

hh_list_z <- hh_list_x%>% dplyr::filter(Ward == "Zango")


# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Zango per EA"),
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
        data = df_kn_z,
        fillColor = "plum",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_z,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~EA.Serial.Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


##Ginginyu Shiny

hh_list_gi <- hh_list_x%>% dplyr::filter(Ward == "Giginyu")
View(hh_list_gi)

# Define UI
ui <- fluidPage(
  titlePanel("Households Listed in Gingiyu per EA"),
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
        data = df_kn_gi,
        fillColor = "plum",
        fillOpacity = 0.4,
        color = "black",
        weight = 2
      ) %>%
      addCircleMarkers(
        data = hh_list_gi,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~EA.Serial.Number,
        labelOptions = labelOptions(noHide = TRUE)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
