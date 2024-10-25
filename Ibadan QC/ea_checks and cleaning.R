library(tidyverse)
library(readxl)
dry <- read.csv("/Users/Mac/Downloads/Gift Wilfred Enang/Household/UrbanMalariaHousehol_DATA_LABELS_2024-09-03_1705.csv")%>%
  filter(nchar(as.character(Serial.Number)) >= 5)

wet_dry_sn <- read_xls ("/Users/Mac/Downloads/Gift Wilfred Enang/Household/IB wet season households with serial nos for dry season.xls")


# Convert both columns to lowercase
#dry$Enumeration.Area.Cluster.Number <- tolower(dry$Enumeration.Area.Cluster.Number)

#remove white space and Convert both columns to lowercase
dry <- dry %>%
  mutate(
    Enumeration.Area.Cluster.Number = str_replace_all(str_to_lower(Enumeration.Area.Cluster.Number), "\\s+", ""),
    Settlement.Type = str_replace_all(str_to_lower(Settlement.Type), "\\s+", "")
  )

#Function to clean the ea column

clean_ea <- function(bi5_value) {
  # Step 1: Replace any non-alphabetic character between the letters and the first number with an underscore
  cleaned_value <- str_replace_all(bi5_value, "([a-zA-Z]+)[^0-9]+(\\d)", "\\1_\\2")
  
  # Step 2: Replace any character (other than slash) between the first set of numbers and the second set with a slash
  cleaned_value <- str_replace_all(cleaned_value, "(\\d{1,3})[^0-9/]+(\\d+)", "\\1/\\2")
  
  # Step 3: Add leading zeros to the first number (after the underscore) only if it has fewer than three digits
  cleaned_value <- str_replace_all(cleaned_value, "([a-zA-Z]+_)(\\d{1,3})(/|$)", function(x) {
    prefix <- str_extract(x, "^[a-zA-Z]+_")
    number <- str_extract(x, "\\d{1,3}")
    suffix <- str_extract(x, "/|$")
    # Only pad with leading zeros if the number is less than 3 digits
    if (nchar(number) < 3) {
      return(sprintf("%s%03d%s", prefix, as.numeric(number), suffix))
    } else {
      return(sprintf("%s%s%s", prefix, number, suffix))
    }
  })
  
  # Step 4: Remove leading zeros in the second number after the slash
  cleaned_value <- str_replace_all(cleaned_value, "/0*(\\d+)", "/\\1")
  
  return(cleaned_value)
}

df$Enumeration.Area.Cluster.Number <- sapply(dry$Enumeration.Area.Cluster.Number, clean_ea)
view(df)



# modify ward names in eas
df1 <- df %>%
  mutate(Enumeration.Area.Cluster.Number = case_when(
    grepl("^a", Enumeration.Area.Cluster.Number) ~ sub("^[^_]+", "agugu", Enumeration.Area.Cluster.Number),
    grepl("^c", Enumeration.Area.Cluster.Number) ~ sub("^[^_]+", "challenge", Enumeration.Area.Cluster.Number),
    grepl("^b", Enumeration.Area.Cluster.Number) ~ sub("^[^_]+", "bashorun", Enumeration.Area.Cluster.Number),
    grepl("^o", Enumeration.Area.Cluster.Number) ~ sub("^[^_]+", "olopomewa", Enumeration.Area.Cluster.Number),
    TRUE ~ Enumeration.Area.Cluster.Number
  ))%>%
  separate(Enumeration.Area.Cluster.Number, into = c("ea", "cluster"), sep = "/", remove = FALSE)%>%
  filter(Enumeration.Area.Cluster.Number != "")



#read EA
# Define a function to standardize column names
standardize_columns <- function(df) {
  colnames(df) <- tolower(colnames(df))  # Convert all column names to lowercase
  colnames(df) <- gsub("enumeration area", "enumeration_area", colnames(df))  # Standardize Enumeration Area
  colnames(df) <- gsub("settlement type", "settlement", colnames(df))  # Standardize Settlement Type
  return(df)
}
sheet_names <- excel_sheets("/Users/Mac/Downloads/Longitudinal/EA-cluster and settlement type.xlsx")

ea_se_combined <- lapply(sheet_names, function(sheet) {
  read_excel("/Users/Mac/Downloads/Longitudinal/EA-cluster and settlement type.xlsx", sheet = sheet) %>%
    standardize_columns()
}) %>%
  bind_rows() %>%
  mutate(
    enumeration_area = tolower(enumeration_area),  # Convert values in enumeration_area to lowercase
    settlement = tolower(settlement),  # Convert values in settlement to lowercase if needed
    enumeration_area = if_else(cluster == 5 & enumeration_area == "challenge_046", 
                               "challenge_050", 
                               enumeration_area),
    settlement = if_else(cluster == 5 & enumeration_area == "challenge_050", 
                         "informal", 
                         settlement)
  ) %>%
  distinct(enumeration_area, cluster, settlement, .keep_all = TRUE)



#eas not in ea listing
missing_enumeration_areas <- df1  %>%
  anti_join(ea_se_combined, by = c("ea" = "enumeration_area")) %>%
  distinct(ea)
view(missing_enumeration_areas)


#EAs in EA listing
correct_enumeration_areas <- df1  %>% 
  inner_join(ea_se_combined, by = c("ea" = "enumeration_area"))
view(correct_enumeration_areas)



#extract households with missing eas from the wet season
extract <- wet_dry_sn %>%
  mutate(`Dry serial no` = as.character(`Dry serial no`))%>%
  inner_join(missing_enumeration_areas, by = c("Dry serial no" = "Serial.Number"))%>%
  select(`Dry serial no`, `Name of Household Head`, Name.of.Household.Head,
         `Enumeration Area/Cluster Number`, Enumeration.Area.Cluster.Number, ea)

view(extract)


#wrong eas
wrong_eas <- missing_enumeration_areas  %>%
  anti_join(extract, by = c("Serial.Number" = "Dry serial no")) %>%
  select(Serial.Number, Enumeration.Area.Cluster.Number, Name.of.Household.Head, Ward)

unique(missing_enumeration_areas$Enumeration.Area.Cluster.Number)

view(wrong_eas)

write_csv(wrong_eas, "/Users/Mac/Downloads/Gift Wilfred Enang/Household/households in dry season with wrong EAs.csv")





#replaced households
replaced <- dry %>%
  anti_join(wet_dry_sn %>% mutate(`Dry serial no` = as.character(`Dry serial no`)), 
            by = c("Serial.Number" = "Dry serial no")) %>%
  filter(Repeat.Instrument == "") %>%
  select("Serial.Number", Name.of.Household.Head,
         Community.Name, Enumeration.Area.Cluster.Number, Ward)

write_csv(replaced, "/Users/Mac/Downloads/Gift Wilfred Enang/Household/replaced and new households.csv")

view(replaced)
















wet_hh <- read.csv("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/Household_information.csv")
setwd("/Users/Mac/Downloads/Gift Wilfred Enang/Household")
shapefile_folder <- "Ibadan_metro_ward_fiveLGAs"
shapefile <- st_read(dsn = shapefile_folder)

st_crs(shapefile) <- 4326
all<- sf::st_as_sf(ed, coords=c("bi7_long", "bi7_lat"), crs=4326)


centroids_Ibadan <- wet_hh %>% 
  dplyr::select(Longitude, Latitude, Ward,
                enumeration_area_cluster, settlement.y) %>% 
  dplyr::distinct()


split_data <- split(centroids_Ibadan, centroids_Ibadan$enumeration_area_cluster)

# functions 
deg2rad <- function(degrees) {
  return(degrees * pi / 180)
}
rad2deg <- function(radians) {
  return(radians * 180 / pi)
}



centroids <- list()

for (index in seq_along(split_data)){
  data <- split_data[[index]]
  # Convert degrees to radians
  data$radians_lat <- deg2rad(data$Latitude)
  data$radians_lon <- deg2rad(data$Longitude)
  # Convert to Cartesian coordinates
  data$x <- cos(data$radians_lat) * cos(data$radians_lon)
  data$y <- cos(data$radians_lat) * sin(data$radians_lon)
  data$z <- sin(data$radians_lat)
  # Calculate mean of Cartesian coordinates
  mean_x <- mean(data$x)
  mean_y <- mean(data$y)
  mean_z <- mean(data$z)
  # Convert back to latitude and longitude
  centroid_lon <- atan2(mean_y, mean_x)
  centroid_lat <- atan2(mean_z, sqrt(mean_x^2 + mean_y^2))
  # Convert radians back to degrees
  centroid_lon <- rad2deg(centroid_lon)
  centroid_lat <- rad2deg(centroid_lat)
  centroids_data <- data.frame(settlement_type_new = data$settlement.y[1],
                               Ward = data$Ward[1], 
                               ea_numbers_new = data$enumeration_area_cluster[1], 
                               longitude = centroid_lon, 
                               latitude = centroid_lat)
  
  centroids[[index]] <- centroids_data
}


final_centroid_data <- data.table::rbindlist(centroids)



all<- sf::st_as_sf(final_centroid_data, coords=c("longitude", "latitude"), crs=4326)%>%
  filter(settlement_type_new != "")


map_theme <- function() {
  theme_minimal() +  # Start with a minimal theme as the base
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.text = element_blank(),         # Remove axis text (x and y labels)
      axis.ticks = element_blank(),        # Remove axis ticks
      panel.background = element_rect(fill = "white", color = NA),  # White background
      legend.position = "right",           # Position legend on the right
      legend.title = element_blank(),      # Remove legend title
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center and bold the title
      plot.margin = margin(10, 10, 10, 10) # Set margin around the plot
    )
} 

ggplot(shapefile) +
  geom_sf(fill = NA) +  # Plot the boundaries
  geom_point(data = all,  # Plot all points
             aes(geometry = geometry, col = settlement_type_new), 
             stat = "sf_coordinates", size = 1) +  # Adjust the size here
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159")) +
  guides(size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Centroids of Enumeration Areas") +
  coord_sf()



# Step 1: Convert df1 (household data) and final_centroid_data into spatial objects (sf objects)
# Assuming df1 has 'Longitude' and 'Latitude' columns
df1_sf <- wrong_eas %>%
  filter(HOUSEHOLD.COORDINATE..Longitude != "", HOUSEHOLD.COORDINATE..Latitude != "")%>%
  st_as_sf(coords = c("HOUSEHOLD.COORDINATE..Longitude", "HOUSEHOLD.COORDINATE..Latitude"), crs = 4326)

# Assuming final_centroid_data has 'longitude' and 'latitude' columns for centroids
centroids_sf <- final_centroid_data %>%
  filter(!is.na(longitude), !is.na(latitude))%>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Step 2: Perform nearest neighbor join
households_with_nearest_centroid <- st_join(df1_sf, centroids_sf, join = st_nearest_feature)

# Step 3: Calculate distance between each household and its nearest centroid
households_with_nearest_centroid <- households_with_nearest_centroid %>%
  mutate(distance_to_centroid = st_distance(geometry, st_geometry(centroids_sf[st_nearest_feature(df1_sf, centroids_sf), ])))

# Step 4: Convert back to a regular data frame if needed (removing geometry)
households_with_nearest_centroid1 <- households_with_nearest_centroid %>%
  st_drop_geometry()%>%
  select(Serial.Number, ea_numbers_new, Ward.x)

write_csv(households_with_nearest_centroid1, "/Users/Mac/Downloads/Gift Wilfred Enang/new EAs.csv")

view(households_with_nearest_centroid1)

#number of EAs in each ward and settlement
sum_ward <- wet_hh %>%
  group_by(enumeration_area_cluster, Ward, settlement.y)%>%
  filter(enumeration_area_cluster != "")%>%
  summarise(number_of_households = n())
write_csv(sum_ward, "/Users/Mac/Downloads/Gift Wilfred Enang/Total households per EA_wet.csv")

view(sum_ward)



