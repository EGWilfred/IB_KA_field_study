# Function to calculate distance between two coordinates using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Radius of the Earth in kilometers
  R <- 6371
  
  # Convert degrees to radians
  lat1 <- radians(lat1)
  lon1 <- radians(lon1)
  lat2 <- radians(lat2)
  lon2 <- radians(lon2)
  
  # Differences in coordinates
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}

# Helper function to convert degrees to radians
radians <- function(degrees) {
  return(degrees * pi / 180)
}

# Example coordinates (latitude and longitude in degrees)
lat1 <- 34.0522
lon1 <- -118.2437
lat2 <- 40.7128
lon2 <- -74.0060

# Calculate distance
distance <- haversine_distance(lat1, lon1, lat2, lon2)

# Print the distance
print(paste("Distance:", distance, "km"))


library(ggplot2)

# Sample data
df <- data.frame(
  x = 1:10,
  y1 = rnorm(10),
  y2 = rnorm(10)
)

# Create a ggplot with geom_line and add a legend
ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1, color = "Line 1"), size = 1.5) +
  geom_line(aes(y = y2, color = "Line 2"), size = 1.5) +
  scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue"), name = "Legend Title") +
  labs(title = "Two Lines Plot with Legend",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()
