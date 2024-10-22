
library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(haven)
library(tidyverse)
library(plotly)


hh <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/1. KN Wet season household data_edited.dta')
net <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/5. KN Wet season household net inspection.dta')
result <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/KN wet season household members RDT results.dta')
visitors <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/2. KN Wet season household visitors.dta')
visitorsmalaria <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/4. KN Wet season household visitors malaria hx.dta')
travellers <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Household questionnaire/3. KN Wet season household travellers.dta')

left_join(hh,net, by="sn")
left_join(hh,result,visitors, by ="sn")
left_join(hh,visitorsmalaria,travellers, by="sn")


hhdata_ <- hh %>%
  left_join(net, by = "sn") %>%
  left_join(result, by = "sn") %>%
  left_join(visitors, by = "sn") %>%
  left_join(visitorsmalaria, by = "sn")%>%
  left_join(travellers, by = "sn")

summarynet <- net %>%
  group_by(sn) %>%
  summarise(total =n())

summaryresult <- result %>%
  group_by(sn) %>%
  summarise(total =n())

summaryvisitors <- visitors %>%
  group_by(sn) %>%
  summarise(total =n())

summaryvisitorsmalaria <- visitorsmalaria %>%
  group_by(sn) %>%
  summarise(total =n())



mendata <- read_dta('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/KN Wet season men survey_edited.dta')
womendata <- read_csv('/Users/user/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano women questionnaire/kn_merged_women_data.csv')

left_join(hh,mendata,womendata, by="sn")

missinghhlatlong <- hhdata %>%
  filter(is.na(bi7_lat) | bi7_lat == "" | is.na(bi7_long) | bi7_long == "")

write_csv(missinghhlatlong,"Missing GPS Wet season.csv")
theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}


summary_missinglatlog <- missinghhlatlong %>%
  group_by( bi2)%>%
  summarise(
    total_issues = n()
  )
summary_missinglatlog$bi2 <- as.character(summary_missinglatlog$bi2)
summary_missinglatlog <- summary_missinglatlog %>%
  mutate(bi2 = recode(bi2, 
                      "1" = "Zango", 
                      "2" = "Dorayi", 
                      "3" = "Giginyu", 
                      "4" = "Gobirawa",
                      "5" = "Fagge D2",
                      "6" = "Giginyu"))




######EA Cluster numbers
summary_EA_cluster <- hhdata %>%
  group_by(bi2)%>%
  summarise(
    total_counts = n()
  )

write.csv(summary_EA_cluster,"summary_EA_cluster_initial.csv")

summary_EA_cluster$bi2 <- as.character(summary_EA_cluster$bi2)

summary_EA_cluster2 <- summary_EA_cluster %>%
mutate(bi2 = recode(bi2, 
                    "1" = "Zango", 
                    "2" = "Dorayi", 
                    "3" = "Giginyu", 
                    "4" = "Gobirawa",
                    "5" = "Fagge D2",
                    "6" = "Giginyu"),  bi5 = str_to_upper(str_replace_all(bi5, "\\s+", "")))

summary_EA_cluster2 <- summary_EA_cluster2 %>%
  group_by(bi2, bi3, bi5)%>%
  summarise(
    total_counts = n()
  )

write.csv(summary_EA_cluster2,"summary_EA_cluster_after removing white space.csv")



# Ensure text/number separation
summary_EA_cluster3 <- summary_EA_cluster2 %>%
  mutate(bi5 = if_else(str_detect(bi5, "^\\d+$"), paste0("/", bi5), bi5), 
         bi5 = if_else(str_detect(bi5, "^\\d+/\\d+$"), str_replace(bi5, "/", ""), bi5),
         bi5 = if_else(str_detect(bi5, "/"), bi5, str_replace(bi5, "(\\D+)(\\d{2,})", "\\1/\\2")))



# Separate text and numbers into two columns
summary_EA_cluster3 <- summary_EA_cluster3 %>%
  separate(bi5, into = c("EA_Name", "cluster_number"), sep = "/", fill = "right") %>%
  mutate(cluster_number = as.numeric(cluster_number),
         text = if_else(is.na(EA_Name), "", EA_Name))

summary_EA_cluster3 <- summary_EA_cluster3 %>%
  mutate(bi3 = recode(as.character(bi3), 
                      "1" = "Formal", 
                      "2" = "Informal", 
                      "3" = "Slum"))

write.csv(summary_EA_cluster3,"summary_EA_cluster_after separating the EA Name_clusterno.csv")


summary_EA_cluster3_ <- summary_EA_cluster3 %>%
  group_by(bi2, cluster_number, bi3, EA_Name )%>%
  summarise(
    total_counts = n()
  )
write.csv(summary_EA_cluster3_,"summary_EA_cluster_after Grouped by clusterno.csv")



summary_clusterNum <- summary_EA_cluster3 %>%
  group_by(bi2, cluster_number, bi3)%>%
  summarise(
    total_counts = n()
  )
write.csv(summary_clusterNum,"summary_EA_cluster_after Grouped by clusterno.csv")



ggplot(summary_missinglatlog, aes(reorder(bi2,-total_issues), total_issues ), colour="brown", fill="khaki") +
  geom_bar(stat = "identity", fill="khaki") +
  geom_point(aes(size=total_issues), colour="brown")+
 scale_size_continuous(range = c(5, 10), guide = FALSE)  +
  geom_text(size=3,color="white", aes(label = total_issues))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Quality Check Update",
       subtitle = "Missing GPS Values",
       caption = "Data source : Wet Season Cross Sectional Survey, Kano")



missinghhdata <- hhdata %>%
  filter_all(any_vars(is.na(.) | (is_empty(.))))

na_counts_dplyr <- hh %>% summarise_all(~ sum(is.na(.)))
print(na_counts_dplyr)

# Display variable labels
var_labels <- sapply(hh, attr, "label")
print(var_labels)

# Display value labels
value_labels <- sapply(hh, function(x) if (is.labelled(x)) val_labels(x) else NULL)
print(value_labels)

rbind(var_labels, valuelabels, na_counts_dplyr)

library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)
library(ggplot2)
library(ggmap)
library(plotly)
library(labelled)

#dframe<-read_csv('/Users/user/Downloads/UrbanMalariaHousehol_DATA_LABELS_2024-05-28_1844.csv')%>%
 # select(`Serial Number`, `INTERVIEWER'S NAME...15` , `Settlement Type` ,Ward,`HOUSEHOLD COORDINATE- Latitude`,`HOUSEHOLD COORDINATE- Longitude` )
#View(dframe)

sampled <- read_xlsx("/Users/user/Downloads/KN Sampled HHs_2024.xlsx")
dframe <- sampled



dframe <- hh

dframe1<-read_csv('/Users/user/Downloads/UrbanMalariaHousehol-DataUpdate2_DATA_LABELS_2024-01-14_1648_Wet_season.csv')%>%
 select(`Serial Number`, `INTERVIEWER'S NAME` , `Settlement Type` ,Ward,`HOUSEHOLD COORDINATE- Latitude`,`HOUSEHOLD COORDINATE- Longitude` )
View(dframe1)


shape <- st_read(dsn = "/Users/user/Downloads/Kano_metro_ward_sixLGAs/", layer = "Kano_metro_ward_sixLGAs")
View(shape)


shapenigeria <- st_read(dsn = "/Users/user/Downloads/gadm36_NGA_shp/", layer = "gadm36_NGA_0")
View(shapenigeria)

shapestates <- st_read(dsn = "/Users/user/Downloads/gadm36_NGA_shp/", layer = "gadm36_NGA_1")
View(shapestates)

shapeslga <- st_read(dsn = "/Users/user/Downloads/gadm36_NGA_shp/", layer = "gadm36_NGA_2")
View(shapeslga)



#Sampled HH rename of Lat and Long
names(dframe)[names(dframe) == "_entergpslocation_latitude"] <- "Latitude"
names(dframe)[names(dframe) == "_entergpslocation_longitude"] <- "Longitude"
	

#Dry season rename of Lat and Long
names(dframe)[names(dframe) == "$bi7_lat"] <- "Latitude"
names(dframe)[names(dframe) == "$bi7_long"] <- "Longitude"


#Wet season rename of Lat and Long
names(dframe1)[names(dframe1) == "HOUSEHOLD COORDINATE- Latitude"] <- "Latitude"
names(dframe1)[names(dframe1) == "HOUSEHOLD COORDINATE- Longitude"] <- "Longitude"


dframe <- dframe%>% dplyr::filter((`bi3`==1 | `bi3`==2 | `bi3`==3),(!is.na(bi7_lat) & !is.na(bi7_long)))
#dframe1<- na.omit(dframe[c("Longitude", "Latitude")])


#Left joining the positive count with map polygon

#shape=left_join(dframe,shape,by=c("Ward"="WardName"))

#define the map theme function
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        panel.grid =  ggplot2::element_blank(),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

#final_centroid_data<-read_csv('/Users/user/Downloads/kano_final_centroids_data.csv')


dff <- final_centroid_data%>%
  filter(!is.na(longitude) & !is.na(latitude))
#remove other wards from the shape file
shp <- shape
#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("longitude", "latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)



#Sampled


dff <- dframe%>%
  filter(!is.na(Longitude) & !is.na(Latitude))
dff$Longitude <- as.numeric(dff$Longitude)
dff$Latitude <- as.numeric(dff$Latitude)

dff <- dff%>%
  filter(!is.na(Longitude) & !is.na(Latitude))
#remove other wards from the shape file
shp <- shape
#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("Longitude", "Latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)

distances <- st_distance(gps, all)



zangoshp <- shp %>%
  filter(WardName =="Zango")

zangodt <- gps %>%
  filter(ward =="Zango")

st_crs(zangodt) <- 4326
st_crs(zangoshp) <- 4326

distances <- st_distance(zangodt, zangoshp)

# Find the minimum distance for each point
min_distances <- apply(distances, 1, min)

# Add the distances as a new column in the gps data frame
zangodt$distance_to_ward <- min_distances


# Summarize the distances
distance_summary <- summary(zangodt$distance_to_ward)

# Extract the required statistics
min_distance <- distance_summary["Min."]
median_distance <- distance_summary["Median"]
mean_distance <- distance_summary["Mean"]
max_distance <- distance_summary["Max."]

# Print the summary statistics
cat("Summary of Distances to Ward Boundaries (in meters):\n")
cat("Minimum Distance:", min_distance, "meters \n")
cat("Median Distance:", median_distance, "meters \n")
cat("Mean Distance:", mean_distance, "meters \n")
cat("Maximum Distance:", max_distance, "meters\n")



# Create a boxplot of the distances
ggplot(zangodt, aes(x = "", y = distance_to_ward)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Distances to Ward Boundaries",
    y = "Distance to Ward",
    x = ""
  ) +
  theme_manuscript()










giginyushp <- shp %>%
  filter(WardName =="Giginyu")

giginyudt <- gps %>%
  filter(ward =="Giginyu")

st_crs(giginyudt) <- 4326
st_crs(giginyushp) <- 4326

distances <- st_distance(giginyudt, giginyushp)

# Find the minimum distance for each point
min_distances <- apply(distances, 1, min)

# Add the distances as a new column in the gps data frame
giginyudt$distance_to_ward <- min_distances


# Summarize the distances
distance_summary <- summary(giginyudt$distance_to_ward)

# Extract the required statistics
min_distance <- distance_summary["Min."]
median_distance <- distance_summary["Median"]
mean_distance <- distance_summary["Mean"]
max_distance <- distance_summary["Max."]

# Print the summary statistics
cat("Summary of Distances to Ward Boundaries (in meters):\n")
cat("Minimum Distance:", min_distance, "meters \n")
cat("Median Distance:", median_distance, "meters \n")
cat("Mean Distance:", mean_distance, "meters \n")
cat("Maximum Distance:", max_distance, "meters\n")



# Create a boxplot of the distances
ggplot(giginyudt, aes(x = "", y = distance_to_ward)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Distances to Giginyu Ward Boundary",
    y = "Distance to Ward",
    x = ""
  ) +
  theme_manuscript()





dorayishp <- shp %>%
  filter(WardName =="Dorayi")

dorayidt <- gps %>%
  filter(ward =="Dorayi")

st_crs(dorayidt) <- 4326
st_crs(dorayishp) <- 4326

distances <- st_distance(dorayidt, dorayishp)

# Find the minimum distance for each point
min_distances <- apply(distances, 1, min)

# Add the distances as a new column in the gps data frame
dorayidt$distance_to_ward <- min_distances


# Summarize the distances
distance_summary <- summary(dorayidt$distance_to_ward)

# Extract the required statistics
min_distance <- distance_summary["Min."]
median_distance <- distance_summary["Median"]
mean_distance <- distance_summary["Mean"]
max_distance <- distance_summary["Max."]

# Print the summary statistics
cat("Summary of Distances to Ward Boundaries (in meters):\n")
cat("Minimum Distance:", min_distance, "meters \n")
cat("Median Distance:", median_distance, "meters \n")
cat("Mean Distance:", mean_distance, "meters \n")
cat("Maximum Distance:", max_distance, "meters\n")



# Create a boxplot of the distances
ggplot(dorayidt, aes(x = "", y = distance_to_ward)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Distances to Dorayi Ward Boundary",
    y = "Distance to Ward",
    x = ""
  ) +
  theme_manuscript()





dff <- dframe%>%
  filter(!is.na(bi7_long) & !is.na(bi7_lat))
dff$bi7_long <- as.numeric(dff$bi7_long)
dff$bi7_lat <- as.numeric(dff$bi7_lat)

dff <- dff%>%
  filter(!is.na(bi7_long) & !is.na(bi7_lat))
#remove other wards from the shape file
shp <- shape
#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("bi7_long", "bi7_lat"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)


invalid_gps <- dframe %>%
  filter(!`_index` %in% intersects_a1$`X_index`)

write_csv(invalid_gps,"Invalid GPS Captured in wet season")



invalid_gps__ <- intersects_a1 %>%
  filter(!(WardName=="Giginyu" | WardName=="Dorayi" | WardName=="Fagge D2" | WardName=="Gobirawa" | WardName=="Zango"))

write_csv(invalid_gps__,"Invalid GPS Captured Wet season.csv")




points <- intersects_a1$geometry
wards <- all %>%
  filter(WardName == "Zango")
# Calculate distances

distances <- st_distance(points, wards)

# Find the minimum distance for each point
min_distances <- apply(distances, 1, min)

# Add the distances as a new column in the points data frame
points$distance_to_ward <- min_distances

# View the points data frame with the new distances column
print(points)



invalid_gps_ <- invalid_gps__ %>%
  group_by(WardName) %>%
  summarise(
    total_issues = n()
  )%>%
  select(WardName, total_issues )

ggplot(invalid_gps_, aes(reorder(WardName,-total_issues), total_issues ), colour="brown", fill="khaki") +
  geom_bar(stat = "identity", colour="brown", fill="tomato") +
  geom_point(aes(size=total_issues), color="brown" , colour="khaki")+
  scale_size_continuous(range = c(5, 10), guide = FALSE)  +
  geom_text(size=3,color="white",aes(label = paste0(total_issues)))+
  
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Quality Check Update",
       subtitle = "GPS Position of households surveyed outside of Kano Metropolis",
       caption = "Data source : Dry Season Cross Sectional Survey, Kano",
  )






serials_ <- invalid_gps %>%
  group_by(ward) %>%
  summarise(
    total_issues = n()
  )%>%
  select(ward, total_issues )

serials_ <- serials_ %>%
  mutate(ward = recode(ward, 
                      "1" = "Zango", 
                      "2" = "Dorayi", 
                      "3" = "Giginyu", 
                      "4" = "Gobirawa",
                      "5" = "Fagge D2",
                      "6" = "Giginyu"))


ggplot(serials_, aes(reorder(ward,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(size=7)+
  geom_text(size=3,colour="white",aes(label =total_issues))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Cross Sectional Survey QA Update",
       subtitle = "GPS Position of households surveyed outside of Kano Metropolis/Invalid GPS by RAs",
       caption = "Data source : Cross Sectional Survey, Kano",
  )






all <- shp %>%
  filter(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa"  )

zangodf <- intersects_a1 %>%
  filter(Ward=="Zango")

zango <- all %>%
  filter(WardName=="Zango")


Giginyudf <- intersects_a1 %>%
  filter(Ward=="Others")

Giginyu <- all %>%
  filter(WardName=="Giginyu")

Faggedf <- intersects_a1 %>%
  filter(Ward=="Fagge D2")

Fagge <- all %>%
  filter(WardName=="Fagge D2")

Gobirawadf <- intersects_a1 %>%
  filter(Ward=="Gobirawa")

Gobirawa <- all %>%
  filter(WardName=="Gobirawa")


Dorayidf <- intersects_a1 %>%
  filter(Ward=="Dorayi")

Dorayi <- all %>%
  filter(WardName=="Dorayi")

intersects_a1 <- intersects_a1 %>%
  mutate(bi3 = recode(bi3, 
                       "1" = "Formal", 
                       "2" = "Informal", 
                       "3" = "Slum"
                      ))



#plot HH Listing
ggplot(all)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, colour=settlement), size=0.2, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
  theme_minimal() +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.line = element_blank(),   # Remove axis lines
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()
  ) +  # Remove panel background
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey Geo-points of Households Sampled",
       subtitle = "GPS Position of HHs Sampled from HH Listing in Kano by Settlement type",
       caption = "Data source : HH Sampling Data, Kano"
  )+
  coord_sf()

#plot
ggplot(all)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, colour=settlement), size=0.5, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
  theme_minimal() +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.line = element_blank(),   # Remove axis lines
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()
  ) +  # Remove panel background
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey Geo-points of Households Surveyed",
       subtitle = "GPS Position of HHs surveyed in Kano by Settlement type",
       caption = "Data source : Wet Season Cross Sectional Survey, Kano"
  )+
  coord_sf()


ggplot(all) +
  geom_point(data = intersects_a1, aes(geometry = geometry, colour = bi3), size = 0.5, stat = "sf_coordinates") +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  geom_sf_text(data = all, aes(label = WardName, geometry = geometry)) +
  geom_sf(fill = NA) +
  guides(size = FALSE) +
  map_theme() +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
       axis.line = element_blank(),   # Remove axis lines
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank()
      ) +  # Remove panel background
  labs(title = "Cross Sectional Survey Geo-points of Households Surveyed",
       subtitle = "GPS Position of HHs surveyed in Kano by Settlement type",
       caption = "Data source: Wet Season Cross Sectional Survey, Kano") +
  coord_sf() +
  ylab(NULL) +  # Remove y-axis label
  xlab(NULL)    # Remove x-axis label



ggplot(all) +
  geom_point(data = intersects_a1, aes(geometry = geometry, colour = bi3), size = 0.5, stat = "sf_coordinates") +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159"), name="Settlement Type") +
  geom_sf_text(data = all, aes(label = WardName, geometry = geometry)) +
  geom_sf(fill = NA) +
  guides(size = FALSE) +
  theme_manuscript() +
  map_theme()+
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.line = element_blank(),   # Remove axis lines
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        plot.background = element_rect(color = "grey", size = 0.1)) +  # Add plot border
  labs(title = "Cross Sectional Survey Geo-points of Households Surveyed",
       subtitle = "GPS Position of HHs surveyed in Kano by Settlement type",
       caption = "Data source: Wet Season Cross Sectional Survey, Kano") +
  coord_sf() +
  ylab(NULL) +  # Remove y-axis label
  xlab(NULL)    # Remove x-axis label











#plot
ggplot(all)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, colour=settlement_type), size=0.5, stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey GeoPoints of EAs Centroid",
       subtitle = "GPS Position of EA's surveyed in Kano by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()



zango <- all %>%
  filter(WardName=="Zango")

zangodf <- intersects_a1 %>%
  filter(Ward=="Zango")

#plot
ggplot(zango)+
  geom_point(data = zangodf,  aes(geometry = geometry, colour = Settlement.Type), size=1.3, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=zango,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Zango by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#####Dorayi Plot

#plot
ggplot(Dorayi)+
  geom_point(data = Dorayidf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Dorayi,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Dorayi by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()

#####Giginyu Plot

ggplot(Giginyu)+
  geom_point(data = Giginyudf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Giginyu,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Giginyu by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#####Fagge Plot

ggplot(Fagge)+
  geom_point(data = Faggedf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Fagge,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Fagge D2 by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#####Gobirawa Plot

ggplot(Gobirawa)+
  geom_point(data = Gobirawadf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Gobirawa,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Gobirawa by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#Nigeria Shape File
shapengn <- read_sf(dsn = "/Users/user/Downloads/NGPR7ADT/shapefile/", layer = "gadm36_NGA_1")



dff_ <- invalid_gps%>%
  filter(!is.na(Longitude) & !is.na(Latitude))
#remove other wards from the shape file
#define the coordinates
gps <- sf::st_as_sf(dff_, coords=c("Longitude", "Latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shapengn) <- 4326
intersects_ng <- st_intersection(gps, shapengn)

nga <- shapengn %>%
  filter(NAME_1=="Kano")

#####NGN Plot

ggplot(nga)+
  geom_point(data = intersects_ng,  aes(geometry = geometry, colour = Settlement.Type), size=0.4, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf(data = all ,fill = NA) +
  geom_sf_text(data=nga,aes(label = NAME_1 , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed falling outside of Kano Metropolis within and outside Kano state",
       caption = "Data source : Dry Season Cross Sectional Survey, Kano"
  )+
  coord_sf()




Wrongwards <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(Ward) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward, total_issues )

ggplot(Wrongwards, aes(reorder(Ward,-total_issues), total_issues ), colour="brown", fill="khaki") +
  geom_bar(stat = "identity", colour="brown", fill="tomato") +
  geom_point(aes(size=total_issues), color="brown" , colour="khaki")+
  scale_size_continuous(range = c(5, 10), guide = FALSE)  +
  geom_text(size=3,color="white",aes(label = paste0(total_issues)))+
  
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Quality Check Update",
       subtitle = "GPS Position of households surveyed outside of supposed wards",
       caption = "Data source : Cross Sectional Survey, Kano",
  )

Serials <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(`INTERVIEWER.S.NAME...15`,Ward) %>%
  select(1,2,3,4,7,14)


library(openxlsx)

Serials <- do.call(cbind, Serials) 
write.xlsx(Serials, "Cross_sectional_errors.xlsx") 


write.csv(Serials, "Cross sectional households surveyed outside of supposed wards_1.csv", row.names = FALSE)

Wrongwards <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(Ward, `INTERVIEWER.S.NAME`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward,`INTERVIEWER.S.NAME`, total_issues )
ggplot(Wrongwards, aes(reorder(`INTERVIEWER.S.NAME`,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(aes(size=total_issues, color=Ward))+
  scale_size_continuous(range = c(5, 13), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=2,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Cross Sectional Survey QA Update",
       subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       caption = "Data source : Cross Sectional Survey, Kano",
  )


dframelong<-read_csv('/Users/user/Downloads/UrbanMalariaLongitud_DATA_LABELS_2024-05-14_1753.csv')

dframelongib<-read_csv('/Users/user/Downloads/UrbanMalariaLongitud_DATA_LABELS_2024-05-14_1751.csv')



dframelong <- dframelong %>%
  group_by(`Serial Number`) %>%
  fill(5:7)


dframelong <- dframelong %>%
  mutate(`INTERVIEWER'S NAME...15` = recode(`INTERVIEWER'S NAME...15`, "AISHa AHMAD" = "AISHA AHMAD", "LAWAN GAMBO" = "LAWAN GAMBO MUHAMMAD", "ABDUL" = "ABDUL", "KABIRU IDRIS" = "KABIRU IDRIS GARBA", "BULKISU"= "BILKISU", "MASHKUR AHAMAD" = "MASHKUR AHMAD", "AISHA ABDULLAHI"= "AISHA MUHAMMAD", "IDRIS" = "KABIRU IDRIS GARBA"))


dframelong <- dframelong %>%
  mutate(`INTERVIEWER'S NAME...31` = recode(`INTERVIEWER'S NAME...31`, "AISHa AHMAD" = "AISHA AHMAD", "LAWAN GAMBO" = "LAWAN GAMBO MUHAMMAD", "ABDUL" = "ABDUL", "KABIRU IDRIS" = "KABIRU IDRIS GARBA", "BULKISU"= "BILKISU", "MASHKUR AHAMAD" = "MASHKUR AHMAD", "AISHA ABDULLAHI"= "AISHA MUHAMMAD", "ASHA ABDULLAHI" = "AISHA MUHAMMAD","28-4-24"="AISHA AHMAD" ,"AISHA"="AISHA AHMAD", "IDRIS" = "KABIRU IDRIS GARBA"))


Baseline <- dframelong %>%
  filter(`Event Name` == "Baseline", `Complete?...25`=="Complete")%>%
  group_by(`Event Name`,`INTERVIEWER'S NAME...15`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(`Event Name`,`INTERVIEWER'S NAME...15`, total_issues )

ggplot(Baseline, aes(reorder(`INTERVIEWER'S NAME...15`,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(aes(size=total_issues))+
  scale_size_continuous(range = c(5, 13), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=4,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )




followup <- dframelong %>%
  filter(`Complete?...39`=="Complete")%>%
  group_by(`Event Name`, `INTERVIEWER'S NAME...31`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(`Event Name`, `INTERVIEWER'S NAME...31`, total_issues )


colnames(Baseline)[colnames(Baseline) == "INTERVIEWER'S NAME...15"] <- "INTERVIEWER'S NAME"
colnames(followup)[colnames(followup) == "INTERVIEWER'S NAME...31"] <- "INTERVIEWER'S NAME"

baseline_fol <- rbind(followup, Baseline)



ggplot(followup, aes(reorder(`INTERVIEWER'S NAME...31`,-total_issues), total_issues )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =1) +
  geom_point(aes(size=total_issues, color=`Event Name`))+
  
  scale_size_continuous(range = c(3, 10), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=4,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )


ggplot(baseline_fol, aes(reorder(`INTERVIEWER'S NAME`,-total_issues), total_issues )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =1) +
  geom_point(aes(size=total_issues, color=`Event Name`))+
  scale_size_continuous(range = c(1, 6), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=2,colour="white",aes(label = paste0(total_issues)))+
  # geom_text(size=4,colour="white",aes(label = paste0(total_issues.y)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )



######

Baselinew <- dframelong %>%
  filter(`Event Name` == "Baseline", `Complete?...25`=="Complete")%>%
  group_by(WARD, `Event Name`,`INTERVIEWER'S NAME...15`) %>%
  fill(WARD)%>%
  summarise(
    total_issues = n()
  )%>%
  select(WARD, `Event Name`, total_issues )


#Follow up script

followupw <- dframelong %>%
  filter(`Complete?...39`=="Complete")

summary_bs_fl <- followupw %>%
  group_by(WARD, `Event Name`,`INTERVIEWER'S NAME...31`) %>%
  fill(WARD)%>%
  summarise(
    total_issues = n()
  )%>%
  select(WARD , `Event Name`, total_issues )


colnames(Baselinew)[colnames(Baselinew) == "INTERVIEWER'S NAME...15"] <- "INTERVIEWER'S NAME"
colnames(followupw)[colnames(followupw) == "INTERVIEWER'S NAME...31"] <- "INTERVIEWER'S NAME"

baseline_folw <- rbind(summary_bs_fl, Baselinew)


#####-----Total by Survey type by Ward 


alldf <- dframelong %>%
  filter(`Complete?...39`=="Complete" | `Complete?...25`=="Complete")%>%
  mutate(`INTERVIEWER'S NAME...15` = ifelse(is.na(`INTERVIEWER'S NAME...15`), `INTERVIEWER'S NAME...31`, `INTERVIEWER'S NAME...15`))


alldf <- dframelongib %>%
  filter(`Complete?...40`=="Complete" | `Complete?...25`=="Complete")%>%
  mutate(`INTERVIEWER'S NAME...15` = ifelse(is.na(`INTERVIEWER'S NAME...15`), `INTERVIEWER'S NAME...31`, `INTERVIEWER'S NAME...15`))



baseline_folw_sum <- alldf %>%
  group_by(WARD ,`INTERVIEWER'S NAME...15`, `Event Name`)%>%
  summarise(total_count = n())%>%
  select(WARD ,`INTERVIEWER'S NAME...15`, `Event Name`, total_count)

baseline_folw_sum <- baseline_folw_sum %>%
  group_by(`INTERVIEWER'S NAME...15`) %>%
  fill(WARD)




t_sum <- baseline_folw_sum %>%
  group_by(WARD, `Event Name`)%>%
  summarise(total_count = sum(total_count))%>%
  select(WARD , `Event Name`, total_count)



ggplot(t_sum, aes(reorder(WARD,-total_count), total_count )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =0.2) +
  geom_point(aes(size=total_count, color=`Event Name`))+
  scale_size_continuous(range = c(3, 10), guide = FALSE)  +
  geom_text(size=2,colour="white",aes(label = paste0(total_count)))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )



m_sum <-t_sum %>%
  group_by(`Event Name`)%>%
  summarise(total_count = sum(total_count))%>%
  select(`Event Name`, total_count)



ggplot(m_sum, aes(`Event Name`, total_count )) +
  geom_line(aes(colour=`Event Name`),size =0.2) +
  geom_point(aes(size=total_count, color=`Event Name`))+
  scale_size_continuous(range = c(4, 12), guide = FALSE)  +
  geom_text(size=3,colour="white",aes(label = paste0(total_count)))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Total Counts")+
  xlab("")+
  labs(title = "Total longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       caption = "Data source : Longitudunal Survey, Kano",
  ) + geom_hline(yintercept = 573, color = "red", size = 0.5) +
  annotate("text", x = Inf, y = 573, label = "Target = 573",size=3, hjust = 1.1, vjust = -0.5, color = "blue")



