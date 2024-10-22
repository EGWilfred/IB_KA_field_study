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

  NuDPDir <- file.path(Drive, "Desktop", "NU STUFF","Desktop")

  NuDPDir <- file.path(Drive, "Desktop")

  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "EA_data")
  EpiDir <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "Shiny data")
  HFDir <- file.path(EpiDir, "Health_Facility")
  HHDir <- file.path(EpiDir, "Household")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}

##Household Listed Plots

hh_list_k <- read.csv ("C:/Users/DELL/Desktop/NU STUFF/Desktop/kn_hh_list_2209.csv")

hh_list_k <- read.csv(file.path(NuDPDir, "kn_hh_list_2209.csv"))

# Extract the 'x' and 'y' columns

hh_list_x <- hh_list_k[, c("Ward", "EA.Serial.Number", "X_Enter.GPS.Location_latitude",
                         "X_Enter.GPS.Location_longitude", "X001..Serial.Number.of.Structure")]

##Extraction of individual sheets and shapefile
names(hh_list_x)[names(hh_list_x) == "X_Enter.GPS.Location_latitude"] <- "Latitude"
names(hh_list_x)[names(hh_list_x) == "X_Enter.GPS.Location_longitude"] <- "Longitude"

hh_list_g <- hh_list_x%>% dplyr::filter(Ward == "Gobirawa")

hh_list_d <- hh_list_x%>% dplyr::filter(Ward == "Dorayi")

hh_list_f <- hh_list_x%>% dplyr::filter(Ward == "Fagge")

hh_list_z <- hh_list_x%>% dplyr::filter(Ward == "Zango")



## read ibadan ward shape files

df_ko = st_read(file.path(NuDPDir, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

df_ko = st_read(file.path(shapepath, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 


p <- ggplot(df_ko) +
  geom_sf(fill = "khaki") +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = dplyr::filter(cdc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano ")+
  coord_sf()

#Spliting Ibadan Shapefile

##Mapping of EAs

#Spliting Kano Shapefile

df_kn_z <- df_ko %>%
  dplyr::filter(WardName == 'Zango')

df_kn_t <- df_ko %>%
  dplyr::filter(WardName == 'Tudun Wazurchi')

df_kn_f <- df_ko %>%
  dplyr::filter(WardName == 'Fagge D2')

df_kn_d <- df_ko %>%
  dplyr::filter(WardName == 'Dorayi')

df_kn_g <- df_ko %>%
  dplyr::filter(WardName == 'Gobirawa')

df_kn_gin <- df_ko %>%
  dplyr::filter(WardName == 'Giginyu')

#Make HH listing plots
hh_list_df_k <- sf::st_as_sf(hh_list_k, coords=c('X_Enter.GPS.Location_longitude',
                                             'X_Enter.GPS.Location_latitude'), crs=4326)
## Perform st transformation
st_crs(df_ko) <- 4326

st_crs(hh_list_df_k) <- 4326

ggplot(df_ko)+
  geom_sf(fill= NA)+
  geom_point(data = hh_list_df_k,  aes(geometry = geometry, size = 0.1, alpha = 0.2, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Wards in Kano showing HH Listed")+
  coord_sf()



##Dorayi

hh_dor <- hh_list_df_k %>% dplyr::filter(Ward == "Dorayi")

hh_dor_sum_cl <- hh_dor %>% dplyr::select(-c('geometry'))%>% 
  group_by(`EA.Serial.Number`) %>% 
  summarise(count = n())

hh_dor_sum_d <- hh_dor %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_dor_sum_d$ward <- "Dorayi"

p <- ggplot(data=hh_dor_sum_cl, aes(x= as.factor(EA.Serial.Number), y=count))+
  geom_point(size = 4.5, color = "seagreen4") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Dorayi as at 28/08/2023")+
  geom_point(size = 3.0, color = "seagreen4") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Dorayi.pdf'), p, width = 8, height = 6)

ggplot(data = hh_dor_sum_d , aes(x = start, y = count)) +
  geom_line()+
  geom_point()+
  #map_theme()+ 
  #theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Dorayi")

## Perform st transformation to plot HH Location
st_crs(df_kn_d) <- 4326

st_crs(hh_dor) <- 4326


p <- ggplot(df_kn_d)+
  geom_sf(fill= NA)+
  geom_point(data = hh_dor,  aes(geometry = geometry, size = 0.2, alpha = 0.8, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_dor,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in Kano showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Dorayi All listed HHs per EA.pdf'), p, width = 8, height = 6)

# hh_dor_1 <- hh_dor %>% dplyr::filter(`EA.Serial.Number` == "1")
# 
# ea_1_bas <- data.frame(table(hh_dor_1$EA.Serial.Number, hh_dor_1$X001..Serial.Number.of.Structure))
# 
# ea_1_bas$Ward <- "dororun"
# 
# table(hh_dor$EA.Serial.Number, hh_dor$X001..Serial.Number.of.Structure)

##Household/ sampling analysis Dorayi
dor_n <- hh_dor %>%
  group_by(`EA.Serial.Number`) %>%
  count(X001..Serial.Number.of.Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

dor_ns <- dor_n %>%
  group_by(`EA.Serial.Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

dor_hs <- dor_n %>%
  group_by(as.factor(`EA.Serial.Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

dor_all_hhs <- cbind(dor_ns, dor_hs)

dor_all_hhs$tot_hh_2 <- dor_all_hhs$Count*2

dor_all_hhs$tot_hh_3 <- dor_all_hhs$Count*3

dor_all_hhs$targets <- 50

dor_all_hhs <- data.frame(dor_all_hhs)

dor_all_hhs1 <- dor_all_hhs %>% dplyr::select(-geometry, -geometry.1)

#dor_all_hhs2 <- dor_all_hhs1 %>% dplyr::select(-geometry)

write.csv(dor_all_hhs1, "C:/Users/ebn2804/OneDrive - Northwestern University/Desktop/dor_summary1.csv"
          , row.names = FALSE)

##Households per structure summary
#dor_hh_s <- hh_dor %>%
  group_by(`EA.Serial.Number`, `X001..Serial.Number.of.Structure`,
           `X_004_Serial_Number_o_old_in_the_structure`) %>%
  summarize(Count = n()) %>%
  ungroup()

col_range <- range(dor_hh_s$Count)
###

ggplot(dor_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA in Dorayi",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(dor_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = dor_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA in Dorayi",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


ggplot(dor_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki2", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = dor_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA in dororun",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


##Gobirawa

hh_gob <- hh_list_df_k %>% dplyr::filter(Ward == "Gobirawa")

hh_gob_sum_cl <- hh_gob %>% group_by(`EA.Serial.Number`) %>% 
  summarise(count = n())

hh_gob_sum_d <- hh_gob %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_gob_sum_d$ward <- "Gobirawa"

p <- ggplot(data=hh_gob_sum_cl, aes(x= as.factor(EA.Serial.Number), y=count))+
  geom_point(size = 4.5, color = "blue") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Gobirawa as at 28/08/2023")+
  geom_point(size = 3.0, color = "blue") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "op"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Gobirawa.pdf'), p, width = 8, height = 6)

ggplot(data = hh_gob_sum_d , aes(x = start, y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Gobirawa")

## Perform st transformation to plot HH Location
st_crs(df_kn_g) <- 4326

st_crs(hh_gob) <- 4326

p <- ggplot(df_kn_g)+
  geom_sf(fill= NA)+
  geom_point(data = hh_gob,  aes(geometry = geometry, size = 0.5, alpha = 0.5, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_dor,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Gobirawa Ward in Kano showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Gobirawa All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)

##Household / sampling analysis Gobirawa
gob_n <- hh_gob %>%
  group_by(`EA.Serial.Number`) %>%
  count(X001..Serial.Number.of.Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

gob_ns <- gob_n %>%
  group_by(`EA.Serial.Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

gob_hs <- gob_n %>%
  group_by(as.factor(`EA.Serial.Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

gob_all_hhs <- cbind(gob_ns, gob_hs)

gob_all_hhs1 <- data.frame(gob_all_hhs)

gob_all_hhs1 <- gob_all_hhs1 %>% dplyr::select(-geometry, -geometry.1)

gob_all_hhs1$tot_hh_2 <- gob_all_hhs1$Count*2

gob_all_hhs1$tot_hh_3 <- gob_all_hhs1$Count*3

gob_all_hhs1$targets <- 50


write.csv(gob_all_hhs1, "C:/Users/ebn2804/OneDrive - Northwestern University/Desktop/gob_summary1.csv"
          , row.names = FALSE)


ggplot(gob_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA in Gobirawa",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(gob_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = gob_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA in Gobirawa",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))


ggplot(gob_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "plum", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = gob_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA in gobgu",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))


gob_all_hhsp <- gob_all_hhs%>% dplyr::select(-geometry.1)

ggplot(df_kn_g)+
  geom_sf(fill= NA)+
  geom_point(data = gob_all_hhsp,  aes(geometry = geometry, size = Count, alpha = 5.0, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  scale_fill_gradient(low = "blue", high = "red", name = "Number")+
  geom_text(
    data = gob_all_hhsp,
    aes(label =  EA.Serial.Number, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE, legend = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Gobirawa Ward in Kano showing Structures Listed")+
  coord_sf()


# 
# table(hh_agu_12$EA.Serial.Number, hh_agu_12$X001..Serial.Number.of.Structure)



##Zango

hh_zn <- hh_list_df_k %>% dplyr::filter(Ward == "Zango")

hh_zn_sum_cl <- hh_zn %>% group_by(`EA.Serial.Number`) %>% 
  summarise(count = n())

hh_zn_sum_d <- hh_zn %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

# hh_zn_sum_nl <- hh_zn %>% dplyr::select(-c('geometry'))%>% 
#   group_by(`Lister_s_Name`) %>% 
#   summarise(count = n())

hh_zn_sum_d$ward <- "Zango"

p <- ggplot(data=hh_zn_sum_cl, aes(x= as.factor(EA.Serial.Number), y=count))+
  geom_point(size = 4.5, color = "orange") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Zango as at 28/08/2023")+
  geom_point(size = 3.0, color = "orange") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in znlenge.pdf'), p, width = 8, height = 6)

ggplot(data = hh_zn_sum_d , aes(x = as.factor(start), y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Zango")

## Perform st transformation
st_crs(df_kn_z) <- 4326

st_crs(hh_zn) <- 4326

## Perform st intersection

zango_a <- st_intersection(hh_zn, df_kn_z)

p <- ggplot(df_kn_z)+
  geom_sf(fill= NA)+
  geom_point(data = zango_a,  aes(geometry = geometry, size = 0.01, alpha = 0.5, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_dor,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Zango Ward in Kano showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'znlenge All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)


##Household / sampling analysis Zango
zn_n <- hh_zn %>%
  group_by(`EA.Serial.Number`) %>%
  count(X001..Serial.Number.of.Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

zn_ns <- zn_n %>%
  group_by(`EA.Serial.Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

zn_hs <- zn_n %>%
  group_by(as.factor(`EA.Serial.Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

zn_all_hhs <- cbind(zn_ns, zn_hs)

zn_all_hhs$tot_hh_2 <- zn_all_hhs$Count*2

zn_all_hhs$tot_hh_3 <- zn_all_hhs$Count*3

zn_all_hhs$targets <- 50

zn_all_hhs2 <- data.frame(zn_all_hhs)


zn_all_hhs1 <- zn_all_hhs2 %>% dplyr::select(-geometry, -geometry.1)

#zn_all_hhs2 <- zn_all_hhs1 %>% dplyr::select(-geometry)



write.csv(zn_all_hhs2, "C:/Users/ebn2804/OneDrive - Northwestern University/Desktop/zn_summary1.csv"
          , row.names = FALSE)


ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red") +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


##Faggae

hh_fg <- hh_list_df_k %>% dplyr::filter(Ward == "Fagge")
row_to_delete <- 1
hh_fg <- hh_fg [-row_to_delete, ]

hh_fg_sum_cl <- hh_fg %>% group_by(`EA.Serial.Number`) %>% 
  summarise(count = n())

hh_fg_sum_d <- hh_fg %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

# hh_fg_sum_nl <- hh_fg %>% dplyr::select(-c('geometry'))%>% 
#   group_by(`Lister_s_Name`) %>% 
#   summarise(count = n())

hh_fg_sum_d$ward <- "Faggae"

p <- ggplot(data=hh_fg_sum_cl, aes(x= as.factor(EA.Serial.Number), y=count))+
  geom_point(size = 3.0, color = "tomato4") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Faggae as at 28/08/2023")+
  geom_point(size = 3.0, color = "tomato4") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in fgomewa.pdf'), p, width = 8, height = 6)

ggplot(data = hh_fg_sum_d , aes(x = as.factor(start), y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in fgomewa")


## Perform st transformation
st_crs(df_kn_f) <- 4326

st_crs(hh_fg) <- 4326

faggae_a <- st_intersection(hh_fg, df_kn_f)

p <- ggplot(df_kn_f)+
  geom_sf(fill= NA)+
  geom_point(data = faggae_a,  aes(geometry = geometry, size = 0.01, alpha = 0.5, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_dor,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Faggae Ward in Kano showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'fgmewa All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)


##Household / sampling analysis Faggae
fg_n <- hh_fg %>%
  group_by(`EA.Serial.Number`) %>%
  count(X001..Serial.Number.of.Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

fg_ns <- fg_n %>%
  group_by(`EA.Serial.Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

fg_hs <- fg_n %>%
  group_by(as.factor(`EA.Serial.Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

fg_all_hhs <- cbind(fg_ns, fg_hs)

fg_all_hhs$tot_hh_2 <- fg_all_hhs$Count*2

fg_all_hhs$tot_hh_3 <- fg_all_hhs$Count*3

fg_all_hhs$targets <- 50

fg_all_hhs2 <- data.frame(fg_all_hhs)


fg_all_hhs1 <- fg_all_hhs2 %>% dplyr::select(-geometry, -geometry.1)

#chal_all_hhs2 <- chal_all_hhs1 %>% dplyr::select(-geometry)



write.csv(fg_all_hhs1, "C:/Users/ebn2804/OneDrive - Northwestern University/Desktop/chal_summary1.csv"
          , row.names = FALSE)


ggplot(fg_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA in Fagge",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(fg_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = fg_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA in Fagge",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


ggplot(fg_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "blue", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = fg_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA in Fagge",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))



##Giginyu

hh_gi <- read.csv(file.path(NuDPDir, "kn_hh_gin.csv"))

hh_gi_sum_cl <- hh_gi %>% group_by(`EA.Serial.Number`) %>% 
  summarise(count = n())

hh_gi_sum_d <- hh_gi %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

# hh_zn_sum_nl <- hh_zn %>% dplyr::select(-c('geometry'))%>% 
#   group_by(`Lister_s_Name`) %>% 
#   summarise(count = n())

hh_zn_sum_d$ward <- "Zango"

p <- ggplot(data=hh_gi_sum_cl, aes(x= as.factor(EA.Serial.Number), y=count))+
  geom_point(size = 4.5, color = "orange") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Ginginyu as at 05/10/2023")+
  geom_point(size = 3.0, color = "orange") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in znlenge.pdf'), p, width = 8, height = 6)

ggplot(data = hh_zn_sum_d , aes(x = as.factor(start), y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Zango")

## Perform st transformation
hh_list_df_gi <- sf::st_as_sf(hh_gi, coords=c('X_Enter.GPS.Location_longitude',
                                                 'X_Enter.GPS.Location_latitude'), crs=4326)

st_crs(df_kn_gin) <- 4326

st_crs(hh_list_df_gi) <- 4326

## Perform st intersection

giginyu_a <- st_intersection(hh_list_df_gi, df_kn_gin)

p <- ggplot(df_kn_gin)+
  geom_sf(fill= NA)+
  geom_point(data = hh_list_df_gi,  aes(geometry = geometry, size = 0.01, alpha = 0.5, col = as.factor(EA.Serial.Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  # data = hh_dor,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Giginyu Ward in Kano showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'znlenge All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)


##Household / sampling analysis Zango
zn_n <- hh_zn %>%
  group_by(`EA.Serial.Number`) %>%
  count(X001..Serial.Number.of.Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

zn_ns <- zn_n %>%
  group_by(`EA.Serial.Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

zn_hs <- zn_n %>%
  group_by(as.factor(`EA.Serial.Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

zn_all_hhs <- cbind(zn_ns, zn_hs)

zn_all_hhs$tot_hh_2 <- zn_all_hhs$Count*2

zn_all_hhs$tot_hh_3 <- zn_all_hhs$Count*3

zn_all_hhs$targets <- 50

zn_all_hhs2 <- data.frame(zn_all_hhs)


zn_all_hhs1 <- zn_all_hhs2 %>% dplyr::select(-geometry, -geometry.1)

#zn_all_hhs2 <- zn_all_hhs1 %>% dplyr::select(-geometry)



write.csv(zn_all_hhs2, "C:/Users/ebn2804/OneDrive - Northwestern University/Desktop/zn_summary1.csv"
          , row.names = FALSE)


ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red") +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_manuscript2()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))


ggplot(zn_all_hhs1) +
  geom_col(aes(x = as.factor(EA.Serial.Number), y = hhs), fill = "lightblue", width = 0.5) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(EA.Serial.Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = zn_all_hhs1$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA in Zango",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="Households available per EA"))





# hh_olop_1 <- hh_chal %>% dplyr::filter(`EA.Serial.Number` == "1")
# 
# hh_chal_1s <- hh_chal_1 %>%  group_by(`X001..Serial.Number.of.Structure`) %>% 
#   summarise(total_households = count(`X_004_Serial_Number_o_old_in_the_structure`, na.rm = T)) %>% ungroup()
# 
# ea_1_chal <- data.frame(table(hh_chal_1$EA.Serial.Number, hh_chal_1$X001..Serial.Number.of.Structure, hh_chal$X_004_Serial_Number_o_old_in_the_structure))
# 
# ea_1_chal$Ward <- "Challenge"
# 
# table(hh_chal$EA.Serial.Number, hh_chal$X001..Serial.Number.of.Structure)




##Further Analysis
all_sum_df <- rbind(hh_agu_sum_d, hh_dor_sum_d, hh_chal_sum_d)

all_sum_df$start <- sub("^\\d{4}-", "", all_sum_df$start)

all_sum_df$start <- sub("^\\d{2}-", "", all_sum_df$start)

ggplot(data = all_sum_df , aes(x = start, y = count)) +
  geom_point(size = 2.0, alpha = 0.7, col = "blue")+
  geom_line()+
  facet_wrap(~ ward)+
  theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day")

##EA 1

all_ea_1 <- rbind(ea_1_chal, ea_1_bas, ea_1_agu)

p <- ggplot(data = all_ea_1 , aes(x = as.factor(Var2), y = Freq)) +
  geom_point(size = 3.0, alpha = 0.7, col = "red")+
  geom_line()+
  facet_wrap(~ Ward)+
  theme_manuscript()+
  ylab("Frequency of Household Structures")+
  xlab("Number of Household Structures")+
  labs(title= "Number of households listed per day by no of structures")

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Households per structure.pdf'), p, width = 8, height = 6)




