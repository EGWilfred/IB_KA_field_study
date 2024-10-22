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
  EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "EA_data")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}

##----------------------------------------------------------------------------
#Libraries--------------------------------------------------------------------
##----------------------------------------------------------------------------

library(readxl)
library(sf)
library(vcd)
library(ggplot2)
library(tmap)
library(ggrepel)
library(tidyverse)
library(geometry)
library(dplyr)
library(rgdal)
library(fun)
library(patchwork)
library(rgeos)
library(maptools)
library(purrr)
library(DescTools)
library(conflicted)
library(stringr)


map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

con_gplot <-function(df,fill,label){
  ggplot()+
    geom_sf(data=df, mapping=aes(fill = !!fill)) +
    map_theme() +
    geom_text_repel(
      data = df,
      aes(label = !!label, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

##----------------------------------------------------------------------------
#CSV files--------------------------------------------------------------------
##----------------------------------------------------------------------------
files <- list.files(path = EADat , pattern = ".csv", full.names = TRUE, recursive = F)
dat <- sapply(files, read_csv, simplify = F)
names(dat)

## read ibadan ward shape files
df_ib = st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))


##Mapping of EAs

#Spliting Ibadan Shapefile

df_ib_b <- df_ib %>%
  dplyr::filter(WardName == 'Bashorun')

df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')

##Agugu
Agugu <- dat[[1]]

agu_hh <- read.csv(file.path(NuDPDir, "Agugu.csv"))

agu_df <- sf::st_as_sf(agu_hh, coords=c('Latitude', 'Longitude'), crs=4326)

# Perform spatial intersection
st_crs(df_ib_a) <- 4326

st_crs(agu_df) <- 4326

intersects_a <- st_intersection(agu_df, df_ib_a)

p <- ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = intersects_a,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_a,
    aes(label =  ea_code, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected enumeration areas that fall within the ward")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Agugu EAs overall.pdf'), p, width = 8, height = 6)


##Bashorun

Bashorun <- dat[[2]]

bas_hh <- read.csv(file.path(NuDPDir, "Bashorun.csv"))

#bas_hh1 <- read.csv(file.path(NuDPDir, "ba_new1.csv"))

bas_hh_df <- sf::st_as_sf(bas_hh, coords=c('Latitude', 'Longitude'), crs=4326)

#bas_sel <- read.csv(file.path(NuDPDir, "bas_sel.csv"))

# Perform spatial intersection
st_crs(df_ib_b) <- 4326

st_crs(bas_hh_df) <- 4326

intersects_b <- st_intersection(bas_hh_df, df_ib_b)

#intersect_sel <- st_intersection(agu_sel_df, df_ib_a)

p <- ggplot(df_ib_b) +
  geom_sf(fill = NA) +
  geom_point(data = intersects_b,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_b,
    aes(label =  ea_code, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing EAs for listing")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Bashorun EAs overall.pdf'), p, width = 8, height = 6)



##Olopomewa

Olopomewa <- dat[[4]]

ol_hh <- read.csv(file.path(NuDPDir, "Olopomewa.csv"))

ol_hh_df <- sf::st_as_sf(ol_hh, coords=c('Latitude', 'Longitude'), crs=4326)

p <- ggplot(df_ib_o) +
  geom_sf(fill = NA) +
  geom_point(data = ol_hh_df,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = ol_hh_df,
    aes(label =  ea_code, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Olopomewa Ward in Ibadan showing EAs for listing")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Olopomewa EAs overal.pdf'), p, width = 8, height = 6)



##Challenge 

Challenge <- dat[[3]]

chal_hh <- read.csv(file.path(NuDPDir, "Challenge.csv"))

chal_df <- chal_hh %>% dplyr::select(ea_code, Longitude, Latitude, Settlement)

chal_hh_o <- sf::st_as_sf(chal_df, coords=c('Latitude', 'Longitude'), crs=4326)


# Perform spatial intersection
st_crs(df_ib_c) <- 4326

st_crs(chal_hh_o) <- 4326

intersects_c <- st_intersection(chal_hh_o, df_ib_c)

p <- ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data = intersects_c,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slums = "#923159"))+
  geom_text_repel(
    data = intersects_c,
    aes(label =  ea_code, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward in Ibadan showing enumeration areas that fall within the ward")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Challenge EAs Location.pdf'), p, width = 8, height = 6)



##Final Selected Enumeration Areas

final_sel <- read.csv(file.path(NuDPDir, "Sampled_SS.csv"))

final_sel_df <- sf::st_as_sf(final_sel, coords=c('Latitude', 'Longitude'), crs=4326)

##Agugu
final_agugu <- final_sel_df %>% dplyr::filter(ward == "Agu")

final_ag_df <- sf::st_as_sf(final_agugu, coords=c('Latitude', 'Longitude'), crs=4326)

p <- ggplot(df_ib_a) +
  geom_sf(fill= NA)+
  geom_point(data = final_ag_df,  aes(geometry = geometry, size = 2.0, col = settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = final_ag_df,
    aes(label =  selected_cluster_no, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected enumeration areas for HH Listing")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Agugu selected EAs.pdf'), p, width = 8, height = 6)

##Bashorun

final_bash <- final_sel_df %>% dplyr::filter(ward == "bas")

final_bash_df <- sf::st_as_sf(final_bash, coords=c('Longitude', 'Latitude'), crs=4326)

p <- ggplot(df_ib_b) +
  geom_sf(fill= NA)+
  geom_point(data = final_bash_df,  aes(geometry = geometry, size = 2.0, col = settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = final_bash_df,
    aes(label =  ea_code_old, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected enumeration areas for HH Listing")+
  coord_sf()


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Bashorun selected EAs.pdf'), p, width = 8, height = 6)


##Challenge

final_chall <- final_sel_df %>% dplyr::filter(ward == "Challenge")

final_chall_df <- sf::st_as_sf(final_chall, coords=c('Longitude', 'Latitude'), crs=4326)

p <- ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data = final_chall_df,  aes(geometry = geometry, size = 2.0, col = settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = final_chall_df,
    aes(label =  selected_cluster_no, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward in Ibadan showing selected enumeration areas for HH Listing")+
  coord_sf()


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Challenge selected EAs.pdf'), p, width = 8, height = 6)


##Olopomewa

final_olop <- final_sel_df %>% dplyr::filter(ward == "ol")

final_olop_df <- sf::st_as_sf(final_olop, coords=c('Longitude', 'Latitude'), crs=4326)

p <- ggplot(df_ib_o) +
  geom_sf(fill= NA)+
  geom_point(data = final_olop_df,  aes(geometry = geometry, size = 2.0, col = settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = final_olop_df,
    aes(label =  ea_code_old, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Olopomewa Ward in Ibadan showing selected enumeration areas for HH Listing")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Olopomewa selected EAs.pdf'), p, width = 8, height = 6)



##Ends Here##----------------------------------------------------------------###
###--------------------------------------------------------------------------###









ggplot(df_ib) +
  geom_sf(data = df_ib) +
  geom_text(data = df_ib,
            aes(label =  WardName, geometry = geometry),color ='black',
            stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1) +
  map_theme()


p <- ggplot(df_ib) +
  geom_sf(fill= NA)+
  geom_point(data = final_sel_df,  aes(geometry = geometry, size = 2.0, alpha = 0.7, col = settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Wards in Ibadan showing selected enumeration areas for HH Listing")+
  coord_sf()


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Ibadan selected EAs.pdf'), p, width = 8, height = 6)


##Facilitate Joining by manipulating data
chal_hh1 <- chal_hh %>% dplyr::select(ea_code, Latitude, Longitude, Settlement)
ol_hh1 <- ol_hh %>% dplyr::select(ea_code, Latitude, Longitude, Settlement)
bas_hh1 <- bas_hh %>% dplyr::select(ea_code, Latitude, Longitude, Settlement)
agu_hh1 <- agu_hh %>% dplyr::select(ea_code, Latitude, Longitude, Settlement)

all_d <- rbind(chal_hh1, ol_hh1, bas_hh1, agu_hh1)

write.csv (all_d, file.path(NuDPDir, "All_data.csv"))

all_d <- read.csv(file.path(NuDPDir, "All_data.csv"))

all_d_df <- sf::st_as_sf(all_d, coords=c('Latitude', 'Longitude'), crs=4326)

view(all_d_df)

# Perform spatial intersection
st_crs(df_ib) <- 4326

st_crs(all_d_df) <- 4326

intersects_all <- st_intersection(all_d_df, df_ib)


p <- ggplot(df_ib) +
  geom_sf(fill= NA)+
  geom_point(data = intersects_all,  aes(geometry = geometry, size = 2.0, alpha = 0.7, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Wards in Ibadan showing possible enumeration areas for HH Listing")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Ibadan All EAs.pdf'), p, width = 8, height = 6)

# agu_hh <- read.csv(file.path(NuDPDir, "Agugu_new1.csv"))
# 
# agu_sel <- read.csv(file.path(NuDPDir, "Agugu_sel.csv"))
# 
# bas_hh <- read.csv(file.path(NuDPDir, "bash_new.csv"))
# 
# bas_sel <- read.csv(file.path(NuDPDir, "bas_sel.csv"))
# 
# 
# chal_hh <- read.csv(file.path(NuDPDir, "chall_hh.csv"))
# 
# ol_sel <- read.csv(file.path(NuDPDir, "ol_hh.csv"))
# 
# hh_list <- read.csv(file.path(NuDPDir, "HHList.csv"))

# p <- ggplot(df_ib_a) +
#   geom_sf(fill = "lightgrey") +
#   geom_point(data = agu_hh, mapping = aes(x = Longitude, y = Latitude, fill = Settlement), size = 3.5, alpha = 0.5, shape = 21)+
#   geom_text_repel(data = agu_hh,
#                   aes(label=ea_code, x = Longitude , y = Latitude,
#                       check_overlap=T
#                   ))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Agugu Ward in Ibadan showing location of enumeration areas, 2023")+
#   coord_sf()
# 
# p <- ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = agu_sel, mapping = aes(x = Latitude, y = Longitude, fill = Settlement), size = 3.5, alpha = 0.5, shape = 21)+
#   geom_text_repel(data = agu_sel,
#                   aes(label=ea_code, x = Latitude, y = Longitude, hjust = 1.2,
#                       check_overlap=T
#                   ))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Agugu Ward in Ibadan showing selected enumeration areas for household listing, 2023")+
#   coord_sf()
# 
# ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Agugu EAs Selected.pdf'), p, width = 8, height = 6)
# 
# 
# ##within Agugu
# 
# length(agu_hh)
# 
# 
# 
# #agu_df <- agu_hh %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# agu_sel_df <- agu_sel %>%
#   st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326)
# 
# class(df_ib_a)
# class(agu_sel_df)
# 
# colnames(df_ib_a)
# 
# # Perform spatial intersection
# st_crs(df_ib_a) <- 4326
# 
# st_crs(agu_df) <- 4326
# 
# intersects <- st_intersection(agu_df, df_ib_a)
# 
# 
# 
# intersect_sel <- st_intersection(agu_sel_df, df_ib_a)
# 
# 
# intersecting_data <- agu_df[intersects, ]
# 
# 
# ggplot(df_ib_a) +
#   geom_sf(fill = "lightblue") +
#   geom_point(data = agu_hh, mapping = aes(x = Longitude , y = Latitude, fill = Settlement), size = 3.5, alpha = 0.5, shape = 21)+
#   geom_text_repel(data = agu_hh,
#                   aes(label=ea_code, x = Longitude, y = Latitude, hjust = 1.2,
#                       check_overlap=T
#                   ))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Agugu Ward in Ibadan showing selected enumeration areas for household listing, 2023")+
#   coord_sf()
# 
# ggplot(df_ib_a) +
#   geom_sf(fill='lightblue')+
#   geom_point(data = intersects,  aes(geometry = geometry, size = 2.5, col = Settlement), stat= "sf_coordinates")+
#   viridis::scale_color_viridis(option='D', discrete=TRUE)+
#   geom_text_repel(
#     data = intersects,
#     aes(label =  ea_code, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Agugu Ward in Ibadan showing all enumeration areas that fall within the ward")+
#   coord_sf()

# p <- ggplot(df_ib_b) +
#   geom_sf(fill = NA) +
#   geom_point(data = bas_sel, mapping = aes(x = Longitude, y = Latitude, fill = Settlement), size = 2.5, alpha = 0.8, shape = 21)+
#   geom_text_repel(data = bas_sel,
#                   aes(label=ea_code, x = Longitude , y = Latitude, hjust = 1.4,
#                       check_overlap=T
#                   ))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Bashorun Ward in Ibadan showing selected enumeration areas, 2023")+
#   coord_sf()
# 
# ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Bashorun EAs Selected.pdf'), p, width = 8, height = 6)

# chal_sel <- read.csv(file.path(NuDPDir, "chal_sel.csv"))
# 
# st_crs <- NULL
# 
# 
# p <- ggplot(df_ib_c) +
#   geom_sf(fill = "grey") +
#   geom_point(data = chal_df, mapping = aes(x =Latitude , y = Longitude, fill = Settlement), size = 1.5, alpha = 0.7, shape = 21)+
#   geom_text_repel(data = chal_df,
#                   aes(label = ea_code, x = Latitude, y = Longitude,
#                       check_overlap=T
#                   ))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Challenge Ward in Ibadan showing enumeration areas, Jan -March, 2023")+
#   coord_sf()
# 
# p <- ggplot(df_ib_c) +
#   geom_sf(fill = NA) +
#   geom_point(data = chal_sel, mapping = aes(x = Longitude , y = Latitude, fill = settlement), 
#              size = 3.5, alpha = 0.5, shape = 21)+
#   geom_text_repel(data = chal_sel,
#                   aes(x = Longitude , y = Latitude, label = ea_code, vjust = 1.2))+
#   viridis::scale_fill_viridis(option='D', discrete=TRUE)+
#   # geom_text_repel(
#   #   data = df_ib_a,
#   #   aes(label =  WardName, geometry = geometry),color ='black',
#   #stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Challenge Ward in Ibadan showing enumeration areas selected for HH, 2023")+
#   coord_sf()
# 
# ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Challenge EAs Selected.pdf'), p, width = 8, height = 6)

# # Perform spatial intersection
# st_crs(df_ib_c) <- 4326
# 
# st_crs(chal_hh_o) <- 4326
# 
# intersects_c <- st_intersection(chal_hh_o, df_ib_c)
# 
# intersect_sel <- st_intersection(agu_sel_df, df_ib_a)
# 
# 
# intersecting_data <- agu_df[intersects, ]
# 
# ggplot(df_ib_c) +
#   geom_sf(fill='grey')+
#   geom_point(data = intersects_c,  aes(geometry = geometry, size = 2.0, col = Settlements), stat= "sf_coordinates")+
#   viridis::scale_color_viridis(option='I', discrete=TRUE)+
#   geom_text_repel(
#     data = intersects_c,
#     aes(label =  ea_code, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   guides(size = FALSE)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Challenge Ward in Ibadan showing enumeration areas that fall within the ward")+
#   coord_sf()

