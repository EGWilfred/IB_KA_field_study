user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
NuCDir <- file.path(Drive, "my_stuff")
NuDPDir <- file.path(Drive, "Desktop", "Desktop")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "EA_data")
ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')


##Analysis of 0-10 for longitudinal survey

ib_0_10ch <- read.csv(file.path(NuDPDir , "summary_children1.csv"))

ib_0_10ch$HOUSEHOLD.COORDINATE..Latitude <- format(ib_0_10ch$HOUSEHOLD.COORDINATE..Latitude, scientific = FALSE)

ib_0_10ch_d <- dplyr::select(ib_0_10ch, Ward, Settlement.Type, HOUSEHOLD.COORDINATE..Longitude,
                             HOUSEHOLD.COORDINATE..Latitude, Sex..Is..NAME..male.or.female....28,
                             Age..How.old.was....NAME..as.at.last.birthday....29)

ib_0_10ch_d <- rename(ib_0_10ch_d, Longitude = HOUSEHOLD.COORDINATE..Longitude,
                      Latitude = HOUSEHOLD.COORDINATE..Latitude,
                      Age = Age..How.old.was....NAME..as.at.last.birthday....29,
                      Sex = Sex..Is..NAME..male.or.female....28)

## read ibadan ward shape files
df_ib = st_read(file.path(NuDPDir, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan ")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_ibadan3.png'), p, width = 8, height = 6)


#Make HH of 0-10 children
ib_0_10ch_df <- sf::st_as_sf(ib_0_10ch_d, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ib) <- 4326

st_crs(ib_0_10ch_df) <- 4326

## Perform st intersection

ib_0_10ch_df_int <- st_intersection(ib_0_10ch_df, df_ib)



##Analysis by Ward
hh_0_10_a <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Agugu")

hh_0_10_b <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Bashorun")

hh_0_10_c <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Challenge")

hh_0_10_o <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Olopomewa")


#Spliting Ibadan Shapefile

df_ib_b <- df_ib %>%
  dplyr::filter(WardName == 'Bashorun')

df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')

#Agugu
pa <- ggplot(df_ib_a) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_a_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_a_df <- st_intersection(hh_0_10_a, df_ib_a)


#Basorun
pb <- ggplot(df_ib_b) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_b_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_b,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Basorun Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_b_df <- st_intersection(hh_0_10_b, df_ib_b)


#Challenge
pc <- ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_c_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_c_df <- st_intersection(hh_0_10_c, df_ib_c)


#Olopomewa

po <- ggplot(df_ib_o) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_o_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_o,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Olopomewa Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_o_df <- st_intersection(hh_0_10_o, df_ib_o)

plot_grid(pa, pb, pc, po)


library(ggplot2)
library(ggrepel)

row_to_change <- 38
col_to_change <- "Latitude"
new_value <- 7.384988

ib_0_10ch_d[row_to_change, col_to_change] <- new_value

ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_d, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.2, color = Settlement.Type)) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf(default_crs = NULL) +
  theme_minimal()


ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_d, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.7, color = as.factor(Settlement.Type)))+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label = WardName, geometry = geometry),
  #   color = 'black',
  #   size = 2.5,
  #   force = 1,
  #   max.overlaps = Inf
  # ) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
  theme_minimal()



## Location of sampled HHs of 0-10 in Ibadan

ib_samp0_10 <- read.csv(file.path(NuDPDir , "ib_samp_long_final.csv"))

names(ib_samp0_10)

colnames(ib_samp0_10) [1] <- "SN"
colnames(ib_samp0_10) [3] <- "Ward"
colnames(ib_samp0_10) [4] <- "Settlement"
colnames(ib_samp0_10) [6] <- "Cluster"
colnames(ib_samp0_10) [8] <- "Longitude"
colnames(ib_samp0_10) [9] <- "Latitude"

ib_samp_long <- ib_samp0_10 %>% dplyr::select("SN","Ward", "Cluster", "Settlement", "Longitude", "Latitude")

ib_samp_long <- separate(ib_samp_long, Cluster, into = c("Loc", "EA_code", "Num"), sep = "_|/")

#ib_samp_long <- ib_samp_long[-585,]

#Make HH Map of sampled 0-10 children
ib_samp_long_df <- sf::st_as_sf(ib_samp_long, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ib) <- 4326

st_crs(ib_samp_long_df) <- 4326

##AGUGU
ib_samp_long_dfa <-ib_samp_long_df %>% dplyr::filter(Ward == "AGG")

#ib_samp_long_dfa <- sf::st_as_sf(ib_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

#st_crs(df_ib_a) <- 4326

st_crs(ib_samp_long_dfa) <- 4326

# ##ll
# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfa,
   #   aes(label = SN, geometry = geometry),color ='black',
    #  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LS(368)")+
  coord_sf()

## Perform st intersection

ib_samp_long_dfa_int <- st_intersection(ib_samp_long_dfa, df_ib_a)

##GEOM
ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = ib_samp_long_dfa,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

ib_samp_long_a <- ib_samp_long %>% filter(Ward == "AGG")

ib_samp_long_a_n <- ib_samp_long_a %>% 
  filter(Latitude <= 7.39) %>% 
  filter(!(SN %in% c(11710, 11435, 10979, 11568, 10797, 10532, 10120,
                     10861, 11731, 11711, 10518,10052,10012,10025,10041,
                     10042, 10043, 10637,10722,11221,11235,10938,11569,
                     11229,11612, 10022, 10922,11531)))

ib_samp_long_dfa_n <- sf::st_as_sf(ib_samp_long_a_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfa_n,
  #aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LS(304)")+
  coord_sf()

##Create Data Frame of Outside HHs

df_outa <- ib_samp_long_a %>% 
  filter(Latitude >= 7.39)

##BASHORUN
ib_samp_long_dfb <-ib_samp_long_df %>% dplyr::filter(Ward == "BASH")

#ib_samp_long_dfa <- sf::st_as_sf(ib_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

#st_crs(df_ib_a) <- 4326

st_crs(ib_samp_long_dfb) <- 4326

# ##ll
# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfb,
  #   aes(label = SN, geometry = geometry),color ='black',
  #  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LS(292)")+
  coord_sf()

## Perform st intersection

ib_samp_long_dfb_int <- st_intersection(ib_samp_long_dfb, df_ib_b)

##GEOM
ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = ib_samp_long_dfb,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

ib_samp_long_b <- ib_samp_long %>% filter(Ward == "BASH")

##Get the mmissing ones for basorun
ib_samp_long_b_n <- ib_samp_long_b %>% 
  filter(Latitude <= 7.5)%>% 
  filter(!(SN %in% c(11994, 12661,12341, 12332, 12334, 12611,
                     12081,12327,12415,12784,12604,12323)))

ib_samp_long_dfb_n <- sf::st_as_sf(ib_samp_long_b_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfb_n,
  #aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LS(278)")+
  coord_sf()

##Create Data Frame of Outside HHs

dfb_out <- ib_samp_long_b %>% 
  filter(Latitude >= 7.5)



## Location of sampled HHs of 0-10 in Kano

#read in kano shape file and split for Dorayi and Ginginyu
df_ko = st_read(file.path(NuDPDir, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

df_kn_d <- df_ko %>%
  dplyr::filter(WardName == 'Dorayi')

df_kn_gin <- df_ko %>%
  dplyr::filter(WardName == 'Giginyu')

#read in kano sampled data frame
kn_samp0_10 <- read.csv(file.path(NuDPDir , "kn_samp_long_final.csv"))

names(kn_samp0_10)

colnames(kn_samp0_10) [1] <- "SN"
colnames(kn_samp0_10) [6] <- "Ward"
colnames(kn_samp0_10) [7] <- "Settlement"
#colnames(ib_samp0_10) [6] <- "Cluster"
colnames(kn_samp0_10) [11] <- "Longitude"
colnames(kn_samp0_10) [12] <- "Latitude"

kn_samp_long <- kn_samp0_10 %>% dplyr::select("SN","Ward", "Settlement", 
                                              "Longitude", "Latitude")

#kn_samp_long <- separate(kn_samp_long, Cluster, into = c("Loc", "EA_code", "Num"), sep = "_|/")

#kn_samp_long <- kn_samp_long[-585,]

#Make HH Map of sampled 0-10 children
kn_samp_long_df <- sf::st_as_sf(kn_samp_long, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ko) <- 4326

st_crs(kn_samp_long_df) <- 4326

##GINGINYU
kn_samp_long_dfg <-kn_samp_long_df %>% dplyr::filter(Ward == "GING")

#kn_samp_long_dfa <- sf::st_as_sf(kn_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

st_crs(df_kn_gin) <- 4326

st_crs(kn_samp_long_dfg) <- 4326

# ##ll
# ggplot(df_kn_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = kn_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_kn_gin)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfg,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
  data = kn_samp_long_dfg,
     aes(label = SN, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in kano showing selected HHs for LS(300)")+
  coord_sf()

## Perform st intersection

kn_samp_long_dfg_int <- st_intersection(kn_samp_long_dfg, df_kn_gin)

##Plot with Intersection
ggplot(df_kn_a)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfa_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = kn_samp_long_dfa,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in knadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

kn_samp_long_g <- kn_samp_long %>% filter(Ward == "GING")

kn_samp_long_g_n <- kn_samp_long_g %>% 
   filter(!(SN %in% c(27931, 27910, 27905, 0227, 0233, 6120))) 

rowrem <- 254
kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]

rowrem <- 254
kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]

# rowrem1 <- 255
# kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]


kn_samp_long_dfg_n <- sf::st_as_sf(kn_samp_long_g_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_kn_gin)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfg_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfg_n,
  # aes(label =SN, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in kano showing selected HHs for LS(293)")+
  coord_sf()

# ##Create Data Frame of Outside HHs
# 
# df_outg <- kn_samp_long_g %>% 
#   filter(Latitude >= 7.39)

##DORAYI
kn_samp_long_dfd <-kn_samp_long_df %>% dplyr::filter(Ward == "DOR")

#kn_samp_long_dfa <- sf::st_as_sf(kn_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

st_crs(df_kn_d) <- 4326

st_crs(kn_samp_long_dfd) <- 4326

# ##ll
# ggplot(df_kn_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = kn_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_kn_d)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfd,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfd,
  #    aes(label = SN, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in kano showing selected HHs for LS(336)")+
  coord_sf()

## Perform st intersection

kn_samp_long_dfd_int <- st_intersection(kn_samp_long_dfd, df_kn_d)

##Ploting the intersection
ggplot(df_kn_b)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfb_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
  data = kn_samp_long_dfd,
  aes(label =  EA_code, geometry = geometry),color ='black',
  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in knadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

kn_samp_long_d <- kn_samp_long %>% filter(Ward == "DOR")

##Get the outside ones for Dorayi
kn_samp_long_d_n <- kn_samp_long_d %>% 
  filter(Latitude <= 11.98)%>% 
  filter(!(SN %in% c(20943, 20090, 20780, 20704, 20710, 20779, 20778,
                     20086, 20083, 20089, 20084, 20555, 20965, 20324,
                     20063, 20062, 20719, 20079, 20668, 21076,
                     21074, 21055, 21054, 20645, 21072, 21044, 21071,
                     21073, 21059, 21052, 109,115,20023,21067, 21062,
                     21048, 20476, 20772, 21175, 21254, 20729, 20064,
                     21078, 21077, 21063, 21066, 20475, 20831, 20028,20662)))

kn_samp_long_dfd_n <- sf::st_as_sf(kn_samp_long_d_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_kn_d)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfd_n,  aes(geometry = geometry, size = 2.0, alpha = 0.5, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfd_n,
  # aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in kano showing selected HHs for LS(298)")+
  coord_sf()

# ##Create Data Frame of Outside HHs
# 
# dfb_out <- kn_samp_long_b %>% 
#   filter(Latitude >= 7.5)



##Distribution of LS data by age group

table(ib_0_10ch_d$Age)

table(ib_0_10ch_d$Ward)

##Select wards for conduct of LS
ib_0_10ch_w <- ib_0_10ch_d %>%  dplyr::filter(Ward == 'Agugu' | Ward == "Bashorun")

#break_pts <- c(0, 5, 10, 17, 30, 100)

break_pts <- c(-Inf, 1, 4, 7, 10)

labels <- c("less than 1", "1-4", "5-7", "8-10")

#labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

#age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat <- cut(ib_0_10ch_w$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

table(ib_0_10ch_w$Age)
table(age_cat)

age_cat_df <- data.frame(age_cat)

age_cat1 <- data.frame(age_cat)

ib_0_10ch_w_n <- cbind(ib_0_10ch_w, age_cat_df)

ggplot(ib_0_10ch_w_n, aes(x = age_cat, fill = age_cat)) +
  geom_bar() +
  labs(title = "Distribution of Age Categories of children 0-10",
       x = "Age Category",
       y = "Count") +
  theme_minimal()

# ggplot(ib_0_10ch_w_n, aes(x = age_cat, y = age_cat)) +
#   geom_bar() +
#   labs(title = "Bar Plot of Age Categories",
#        x = "Age Category",
#        y = "Count") +
#   theme_minimal()

# Create a line plot
ggplot(ib_0_10ch_w_n, aes(x = Age, group = 1)) +
  geom_line(stat = "count") +
  labs(title = "Distribution of Age Categories of children 0-10",
       x = "Age Category",
       y = "Count") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 10, 1))


##read in list of sampled housholds for LS(Ibadan)
ib_0_10_samp <- read.csv(file.path(NuDPDir , "ib_samp_0_10.csv"))

##Distribution of LS sampled data by age group

table(ib_0_10_samp$hh_hl5)

#break_pts <- c(0, 5, 10, 17, 30, 100)

break_pts <- c(-Inf, 1, 4, 7, 10)

labels <- c("less than 1", "1-4", "5-7", "8-10")

#labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

#age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat_s <- cut(ib_0_10_samp$hh_hl5, breaks = break_pts, labels = labels, na.rm = TRUE)

table(ib_0_10ch_d$Age)
table(age_cat)
age_cat_dfs <- data.frame(age_cat_s)

age_cat1 <- data.frame(age_cat)

ib_0_10_samp_n <- cbind(ib_0_10_samp, age_cat_dfs)

ggplot(ib_0_10_samp_n, aes(x = age_cat_s, fill = age_cat_s)) +
  geom_bar() +
  labs(title = "Distribution of Age Categories for sampled HHs",
       x = "Age Category",
       y = "Count") +
  theme_manuscript()

ggplot(ib_0_10_samp_n, aes(x = hh_hl5, group = 1)) +
  geom_line(stat = "count") +
  labs(title = "Distribution of Age Categories of Sampled HHs",
       x = "Age Category",
       y = "Count") +
  theme_manuscript()+
  scale_x_continuous(breaks = seq(0, 10, 1))














# 
# ##BASHORUN
# ib_samp_long_b <-ib_samp_long %>% dplyr::filter(Ward == "BASH")
# 
# ib_samp_long_dfb <- sf::st_as_sf(ib_samp_long_b, coords=c('Longitude', 'Latitude'), crs=4326)
# 
# st_crs(df_ib_b) <- 4326
# 
# st_crs(ib_samp_long_dfb) <- 4326
# 
# st_crs(df_ib_b) <- 4326
# 
# ## Perform st intersection
# 
# ib_samp_long_dfb_int <- st_intersection(ib_samp_long_dfb, df_ib)
# 
# ggplot(df_ib_b) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long_b, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   labs(title = "Sampled HHs in Bashorun for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# 
# ggplot(df_ib_b)+
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long_dfb,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
#   # geom_text_repel(
#   #   data = ib_samp_long_dfb,
#   #   aes(label =  Cluster, geometry = geometry),color ='black',
#     #stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   guides(size = FALSE)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Bashorun Ward in Ibadan showing selected HHs for LSS")+
#   coord_sf()


## Perform st intersection

ib_samp_long_df_int <- st_intersection(ib_samp_long_df, df_ib)


Settle <- c("Formal", "Informal", "Slum")
Within <- c(78, 260, 233)
Total <- c(81,270,242)

long_df <- data.frame(Settle, Within, Total)

long_df$Outside <- long_df$Total - long_df$Within

library(gridExtra)

print(tableGrob(long_df))


long_df_sampl <- ggplot(long_df, aes(x = Settle)) +
  geom_bar(aes(y = Within), stat = "identity", fill = "plum")+
  geom_bar(aes(y= Total), stat = "identity", fill = "yellow")+
  theme_minimal()

data("mtcars")
p <-    
  ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
  geom_bar(position = "dodge2")


ggplot(s_kn_css_sum, aes(x=Settlement.Type, y=count, fill=Settlement.Type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`Formal` = "#A7D397", `Informal` = "#FED2C7", `Slum` = "#8C3333")) +
  geom_text(aes(x = Settlement.Type, y= count, label = count, color = "black"), vjust = -0.2, size = 4.5) +
  #facet_grid(~ Settlement)+
  labs(title = "Number of households per Settlement type")+
  guides(color = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))



user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
NuCDir <- file.path(Drive, "my_stuff")
NuDPDir <- file.path(Drive, "Desktop", "Desktop")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "EA_data")
ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')


##Analysis of 0-10 for longitudinal survey

ib_0_10ch <- read.csv(file.path(NuDPDir , "summary_children1.csv"))

ib_0_10ch$HOUSEHOLD.COORDINATE..Latitude <- format(ib_0_10ch$HOUSEHOLD.COORDINATE..Latitude, scientific = FALSE)

ib_0_10ch_d <- dplyr::select(ib_0_10ch, Ward, Settlement.Type, HOUSEHOLD.COORDINATE..Longitude,
                             HOUSEHOLD.COORDINATE..Latitude, Sex..Is..NAME..male.or.female....28,
                             Age..How.old.was....NAME..as.at.last.birthday....29)

ib_0_10ch_d <- rename(ib_0_10ch_d, Longitude = HOUSEHOLD.COORDINATE..Longitude,
                      Latitude = HOUSEHOLD.COORDINATE..Latitude,
                      Age = Age..How.old.was....NAME..as.at.last.birthday....29,
                      Sex = Sex..Is..NAME..male.or.female....28)

## read ibadan ward shape files
df_ib = st_read(file.path(NuDPDir, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan ")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_ibadan3.png'), p, width = 8, height = 6)


#Make HH of 0-10 children
ib_0_10ch_df <- sf::st_as_sf(ib_0_10ch_d, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ib) <- 4326

st_crs(ib_0_10ch_df) <- 4326

## Perform st intersection

ib_0_10ch_df_int <- st_intersection(ib_0_10ch_df, df_ib)



##Analysis by Ward
hh_0_10_a <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Agugu")

hh_0_10_b <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Bashorun")

hh_0_10_c <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Challenge")

hh_0_10_o <- ib_0_10ch_df_int%>% dplyr::filter(Ward == "Olopomewa")


#Spliting Ibadan Shapefile

df_ib_b <- df_ib %>%
  dplyr::filter(WardName == 'Bashorun')

df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')

#Agugu
pa <- ggplot(df_ib_a) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_a_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_a,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_a_df <- st_intersection(hh_0_10_a, df_ib_a)


#Basorun
pb <- ggplot(df_ib_b) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_b_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_b,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Basorun Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_b_df <- st_intersection(hh_0_10_b, df_ib_b)


#Challenge
pc <- ggplot(df_ib_c) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_c_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_c,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_c_df <- st_intersection(hh_0_10_c, df_ib_c)


#Olopomewa

po <- ggplot(df_ib_o) +
  geom_sf(fill= NA)+
  geom_point(data = hh_0_10_o_df,  aes(geometry = geometry, size = 0.01, alpha = 0.7, col = Settlement.Type), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib_o,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Olopomewa Ward showing HH Listed for 0-10 year children")+
  coord_sf()

##Intersection to remove locations outside boundaries
hh_0_10_o_df <- st_intersection(hh_0_10_o, df_ib_o)

plot_grid(pa, pb, pc, po)


library(ggplot2)
library(ggrepel)

row_to_change <- 38
col_to_change <- "Latitude"
new_value <- 7.384988

ib_0_10ch_d[row_to_change, col_to_change] <- new_value

ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_d, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.2, color = Settlement.Type)) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf(default_crs = NULL) +
  theme_minimal()


ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_d, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.7, color = as.factor(Settlement.Type)))+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label = WardName, geometry = geometry),
  #   color = 'black',
  #   size = 2.5,
  #   force = 1,
  #   max.overlaps = Inf
  # ) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
  theme_minimal()



## Location of sampled HHs of 0-10 in Ibadan

ib_samp0_10 <- read.csv(file.path(NuDPDir , "ib_samp_long_final.csv"))

names(ib_samp0_10)

colnames(ib_samp0_10) [1] <- "SN"
colnames(ib_samp0_10) [3] <- "Ward"
colnames(ib_samp0_10) [4] <- "Settlement"
colnames(ib_samp0_10) [6] <- "Cluster"
colnames(ib_samp0_10) [8] <- "Longitude"
colnames(ib_samp0_10) [9] <- "Latitude"

ib_samp_long <- ib_samp0_10 %>% dplyr::select("SN","Ward", "Cluster", "Settlement", "Longitude", "Latitude")

ib_samp_long <- separate(ib_samp_long, Cluster, into = c("Loc", "EA_code", "Num"), sep = "_|/")

#ib_samp_long <- ib_samp_long[-585,]

#Make HH Map of sampled 0-10 children
ib_samp_long_df <- sf::st_as_sf(ib_samp_long, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ib) <- 4326

st_crs(ib_samp_long_df) <- 4326

##AGUGU
ib_samp_long_dfa <-ib_samp_long_df %>% dplyr::filter(Ward == "AGG")

#ib_samp_long_dfa <- sf::st_as_sf(ib_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

#st_crs(df_ib_a) <- 4326

st_crs(ib_samp_long_dfa) <- 4326

# ##ll
# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfa,
   #   aes(label = SN, geometry = geometry),color ='black',
    #  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LS(368)")+
  coord_sf()

## Perform st intersection

ib_samp_long_dfa_int <- st_intersection(ib_samp_long_dfa, df_ib_a)

##GEOM
ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = ib_samp_long_dfa,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

ib_samp_long_a <- ib_samp_long %>% filter(Ward == "AGG")

ib_samp_long_a_n <- ib_samp_long_a %>% 
  filter(Latitude <= 7.39) %>% 
  filter(!(SN %in% c(11710, 11435, 10979, 11568, 10797, 10532, 10120,
                     10861, 11731, 11711, 10518,10052,10012,10025,10041,
                     10042, 10043, 10637,10722,11221,11235,10938,11569,
                     11229,11612, 10022, 10922,11531)))

ib_samp_long_dfa_n <- sf::st_as_sf(ib_samp_long_a_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfa_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfa_n,
  #aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing selected HHs for LS(304)")+
  coord_sf()

##Create Data Frame of Outside HHs

df_outa <- ib_samp_long_a %>% 
  filter(Latitude >= 7.39)

##BASHORUN
ib_samp_long_dfb <-ib_samp_long_df %>% dplyr::filter(Ward == "BASH")

#ib_samp_long_dfa <- sf::st_as_sf(ib_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

#st_crs(df_ib_a) <- 4326

st_crs(ib_samp_long_dfb) <- 4326

# ##ll
# ggplot(df_ib_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfb,
  #   aes(label = SN, geometry = geometry),color ='black',
  #  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LS(292)")+
  coord_sf()

## Perform st intersection

ib_samp_long_dfb_int <- st_intersection(ib_samp_long_dfb, df_ib_b)

##GEOM
ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = ib_samp_long_dfb,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

ib_samp_long_b <- ib_samp_long %>% filter(Ward == "BASH")

##Get the mmissing ones for basorun
ib_samp_long_b_n <- ib_samp_long_b %>% 
  filter(Latitude <= 7.5)%>% 
  filter(!(SN %in% c(11994, 12661,12341, 12332, 12334, 12611,
                     12081,12327,12415,12784,12604,12323)))

ib_samp_long_dfb_n <- sf::st_as_sf(ib_samp_long_b_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = ib_samp_long_dfb_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  #geom_text_repel(
  #data = ib_samp_long_dfb_n,
  #aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing selected HHs for LS(278)")+
  coord_sf()

##Create Data Frame of Outside HHs

dfb_out <- ib_samp_long_b %>% 
  filter(Latitude >= 7.5)



## Location of sampled HHs of 0-10 in Kano

#read in kano shape file and split for Dorayi and Ginginyu
df_ko = st_read(file.path(NuDPDir, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

df_kn_d <- df_ko %>%
  dplyr::filter(WardName == 'Dorayi')

df_kn_gin <- df_ko %>%
  dplyr::filter(WardName == 'Giginyu')

#read in kano sampled data frame
kn_samp0_10 <- read.csv(file.path(NuDPDir , "kn_samp_long_final.csv"))

names(kn_samp0_10)

colnames(kn_samp0_10) [1] <- "SN"
colnames(kn_samp0_10) [6] <- "Ward"
colnames(kn_samp0_10) [7] <- "Settlement"
#colnames(ib_samp0_10) [6] <- "Cluster"
colnames(kn_samp0_10) [11] <- "Longitude"
colnames(kn_samp0_10) [12] <- "Latitude"

kn_samp_long <- kn_samp0_10 %>% dplyr::select("SN","Ward", "Settlement", 
                                              "Longitude", "Latitude")

#kn_samp_long <- separate(kn_samp_long, Cluster, into = c("Loc", "EA_code", "Num"), sep = "_|/")

#kn_samp_long <- kn_samp_long[-585,]

#Make HH Map of sampled 0-10 children
kn_samp_long_df <- sf::st_as_sf(kn_samp_long, coords=c('Longitude', 'Latitude'), crs=4326)

#Perform st_transformation
st_crs(df_ko) <- 4326

st_crs(kn_samp_long_df) <- 4326

##GINGINYU
kn_samp_long_dfg <-kn_samp_long_df %>% dplyr::filter(Ward == "GING")

#kn_samp_long_dfa <- sf::st_as_sf(kn_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

st_crs(df_kn_gin) <- 4326

st_crs(kn_samp_long_dfg) <- 4326

# ##ll
# ggplot(df_kn_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = kn_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_kn_gin)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfg,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
  data = kn_samp_long_dfg,
     aes(label = SN, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in kano showing selected HHs for LS(300)")+
  coord_sf()

## Perform st intersection

kn_samp_long_dfg_int <- st_intersection(kn_samp_long_dfg, df_kn_gin)

##Plot with Intersection
ggplot(df_kn_a)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfa_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  #   data = kn_samp_long_dfa,
  #   aes(label =  EA_code, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in knadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

kn_samp_long_g <- kn_samp_long %>% filter(Ward == "GING")

kn_samp_long_g_n <- kn_samp_long_g %>% 
   filter(!(SN %in% c(27931, 27910, 27905, 0227, 0233, 6120))) 

rowrem <- 254
kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]

rowrem <- 254
kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]

# rowrem1 <- 255
# kn_samp_long_g_n <- kn_samp_long_g_n[-rowrem, ,drop=FALSE]


kn_samp_long_dfg_n <- sf::st_as_sf(kn_samp_long_g_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_kn_gin)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfg_n,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfg_n,
  # aes(label =SN, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Ginginyu Ward in kano showing selected HHs for LS(293)")+
  coord_sf()

# ##Create Data Frame of Outside HHs
# 
# df_outg <- kn_samp_long_g %>% 
#   filter(Latitude >= 7.39)

##DORAYI
kn_samp_long_dfd <-kn_samp_long_df %>% dplyr::filter(Ward == "DOR")

#kn_samp_long_dfa <- sf::st_as_sf(kn_samp_long_df, coords=c('Longitude', 'Latitude'), crs=4326)

st_crs(df_kn_d) <- 4326

st_crs(kn_samp_long_dfd) <- 4326

# ##ll
# ggplot(df_kn_a) +
#   geom_sf(fill = NA) +
#   geom_point(data = kn_samp_long, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   #geom_text(aes(label=Cluster), vjust=1.6, color="black", size=0.5)+
#   labs(title = "Sampled HHs in Agugu for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())

##GEOM
ggplot(df_kn_d)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfd,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfd,
  #    aes(label = SN, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in kano showing selected HHs for LS(336)")+
  coord_sf()

## Perform st intersection

kn_samp_long_dfd_int <- st_intersection(kn_samp_long_dfd, df_kn_d)

##Ploting the intersection
ggplot(df_kn_b)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfb_int,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
  data = kn_samp_long_dfd,
  aes(label =  EA_code, geometry = geometry),color ='black',
  stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in knadan showing selected HHs for LSS")+
  coord_sf()

##Removal One after the other

kn_samp_long_d <- kn_samp_long %>% filter(Ward == "DOR")

##Get the outside ones for Dorayi
kn_samp_long_d_n <- kn_samp_long_d %>% 
  filter(Latitude <= 11.98)%>% 
  filter(!(SN %in% c(20943, 20090, 20780, 20704, 20710, 20779, 20778,
                     20086, 20083, 20089, 20084, 20555, 20965, 20324,
                     20063, 20062, 20719, 20079, 20668, 21076,
                     21074, 21055, 21054, 20645, 21072, 21044, 21071,
                     21073, 21059, 21052, 109,115,20023,21067, 21062,
                     21048, 20476, 20772, 21175, 21254, 20729, 20064,
                     21078, 21077, 21063, 21066, 20475, 20831, 20028,20662)))

kn_samp_long_dfd_n <- sf::st_as_sf(kn_samp_long_d_n, coords=c('Longitude', 'Latitude'), crs=4326)

ggplot(df_kn_d)+
  geom_sf(fill = NA) +
  geom_point(data = kn_samp_long_dfd_n,  aes(geometry = geometry, size = 2.0, alpha = 0.5, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  # geom_text_repel(
  # data = kn_samp_long_dfd_n,
  # aes(label =SN, geometry = geometry),color ='black',
  #stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in kano showing selected HHs for LS(298)")+
  coord_sf()

# ##Create Data Frame of Outside HHs
# 
# dfb_out <- kn_samp_long_b %>% 
#   filter(Latitude >= 7.5)



##Distribution of LS data by age group

table(ib_0_10ch_d$Age)

table(ib_0_10ch_d$Ward)

##Select wards for conduct of LS
ib_0_10ch_w <- ib_0_10ch_d %>%  dplyr::filter(Ward == 'Agugu' | Ward == "Bashorun")

#break_pts <- c(0, 5, 10, 17, 30, 100)

break_pts <- c(-Inf, 1, 4, 7, 10)

labels <- c("less than 1", "1-4", "5-7", "8-10")

#labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

#age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat <- cut(ib_0_10ch_w$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

table(ib_0_10ch_w$Age)
table(age_cat)

age_cat_df <- data.frame(age_cat)

age_cat1 <- data.frame(age_cat)

ib_0_10ch_w_n <- cbind(ib_0_10ch_w, age_cat_df)

ggplot(ib_0_10ch_w_n, aes(x = age_cat, fill = age_cat)) +
  geom_bar() +
  labs(title = "Distribution of Age Categories of children 0-10",
       x = "Age Category",
       y = "Count") +
  theme_minimal()

# ggplot(ib_0_10ch_w_n, aes(x = age_cat, y = age_cat)) +
#   geom_bar() +
#   labs(title = "Bar Plot of Age Categories",
#        x = "Age Category",
#        y = "Count") +
#   theme_minimal()

# Create a line plot
ggplot(ib_0_10ch_w_n, aes(x = Age, group = 1)) +
  geom_line(stat = "count") +
  labs(title = "Distribution of Age Categories of children 0-10",
       x = "Age Category",
       y = "Count") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 10, 1))


##read in list of sampled housholds for LS(Ibadan)
ib_0_10_samp <- read.csv(file.path(NuDPDir , "ib_samp_0_10.csv"))

##Distribution of LS sampled data by age group

table(ib_0_10_samp$hh_hl5)

#break_pts <- c(0, 5, 10, 17, 30, 100)

break_pts <- c(-Inf, 1, 4, 7, 10)

labels <- c("less than 1", "1-4", "5-7", "8-10")

#labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

#age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat_s <- cut(ib_0_10_samp$hh_hl5, breaks = break_pts, labels = labels, na.rm = TRUE)

table(ib_0_10ch_d$Age)
table(age_cat)
age_cat_dfs <- data.frame(age_cat_s)

age_cat1 <- data.frame(age_cat)

ib_0_10_samp_n <- cbind(ib_0_10_samp, age_cat_dfs)

ggplot(ib_0_10_samp_n, aes(x = age_cat_s, fill = age_cat_s)) +
  geom_bar() +
  labs(title = "Distribution of Age Categories for sampled HHs",
       x = "Age Category",
       y = "Count") +
  theme_manuscript()

ggplot(ib_0_10_samp_n, aes(x = hh_hl5, group = 1)) +
  geom_line(stat = "count") +
  labs(title = "Distribution of Age Categories of Sampled HHs",
       x = "Age Category",
       y = "Count") +
  theme_manuscript()+
  scale_x_continuous(breaks = seq(0, 10, 1))














# 
# ##BASHORUN
# ib_samp_long_b <-ib_samp_long %>% dplyr::filter(Ward == "BASH")
# 
# ib_samp_long_dfb <- sf::st_as_sf(ib_samp_long_b, coords=c('Longitude', 'Latitude'), crs=4326)
# 
# st_crs(df_ib_b) <- 4326
# 
# st_crs(ib_samp_long_dfb) <- 4326
# 
# st_crs(df_ib_b) <- 4326
# 
# ## Perform st intersection
# 
# ib_samp_long_dfb_int <- st_intersection(ib_samp_long_dfb, df_ib)
# 
# ggplot(df_ib_b) +
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long_b, aes(x = Longitude, y = Latitude, size = 0.2, alpha = 0.05, color = Settlement)) +
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
#   guides(alpha = FALSE, size = FALSE) +
#   labs(title = "Sampled HHs in Bashorun for longitudinal survey(0-10years children)") +
#   coord_sf(default_crs = NULL) +
#   map_theme()+
#   theme_manuscript()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# 
# ggplot(df_ib_b)+
#   geom_sf(fill = NA) +
#   geom_point(data = ib_samp_long_dfb,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
#   # geom_text_repel(
#   #   data = ib_samp_long_dfb,
#   #   aes(label =  Cluster, geometry = geometry),color ='black',
#     #stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   guides(size = FALSE)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Bashorun Ward in Ibadan showing selected HHs for LSS")+
#   coord_sf()


## Perform st intersection

ib_samp_long_df_int <- st_intersection(ib_samp_long_df, df_ib)


Settle <- c("Formal", "Informal", "Slum")
Within <- c(78, 260, 233)
Total <- c(81,270,242)

long_df <- data.frame(Settle, Within, Total)

long_df$Outside <- long_df$Total - long_df$Within

library(gridExtra)

print(tableGrob(long_df))


long_df_sampl <- ggplot(long_df, aes(x = Settle)) +
  geom_bar(aes(y = Within), stat = "identity", fill = "plum")+
  geom_bar(aes(y= Total), stat = "identity", fill = "yellow")+
  theme_minimal()

ggplot(s_kn_css_sum, aes(x=Settlement.Type, y=count, fill=Settlement.Type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`Formal` = "#A7D397", `Informal` = "#FED2C7", `Slum` = "#8C3333")) +
  geom_text(aes(x = Settlement.Type, y= count, label = count, color = "black"), vjust = -0.2, size = 4.5) +
  #facet_grid(~ Settlement)+
  labs(title = "Number of households per Settlement type")+
  guides(color = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

