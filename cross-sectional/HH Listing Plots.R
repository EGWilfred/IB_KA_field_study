library(gridExtra)


##Household Listed Plots

hh_list <- read.csv(file.path(NuDPDir, "hh_list_1207.csv"))

# Extract the 'x' and 'y' columns

hh_list_x <- hh_list[, c("Ward", "Cluster_Number", "X_Enter_GPS_Location_latitude",
                         "X_Enter_GPS_Location_longitude")]

##Extraction of individual sheets and shapefile
names(hh_list_x)[names(hh_list_x) == "X_Enter_GPS_Location_latitude"] <- "Latitude"
names(hh_list_x)[names(hh_list_x) == "X_Enter_GPS_Location_longitude"] <- "Longitude"

hh_list_a <- hh_list_x%>% dplyr::filter(Ward == "Agugu")

hh_list_b <- hh_list_x%>% dplyr::filter(Ward == "Bashorun")

hh_list_c <- hh_list_x%>% dplyr::filter(Ward == "Challenge")

hh_list_o <- hh_list_x%>% dplyr::filter(Ward == "Olopomewa")


#Spliting Ibadan Shapefile

df_ib_b <- df_ib %>%
  dplyr::filter(WardName == 'Bashorun')

df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')

#Make HH listing plots
hh_list_df <- sf::st_as_sf(hh_list, coords=c('X_Enter_GPS_Location_longitude', 'X_Enter_GPS_Location_latitude'), crs=4326)

ggplot(df_ib) +
  geom_sf(fill= NA)+
  geom_point(data = hh_list_df,  aes(geometry = geometry, size = 0.2, alpha = 0.7, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Wards in Ibadan showing HH Listed")+
  coord_sf()



##Bashorun

hh_bash <- hh_list_df %>% dplyr::filter(Ward == "Bashorun")

hh_bash_sum_cl <- hh_bash %>% dplyr::select(-c('geometry'))%>% 
  group_by(`Cluster_Number`) %>% 
  summarise(count = n())

hh_bash_sum_d <- hh_bash %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_bash_sum_d$ward <- "Bashorun"

p <- ggplot(data=hh_bash_sum_cl, aes(x= as.factor(Cluster_Number), y=count))+
  geom_point(size = 3.0, color = "seagreen4") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Bashorun as at 08/07/2023")+
  geom_point(size = 3.0, color = "seagreen4") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Bashorun.pdf'), p, width = 8, height = 6)

ggplot(data = hh_bash_sum_d , aes(x = start, y = count)) +
  geom_line()+
  geom_point()+
  #map_theme()+ 
  #theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Bashorun")


p <- ggplot(df_ib_b)+
  geom_sf(fill= NA)+
  geom_point(data = hh_bash,  aes(geometry = geometry, size = 0.5, alpha = 0.8, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_bash,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward in Ibadan showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Bashorun All listed HHs per EA.pdf'), p, width = 8, height = 6)

hh_bash_1 <- hh_bash %>% dplyr::filter(`Cluster_Number` == "1")

ea_1_bas <- data.frame(table(hh_bash_1$Cluster_Number, hh_bash_1$X_001_Serial_Number_of_Structure))

ea_1_bas$Ward <- "Bashorun"

table(hh_bash$Cluster_Number, hh_bash$X_001_Serial_Number_of_Structure)

bash_n <- hh_bash %>% group_by(`Cluster_Number`, `X_001_Serial_Number_of_Structure`) %>% 
  count.fields(total_hhs = count(`X_004_Serial_Number_o_old_in_the_structure`, na.rm = T))%>% 
  ungroup()

bash_n <- hh_bash %>%
  group_by(Cluster_Number, X_001_Serial_Number_of_Structure) %>%
  count(X_004_Serial_Number_o_old_in_the_structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()


##Agugu

hh_agu <- hh_list_df %>% dplyr::filter(Ward == "Agugu")

hh_agu_sum_cl <- hh_agu %>% group_by(`Cluster_Number`) %>% 
  summarise(count = n())

hh_agu_sum_d <- hh_agu %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_agu_sum_d$ward <- "Agugu"

p <- ggplot(data=hh_agu_sum_cl, aes(x= as.factor(Cluster_Number), y=count))+
  geom_point(size = 3.0, color = "blue") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Agugu as at 08/07/2023")+
  geom_point(size = 3.0, color = "blue") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Agugu.pdf'), p, width = 8, height = 6)

ggplot(data = hh_agu_sum_d , aes(x = start, y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Agugu")

p <- ggplot(df_ib_a)+
  geom_sf(fill= NA)+
  geom_point(data = hh_agu,  aes(geometry = geometry, size = 0.5, alpha = 0.5, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_bash,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Agugu All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)

view(hh_agu)

# hh_agu_1 <- hh_agu %>% dplyr::filter(`Cluster_Number` == "1")
# 
# ea_1_agu <- data.frame(table(hh_agu_1$Cluster_Number, hh_agu_1$X_001_Serial_Number_of_Structure))
# 
# ea_1_agu$Ward <- "Agugu"

agu_n <- hh_agu %>%
  group_by(`Cluster_Number`) %>%
  count(X_001_Serial_Number_of_Structure, na.rm = TRUE, name = "total_hhs") %>%
  ungroup()

agu_ns <- agu_n %>%
  group_by(`Cluster_Number`)%>%
  summarize(Count = n()) %>%
  ungroup()

agu_hs <- agu_n %>%
  group_by(as.factor(`Cluster_Number`)) %>%
  summarize(hhs = sum(total_hhs)) %>%
  ungroup()

agu_all_hhs <- cbind(agu_ns, agu_hs)

agu_all_hhs$tot_hh_2 <- agu_all_hhs$Count*2

agu_all_hhs$tot_hh_3 <- agu_all_hhs$Count*3

agu_all_hhs$targets <- 50


ggplot(agu_all_hhs) +
  geom_col(aes(x = as.factor(Cluster_Number), y = hhs), fill = "lightblue", width = 0.5) +
  geom_line(aes(x = as.factor(Cluster_Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  #geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen(Blue) and Household Structures(Red)
       per EA",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))

ggplot(agu_all_hhs) +
  geom_col(aes(x = as.factor(Cluster_Number), y = hhs), fill = "khaki", width = 0.5) +
  #geom_line(aes(x = as.factor(Cluster_Number), y = Count, group = 1), color = "red", size = 1) +
  geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = agu_all_hhs$targets, linetype = "dashed", color = "red")+
  #geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_3, group = 1), color = "green", size = 1) +
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(2 per structure)
       per EA",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))


ggplot(agu_all_hhs) +
  geom_col(aes(x = as.factor(Cluster_Number), y = hhs), fill = "khaki2", width = 0.5) +
  #geom_line(aes(x = as.factor(Cluster_Number), y = Count, group = 1), color = "red", size = 1) +
  #geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_2, group = 1), color = "black", size = 1) +
  geom_line(aes(x = as.factor(Cluster_Number), y = tot_hh_3, group = 1), color = "black", size = 1) +
  geom_hline(yintercept = agu_all_hhs$targets, linetype = "dashed", color = "red")+
  #geom_label(aes(label = Count), vjust = -1)+
  labs(title = "Distribution of Number of households seen and HHs avaiable(3 per structure)
       per EA",
       x = "EA Code",
       y = "Number of Households seen") +
  theme_minimal()+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="House Structures"))


ggplot(agu_all_hhs, aes(fill= , 
                            y=number, 
                            x=Cluster_Number)) + 
  geom_bar(position='dodge', stat='identity')+ 
  #scale_x_continuous(breaks = seq(1,7,1) )+
  theme(legend.position = "right", legend.background = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x = "clustering classification", fill = "prevalence classes",
       y = "Frequency")

fig <- plotly::plot_ly(agu_all_hhs,x = ~Cluster_Number,y=~tot_hh_2,
                       type = 'bar', name = '2 per HH') %>%
  add_trace(y=~tot_hh_3,name = '3 per HH') %>%
  layout(yaxis = list(title = 'Number of HHs'),
         barmode = 'unstack',title="Placement Statistics")

fig

agu_all_hhsp <- agu_all_hhs%>% dplyr::select(-geometry.1)

ggplot(df_ib_a)+
  geom_sf(fill= NA)+
  geom_point(data = agu_all_hhsp,  aes(geometry = geometry, size = Count, alpha = 5.0, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  scale_fill_gradient(low = "blue", high = "red", name = "Number")+
  geom_text(
  data = agu_all_hhsp,
  aes(label =  Cluster_Number, geometry = geometry),color ='black',
  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE, legend = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward in Ibadan showing Structures Listed")+
  coord_sf()

# hh_agu_12 <- hh_agu %>% dplyr::filter(`Cluster_Number` == "12")
# 
# table(hh_agu_12$Cluster_Number, hh_agu_12$X_001_Serial_Number_of_Structure)



##Challenge

hh_chal <- hh_list_df %>% dplyr::filter(Ward == "Challenge")

hh_chal_sum_cl <- hh_chal %>% group_by(`Cluster_Number`) %>% 
  summarise(count = n())

hh_chal_sum_d <- hh_chal %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_chal_sum_nl <- hh_chal %>% dplyr::select(-c('geometry'))%>% 
  group_by(`Lister_s_Name`) %>% 
  summarise(count = n())

hh_chal_sum_d$ward <- "Challenge"

p <- ggplot(data=hh_chal_sum_cl, aes(x= as.factor(Cluster_Number), y=count))+
  geom_point(size = 3.0, color = "orange") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Challenge as at 08/07/2023")+
  geom_point(size = 3.0, color = "orange") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Challenge.pdf'), p, width = 8, height = 6)

ggplot(data = hh_chal_sum_d , aes(x = as.factor(start), y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Challenge")

p <- ggplot(df_ib_c)+
  geom_sf(fill= NA)+
  geom_point(data = hh_chal,  aes(geometry = geometry, size = 0.01, alpha = 0.5, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_bash,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward in Ibadan showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Challenge All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)

hh_chal_1 <- hh_chal %>% dplyr::filter(`Cluster_Number` == "1")

hh_chal_1s <- hh_chal_1 %>%  group_by(`X_001_Serial_Number_of_Structure`) %>% 
   summarise(total_households = count(`X_004_Serial_Number_o_old_in_the_structure`, na.rm = T)) %>% ungroup()

ea_1_chal <- data.frame(table(hh_chal_1$Cluster_Number, hh_chal_1$X_001_Serial_Number_of_Structure, hh_chal$X_004_Serial_Number_o_old_in_the_structure))

ea_1_chal$Ward <- "Challenge"

table(hh_chal$Cluster_Number, hh_chal$X_001_Serial_Number_of_Structure)


##Olopomewa

hh_olop <- hh_list_df %>% dplyr::filter(Ward == "Olopomewa")

hh_olop_sum_cl <- hh_olop %>% group_by(`Cluster_Number`) %>% 
  summarise(count = n())

hh_olop_sum_d <- hh_olop %>% dplyr::select(-c('geometry'))%>% 
  group_by(`start`) %>% 
  summarise(count = n())

hh_olop_sum_nl <- hh_olop %>% dplyr::select(-c('geometry'))%>% 
  group_by(`Lister_s_Name`) %>% 
  summarise(count = n())

hh_olop_sum_d$ward <- "Olopomewa"

p <- ggplot(data=hh_olop_sum_cl, aes(x= as.factor(Cluster_Number), y=count))+
  geom_point(size = 3.0, color = "tomato4") +
  labs(y= "Number of Households", x = "EA Code")+
  geom_line()+
  ggtitle("Number of Households listed per EA in Olopomewa as at 10/07/2023")+
  geom_point(size = 3.0, color = "tomato4") +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Number of Households lister per EA in Olopomewa.pdf'), p, width = 8, height = 6)

ggplot(data = hh_olop_sum_d , aes(x = as.factor(start), y = count)) +
  geom_line()+
  geom_point()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households listed per day in Olopomewa")

p <- ggplot(df_ib_o)+
  geom_sf(fill= NA)+
  geom_point(data = hh_chal,  aes(geometry = geometry, size = 0.01, alpha = 0.5, col = as.factor(Cluster_Number)), stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  #geom_text_repel(
  # data = hh_bash,
  # aes(label =  Enumeration_Area, geometry = geometry),color ='black',
  # stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE, alpha = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Challenge Ward in Ibadan showing HH Listed")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Olopmewa All listed HHs per EA.pdf.pdf'), p, width = 8, height = 6)

# hh_olop_1 <- hh_chal %>% dplyr::filter(`Cluster_Number` == "1")
# 
# hh_chal_1s <- hh_chal_1 %>%  group_by(`X_001_Serial_Number_of_Structure`) %>% 
#   summarise(total_households = count(`X_004_Serial_Number_o_old_in_the_structure`, na.rm = T)) %>% ungroup()
# 
# ea_1_chal <- data.frame(table(hh_chal_1$Cluster_Number, hh_chal_1$X_001_Serial_Number_of_Structure, hh_chal$X_004_Serial_Number_o_old_in_the_structure))
# 
# ea_1_chal$Ward <- "Challenge"
# 
# table(hh_chal$Cluster_Number, hh_chal$X_001_Serial_Number_of_Structure)




##Further Analysis
all_sum_df <- rbind(hh_agu_sum_d, hh_bash_sum_d, hh_chal_sum_d)

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




