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

##Household survey analysis

ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

kn_css_hh <- read.csv(file.path(NuDPDir , "hh_data_kano_3011.csv")) 

##Overall Summary
State <- c("Ibadan", "Kano")

hh_data_report <- data.frame(State,    
                             final_hh_figures = c(3109, 4463),
                             target_hh_figures = c(7000,9625),
                             sampled_hh_figures = c(4598,5184)
)

hh_data_report <- hh_data_report %>% 
  mutate(Percent_Complete_T = round(hh_data_report$final_hh_figures/hh_data_report$target_hh_figures * 100, 1)) %>% 
  mutate(Percent_Complete_S = round(hh_data_report$final_hh_figures/hh_data_report$sampled_hh_figures * 100, 1))


ib_hh_visited <- ggplot(hh_data_report, aes(x = State)) +
  geom_bar(aes(y = final_hh_figures), stat = "identity", fill = "plum")+
  geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of households visited during the wet season household survey in \n Ibadan and Kano showing the protocol targets(red), percent sampled(green)", size = 8,
       x = "State",
       y = "Total number of households") +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))




##Daily Monitoring for Wet season Mop up
#Kano
kn_unique_hhss <- unique(kn_css_hh$Serial.Number)

kn_css_sum_d <- kn_css_hh %>% 
  group_by(`Date`) %>% 
  summarise(count = n())

View(kn_css_sum_d)

# Extract rows for November
nov_kn_css_hh <- kn_css_sum_d[c(38,39,40,41,42,43,44,45), ]

ggplot(data = nov_kn_css_hh , aes(x = Date, y = count)) +
  geom_line()+
  geom_point(size = 5.5, col = "blue")+
  #map_theme()+ 
  theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households interviewd per day in Kano")

## Settlement Type
kn_css_sum_s <- kn_css_hh %>% 
  group_by(`Settlement.Type`) %>% 
  summarise(count = n())

s_kn_css_sum <- kn_css_sum_s[c(2,6,8), ]

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


#Ibadan
ib_unique_hhss <- unique(ib_css_hh$Serial.Number)

ib_css_sum_d <- ib_css_hh %>% 
  group_by(`Date`) %>% 
  summarise(count = n())

View(ib_css_sum_d)

# Extract rows for November
nov_ib_css_hh <- ib_css_sum_d[c(33,36,37,38,39,40,41,42,43,44,45), ]

ggplot(data = nov_ib_css_hh , aes(x = Date, y = count)) +
  geom_line()+
  geom_point(size = 5.5, col = "red")+
  #map_theme()+ 
  theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households interviewd per day in Ibadan")


## Settlement Type
kn_css_sum_s <- kn_css_hh %>% 
  group_by(`Settlement.Type`) %>% 
  summarise(count = n())






library(ggplot2)
library(ggrepel)  # Make sure ggrepel is installed

ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],
                                      size = 0.2, alpha = 0.7, color = as.factor(Settlement.Type))) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  geom_text_repel(data = df_ib, aes(label = WardName, x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
                  color = 'black', size = 2.5, force = 1, min.segment.length = 0, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf() +
  theme_minimal()

##Men and Women analysis
hh_data_mw <- data.frame(State,    
                             men = c(1448, 3442),
                             women = c(2405,3985)
)

w_hh_sampled <- ggplot(hh_data_mw, aes(x = State)) +
  geom_bar(aes(y = women), stat = "identity", fill = "plum")+
  #geom_bar(aes(y = women), stat = "identity", fill = "khaki")+
  # geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  # geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  # scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  # geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  # geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  # geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of women interviewed during the wet season household survey in Ibadan and Kano", size = 8,
       x = "State",
       y = "Total number of women respondents") +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))


m_hh_sampled <- ggplot(hh_data_mw, aes(x = State)) +
  #geom_bar(aes(y = men), stat = "identity", fill = "plum")+
  geom_bar(aes(y = women), stat = "identity", fill = "khaki")+
  # geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  # geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  # scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  # geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  # geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  # geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of men interviewed during the wet season household survey in Ibadan and Kano")+
       x = "State"
       y = "Total number of men interviewd in households" +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))

# kn_css_hh_new <- kn_css_hh %>% 
#   select(Serial.Number, LOCAL.GOVT..AREA, Ward,
#          Settlement.Type, Enumeration.Area.Cluster.Number,
#          HOUSEHOLD.COORDINATE..Longitude, 
#          HOUSEHOLD.COORDINATE..Latitude, whatever)
#   group_by(Serial.Number, LOCAL.GOVT..AREA, Ward,
#           Settlement.Type, Enumeration.Area.Cluster.Number,
#            HOUSEHOLD.COORDINATE..Longitude, 
#            HOUSEHOLD.COORDINATE..Latitude) %>% 
#   summerize(total = n())
#   uncount(total) %>% 



# mutate(Serial_Number = ifelse(Serial.Number == "", Serial.Number[1], Serial.Number),
#        lga = ifelse(LOCAL.GOVT..AREA == "", LOCAL.GOVT..AREA[1], LOCAL.GOVT..AREA))


css_sum_wwq <- css_hh %>% 
  group_by(`Ward`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

names(kn_css_hh)

table(css_hh$Ward)


##Number of HHs
unique_hhss <- unique(css_hh$Serial.Number)

css_sum_w <- css_hh %>% 
  group_by(`Ward`) %>% 
  summarise(count = n())

css_sum_wwq <- css_hh %>% 
  group_by(`Ward`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())


##RDT Tested in Househoulds

# rdt_hh <- read.csv(file.path(NuDPDir, "rdt_hh.csv"))
# 
# rdt_hh <- data.frame(rdt_hh)
# 
# view(rdt_hh)

break_pts <- c(0, 5, 10, 17, 30, 100)

labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat1 <- data.frame(age_cat)

css_hh1 <- cbind(css_hh, age_cat1)

css_hh1 <- css_hh1 %>%
  mutate(q302..RESULT = case_when(
    q302..RESULT == "POSITIVE" ~ 1,
    q302..RESULT == "NEGATIVE" ~ 0,
    TRUE ~ 2  # For all other cases, including empty or blank values
  ))

# Create a factor with custom labels
css_hh1$q302..RESULT <- factor(css_hh1$q302..RESULT, 
                                  levels = c(0, 1, 2), labels = c("NEG", "POS", "ND"))

library(dplyr)

# Remove rows where ColumnName is equal to "LabelToRemove"
css_hh1n <- css_hh1 %>%
  dplyr::filter(`q302..RESULT`!="ND")


css_sum_h <- css_hh1n%>% 
  group_by(`Ward`, `age_cat`) %>% 
  summarise(count = n())

css_sum_h <- css_sum_h[-6, ]
css_sum_h <- css_sum_h[-11,]
css_sum_h <- css_sum_h[-16,]


rdt_sum_hp <- css_hh1n %>% 
  group_by(`Ward`, `Sex..Is..NAME..male.or.female.`, `q302..RESULT`) %>% 
  summarise(count = n())

rdt_sum_hp <- rdt_sum_hp[-5, ]

# # Create a factor with custom labels
# rdt_sum_hp$q302..RESULT <- factor(rdt_sum_hp$q302..RESULT, 
#                                   levels = c(0, 1, 2), labels = c("NEG", "POS", "ND"))
# 
# library(dplyr)
# 
# # Remove rows where ColumnName is equal to "LabelToRemove"
# rdt_sum_hpn <- rdt_sum_hp %>%
#   dplyr::filter(`q302..RESULT`!="ND")

##Overall results by wards
rdt_sum_wps <- css_hh1n%>% 
  group_by(`Ward`, `q302..RESULT`, `Settlement`)%>% 
  summarise(count = n())


library(tidyr)

##Convert data to wide to enable computing proportion

wide_rdt_sum_wp <- rdt_sum_wp %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_wp$TOT <- wide_rdt_sum_wp$NEG + wide_rdt_sum_wp$POS

wide_rdt_sum_wp$PROP_POS <- round(wide_rdt_sum_wp$POS/wide_rdt_sum_wp$TOT * 100, 1)

wide_rdt_sum_wp$PROP_NEG <- round(wide_rdt_sum_wp$NEG/wide_rdt_sum_wp$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_wp_df <- wide_rdt_sum_wp %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
    names_to = "RESULT",
              values_to = "count")

ggplot(rdt_sum_wp_df, aes(fill=RESULT, y=count, x=Ward)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Ward, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "Results of RDTs done per Ward")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

##Results by Settlement
rdt_sum_ss <- css_hh1n%>% 
  group_by(`Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

##Convert data to wide to enable computing proportion
wide_rdt_sum_ss <- rdt_sum_ss %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_ss$TOT <- wide_rdt_sum_ss$NEG + wide_rdt_sum_ss$POS

wide_rdt_sum_ss$PROP_POS <- round(wide_rdt_sum_ss$POS/wide_rdt_sum_ss$TOT * 100, 1)

wide_rdt_sum_ss$PROP_NEG <- round(wide_rdt_sum_ss$NEG/wide_rdt_sum_ss$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_ss_df <- wide_rdt_sum_ss %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ss <- ggplot(rdt_sum_ss, aes(fill=q302..RESULT, y=count, x=Settlement)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = Settlement, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "Results of RDTs done per Settlement")+
  theme_manuscript()+
  #theme(legend.position = c(0.90, 0.90))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS and settlement.pdf'), ss, width = 8, height = 6)

##Results by wards and settlement
rdt_sum_ws <- css_hh1n%>% 
  group_by(`Ward`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

##Convert data to wide to enable computing proportion
wide_rdt_sum_ws <- rdt_sum_ws %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions
wide_rdt_sum_ws$TOT <- wide_rdt_sum_ws$NEG + wide_rdt_sum_ws$POS

wide_rdt_sum_ws$PROP_POS <- round(wide_rdt_sum_ws$POS/wide_rdt_sum_ws$TOT * 100, 1)

wide_rdt_sum_ws$PROP_NEG <- round(wide_rdt_sum_ws$NEG/wide_rdt_sum_ws$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_ws_df <- wide_rdt_sum_ws %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ws <- ggplot(rdt_sum_ws_df, aes(fill=RESULT, y=count, x=Settlement)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Settlement, y= count, label = count), vjust = 0.9, size = 3)+
  facet_grid(~ Ward)+
  labs(title = "Results of RDTs done by Ward and Settlement")+
  theme_manuscript()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS wards and settlement.pdf'), ws, width = 8, height = 6)

#Results by Gender and settlement
rdt_sum_g <- css_hh1n%>% 
  group_by(`Sex..Is..NAME..male.or.female.`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

rdt_sum_g <- rdt_sum_g[-1,]

##Convert data to wide to enable computing proportion
wide_rdt_sum_g <- rdt_sum_g %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_g$TOT <- wide_rdt_sum_g$NEG + wide_rdt_sum_g$POS

wide_rdt_sum_g$PROP_POS <- round(wide_rdt_sum_g$POS/wide_rdt_sum_g$TOT * 100, 1)

wide_rdt_sum_g$PROP_NEG <- round(wide_rdt_sum_g$NEG/wide_rdt_sum_g$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_g_df <- wide_rdt_sum_g %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ggplot(rdt_sum_g_df, aes(fill=RESULT, y=count, x=Sex..Is..NAME..male.or.female.)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Sex..Is..NAME..male.or.female., y= count, label = count), vjust = 0.9, size = 3) +
  facet_grid(~ Settlement)+
  labs(title = "Results of RDTs done by Gender and Settlement")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.95))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))


#Results by Age Category and Settlement
rdt_sum_a <- css_hh1n%>% 
  group_by(`age_cat`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

rdt_sum_a <- rdt_sum_a[-31,]
rdt_sum_a <- rdt_sum_a[-31,]
rdt_sum_a <- rdt_sum_a[-31,]

install.packages("cli")
library(dplyr)
library(data.table)

##Convert data to wide to enable computing proportion
wide_rdt_sum_a <- rdt_sum_a %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_a$TOT <- wide_rdt_sum_a$NEG + wide_rdt_sum_a$POS

wide_rdt_sum_a$PROP_POS <- round(wide_rdt_sum_a$POS/wide_rdt_sum_a$TOT * 100, 1)

wide_rdt_sum_a$PROP_NEG <- round(wide_rdt_sum_a$NEG/wide_rdt_sum_a$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_a_df <- wide_rdt_sum_a %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")


#Selecting per settlement type
rdt_sum_a_f <- rdt_sum_a %>% dplyr::filter(Settlement == "Formal")

asf <- ggplot(rdt_sum_a_f, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Formal settlement.pdf'), asf, width = 8, height = 6)


rdt_sum_a_i <- rdt_sum_a %>% dplyr::filter(Settlement == "Informal")

asi <- ggplot(rdt_sum_a_i, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Informal settlement.pdf'), asi, width = 8, height = 6)

rdt_sum_a_s <- rdt_sum_a %>% dplyr::filter(Settlement == "Slum")

ass <- ggplot(rdt_sum_a_s, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Slum settlement.pdf'), ass, width = 8, height = 6)

library(cowplot)

ppt <- plot_grid(asf, asi, ass, ncol=3)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), ppt, width = 8, height = 6)


ggplot(data, aes(x = X, y = Stacked_Y, color = Category)) +
  geom_point(position = "stack") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Stacked Point Plot") +
  theme_minimal()


##Overall by settlements
as <- ggplot(rdt_sum_a, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  facet_grid(~ Settlement)+
  labs(title = "Results of RDTs done by Age Category and Settlement")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), as, width = 8, height = 6)







library(plyr)

# adding the
data_frame = ddply(rdt_sum_a, .(q302..RESULT), transform,
                   percentage=count/sum(count) * 100)

# adding the percentage label
data_frame$prcntlabel = paste0(sprintf("%.0f",
                                       data_frame$percentage),
                               "%")

library(ggplot2)

# Assuming your dataframe is named "df"
ggplot(df, aes(x = age_cat, fill = q302..RESULT)) +
  geom_bar(position = "fill", stat = "count") +
  geom_text(aes(label = scales::percent(..count..), group = q302..RESULT),
            stat = "count", position = position_fill(vjust = 0.5), size = 4) +
  labs(x = "Age Category", y = "Proportion", fill = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()


library(dplyr)

# Assuming your dataframe is named "df"
# Filter only "POS" rows
positive_df <- css_hh1n %>% filter(q302..RESULT == "POS")

# Group the filtered dataframe by age_cat and calculate proportions
proportions_df <- positive_df %>%
  group_by(age_cat) %>%
  summarise(Proportion_POS = n() / nrow(css_hh1n))

# Print the proportions for each age category

p_df <- proportions_df*100




ggplot(rdt_sum_hp, aes(x = as.factor(Ward), y = count))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5, position = "stack") +
  facet_wrap(~ age_cat)+
  labs(title = "Number of RDTs done per Ward/Age category",
       x = "Ward Visited",
       y = "Number of RDTs")+
  theme_manuscript1()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggplot( css_sum_wwq, aes(x = as.factor(Ward), y = count))+
  geom_bar(position = "stack", stat = "identity", fill = "lightblue", width = 0.5)+
  geom_text(aes(x = Ward, y= count, label = count), vjust = -0.5)+
  facet_wrap(~ Sex..Is..NAME..male.or.female.)+
  labs(title = "Results of RDTs done per Ward/Gender",
       x = "Ward Visited",
       y = "Number of RDTs")+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))






p <- ggplot(data=hbr_indoor, aes(x= `Settlement Classification`, y=HBR, group = `Settlement Classification`,
                                 colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("Formal", "Informal", "Slum")) +
  geom_point() +labs(y= "Human Biting Rate", x = "Settlement Type")+
  geom_line()+
  ggtitle("Indoor Human Biting Rate by settlement type, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


library(ggplot2)

# Sample data
data <- data.frame(
  Settlement = c("Formal", "Formal", "Formal", "Formal", "Formal",
               "Informal", "Informal", "Informal","Informal", "Informal",
               "Slum", "Slum", "Slum", "Slum", "Slum"),
  X = c("Under 5", "6-10", "11-17", "18-30", "31 and above",
        "Under 5", "6-10", "11-17", "18-30", "31 and above",
        "Under 5", "6-10", "11-17", "18-30", "31 and above"),
  Y = c(3.4, 11.8, 4.8, 5.5, 1.1,
        6.8, 15.2, 13.5, 6.5, 5.0,
        19.7, 23.0, 28.1, 15.2,6.4)
)

# Define the desired order of X
x_order <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

# Convert X to a factor with specified levels
data$X <- factor(data$X, levels = x_order)


# Create a stacked point plot
ggplot(data, aes(x = X, y = Y, color = Settlement)) +
  geom_point() +
  xlab <-  ("Unadjusted age in years") +
  ylab <- ("Malaria Prevalence")+
  scale_color_brewer(palette = "Set1") +
  labs(title = "Malaria prevalence by age category per settlement type") +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),  # Custom breaks
    labels = seq(0, 30, by = 5),  # Custom labels
    limits = c(0, 30)  # Custom limits
  ) +
  theme_manuscript()

asp <- ggplot(data, aes(x = X, y = Y, color = Settlement)) +
  geom_point(size = 5.5) +
  xlab("Age in years") +  # Add X-axis label
  ylab("Malaria Prevalence(unweighted)") +  # Add Y-axis label
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "Malaria prevalence by age category per settlement type") +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),  # Custom breaks
    labels = seq(0, 30, by = 5),  # Custom labels
    limits = c(0, 30)  # Custom limits
  ) +
  theme_manuscript()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), asp, width = 8, height = 6)


#Ibadan positivity as at 29th

sdata <- data.frame(
  Settlement = c("Formal", "Formal", "Formal", "Formal",
                 "Informal", "Informal", "Informal","Informal",
                 "Slum", "Slum", "Slum", "Slum"),
  W = c("Agugu", "Basorun", "Challenge", "Olopomewa",
        "Agugu", "Basorun", "Challenge", "Olopomewa",
        "Agugu", "Basorun", "Challenge", "Olopomewa"),
  P = c(0.0, 4.5, 2.7, 4.0, 
        11.2, 9.3, 3.9, 2.6, 
        15.9, 12.9, 0.0, 3.1)
)

# # Define the desired order of X
# x_order <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")
# 
# # Convert X to a factor with specified levels
# data$X <- factor(data$X, levels = x_order)


# # Create a stacked point plot
# ggplot(data, aes(x = X, y = Y, color = Settlement)) +
#   geom_point() +
#   xlab <-  ("Unadjusted age in years") +
#   ylab <- ("Malaria Prevalence")+
#   scale_color_brewer(palette = "Set1") +
#   labs(title = "Malaria prevalence by age category per settlement type") +
#   scale_y_continuous(
#     breaks = seq(0, 30, by = 5),  # Custom breaks
#     labels = seq(0, 30, by = 5),  # Custom labels
#     limits = c(0, 30)  # Custom limits
#   ) +
#   theme_manuscript()

wasp <- ggplot(sdata, aes(x = W, y = P, color = Settlement)) +
  geom_point(size = 5.5) +
  xlab("Wards") +  # Add X-axis label
  ylab("Malaria Prevalence(unweighted)") +  # Add Y-axis label
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "Malaria prevalence by Wards and settlement type(Ibadan) as at 29th October") +
  scale_y_continuous(
    breaks = seq(0, 20, by = 5),  # Custom breaks
    labels = seq(0, 20, by = 5),  # Custom labels
    limits = c(0, 20)  # Custom limits
  ) +
  theme_manuscript()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Ward and settlement.pdf'), asp, width = 8, height = 6)






##Women
w_surv <- read.csv(file.path(NuDPDir, "WomenSur_0828.csv"))

names(w_surv)

wsurv_sum_w <- w_surv%>% 
  group_by(`WARD`) %>% 
  summarise(count = n())

wsurv_sum_w$Gender <- "Female"

range(w_surv$q201a..How.old.were.you.on.your.last.birthday.AGE.AT.LAST.BIRTHDAY..IN.YEARS.)

table(w_surv$WARD, w_surv$q702..RESULT)

##Men
m_surv <- read.csv(file.path(NuDPDir, "MenSur_0828.csv"))

names(m_surv)

msurv_sum_m <- m_surv%>% 
  group_by(`WARD`) %>% 
  summarise(count = n())

msurv_sum_m$Gender <- "Male"

##Combine both Women and Men

ind_surv <- rbind(wsurv_sum_w, msurv_sum_m)


ggplot(ind_surv, aes(x = as.factor(WARD), y = count))+
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5, position = "stack") +
  facet_wrap(~ Gender)+
  labs(title = "Number of Questionnaires Administered per Ward"
       x = "Ward Visited")

colors <- c("Checked" = "green3", "Unchecked" = "red3")

# Create  plot with proportions and colored bars


summary_SET <- summary_SET %>% 
          dplyr::rename('ACT_use' = `q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria..choice.Artemisinin.Combination.Therapy..ACT..`)

summary_SET <- summary_SET %>%
  mutate(ACT_use = ifelse(ACT_use == "Checked", "Yes", ACT_use))

summary_SET <- summary_SET %>%
  mutate(ACT_use = ifelse(ACT_use == "Unchecked", "No", ACT_use))

ggplot(summary_SET, aes(fill=ACT_use, y=Proportion, x=SETTLEMENT.TYPE)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = SETTLEMENT.TYPE, y= Proportion, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "ACT use by settlement type")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

p_net <- ggplot(summary_SET, aes(x = `SETTLEMENT.TYPE`, y = Proportion, fill = `ACT_use`))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c(`Yes` = "palegreen", `No` = "coral3")) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5)+
labs(title = "ACT use for malaria in children by settlement type in households in Ibadan")+
xlab("Settlement Type") +
  ylab("Proportion of women")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.90))

##Boxplot of ACT Use
act_boxpl <- ggplot(data=summary_SET, aes(x=as.factor(SETTLEMENT.TYPE), 
                                          y=Proportion, color= as.factor(SETTLEMENT.TYPE))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Distribution of ACT use clustered by enumeration area per settlement type")+
  labs(x = "settle", fill = "EA",
       y = "act_use")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.90))


# Apply the color scale

# Print the plot
print(p_net)


kn_wm_surv <- kn_wm_surv %>%
  mutate_if(is.character, as.factor)

kn_wm_survc <- kn_wm_surv %>%
  group_by(`Serial.Number`) %>%
  fill(everything())

kn_wm_survc <- kn_wm_surv %>%
    group_by(`Serial.Number`) %>%
    mutate_all(na.locf)

wm_surv_all <- rbind(kn_wm_surv, ib_wm_surv)

View(ib_wm_surv)

table(kn_wm_surv$SETTLEMENT.TYPE, kn_wm_surv$q401..Have.any.of.your.children.been.ill.with.a.fever.in.the.last.2.weeks.)

child_fever <- ib_wm_surv %>%
  dplyr::filter(q401..Have.any.of.your.children.been.ill.with.a.fever.in.the.last.2.weeks. == "Yes")


case_sm <- child_fever %>% 
  group_by(`SETTLEMENT.TYPE`) %>% 
  summarise(count = n())%>%
  drop_na()

  

case_m <- child_fever %>% 
  group_by(`SETTLEMENT.TYPE`, `q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria`) %>% 
  summarise(count = n())


table(child_fever$SETTLEMENT.TYPE, child_fever$q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria)

settle <- c("Formal", "Informal", "Slum", "Formal", "Informal", "Slum")
EA <- c("23", "45", "45", "12", "01", "09")
act_use <- c(34,23,14, 2,7,8)

act_used <- cbind(settle, EA, act_use)
act_used <- data.frame(act_used)

##Boxplot of ACT Use
act_boxpl <- ggplot(data=summary_SET, aes(x=as.factor(SETTLEMENT.TYPE), 
                                                 y=Proportion, color= as.factor(SETTLEMENT.TYPE))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Distribution of ACT use clustered by enumeration area per settlement type")+
  labs(x = "settle", fill = "EA",
       y = "act_use")+
  theme(legend.position = "none")



# Create a sample data frame with groups
set.seed(123)
data <- data.frame(
  Group = rep(c("A", "B", "C"), each = 20),
  Values = rnorm(1000)
)

# Create a boxplot by groups
boxplot(Values ~ Group, data = data, 
        main = "Boxplot by Groups",
        xlab = "Group",
        ylab = "Values",
        col = c("lightblue", "lightgreen", "lightpink"))

ggplot(data, aes(x = Group, y = Values)) +
  geom_boxplot()+
  geom_jitter()+
  labs(title = "Boxplot by Groups", x = "Group", y = "Values",
       col = c("lightblue", "lightgreen", "lightpink"))




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

##Household survey analysis

ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

kn_css_hh <- read.csv(file.path(NuDPDir , "hh_data_kano_3011.csv")) 

##Overall Summary
State <- c("Ibadan", "Kano")

hh_data_report <- data.frame(State,    
                             final_hh_figures = c(3109, 4463),
                             target_hh_figures = c(7000,9625),
                             sampled_hh_figures = c(4598,5184)
)

hh_data_report <- hh_data_report %>% 
  mutate(Percent_Complete_T = round(hh_data_report$final_hh_figures/hh_data_report$target_hh_figures * 100, 1)) %>% 
  mutate(Percent_Complete_S = round(hh_data_report$final_hh_figures/hh_data_report$sampled_hh_figures * 100, 1))


ib_hh_visited <- ggplot(hh_data_report, aes(x = State)) +
  geom_bar(aes(y = final_hh_figures), stat = "identity", fill = "plum")+
  geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of households visited during the wet season household survey in \n Ibadan and Kano showing the protocol targets(red), percent sampled(green)", size = 8,
       x = "State",
       y = "Total number of households") +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))




##Daily Monitoring for Wet season Mop up
#Kano
kn_unique_hhss <- unique(kn_css_hh$Serial.Number)

kn_css_sum_d <- kn_css_hh %>% 
  group_by(`Date`) %>% 
  summarise(count = n())

View(kn_css_sum_d)

# Extract rows for November
nov_kn_css_hh <- kn_css_sum_d[c(38,39,40,41,42,43,44,45), ]

ggplot(data = nov_kn_css_hh , aes(x = Date, y = count)) +
  geom_line()+
  geom_point(size = 5.5, col = "blue")+
  #map_theme()+ 
  theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households interviewd per day in Kano")

## Settlement Type
kn_css_sum_s <- kn_css_hh %>% 
  group_by(`Settlement.Type`) %>% 
  summarise(count = n())

s_kn_css_sum <- kn_css_sum_s[c(2,6,8), ]

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


#Ibadan
ib_unique_hhss <- unique(ib_css_hh$Serial.Number)

ib_css_sum_d <- ib_css_hh %>% 
  group_by(`Date`) %>% 
  summarise(count = n())

View(ib_css_sum_d)

# Extract rows for November
nov_ib_css_hh <- ib_css_sum_d[c(33,36,37,38,39,40,41,42,43,44,45), ]

ggplot(data = nov_ib_css_hh , aes(x = Date, y = count)) +
  geom_line()+
  geom_point(size = 5.5, col = "red")+
  #map_theme()+ 
  theme_manuscript()+
  ylab("Number of Households captured daily")+
  xlab("Date of Visit")+
  labs(title= "Number of households interviewd per day in Ibadan")


## Settlement Type
kn_css_sum_s <- kn_css_hh %>% 
  group_by(`Settlement.Type`) %>% 
  summarise(count = n())






library(ggplot2)
library(ggrepel)  # Make sure ggrepel is installed

ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = ib_0_10ch_df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],
                                      size = 0.2, alpha = 0.7, color = as.factor(Settlement.Type))) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2", Slum = "#923159")) +
  geom_text_repel(data = df_ib, aes(label = WardName, x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
                  color = 'black', size = 2.5, force = 1, min.segment.length = 0, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE) +
  labs(title = "Wards in Ibadan showing HH Listed for 0-10 year children") +
  coord_sf() +
  theme_minimal()

##Men and Women analysis
hh_data_mw <- data.frame(State,    
                             men = c(1448, 3442),
                             women = c(2405,3985)
)

w_hh_sampled <- ggplot(hh_data_mw, aes(x = State)) +
  geom_bar(aes(y = women), stat = "identity", fill = "plum")+
  #geom_bar(aes(y = women), stat = "identity", fill = "khaki")+
  # geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  # geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  # scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  # geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  # geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  # geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of women interviewed during the wet season household survey in Ibadan and Kano", size = 8,
       x = "State",
       y = "Total number of women respondents") +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))


m_hh_sampled <- ggplot(hh_data_mw, aes(x = State)) +
  #geom_bar(aes(y = men), stat = "identity", fill = "plum")+
  geom_bar(aes(y = women), stat = "identity", fill = "khaki")+
  # geom_line(aes(y = target_hh_figures, group = 1, color = "Protocol Target"), size = 1.5)+
  # geom_line(aes(y = sampled_hh_figures, group = 1, color = "Percent Sampled"), size = 1.5) +
  # scale_color_manual(values = c("Protocol Target" = "red", "Percent Sampled" = "green"))+
  # geom_text(aes(label = target_hh_figures, y = target_hh_figures, hjust = 0.9, vjust = - 0.5))+
  # geom_text(aes(label = final_hh_figures, y = final_hh_figures, hjust = 0.9, vjust = 0))+
  # geom_text(aes(label = sampled_hh_figures, y = sampled_hh_figures, hjust = 0.9, vjust = - 0.5))+
  labs(title = "Total number of men interviewed during the wet season household survey in Ibadan and Kano")+
       x = "State"
       y = "Total number of men interviewd in households" +
  theme_manuscript()+
  guides(color = guide_legend(title = NULL))

# kn_css_hh_new <- kn_css_hh %>% 
#   select(Serial.Number, LOCAL.GOVT..AREA, Ward,
#          Settlement.Type, Enumeration.Area.Cluster.Number,
#          HOUSEHOLD.COORDINATE..Longitude, 
#          HOUSEHOLD.COORDINATE..Latitude, whatever)
#   group_by(Serial.Number, LOCAL.GOVT..AREA, Ward,
#           Settlement.Type, Enumeration.Area.Cluster.Number,
#            HOUSEHOLD.COORDINATE..Longitude, 
#            HOUSEHOLD.COORDINATE..Latitude) %>% 
#   summerize(total = n())
#   uncount(total) %>% 



# mutate(Serial_Number = ifelse(Serial.Number == "", Serial.Number[1], Serial.Number),
#        lga = ifelse(LOCAL.GOVT..AREA == "", LOCAL.GOVT..AREA[1], LOCAL.GOVT..AREA))


css_sum_wwq <- css_hh %>% 
  group_by(`Ward`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

names(kn_css_hh)

table(css_hh$Ward)


##Number of HHs
unique_hhss <- unique(css_hh$Serial.Number)

css_sum_w <- css_hh %>% 
  group_by(`Ward`) %>% 
  summarise(count = n())

css_sum_wwq <- css_hh %>% 
  group_by(`Ward`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())


##RDT Tested in Househoulds

# rdt_hh <- read.csv(file.path(NuDPDir, "rdt_hh.csv"))
# 
# rdt_hh <- data.frame(rdt_hh)
# 
# view(rdt_hh)

break_pts <- c(0, 5, 10, 17, 30, 100)

labels <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

age_cat <- cut(css_hh$Age, breaks = break_pts, labels = labels, na.rm = TRUE)

age_cat1 <- data.frame(age_cat)

css_hh1 <- cbind(css_hh, age_cat1)

css_hh1 <- css_hh1 %>%
  mutate(q302..RESULT = case_when(
    q302..RESULT == "POSITIVE" ~ 1,
    q302..RESULT == "NEGATIVE" ~ 0,
    TRUE ~ 2  # For all other cases, including empty or blank values
  ))

# Create a factor with custom labels
css_hh1$q302..RESULT <- factor(css_hh1$q302..RESULT, 
                                  levels = c(0, 1, 2), labels = c("NEG", "POS", "ND"))

library(dplyr)

# Remove rows where ColumnName is equal to "LabelToRemove"
css_hh1n <- css_hh1 %>%
  dplyr::filter(`q302..RESULT`!="ND")


css_sum_h <- css_hh1n%>% 
  group_by(`Ward`, `age_cat`) %>% 
  summarise(count = n())

css_sum_h <- css_sum_h[-6, ]
css_sum_h <- css_sum_h[-11,]
css_sum_h <- css_sum_h[-16,]


rdt_sum_hp <- css_hh1n %>% 
  group_by(`Ward`, `Sex..Is..NAME..male.or.female.`, `q302..RESULT`) %>% 
  summarise(count = n())

rdt_sum_hp <- rdt_sum_hp[-5, ]

# # Create a factor with custom labels
# rdt_sum_hp$q302..RESULT <- factor(rdt_sum_hp$q302..RESULT, 
#                                   levels = c(0, 1, 2), labels = c("NEG", "POS", "ND"))
# 
# library(dplyr)
# 
# # Remove rows where ColumnName is equal to "LabelToRemove"
# rdt_sum_hpn <- rdt_sum_hp %>%
#   dplyr::filter(`q302..RESULT`!="ND")

##Overall results by wards
rdt_sum_wps <- css_hh1n%>% 
  group_by(`Ward`, `q302..RESULT`, `Settlement`)%>% 
  summarise(count = n())


library(tidyr)

##Convert data to wide to enable computing proportion

wide_rdt_sum_wp <- rdt_sum_wp %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_wp$TOT <- wide_rdt_sum_wp$NEG + wide_rdt_sum_wp$POS

wide_rdt_sum_wp$PROP_POS <- round(wide_rdt_sum_wp$POS/wide_rdt_sum_wp$TOT * 100, 1)

wide_rdt_sum_wp$PROP_NEG <- round(wide_rdt_sum_wp$NEG/wide_rdt_sum_wp$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_wp_df <- wide_rdt_sum_wp %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
    names_to = "RESULT",
              values_to = "count")

ggplot(rdt_sum_wp_df, aes(fill=RESULT, y=count, x=Ward)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Ward, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "Results of RDTs done per Ward")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

##Results by Settlement
rdt_sum_ss <- css_hh1n%>% 
  group_by(`Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

##Convert data to wide to enable computing proportion
wide_rdt_sum_ss <- rdt_sum_ss %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_ss$TOT <- wide_rdt_sum_ss$NEG + wide_rdt_sum_ss$POS

wide_rdt_sum_ss$PROP_POS <- round(wide_rdt_sum_ss$POS/wide_rdt_sum_ss$TOT * 100, 1)

wide_rdt_sum_ss$PROP_NEG <- round(wide_rdt_sum_ss$NEG/wide_rdt_sum_ss$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_ss_df <- wide_rdt_sum_ss %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ss <- ggplot(rdt_sum_ss, aes(fill=q302..RESULT, y=count, x=Settlement)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = Settlement, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "Results of RDTs done per Settlement")+
  theme_manuscript()+
  #theme(legend.position = c(0.90, 0.90))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS and settlement.pdf'), ss, width = 8, height = 6)

##Results by wards and settlement
rdt_sum_ws <- css_hh1n%>% 
  group_by(`Ward`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

##Convert data to wide to enable computing proportion
wide_rdt_sum_ws <- rdt_sum_ws %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions
wide_rdt_sum_ws$TOT <- wide_rdt_sum_ws$NEG + wide_rdt_sum_ws$POS

wide_rdt_sum_ws$PROP_POS <- round(wide_rdt_sum_ws$POS/wide_rdt_sum_ws$TOT * 100, 1)

wide_rdt_sum_ws$PROP_NEG <- round(wide_rdt_sum_ws$NEG/wide_rdt_sum_ws$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_ws_df <- wide_rdt_sum_ws %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ws <- ggplot(rdt_sum_ws_df, aes(fill=RESULT, y=count, x=Settlement)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Settlement, y= count, label = count), vjust = 0.9, size = 3)+
  facet_grid(~ Ward)+
  labs(title = "Results of RDTs done by Ward and Settlement")+
  theme_manuscript()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS wards and settlement.pdf'), ws, width = 8, height = 6)

#Results by Gender and settlement
rdt_sum_g <- css_hh1n%>% 
  group_by(`Sex..Is..NAME..male.or.female.`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

rdt_sum_g <- rdt_sum_g[-1,]

##Convert data to wide to enable computing proportion
wide_rdt_sum_g <- rdt_sum_g %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_g$TOT <- wide_rdt_sum_g$NEG + wide_rdt_sum_g$POS

wide_rdt_sum_g$PROP_POS <- round(wide_rdt_sum_g$POS/wide_rdt_sum_g$TOT * 100, 1)

wide_rdt_sum_g$PROP_NEG <- round(wide_rdt_sum_g$NEG/wide_rdt_sum_g$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_g_df <- wide_rdt_sum_g %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ggplot(rdt_sum_g_df, aes(fill=RESULT, y=count, x=Sex..Is..NAME..male.or.female.)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = Sex..Is..NAME..male.or.female., y= count, label = count), vjust = 0.9, size = 3) +
  facet_grid(~ Settlement)+
  labs(title = "Results of RDTs done by Gender and Settlement")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.95))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))


#Results by Age Category and Settlement
rdt_sum_a <- css_hh1n%>% 
  group_by(`age_cat`, `Settlement`, `q302..RESULT`) %>% 
  dplyr::summarise(count = n())

rdt_sum_a <- rdt_sum_a[-31,]
rdt_sum_a <- rdt_sum_a[-31,]
rdt_sum_a <- rdt_sum_a[-31,]

install.packages("cli")
library(dplyr)
library(data.table)

##Convert data to wide to enable computing proportion
wide_rdt_sum_a <- rdt_sum_a %>%
  pivot_wider(names_from = q302..RESULT,
              values_from = count)

##Compute Proportions

wide_rdt_sum_a$TOT <- wide_rdt_sum_a$NEG + wide_rdt_sum_a$POS

wide_rdt_sum_a$PROP_POS <- round(wide_rdt_sum_a$POS/wide_rdt_sum_a$TOT * 100, 1)

wide_rdt_sum_a$PROP_NEG <- round(wide_rdt_sum_a$NEG/wide_rdt_sum_a$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
rdt_sum_a_df <- wide_rdt_sum_a %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")


#Selecting per settlement type
rdt_sum_a_f <- rdt_sum_a %>% dplyr::filter(Settlement == "Formal")

asf <- ggplot(rdt_sum_a_f, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Formal settlement.pdf'), asf, width = 8, height = 6)


rdt_sum_a_i <- rdt_sum_a %>% dplyr::filter(Settlement == "Informal")

asi <- ggplot(rdt_sum_a_i, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Informal settlement.pdf'), asi, width = 8, height = 6)

rdt_sum_a_s <- rdt_sum_a %>% dplyr::filter(Settlement == "Slum")

ass <- ggplot(rdt_sum_a_s, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Settlement)+
  #labs(title = "Results of RDTs done by Age Category and Settlement")+
  guides(fill = "none")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and Slum settlement.pdf'), ass, width = 8, height = 6)

library(cowplot)

ppt <- plot_grid(asf, asi, ass, ncol=3)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), ppt, width = 8, height = 6)


ggplot(data, aes(x = X, y = Stacked_Y, color = Category)) +
  geom_point(position = "stack") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Stacked Point Plot") +
  theme_minimal()


##Overall by settlements
as <- ggplot(rdt_sum_a, aes(fill=q302..RESULT, y=count, x=age_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`NEG` = "#FED2C7", `POS` = "#DD80AD")) +
  geom_text(aes(x = age_cat, y= count, label = count), vjust = 0.9, size = 3) +
  facet_grid(~ Settlement)+
  labs(title = "Results of RDTs done by Age Category and Settlement")+
  theme_manuscript1()+
  #theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), as, width = 8, height = 6)







library(plyr)

# adding the
data_frame = ddply(rdt_sum_a, .(q302..RESULT), transform,
                   percentage=count/sum(count) * 100)

# adding the percentage label
data_frame$prcntlabel = paste0(sprintf("%.0f",
                                       data_frame$percentage),
                               "%")

library(ggplot2)

# Assuming your dataframe is named "df"
ggplot(df, aes(x = age_cat, fill = q302..RESULT)) +
  geom_bar(position = "fill", stat = "count") +
  geom_text(aes(label = scales::percent(..count..), group = q302..RESULT),
            stat = "count", position = position_fill(vjust = 0.5), size = 4) +
  labs(x = "Age Category", y = "Proportion", fill = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()


library(dplyr)

# Assuming your dataframe is named "df"
# Filter only "POS" rows
positive_df <- css_hh1n %>% filter(q302..RESULT == "POS")

# Group the filtered dataframe by age_cat and calculate proportions
proportions_df <- positive_df %>%
  group_by(age_cat) %>%
  summarise(Proportion_POS = n() / nrow(css_hh1n))

# Print the proportions for each age category

p_df <- proportions_df*100




ggplot(rdt_sum_hp, aes(x = as.factor(Ward), y = count))+
  geom_bar(stat = "identity", fill = "orange", width = 0.5, position = "stack") +
  facet_wrap(~ age_cat)+
  labs(title = "Number of RDTs done per Ward/Age category",
       x = "Ward Visited",
       y = "Number of RDTs")+
  theme_manuscript1()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggplot( css_sum_wwq, aes(x = as.factor(Ward), y = count))+
  geom_bar(position = "stack", stat = "identity", fill = "lightblue", width = 0.5)+
  geom_text(aes(x = Ward, y= count, label = count), vjust = -0.5)+
  facet_wrap(~ Sex..Is..NAME..male.or.female.)+
  labs(title = "Results of RDTs done per Ward/Gender",
       x = "Ward Visited",
       y = "Number of RDTs")+
  theme_manuscript()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))






p <- ggplot(data=hbr_indoor, aes(x= `Settlement Classification`, y=HBR, group = `Settlement Classification`,
                                 colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("Formal", "Informal", "Slum")) +
  geom_point() +labs(y= "Human Biting Rate", x = "Settlement Type")+
  geom_line()+
  ggtitle("Indoor Human Biting Rate by settlement type, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


library(ggplot2)

# Sample data
data <- data.frame(
  Settlement = c("Formal", "Formal", "Formal", "Formal", "Formal",
               "Informal", "Informal", "Informal","Informal", "Informal",
               "Slum", "Slum", "Slum", "Slum", "Slum"),
  X = c("Under 5", "6-10", "11-17", "18-30", "31 and above",
        "Under 5", "6-10", "11-17", "18-30", "31 and above",
        "Under 5", "6-10", "11-17", "18-30", "31 and above"),
  Y = c(3.4, 11.8, 4.8, 5.5, 1.1,
        6.8, 15.2, 13.5, 6.5, 5.0,
        19.7, 23.0, 28.1, 15.2,6.4)
)

# Define the desired order of X
x_order <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")

# Convert X to a factor with specified levels
data$X <- factor(data$X, levels = x_order)


# Create a stacked point plot
ggplot(data, aes(x = X, y = Y, color = Settlement)) +
  geom_point() +
  xlab <-  ("Unadjusted age in years") +
  ylab <- ("Malaria Prevalence")+
  scale_color_brewer(palette = "Set1") +
  labs(title = "Malaria prevalence by age category per settlement type") +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),  # Custom breaks
    labels = seq(0, 30, by = 5),  # Custom labels
    limits = c(0, 30)  # Custom limits
  ) +
  theme_manuscript()

asp <- ggplot(data, aes(x = X, y = Y, color = Settlement)) +
  geom_point(size = 5.5) +
  xlab("Age in years") +  # Add X-axis label
  ylab("Malaria Prevalence(unweighted)") +  # Add Y-axis label
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "Malaria prevalence by age category per settlement type") +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),  # Custom breaks
    labels = seq(0, 30, by = 5),  # Custom labels
    limits = c(0, 30)  # Custom limits
  ) +
  theme_manuscript()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Age and settlement.pdf'), asp, width = 8, height = 6)


#Ibadan positivity as at 29th

sdata <- data.frame(
  Settlement = c("Formal", "Formal", "Formal", "Formal",
                 "Informal", "Informal", "Informal","Informal",
                 "Slum", "Slum", "Slum", "Slum"),
  W = c("Agugu", "Basorun", "Challenge", "Olopomewa",
        "Agugu", "Basorun", "Challenge", "Olopomewa",
        "Agugu", "Basorun", "Challenge", "Olopomewa"),
  P = c(0.0, 4.5, 2.7, 4.0, 
        11.2, 9.3, 3.9, 2.6, 
        15.9, 12.9, 0.0, 3.1)
)

# # Define the desired order of X
# x_order <- c("Under 5", "6-10", "11-17", "18-30", "31 and above")
# 
# # Convert X to a factor with specified levels
# data$X <- factor(data$X, levels = x_order)


# # Create a stacked point plot
# ggplot(data, aes(x = X, y = Y, color = Settlement)) +
#   geom_point() +
#   xlab <-  ("Unadjusted age in years") +
#   ylab <- ("Malaria Prevalence")+
#   scale_color_brewer(palette = "Set1") +
#   labs(title = "Malaria prevalence by age category per settlement type") +
#   scale_y_continuous(
#     breaks = seq(0, 30, by = 5),  # Custom breaks
#     labels = seq(0, 30, by = 5),  # Custom labels
#     limits = c(0, 30)  # Custom limits
#   ) +
#   theme_manuscript()

wasp <- ggplot(sdata, aes(x = W, y = P, color = Settlement)) +
  geom_point(size = 5.5) +
  xlab("Wards") +  # Add X-axis label
  ylab("Malaria Prevalence(unweighted)") +  # Add Y-axis label
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "Malaria prevalence by Wards and settlement type(Ibadan) as at 29th October") +
  scale_y_continuous(
    breaks = seq(0, 20, by = 5),  # Custom breaks
    labels = seq(0, 20, by = 5),  # Custom labels
    limits = c(0, 20)  # Custom limits
  ) +
  theme_manuscript()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS Ward and settlement.pdf'), asp, width = 8, height = 6)






##Women
w_surv <- read.csv(file.path(NuDPDir, "WomenSur_0828.csv"))

names(w_surv)

wsurv_sum_w <- w_surv%>% 
  group_by(`WARD`) %>% 
  summarise(count = n())

wsurv_sum_w$Gender <- "Female"

range(w_surv$q201a..How.old.were.you.on.your.last.birthday.AGE.AT.LAST.BIRTHDAY..IN.YEARS.)

table(w_surv$WARD, w_surv$q702..RESULT)

##Men
m_surv <- read.csv(file.path(NuDPDir, "MenSur_0828.csv"))

names(m_surv)

msurv_sum_m <- m_surv%>% 
  group_by(`WARD`) %>% 
  summarise(count = n())

msurv_sum_m$Gender <- "Male"

##Combine both Women and Men

ind_surv <- rbind(wsurv_sum_w, msurv_sum_m)


ggplot(ind_surv, aes(x = as.factor(WARD), y = count))+
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5, position = "stack") +
  facet_wrap(~ Gender)+
  labs(title = "Number of Questionnaires Administered per Ward",
       x = "Ward Visited")

colors <- c("Checked" = "green3", "Unchecked" = "red3")

# Create  plot with proportions and colored bars


summary_SET <- summary_SET %>% 
          dplyr::rename('ACT_use' = `q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria..choice.Artemisinin.Combination.Therapy..ACT..`)

summary_SET <- summary_SET %>%
  mutate(ACT_use = ifelse(ACT_use == "Checked", "Yes", ACT_use))

summary_SET <- summary_SET %>%
  mutate(ACT_use = ifelse(ACT_use == "Unchecked", "No", ACT_use))

ggplot(summary_SET, aes(fill=ACT_use, y=Proportion, x=SETTLEMENT.TYPE)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = SETTLEMENT.TYPE, y= Proportion, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "ACT use by settlement type")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

p_net <- ggplot(summary_SET, aes(x = `SETTLEMENT.TYPE`, y = Proportion, fill = `ACT_use`))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c(`Yes` = "palegreen", `No` = "coral3")) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5)+
labs(title = "ACT use for malaria in children by settlement type in households in Ibadan")+
xlab("Settlement Type") +
  ylab("Proportion of women")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.90))

##Boxplot of ACT Use
act_boxpl <- ggplot(data=summary_SET, aes(x=as.factor(SETTLEMENT.TYPE), 
                                          y=Proportion, color= as.factor(SETTLEMENT.TYPE))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Distribution of ACT use clustered by enumeration area per settlement type")+
  labs(x = "settle", fill = "EA",
       y = "act_use")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.90))


# Apply the color scale

# Print the plot
print(p_net)


kn_wm_surv <- kn_wm_surv %>%
  mutate_if(is.character, as.factor)

kn_wm_survc <- kn_wm_surv %>%
  group_by(`Serial.Number`) %>%
  fill(everything())

kn_wm_survc <- kn_wm_surv %>%
    group_by(`Serial.Number`) %>%
    mutate_all(na.locf)

wm_surv_all <- rbind(kn_wm_surv, ib_wm_surv)

View(ib_wm_surv)

table(kn_wm_surv$SETTLEMENT.TYPE, kn_wm_surv$q401..Have.any.of.your.children.been.ill.with.a.fever.in.the.last.2.weeks.)

child_fever <- ib_wm_surv %>%
  dplyr::filter(q401..Have.any.of.your.children.been.ill.with.a.fever.in.the.last.2.weeks. == "Yes")


case_sm <- child_fever %>% 
  group_by(`SETTLEMENT.TYPE`) %>% 
  summarise(count = n())%>%
  drop_na()

  

case_m <- child_fever %>% 
  group_by(`SETTLEMENT.TYPE`, `q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria`) %>% 
  summarise(count = n())


table(child_fever$SETTLEMENT.TYPE, child_fever$q415..What.drugs.did..NAME..take.for.the.last.episode.of.fever.or.suspected.diagnosed.malaria....DO.NOT.READ.OUT.OPTIONS..MULTIPLE.RESPONSE.ALLOWED..PROBE.FOR.ALL.DRUGS.USED.BY..NAME...Repeat.for.every.child.who.had.a.fever.or.suspected.diagnosed.malaria)

settle <- c("Formal", "Informal", "Slum", "Formal", "Informal", "Slum")
EA <- c("23", "45", "45", "12", "01", "09")
act_use <- c(34,23,14, 2,7,8)

act_used <- cbind(settle, EA, act_use)
act_used <- data.frame(act_used)

##Boxplot of ACT Use
act_boxpl <- ggplot(data=summary_SET, aes(x=as.factor(SETTLEMENT.TYPE), 
                                                 y=Proportion, color= as.factor(SETTLEMENT.TYPE))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Distribution of ACT use clustered by enumeration area per settlement type")+
  labs(x = "settle", fill = "EA",
       y = "act_use")+
  theme(legend.position = "none")



# Create a sample data frame with groups
set.seed(123)
data <- data.frame(
  Group = rep(c("A", "B", "C"), each = 20),
  Values = rnorm(1000)
)

# Create a boxplot by groups
boxplot(Values ~ Group, data = data, 
        main = "Boxplot by Groups",
        xlab = "Group",
        ylab = "Values",
        col = c("lightblue", "lightgreen", "lightpink"))

ggplot(data, aes(x = Group, y = Values)) +
  geom_boxplot()+
  geom_jitter()+
  labs(title = "Boxplot by Groups", x = "Group", y = "Values",
       col = c("lightblue", "lightgreen", "lightpink"))

