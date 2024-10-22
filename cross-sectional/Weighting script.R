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


library(tidyverse)

###Household Listed
ib_hh_listed <- read.csv(file.path(NuDPDir , "hh_listed_final.csv"))

###Household Sampled
ib_hh_sampled <- read.csv(file.path(NuDPDir , "Ib_sampled_list_final.csv"))


##Ibadan
##HH structure List and number of HHs per structure
#ag_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Agugu")

ib_hh_list <- ib_hh_listed %>%
  group_by(Enumeration_Area, `Settlement`, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, `Settlement`,X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>% 
  ungroup()

View(ib_hh_list)

##Total Number of HHs listed per EA
ib_hh_list_s <- ib_hh_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

View(ib_hh_list_s)

##Sampled

#ib_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

##Wrangling to generate number of HHs per structure in sampled list
ib_ea_sampled <- ib_hh_sampled %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()

View(ib_ea_sampled)

##Generate number of HHs sampled per EA
ib_ea_sampled_s <- ib_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh3 = sum(tot_hh2)) %>%
  ungroup()

View(ib_ea_sampled_s)



# ag_ea_sampled <- ag_hh_sampled %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
#   count() %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure) %>%
#   summarise(tot_hh2 = sum(n)) %>%
#   ungroup()

#View(ag_ea_sampled)

##Combine listed and sampled data
# install.packages("devtools")
# library(devtools)
# devtools::install_github("cran/rowr")

library(rowr)
library(tidyr)

##Probability of selecting HHs within EAs(Stage 2)

colnames(ib_ea_sampled_s) [1] <- ("enumeration_area")

ib_hhs_comb <- cbind.fill(ib_hh_list_s, ib_ea_sampled_s, by = "Enumeration_Area")

View(ib_hhs_comb)

ib_hh_prob <- ib_hhs_comb %>% 
  dplyr::mutate(pr_hhs = tot_hh3/tot_hh2)

View(ib_hh_prob)
##Stage 3
##Probability by individual HHs
colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

colnames(aa_ea_comb) [5] <- "Enumeration_Area1"

colnames(aa_ea_comb) [8] <- "tot_hh_s"


##Probability of HHs Sampled per structure
head(aa_ea_comb)

#aa_ea_comb_p <- aa_ea_comb %>% dplyr::select (`Enumeration_Area`, `tot_hh`, `tot_hh_s`)

aa_ea_prob <- aa_ea_comb %>% 
  dplyr::mutate(pr_hh = tot_hh_s/tot_hh)

View(aa_ea_prob)

##Write csv to desktop for exploring
if (!file.exists("NuDPDir")) {
  dir.create("NuDPDir")
}

write.csv(aa_ea_prob, file.path("C:/Users/DELL/Desktop", "prob_hh_hs.csv"))


##Read in CSS data for individual probability of testing
ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

##Wrangling of data for individuals tested per households

##Extract required columns
ib_css_hh_w <- dplyr::select(ib_css_hh, Serial.Number, 
                             Repeat.Instrument,            
                             Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                             LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                             Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                             Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                             Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                             Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                             Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                             Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                             Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                             HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                             HOUSEHOLD.COORDINATE..Latitude,
                             q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                             q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                             q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                             q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                             q304..DBS.CODE)

ib_css_hh_df <- rename(ib_css_hh_w, 
                       SN = Serial.Number, 
                       Repeat_Instrument = Repeat.Instrument,            
                       Repeat_Instrance = Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       LGA = LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Ward = Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                       Formal = Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                       Informal = Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                       Slum = Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                       Community_Name = Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       EA_Clust_No = Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       HH_n = Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Longitude = HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       Latitude = HOUSEHOLD.COORDINATE..Latitude,
                       Line_No = q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       RDT_CONSENT = q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                       RESULT = q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                       DBS = q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       DBS_CODE = q304..DBS.CODE)

##Fill necessary cells
library(dplyr)

# Replicate values to all rows with the same serial number
ib_css_hh_dfn <- ib_css_hh_df %>%
  group_by(SN) %>%
  mutate(LGA = first(LGA),
         Ward = first(Ward),
         EA_Clust_No = first(EA_Clust_No),
         HH_n = first(HH_n),
         Longitude = first(Longitude),
         Latitude = first(Latitude)) %>%
  ungroup()

ib_hh_list_frame <- ib_css_hh_dfn %>% dplyr::filter(Repeat_Instrument == 'Household List')

ib_hh_list_frame_sum <- ib_hh_list_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n()) %>% 
  ungroup()

row_to_delete <- 1
ib_hh_list_frame_sum <- ib_hh_list_frame_sum [-row_to_delete, ]
# %>% 
#   ungroup()

ib_hh_test_frame <- ib_css_hh_dfn %>% 
  dplyr::filter(Repeat_Instrument == 'Section 3 Malaria Screening')

ib_hh_test_frame_sum <- ib_hh_test_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n())

row_to_delete <- 1
ib_hh_test_frame_sum <- ib_hh_test_frame_sum [-row_to_delete, ]

#ib_final_list <- cbind(ib_hh_list_frame_sum, ib_hh_test_frame_sum)

ib_final_list_df <- merge(ib_hh_list_frame_sum, ib_hh_test_frame_sum, by = "SN", all = FALSE)

ib_final_list_prop_df <- ib_final_list_df %>% 
  dplyr::mutate(pr_ind = Count.y/Count.x)




##Agugu
##HH structure List and number of HHs per structure
ag_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Agugu")

ag_ea_list <- ag_hh_listed %>%
  group_by(Enumeration_Area, `Settlement`, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, `Settlement`,X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>% 
  ungroup()

View(ag_ea_list)

##Total Number of HHs listed per EA
ag_ea_list_s <- ag_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

View(ag_ea_list_s)

##Sampled

ag_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

##Wrangling to generate number of HHs per structure in sampled list
ag_ea_sampled <- ag_hh_sampled %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()


View(ag_ea_sampled)

# ag_ea_sampled <- ag_hh_sampled %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
#   count() %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure) %>%
#   summarise(tot_hh2 = sum(n)) %>%
#   ungroup()

View(ag_ea_sampled)

##Generate number of HHs sampled per EA
ag_ea_sampled_s <- ag_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh3 = sum(tot_hh2)) %>%
  ungroup()

View(ag_ea_sampled_s)

##Combine listed and sampled data
install.packages("devtools")
library(devtools)
devtools::install_github("cran/rowr")

library(rowr)
library(tidyr)

##Probability of selecting HHs within EAs(Stage 2)

colnames(ag_ea_sampled_s) [1] <- ("enumeration_area")

aa_hhs_comb <- cbind.fill(ag_ea_list_s, ag_ea_sampled_s, by = "Enumeration_Area")

aa_hh_prob <- aa_hhs_comb %>% 
  dplyr::mutate(pr_hhs = tot_hh3/tot_hh2)


##Probability by individual HHs
colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

colnames(aa_ea_comb) [5] <- "Enumeration_Area1"

colnames(aa_ea_comb) [8] <- "tot_hh_s"


##Probability of HHs Sampled per structure
head(aa_ea_comb)

#aa_ea_comb_p <- aa_ea_comb %>% dplyr::select (`Enumeration_Area`, `tot_hh`, `tot_hh_s`)

aa_ea_prob <- aa_ea_comb %>% 
  dplyr::mutate(pr_hh = tot_hh_s/tot_hh)

View(aa_ea_prob)

##Write csv to desktop for exploring
if (!file.exists("NuDPDir")) {
  dir.create("NuDPDir")
}

write.csv(aa_ea_prob, file.path("C:/Users/DELL/Desktop", "prob_hh_hs.csv"))


##Read in CSS data for individual probability of testing
ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

##Wrangling of data for individuals tested per households

##Extract required columns
ib_css_hh_w <- dplyr::select(ib_css_hh, Serial.Number, 
                           Repeat.Instrument,            
                           Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                           LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                           Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                           Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                           Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                           Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                           Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                           Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                           Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                           HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                           HOUSEHOLD.COORDINATE..Latitude,
                           q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                           q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                           q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                           q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                           q304..DBS.CODE)

ib_css_hh_df <- rename(ib_css_hh_w, 
                       SN = Serial.Number, 
                       Repeat_Instrument = Repeat.Instrument,            
                       Repeat_Instrance = Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       LGA = LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Ward = Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                       Formal = Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                       Informal = Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                       Slum = Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                       Community_Name = Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       EA_Clust_No = Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       HH_n = Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Longitude = HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       Latitude = HOUSEHOLD.COORDINATE..Latitude,
                       Line_No = q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       RDT_CONSENT = q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                       RESULT = q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                       DBS = q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       DBS_CODE = q304..DBS.CODE)

##Fill necessary cells
library(dplyr)

# Replicate values to all rows with the same serial number
ib_css_hh_dfn <- ib_css_hh_df %>%
  group_by(SN) %>%
  mutate(LGA = first(LGA),
         Ward = first(Ward),
         EA_Clust_No = first(EA_Clust_No),
         HH_n = first(HH_n),
         Longitude = first(Longitude),
         Latitude = first(Latitude)) %>%
  ungroup()

ib_hh_list_frame <- ib_css_hh_dfn %>% dplyr::filter(Repeat_Instrument == 'Household List')

ib_hh_list_frame_sum <- ib_hh_list_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n()) %>% 
  ungroup()

row_to_delete <- 1
ib_hh_list_frame_sum <- ib_hh_list_frame_sum [-row_to_delete, ]
# %>% 
#   ungroup()

ib_hh_test_frame <- ib_css_hh_dfn %>% 
  dplyr::filter(Repeat_Instrument == 'Section 3 Malaria Screening')

ib_hh_test_frame_sum <- ib_hh_test_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n())

row_to_delete <- 1
ib_hh_test_frame_sum <- ib_hh_test_frame_sum [-row_to_delete, ]

#ib_final_list <- cbind(ib_hh_list_frame_sum, ib_hh_test_frame_sum)

ib_final_list_df <- merge(ib_hh_list_frame_sum, ib_hh_test_frame_sum, by = "SN", all = FALSE)

ib_final_list_prop_df <- ib_final_list_df %>% 
  dplyr::mutate(pr_ind = Count.y/Count.x)



library(dplyr)

result <- ib_css_hh %>%
  group_by(Serial.Number, Line.Number) %>%
  summarise(Count = n()) %>%
  group_by(Serial.Number) %>%
  summarise(TotalCount = n())


df1 <- ib_css_hh %>%
  mutate_if(is.character, as.factor)

df_filled3 <- df1 %>%
  group_by(`Serial.Number`) %>%
  fill(everything())

# Assuming your data frame is named 'your_data' and the column is 'your_column'
library(dplyr)
df1a <- df1 %>% filter (Ward == "Agugu")

your_data <- df1a %>%
  mutate(Ward = ifelse(Ward == "", "Agugu", Ward))





##Bashorun

ba_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Bashorun")

ba_ea_list <- ba_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_list)

ba_ea_list_s <- ba_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

ba_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Bashorun")

ba_ea_sampled <- ba_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_sampled)

ba_ea_sampled_s <- ba_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

#Combining the listed and sampled data frames

colnames(ba_ea_sampled) [1] <- "Enumeration_Area"

ba_ea_comb <- cbind.fill(ba_ea_list, ba_ea_sampled, by = "Enumeration_Area")



##Challenge
chal_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Challenge")

chal_ea_list <- chal_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(chal_ea_list)

chal_ea_list_s <- chal_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

##Olopomewa
olop_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Olopomewa")

olop_ea_list <- olop_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(olop_ea_list)

olop_ea_list_s <- olop_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()



##Households Sampled

ib_hh_sampled <- read.csv(file.path(NuDPDir , "Ib_sampled_list_final.csv"))

#Agugu
ag_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

ag_ea_sampled <- ag_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()

View(ag_ea_sampled)

ag_ea_sampled_s <- ag_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


#Bashorun
ba_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Bashorun")

ba_ea_sampled <- ba_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_sampled)

ba_ea_sampled_s <- ba_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


#Challenge
chal_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Challenge")

chal_ea_sampled <- chal_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(chal_ea_sampled)

chal_ea_sampled_s <- chal_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()




#Olopomewa
olop_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Olopomewa")

olop_ea_sampled <- olop_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(olop_ea_sampled)

olop_ea_sampled_s <- olop_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


##Probability of selecting households per EA

install.packages("devtools")
library(devtools)
devtools::install_github("cran/rowr")

library(rowr)


##Agugu

colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

##Bashorun

colnames(ba_ea_sampled) [1] <- "Enumeration_Area"

ba_ea_comb <- cbind.fill(ba_ea_list, ba_ea_sampled, by = "Enumeration_Area")


##Challenge

colnames(chal_ea_sampled) [1] <- "Enumeration_Area"

chal_ea_comb <- cbind.fill(chal_ea_list, chal_ea_sampled, by = "Enumeration_Area")


##Olopomewa

colnames(olop_ea_sampled) [1] <- "Enumeration_Area"

olop_ea_comb <- cbind.fill(olop_ea_list, olop_ea_sampled, by = "Enumeration_Area")


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


library(tidyverse)

###Household Listed
ib_hh_listed <- read.csv(file.path(NuDPDir , "hh_listed_final.csv"))

###Household Sampled
ib_hh_sampled <- read.csv(file.path(NuDPDir , "Ib_sampled_list_final.csv"))


##Ibadan
##HH structure List and number of HHs per structure
#ag_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Agugu")

ib_hh_list <- ib_hh_listed %>%
  group_by(Enumeration_Area, `Settlement`, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, `Settlement`,X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>% 
  ungroup()

View(ib_hh_list)

##Total Number of HHs listed per EA
ib_hh_list_s <- ib_hh_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

View(ib_hh_list_s)

##Sampled

#ib_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

##Wrangling to generate number of HHs per structure in sampled list
ib_ea_sampled <- ib_hh_sampled %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()

View(ib_ea_sampled)

##Generate number of HHs sampled per EA
ib_ea_sampled_s <- ib_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh3 = sum(tot_hh2)) %>%
  ungroup()

View(ib_ea_sampled_s)



# ag_ea_sampled <- ag_hh_sampled %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
#   count() %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure) %>%
#   summarise(tot_hh2 = sum(n)) %>%
#   ungroup()

#View(ag_ea_sampled)

##Combine listed and sampled data
# install.packages("devtools")
# library(devtools)
# devtools::install_github("cran/rowr")

library(rowr)
library(tidyr)

##Probability of selecting HHs within EAs(Stage 2)

colnames(ib_ea_sampled_s) [1] <- ("enumeration_area")

ib_hhs_comb <- cbind.fill(ib_hh_list_s, ib_ea_sampled_s, by = "Enumeration_Area")

View(ib_hhs_comb)

ib_hh_prob <- ib_hhs_comb %>% 
  dplyr::mutate(pr_hhs = tot_hh3/tot_hh2)

View(ib_hh_prob)
##Stage 3
##Probability by individual HHs
colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

colnames(aa_ea_comb) [5] <- "Enumeration_Area1"

colnames(aa_ea_comb) [8] <- "tot_hh_s"


##Probability of HHs Sampled per structure
head(aa_ea_comb)

#aa_ea_comb_p <- aa_ea_comb %>% dplyr::select (`Enumeration_Area`, `tot_hh`, `tot_hh_s`)

aa_ea_prob <- aa_ea_comb %>% 
  dplyr::mutate(pr_hh = tot_hh_s/tot_hh)

View(aa_ea_prob)

##Write csv to desktop for exploring
if (!file.exists("NuDPDir")) {
  dir.create("NuDPDir")
}

write.csv(aa_ea_prob, file.path("C:/Users/DELL/Desktop", "prob_hh_hs.csv"))


##Read in CSS data for individual probability of testing
ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

##Wrangling of data for individuals tested per households

##Extract required columns
ib_css_hh_w <- dplyr::select(ib_css_hh, Serial.Number, 
                             Repeat.Instrument,            
                             Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                             LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                             Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                             Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                             Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                             Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                             Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                             Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                             Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                             HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                             HOUSEHOLD.COORDINATE..Latitude,
                             q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                             q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                             q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                             q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                             q304..DBS.CODE)

ib_css_hh_df <- rename(ib_css_hh_w, 
                       SN = Serial.Number, 
                       Repeat_Instrument = Repeat.Instrument,            
                       Repeat_Instrance = Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       LGA = LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Ward = Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                       Formal = Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                       Informal = Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                       Slum = Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                       Community_Name = Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       EA_Clust_No = Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       HH_n = Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Longitude = HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       Latitude = HOUSEHOLD.COORDINATE..Latitude,
                       Line_No = q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       RDT_CONSENT = q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                       RESULT = q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                       DBS = q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       DBS_CODE = q304..DBS.CODE)

##Fill necessary cells
library(dplyr)

# Replicate values to all rows with the same serial number
ib_css_hh_dfn <- ib_css_hh_df %>%
  group_by(SN) %>%
  mutate(LGA = first(LGA),
         Ward = first(Ward),
         EA_Clust_No = first(EA_Clust_No),
         HH_n = first(HH_n),
         Longitude = first(Longitude),
         Latitude = first(Latitude)) %>%
  ungroup()

ib_hh_list_frame <- ib_css_hh_dfn %>% dplyr::filter(Repeat_Instrument == 'Household List')

ib_hh_list_frame_sum <- ib_hh_list_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n()) %>% 
  ungroup()

row_to_delete <- 1
ib_hh_list_frame_sum <- ib_hh_list_frame_sum [-row_to_delete, ]
# %>% 
#   ungroup()

ib_hh_test_frame <- ib_css_hh_dfn %>% 
  dplyr::filter(Repeat_Instrument == 'Section 3 Malaria Screening')

ib_hh_test_frame_sum <- ib_hh_test_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n())

row_to_delete <- 1
ib_hh_test_frame_sum <- ib_hh_test_frame_sum [-row_to_delete, ]

#ib_final_list <- cbind(ib_hh_list_frame_sum, ib_hh_test_frame_sum)

ib_final_list_df <- merge(ib_hh_list_frame_sum, ib_hh_test_frame_sum, by = "SN", all = FALSE)

ib_final_list_prop_df <- ib_final_list_df %>% 
  dplyr::mutate(pr_ind = Count.y/Count.x)




##Agugu
##HH structure List and number of HHs per structure
ag_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Agugu")

ag_ea_list <- ag_hh_listed %>%
  group_by(Enumeration_Area, `Settlement`, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, `Settlement`,X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>% 
  ungroup()

View(ag_ea_list)

##Total Number of HHs listed per EA
ag_ea_list_s <- ag_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

View(ag_ea_list_s)

##Sampled

ag_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

##Wrangling to generate number of HHs per structure in sampled list
ag_ea_sampled <- ag_hh_sampled %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, settlement, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()


View(ag_ea_sampled)

# ag_ea_sampled <- ag_hh_sampled %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
#   count() %>%
#   group_by(enumeration_area, X_001_serial_number_of_structure) %>%
#   summarise(tot_hh2 = sum(n)) %>%
#   ungroup()

View(ag_ea_sampled)

##Generate number of HHs sampled per EA
ag_ea_sampled_s <- ag_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh3 = sum(tot_hh2)) %>%
  ungroup()

View(ag_ea_sampled_s)

##Combine listed and sampled data
install.packages("devtools")
library(devtools)
devtools::install_github("cran/rowr")

library(rowr)
library(tidyr)

##Probability of selecting HHs within EAs(Stage 2)

colnames(ag_ea_sampled_s) [1] <- ("enumeration_area")

aa_hhs_comb <- cbind.fill(ag_ea_list_s, ag_ea_sampled_s, by = "Enumeration_Area")

aa_hh_prob <- aa_hhs_comb %>% 
  dplyr::mutate(pr_hhs = tot_hh3/tot_hh2)


##Probability by individual HHs
colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

colnames(aa_ea_comb) [5] <- "Enumeration_Area1"

colnames(aa_ea_comb) [8] <- "tot_hh_s"


##Probability of HHs Sampled per structure
head(aa_ea_comb)

#aa_ea_comb_p <- aa_ea_comb %>% dplyr::select (`Enumeration_Area`, `tot_hh`, `tot_hh_s`)

aa_ea_prob <- aa_ea_comb %>% 
  dplyr::mutate(pr_hh = tot_hh_s/tot_hh)

View(aa_ea_prob)

##Write csv to desktop for exploring
if (!file.exists("NuDPDir")) {
  dir.create("NuDPDir")
}

write.csv(aa_ea_prob, file.path("C:/Users/DELL/Desktop", "prob_hh_hs.csv"))


##Read in CSS data for individual probability of testing
ib_css_hh <- read.csv(file.path(NuDPDir , "hh_data_ib_3011.csv"))

##Wrangling of data for individuals tested per households

##Extract required columns
ib_css_hh_w <- dplyr::select(ib_css_hh, Serial.Number, 
                           Repeat.Instrument,            
                           Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                           LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                           Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                           Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                           Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                           Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                           Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                           Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                           Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                           HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                           HOUSEHOLD.COORDINATE..Latitude,
                           q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                           q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                           q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                           q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                           q304..DBS.CODE)

ib_css_hh_df <- rename(ib_css_hh_w, 
                       SN = Serial.Number, 
                       Repeat_Instrument = Repeat.Instrument,            
                       Repeat_Instrance = Repeat.Instance,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       LGA = LOCAL.GOVT..AREA,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Ward = Ward,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                       Formal = Settlement.Type..choice.Formal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                       Informal = Settlement.Type..choice.Informal.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                       Slum = Settlement.Type..choice.Slum.,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                       Community_Name = Community.Name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                       EA_Clust_No = Enumeration.Area.Cluster.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       HH_n = Household.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                       Longitude = HOUSEHOLD.COORDINATE..Longitude,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                       Latitude = HOUSEHOLD.COORDINATE..Latitude,
                       Line_No = q300i..Line.Number,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       RDT_CONSENT = q301..CONSENT.FOR.RDT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                       RESULT = q302..RESULT,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                       DBS = q303..DRIED.BLOOD.SAMPLE.COLLECTED,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                       DBS_CODE = q304..DBS.CODE)

##Fill necessary cells
library(dplyr)

# Replicate values to all rows with the same serial number
ib_css_hh_dfn <- ib_css_hh_df %>%
  group_by(SN) %>%
  mutate(LGA = first(LGA),
         Ward = first(Ward),
         EA_Clust_No = first(EA_Clust_No),
         HH_n = first(HH_n),
         Longitude = first(Longitude),
         Latitude = first(Latitude)) %>%
  ungroup()

ib_hh_list_frame <- ib_css_hh_dfn %>% dplyr::filter(Repeat_Instrument == 'Household List')

ib_hh_list_frame_sum <- ib_hh_list_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n()) %>% 
  ungroup()

row_to_delete <- 1
ib_hh_list_frame_sum <- ib_hh_list_frame_sum [-row_to_delete, ]
# %>% 
#   ungroup()

ib_hh_test_frame <- ib_css_hh_dfn %>% 
  dplyr::filter(Repeat_Instrument == 'Section 3 Malaria Screening')

ib_hh_test_frame_sum <- ib_hh_test_frame %>% 
  group_by(SN, Ward, EA_Clust_No) %>%
  summarise(Count = n())

row_to_delete <- 1
ib_hh_test_frame_sum <- ib_hh_test_frame_sum [-row_to_delete, ]

#ib_final_list <- cbind(ib_hh_list_frame_sum, ib_hh_test_frame_sum)

ib_final_list_df <- merge(ib_hh_list_frame_sum, ib_hh_test_frame_sum, by = "SN", all = FALSE)

ib_final_list_prop_df <- ib_final_list_df %>% 
  dplyr::mutate(pr_ind = Count.y/Count.x)



library(dplyr)

result <- ib_css_hh %>%
  group_by(Serial.Number, Line.Number) %>%
  summarise(Count = n()) %>%
  group_by(Serial.Number) %>%
  summarise(TotalCount = n())


df1 <- ib_css_hh %>%
  mutate_if(is.character, as.factor)

df_filled3 <- df1 %>%
  group_by(`Serial.Number`) %>%
  fill(everything())

# Assuming your data frame is named 'your_data' and the column is 'your_column'
library(dplyr)
df1a <- df1 %>% filter (Ward == "Agugu")

your_data <- df1a %>%
  mutate(Ward = ifelse(Ward == "", "Agugu", Ward))





##Bashorun

ba_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Bashorun")

ba_ea_list <- ba_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_list)

ba_ea_list_s <- ba_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

ba_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Bashorun")

ba_ea_sampled <- ba_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_sampled)

ba_ea_sampled_s <- ba_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

#Combining the listed and sampled data frames

colnames(ba_ea_sampled) [1] <- "Enumeration_Area"

ba_ea_comb <- cbind.fill(ba_ea_list, ba_ea_sampled, by = "Enumeration_Area")



##Challenge
chal_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Challenge")

chal_ea_list <- chal_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(chal_ea_list)

chal_ea_list_s <- chal_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()

##Olopomewa
olop_hh_listed <- ib_hh_listed %>% dplyr::filter(Ward == "Olopomewa")

olop_ea_list <- olop_hh_listed %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure, X_004_Serial_Number_o_old_in_the_structure) %>%
  count() %>%
  group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(olop_ea_list)

olop_ea_list_s <- olop_ea_list %>%
  group_by(Enumeration_Area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()



##Households Sampled

ib_hh_sampled <- read.csv(file.path(NuDPDir , "Ib_sampled_list_final.csv"))

#Agugu
ag_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Agugu")

ag_ea_sampled <- ag_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh2 = sum(n)) %>%
  ungroup()

View(ag_ea_sampled)

ag_ea_sampled_s <- ag_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


#Bashorun
ba_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Bashorun")

ba_ea_sampled <- ba_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(ba_ea_sampled)

ba_ea_sampled_s <- ba_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


#Challenge
chal_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Challenge")

chal_ea_sampled <- chal_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(chal_ea_sampled)

chal_ea_sampled_s <- chal_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()




#Olopomewa
olop_hh_sampled <- ib_hh_sampled %>% dplyr::filter(ward == "Olopomewa")

olop_ea_sampled <- olop_hh_sampled %>%
  group_by(enumeration_area, X_001_serial_number_of_structure, X_004_serial_number_o_old_in_the_) %>%
  count() %>%
  group_by(enumeration_area, X_001_serial_number_of_structure) %>%
  summarise(tot_hh = sum(n)) %>%
  ungroup()

View(olop_ea_sampled)

olop_ea_sampled_s <- olop_ea_sampled %>%
  group_by(enumeration_area) %>%
  # count() %>%
  # group_by(Enumeration_Area, X_001_Serial_Number_of_Structure) %>%
  summarise(tot_hh2 = sum(tot_hh)) %>%
  ungroup()


##Probability of selecting households per EA

install.packages("devtools")
library(devtools)
devtools::install_github("cran/rowr")

library(rowr)


##Agugu

colnames(ag_ea_sampled) [1] <- "Enumeration_Area"

aa_ea_comb <- cbind.fill(ag_ea_list, ag_ea_sampled, by = "Enumeration_Area")

##Bashorun

colnames(ba_ea_sampled) [1] <- "Enumeration_Area"

ba_ea_comb <- cbind.fill(ba_ea_list, ba_ea_sampled, by = "Enumeration_Area")


##Challenge

colnames(chal_ea_sampled) [1] <- "Enumeration_Area"

chal_ea_comb <- cbind.fill(chal_ea_list, chal_ea_sampled, by = "Enumeration_Area")


##Olopomewa

colnames(olop_ea_sampled) [1] <- "Enumeration_Area"

olop_ea_comb <- cbind.fill(olop_ea_list, olop_ea_sampled, by = "Enumeration_Area")

