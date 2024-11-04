# Load necessary libraries
library(stringdist)
library(dplyr)
library(purrr)

hf_dry <- read.csv("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/HF_data/HF_data_merged_kn_ib_dryseason.csv")

 kano <- read_xlsx("/Users/Mac/Downloads/Gift Wilfred Enang/all about Health facility/HFS_data_Kano_all.xlsx") %>%
  mutate(`NAME OF HEALTH FACILITY` = case_when(
    `NAME OF HEALTH FACILITY` == "SABO GARBA PHC" ~ "Fatima Ganduje PHC",
    grepl("SARGALLE|Shargalle", `NAME OF HEALTH FACILITY`, ignore.case = TRUE) ~ "SHARGALLE KYAUYEN ALU PHC",
    grepl("GANDU ALBASA PHC", `NAME OF HEALTH FACILITY`, ignore.case = TRUE) ~ "GANDUN ALBASA PHC",
    grepl("PRIMARY HEALTH CARE SHARADA", `NAME OF HEALTH FACILITY`, ignore.case = TRUE) ~ "Sharada PHC",
    grepl("hadiza shehu", `NAME OF HEALTH FACILITY`, ignore.case = TRUE) ~ "Dala MCH",
    grepl("kabaga phc", `NAME OF HEALTH FACILITY`, ignore.case = TRUE) ~ "Kabuga PHC",
    TRUE ~ `NAME OF HEALTH FACILITY`  # Keep original names for non-matching rows
  ))        
         
kano$`NAME OF HEALTH FACILITY` <- tolower(kano$`NAME OF HEALTH FACILITY`)


# Convert both facility name columns to lowercase and keep only alphabetical characters
kano_facilities <- tolower(kano$`NAME OF HEALTH FACILITY`)

hf_dry_facilities <- tolower(hf_dry$NAME.OF.HEALTH.FACILITY)

# Create a data frame to store the matches
matched_facilities <- map2_df(kano_facilities, seq_along(kano_facilities), function(facility, index) {
  tryCatch({
    # Extract the first four characters of the facility name
    facility_sub <- substr(facility, 1, 4)
    
    # Extract the first four characters for each facility in hfhf_dry_facilities
    hf_dry_sub <- substr(hf_dry_facilities, 1, 4)
    
    # Calculate distances based on the first four letters
    distances <- stringdist(facility_sub, hf_dry_sub, method = "jw")
    
    # Find the best match based on the shortest distance
    best_match <- hf_dry_facilities[which.min(distances)]
    
    # Return a data frame row with both the original facility and the best match
    data.frame(Requested = facility, Matched = best_match)
  }, error = function(e) {
    # Return NA for the match if there's an error
    data.frame(Requested = facility, Matched = NA)
  })
})
view(matched_facilities)


# Perform an exact inner join based on exact matches in 'Requested' and 'NAME OF HEALTH FACILITY'
# This will add all columns from 'matched_facilities' to 'kano' where the names match exactly
kano_new_hf_names <- kano %>%
  inner_join(matched_facilities, by = c("NAME OF HEALTH FACILITY" = "Requested"))%>%
  distinct()%>%
  mutate(
    Matched = case_when(
      `NAME OF HEALTH FACILITY` == "gandun albasa phc" ~ "Gandun Albasa PHC",
      `NAME OF HEALTH FACILITY` == "shargalle kyauyen alu phc" ~ "Shargalle Kyauyen Alu PHC",
      TRUE ~ Matched  # Keep the existing match if none of the conditions are met
    )
  )%>%
  select(-`NAME OF HEALTH FACILITY`) %>%
  rename(`NAME OF HEALTH FACILITY` = Matched) %>%
  mutate(`LOCAL GOVT. AREA` = case_when(
    is.na(`NAME OF HEALTH FACILITY`) ~ "Fagge",
    `NAME OF HEALTH FACILITY` == "dala mch" ~ "Dala",
    `NAME OF HEALTH FACILITY` == "sharada phc" ~ "Kano Municipal",
    `NAME OF HEALTH FACILITY` == "rijiyar lemo phc" ~ "Fagge",
    `NAME OF HEALTH FACILITY` == "kabuga phc" ~ "Gwale",
    `NAME OF HEALTH FACILITY` == "kawaji phc" ~ "Nasarawa",
    `NAME OF HEALTH FACILITY` == "gwagwarwa phc" ~ "Nasarawa",
    `NAME OF HEALTH FACILITY` == "unguwa uku phc" ~ "Tarauni",
    `NAME OF HEALTH FACILITY` == "Shargalle Kyauyen Alu PHC" ~ "Tarauni",
    `NAME OF HEALTH FACILITY` == "fatima ganduje phc" ~ "Fagge",
    `NAME OF HEALTH FACILITY` == "Gandun Albasa PHC" ~ "Kano Municipal",
    TRUE ~ `LOCAL GOVT. AREA`  # Leave other values unchanged
  ))%>%
  mutate(`NAME OF HEALTH FACILITY` = case_when(
  `NAME OF HEALTH FACILITY` == "dala mch" ~ "Dala MCH",
  `NAME OF HEALTH FACILITY` == "sharada phc" ~ "Sharada PHC",
  `NAME OF HEALTH FACILITY` == "rijiyar lemo phc" ~ "Rijiyar Lemo PHC",
  `NAME OF HEALTH FACILITY` == "kabuga phc" ~ "Kabuga PHC",
  `NAME OF HEALTH FACILITY` == "kawaji phc" ~ "Kawaji PHC",
  `NAME OF HEALTH FACILITY` == "gwagwarwa phc" ~  "Gwagwarwa PHC",
  `NAME OF HEALTH FACILITY` == "unguwa uku phc" ~ "Unguwa Uku PHC",
  `NAME OF HEALTH FACILITY` == "Shargalle Kyauyen Alu PHC" ~ "Shargalle Kyauyen Alu PHC",
  `NAME OF HEALTH FACILITY` == "fatima ganduje phc" ~ "Fatima Ganduje PHC",
  `NAME OF HEALTH FACILITY` == "Gandun Albasa PHC" ~ "Gandun Albasa PHC",
  TRUE ~ `NAME OF HEALTH FACILITY`  # Leave other values unchanged
))%>%
  mutate(across(everything(), as.character))

unique(kano_new_hf_names$`LOCAL GOVT. AREA`)

unique(kano_new_hf_names$`NAME OF HEALTH FACILITY`)



















ibadan <- read_xlsx("/Users/Mac/Downloads/Gift Wilfred Enang/all about Health facility/HFS_data_Ibadan_all.xlsx")%>%
  mutate(`NAME OF HEALTH FACILITY` = case_when(
    `NAME OF HEALTH FACILITY` == "IDI OGUNGUN" ~ "Idi Ogungun PHC",
    `NAME OF HEALTH FACILITY` == "IDIOGUNGUN" ~ "Idi Ogungun PHC",
    `NAME OF HEALTH FACILITY` == "OKE ADU PHC" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKEADU PHC" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKE ADU HEALTH CENTRE" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKE ADU HEALTH FACILITY" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKE ADU HEALTH CENTER" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKE ADU" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "OKEADU HEALTH FACILITY" ~ "Oke Adu PHC",
    `NAME OF HEALTH FACILITY` == "UCH" ~ "UCH",
    `NAME OF HEALTH FACILITY` == "UH" ~ "UCH",
    `NAME OF HEALTH FACILITY` == "JERICHO SPECIALIST HOSPITAL" ~ "Jericho SH",
    `NAME OF HEALTH FACILITY` == "JERICHO SPECIALISTS HOSPITAL" ~ "Jericho SH",
    `NAME OF HEALTH FACILITY` == "JERICHO SPECIALIST" ~ "Jericho SH",
    `NAME OF HEALTH FACILITY` == "AGBONGBON" ~ "Agbongbon PHC",
    `NAME OF HEALTH FACILITY` == "AGBONGBON PHC" ~ "Agbongbon PHC",
    `NAME OF HEALTH FACILITY` == "AGBOGBON" ~ "Agbongbon PHC",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY HOSPITAL" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY HOSPITAL I 6" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING HOSPITAL YEMETU" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY  TEACHING HOSPITAL YEMETU" ~ "Adeoyo MTH", 
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY  TEACHING HOSPITAL" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING HOSPITAL" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING HOSPITAL, YEMETU" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING HOSPITAL, YEMETU." ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY HOSPITAL YEMETU" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO HEALTH FACILITY" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "ADEOYO" ~ "Adeoyo MTH", 
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY" ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "RINGROAD STATE SPECIALIST" ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD STATE HOSPITAL." ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD STATE HOSPITAL" ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD HOSPITAL" ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD HOSPITAL." ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD STATE SPECIALIST" ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RINGROAD HOSPITAL." ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RINGROAD HOSPITAL" ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "RING ROAD STATE SPECIALIST, RING ROAD." ~ "RingRoad SSH",
    `NAME OF HEALTH FACILITY` == "ONIYANRIN COMPREHENSIVE HEALTH CENTER" ~ "Oniyanrin Comp HC",
    `NAME OF HEALTH FACILITY` == "ONIYANRIN COMPREHENSIVE HEALTH CENTER need" ~ "Oniyanrin Comp HC",
    `NAME OF HEALTH FACILITY` == "ONIYANRIN COMPREHENSIVE HOSPITAL, IBADAN" ~ "Oniyanrin Comp HC",
    `NAME OF HEALTH FACILITY` == "NAOMI MEDICAL CENTRE" ~ "Naomi Medical Centre",
    `NAME OF HEALTH FACILITY` == "NAOMI MEDICAL CENTRE." ~ "Naomi Medical Centre",
    `NAME OF HEALTH FACILITY` == "NAOMI MEDICAL CENTER" ~ "Naomi Medical Centre",
    `NAME OF HEALTH FACILITY` == "ORANYAN PHC" ~ "ORANYAN PHC",
    `NAME OF HEALTH FACILITY` == "ALAFARA PHC" ~ "Alafara PHC",
    `NAME OF HEALTH FACILITY` == "ALAFARA" ~ "Alafara PHC",
    `NAME OF HEALTH FACILITY` == "ADEOYO MATERNITY TEACHING HOSPITAL." ~ "Adeoyo MTH",
    `NAME OF HEALTH FACILITY` == "IDIOGUNGUN HEALTH FACILITY" ~ "Idi Ogungun PHC",
    `NAME OF HEALTH FACILITY` == "IDI OGUNOGUN" ~ "Idi Ogungun PHC",
    TRUE ~ `NAME OF HEALTH FACILITY`  # Leave other values unchanged
  ))%>%
  mutate(`LOCAL GOVT. AREA` = case_when(
    `NAME OF HEALTH FACILITY` == "OLUYORO CATHOLIC HOSPITAL" ~ "IBADAN NORTHEAST",
    `NAME OF HEALTH FACILITY` == "Idi Ogungun PHC" ~ "IBADAN NORTH",
    `NAME OF HEALTH FACILITY` == "Oke Adu PHC" ~ "IBADAN NORTHEAST",
    `NAME OF HEALTH FACILITY` == "UCH" ~ "IBADAN NORTH",
    `NAME OF HEALTH FACILITY` == "Jericho SH" ~ "IBADAN SOUTHWEST",
    `NAME OF HEALTH FACILITY` == "Agbongbon PHC" ~ "IBADAN SOUTHEAST",
    `NAME OF HEALTH FACILITY` == "Adeoyo MTH" ~ "IBADAN NORTH",
    `NAME OF HEALTH FACILITY` == "RingRoad SSH" ~ "IBADAN SOUTHWEST",
    `NAME OF HEALTH FACILITY` == "Oniyanrin Comp HC" ~ "IBADAN NORTHWEST",
    `NAME OF HEALTH FACILITY` == "Naomi Medical Centre" ~ "IBADAN NORTHWEST",
    `NAME OF HEALTH FACILITY` == "ORANYAN PHC" ~ "IBADAN SOUTHEAST",
    `NAME OF HEALTH FACILITY` == "Alafara PHC" ~ "IBADAN NORTHEAST",
    TRUE ~ `LOCAL GOVT. AREA`))%>%
  mutate(across(everything(), as.character))
    

hf_wet_season <- full_join(kano_new_hf_names, ibadan)

write_csv(hf_wet_season, "/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/HF_data/HF_data_merged_wet_season.csv")




unique(ibadan$`LOCAL GOVT. AREA`)

unique(ibadan$`NAME OF HEALTH FACILITY`)




view(hf_wet_season)
view(ibadan)














