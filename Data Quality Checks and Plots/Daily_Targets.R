library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
df1<-read_csv('/Users/user/Downloads/kn_hh_data_3011.csv')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)



df_filled <- df1 %>%
  group_by(`Serial Number`) %>%
  fill(2:7, 20:26)

View(df_filled3)



theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          #axis.title.x = element_blank(),
          axis.title.y = element_text(size =8),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}


mydata <-table(df_filled3$`If others, specify` )

View(mydata)




#file_path <- "/Users/user/Downloads/missing_columns_kn_1.csv"

# Export the data frame to a CSV file
#write.csv(column_missingkn, file = file_path, row.names = TRUE)


# Check for missing values by individual questions ------
missing_lga<- df_filled3 %>%
  filter(is.na(`LOCAL GOVT. AREA`)) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing Values LGA Name" = n(),
  )
View(missing_lga)


missing_ward<- df_filled3 %>%
  filter(is.na(`Ward`)) |>
select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing Values Ward Name" = n(),
  )
View(missing_ward)

missing_settlement<- df_filled3 %>%
  filter(is.na(`Settlement Type`)) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing Values Settlement Type" = n(),
  )

View(missing_settlement)


missing_comm<- df_filled3 %>%
  filter(is.na(`Community Name`)) |>
  select(1,24,25,27)
View(missing_comm)

missing_ea<- df_filled3 %>%
  filter(is.na(`Enumeration Area/Cluster Number`)) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing Values EA Name" = n(),
  )
View(missing_ea)



missing_comm<- df_filled3 %>%
  filter(is.na(`Community Name`)) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing Community Name" = n(),
  )
View(missing_comm)



missing_coord<- df_filled3 %>%
  filter(is.na(`HOUSEHOLD COORDINATE- Longitude`) | is.na(`HOUSEHOLD COORDINATE- Latitude`), `Complete?...94`=="Complete" ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    "Missing GPS Coordinates Lat or Long" = n(),
  )
View(missing_coord)





Column1<- df_filled3 %>%
  filter(is.na(`Serial Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Serial Number_Missing' = n(),
  )
View(Column1)
Column2<- df_filled3 %>%
  filter(is.na(`LOCAL GOVT. AREA`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'LOCAL GOVT. AREA_Missing' = n(),
  )
View(Column2)
Column3<- df_filled3 %>%
  filter(is.na(`Ward`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Ward_Missing' = n(),
  )
View(Column3)
Column4<- df_filled3 %>%
  filter(is.na(`Settlement Type`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Settlement Type_Missing' = n(),
  )
View(Column4)
Column5<- df_filled3 %>%
  filter(is.na(`Enumeration Area/Cluster Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Enumeration Area/Cluster Number_Missing' = n(),
  )
View(Column5)
Column6<- df_filled3 %>%
  filter(is.na(`HOUSEHOLD COORDINATE- Longitude`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'HOUSEHOLD COORDINATE- Longitude_Missing' = n(),
  )
View(Column6)

Column7<- df_filled3 %>%
  filter(is.na(`HOUSEHOLD COORDINATE- Latitude`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'HOUSEHOLD COORDINATE- Latitude_Missing' = n(),
  )


View(Column7)
Column8<- df_filled3 %>%
  filter(is.na(`Line Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Line Number_Missing' = n(),
  )


View(Column8)
Column9<- df_filled3 %>%
  filter(is.na(`Usual Residents, Please give me the names of the persons who usually live here`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Usual Residents, Please give me the names of the persons who usually live here_Missing' = n(),
  )


View(Column9)
Column10<- df_filled3 %>%
  filter(is.na(`Relationship to Household   Head (HH): What is the relationship of (NAME) to the HH`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Relationship to Household   Head (HH): What is the relationship of (NAME) to the HH_Missing' = n(),
  )


View(Column10)
Column11<- df_filled3 %>%
  filter(is.na(`Sex: Is (NAME) male or female?...11`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Sex: Is (NAME) male or female?_Missing' = n(),
  )


View(Column11)


Column12<- df_filled3 %>%
  filter(is.na(`Age: How old was   (NAME) as at last birthday?...12`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Age: How old was   (NAME) as at last birthday?_Missing' = n(),
  )
View(Column12)


Column13<- df_filled3 %>%
  filter(is.na(`q300: As part of this survey, we are asking eligible respondents to take a test to see if they have malaria. Malaria is a serious illness caused by a parasite transmitted by a mosquito bite. This survey will assist the government to develop programs to prevent malaria.  We ask all eligible respondents to take part in malaria testing in this survey and give a few drops of blood from a finger or heel. One blood drop will be tested for malaria immediately, and the result will be told to you right away. All results will be kept strictly confidential and will not be shared with anyone other than members of our survey team.     Do you have any questions?  You can say yes or no. It is up to you to decide. Will you participate in the malaria test?'`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q300: As part of this survey, we are asking eligible respondents to take a test to see if they have malaria. Malaria is a serious illness caused by a parasite transmitted by a mosquito bite. This survey will assist the government to develop programs to prevent malaria.  We ask all eligible respondents to take part in malaria testing in this survey and give a few drops of blood from a finger or heel. One blood drop will be tested for malaria immediately, and the result will be told to you right away. All results will be kept strictly confidential and will not be shared with anyone other than members of our survey team.     Do you have any questions?  You can say yes or no. It is up to you to decide. Will you participate in the malaria test?_Missing' = n(),
  )
View(Column13)


Column14<- df_filled3 %>%
  filter(is.na(`q300i: Line Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q300i: Line Number_Missing' = n(),
  )
View(Column14)
Column15<- df_filled3 %>%
  filter( (`q302: RESULT`=="POSITIVE" | `q302: RESULT`=="NEGATIVE"), is.na(`q301: CONSENT FOR RDT`) ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q301: CONSENT FOR RDT_Missing' = n(),
  )
View(Column15)


Column16<- df_filled3 %>%
  filter(is.na(`q302: RESULT`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q302: RESULT_Missing' = n(),
  )

View(Column16)
Column17<- df_filled3 %>%
  filter(!(is.na(`q303: DRIED BLOOD SAMPLE COLLECTED`)) , is.na(`q301: CONSENT FOR RDT`),  `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q303: DRIED BLOOD SAMPLE COLLECTED_Missing' = n(),
  )
View(Column17)


Column18<- df_filled3 %>%
  filter(is.na(`q304: DBS CODE`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q304: DBS CODE_Missing' = n(),
  )
View(Column18)
Column19<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column19)
Column20<- df_filled3 %>%
  filter(is.na(`If others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'If others, specify_Missing' = n(),
  )
View(Column20)
Column21<- df_filled3 %>%
  filter(is.na(`Community Name`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Community Name_Missing' = n(),
  )
View(Column21)
Column22<- df_filled3 %>%
  filter(is.na(`Household Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Household Number_Missing' = n(),
  )
View(Column22)
Column23<- df_filled3 %>%
  filter(is.na(`Name of Household Head`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Name of Household Head_Missing' = n(),
  )
View(Column23)
Column24<- df_filled3 %>%
  filter(is.na(`Date`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Date_Missing' = n(),
  )
View(Column24)
Column25<- df_filled3 %>%
  filter(is.na(`INTERVIEWER'S NAME `) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'INTERVIEWER'S NAME _Missing' = n(),
  )
View(Column25)
Column26<- df_filled3 %>%
  filter(is.na(`INTERVIEWERS PHONE NO`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'INTERVIEWERS PHONE NO_Missing' = n(),
  )
View(Column26)
Column27<- df_filled3 %>%
  filter(is.na(`NTERVIEWER VISIT 1 result`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'NTERVIEWER VISIT 1 result_Missing' = n(),
  )
View(Column27)
Column28<- df_filled3 %>%
  filter(is.na(`Visit 1 Date`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Visit 1 Date_Missing' = n(),
  )
View(Column28)
Column29<- df_filled3 %>%
  filter(is.na(`NTERVIEWER VISIT 2 result`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'NTERVIEWER VISIT 2 result_Missing' = n(),
  )
View(Column29)
Column30<- df_filled3 %>%
  filter(is.na(`Visit 2 Date`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Visit 2 Date_Missing' = n(),
  )
View(Column30)
Column31<- df_filled3 %>%
  filter(is.na(`NTERVIEWER VISIT 3 result`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'NTERVIEWER VISIT 3 result_Missing' = n(),
  )
View(Column31)
Column32<- df_filled3 %>%
  filter(is.na(`Visit 3 Date`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Visit 3 Date_Missing' = n(),
  )
View(Column32)
Column33<- df_filled3 %>%
  filter(is.na(`SUPERVISORS NAME`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'SUPERVISORS NAME_Missing' = n(),
  )
View(Column33)
Column34<- df_filled3 %>%
  filter(is.na(`FIELD EDITOR`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'FIELD EDITOR_Missing' = n(),
  )
View(Column34)
Column35<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column35)
Column36<- df_filled3 %>%
  filter(is.na(`Date of Birth (day/month/year)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Date of Birth (day/month/year)_Missing' = n(),
  )
View(Column36)
Column37<- df_filled3 %>%
  filter(is.na(`ASK IF MOTHER STAYS IN HOUSEHOLD, IF YES RECORD  MOTHERS LINE NO`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'ASK IF MOTHER STAYS IN HOUSEHOLD, IF YES RECORD  MOTHERS LINE NO_Missing' = n(),
  )
View(Column37)
Column38<- df_filled3 %>%
  filter(is.na(`Marital Status: What is (NAME) current marital status`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Marital Status: What is (NAME) current marital status_Missing' = n(),
  )
View(Column38)
Column39<- df_filled3 %>%
  filter(is.na(`RDT Eligibility: CRITERIA FOR RDT  (if less than or equal to 5 people in the household, test everyone, if greater than 5 people, test one person in these age groups: 0-5 years;  6-10 years; 11-17 years; 18-30 years; 30 years and above    If age category is missing, test two persons in the youngest category `) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'RDT Eligibility: CRITERIA FOR RDT  (if less than or equal to 5 people in the household, test everyone, if greater than 5 people, test one person in these age groups: 0-5 years;  6-10 years; 11-17 years; 18-30 years; 30 years and above    If age category is missing, test two persons in the youngest category _Missing' = n(),
  )
View(Column39)
Column40<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column40)
Column41<- df_filled3 %>%
  filter(is.na(`Are there people who don't usually live here and who slept here the night before`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Are there people who don't usually live here and who slept here the night before_Missing' = n(),
  )
View(Column41)
Column42<- df_filled3 %>%
  filter(is.na(`Line Number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Line Number_Missing' = n(),
  )
View(Column42)
Column43<- df_filled3 %>%
  filter(is.na(`Visitors: Please give me the names of all persons who don't usually live here and who slept here the night before`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Visitors: Please give me the names of all persons who don't usually live here and who slept here the night before_Missing' = n(),
  )
View(Column43)
Column44<- df_filled3 %>%
  filter(is.na(`How long have they been visiting?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'How long have they been visiting?_Missing' = n(),
  )
View(Column44)
Column45<- df_filled3 %>%
  filter(is.na(`Sex: Is (NAME) male or female?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Sex: Is (NAME) male or female?_Missing' = n(),
  )
View(Column45)
Column46<- df_filled3 %>%
  filter(is.na(`Age: How old was   (NAME) as at last birthday?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Age: How old was   (NAME) as at last birthday?_Missing' = n(),
  )
View(Column46)
Column47<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column47)
Column48<- df_filled3 %>%
  filter(is.na(`q100: What is the main source of drinking water for members of your household?  (enter the number for the most commonly used)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q100: What is the main source of drinking water for members of your household?  (enter the number for the most commonly used)_Missing' = n(),
  )
View(Column48)
Column49<- df_filled3 %>%
  filter(is.na(`q100i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q100i: Others, specify_Missing' = n(),
  )
View(Column49)
Column50<- df_filled3 %>%
  filter(is.na(`q101: What is the main source of water used by your household for other purposes such as cooking and handwashing?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q101: What is the main source of water used by your household for other purposes such as cooking and handwashing?_Missing' = n(),
  )
View(Column50)
Column51<- df_filled3 %>%
  filter(is.na(`q101i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q101i: Others, specify_Missing' = n(),
  )
View(Column51)
Column52<- df_filled3 %>%
  filter(is.na(`q102: What is the main source of power/energy use for cooking in your household?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q102: What is the main source of power/energy use for cooking in your household?_Missing' = n(),
  )
View(Column52)
Column53<- df_filled3 %>%
  filter(is.na(`q102i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q102i: Others, specify_Missing' = n(),
  )
View(Column53)
Column54<- df_filled3 %>%
  filter(is.na(`q103: What kind of toilet facilities do members of your family usually use?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q103: What kind of toilet facilities do members of your family usually use?_Missing' = n(),
  )
View(Column54)
Column55<- df_filled3 %>%
  filter(is.na(`q103i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q103i: Others, specify_Missing' = n(),
  )
View(Column55)
Column56<- df_filled3 %>%
  filter(is.na(`q104: Which of the following items do you, your spouse or your family have? (Click as appropriate)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q104: Which of the following items do you, your spouse or your family have? (Click as appropriate)_Missing' = n(),
  )
View(Column56)
Column57<- df_filled3 %>%
  filter(is.na(`q105: Does any member of your household have livestock?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q105: Does any member of your household have livestock?_Missing' = n(),
  )
View(Column57)
Column58<- df_filled3 %>%
  filter(is.na(`q106: What is the status of your home ownership?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q106: What is the status of your home ownership?_Missing' = n(),
  )
View(Column58)
Column59<- df_filled3 %>%
  filter(is.na(`q107: Type of housing`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q107: Type of housing_Missing' = n(),
  )
View(Column59)
Column60<- df_filled3 %>%
  filter(is.na(`q107i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q107i: Others, specify_Missing' = n(),
  )
View(Column60)
Column61<- df_filled3 %>%
  filter(is.na(`q108: Do you share your compound with other households?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q108: Do you share your compound with other households?_Missing' = n(),
  )
View(Column61)
Column62<- df_filled3 %>%
  filter(is.na(`q109: If yes to Q108, how many households do you share your compound with?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q109: If yes to Q108, how many households do you share your compound with?_Missing' = n(),
  )
View(Column62)
Column63<- df_filled3 %>%
  filter(is.na(`q110: How many floors does your housing setting have?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q110: How many floors does your housing setting have?_Missing' = n(),
  )
View(Column63)
Column64<- df_filled3 %>%
  filter(is.na(`q111: If more than one, to Q110, which floor do you stay (This question will not apply to those in face to face dwelling)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q111: If more than one, to Q110, which floor do you stay (This question will not apply to those in face to face dwelling)_Missing' = n(),
  )
View(Column64)
Column65<- df_filled3 %>%
  filter(is.na(`q112: Concerning your household setting, where do you stay?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q112: Concerning your household setting, where do you stay?_Missing' = n(),
  )
View(Column65)
Column66<- df_filled3 %>%
  filter(is.na(`q112i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q112i: Others, specify_Missing' = n(),
  )
View(Column66)
Column67<- df_filled3 %>%
  filter(is.na(`q113: How many sleeping rooms/bedrooms does your household have?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q113: How many sleeping rooms/bedrooms does your household have?_Missing' = n(),
  )
View(Column67)
Column68<- df_filled3 %>%
  filter(is.na(`q114: Do you have your own toilet or do you share toilet with other households?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q114: Do you have your own toilet or do you share toilet with other households?_Missing' = n(),
  )
View(Column68)
Column69<- df_filled3 %>%
  filter(is.na(`q115: Where is the bathroom of your house located?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q115: Where is the bathroom of your house located?_Missing' = n(),
  )
View(Column69)
Column70<- df_filled3 %>%
  filter(is.na(`q116: Thinking about the environment where you live, is there a lot of trash and litter on the street in your neighbourhood`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q116: Thinking about the environment where you live, is there a lot of trash and litter on the street in your neighbourhood_Missing' = n(),
  )
View(Column70)
Column71<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column71)
Column72<- df_filled3 %>%
  filter(is.na(`Has anyone in your household travelled out of your current residence in the last 4 weeks?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Has anyone in your household travelled out of your current residence in the last 4 weeks?_Missing' = n(),
  )
View(Column72)
Column73<- df_filled3 %>%
  filter(is.na(`How many people in the household traveled in the last 4 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'How many people in the household traveled in the last 4 weeks_Missing' = n(),
  )
View(Column73)
Column74<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column74)
Column75<- df_filled3 %>%
  filter(is.na(`1. Name`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    '1. Name_Missing' = n(),
  )
View(Column75)
Column76<- df_filled3 %>%
  filter(is.na(`2. Line No`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    '2. Line No_Missing' = n(),
  )
View(Column76)
Column77<- df_filled3 %>%
  filter(is.na(`q117: Has (NAME) travelled out of your current residence in the last 4 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q117: Has (NAME) travelled out of your current residence in the last 4 weeks_Missing' = n(),
  )
View(Column77)
Column78<- df_filled3 %>%
  filter(is.na(`q118: Has (NAME) been ill with fever in the last 2 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q118: Has (NAME) been ill with fever in the last 2 weeks_Missing' = n(),
  )
View(Column78)
Column79<- df_filled3 %>%
  filter(is.na(`q119: Has (NAME) been diagnosed with malaria in the last 2 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q119: Has (NAME) been diagnosed with malaria in the last 2 weeks_Missing' = n(),
  )
View(Column79)
Column80<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column80)
Column81<- df_filled3 %>%
  filter(is.na(`1. Name`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    '1. Name_Missing' = n(),
  )
View(Column81)
Column82<- df_filled3 %>%
  filter(is.na(`2. Line No`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    '2. Line No_Missing' = n(),
  )
View(Column82)
Column83<- df_filled3 %>%
  filter(is.na(`q120: Has (NAME) travelled out of your current residence in the last 4 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q120: Has (NAME) travelled out of your current residence in the last 4 weeks_Missing' = n(),
  )
View(Column83)
Column84<- df_filled3 %>%
  filter(is.na(`q121: Has (NAME) been ill with fever in the last 2 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q121: Has (NAME) been ill with fever in the last 2 weeks_Missing' = n(),
  )
View(Column84)
Column85<- df_filled3 %>%
  filter(is.na(`q122: Has (NAME) been diagnosed with malaria in the last 2 weeks`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q122: Has (NAME) been diagnosed with malaria in the last 2 weeks_Missing' = n(),
  )
View(Column85)
Column86<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column86)
Column87<- df_filled3 %>%
  filter(is.na(`q123: If you had a household member diagnosed with malaria, will you be willing to notify a designated health professional in your community?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q123: If you had a household member diagnosed with malaria, will you be willing to notify a designated health professional in your community?_Missing' = n(),
  )
View(Column87)
Column88<- df_filled3 %>%
  filter(is.na(`q124: Which of these would be your preferred method of notification`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q124: Which of these would be your preferred method of notification_Missing' = n(),
  )
View(Column88)
Column89<- df_filled3 %>%
  filter(is.na(`q124i: Other (specify)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q124i: Other (specify)_Missing' = n(),
  )
View(Column89)
Column90<- df_filled3 %>%
  filter(is.na(`q125: Would you be able to report as soon as you find out?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q125: Would you be able to report as soon as you find out?_Missing' = n(),
  )
View(Column90)
Column91<- df_filled3 %>%
  filter(is.na(`q126: Who in your household would be the best person to report such cases?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q126: Who in your household would be the best person to report such cases?_Missing' = n(),
  )
View(Column91)
Column92<- df_filled3 %>%
  filter(is.na(`q126i: Other (specify)`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q126i: Other (specify)_Missing' = n(),
  )
View(Column92)
Column93<- df_filled3 %>%
  filter(is.na(`q127: Why would you not want to report to a designated health professional?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q127: Why would you not want to report to a designated health professional?_Missing' = n(),
  )
View(Column93)
Column94<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column94)
Column95<- df_filled3 %>%
  filter(is.na(`q201: Are there leaves of the house or building occupied by this household open or close`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q201: Are there leaves of the house or building occupied by this household open or close_Missing' = n(),
  )
View(Column95)
Column96<- df_filled3 %>%
  filter(is.na(`q202: Does the part of the house or building occupied by the householdhave a ceiling?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q202: Does the part of the house or building occupied by the householdhave a ceiling?_Missing' = n(),
  )
View(Column96)
Column97<- df_filled3 %>%
  filter(is.na(`q203: How would you describe the road type in the neighbourhood?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q203: How would you describe the road type in the neighbourhood?_Missing' = n(),
  )
View(Column97)
Column98<- df_filled3 %>%
  filter(is.na(`q204: Are there bushes around the neighbourhood?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q204: Are there bushes around the neighbourhood?_Missing' = n(),
  )
View(Column98)
Column99<- df_filled3 %>%
  filter(is.na(`q205: Are there dumpsites around the neighbourhood?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q205: Are there dumpsites around the neighbourhood?_Missing' = n(),
  )
View(Column99)
Column100<- df_filled3 %>%
  filter(is.na(`q206: Is there stagnant water present in the compound?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q206: Is there stagnant water present in the compound?_Missing' = n(),
  )
View(Column100)
Column101<- df_filled3 %>%
  filter(is.na(`q207: Are there vessels that could potentially hold water for mosquito breeding in the compound?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q207: Are there vessels that could potentially hold water for mosquito breeding in the compound?_Missing' = n(),
  )
View(Column101)
Column102<- df_filled3 %>%
  filter(is.na(`q208: Are there overgrown vegetation within 5 meters of houses?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q208: Are there overgrown vegetation within 5 meters of houses?_Missing' = n(),
  )
View(Column102)
Column103<- df_filled3 %>%
  filter(is.na(`q209: Are there open drainages in the neighbourhood?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q209: Are there open drainages in the neighbourhood?_Missing' = n(),
  )
View(Column103)
Column104<- df_filled3 %>%
  filter(is.na(`q210: If yes to above, are the open drainage clogged with dirt or rubbish?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q210: If yes to above, are the open drainage clogged with dirt or rubbish?_Missing' = n(),
  )
View(Column104)
Column105<- df_filled3 %>%
  filter(is.na(`q211: Is there a garden / farm within the compound?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q211: Is there a garden / farm within the compound?_Missing' = n(),
  )
View(Column105)
Column106<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column106)
Column107<- df_filled3 %>%
  filter(is.na(`q101: Does your household own a mosquito net?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q101: Does your household own a mosquito net?_Missing' = n(),
  )
View(Column107)
Column108<- df_filled3 %>%
  filter(is.na(`q101b: If your household does not have a net, why do you not have one`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q101b: If your household does not have a net, why do you not have one_Missing' = n(),
  )
View(Column108)
Column109<- df_filled3 %>%
  filter(is.na(`q102: Where did you get the nets? Tick all that apply `) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q102: Where did you get the nets? Tick all that apply _Missing' = n(),
  )
View(Column109)
Column110<- df_filled3 %>%
  filter(is.na(`q102i: If others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q102i: If others, specify_Missing' = n(),
  )
View(Column110)
Column111<- df_filled3 %>%
  filter(is.na(`q103: Did you pay for your most recent net?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q103: Did you pay for your most recent net?_Missing' = n(),
  )
View(Column111)
Column112<- df_filled3 %>%
  filter(is.na(`q104: if you paid for the net, how much did you pay?    If not sure, ask the respondent to ask partner`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q104: if you paid for the net, how much did you pay?    If not sure, ask the respondent to ask partner_Missing' = n(),
  )
View(Column112)
Column113<- df_filled3 %>%
  filter(is.na(`q105: How often does anyone sleep inside the mosquito nets in your household?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q105: How often does anyone sleep inside the mosquito nets in your household?_Missing' = n(),
  )
View(Column113)
Column114<- df_filled3 %>%
  filter(is.na(`q105i: If Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q105i: If Others, specify_Missing' = n(),
  )
View(Column114)
Column115<- df_filled3 %>%
  filter(is.na(`q106: Do you have your nets permanently hung?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q106: Do you have your nets permanently hung?_Missing' = n(),
  )
View(Column115)
Column116<- df_filled3 %>%
  filter(is.na(`q107: If yes, can we inspect  Document for each net inspected in Q108 to Q114`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q107: If yes, can we inspect  Document for each net inspected in Q108 to Q114_Missing' = n(),
  )
View(Column116)
Column117<- df_filled3 %>%
  filter(is.na(`q107a: If No, can you bring out all the mosquito nets in your household that are not hung so we can inspect them?  Document for each net inspected in Q108 to Q114`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q107a: If No, can you bring out all the mosquito nets in your household that are not hung so we can inspect them?  Document for each net inspected in Q108 to Q114_Missing' = n(),
  )
View(Column117)
Column118<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column118)
Column119<- df_filled3 %>%
  filter(is.na(`q108: Net number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q108: Net number_Missing' = n(),
  )
View(Column119)
Column120<- df_filled3 %>%
  filter(is.na(`q108a: Is the net hung?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q108a: Is the net hung?_Missing' = n(),
  )
View(Column120)
Column121<- df_filled3 %>%
  filter(is.na(`q109: Does the net have holes?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q109: Does the net have holes?_Missing' = n(),
  )
View(Column121)
Column122<- df_filled3 %>%
  filter(is.na(`q110: Has the net been stitched?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q110: Has the net been stitched?_Missing' = n(),
  )
View(Column122)
Column123<- df_filled3 %>%
  filter(is.na(`q111: What type of material is the net made of?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q111: What type of material is the net made of?_Missing' = n(),
  )
View(Column123)
Column124<- df_filled3 %>%
  filter(is.na(`q111i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q111i: Others, specify_Missing' = n(),
  )
View(Column124)
Column125<- df_filled3 %>%
  filter(is.na(`q112: How many months ago did your household get the mosquito net?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q112: How many months ago did your household get the mosquito net?_Missing' = n(),
  )
View(Column125)
Column126<- df_filled3 %>%
  filter(is.na(`q113: Did someone sleep under this net last night`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q113: Did someone sleep under this net last night_Missing' = n(),
  )
View(Column126)
Column127<- df_filled3 %>%
  filter(is.na(`q113a: How many people slept under the net`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q113a: How many people slept under the net_Missing' = n(),
  )
View(Column127)



Column128<- df_filled3 %>%
  filter(is.na(`q114: What is the name of the person that slept inside last night`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q114: What is the name of the person that slept inside last night_Missing' = n(),
  )
View(Column128)


Column129<- df_filled3 %>%
  filter(is.na(`q114a: Line number`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q114a: Line number_Missing' = n(),
  )
View(Column129)


Column130<- df_filled3 %>%
  filter(is.na(`q115: If no to Q113, Why did no one sleep inside this net?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q115: If no to Q113, Why did no one sleep inside this net?_Missing' = n(),
  )
View(Column130)

Column131<- df_filled3 %>%
  filter(is.na(`q115i: Others, specify`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'q115i: Others, specify_Missing' = n(),
  )
View(Column131)

Column132<- df_filled3 %>%
  filter(is.na(`Complete?`) , `Complete?...94`=='Complete' ) |>
  select(1,24,25,27)%>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    'Complete?_Missing' = n(),
  )
View(Column132)



#Summary Children 0 to 10 years----

summary_Children <-  df_filled3   %>%
 filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...12` <= 10 ) %>%
  group_by(`Settlement Type`, Ward) %>%
  summarise(
    Total_Count = n(),
  ) |>
  select( Ward,`Settlement Type`, Total_Count )

View(summary_Children)


#Data of children 0 - 10
summary_Children_0_to_10 <-  df_filled3   %>%
  filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...12` <= 10 )
View(summary_Children_0_to_10)


dateplot<-ggplot(summary_Children, aes(reorder(`Ward`, -Total_Count ),pmax(Total_Count,30), fill = `Settlement Type`)) +
 # geom_line()
 geom_bar(stat = "identity")+
geom_text(aes(label = paste( `Settlement Type`, ": ", signif(Total_Count)))  , position = position_stack(vjust = 0.3),
          vjust = -0.3 ) +

 # geom_text(aes(label = Ward ), position = position_stack(vjust = 0.6),
    #        vjust = -0.6 ) +
 # coord_cartesian(ylim = c(0, max(summary_Children$Total_Count) + 1)) +
  labs(title = "Household Survey - Cross Sectional",
    subtitle = "Plot of Children who are 0 to 10 by Ward and Settlement type",
    caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Count by Ward", x = "") #+ theme(axis.text.x = element_text(angle = 90))
dateplot





#Summary by DATE by Enumerator and start date----

df1$Date <- as.Date(df1$`Date`, format = "%m/%d/%Y")
View(df1)

# Now find the start date
start_date <- min(df1$Date, na.rm = TRUE)


startd <- df1 %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  mutate(Start_Date = min(Date, na.rm = TRUE)) %>%
  mutate(End_Date = max(Date, na.rm = TRUE)) %>%
  mutate(Days_worked = End_Date - Start_Date) %>%
  ungroup()
View(startd)

summary_Date_RA <-  startd  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`INTERVIEWER'S NAME`, Date, Start_Date, End_Date, Days_worked) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(Date)),
  ) |>
  select(Date, `INTERVIEWER'S NAME` , Total_Completed_by_DATE, Start_Date,  End_Date, Days_worked)
  View(summary_Date_RA)


dateplot<-ggplot(summary_Date_RA,
                 aes(reorder(`Visit 1 Date`, -Total_Completed_by_DATE ),Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Health Facility Survey",
    # subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "") + theme(axis.text.x = element_text(angle = 90))
dateplot


#Summary table for Completion by Date by Interviewer ----
table_result <- summary_Date_RA  %>%
  spread(`Visit 1 Date`, Total_Completed_by_DATE)
View(table_result)

# Now find the start date
start_date <- min(summary_Date_RA$`Visit 1 Date`, na.rm = TRUE)
print(start_date)


#Summary by DATE by Enumerator by Target----

summary_Date_RA_target <-  summary_Date_RA  %>%
 # filter (Total_Completed_by_DATE < 3  ) %>%
  group_by(`INTERVIEWER'S NAME`,Start_Date, End_Date, Days_worked) %>%
  summarise(
   # Days_worked = End_Date - Start_Date,
    Total_Completed_by_DATE = n(),
    Days_target_not_met = sum(Total_Completed_by_DATE < 3 )
  ) |>
select(`INTERVIEWER'S NAME`, Start_Date, End_Date, Days_worked, Total_Completed_by_DATE, Days_target_not_met)
#summary_Date_RA_target$Number_of_Work_Days <- 75
View(summary_Date_RA_target)



breaks <- c(3, 5)
colors <- c("green","red3")

dateplot<-ggplot(summary_Date_RA_target ,
                 aes(x=reorder(`INTERVIEWER'S NAME`, -Days_worked  ), y = Days_worked, Fill = Days_target_not_met) ) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Days_target_not_met)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Health Facility Survey",
    # subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "") + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = colors)
dateplot





#Summary by DATE----

summary_Date <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date` , Total_Completed_by_DATE )
summary_Date$MonthNum <- gsub("/","" ,substr(summary_Date$`Visit 1 Date`, 1, 2))
View(summary_Date)

#Summary by DATE and Enumerators----
dateplot<-ggplot(summary_Date, aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , group = MonthNum, color = Month)+
  geom_bar(stat = "identity", fill="#661133") +
  facet_wrap(~MonthNum, scales = "free_x") +
  #geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Cross Sectional Survey",
    # subtitle = "Plot of Achievement by Date",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Date", x = "") + theme(axis.text.x = element_text(angle = 90))
dateplot



#Summary by Dates All Months----
summary_Date_ <-  df1  %>%
#  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ) ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`)),
    InterviewCount = sum(nrow(`INTERVIEWER'S NAME`))
  ) |>
  select( `Visit 1 Date`, `INTERVIEWER'S NAME`, Total_Completed_by_DATE )
View(summary_Date_)

summary_Date_$MonthNum <- gsub("/","" ,substr(summary_Date_$`Visit 1 Date`, 1, 2))

summary_Month_ <-  summary_Date_  %>%
group_by(MonthNum, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)

  )

View(summary_Month_)



summary_Month <-  summary_Date_  %>%
  group_by(MonthNum) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)
    
  )

View(summary_Month)



#Summary table for Completion by Month by Interviewer ----
table_result_month <- summary_Month_  %>%
  spread(MonthNum, Total_Completed_in_Month)
View(table_result_month)


plot(summary_Month_$`INTERVIEWER'S NAME`,summary_Month_$Total_Completed_in_Month) 

# Plot by DATE All Month----
mdateplot <- ggplot(table_result_month,
                    aes(reorder(MonthNum, -Total_Completed_in_Month), Total_Completed_in_Month)) +
  geom_bar(stat = "identity", fill = "#661133") +
  geom_text(aes(label = paste("Month", MonthNum, " : ", signif(Total_Completed_in_Month))), 
            nudge_y = 0, vjust = -0.7) +
  labs(subtitle = "Plot of Achievement by Month")

dateplot1 <- mdateplot + theme_manuscript() + labs(y = "Total Completed by Month", x = "Month of Completion")
dateplot1



#BoxPlot by DATE All Month----
mdateplot_<-ggplot(summary_Month_,
                 aes(reorder(`INTERVIEWER'S NAME`,- Total_Completed_in_Month),Total_Completed_in_Month), size =10 ,  ) +
  geom_boxplot(fill ="grey" )+
  geom_text(aes(label = paste( MonthNum, "\n", signif(Total_Completed_in_Month))), 
            nudge_y = 0, vjust = -0.7) +#geom_text(aes(label = paste("Month", MonthNum) , nudge_y = 0, vjust = -0.7, )) +
  labs(#title = "Cross Sectional Survey",
   subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot1 <- mdateplot_ + theme_manuscript() +labs(y= "Total Completed by Month", x = "Month of Completion") + theme(axis.text.x = element_text(angle = 90))
dateplot1



# Plot by DATE All Month----
mdateplot <- ggplot(summary_Month,
                    aes(reorder(MonthNum, -Total_Completed_in_Month), Total_Completed_in_Month)) +
  geom_bar(stat = "identity", fill = "#661133") +
  geom_text(aes(label = paste("Month", MonthNum, " : ", signif(Total_Completed_in_Month))), 
            nudge_y = 0, vjust = -0.7) +
  labs(subtitle = "Plot of Achievement by Month")

dateplot1 <- mdateplot + theme_manuscript() + labs(y = "Total Completed by Month", x = "Month of Completion")
dateplot1




ggsave("output_plot.png", plot = dateplot1, width = 10, height = 8, units = "in", dpi = 300)
ggsave("output_plot.pdf", plot = dateplot1, width = 10, height = 8, units = "in")
update.packages(ask = FALSE)
theme(text = element_text(size = 12, family = "Arial"))

if(summary_Month_$MonthNum == "9" )
  summary_Month_$MonthName <- 


#Plot by DATE All Month----
dateplot<-ggplot(summary_Month_,
                 aes(MonthNum,Total_Completed_in_Month), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_in_Month)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =paste("Month", MonthName) ), nudge_y = 25, vjust = -0.7, ) +
  labs(title = "Plots of Achievement by Date",
       subtitle = "September 2023",
       caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript()  +labs(y= "Total Completed by Date", x = "Date of Interviews") + theme(axis.text.x = element_text(angle = 90))
dateplot




#Summary by September With Dates----

summary_Date_Sep <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ), str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Date_Sep)


#Plot by DATE All Month----
dateplot<-ggplot(summary_Date_Sep,
                 aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
#  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(title = "Plots of Achievement by Date",
     subtitle = "September 2023",
    caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript()  +labs(y= "Total Completed by Date", x = "Date of Interviews") + theme(axis.text.x = element_text(angle = 90))
dateplot



#Summary by October With Dates----

summary_Date_Oct <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ), str_detect(df1$`Visit 1 Date`, "^10") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Date_Oct)


#Plot by DATE All Month----
dateplot<-ggplot(summary_Date_Oct,
                 aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  #  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(title = "Plots of Achievement by Date",
       subtitle = "October 2023",
       caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript()  +labs(y= "Total Completed by Date", x = "Date of Interviews") + theme(axis.text.x = element_text(angle = 90))
dateplot




#Summary by November With Dates----

summary_Date_Nov <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ), str_detect(df1$`Visit 1 Date`, "^11") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Date_Oct)


#Plot by DATE All Month----
dateplot<-ggplot(summary_Date_Nov,
                 aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  #  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(title = "Plots of Achievement by Date",
       subtitle = "November 2023",
       caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript()  +labs(y= "Total Completed by Date", x = "Date of Interviews") + theme(axis.text.x = element_text(angle = 90))
dateplot




#Summary of HH Total by Those Tested with Interviewer name----

summary_hh <-  df_filled3  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ) ) %>%
  group_by(`Serial Number`, `Name of Household Head`,`INTERVIEWER'S NAME`) %>%
  summarise(
   Num_HH = sum(!is.na(`Relationship to Household   Head (HH): What is the relationship of (NAME) to the HH`)),
   Num_DBS = sum(!is.na(`q303: DRIED BLOOD SAMPLE COLLECTED`)),
  # Interviewer = `INTERVIEWER'S NAME`
  )



View(summary_hh)

file_path <- "/Users/user/Downloads/summary_by_hh.csv"

# Export the data frame to a CSV file
write.csv(summary_hh, file = file_path, row.names = TRUE)


#Summary by Dates Sept----

summary_Sep_Date_ <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Sep_Date_)

#Summary by September With Dates----

summary_Sep_Date <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, `INTERVIEWER'S NAME` , Total_Completed_by_DATE )

View(summary_Sep_Date)


#Summary by September with Interviewer name----

summary_Sep <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    Completed_in_September = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`INTERVIEWER'S NAME` , Completed_in_September )

View(summary_Sep)



#Summary by Enumeration Area/Cluster Number with Interviewer name----

summary_Sep <-  df_filled3  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    Completed_in_September = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`INTERVIEWER'S NAME` , Completed_in_September )

View(summary_Sep)



rows_with_keyword <- which(grepl("/", df_filled3$`Enumeration Area/Cluster`, ignore.case = TRUE))

# Print the result
if (length(rows_with_keyword) > 0) {
  cat("Keyword found in rows:", rows_with_keyword, "\n")
} else {
  cat("Keyword not found in any row.\n")
}


#Summary by DATE and Enumerators----
dateplot<-ggplot(filtered_df,
                 aes(reorder(`Visit 1 Date`, -Total_Completed_by_DATE ),Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Cross Sectional Survey",
    # subtitle = "Plot of Achievement by Date",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Date", x = "")
dateplot



library(openxlsx)

# Create a new Excel workbook
wb <- createWorkbook()

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Summary Date RAs")
writeData(wb, sheet = "Summary Date RAs", x = table_result , row.names = TRUE)

addWorksheet(wb, sheetName = "Summary by Target 3+ Perday")
writeData(wb, sheet = "Summary by Target 3+ Perday", x = summary_Date_RA_target, row.names = TRUE)

addWorksheet(wb, sheetName = "Summary by Date by RAs")
writeData(wb, sheet = "Summary by Date by RAs", x = summary_Date_, row.names = TRUE)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Summary by Month by RAs")
writeData(wb, sheet = "Summary by Month by RAs", x = table_result_month, row.names = TRUE)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Whole Data")
writeData(wb, sheet = "Whole Data", x = df_filled3, row.names = TRUE)


# Add dataframes to different sheets
addWorksheet(wb, sheetName = "summary_Children_0_to_10")
writeData(wb, sheet = "summary_Children_0_to_10", x = summary_Children_0_to_10, row.names = TRUE)


# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Missing Columns")
writeData(wb, sheet = "Missing Columns", x = column_missingkn , row.names = TRUE)

addWorksheet(wb, sheetName = "Missing Columns")
writeData(wb, sheet = "Missing Columns", x = column_missing, row.names = TRUE)

addWorksheet(wb, sheetName = "Missing LGA Records")
writeData(wb, sheet = "Missing LGA Records", x = missing_lga)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Missing Wards")
writeData(wb, sheet = "Missing Wards", x = missing_ward)

addWorksheet(wb, sheetName = "Missing Settlement Type")
writeData(wb, sheet = "Missing Settlement Type", x = missing_settlement)

addWorksheet(wb, sheetName = "Missing Community Name")
writeData(wb, sheet = "Missing Community Name", x = missing_comm)

addWorksheet(wb, sheetName = "Missing EA ")
writeData(wb, sheet = "Missing EA , x = missing_ea")


# Save the workbook to a file
saveWorkbook(wb, file = "/Users/user/Downloads/summary_rows.xlsx")






#Loading the map

library(sf)
library(ggplot2)
library(ggmap)
library(plotly)
shape <- read_sf(dsn = "/Users/user/Downloads/Kano_metro_ward_sixLGAs/", layer = "Kano_metro_ward_sixLGAs")


df_filled3_new <- df_filled3 %>%

  filter(!(is.na(`HOUSEHOLD COORDINATE- Latitude`)), !(is.na(`HOUSEHOLD COORDINATE- Longitude`))) |>
  select(`INTERVIEWER'S NAME`, `HOUSEHOLD COORDINATE- Latitude`,`HOUSEHOLD COORDINATE- Longitude`,`Settlement Type`,`Ward` )
  
View(df_filled3_new)


df_filled3_cleaned <- df_filled3_new[complete.cases(df_filled3[, c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude")]), ]
gps_sf <- st_as_sf(df_filled3_cleaned, coords = c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude"))




# Filter rows with complete coordinates
df_filled3_cleaned <- df_filled3_new %>%
  filter(!is.na(`HOUSEHOLD COORDINATE- Latitude`) & !is.na(`HOUSEHOLD COORDINATE- Longitude`))

# Convert to sf object
gps_sf <- st_as_sf(df_filled3_cleaned, coords = c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude"))

# Filter rows with complete coordinates using df_filled3_cleaned
df_filled3_cleaned <- df_filled3_new[complete.cases(df_filled3_cleaned[, c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude")]), ]






gps_sf <- st_as_sf(df_filled3_new, coords = c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude"))



#gps_sf <- st_as_sf(df_filled3, coords = c("HOUSEHOLD COORDINATE- Longitude", "HOUSEHOLD COORDINATE- Latitude"), crs = st_crs(shape))

ggplot() +
  geom_sf(data = shape) +
  geom_sf(data = gps_sf, color = "red", size = 3) +
  labs(title = "GPS Coordinates on Shapefile") +
  theme_minimal()



ggplot() +
  geom_sf(data = shape) +
  geom_sf(data = gps_sf, color = "red", size = 3) +
  labs(title = "GPS Coordinates on Shapefile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_sf(xlim = c(min(gps_sf$`HOUSEHOLD COORDINATE- Longitude`, na.rm = TRUE), max(gps_sf$`HOUSEHOLD COORDINATE- Longitude`, na.rm = TRUE)),
           ylim = c(min(gps_sf$`HOUSEHOLD COORDINATE- Latitude`, na.rm = TRUE), max(gps_sf$`HOUSEHOLD COORDINATE- Latitude`, na.rm = TRUE)))





breaks <- c(0, 5, 8, 15, 25, 50)
colors <- c("green4","green", "yellow",  "orange", "red", "red3")
#Plotting the map
mymapplot<- ggplot(mymap2) +
  geom_sf(data=mymap2, mapping = aes(fill=cut(Total_Count, breaks = breaks), geometry=geometry, colors="white"), show.legend = TRUE) +
  geom_sf_text(data=mymap2,aes(label = WardName, geometry=geometry))+
  scale_fill_manual(values = colors) +
  #  geom_sf_text(data=mymap2,aes(label = Total_Count, geometry=geometry))+
  theme_manuscript()

mymapplot



#Plotting the map
ggplot() +
  geom_sf(data=mymap2, mapping = aes(fill= cut(Total_Count, breaks = breaks) , geometry=geometry, colors="white"), show.legend = TRUE) +
  # geom_sf_text(data=mymap2,aes(label = Total_Count, geometry=geometry)) +
  geom_sf_text(data=mymap2,aes(label = NAME_1 , geometry=geometry) ) +
  scale_fill_manual(values = colors)
#+geom_point(data = df1, aes(x="latitude", y="longitude"))
mymapplot


p <-  mymapplot + geom_point(aes(df1$latitude, df1$longitude))

ggplotly(p)





##Dummy Data set---

# Set the seed for reproducibility
set.seed(123)

# Number of records
num_records <- 1500

# Create a dummy dataframe
dummy_data <- data.frame(
  Serial_Number = 1:num_records,
  State = sample(c("StateA", "StateB", "StateC"), num_records, replace = TRUE),
  LOCAL_GOVT_AREA = sample(c("LGA1", "LGA2", "LGA3"), num_records, replace = TRUE),
  NAME_OF_HEALTH_FACILITY = sample(c("HospitalA", "HospitalB", "HospitalC"), num_records, replace = TRUE),
  RESPONDENTS_ADDRESS = paste0("No 1", "Kamkamba Street, Giginyu", 1:num_records),
  Date = sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = "1 day"), num_records, replace = TRUE),
  INTERVIEWERS_NAME = paste0("Interviewer", sample(1:5, num_records, replace = TRUE)),
  INTERVIEWERS_PHONE_NO = sample(1000000000:9999999999, num_records, replace = TRUE),
  SUPERVISORS_NAME = paste0("Supervisor", sample(1:3, num_records, replace = TRUE)),
  FIELD_EDITOR = paste0("Editor", sample(1:2, num_records, replace = TRUE)),
  Complete = sample(c("Yes", "No"), num_records, replace = TRUE),
  q100_Years = sample(18:80, num_records, replace = TRUE),
  q100i_Months = sample(0:11, num_records, replace = TRUE),
  # ... (add more columns as needed)
  
  stringsAsFactors = FALSE
)



# Load required library
library(tibble)

# Set seed for reproducibility
set.seed(123)

# Number of records
num_records <- 1500

# Function to generate random addresses
generate_random_addresses <- function(n) {
  street_names <- c("Babba Kanya Street, Kwanar Ganduje", "Dolo Street, Dala", "Kwanar Ganduje, ShaShaSha", "bakin Kwalta, Gwale", "Tambarin Dala Nassarawa", "Bakin Tasha, Tarauni", "Gidan Samari, Ungogo", "Layin Dan Pullo, Kumbotso")
  lgas <- c("Fagge", "Kano Municipal", "Tarauni", "Dawakin Kudu", "Kano Municipal", "Nassarawa", "Ungogo", "Kumbotso")
  
  tibble(
    Serial_Number = 1:n,
    State = "Kano",
    LOCAL_GOVT_AREA = sample(lgas, n, replace = TRUE),
    NAME_OF_HEALTH_FACILITY = sample(c("HospitalA", "HospitalB", "HospitalC", "Hospital D"), num_records, replace = TRUE),
    RESPONDENTS_ADDRESS = sample(1:100, n, replace = TRUE),paste(sample(street_names, n, replace = TRUE)),
    Wards = paste("Ward", sample(1:5, n, replace = TRUE)),
    Date = as.Date(sample(seq(as.Date('2022-01-01'), as.Date('2023-01-01'), by="day"), n, replace = TRUE), format="%Y-%m-%d"),
    INTERVIEWERS_NAME = paste0("Interviewer", sample(1:20, num_records, replace = TRUE)),    INTERVIEWERS_PHONE_NO = sample(sprintf("080%d", sample(10000000:99999999, n, replace = TRUE)), n, replace = TRUE),
    SUPERVISORS_NAME = paste0("Supervisor", sample(1:5, num_records, replace = TRUE)),
    FIELD_EDITOR = paste("Editor"),
    Complete = sample(c("Yes", "No"), n, replace = TRUE)
  )
}

# Generate random addresses
dummy_data <- generate_random_addresses(num_records)

# Display the generated dummy data
print(dummy_data)





file_path <- "/Users/user/Downloads/Dummy_Reviewed.csv"

# Export the data frame to a CSV file
write.csv(dummy_data, file = file_path, row.names = TRUE)




# View the first few rows of the dummy dataframe
head(dummy_data)
