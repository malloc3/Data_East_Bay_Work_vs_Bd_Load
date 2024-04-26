# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(dplyr)


# Load all the needed CSV!
survey_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/SURVEY_SPP_piet4.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
#wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")


#rw_work_types_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Work_Types.csv")
#rw_Observer_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/EB_Resto_Work_Done.csv")
rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/EB_Resto_Work_Done.csv")

# Makes all Blanks NA and then drops all NAs from end date
# Also converts all to upper case
rw_work_done_df[rw_work_done_df==""] = NA
rw_work_done_df = rw_work_done_df %>% drop_na(Date_End)
rw_work_done_df$SiteCode = toupper(rw_work_done_df$SiteCode)
rw_work_done_df$Date_End = mdy(rw_work_done_df$Date_End)
rw_work_done_df$Date_Start = mdy(rw_work_done_df$Date_Start)

#Convert the survey df data to better date format
survey_df$Date = ymd(survey_df$Date)

#Now I need to filter all of these CSV to only include the data that is applicable (aka ponds that work was done to... for now)

#Lets first get the list of ponds that have done work on
site_codes_list = unique(rw_work_done_df$SiteCode)
reduced_survey_df <- survey_df[survey_df$SiteCode %in% site_codes_list, ] #only the sites where work has been done!

# Lets aggregate these data based on date, AssmetCode, and SppGrp
sum_survey_df <- reduced_survey_df %>% 
  group_by(SiteCode, AssmtCode, Date, SppGrp) %>% 
  summarize(Count = sum(Count))

sum_survey_df = merge(sum_survey_df, rw_work_done_df, by = "SiteCode")


#Manually dropping sites where work happend but we have no data on species
dropped_sum_survey_df = sum_survey_df %>% 
  filter(!(SiteCode %in% c("BDPND015", "MTPND008", "MTPND009", "OHPND033", "TIPND001", "TIPND002", "BDPND006")))

ggplot(data = dropped_sum_survey_df) +
  geom_line(mapping = aes(x = Date, y = Count, color = SppGrp))+
  geom_point(mapping = aes(x = Date, y = Count, color = SppGrp))+
  geom_vline(aes(xintercept = Date_End), color = "black", linewidth=.75) + 
  geom_vline(aes(xintercept = Date_Start), color = "blue", linewidth=.75) + 
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")






#Okay now I want to find my TOP sites.  The sites with the most number of samples
# Group by Site and count the number of measurements for each site
site_counts <- survey_df %>%
  group_by(SiteCode) %>%
  summarise(Count = n())

# Select the top 6 sites with the most measurements
top_sites <- site_counts %>%
  top_n(6, Count)

# Filter the original dataframe based on the top 6 sites
top_survey_sites <- survey_df %>%
  filter(SiteCode %in% top_sites$SiteCode)

# Lets aggregate these data based on date, AssmetCode, and SppGrp
top_sum_survey_df <- top_survey_sites %>% 
  group_by(SiteCode, AssmtCode, Date, SppGrp) %>% 
  summarize(Count = sum(Count))

ggplot(data = top_sum_survey_df) +
  geom_line(mapping = aes(x = Date, y = Count, color = SppGrp))+
  geom_point(mapping = aes(x = Date, y = Count, color = SppGrp))+
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "Top Sites, Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")







