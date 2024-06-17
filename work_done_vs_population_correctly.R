# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(dplyr)
library("RColorBrewer")

source('f_rosetta_manager.R')

# Load all the needed CSV!
survey_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/SURVEY_SPP_piet4.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
#wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")


rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/work_done_formatted_CM.csv")
work_types = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Work_Types.csv")


# Makes all Blanks NA and then drops all NAs from end date
rw_work_done_df[rw_work_done_df==""] = NA
rw_work_done_df = rw_work_done_df %>% drop_na(Date_End)
rw_work_done_df$Date_End = mdy(rw_work_done_df$Date_End)
rw_work_done_df$Date_Start = mdy(rw_work_done_df$Date_Start)
#LEts also rename all the site codes to Peits site code names.  This is fastest done here
rw_work_done_df$SiteCode = sapply(rw_work_done_df$SiteCode, park_to_peit_code) #This renames them all to the Peit style Names
rw_work_done_df = rw_work_done_df %>% drop_na(SiteCode) #And lets drop all the sites that dont exist in Peits data...
#Lets also add the work type names long form
rw_work_done_df = merge(rw_work_done_df, work_types, by = "Work_Done")


#Convert the survey df data to better date format
survey_df$Date = ymd(survey_df$Date)

#Now I need to filter all of these CSV to only include the data that is applicable (aka ponds that work was done to... for now)

#Lets first get the list of ponds that have done work on and that have Field Data on
#We already used Rosetta to convert the SiteCodes to match Peits data
reduced_survey_df <- survey_df[survey_df$SiteCode %in% unique(rw_work_done_df$SiteCode), ] #reducing to sites were we have data



# Lets aggregate these data based on date, AssmetCode, and SppGrp
sum_survey_df <- reduced_survey_df %>% 
  group_by(SiteCode, AssmtCode, Date, SppGrp, SppCode) %>% 
  filter(SppGrp == "AMPHIBIAN") %>%
  summarize(Count = sum(Count))

sum_survey_df = merge(sum_survey_df, rw_work_done_df, by = "SiteCode")



ggplot(data = sum_survey_df) +
  geom_line(mapping = aes(x = Date, y = Count, color = SppCode))+
  geom_point(mapping = aes(x = Date, y = Count, color = SppCode))+
  geom_vline(aes(xintercept = Date_End), color = "black", linetype = "dashed", linewidth=.75) + 
  #geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid", linewidth=.75) + 
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")






#Okay now I want to find my TOP sites.  The sites with the most number of samples
# Group by Site and count the number of measurements for each site
site_counts <- reduced_survey_df %>%
  group_by(SiteCode) %>%
  summarise(Count = n())

# Select the top 6 sites with the most measurements
top_sites <- site_counts %>%
  top_n(15, Count)

# Filter the original dataframe based on the top 6 sites
top_survey_sites <- reduced_survey_df %>%
  filter(SiteCode %in% top_sites$SiteCode)

# Lets aggregate these data based on date, AssmetCode, and SppGrp
top_sum_survey_df <- top_survey_sites %>% 
  group_by(SiteCode, AssmtCode, Date, SppGrp, SppCode) %>%
  filter(SppGrp == "AMPHIBIAN") %>%
  summarize(Count = sum(Count))

#Now lets not forget out work done data!
top_rw_work_done_df = rw_work_done_df %>%
  filter(SiteCode %in% top_sites$SiteCode)

#LEts drop the snail and fish.
#top_amphib_survey_df = top_sum_survey_df %>%
#  filter(SppGrp != "FISH") %>%
#  filter(SppGrp != "SNAIL") %>%
#  filter(SppGrp != "") %>%
#  filter(SppGrp != "CRAYFISH") %>%
#  filter(SppGrp != "INVERTEBRATE")

# Now lets add our work done to these too!

ggplot() +
  geom_line(data = top_sum_survey_df, mapping = aes(x = Date, y = Count, color = SppCode, shape = SppCode))+
  geom_point(data = top_sum_survey_df, mapping = aes(x = Date, y = Count, color = SppCode, shape = SppCode))+
  geom_vline(data = top_rw_work_done_df, aes(xintercept = Date_End, linetype = Description), linewidth=.75) + 
  scale_color_manual(values = brewer.pal(n=10, name= "Spectral")) + 
  #geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid", linewidth=.75) + 
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "Top Sites, Amphibians Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")






# LEts make a different plot with all the ponds and just amphibians!
all_amphibs_survey_df = sum_survey_df %>%
  filter(SppGrp != "FISH") %>%
  filter(SppGrp != "SNAIL") %>%
  filter(SppGrp != "INVERTEBRATE")

all_amphib_rw_work_done_df = rw_work_done_df %>%
  filter(SiteCode %in% all_amphibs_survey_df$SiteCode)

ggplot() +
  geom_line(data = all_amphibs_survey_df, mapping = aes(x = Date, y = Count), color = "blue")+
  geom_point(data = all_amphibs_survey_df, mapping = aes(x = Date, y = Count, shape = SppGrp), color = "blue")+
  geom_vline(data = all_amphib_rw_work_done_df, aes(xintercept = Date_End, color = Description), linewidth=.75) + 
  scale_color_manual(values = brewer.pal(n=10, name= "Spectral")) + 
  #geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid", linewidth=.75) + 
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "All Sites, Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")



top_sites_full_data = site_info_df %>%
  filter(SiteCode %in% top_sites$SiteCode)


