# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(dplyr)
library("RColorBrewer")
library(patchwork)
library(png)
library(forcats)

source('f_rosetta_manager.R')

date_cutoff = ymd(20000101) #YEar 2000 is too early to examin work done since we have no data

# Load all the needed CSV!
survey_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/SURVEY_SPP_piet4.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")


rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/work_done_formatted_CM.csv")
work_types = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Work_Types.csv")
work_type_generic = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Work_Types_Generic.csv")


# Makes all Blanks NA and then drops all NAs from end date
rw_work_done_df[rw_work_done_df==""] = NA
rw_work_done_df = rw_work_done_df %>% drop_na(Date_End)
rw_work_done_df$Date_End = mdy(rw_work_done_df$Date_End)
rw_work_done_df$Date_Start = mdy(rw_work_done_df$Date_Start)
#LEts also rename all the site codes to Peits site code names.  This is fastest done here
rw_work_done_df$SiteCode = sapply(rw_work_done_df$SiteCode, park_to_peit_code) #This renames them all to the Peit style Names
rw_work_done_df = rw_work_done_df %>% drop_na(SiteCode) #And lets drop all the sites that dont exist in Peits data...
rw_work_done_df = merge(rw_work_done_df, work_types, by = "Work_Done") #Lets also add the work type names
rw_work_done_df = merge(rw_work_done_df, work_type_generic, by = "Generic_ID")


#Convert the survey df data to better date format
survey_df$Date = ymd(survey_df$Date)
survey_df = survey_df %>%
  filter(Date > date_cutoff)

#Now I need to filter all of these CSV to only include the data that is applicable (aka ponds that work was done to... for now)
#First lets drop locations where work was done way outside the timing of our research scope
rw_work_done_df = rw_work_done_df %>%
  filter(Date_End > date_cutoff)

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


ggplot() +
  geom_line(data = top_sum_survey_df, mapping = aes(x = Date, y = Count, color = SppCode, shape = SppCode))+
  geom_point(data = top_sum_survey_df, mapping = aes(x = Date, y = Count, color = SppCode, shape = SppCode))+
  geom_vline(data = top_rw_work_done_df, aes(xintercept = Date_End, linetype = Generic_Description), linewidth=.75) + 
  scale_color_manual(values = brewer.pal(n=10, name= "Spectral")) + 
  #geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid", linewidth=.75) + 
  facet_wrap(~ SiteCode) +
  labs(x = "Date", y = "Count", title = "Top Sites, Amphibians Count vs Time") +
  scale_x_date(date_labels = "%Y-%m-%d")




work_done_count = rw_work_done_df %>%
  group_by(SiteCode) %>%
  mutate(total_intervention_count = n()) %>%
  group_by(Generic_Description, total_intervention_count) %>%
  count(SiteCode, name = "intervention_count")

legend_vals = sort(unique(work_done_count$Generic_Description))
legend_vals[length(legend_vals) + 1] = "Top Sites"
rect_alpha = 0.2
rect_color = "blue"
bar_color = brewer.pal(n=9, name= "Greys")[5:9]
bar_color[length(legend_vals) + 1] = rect_color
p1 = ggplot() +
  geom_col(work_done_count, mapping = aes(x = fct_reorder(SiteCode, total_intervention_count), y = intervention_count, fill = Generic_Description)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  theme_minimal() +
  scale_fill_manual(values = bar_color,
                    name = "Restoration Classification",
                    limits = legend_vals,
                    labels = legend_vals)  +
  labs(x = "Site Code", y = "Number of Restoration Projects",
       fill = "Restoration Project Types") +
  theme(axis.text.x = element_text(angle = 90)) 
p1
# Lets add in the top sites 
p1 +
  annotate("rect", xmin = 39.5, xmax = 40.5, ymin = 0, ymax = 6, fill = rect_color, alpha = rect_alpha) + #MUD65
  annotate("rect", xmin = 40.5, xmax = 41.5, ymin = 0, ymax = 6, fill = rect_color, alpha = rect_alpha) + #MUD65
  annotate("rect", xmin = 44.5, xmax = 45.5, ymin = 0, ymax = 10, fill = rect_color, alpha = rect_alpha)+ #MUD67
  annotate("rect", xmin = 44.5, xmax = 45.5, ymin = 0, ymax = 10, fill = rect_color, alpha = rect_alpha)+ #MUD67
  annotate("rect", xmin = 15.5, xmax = 16.5, ymin = 0, ymax = 2, fill = rect_color, alpha = rect_alpha)+ #GDPND005
  annotate("rect", xmin = 16.5, xmax = 17.5, ymin = 0, ymax = 2, fill = rect_color, alpha = rect_alpha)+ #GDPND006
  annotate("rect", xmin = 17.5, xmax = 18.5, ymin = 0, ymax = 2, fill = rect_color, alpha = rect_alpha)+ #GDPND009
  annotate("rect", xmin = 18.5, xmax = 19.5, ymin = 0, ymax = 2, fill = rect_color, alpha = rect_alpha)+ #GDPND014
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = rect_color, alpha = rect_alpha) +  #MUD31
  annotate("rect", xmin = 29.5, xmax = 30.5, ymin = 0, ymax = 3, fill = rect_color, alpha = rect_alpha) + #MUD74
  annotate("rect", xmin = 35.5, xmax = 36.5, ymin = 0, ymax = 4, fill = rect_color, alpha = rect_alpha)+ #MUD77
  annotate("rect", xmin = 43.5, xmax = 44.5, ymin = 0, ymax = 8, fill = rect_color, alpha = rect_alpha)+ #MUD67
  annotate("rect", xmin = 38.5, xmax = 39.5, ymin = 0, ymax = 5, fill = rect_color, alpha = rect_alpha)+ #MUDRS
  annotate("rect", xmin = 25.5, xmax = 26.5, ymin = 0, ymax = 2, fill = rect_color, alpha = rect_alpha)+ #PRNTHIDK
  annotate("rect", xmin = 11.5, xmax = 12.5, ymin = 0, ymax = 1, fill = rect_color, alpha = rect_alpha)+ #PRPND009
  annotate("rect", xmin = 12.5, xmax = 13.5, ymin = 0, ymax = 1, fill = rect_color, alpha = rect_alpha) +  #PRPND010
  scale_color_manual(name='Regression Model',
                     breaks=c('Linear', 'Quadratic', 'Cubic'))
  