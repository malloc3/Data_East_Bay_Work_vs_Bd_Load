# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)


source("f_get_swab_statistics.R")
source("f_generate_date_load_df.R")
# Load all the needed CSV!
am_bd_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Amphib_Bd_pietv10.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")


rw_work_types_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Work_Types.csv")
rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/EB_Resto_Work_Done.csv")
rw_Observer_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/EB_Resto_Work_Done.csv")

# Makes all Blanks NA and then drops all NAs from end date
# Also converts all to upper case
rw_work_done_df[rw_work_done_df==""] = NA
rw_work_done_df = rw_work_done_df %>% drop_na(Date_End)
rw_work_done_df$SiteCode = toupper(rw_work_done_df$SiteCode)
rw_work_done_df$Date_End = mdy(rw_work_done_df$Date_End)
rw_work_done_df$Date_Start = mdy(rw_work_done_df$Date_Start)

#Now I need to filter all of these CSV to only include the data that is applicable (aka ponds that work was done to... for now)

#Lets first get the list of ponds that have done work on
site_codes_list = unique(rw_work_done_df$SiteCode)
rw_wetland_info_df <- wetland_info_df[wetland_info_df$SiteCode %in% site_codes_list, ]

#Okay now lets focus on only one pond (we will make it about more ponds later!!)
non_existant_sites = c()
existing_sites = c()
for (i in 1:length(site_codes_list)){
  site_code = site_codes_list[i]
  site_bd_over_time = generate_date_load_df(site_code, rw_wetland_info_df, am_bd_df, FALSE)
  site_work_over_time = generate_date_work_df(site_code, rw_work_done_df)
  if(is.data.frame(site_bd_over_time) & is.data.frame(site_work_over_time)){
    existing_sites = c(existing_sites, site_code)
    print(
      ggplot() +
        geom_point(data = site_bd_over_time, aes(x = date, y = min_ze), color = "blue") + #Plot BD data
        geom_line(data = site_bd_over_time, aes(x = date, y = min_ze), color = "blue") + #Plot BD data
        geom_vline(data = site_work_over_time, aes(xintercept = Date_End), color = "red", size=.75) + 
        labs(x = "Dates", y = "Percent Positive", title = site_code)
    )
  }else{
    non_existant_sites = c(non_existant_sites, site_code)
  }
}

print("Existing Sites")
print(length(existing_sites))
print("Non Existant Sites")
print(length(non_existant_sites))

