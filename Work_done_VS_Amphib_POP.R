# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(patchwork)


source("f_get_survey_stats.R")
source("f_get_swab_statistics.R")
source("f_generate_timeseries_df.R")
source("f_rosetta_manager.R")
# Load all the needed CSV!
survey_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/SURVEY_SPP_piet4.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")


rw_work_types_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Work_Types.csv")
rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/work_done_formatted_CM.csv")

# Makes all Blanks NA and then drops all NAs from end date
# Also converts all to upper case
rw_work_done_df[rw_work_done_df==""] = NA
rw_work_done_df = rw_work_done_df %>% drop_na(Date_End)

#rw_work_done_df$SiteCode = toupper(rw_work_done_df$SiteCode) #Sets them all to uppcase but not needed with the rosetta

rw_work_done_df$Date_End = mdy(rw_work_done_df$Date_End)
rw_work_done_df$Date_Start = mdy(rw_work_done_df$Date_Start)

#Now I need to filter all of these CSV to only include the data that is applicable (aka ponds that work was done to... for now)

#Lets first get the list of ponds that have done work on
site_codes_list = unique(rw_work_done_df$SiteCode)

# Then lets reduce the wetland file to only the overlapping sites.  This helps reduce time later
# Remember we have to put the site codes list through the rosetta.
rw_wetland_info_df <- wetland_info_df[wetland_info_df$SiteCode %in% lapply(site_codes_list, park_to_peit_code), ]

non_existant_sites = c()
existing_sites = c()
existing_sites_plots = list()
plot_pos = 0
for (i in 1:length(site_codes_list)){
  parks_site_code = site_codes_list[i]

  site_amphib_pop_over_time = generate_amphib_pop_over_time(park_to_peit_code(parks_site_code), rw_wetland_info_df, survey_df, FALSE)
  
  site_work_over_time = generate_date_work_df(parks_site_code, rw_work_done_df)
  
  print(site_amphib_pop_over_time)
  print(site_work_over_time)
  
  if(is.data.frame(site_amphib_pop_over_time) & is.data.frame(site_work_over_time)){
    plot_pos = plot_pos + 1

    existing_sites = c(existing_sites, parks_site_code)
    
    pond_plot = ggplot() +
        geom_point(data = site_amphib_pop_over_time, aes(x = date, y = total_amphib_count, color = "Amphibians")) +
        geom_line(data = site_amphib_pop_over_time, aes(x = date, y = total_amphib_count, color = "Amphibians")) +
        geom_point(data = site_amphib_pop_over_time, aes(x = date, y = total_invert_count, color = "Inverts")) +
        geom_line(data = site_amphib_pop_over_time, aes(x = date, y = total_invert_count, color = "Inverts")) +
        geom_vline(data = site_work_over_time, aes(xintercept = Date_End, color = "Work Done"), size=.75) + 
        labs(x = "Dates", y = "Raw Count", title = parks_site_code) + 
        scale_color_manual(breaks = c("Amphibians", "Inverts", "Work Done"), values=c("red", "blue", "black"))
    
    existing_sites_plots[[plot_pos]] = pond_plot # Add the plots to a list
    
  }else{
    non_existant_sites = c(non_existant_sites, parks_site_code)
  }
}

#print(length(existing_sites_plots))
num_col = 3
num_rows = ceiling(length(existing_sites_plots))
patchwork::wrap_plots(existing_sites_plots, nrow = 7, ncol = 3)

print("Existing Sites")
print(length(existing_sites))
print("Non Existant Sites")
print(length(non_existant_sites))

