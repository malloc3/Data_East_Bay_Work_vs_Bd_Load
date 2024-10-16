#This script takes the Parks site codes and coorelates them with site codes on other data via 
#Geographic closeness.  If a site is not in the other data it will be listed as NA
# IF the site codes are different the Flag column will be set to TRUE


library(tidyr)
library(geosphere)
library(measurements)

source("f_find_by_lat_long.r")

#DF1 the dataframe with sites that we want to find in DF2
site_codes_filepath = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Site_CODE_MASTER.csv"
site_codes_save_path = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Site_CODE_MASTER.csv"

#DF2 teh dataframe with the data that we want to find sites at
df2_file_path = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv"

max_site_distance = 50 #in meters



site_codes = read.csv(site_codes_filepath)
df2 = read.csv(df2_file_path)

for(i in 1:nrow(site_codes)){
  site = site_codes[i,]
  park_lat = site$Park_Lat
  park_lon = site$Park_Lon
  
  new_site = find_by_lat_lon(df2, park_lat, park_lon, max_site_distance)
  new_site_id = new_site[[1]]
  distance = new_site[[2]]
  new_lat = new_site[[3]]
  new_lon = new_site[[4]]
  
  site_codes[i, "Peit_Site_Code"] = new_site_id
  site_codes[i, "Peit_Distance"] = distance
  site_codes[i, "Peit_Lat"] = new_lat
  site_codes[i, "Peit_Lon"] = new_lon
}


write.csv(site_codes, file = site_codes_save_path)

  



