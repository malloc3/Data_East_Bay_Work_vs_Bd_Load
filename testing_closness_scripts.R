library(tidyr)
library(geosphere)

source("f_find_by_lat_long.r")


rw_work_done_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/EBMUD_Pond_Attributes.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")

rw_work_done_df$new_id = NA
rw_work_done_df$dist_from_given = NA

for (i in 1:nrow(rw_work_done_df)){
  row = rw_work_done_df[i,]
  new_site = find_by_lat_lon(site_info_df, row$Y, row$X, 100)
  new_site_id = new_site[[1]]
  distance = new_site[[2]]
  new_lat = new_site[[3]]
  new_lon = new_site[[3]]
  rw_work_done_df[i, "new_id"] = new_site_id
  rw_work_done_df[i, "dist_from_given"] = distance
}
