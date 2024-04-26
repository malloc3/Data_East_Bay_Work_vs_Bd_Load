# This script will load our Field DATA CSVs
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(dplyr)


# Load all the needed CSV!
survey_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/SURVEY_SPP_piet4.csv")
site_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Site_Info_piet6.csv")
water_qual_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Water_Quality_piet5.csv")
wetland_info_df = read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Raw_Data_East_Bay_Work_vs_Load/Wetland_Info_piet9.csv")

par(mfrow = c(2,2))
hist(water_qual_df$WaterTemp_C, main = "Water Temp", xlab = "Temp C", xlim = c(0, 40), breaks = 15)
hist(water_qual_df$pH, main = "pH", xlab = "pH", xlim = c(0, 12), breaks = 12)
hist(water_qual_df$Salinity/1000, main = "Salinity", xlab = "Salinity PPT",  xlim = c(0, 2), breaks = 200)


drought_years <- survey_df[survey_df$Year %in% c(2020, 2021, 2022), ]
assmt_codes = drought_years$AssmtCode

drought_years_water_qual = water_qual_df[water_qual_df$AssmtCode %in% assmt_codes, ]
par(mfrow = c(2,2))
hist(drought_years_water_qual$WaterTemp_C, main = "Water Temp", xlab = "Temp C", xlim = c(0, 40), breaks = 15)
hist(drought_years_water_qual$pH, main = "pH", xlab = "pH", xlim = c(0, 12), breaks = 12)
hist(drought_years_water_qual$Salinity/1000, main = "Salinity", xlab = "Salinity PPT",  xlim = c(0, 2), breaks = 60)
