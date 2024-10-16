# Required imports
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date handling
library(ggplot2)    # For creating plots
library(patchwork)  # For combining multiple plots
library(viridis)    # For color palettes


# Set the folder path
folder_path <- "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/cleaned_data"

#The file path for where to save plots
fig_save_path <- "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/figures"

#Load all the CSV files as dataframes
wetland <- read.csv(paste(folder_path, "wetland_full_clean_20240817.csv", sep = "/"))

wetland <- wetland %>% 
    mutate(date = lubridate::mdy(Date_End)) %>%






sitecode <- "PRPND014"

# Filter wetland data for sitecode "PRPND0015"
wetland_filtered <- wetland %>%
  filter(sitecode == sitecode)

# Convert date to Date type if it's not already
wetland_filtered$date <- as.Date(wetland_filtered$date)

date_range <- range(wetland_filtered$date, na.rm = TRUE)

# Replace NA values in pond_area_m with 0
wetland_filtered <- wetland_filtered %>%
  mutate(pond_area_m = replace_na(pond_area_m, 0))


# Create the plot
ggplot(wetland_filtered, aes(x = date, y = pond_area_m)) +
  geom_point(aes(color = dry), size = 3) +  # Add larger points
  geom_line() +   # Add line
  labs(title = paste("Pond Area Over Time for", sitecode),
       x = "Date",
       y = "Pond Area (m²)") +
  theme_minimal() +
  scale_x_date(limits = date_range, date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save the plot
ggsave(filename = file.path(fig_save_path, paste(sitecode, "_pond_area_over_time.png", sep = "")), 
       width = 10, 
       height = 6, 
       dpi = 300)


