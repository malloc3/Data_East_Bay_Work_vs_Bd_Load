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
work_done <- read.csv(paste(folder_path, "cleaned_work_done_data_cm.csv", sep = "/"))
amphib_bd <- read.csv(paste(folder_path, "amphib_bd_full_clean_20240817.csv", sep = "/"))
amphib_dissect <- read.csv(paste(folder_path, "amphib_dissect_full_clean_20240817.csv", sep = "/"))
amphib_parasite <- read.csv(paste(folder_path, "amphib_parasite_full_clean_20240817.csv", sep = "/"))
bd_full <- read.csv(paste(folder_path, "bd_full_clean_20240817.csv", sep = "/"))
netting_info <- read.csv(paste(folder_path, "netting_info_full_clean_20240817.csv", sep = "/"))
site_full <- read.csv(paste(folder_path, "site_full_clean_20240817.csv", sep = "/"))
snail_dissect <- read.csv(paste(folder_path, "snail_dissect_full_clean_20240817.csv", sep = "/"))
survey_spp <- read.csv(paste(folder_path, "survey_spp_full_clean_20240817.csv", sep = "/"))
transect_spp <- read.csv(paste(folder_path, "transect_sppsum_full_clean_20240817.csv", sep = "/"))
malform_transect <- read.csv(paste(folder_path, "malform_transect_full_clean_20240817.csv", sep = "/"))
ves_spp_code <- read.csv(paste(folder_path, "ves_spp_code.csv", sep = "/"))
ves_table <- read.csv(paste(folder_path, "ves_table_full_clean_20240817.csv", sep = "/"))
visual_spp <- read.csv(paste(folder_path, "visual_spp_full_clean_20240817.csv", sep = "/"))
water_qual <- read.csv(paste(folder_path, "water_qual_full_clean_20240817.csv", sep = "/"))
wetland <- read.csv(paste(folder_path, "wetland_full_clean_20240817.csv", sep = "/"))

#----- Start Transect Survey Wrangleuing -----#
# I want to get the transect survey results.  TO do this I need the transect_sum dataframe but this only has a transect code NOT an 
# assesment code or site code.   So I am getting the relationship between trans_code and assmt_code from the malform_transect df
# and then appending that to the transect_spp df.  I will need to merge this with the other daframes later to get site_code ino.
# Extract 'trans_code' and 'assmt_code' columns from malform_transect
trans_code_to_assmt_code <- malform_transect %>%
  select(trans_code, assmt_code) %>%
  distinct()


assmt_code_to_site_code <- wetland %>%
  select(assmt_code, sitecode) %>%
  distinct()

trans_code_to_site_code <- merge(trans_code_to_assmt_code, assmt_code_to_site_code, by = "assmt_code", all.x = TRUE)

# Merge malform_codes with transect_spp
merged_transect_data <- merge(transect_spp, trans_code_to_site_code, by = "trans_code", all.x = TRUE)

# Update the transect_spp dataframe with the merged data
transect_spp <- merged_transect_data

# Clean up temporary variables
rm(merged_transect_data, unmerged_rows)
colnames(transect_spp)
#----- End Transect Survey Wrangleuing -----#


#-------------------Code to filter amphib_bd to keep only rows with SiteCodes that are in work_done -------------------#
# Get the unique SiteCodes from the work_done dataframe
work_done_sitecodes <- unique(work_done$sitecode)
    
# Filter df to keep only rows with sitecodes that are in work_done
amphib_bd <- amphib_bd %>%
  filter(sitecode %in% work_done_sitecodes)

amphib_dissect <- amphib_dissect %>%
  filter(sitecode %in% work_done_sitecodes)

bd_full <- bd_full %>%
  filter(sitecode %in% work_done_sitecodes)

survey_spp <- survey_spp %>%
  filter(sitecode %in% work_done_sitecodes)

transect_spp <- transect_spp %>%
  filter(sitecode %in% work_done_sitecodes)

wetland <- wetland %>%
  filter(sitecode %in% work_done_sitecodes)

site_full <- site_full %>%
    filter(sitecode %in% work_done_sitecodes)
#-------------------End of Code to filter amphib_bd to keep only rows with SiteCodes that are in work_done -------------------#

#Fix the dates
work_done <- work_done %>% 
    mutate(date = lubridate::mdy(Date_End)) %>%
    mutate(Date_End = lubridate::mdy(Date_End)) %>%
    mutate(Date_Start = lubridate::mdy(Date_Start))

wetland <- wetland %>%
    mutate(date = lubridate::mdy(date))

transect_spp <- transect_spp %>%
    mutate(date = lubridate::ymd(date))  
#End Fix Dates

# Merge Dataframes
merged_data <- merge(work_done, site_full, by = c("sitecode"), all = TRUE)
glimpse(merged_data)

merged_data <- merge(merged_data, wetland, by = c("sitecode", "date"), all= TRUE)
glimpse(merged_data)

merged_data <- merge(merged_data, transect_spp, by = c("sitecode", "date"), all = TRUE)
glimpse(merged_data)

# site = "PRPND009"
#  single_site = filter(merged_data, sitecode == site)
#  ggplot(single_site, aes(x = date, y = spp_total)) +
#      geom_point(aes(color = spp_code)) +
#      geom_line(aes(group = spp_code, color = spp_code)) +
#      geom_vline(aes(xintercept = Date_End, color = Work_Done)) +
#      geom_vline(aes(xintercept = Date_Start, color = Work_Done)) +
#      labs(title = paste("Site:", site),
#           x = "Date",
#           y = "Species Total",
#           color = "Species Code") +
#      theme_minimal() +
#      theme(legend.position = "bottom") +
#      scale_x_date(date_breaks = "1 year", date_labels = "%d %b %Y") +
#      scale_y_continuous(breaks = seq(0, max(single_site$spp_total, na.rm = TRUE), by = 1)) +
#      scale_color_manual(values = color_values)
#      theme(axis.text.x = element_text(angle = 45, hjust = 1),
#            axis.text.y = element_text(size = 8))



#Dataset$Protected_area<-as.factor (Dataset$Protected_area)
merged_data$sitecode <- as.factor(merged_data$sitecode)
merged_data$Work_Done <- as.factor(merged_data$Work_Done)
merged_data$trophic_state <- as.factor(merged_data$trophic_state)
merged_data$spp_code <- as.factor(merged_data$spp_code)



# Filter out sites with fewer than 3 total measurements in species total
#merged_data <- merged_data %>%
#  group_by(sitecode) %>%
#  filter(sum(!is.na(spp_total)) > 3) %>%
#  ungroup()

top_15_sites <- merged_data %>%
  group_by(sitecode) %>%
  summarise(site_count = n()) %>%
  arrange(desc(site_count)) %>%
  slice_head(n = 15) %>%
  pull(sitecode)

length(unique(merged_data$sitecode))
# Filter transect_spp to keep only the top 15 sites
merged_data <- merged_data %>%
  filter(sitecode %in% top_15_sites)

length(unique(merged_data$sitecode))

# Print the number of remaining sites
cat("Number of sites with 3 or more measurements:", n_distinct(merged_data$sitecode), "\n")



#----- Start Plotting -----#
color_values <- c(
      "PSRE" = "#1f77b4", "RACA" = "#ff7f0e",
      "RADR" = "#d62728", "TAGR" = "#e377c2", 
      "TATO" = "#17becf", "BUBO" = "#98df8a",
      "OTH" = "#c5b0d5",
      "SWA02" = "#648FFF", "GR01" = "#F10909",
      "SWA01" = "#785EF0", "EXC01" = "#252423", 
      "NNBF_RM" = "#E2FF00", "W_RM" = "#E2FF00"
)


# Define a consistent color palette and theme
color_palette <- scale_color_viridis_d(option = "plasma", end = 0.9)  # Using viridis color palette which is colorblind-friendly
consistent_theme <- theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))

# Determine global x and y axis limits
date_range <- range(merged_data$date, na.rm = TRUE)
y_max <- 120  # Set y_max to 120

# Create a list to store individual plots for each sitecode
site_plots <- list()

# Loop through each unique sitecode
for (site in unique(merged_data$sitecode)) {
  # Filter data for the current sitecode
  site_data <- filter(merged_data, sitecode == site)
  
  # Create a plot for the current sitecode
  plot <- ggplot(site_data, aes(x = date, y = spp_total)) +
    geom_point(aes(color = spp_code)) +
    geom_line(aes(group = spp_code, color = spp_code)) +
    geom_vline(aes(xintercept = Date_End, color = Work_Done), linetype = "dashed") +
    geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid") +
    labs(title = paste("Site:", site),
         x = "Date",
         y = "Count") +
    color_palette +
    consistent_theme +
    scale_x_date(limits = date_range, date_breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = color_values) +
    scale_y_continuous(limits = c(0, y_max), breaks = seq(0, y_max, by = 20)) +  # Adjust y-axis limits and breaks
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")  # Ensure no legend is displayed
  
  # Add the plot to the list
  site_plots[[site]] <- plot
}

length(site_plots)

# Create a data frame for the color values
color_table <- data.frame(
  Key = names(color_values),
  Color = unname(color_values),
  stringsAsFactors = FALSE
)

# Create a ggplot object for the color table
site_plots[[length(site_plots) + 1]] <- ggplot(color_table, aes(x = 1, y = Key, fill = Color)) +
  geom_tile(width = .5, height = 1) +  # Increase tile size
  geom_text(aes(label = Key), color = "white", fontface = "bold", size = 3) +  # Adjust text size if needed
  scale_fill_identity() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_equal(expand = FALSE) +  # Ensure square tiles
  scale_x_discrete(expand = c(0, 0)) +  # Remove padding
  scale_y_discrete(expand = c(0, 0))    # Remove padding

length(site_plots)

# Combine all plots into a single multi-panel plot
ncol_plots <- 3
combined_plot <- wrap_plots(site_plots, ncol = ncol_plots) +
  plot_layout(guides = "collect")


# Add common x and y axis labels
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Species Total Over Time for Different Sites"
  )

# Display the combined plot
print(combined_plot)
# Calculate the number of rows needed based on the number of plots
num_plots <- length(site_plots)
num_rows <- ceiling(num_plots / ncol_plots)

# Calculate the appropriate height based on the number of rows
plot_height <- 6 * num_rows  # Adjust the multiplier as needed

# Increase the width to accommodate larger subplots
plot_width <- 20  # Adjust as needed


# Optionally, you can increase the text size for better readability
combined_plot <- combined_plot + 
  theme(text = element_text(size = 14),  # Increase overall text size
        axis.text = element_text(size = 12),  # Increase axis text size
        plot.title = element_text(size = 16))  # Increase title size

# Save the updated plot
ggsave(filename = file.path(fig_save_path, "combined_plot_large_text_work_done.png"), 
       plot = combined_plot, 
       width = plot_width, 
       height = plot_height, 
       dpi = 300,
       limitsize = FALSE)


#------End Plotting------#





#----- Single site plot -----#
site = "PRPND009"
site_data = filter(merged_data, sitecode == site)
  plot <- ggplot(site_data, aes(x = date, y = spp_total)) +
    geom_point(aes(color = spp_code)) +
    geom_line(aes(group = spp_code, color = spp_code)) +
    geom_vline(aes(xintercept = Date_End, color = Work_Done), linetype = "dashed") +
    geom_vline(aes(xintercept = Date_Start, color = Work_Done), linetype = "solid") +
    labs(title = paste("Site:", site),
         x = "Date",
         y = "Count") +
    color_palette +
    consistent_theme +
    scale_x_date(limits = date_range, date_breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = color_values) +
    scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +  # Adjust y-axis limits and breaks
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom")  # Ensure no legend is displayed
# Save the plot as a PNG file with the site name
ggsave(filename = file.path(fig_save_path, paste0(site, ".png")), plot = plot, width = 10, height = 6, dpi = 300)
