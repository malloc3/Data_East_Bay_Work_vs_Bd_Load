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

# ------- Reduce site_full to only be california sites ------- #
# Filter site_full to include only California sites
site_full <- site_full %>%
  filter(state == "CA")

# Print the number of California sites
cat("Number of California sites:", nrow(site_full), "\n")



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

cat("Number of sites in transect_spp after filtering:", n_distinct(transect_spp$sitecode), "\n")
# Filter transect_spp to keep only sites that are in site_full
transect_spp <- transect_spp %>%
  filter(sitecode %in% site_full$sitecode)

# Drop all rows with NA sitecodes from transect_spp
transect_spp <- transect_spp %>%
  filter(!is.na(sitecode))

# Print the number of remaining sites in transect_spp
cat("Number of sites in transect_spp after filtering:", n_distinct(transect_spp$sitecode), "\n")

transect_spp <- transect_spp %>%
    mutate(date = lubridate::ymd(date))  

#Dataset$Protected_area<-as.factor (Dataset$Protected_area)
transect_spp$sitecode <- as.factor(transect_spp$sitecode)
transect_spp$trophic_state <- as.factor(transect_spp$trophic_state)
transect_spp$spp_code <- as.factor(transect_spp$spp_code)

#----- End Transect Survey Wrangleuing -----#

wetland <- wetland %>%
    mutate(date = lubridate::mdy(date))

# Filter wetland to keep only sites that are in site_full
wetland <- wetland %>%
  filter(sitecode %in% site_full$sitecode)

# Drop all rows with NA sitecodes from wetland
wetland <- wetland %>%
  filter(!is.na(sitecode))

# Print the number of remaining sites in wetland
cat("Number of sites in wetland after filtering:", n_distinct(wetland$sitecode), "\n")

# Ensure sitecode is a factor in wetland
wetland$sitecode <- as.factor(wetland$sitecode)


# --- Merge Dataframes --- #
merged_data <- merge(site_full, wetland, by = c("sitecode"), all= TRUE)
glimpse(merged_data)

merged_data <- merge(merged_data, transect_spp, by = c("sitecode", "date"), all = TRUE)
glimpse(merged_data)

#---- End Merge Dataframes ----#

top_work_sites = c("CA-MUD65", "CA-MUD66", "CA-MUD67", "GDPND005", "GDPND006", "GDPND014", "MUD40", "MUD41", "MUD74", "MUD77", "MUDEF", "MUDRS", "PRNTHIDK", "PRPND009", "PRPND010")

# Remove top_work_sites from merged_data
merged_data <- merged_data %>%
  filter(!sitecode %in% top_work_sites)

# Filter merged_data to keep the top 15 sites with the most measurements
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



#----- Start Plotting -----#
color_values <- c(
      "PSRE" = "#1f77b4", "RACA" = "#ff7f0e",
      "RADR" = "#d62728", "TAGR" = "#e377c2", 
      "TATO" = "#17becf", "BUBO" = "#98df8a",
      "OTH" = "#c5b0d5"
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
#print(combined_plot)
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
ggsave(filename = file.path(fig_save_path, "top_15_no_work_sites_ca.png"), 
       plot = combined_plot, 
       width = plot_width, 
       height = plot_height, 
       dpi = 300,
       limitsize = FALSE)

#------End Plotting------#


#----- Single site plot -----#
site = "PRPND015"
site_data = filter(merged_data, sitecode == site)
  plot <- ggplot(site_data, aes(x = date, y = spp_total)) +
    geom_point(aes(color = spp_code)) +
    geom_line(aes(group = spp_code, color = spp_code)) +
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
