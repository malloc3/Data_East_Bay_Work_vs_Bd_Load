# This script cleans and processes the work done data
# It performs the following operations:
# 1. Removes rows with SiteCode "snpnds"
# 2. Converts Parks SiteCodes to Piet's SiteCodes using the rosetta manager
# 3. Removes any rows where the SiteCode conversion resulted in NA
#    (i.e., sites not found in Piet's dataset)
# 4. Saves the cleaned data to a new CSV file
#
# The script also prints the number of rows removed in each step for tracking purposes.


# Import the work done data
work_done <- read.csv("/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/work_done_formatted_CM.csv")

# Source the rosetta manager function
source("f_rosetta_manager.R")


# Print the number of rows removed
cat("Number of rows removed:", sum(work_done$SiteCode == "snpnds"), "\n")
# Remove rows with SiteCode "snpnds"
work_done <- work_done[work_done$SiteCode != "snpnds", ]

# Reformat the SiteCode using the park_to_peit_code function
work_done$SiteCode <- sapply(work_done$SiteCode, park_to_peit_code)

# Print the number of rows removed due to NA SiteCode
cat("Number of rows removed due to NA SiteCode:", nrow(work_done) - sum(!is.na(work_done$SiteCode)), "\n")
# Remove rows with NA in SiteCode
work_done <- work_done[!is.na(work_done$SiteCode), ]


# Define the new file path
new_file_path <- "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/cleaned_data/cleaned_work_done_data_cm.csv"


# Save the cleaned dataframe as a new CSV file
write.csv(work_done, file = new_file_path, row.names = FALSE)

cat("Cleaned data saved to:", new_file_path, "\n")

