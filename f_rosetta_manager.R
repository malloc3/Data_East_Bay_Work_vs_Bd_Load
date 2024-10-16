# Title: Rosetta Stone Manager for Park IDs and Titles
# Author: [Your Name]
# Date: [Current Date]
# Description: This script provides functions to translate between different
#              database identifiers for park sites. It uses a CSV file as a
#              "Rosetta Stone" to map between Parks and Peit database site codes.

library(tidyr)
library(dplyr)

# File path for the Rosetta Stone CSV
rosetta_file_path = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Site_code_rosetta.csv"

# Read the Rosetta Stone CSV file
site_code_rosetta = read.csv(rosetta_file_path)

# Function to convert Parks site code to Peit site code
#
# Input:
#   parks_site_code - String: The site code from the Parks database
# Output:
#   String: The corresponding site code from the Peit database
park_to_peit_code = function(parks_site_code){
  matching_rows = site_code_rosetta %>%
    drop_na(Parks_Site_Code) %>%
    filter(Parks_Site_Code == parks_site_code)
  
  if (nrow(matching_rows) > 1){
    stop(paste('There are multiple rows in the rosetta file with the parks site code', parks_site_code))
  }else if (nrow(matching_rows) == 0){
    print(matching_rows)
    stop(paste('There are no rows in the rosetta file with the parks site code', parks_site_code))
  }else{
    return(matching_rows[1, "Piet_Site_Code"])
  }
}

# Function to convert Peit site code to Parks site code
#
# Input:
#   peit_site_code - String: The site code from the Peit database
# Output:
#   String: The corresponding site code from the Parks database
peit_to_park_code = function(peit_site_code){
  matching_rows = site_code_rosetta %>%
    drop_na(Piet_Site_Code) %>%
    filter(Piet_Site_Code == peit_site_code)

  if (nrow(matching_rows) > 1){
    stop(paste('There are multiple rows in the rosetta file with the Peit site code', peit_site_code))
  }else if (nrow(matching_rows) == 0){
    stop(paste('There are no rows in the rosetta file with the Peit site code', peit_site_code))
  }else{
    return(matching_rows[1, "Parks_Site_Code"])
  }
}

# Test functions
test_rosetta_functions = function() {
  # Test park_to_peit_code
  test_park_to_peit_code()
  
  # Test peit_to_park_code
  test_peit_to_park_code()
  
  cat("All tests completed.\n")
}

test_park_to_peit_code = function() {
  cat("Testing park_to_peit_code function:\n")
  
  # Test case 1: Valid conversion
  tryCatch({
    result = park_to_peit_code("cspnd013")
    if (!is.na(result) && is.character(result)) {
      cat("  PASS: Valid conversion successful\n")
    } else {
      cat("  FAIL: Valid conversion unsuccessful\n")
    }
  }, error = function(e) {
    cat("  FAIL: Valid conversion threw an error:", conditionMessage(e), "\n")
  })
  
  # Test case 2: Non-existent Parks site code
  tryCatch({
    park_to_peit_code("NONEXISTENT")
    cat("  FAIL: Non-existent Parks site code did not throw an error\n")
  }, error = function(e) {
    if (grepl("There are no rows", conditionMessage(e))) {
      cat("  PASS: Non-existent Parks site code correctly threw an error\n")
    } else {
      cat("  FAIL: Unexpected error for non-existent Parks site code:", conditionMessage(e), "\n")
    }
  })
}

test_peit_to_park_code = function() {
  cat("Testing peit_to_park_code function:\n")
  
  # Test case 1: Valid conversion
  tryCatch({
    result = peit_to_park_code("CSPND013")
    if (!is.na(result) && is.character(result)) {
      cat("  PASS: Valid conversion successful\n")
    } else {
      cat("  FAIL: Valid conversion unsuccessful\n")
    }
  }, error = function(e) {
    cat("  FAIL: Valid conversion threw an error:", conditionMessage(e), "\n")
  })
  
  # Test case 2: Non-existent Peit site code
  tryCatch({
    peit_to_park_code("NONEXISTENT")
    cat("  FAIL: Non-existent Peit site code did not throw an error\n")
  }, error = function(e) {
    if (grepl("There are no rows", conditionMessage(e))) {
      cat("  PASS: Non-existent Peit site code correctly threw an error\n")
    } else {
      cat("  FAIL: Unexpected error for non-existent Peit site code:", conditionMessage(e), "\n")
    }
  })
}

# Run tests
test_rosetta_functions()
