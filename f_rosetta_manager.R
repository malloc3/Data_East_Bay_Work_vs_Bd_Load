library(tidyr)
library(dplyr)

#This script helps navigate the rosetta stone of park ids and titles
# Things are named ever so slighlty differently in the different databases
#
# This has scripts that can help "translate" between these databases

rosetta_file_path = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/Site_code_rosetta.csv"

site_code_rosetta = read.csv(rosetta_file_path)


# Takes in a parks site code and converts it to Peits database site code
#
# Input 
#  parks_site_code = String
# Output
#  peit_site_code = String
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
    return(matching_rows[1, "Peit_Site_Code"])
  }
}


# Takes in a Peit site code and converts it to Parks database site code
#
# Input 
#  peit_site_code = String
# Output
#  parks_site_code = String
peit_to_park_code = function(peit_site_code){
  matching_rows = site_code_rosetta %>%
    drop_na(Peit_Site_Code) %>%
    filter(Peit_Site_Code == peit_site_code)

  if (nrow(matching_rows) > 1){
    stop(paste('There are multiple rows in the rosetta file with the Peit site code', peit_site_code))
  }else if (nrow(matching_rows) == 0){
    stop(paste('There are no rows in the rosetta file with the Peit site code', peit_site_code))
  }else{
    return(matching_rows[1, "Parks_Site_Code"])
  }
}


