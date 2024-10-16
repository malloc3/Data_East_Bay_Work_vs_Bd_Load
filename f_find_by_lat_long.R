# Load required libraries
library(dplyr)
library(geosphere)

# Main function to find the closest pond within a specified distance
find_by_lat_lon <- function(site_info_df, target_lat, target_lon, max_distance) {
  # Input validation
  if (max_distance <= 0 || is.null(max_distance) || is.na(max_distance)) {
    warning("The given maximum distance is invalid")
    return(rep(NA, 4))
  }
  if (is.null(target_lat) || is.na(target_lat) || is.null(target_lon) || is.na(target_lon)) {
    warning("Latitude and/or Longitude are NA or NULL")
    return(rep(NA, 4))
  }
  
  # Find the closest pond
  closest_site_info <- find_closest_pond(site_info_df, target_lat, target_lon)
  site_id <- closest_site_info[[1]]
  distance <- closest_site_info[[2]]
  new_lat <- closest_site_info[[3]]
  new_lon <- closest_site_info[[4]]
  
  # Check if the closest pond is within the maximum distance
  if (distance < max_distance) {
    return(c(site_id, distance, new_lat, new_lon))
  } else {
    return(rep(NA, 4))
  }
}

# Helper function to find the closest pond in the dataset
find_closest_pond <- function(site_info_df, target_lat, target_lon) {
  # Calculate distances for all ponds
  distances <- sapply(1:nrow(site_info_df), function(i) {
    find_distance_in_meters(target_lat, target_lon, 
                            site_info_df$Latitude[i], site_info_df$Longitude[i])
  })
  
  # Find the closest pond
  closest_index <- which.min(distances)
  closest_row <- site_info_df[closest_index, ]
  
  # Return relevant information
  list(closest_row$SiteCode, distances[closest_index], 
       closest_row$Latitude, closest_row$Longitude)
}

# Helper function to calculate distance between two points using Haversine formula
find_distance_in_meters <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}
