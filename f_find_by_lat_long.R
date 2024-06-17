# The goal of this script is to find pond IDs from lat long info instead of 
# Simply from the pond id.   This is needed because the pond IDs are not always
# consistent
library(dplyr)
library(geosphere)


# This takes piets site_inf_df and finds the Piets site ID that is closest
# to the given lat, lon and is within the minimum distance
# Minimum distance is in METERS
find_by_lat_lon = function(site_info_df, target_lat, target_lon, max_distance){
  if (max_distance <= 0 | is.null(max_distance) | is.na(max_distance)){
    warning("The given minimum distance is invalid")
    return(NA)
  }
  if (is.null(target_lat) | is.na(target_lat) | is.null(target_lon) | is.na(target_lon)){
    warning("Lat and or Lon are NA or Null")
    return(NA)
  }
  
  closest_site_info = find_closest_pond(site_info_df, target_lat, target_lon)
  site_id = closest_site_info[[1]]
  distance = closest_site_info[[2]]
  new_lat = closest_site_info[[3]]
  new_lon = closest_site_info[[4]]
  
  if (distance < max_distance){
    return(c(site_id, distance, new_lat, new_lon))
  } else {
    return(list(NA, NA, NA, NA))
  }
}


# This finds the closest pond and returns the pond ID and the distance
find_closest_pond = function(site_info_df, target_lat, target_lon){
  df = site_info_df
  df$distance = NA
  for (i in 1:nrow(site_info_df)){
    df[i,"distance"] = find_distance_in_meters(target_lat, target_lon, df[i, "Latitude"], df[i, "Longitude"])
  }
  
  closest_row = df[which.min(df$distance),]
  return(list(closest_row$SiteCode, closest_row$distance, closest_row$Latitude, closest_row$Longitude))
}


find_distance_in_meters = function(lat1, lon1, lat2, lon2){
  return(distHaversine(c(lon1, lat1), c(lon2, lat2)))
}


