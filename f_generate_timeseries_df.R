
# Generate a useful small dataframe of site BD loads over time.
# Returns NULL if no site data exists
# Will skip site assesments where no amphibian BD loads were measured
generate_date_load_df = function(site_code, rw_wetland_info_df, am_bd_df, report){
  #Get all the site visits that are associated with the specific site code
  site_wl_info_df = rw_wetland_info_df[rw_wetland_info_df$SiteCode == site_code, ]
  #Checks that there are existing site visits  If not hen returnes NULL
  if (nrow(site_wl_info_df) == 0) {
    return(NULL)
  }
  
  date_list = c()
  percent_pos_list = c()
  max_ze_list = c()
  min_ze_list = c()
  ave_ze_list = c()
  
  non_existant_assesment_codes = c()
  
  for (i in 1:nrow(site_wl_info_df)){
    code = site_wl_info_df$AssmtCode[i]
    if (code %in% am_bd_df$AssmtCode) { #Must make sure that swabs were taken at the applicable assesments
      date_list = c(date_list, site_wl_info_df$Date[i])
      percent_pos_list = c(percent_pos_list, get_fraction_positive_by_assesmet(code, am_bd_df))
      max_ze_list = c(max_ze_list, get_max_ze(code, am_bd_df))
      min_ze_list = c(min_ze_list, get_min_ze(code, am_bd_df))
      ave_ze_list = c(ave_ze_list, get_ave_ze(code, am_bd_df))
      
    } else { #Catches all the times an assment code did not correlate with swabs taken!
      non_existant_assesment_codes = c(non_existant_assesment_codes, code)
    }
  }
  
  site_bd_over_time <- data.frame(
    date = ymd(date_list),
    percent_pos = percent_pos_list,
    max_ze = max_ze_list,
    min_ze = min_ze_list,
    ave_ze = ave_ze_list
  )
  
  if (length(non_existant_assesment_codes) > 0 & report) {
    
    print("Some of the assesment codes are no in the Amphibian BD Swab Dataframe.  This is probably because those assesments no BD swabs were obtained")
    print(non_existant_assesment_codes)
    
  }
  if (nrow(site_bd_over_time) == 0){ #This happens when there is a wetland info.  But There was never any Amphibian data collected in this region
    return(NULL)
  }else{
    return(site_bd_over_time) 
  }
  
}

# Fetches completed work from the work done DF that happened at a specific site
generate_date_work_df = function(site_code, rw_work_done_df){
  completed_work = rw_work_done_df[rw_work_done_df$SiteCode == site_code, ]
  if (nrow(completed_work) == 0){
    return(NULL)
  }
  return(completed_work)
}








generate_amphib_pop_over_time = function(site_code, rw_wetland_info_df, survey_df, report){
  #Get all the site visits that are associated with the specific site code
  site_wl_info_df = rw_wetland_info_df[rw_wetland_info_df$SiteCode == site_code, ]
  #Checks that there are existing site visits  If not hen returnes NULL
  if (nrow(site_wl_info_df) == 0) {
    return(NULL)
  }
  
  date_list = c()
  total_amphib_count = c()
  total_invert_count = c()
  total_snail_count = c()
  total_fish_count = c()
  
  non_existant_assesment_codes = c()
  for (i in 1:nrow(site_wl_info_df)){
    assmt_code = site_wl_info_df$AssmtCode[i]
    if (assmt_code %in% survey_df$AssmtCode) { #Must make sure that swabs were taken at the applicable assesments
      date_list = c(date_list, site_wl_info_df$Date[i])
      total_amphib_count = c(total_amphib_count, get_total_sppgrp_count(assmt_code, survey_df, "AMPHIBIAN"))
      total_invert_count = c(total_invert_count, get_total_sppgrp_count(assmt_code, survey_df, "INVERTEBRATE"))
      total_snail_count = c(total_snail_count, get_total_sppgrp_count(assmt_code, survey_df, "SNAIL"))
      total_fish_count = c(total_fish_count, get_total_sppgrp_count(assmt_code, survey_df, "FISH"))
      
    } else { #Catches all the times an assment code did not correlate with swabs taken!
      non_existant_assesment_codes = c(non_existant_assesment_codes, assmt_code)
    }
  }
  
  site_amphibs_over_time <- data.frame(
    date = ymd(date_list),
    total_amphib_count = total_amphib_count,
    total_invert_count = total_invert_count,
    total_snail_count = total_snail_count,
    total_fish_count = total_fish_count
  )
  
  if (length(non_existant_assesment_codes) > 0 & report) {
    
    print("Some of the assesment codes are no in the Amphibian BD Swab Dataframe.  This is probably because those assesments no BD swabs were obtained")
    print(non_existant_assesment_codes)
    
  }
  if (nrow(site_amphibs_over_time) == 0){ #This happens when there is a wetland info.  But There was never any Amphibian data collected in this region
    return(NULL)
  }else{
    return(site_amphibs_over_time) 
  }
  
}