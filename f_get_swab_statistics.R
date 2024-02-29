get_fraction_positive_by_assesmet = function(assesment_code, amphibian_bd_info_df){
  swabs = get_swabs(assesment_code, amphibian_bd_info_df)
  
  infect = swabs$Bd_infect
  infect = as.numeric(infect)
  
  num_positive = length(na.omit(infect[infect == 1]))
  num_neg = length(na.omit(infect[infect == 0]))
  return(num_positive/(num_positive + num_neg))
}


#This function gets all the swabs associated with a certain assesment code
get_swabs = function(assesment_code, amphibian_bd_info_df){
  swabs = amphibian_bd_info_df[amphibian_bd_info_df$AssmtCode == assesment_code, ]
  return(swabs)
}

get_bd_ave_ze = function(swabs){
  Bd_ave_ZEs = swabs$Bd_aveZE
  Bd_ave_ZEs = gsub("", 0, Bd_ave_ZEs) # Sets all non to zero
  return(as.numeric(Bd_ave_ZEs))
}

get_ave_ze = function(assesment_code, amphibian_bd_info_df){
  swabs = get_swabs(assesment_code, amphibian_bd_info_df)
  return(mean(get_bd_ave_ze(swabs)))
}

get_max_ze = function(assesment_code, amphibian_bd_info_df){
  swabs = get_swabs(assesment_code, amphibian_bd_info_df)
  return(max(get_bd_ave_ze(swabs)))
}

get_min_ze = function(assesment_code, amphibian_bd_info_df){
  swabs = get_swabs(assesment_code, amphibian_bd_info_df)
  all_min = get_bd_ave_ze(swabs)
  all_min = all_min[all_min > 0]
  return(min(all_min))
}