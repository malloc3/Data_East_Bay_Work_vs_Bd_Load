#Gets the surveys associated with a specieic assement code
get_surveys = function(assmt_code, survey_df){
  return(survey_df[survey_df$AssmtCode == assmt_code, ])
}


#Gets the total number of amphibians counted in a specific assesment code
get_total_sppgrp_count = function(assmt_code, survey_df, species_group){
  all_surveys = get_surveys(assmt_code, survey_df)
  captures = all_surveys[all_surveys$SppGrp == species_group, ]
  total_count = sum(captures$Count)
  return(total_count)
}
