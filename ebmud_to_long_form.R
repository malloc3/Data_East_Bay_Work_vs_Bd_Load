#This is a throw away script to ormat the ebmud data into a longform dataset.  
# Currently it is wideform which is fine but not ultimately that helpful for data analysis

file = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/EBMUD_PondWork1997_2023.csv"
save_file = "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Work_Done/reformatted_EBMUD_PondWork1997_2023.csv"


wide_form_data = read.csv(file)
long_form_data = data.frame(matrix(ncol= 5, nrow = 0))
colnames(long_form_data) = c("SiteCode", "Pond_Name", "Date_Start", "Date_End", "Work_Done")

for (i in 1:nrow(wide_form_data)){
  row = wide_form_data[i,]
  site_code = row[1, "Pond.ID"]
  pond_name = row[1, "Pond.Name"]
  date_start = paste("1/1/", row[1, "Date"], rep="")
  date_end = paste("1/1/", row[1, "Date"], rep="")
  
  if(row[1, 4] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "EXC01")  
  }
  
  if(row[1, 5] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "GR01")  
  }
  
  if(row[1, 6] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "DR01")
  }

  if(row[1, 7] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "SWA02")
  }
  
  if(row[1, 8] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "W_RM")
  }
  
  if(row[1, 9] == "X"){
    long_form_data[nrow(long_form_data) + 1,] = list(site_code, pond_name, date_start, date_end, "CMP")
  }

}

write.csv(long_form_data, save_file)






