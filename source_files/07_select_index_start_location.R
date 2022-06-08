#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 08 - This file adds the following three columns...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
index_times_loc<-index_times |> add_column(start_location = as.character(NA))  
for(row in 1:nrow(index_times_loc)){
  temp_start_loc<-c()
  for(surveyor in 1:num_surveyors){
    surveyor<-unique(sub_index_sites$surveyor_num)[surveyor]
    sub_sites_surveyor<-sub_index_sites |> filter(surveyor_num == unique(surveyor_num)[surveyor]) |> select(site_num) |> pull()
    temp_samp<-paste(surveyor, mysample(sub_sites_surveyor, 1, F), mysample(c("Up", "Dw"), 1, F), sep="_")
    temp_start_loc<-c(temp_start_loc, temp_samp)
  }
  index_times_loc[row, "start_location"]<-toString(temp_start_loc)
}  
