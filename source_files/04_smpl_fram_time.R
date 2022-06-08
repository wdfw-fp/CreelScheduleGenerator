#---------------------------------------------------------------------------
# Define the sampling bounds of time within a day based on user inputs (ui)
#---------------------------------------------------------------------------
#Specify "lat" and "long" of location of interest
  qry_site<-lut$water_body |> filter(water_body %in% ui_water_bodies[1]) 

# Earliest start time for a creel for each survey date 
if( ui_start_time != "manual"){
  time_start<- 
    tibble(
      getSunlightTimes(
        keep=c(ui_start_time), 
        date=creel_dates$dates, #seq(as.Date(min(creel_dates$dates)), as.Date(max(creel_dates$dates)),1), 
        lat = qry_site |> select(Lat) |> pull(), 
        lon=qry_site |> select(Long) |> pull(),
        tz = "America/Los_Angeles"
      )
      ) |> 
      select(-lat, -lon) |> 
      rename_with(.cols = 2, ~"earliest_start") 
}else{
  time_start<- 
    tibble(date = creel_dates$dates) |> 
    mutate(earliest_start = paste(creel_dates$dates, ui_start_manual)) %>%   
    mutate(across('earliest_start', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))
}
  
# Latest end time for a creel for each survey date 
if(ui_end_time != "manual"){
  time_end<- 
    tibble(
      getSunlightTimes(
        keep=c(ui_end_time), 
        date=creel_dates$dates, #seq(as.Date(min(creel_dates$dates)), as.Date(max(creel_dates$dates)),1), 
        lat = qry_site |> select(Lat) |> pull(), 
        lon=qry_site |> select(Long) |> pull(),
        tz = "America/Los_Angeles"
      )
      ) |> 
      select(-lat, -lon) |> 
      rename_with(.cols = 2, ~"latest_end")  
}else{
  time_end<- 
    tibble(date = creel_dates$dates) |> 
    mutate(latest_end = paste(creel_dates$dates, ui_end_manual)) %>%   
    mutate(across('latest_end', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))
}
  
# Create within day temporal sample frame
  sampl_fram_time<-
    time_start |> 
    left_join(time_end, by="date") |> 
    mutate(
      #earliest_start = lubridate::round_date(earliest_start, "15 minutes"), 
      #latest_end = lubridate::round_date(latest_end, "15 minutes"),
      mid_day = earliest_start + (latest_end-earliest_start)/2, 
      total_sample_time=round(latest_end-earliest_start,2) 
    ) |> 
    relocate(mid_day , .after = earliest_start)

# Time on River - Potential (based on "potential_shift_length")
  River_Time_Potential<-potential_shift_length - ui_drive_time

# Add columns for suggested number of shifts per day and maximum amount of overlap in time (hours) between the two shifts for each potential shift length 
  shift_eval<-
    sampl_fram_time |> 
    mutate(
        earliest_start = times(strftime(earliest_start,"%H:%M:%S"))
      , mid_day = times(strftime(mid_day,"%H:%M:%S"))
      , latest_end = times(strftime(latest_end,"%H:%M:%S"))

      )

  for(shift_length in 1:length(unique(potential_shift_length))){
          shift_eval$Shifts.Dec<-0; shift_eval$Overlap<-0; 
          for(row in 1:nrow(shift_eval)){
            shift_eval$Shifts.Dec[row]<-round(shift_eval$total_sample_time[row]/River_Time_Potential[shift_length],0)     # suggested number of shifts per day
            #shift_eval$Overlap[row]<-River_Time_Potential[shift_length] - (as.numeric(shift_eval$total_sample_time[row])/shift_eval$Shifts.Dec[row]) # Max overlap in shifts within a day
            shift_eval$Overlap[row]<-(shift_eval$Shifts.Dec[row] * River_Time_Potential[shift_length]) - as.numeric(shift_eval$total_sample_time[row])
          }
          names(shift_eval)[names(shift_eval) == 'Shifts.Dec'] <- paste("Sugg_Shifts", potential_shift_length[shift_length],sep="_")
          names(shift_eval)[names(shift_eval) == 'Overlap'] <-   paste("Max_Overlap", potential_shift_length[shift_length],sep="_")
  }