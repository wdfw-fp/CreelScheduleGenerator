#---------------------------------------------------------------------------
# Define the sampling bounds of time within a day based on user inputs (ui)
#---------------------------------------------------------------------------
#Specify "lat" and "long" of location of interest
  qry_site<-lut$water_body |> filter(water_body %in% ui_water_bodies[1]) 

# If manual start and/or end times enter, specify "ui_start_time" and "ui_end_time" in case they were left blank
 if( ui_start_time == "manual"){ui_start_time<-c("sunrise")}
 if( ui_end_time == "manual")  {ui_end_time<-c("sunset")} 

# Day length based on sunrise/sunset 
  day_length<- 
    tibble(
      getSunlightTimes(
        keep=c("sunrise", "sunset"), 
        date=dat$dates, 
        lat = qry_site |> select(Lat) |> pull(), 
        lon=qry_site |> select(Long) |> pull(),
        tz = "America/Los_Angeles"
      )
      ) |> 
      select(-lat, -lon) 

# Specify earliest start time for each given date
if(ui_start_time != "manual"){
  sampl_fram_time<- 
    day_length |> 
    filter(date %in% creel_dates$dates) |> 
    mutate("earliest" = cur_data()[[2]] - (60*60*ui_start_adj)) |> 
    relocate(earliest , .after = date)
}else{
  sampl_fram_time<-
    day_length |> 
    filter(date %in% creel_dates$dates) |> 
    mutate(earliest = paste(day_length$date, ui_start_manual)) %>%   
    mutate(across('earliest', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S"))) |> 
    relocate(earliest , .after = date)
}
  
# Latest end time  for each given date
if(ui_end_time != "manual"){
  sampl_fram_time<- 
    sampl_fram_time |> 
    mutate("latest" = cur_data()[[4]] + (60*60*ui_end_adj))|> 
    relocate(latest , .after = last_col())
}else{
  sampl_fram_time<- 
    sampl_fram_time |>
    mutate(latest = paste(day_length$date, ui_end_manual)) %>%   
    mutate(across('latest', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))
}

# Create within day temporal sample frame
  sampl_fram_time<-
    sampl_fram_time |> 
    mutate(
      earliest = lubridate::round_date(earliest, "15 minutes"),
      latest = lubridate::round_date(latest, "15 minutes"),
      mid_day = earliest + (latest-earliest)/2, 
      survey_window=round(latest-earliest,2) 
    ) |> 
  relocate(mid_day , .after = sunrise)

# Time on River - Potential (based on "potential_shift_length")
  survey_time_potential<-potential_shift_length - ui_drive_time

# Add columns for suggested number of shifts per day and maximum amount of overlap in time (hours) between the two shifts for each potential shift length 
  shift_eval<-
    sampl_fram_time |> 
    mutate(
        earliest = times(strftime(earliest,"%H:%M:%S"))
      , sunrise = times(strftime(sunrise,"%H:%M:%S"))
      , mid_day = times(strftime(mid_day,"%H:%M:%S"))
      , sunset = times(strftime(sunset,"%H:%M:%S"))
      , latest = times(strftime(latest,"%H:%M:%S"))
      ) 

  for(shift_length in 1:length(unique(potential_shift_length))){
    shift_eval$Shifts.Dec<-0; shift_eval$Overlap<-0; 
    for(row in 1:nrow(shift_eval)){
      shift_eval$Shifts.Dec[row]<-round(shift_eval$survey_window[row]/survey_time_potential[shift_length],0)     # suggested number of shifts per day
      shift_eval$Overlap[row]<-(shift_eval$Shifts.Dec[row] * survey_time_potential[shift_length]) - as.numeric(shift_eval$survey_window[row]) # Max. hours overlap (if positive); if negative, there's no overlap
    }
    names(shift_eval)[names(shift_eval) == 'Shifts.Dec'] <- paste("shifts", potential_shift_length[shift_length],sep="_")
    names(shift_eval)[names(shift_eval) == 'Overlap'] <-   paste("max_overlap", potential_shift_length[shift_length],sep="_")
  }
  