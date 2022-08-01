#---------------------------------------------------------------------------
# Select creel sample time within a day based on user inputs (ui)
#---------------------------------------------------------------------------
if(ui_shifts_smpl>ui_shifts_total){
 print("ERROR: The number of sampled shifts can't be greater than the total number of shifts") 
}else{
# Time on River - final (based on "ui_shift_length")
  (survey_time_final<-ui_shift_length - ui_drive_time)

# Identify earliest start time for each shift
  creel_shifts<- tibble(date = as.Date(as.POSIXct(NA, format = "%Y-%m-%d")), shift = as.numeric(), shift_size = structure(NA_real_, class = "difftime"), earliest = as.POSIXct(NA))
  for(row in 1:nrow(sampl_fram_time)){
    temp_date<-sampl_fram_time |> slice(row)
    date_shift_length<-temp_date |> mutate(temp_shft_lng=survey_window/ui_shifts_total) |> select(temp_shft_lng) |> pull()
    samp_shifts<-mysample(x = 1:ui_shifts_total, y=ui_shifts_smpl, z = FALSE)
    
    for(shift in 1:ui_shifts_smpl){
      temp_early_start<-temp_date |> mutate(early = earliest + ((samp_shifts[shift] - 1) * date_shift_length ))
      
      temp_DF<-tibble(
        date =  temp_date |> select(date) |> pull(), 
        shift = as.numeric(samp_shifts[shift]), 
        shift_size = date_shift_length,
        earliest = temp_early_start |> select(early) |> pull()
        )
      creel_shifts<-rbind(creel_shifts, temp_DF)
    }
  }
  creel_shifts<-creel_shifts |> arrange(date, shift)

# [Randomly] select survey start time
  creel_date_times<-creel_shifts |> add_column(survey_start = as.POSIXct(NA), survey_end = as.POSIXct(NA))
  for(row in 1:nrow(creel_date_times)){
    # Calculate different between total shift size and "on-water" survey time
      shift_survey_difftime<-creel_date_times$shift_size[row] - survey_time_final
      
    # if "shift_survey_difftime" is less than 0 (meaning survey time is greater than the shift length)...  
    if(as.numeric(shift_survey_difftime) < 0){
    # ...AND 
      if(creel_date_times$shift[row] == max(creel_date_times$shift) & ui_shifts_total!=1 ){ # if more than 1 shift & last shift of day...
        creel_date_times[row, "survey_start"]<-creel_date_times[row, "earliest"] + as.numeric(shift_survey_difftime*60*60) #...Need to make start time early otherwise will go past dark
    # ...OR  
      }else{
        creel_date_times[row, "survey_start"]<-creel_date_times[row, "earliest"] 
      }
      
    # if "shift_survey_difftime" is greater than zero...
    }else{
      temp_start_times<-seq(creel_date_times$earliest[row], creel_date_times$earliest[row]+round(shift_survey_difftime), 60*60)
      creel_date_times[row, "survey_start"]<-mysample(x = temp_start_times, y = 1, z=FALSE)
    }
    creel_date_times[row, "survey_end"]<-creel_date_times[row, "survey_start"] + (survey_time_final*60*60)
    
  }
  creel_date_times<-
    creel_date_times |> 
    mutate(
        earliest = lubridate::round_date(earliest, "15 minutes") 
      , survey_start = lubridate::round_date(survey_start, "15 minutes") 
      , survey_end = lubridate::round_date(survey_end, "15 minutes")
      #, river_diff = survey_end - survey_start
    ) 
}

# *** come back to this code to update and reincorporate ***

# # Evaluate Start Times by River and Month
#     windows(width=8, height=3*ceiling(length(unique(scheduledWeekDays_Shift$month.no))/2))
#     par(mfcol=c(ceiling(length(unique(scheduledWeekDays_Shift$month.no))/2),2),family='sans', xaxs="i", yaxs="i",cex.axis=1, cex=1, mgp=c(2,0.75, 0.5),mar=c(2,1.5,1,1), oma=c(2.5,2.5,0.5,1))  
#     bins<-seq(4,22,1)
#     
#     decimaltime.expansion<-0.5 #number of minutes in an hour to expand by
#     
#     for(month in 1:length(unique(scheduledWeekDays_Shift$month.no))){
#       
#       sub.month<-scheduledWeekDays_Shift[scheduledWeekDays_Shift$month.no == unique(scheduledWeekDays_Shift$month.no)[month],]
#       sub.month.tim.expan<-c()
#       
#        for(j in 1:length(unique(sub.month$dates))){
#         
#         sub.day<-sub.month[sub.month$dates == unique(sub.month$dates)[j],]
#         time.expan<-seq(sub.day$survey_start, sub.day$survey_end, decimaltime.expansion)
#         sub.month.tim.expan<-c(sub.month.tim.expan, time.expan)
#         
#       }
#       #Plot distribution of continuous time on river by month
#           hist(sub.month.tim.expan, col="gray", main=paste(sub.month$month[1], sep="-"), xlim=c(min(floor(avg.dawn.dusk$d.Dawn)), max(ceiling(avg.dawn.dusk$d.Dusk))), breaks=bins)
#       #Plot average time of dawn and dusk
#           abline(v=as.numeric(avg.dawn.dusk[as.character(sub.month$month[1])==as.character(avg.dawn.dusk$Month), "d.Dawn"]), lty=2, lwd=2)
#           abline(v=as.numeric(avg.dawn.dusk[as.character(sub.month$month[1])==as.character(avg.dawn.dusk$Month), "d.Dusk"]), lty=2, lwd=2)
#           title(xlab = "Time of Day (24 hr)", ylab = "Number of Times Sampling", outer=T, line=1)
#     }
#       
        

