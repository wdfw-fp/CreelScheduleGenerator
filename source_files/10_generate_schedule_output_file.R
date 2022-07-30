# Summarize user inputs
  ui_list_sampling_frame_among_day<-
    as.data.frame(matrix(
      c(  "Among Day", "Creel survey start date", paste(ui_startDate, collapse = ",")
        , "Among Day", "Creel survey end date", paste(ui_endDate, collapse = ",") 
        , "Among Day", "Weekend [day-type] days", paste(ui_daytype_weekends, collapse = ",") 
        , "Among Day", "Total number of surveys per week", paste(ui_surveys_perweek_total, collapse = ",") 
        , "Among Day", "Number of weekend surveys per week", paste(ui_surveys_perweek_weekend, collapse = ",") 
        , "Among Day", "Total number of holidays within creel time frame", nrow(holidays_creel)
        , "Among Day", "Remove Holidays from Potential Creel Schedule?", paste(ui_remove_holidays, collapse = ",") 
        , "Among Day", "Reduce creel surveys per week by number of holidays?", paste(ui_reduce_surveys_holidays, collapse = ",") 
        , "Among Day", "Day-type to remove holiday days from", paste(ui_reduce_daytype, collapse = ",") 
        , "Among Day", "Other dates to remove from potential creel sample dates", paste(ui_dates_to_remove, collapse = ",") 
        )
      , ncol = 3, byrow = T))
  colnames(ui_list_sampling_frame_among_day)<-c("Sample Frame", "Variable", "User Input")

  if( ui_start_time == "manual" | ui_end_time == "manual"){
  ui_list_sampling_frame_within_day<-
    as.data.frame(matrix(
      c(  "Within Day", "Sample location(s)", paste(ui_water_bodies, collapse = ",")
        , "Within Day", "[Earliest] Daily Survey Start Time", paste(ui_start_time, collapse = ",") 
        , "Within Day", "[Latest] Survey End Time", paste(ui_end_time, collapse = ",") 
        , "Within Day", "Manual Survey Start Time", paste(ui_start_manual, collapse = ",") 
        , "Within Day", "Manual Survey End Time", paste(ui_end_manual, collapse = ",") 
        , "Within Day", "Total non-river time per survey day e.g., commute time (hours)", paste(ui_drive_time, collapse = ",")
        , "Within Day", "Total number of potential shifts per day", paste(ui_shifts_total, collapse = ",")
        , "Within Day", "Number of shifts to sample within a day", paste(ui_shifts_smpl, collapse = ",")
        , "Within Day", "Total shift length including non-river time per survey day (hours)", paste(ui_shift_length, collapse = ",")
        )
      , ncol = 3, byrow = T))
  }else{
   ui_list_sampling_frame_within_day<-
    as.data.frame(matrix(
      c(  "Within Day", "Sample location(s)", paste(ui_water_bodies, collapse = ",")
        , "Within Day", "[Earliest] Daily Survey Start Time", paste(ui_start_time, collapse = ",") 
        , "Within Day", "[Latest] Survey End Time", paste(ui_end_time, collapse = ",") 
        , "Within Day", "Adjustment to [Earliest] Daily Survey Start Time", paste(ui_start_adj, collapse = ",") 
        , "Within Day", "Adjustment to [Latest] Daily Survey End Time", paste(ui_end_adj, collapse = ",") 
        , "Within Day", "Total non-river time per survey day e.g., commute/drive time (hours)", paste(ui_drive_time, collapse = ",")
        , "Within Day", "Total number of potential shifts per day", paste(ui_shifts_total, collapse = ",")
        , "Within Day", "Number of shifts to sample within a day", paste(ui_shifts_smpl, collapse = ",")
        , "Within Day", "Total shift length including non-river time per survey day (hours)", paste(ui_shift_length, collapse = ",")
        )
      , ncol = 3, byrow = T)) 
  }
  colnames(ui_list_sampling_frame_within_day)<-c("Sample Frame", "Variable", "User Input")

   ui_list_effort_counts<-
    as.data.frame(matrix(
      c(  "Effort Count", "Number of index counts per shift", paste(ui_num_index_counts, collapse = ",")
        , "Effort Count", "Total number of index counts per survey day", paste(ui_num_index_counts*ui_shifts_smpl, collapse = ",")
        , "Effort Count", "Approx. time for one surveyor to complete an index effort count circuit (hours)", paste(ui_index_count_time*num_surveyors, collapse = ",") 
        , "Effort Count", "Number of surveyors simultaneously conducting an index effort count", paste(num_surveyors, collapse = ",") 
        , "Effort Count", "Time to complete an index effort count circuit (hours)", paste(ui_index_count_time, collapse = ",") 
        , "Effort Count", "Total number of census (i.e., tie-in) effort counts throughout survey season", paste(ui_num_census_counts, collapse = ",")
        )
      , ncol = 3, byrow = T)) 
  colnames(ui_list_effort_counts)<-c("Sample Frame", "Variable", "User Input")
  ui_list_total<-rbind(ui_list_sampling_frame_among_day, ui_list_sampling_frame_within_day, ui_list_effort_counts)


# record some creel schedule notes/definitions
schedule_notes<-
as.data.frame(matrix(
  c(  "shift", "period number within a day when a creel survey will be conducted"
    , "shift_start_time", "Approx. time of day when a technician should begin their shift (e.g., arrive at their duty station), which is approximately the 'river_start' minus the estimated drive time to the fishery (i.e., 'ui_drive_time' divided by two)"
    , "river_start", "Time when the technician(s) needs to arrive at the waterbody and be ready to begin the creel survey"
    , "index_start_time_nth", "Time when the technician(s) need to start the 'nth' index effort count"
    , "shift_end_time", "Approx. time when a technician should end their shift (i.e., leave their duty station)"
    , "cenus_count", "Denoting whether a census (i.e., 'tie-in') count will be conducted on a given date"
    , "census_start_time", "Time when census count will be conducted (subject to scheduling; adjust census & index times within the day as needed)"
    )
  , ncol = 2, byrow = T)) 
colnames(schedule_notes)<-c("Field", "Definition")

# Write schedule to output folder
  ifelse(!dir.exists(wd_output_files), {dir.create(wd_output_files); "Output sub-folder created"},"Output sub-folder exists already")
  write.xlsx(as.data.frame(ui_list_total), paste(wd_output_files, paste("CreelSchedule_", paste(ui_water_bodies, collapse = ","),"_", format(ui_startDate, "%b%Y"), "_thru_", format(ui_endDate, "%b%Y"),".xlsx", collapse="", sep=""), sep="/"), row.names=F, sheetName = "user inputs (ui)")
  write.xlsx(as.data.frame(final_schedule), paste(wd_output_files, paste("CreelSchedule_", paste(ui_water_bodies, collapse = ","),"_", format(ui_startDate, "%b%Y"), "_thru_", format(ui_endDate, "%b%Y"),".xlsx", collapse="", sep=""), sep="/"), row.names=F, sheetName = "schedule", append = TRUE) 
  write.xlsx(as.data.frame(schedule_notes), paste(wd_output_files, paste("CreelSchedule_", paste(ui_water_bodies, collapse = ","),"_", format(ui_startDate, "%b%Y"), "_thru_", format(ui_endDate, "%b%Y"),".xlsx", collapse="", sep=""), sep="/"), row.names=F, sheetName = "definitions", append = TRUE) # 
  write.xlsx(as.data.frame(sub_index_sites |> select(surveyor_num, site_num, site_name)), paste(wd_output_files, paste("CreelSchedule_", paste(ui_water_bodies, collapse = ","),"_", format(ui_startDate, "%b%Y"), "_thru_", format(ui_endDate, "%b%Y"),".xlsx", collapse="", sep=""), sep="/"), row.names=F, sheetName = "index sites by surveyor", append = TRUE) # 
  
  print("Creel schedule output file generated")