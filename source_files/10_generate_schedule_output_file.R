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
  c(  "shift", "Period number within a day when a creel survey will be conducted"
    , "shift_start", "Approx. time of day when a technician should begin their work day shift (e.g., arrive at their duty station)"
    , "survey_start", "Time when the technician(s) needs to arrive at the waterbody and be ready to begin the creel survey"
    , "index_time_nth", "Time when the technician(s) need to start the 'nth' index effort count"
    , "survey_end", "Approx. time when the technician(s) should be leaving the waterbody and driving back to their duty station"
    , "shift_end", "Approx. time when the technician(s) should end their work day shift (i.e., leave their duty station)"
    , "census_start_time", "Time when census count will be conducted (subject to scheduling; adjust census & index times within the day as needed)"
    )
  , ncol = 2, byrow = T)) 
colnames(schedule_notes)<-c("Field", "Definition")

# Write schedule to output folder
  ifelse(!dir.exists(wd_output_files), {dir.create(wd_output_files); "Output sub-folder created"},"Output sub-folder exists already")

  sheets <- list("ui_list_total" = ui_list_total,
                 "final_schedule" = final_schedule,
                 "schedule_notes" = schedule_notes,
                 "sub_index_sites" = sub_index_sites)
  
  sheet_names <- list("user inputs (ui)", "schedule", "definitions", "index sites by surveyor")
  
  openxlsx::write.xlsx(x = sheets, 
                       file = paste0(wd_output_files, "/CreelSchedule_", ui_water_bodies, "_", format(ui_startDate, "%b%Y"), "_thru_", format(ui_endDate, "%b%Y"), ".xlsx"),
                       sheetName = sheet_names)
  
  print("Creel schedule output file generated")