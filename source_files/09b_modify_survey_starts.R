#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the final schedule by either (1) updating the "survey_start", or (2) keeping survey start times the same
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_draft<-
    dates_final |> 
    left_join(index_final, by = c("date", "shift")) |> 
    relocate(survey_end, .after = last_col()) |> 
    left_join(sampl_fram_time |> select(date, latest), by = "date" ) |> 
    mutate(temp_start = paste(dates_final$date, ui_modify_ss_latest)) %>%   
    mutate(across('temp_start', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S"))) |>
    mutate(temp_end =  temp_start + (survey_time_final*60*60)) |> 
    mutate(darkness = ifelse(temp_end>latest, "Yes", "No" )) |> 
    mutate(index_b4_temp = ifelse(index_time_1<=temp_start, "Yes", "No")) |> 
    mutate(temp_b4_survey = ifelse(temp_start<=survey_start, "Yes", "No")) |> 
    mutate(latest_start = latest - (survey_time_final*60*60)) |> 
    mutate(index_b4_latest = ifelse(index_time_1<=latest_start, "Yes", "No")) |> 
    mutate(latest_b4_temp = ifelse(latest_start<=temp_start, "Yes", "No"))
  
  final_unchanged<-
    final_draft |> 
    filter(shift != 1 | is.na(shift)==TRUE | temp_b4_survey == "Yes") 
  
  final_changed<- 
    final_draft |>
    filter(!date %in% as.Date(final_unchanged |> pull(date))) 
  
  final_changed_start_to_index1<-    
    final_changed |> 
    filter(index_b4_temp == "Yes" & index_b4_latest == "Yes") |> 
    select(-survey_start) |> 
    mutate(survey_start = index_time_1) |>  
    relocate(survey_start, .after = shift) 
  
  final_changed_start_to_latest<- 
    final_changed |> 
    filter(!date %in% as.Date(final_changed_start_to_index1 |> pull(date))) |> 
    filter(latest_b4_temp == "Yes") |> 
    select(-survey_start) |> 
    mutate(survey_start = latest_start) |>  
    relocate(survey_start, .after = shift)
  
  final_changed_start_to_temp<-    
    final_changed |> 
    filter(!date %in% as.Date(final_changed_start_to_index1 |> pull(date))) |> 
    filter(latest_b4_temp == "No") |> 
    select(-survey_start) |> 
    mutate(survey_start = temp_start) |>  
    relocate(survey_start, .after = shift)
  
  final_all<-
    rbind(final_unchanged, final_changed_start_to_index1, final_changed_start_to_latest, final_changed_start_to_temp) |>
    mutate(survey_end = survey_start + survey_time_final*60*60)  |>
    select(-temp_start, -temp_end, -temp_b4_survey, -index_b4_temp, -darkness, -latest_start, -latest, -index_b4_latest, -latest_b4_temp)|>
    arrange(date, shift)

  final_schedule<-
    final_all |> 
    mutate(
      shift_start = lubridate::floor_date(survey_start - (ui_drive_time/2)*60*60, "15 minutes"), 
      shift_end = lubridate::ceiling_date(survey_end + (ui_drive_time/2)*60*60, "15 minutes")) |> 
    relocate(shift_start, .before = survey_start) |> 
    relocate(shift_end, .after = survey_end) |> 
    left_join(final_census_date_time |> select(-day), by = c("date", "shift")) |>
    select(-index_count) |>
    mutate(
          shift_start = format(shift_start, format = "%H:%M")
        , survey_start = format(survey_start, format = "%H:%M")
        , survey_end = format(survey_end, format = "%H:%M")
        , shift_end = format(shift_end, format = "%H:%M")
        , census_start_time = format(census_start_time, format = "%H:%M")
      )|>
    mutate(across(starts_with("index"), ~format(as.POSIXct(.), "%H:%M")), shift = as.character(shift)) |> 
    replace_na(list(shift = "OFF")) %>%
    mutate_at(vars(-c(date)), ~replace(., is.na(.), "-")) |> 
    left_join(day_length, by = "date") |> 
    mutate(
          sunrise = format(sunrise, format = "%H:%M")
        , sunset = format(sunset, format = "%H:%M")
    )
  
  if(ui_num_census_counts==0){
    final_schedule<-final_schedule |> select( -census_start_time)
  }
  
  updates_to_shift1_survey_starts<-
    date_times_preview |> 
    filter(shift == 1) |> 
    select(date, shift, survey_start, index_time_1, survey_end) |>
    rename(survey_start_initial = survey_start, survey_end_initial = survey_end) |>
    left_join(final_schedule |> select(date, shift, survey_start, survey_end, sunrise, sunset), by=c("date", "shift")) |>
    rename(survey_start_final = survey_start, survey_end_final = survey_end) |>
    select(date, sunrise, survey_start_initial, survey_start_final, index_time_1, survey_end_initial, survey_end_final, sunset) |> 
    mutate(
      change_hrs = as.numeric(strptime(survey_start_final, "%H:%M") - strptime(survey_start_initial, "%H:%M"))/60/60,
      index_b4_start = if_else(strptime(survey_start_final, "%H:%M")>strptime(index_time_1, "%H:%M"), "Yes", "-"),
      end_minus_sunset = (as.numeric(strptime(survey_end_final, "%H:%M")) - as.numeric(strptime(sunset, "%H:%M")))/(60*60)
      )
  