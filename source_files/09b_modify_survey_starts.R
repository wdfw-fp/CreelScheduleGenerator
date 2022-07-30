#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the final schedule by either (1) updating the "survey_start", or (2) keeping survey start times the same
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final_draft<-
    dates_final |> 
    left_join(index_final, by = c("date", "shift")) |> 
    relocate(survey_end, .after = last_col()) |> 
    mutate(temp_start = paste(dates_final$date, ui_modify_ss_index1)) %>%   
    mutate(across('temp_start', ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S"))) 
  
  final_unchanged<-
    final_draft |> 
    filter(!shift %in% ui_modify_ss_shifts) |> 
    select(-temp_start)
  
  final_changed<-
    final_draft |> 
    filter(shift %in% ui_modify_ss_shifts) |> 
    mutate(index_start = ifelse(index_time_1<=temp_start, "Yes", "No"))  

  final_changed_start_to_index<-
    final_changed |> 
    filter(index_start == "Yes") |> 
    select(-survey_start) |> 
    mutate(survey_start = index_time_1) |>  
    relocate(survey_start, .after = shift)  |> 
    select(-temp_start, -index_start)
  
  final_changed_start_to_latest<- 
    final_changed |> 
    filter(index_start == "No") |> 
    select(-survey_start) |> 
    mutate(survey_start = temp_start) |>  
    relocate(survey_start, .after = shift)  |> 
    select(-temp_start, -index_start)

  final_all<-
    rbind(final_unchanged, final_changed_start_to_index, final_changed_start_to_latest) |> 
    mutate(survey_end = survey_start + survey_time_final*60*60)  |> 
    arrange(date, shift)
  
final_schedule<-
  final_all |> 
  mutate(shift_start = survey_start - (ui_drive_time/2)*60*60, shift_end = survey_end + (ui_drive_time/2)*60*60) |> 
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
  mutate(across(starts_with("index"), ~format(as.POSIXct(.), "%H:%M"))) |>
  replace_na(list(shift = "OFF")) %>%
  mutate_at(vars(-c(date)), ~replace(., is.na(.), "-")) |> 
  left_join(day_length, by = "date") |> 
  mutate(
        sunrise = format(sunrise, format = "%H:%M")
      , sunset = format(sunset, format = "%H:%M")
  )


updates_to_shift1_survey_starts<-
  date_times_preview |> 
  mutate(start_diff_index = ifelse(survey_start != index_time_1, "yes","no")) |> 
  filter(shift ==1) |> 
  select(date, shift, survey_start, start_diff_index) |>
  rename(old_start = survey_start) |> 
  left_join(final_schedule |> select(date, shift, survey_start, index_time_1), by=c("date", "shift")) |> 
  rename(new_start = survey_start) |> 
  select(date, start_diff_index, old_start, new_start, index_time_1) |> 
  mutate(change_hrs = as.numeric(strptime(new_start, "%H:%M") - strptime(old_start, "%H:%M"))/60/60)


  




