#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the "preview" (final) schedule by combining selected survey dates and index times/locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dates_final<-
  dat |> 
  select(dates, day, fishery_status, holiday)|> 
  filter(dates>=as.Date(ui_startDate) & dates<=as.Date(ui_endDate)) |> 
  rename(date = dates) |> 
  left_join(
    creel_date_times |> 
      select(-shift_size, -earliest)  
    , by = "date") 


index_final<-
  index_times_loc |> 
  rename(index_time = index_start_time) |> 
  pivot_wider(
      names_from = index_count
    , values_from = c(index_time, start_location)
  )
  
if(drop_start_location == "Yes"){index_final<-index_final |> select(!contains("start_location"))}

date_times_preview<-
  dates_final |>
  mutate(shift_start = lubridate::floor_date(survey_start - (ui_drive_time/2)*60*60, "15 minutes"), shift_end = lubridate::ceiling_date(survey_end + (ui_drive_time/2)*60*60, "15 minutes")) |> 
  
  relocate(shift_start, .before = survey_start) |> 
  left_join(index_final, by = c("date", "shift")) |> 
  relocate(survey_end, .after = last_col()) |> 
  relocate(shift_end, .after = survey_end) |> 
  left_join(final_census_date_time |> select(-day), by = c("date", "shift")) |>
  mutate(
        shift_start = format(shift_start, format = "%H:%M")
      , survey_start = format(survey_start, format = "%H:%M")
      , survey_end = format(survey_end, format = "%H:%M")
      , shift_end = format(shift_end, format = "%H:%M")
      , census_start_time = format(census_start_time, format = "%H:%M")
    )|> 
  rename(census_count = index_count) |>
  mutate(across(starts_with("index"), ~format(as.POSIXct(.), "%H:%M"))) |>
  replace_na(list(shift = "OFF")) %>%
  mutate_at(vars(census_count), ~replace(., !is.na(.), "Yes")) |> 
  mutate_at(vars(-c(date)), ~replace(., is.na(.), "-")) |> 
  left_join(day_length, by = "date") |> 
  mutate(
        sunrise = format(sunrise, format = "%H:%M")
      , sunset = format(sunset, format = "%H:%M")
  )

if(ui_num_census_counts==0){
  date_times_preview<-date_times_preview |> select( -census_start_time)
}
