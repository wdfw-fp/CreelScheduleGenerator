dates_final<-
  dat |> 
  select(dates, day, fishery_status, holiday)|> 
  filter(dates>=as.Date(ui_startDate) & dates<=as.Date(ui_endDate)) |> 
  rename(date = dates) |> 
  left_join(
    creel_date_times |> 
      select(-shift_size, -earliest_river_start)  
    , by = "date") 


index_final<-
  index_times_loc |> 
  pivot_wider(
    names_from = index_count
    , values_from = c(index_start_time, start_location)
  )
  
if(drop_start_location == "Yes"){index_final<-index_final |> select(!contains("start_location"))}

date_times_final<-
  dates_final |> 
  left_join(index_final, by = c("date", "shift")) |> 
  relocate(river_end, .after = last_col()) |> 
  left_join(final_census_date_time |> select(-day), by = c("date", "shift")) |>
  mutate(
        river_start = format(river_start, format = "%H:%M")
      , river_end = format(river_end, format = "%H:%M")
      , census_start_time = format(census_start_time, format = "%H:%M")
    )|> 
  rename(census_count = index_count) |>
  mutate(across(starts_with("index"), ~format(as.POSIXct(.), "%H:%M"))) |>
  replace_na(list(shift = "OFF")) %>%
  mutate_at(vars(census_count), ~replace(., !is.na(.), "Yes")) |> 
  mutate_at(vars(-c(date)), ~replace(., is.na(.), "-")) #|> 

