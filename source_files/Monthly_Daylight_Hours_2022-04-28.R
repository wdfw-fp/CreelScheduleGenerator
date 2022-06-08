d_days <- tibble(
  event_date = seq(
    as.Date(startDate, "%Y-%m-%d"),
    as.Date(endDate, "%Y-%m-%d"),
    by = "day")
) |> 
  mutate(
    Day = weekdays(event_date),
    DayType = if_else(Day == "Saturday" | Day == "Sunday" | Day %in% dates_holidays_2015_2030, "Weekend", "Weekday"),
    DayType_num = if_else(str_detect(DayType, "end"),1,0),
    # Week = as.numeric(format(event_date, "%V")),
    Week = lubridate::isoweek(event_date), #week defined as Monday to SUnday
    Month = as.numeric(format(event_date, "%m")))


legal_fishing_times <- suncalc::getSunlightTimes(
  date = d_days$event_date,
  tz = "America/Los_Angeles",
  #need to add flexibility for other rivers/multiple lines in River.Locations lut
  lat = river_loc$Lat,
  lon = river_loc$Long) |>
  mutate(
    legal_fishing_start = sunrise - 1*60*60,
    legal_fishing_end = sunset + 1*60*60,
    daylength = legal_fishing_end - legal_fishing_start) |> 
  select(
    event_date = date,
    legal_fishing_start,
    legal_fishing_end,
    daylength) 


d_days_final <- d_days |> 
  left_join(legal_fishing_times, by = "event_date") |> 
  mutate(
    start_time = format(as.POSIXct(legal_fishing_start), format = "%H:%M"),
    end_time = format(as.POSIXct(legal_fishing_end), format = "%H:%M"))

d_days_final$start_time.d <- sapply(strsplit(d_days_final$start_time,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

d_days_final$end_time.d <- sapply(strsplit(d_days_final$end_time,":"),
                              function(x) {
                                x <- as.numeric(x)
                                x[1]+x[2]/60
                              }
)


day_length_monthly <- d_days_final |>
  group_by(Month) |> 
  summarise(
    # Dawn = mean(start_time),
    d.Dawn = mean(start_time.d),
    # Dusk = mean(end_time),
    d.Dusk = mean(end_time.d)
  )

write_csv(day_length_monthly, paste0(source_file_wd, "/02_Skagit.DayLength.csv"))

      