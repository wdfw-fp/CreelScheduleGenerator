#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 09 - This file randomly selects survey dates on which to conduct tie-in surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create sample frame of all dates when a census survey could occur based on all creel survey dates
  smpl_fram_census<-creel_dates |> select(dates, Num, weeknum, weeknum_adj, weekend)
  season_length_days<-max(smpl_fram_census$Num) - min(smpl_fram_census$Num)
  census_ct_int<- ceiling(season_length_days/ui_num_census_counts)
  hypoth_census_Num<-seq(from = min(smpl_fram_census$Num), by = census_ct_int, length.out = ui_num_census_counts)

  initital_census_Num<-smpl_fram_census |> filter(Num %in% hypoth_census_Num) |> pull(Num)
  leftover_hypoth_census_date<-hypoth_census_Num[!hypoth_census_Num %in% initital_census_Num]
  final_census_Num<-c(initital_census_Num)
  if(length(leftover_hypoth_census_date)>0){
    for(day in 1:length(leftover_hypoth_census_date)){
      sub_census_day<-
        smpl_fram_census |> 
        mutate(diff_days = abs(Num - leftover_hypoth_census_date[day])) |> 
        filter(diff_days == min(diff_days)) |> 
        slice(1) |> 
        pull(Num)
      
      final_census_Num<-c(final_census_Num, sub_census_day)
    }
  }
  final_census_Num<-sort(final_census_Num)
  
final_census_dates<-
  creel_dates |> 
  filter(creel_dates$Num %in% final_census_Num) |> 
  select(date = dates, day) 

closet_to_midday_index_times<-  
  index_times_loc |> 
  left_join(shift_eval |> select(date, mid_day), by="date") |>
  mutate(diff_time = abs(mid_day -  times(strftime(index_start_time,"%H:%M:%S")))) |> 
  group_by(date) |> 
  slice_min(diff_time) |> 
  select(date, shift, index_count, index_start_time)

final_census_date_time<-
  final_census_dates |> 
  left_join(closet_to_midday_index_times, by = "date") |> 
  rename(census_start_time = index_start_time)
