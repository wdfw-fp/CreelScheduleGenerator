#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the sampling frame for the creel surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dataframe of all possible creel survey dates
  adjusted.start.date<-as.Date(cut(as.Date(ui_startDate), breaks = 'week', start.on.monday = T))
  adjusted.end.date  <-as.Date(cut(as.Date(ui_endDate)+7, breaks = 'week', start.on.monday = T))-1

  dates<-seq(adjusted.start.date, adjusted.end.date, 1)
  dat<-date_df(dates) # Use "date_df" function to create "dat" sample frame
  
# If needed, removing any dates identified in either "drop_holidays"  or "drop_other" from sampling frame (if desired)
  if(ui_fishery_closures == "Yes"){
    drop_closures_weekdays<-
      dat |> 
      filter(day %in% ui_closed_weekdays) |> 
      pull(dates)
    
    drop_closure_dates<-ui_closed_dates
    drop_closure_all<-c(drop_closures_weekdays,drop_closure_dates)
    
    dat<-
      dat |> filter(!dates %in% as.Date(drop_closure_all)) 
  }

# Identify Federal holidays and ones that fall within the creel survey schedule period
  holidays_all<-GetHolidays(unique(dat$dates))
  holidays_creel<- date_df(holidays_all) |> filter(dates>=ui_startDate & dates<=ui_endDate)
  