#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the sampling frame for the creel surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dataframe of all possible creel survey dates
  # adjusted_startDate<-as.Date(cut(as.Date(ui_startDate), breaks = 'week', start.on.monday = T))
  # adjusted_endDate  <-as.Date(cut(as.Date(ui_endDate)+7, breaks = 'week', start.on.monday = T))-1
  # dates<-seq(adjusted_startDate, adjusted_endDate, 1)
  
  dates<-seq(ui_startDate, ui_endDate, 1)
  dat<-date_df(dates) # Use "date_df" function to create "dat" sample frame

# Return ERROR messages for incorrect specifications of closure dates
  if(length(ui_closed_weekdays)>0 & any(!ui_closed_weekdays %in% unique(weekdays(dates))) == TRUE){print("***ERROR: Weekday closure [name] incorrectly entered***")}
  if(length(ui_closed_weekdays)>0 & length(ui_closed_start_end)!=2){print("***ERROR: Need to enter start and end dates for when weekday closures will occur***")}
  if(length(ui_closed_weekdays)==0 & length(ui_closed_start_end)>0){print("***ERROR: Need to supply at least one weekday closure [name] if closure dates are supplied")}

# Create list of all closure dates
  closures_weekdays<-
    dat |> 
    filter(
      if(length(ui_closed_weekdays)>0){day %in% ui_closed_weekdays & dates>=as.Date(ui_closed_start_end[1]) & dates<=as.Date(ui_closed_start_end[2])}else{ dates<min(dates)}
    )
  if(nrow(closures_weekdays)==0){closures_weekdays<-NULL}else{closures_weekdays<-closures_weekdays |> pull(dates)}
  closures_all<-c(closures_weekdays, ui_closed_dates)
  
# Identify Federal holidays and ones that fall within the creel survey schedule period
  holidays_all<-GetHolidays(unique(dat$dates))
  holidays_creel<- date_df(holidays_all) |> filter(dates>=ui_startDate & dates<=ui_endDate) 
 
# Create two new columns that specifies : 1) the status of fishery as either "Open" or "Closed", 2) if the date is a holiday ("Yes") or not ("No")     
 dat<-   
   dat |> 
   mutate(  fishery_status = if_else(dates %in% as.Date(closures_all), "Closed", "Open")
          , holiday = if_else(dates %in% holidays_creel$dates, "Yes", "No")
          )
