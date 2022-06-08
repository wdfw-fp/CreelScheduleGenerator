#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This creates the sampling frame for the creel surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dataframe of all possible creel survey dates
  adjusted.start.date<-as.Date(cut(as.Date(ui_startDate), breaks = 'week', start.on.monday = T))
  adjusted.end.date  <-as.Date(cut(as.Date(ui_endDate)+7, breaks = 'week', start.on.monday = T))-1

  dates<-seq(adjusted.start.date, adjusted.end.date, 1)
  dat<-date_df(dates) # Use "date_df" function to create "dat" sample frame

# Identify Federal holidays and ones that fall within the creel survey schedule period
  holidays_all<-GetHolidays(unique(dat$dates))
  holidays_creel<- date_df(holidays_all) |> filter(dates>=ui_startDate & dates<=ui_endDate)
  