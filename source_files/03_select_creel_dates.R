#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file randomly selects the creel surveys sample dates from the sampling frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create updated sampling frame ("dat_updated") by removing any dates identified in either "drop_holidays"  or "drop_other" from sampling frame (if desired)
  ifelse(ui_remove_holidays=="Yes", drop_holidays <- holidays_creel, drop_holidays <-c(""))
  ifelse(length(ui_dates_to_remove)>0, drop_other <- as.Date(ui_dates_to_remove), drop_other <-c(""))
  drop_dates<-c(drop_holidays |> select(dates) |> pull(), as.Date(drop_other))
  dat_updated<-dat |> filter(!dates %in% as.Date(drop_dates))

# Identify number of days to sample (i.e., conduct a creel survey on) per week by day-type
  weekendcount<-ui_surveys_perweek_weekend                          # Number of weekend days to sample per week
  weekdaycount<-ui_surveys_perweek_total - ui_surveys_perweek_weekend  # Number of weekday days to sample per week
  numweeks<-length(unique(dat$weeknum.adj))                      # Number of sample weeks
    
#Create empty matrix to fill with sample dates  
  sampl_dates<-matrix(NA,numweeks,weekdaycount+weekendcount)

#Loop through all potential dates, sample, and fill dates into "sampledat" matrix
  for(row in 1:(numweeks)){
    # Weekdays
        dat.temp.weekday<-dat_updated[dat_updated$weeknum.adj==row & dat_updated$weekend==0,]
          if(ui_reduce_surveys_holidays == "Yes" & ui_reduce_daytype == "weekday" & unique(dat.temp.weekday$weeknum) %in% holidays_creel$weeknum){
            weekdaycount_temp<- weekdaycount - (holidays_creel |> filter(weeknum %in% unique(dat.temp.weekday$weeknum)) |> count(weeknum) |> pull())
          }else{
            weekdaycount_temp<- weekdaycount
          }
        if(nrow(dat.temp.weekday)<1){
          samplweekdays<-rep(NA,weekdaycount_temp)
        }else{
          if(nrow(dat.temp.weekday)<weekdaycount_temp){
            samplweekdays<-c(as.numeric(mysample(as.character(dat.temp.weekday$Num), nrow(dat.temp.weekday), F)), rep(NA,weekdaycount_temp-nrow(dat.temp.weekday)))   
            
          }else{
            samplweekdays<-mysample(dat.temp.weekday$Num, weekdaycount_temp, F)      
          }
        }
      if(length(samplweekdays)<weekdaycount){
        samplweekdays<-c(samplweekdays, rep(NA, (weekdaycount - length(samplweekdays))))
      }  
    # Weekends
        dat.temp.weekend<-dat_updated[dat_updated$weeknum.adj==row & dat_updated$weekend==1,]
          if(ui_reduce_surveys_holidays == "Yes" & ui_reduce_daytype == "weekend" & unique(dat.temp.weekend$weeknum) %in% holidays_creel$weeknum){
              weekendcount_temp<- weekendcount - (holidays_creel |> filter(weeknum %in% unique(dat.temp.weekend$weeknum)) |> count(weeknum) |> pull())
            }else{
              weekendcount_temp<- weekendcount
            }
        if(nrow(dat.temp.weekend)<1){
          samplweekends<-rep(NA,weekendcount_temp)
        }else{
          if(nrow(dat.temp.weekend)<weekendcount_temp){
            samplweekends<-c(as.numeric(mysample(as.character(dat.temp.weekend$Num), nrow(dat.temp.weekend), F)), rep(NA,weekendcount_temp-nrow(dat.temp.weekend)))   
            
          }else{
            samplweekends<-mysample(dat.temp.weekend$Num, weekendcount_temp, F)      
          }
        }
      if(length(samplweekends)<weekendcount){
        samplweekends<-c(samplweekends, rep(NA, (weekendcount - length(samplweekends))))
      } 
      sampl_dates[row,]<-c(samplweekdays, samplweekends) 
  }
  
# matrix of randomly selected creel dates (index first column "num")
  sampl_dates 

# extract creel survey days (i.e., samples) from all possible survey dates
  creel_dates<-dat_updated |> filter(Num %in% sampl_dates) |> filter(dates>=as.Date(ui_startDate) & dates<=as.Date(ui_endDate))

#--------------------------------------------------------------
# Create Index for "creel_dates"
#--------------------------------------------------------------   
  creel_dates$Index<-seq(1, nrow(creel_dates),1)
  nrow(creel_dates)
  