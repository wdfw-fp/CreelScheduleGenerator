# Install/Load packages included in "package_list"
  install_or_load_pack <- function(pack){
    create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
    if (length(create.pkg))
      install.packages(create.pkg, dependencies = TRUE)
    sapply(pack, require, character.only = TRUE)
  }

# "mysample" function throughout code with a set.seed
     # Doesn't matter what number this is
    mysample<-function(x,y,z){
      set.seed(seed.number + row + surveyor + day + group) #set.seed to equal "i" (corresponds to rownum in loops) plus "seed.number"
      sample(x=x, size=y, replace=z) 
    }
    row=0; surveyor=0; day=0; group=0; seed.number<-1147856
    mysample2<-function(n,a,b){
      set.seed(group + seed.number) #set.seed to equal "i" (corresponds to rownum in loops) plus "seed.number"
      rdunif(n=n, a=a, b=b) 
    }
    # Note: If you want a "truly" random sample, use this code to get a random "seed.number"
    #seed.number<-floor(runif(1,-1000000,1000000))
  
# Function to ask "not in"
    "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0  

# Month abbreviation to number function
    mo2Num <- function(x) match(tolower(x), tolower(month.abb))

# Create data frame of dates       
date_df<-function(date_list){
  x<- tibble(dates = as.Date(date_list)) |> 
      mutate(
        Num = seq(1, length(dates),1),
        day = weekdays(dates),
        wday = as.numeric(format(dates, "%u")),
        month=format(dates,"%b"),
        month_no=as.numeric(format(dates,"%m")),
        weeknum=(as.numeric(format(dates, "%W"))),
        weeknum_adj=(interval(min(dates), dates) %/% weeks(1)) + 1,
        weekend = ifelse(day %in% ui_daytype_weekends, 1, 0)
      )
  return(x)
}    
        
#Function "GetHolidays"
  GetHolidays <- function(x) { 
  years = as.POSIXlt(x)$year+1900 
  years = unique(years) 
  holidays <- NULL 
  for (y in years) { 
          holidays <- c(holidays, as.character(USNewYearsDay(y))) 
          holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
          holidays <- c(holidays, as.character(USPresidentsDay(y)))
          holidays <- c(holidays, as.character(USMemorialDay(y))) 
          holidays <- c(holidays, as.character(timeDate(as.character(y * 10000 + 619)))) # Juneteenth
          holidays <- c(holidays, as.character(USIndependenceDay(y))) 
          holidays <- c(holidays, as.character(USLaborDay(y))) 
          holidays <- c(holidays, as.character(USVeteransDay(y))) 
          holidays <- c(holidays, as.character(USThanksgivingDay(y))) 
          holidays <- c(holidays, as.character(timeDate(as.character(.nth.of.nday(y, 11, 5, 4))))) #Native American Heritage Day
          holidays <- c(holidays, as.character(USChristmasDay(y))) 
   } 
   holidays = as.Date(holidays,format="%Y-%m-%d") 
   #ans = x %in% holidays 
  return(holidays) 
  } 
  