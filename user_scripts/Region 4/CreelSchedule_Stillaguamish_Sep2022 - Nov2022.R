#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file creates roving creel survey schedule using a multi-stage, stratified, probabilistic sampling approach
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#NOTE: there are many objects that need to be specified by the user in sections 2-6 and 8, which are denoted with a leading "ui_" (user_input) 

#---------------------------------------------------------------------------------------------------------- -  
# (1) SPECIFY WORKING DIRECTORIES, LOAD R PACKAGES, FUNCTIONS, AND X-WALK TABLES                         ----
#---------------------------------------------------------------------------------------------------------- -
# Specify relative working directories for sub-folders:
  wd_input_files  <-"luts"         # Location where look-up table (lut) files are stored
  wd_source_files <-"source_files" # Location of source file (code working "behind the scenes")
  wd_output_files <-"output"       # Location of summary figures/tables and model results

# Load functions
  source(paste0(wd_source_files, "/01_Load_Functions.R"))
  
# Install/Load packages
  package_list<-c("here", "tidyverse", "xlsx", "timeDate", "suncalc", "lubridate", "chron") 
  install_or_load_pack(package_list)  
    
# Load look-up tables
  lut <- map(
    list(
        water_body = file.path(wd_input_files, "lut_waterbody_lat_long_master.csv")
      , locations = file.path(wd_input_files, "lut_waterbody_locations_master.csv")
      ),
    ~readr::read_csv(file.path(.x), show_col_types = FALSE)
  )

#---------------------------------------------------------------------------------------------------------- -  
# (2) DEFINE AMONG DAY SAMPLING FRAME (i.e., potential dates to be creel sampled)                        ----
#---------------------------------------------------------------------------------------------------------- -    
# Specify start and end dates for creel survey schedule (format "yyyy-mm-dd")
  ui_startDate<-as.Date(c("2022-09-01")) 
  ui_endDate<-as.Date(c("2022-11-30")) 
    
# Specify groups of days within a week that should be considered a "weekend" (where angler effort is anticipated to be highest and similar)  
  ui_daytype_weekends <-c("Saturday", "Sunday") # NOTE: Default is "Saturday" and "Sunday" but often "Friday" can be consider weekend day too

# If needed, specify days of the week and/or individual dates when the (entire) fishery will be closed (NOTE: do NOT enter any dates where only portions of the fishery will closed.  Here adjustments will need to be made to data collection protocol)
  ui_closed_weekdays<-c()  # If applicable, list weekdays that the fishery will be closed; Example: c("Sunday", "Monday")
  ui_closed_start_end<-c() # If certain weekdays are closed, enter the start and end dates for closures; Example: c("2022-08-07", "2022-08-31")
  ui_closed_dates<-c()     # If applicable, list individual closure dates; NOTE: these can be in addition to any "ui_closed_weekdays"; Example: c("2022-08-24", "2022-08-31")   

# Create full sampling frame
  source(paste0(wd_source_files, "/02_smpl_fram_dates.R"))
    
# Preview sample frame (NOTE: don't worry if dates in this output don't exactly match the input date range above)
  dat 
    
#---------------------------------------------------------------------------------------------------------- -  
# (3) [RANDOMLY] SELECT CREEL SURVEY DATES (FROM AMONG DAY SAMPLING FRAME)                               ----
#---------------------------------------------------------------------------------------------------------- -   
# Specify the number of creel survey days per week 
  ui_surveys_perweek_total   <- 4  # TOTAL number per week
  ui_surveys_perweek_weekend <- 2  # Weekend days per week (subset of total)

# Do you want to potentially creel sample dates when the fishery is closed   
  ui_survey_closures<-c("No") # Enter "Yes" or "No"; NOTE: we are generally trying to survey legal fishing; therefore, default should be "No"
    
# Do you want to remove all from potential creel schedule (if "Yes", creel technician will NOT be schedule to work on the exact date of all holidays)
  ui_remove_holidays<-c("Yes") # Enter "Yes" or "No"
  
# Do you want to reduce the number of survey day by the number of holidays within a week?
  ui_reduce_surveys_holidays<-c("Yes") # Enter "Yes" (Default) or "No"
  ui_reduce_daytype<-c("weekday")      # If "Yes" to previous question, enter "weekday" or "weekend" to specific what daytype the reduction in creel days should come from

# List any other specific dates from potential creel schedule that aren't identified in the list of holidays?
  ui_dates_to_remove<-c() # format: "2014-06-30"

# Select creel survey dates  
  seed.number<-2  #Set seed number for this section 
  source(paste0(wd_source_files, "/03_select_creel_dates.R"))

#Preview creel schedule        
  creel_dates

#---------------------------------------------------------------------------------------------------------------------------- -  
# (4) DEFINE WITHIN DAY SAMPLING FRAME & EVALUATE SUGGESTED NUMBER OF SHIFTS PER DAY FOR GIVEN SHIFT LENGTHS ----
#---------------------------------------------------------------------------------------------------------------------------- - 
# Sample location(s) 
    lut$water_body |> print(n=50) # List of water bodies and their associated lat/long data that have been entered into the "master" LUT list
    ui_water_bodies<-c("Stillaguamish River - Mainstem", "Stillaguamish River - South Fork", "Stillaguamish River - North Fork") # Enter the waterbody(s) that will be surveyed (case sensitive and must match water_body listed in lut$water_body
# Set bounds for the time during any given day when creel surveys can occur; should be based on fishing regulations (i.e., legal fishing hours)
    # Step #1: Select general strategy for [earliest] start and [latest] end time
    ui_start_time<-c("sunrise") # enter either "sunrise" or "manual" 
    ui_end_time<-c("sunset")    # enter either "sunset"  or "manual" 
    # Step #2A: If necessary, specify an offset to the start and end times  (e.g., if legal fishing occurs 1 hr. prior to sunrise & sunset, enter 1 below for both); enter 0 if no offset needed
    ui_start_adj<-c(1) # Specify an offset for the start time (in hours);  
    ui_end_adj<-c(1)   # Specify an offset for the end time (in hours)
    # Step #2B: If "manual" entered for "ui_start_time" or "ui_end_time", enter the earliest start and/or latest end time for a creel survey event
    ui_start_manual<-c("") # Specify manual start time (format "HH:MM:SS", e.g., 6 AM = "06:00:00")
    ui_end_manual<-  c("") # Specify manual end time (format "HH:MM:SS")  
# Estimated total non-river time for a given survey date (e.g., daily drive time (hours) to/from duty station to sampling location (i.e., time per shift not spent on the water sampling) 
    ui_drive_time<-2
    
# Potential shift lengths (i.e., total work day length in hours)
    potential_shift_length<-c(8, 10) # Generally 8 or 10 to accommodate 5-8s or 4-10s weekly work schedules

# Run source code    
  source(paste0(wd_source_files, "/04_smpl_fram_time.R"))   
 
# Evaluate within day sample frame and        
  shift_eval |> print(n =Inf)  #NOTE: negative overlap is the maximum separation between shifts 

#---------------------------------------------------------------------------------------------------------------------------- -  
# (5) [RANDOMLY] SELECT CREEL SURVEY TIMES (FROM WITHIN DAY SAMPLING FRAME)   ----
#---------------------------------------------------------------------------------------------------------------------------- -             
# Shifts & Shift Length
  ui_shifts_total<-1   # Total number of potential shifts per day
  ui_shifts_smpl<-1    # Number of shifts to sample within a day
  ui_shift_length<-10  # shift length (i.e., length of entire work day including drive time to and from river from office/home)   

# Run source code
  seed.number<-123
  source(paste0(wd_source_files, "/05_select_creel_times.R"))

# Preview output
  creel_date_times |> print(n=10)
  creel_date_times |> count(shift)
      
#---------------------------------------------------------------------------------------------------------- -  
# (6) SELECT RANDOM INDEX EFFORT COUNT TIMES                    ----
#---------------------------------------------------------------------------------------------------------- -
# Index effort count details
  ui_num_index_counts<-3   # Number of index effort counts per shift
  ui_index_count_time<-0.75   # [Estimated] time (in hours) it takes to conduct an entire index effort count circuit based on anticipated approach (e.g., number/location of sites, number of creelers conducting index count simultaneously) 

# Run source code
  seed.number<-1000
  source(paste0(wd_source_files, "/06_select_index_effort_times.R"))  
      
# NOTE TO SELF: UPDATE PLOT (EXPAND INDEX COUNT TIMES )  
  
# Preview schedule
  index_times|> print(n=10)

#---------------------------------------------------------------------------------------------------------- -  
# (7) Generate start location for each index count by surveyor
#---------------------------------------------------------------------------------------------------------- -
# Subset list of index survey sites  
  (sub_index_sites<-lut$locations |> filter(water_body %in% ui_water_bodies & survey_type == "index"))

# Number of surveyors that will be used to complete each index count (i.e., number of surveyors working together to complete an index count circuit)  
  (num_surveyors<-length(unique(sub_index_sites$surveyor_num))) # This number is defined in the section lut (i.e., lut$sections)
  
# Run Source Code
  seed.number<-123
  source(paste0(wd_source_files, "/07_select_index_start_location.R"))  

# Preview schedule
  index_times_loc |> print(n=20) 
  sub_index_sites |> select(surveyor_num, site_num, site_name)
  #NOTE: The "start_location" code is divided into three parts: 'surveyor_section'_'site_num'_'direction' (Up = Upstream or Dw = downstream)

#---------------------------------------------------------------------------------------------------------- -  
# (8) SELECT CENSUS (i.e., TIE-IN) EFFORT COUNT SURVEY DATES
#---------------------------------------------------------------------------------------------------------- -
# Use the following summaries to specify the TOTAL number of tie-in counts to conduct across the entire length of the survey period
  creel_dates |> group_by(weekend) |> distinct(Num) |> summarise("total days" = n())      # Total number of days surveys by daytype (1=weekend, 0 = weekday)
  creel_dates |> group_by(weekend) |> distinct(weeknum) |> summarise("total weeks" = n()) # Total number of unique weeks surveyed by daytype
  creel_dates |> distinct(weeknum_adj)
  
# Specify total number of tie-in counts per frequency interval
  ui_num_census_counts<-6
  # NOTE: right now, script only set up to select a total number of census counts & distributes them evenly throughout the season
  # ...but could update script to allow for different frequencies (e.g., weekly, monthly) plus what daytype the surveys would occur on
  # # Specify the frequency at which tie-in counts should occur  
  #   ui_census_count_freq<-c()
  # # Select daytype for tie-in
  #   ui_census_daytype<-c("either") # enter "weekend", "weekday", or "either"

# Run source code
  source(paste0(wd_source_files, "/08_select_census_count_dates.R"))  
    
# [Suggested] Census count dates
  final_census_date_time

#---------------------------------------------------------------------------------------------------------- -  
# (9) PREVIEW SCHEDULE
#---------------------------------------------------------------------------------------------------------- -  
# Format final schedule
  drop_start_location<-c("Yes")  #Enter "Yes" or "No"
  source(paste0(wd_source_files, "/09a_preview_formatted_schedule.R")) 
    
# Preview schedule
  date_times_preview |> print(n=100)
  
# Decide if you want to modify the "survey_start" under specific conditions
  ui_modify_ss<-c("Yes")          # Enter "Yes" if you want to update "survey_start" (which will also effect "survey_end")
  ui_modify_ss_shifts<-c(1)       # Enter the "shift" number where the "survey_start" can be modified
  ui_modify_ss_latest<-c("10:00:00") # Enter the latest "survey_start" time (if "index_time_1" isn't earliest)
  
# If desired, updated "survey_start" times 
  if(ui_modify_ss == "Yes"){source(paste0(wd_source_files, "/09b_modify_survey_starts.R"))}else{final_schedule<-date_times_preview} 
  if(ui_modify_ss == "Yes"){updates_to_shift1_survey_starts |> print(n = Inf)}
  
# Final schedule
  final_schedule |> print(n=10)  

#---------------------------------------------------------------------------------------------------------- -  
# (10) GENERATE FILE WITH SCHEDULE WITH ASSOCIATED USER INPUTS, NOTES, AND LIST OF INDEX SITES BY SURVEYOR(S)
#---------------------------------------------------------------------------------------------------------- -  
  source(paste0(wd_source_files, "/10_generate_schedule_output_file.R"))
  