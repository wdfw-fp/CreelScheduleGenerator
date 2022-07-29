#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file randomly selects the effort count times on sample dates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shift_subunit_time<-round(River_Time_Final/ui_num_index_counts/0.25, 0)*0.25
creel_date_times_index<-creel_date_times
for(index in 1:ui_num_index_counts){
    newname = paste0("index_start_", index)
    creel_date_times_index<-creel_date_times_index|> add_column(temp = as.POSIXct(NA)) |> rename(!!newname := temp) 
}

for(row in 1:nrow(creel_date_times_index)){

  index_1_start_choices<-seq(creel_date_times_index$river_start[row], creel_date_times_index$river_start[row] + (shift_subunit_time - ui_index_count_time)*60*60, ui_index_count_time*60*60)
  creel_date_times_index[row, "index_start_1"]<-mysample(index_1_start_choices,1,F)
  
  for(counts in 1:(ui_num_index_counts-1)){
        creel_date_times_index[row, which(names(creel_date_times_index)==paste0("index_start_",counts+1))]<-creel_date_times_index$index_start_1[row] + (shift_subunit_time*counts*60*60)
  }
  
}

index_times<-
  creel_date_times_index |> 
  select(date, shift, contains("index_start")) |>  
  pivot_longer(
    cols = starts_with("index_start_"),
    names_to = "index_count",
    names_prefix = "index_start_",
    values_to = "index_start_time",
    values_drop_na = TRUE
  )

index_times_dat_plot<-
  index_times|> 
  left_join(dat |> select(dates, month_no, weeknum_adj), by = c("date" = "dates") ) |> 
  mutate(Time_new = hour(index_start_time))

# Plot effort count times (rounded by the hour)
    windows(width=8, height=3*ceiling(length(unique(index_times_dat_plot$month_no))/2))
    par(mfcol=c(ceiling(length(unique(index_times_dat_plot$month_no))/2),2),family='sans', xaxs="i", yaxs="i",cex.axis=1, cex=1, mgp=c(2,0.75, 0.5),mar=c(2,1.5,1,1), oma=c(2.5,2.5,2,1))
    bins<-seq(4,22,1)

  for(month in 1:length(unique(index_times_dat_plot$month_no))){
      sub.Month<-index_times_dat_plot[index_times_dat_plot$month_no==unique(index_times_dat_plot$month_no)[month],]
      sub_month_nm<-creel_dates |> select(month, month_no) |> distinct(month_no, .keep_all = TRUE) |> filter(month_no ==sub.Month$month_no[1]) |> pull(month)
  #Plot distribution of effort times
      #hist(c(sub.Month$Effort.2, sub.Month$Effort.1), main=paste(sub.Month$Month[1], sub.Site$Site[1], sep="-"), xlim=c(min(floor(avg.dawn.dusk$d.Dawn)), max(ceiling(avg.dawn.dusk$d.Dusk))), breaks=max(c(effort.times$Effort.2, effort.times$Effort.1)) - min(c(effort.times$Effort.2, effort.times$Effort.1)) +1)
      effort.plot<-hist(col="gray", sub.Month %>% select(Time_new) %>% gather() %>% pull(value), main=sub_month_nm, breaks=bins, xaxt='n')
      axis(side=1, at = (effort.plot$breaks[seq(1,20,4)] - 0.5), labels = effort.plot$breaks[seq(1,20,4)])
      axis(side=1, at = (effort.plot$breaks - 0.5), labels = rep("", length(effort.plot$breaks)) )
      #legend("topleft", legend=saved.seed.plot, bty='n')

  #Plot time of dawn and dusk
      # abline(v=as.numeric(avg.dawn.dusk[as.character(sub.month$month[1])==as.character(avg.dawn.dusk$Month), "d.Dawn"]), lty=2, lwd=2)
      # abline(v=as.numeric(avg.dawn.dusk[as.character(sub.month$month[1])==as.character(avg.dawn.dusk$Month), "d.Dusk"]), lty=2, lwd=2)
      title(xlab = "Time of Day (24 hr)", ylab = "# of Index Counts", outer=T, line=1, main = paste0("seed = ", seed.number))
    }
  
