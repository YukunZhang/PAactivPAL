#' Activity Summary--Standing and Stepping
#'
#' Summarize Standing and Stepping activity measures
#' @param final_dat cleaned final data that is between record getup time and sleep time

#' @return \code{total_number_of_activity_bouts} Total number of activity bouts
#' @return \code{mean_activity_bout_length} Average length of activity bout
#' @return \code{prop_of_activity_time_greater_5min} Proportions of activity greater than 5 minutes
#' @return \code{prop_of_activity_time_greater_10min} Proportions of activity greater than 10 minutes
#' @return \code{prop_of_activity_time_greater_30min} Proportions of activity greater than 30 minutes
#' @return \code{total_activity_time_greater_5min} Total activity time in the bouts greater than 5 minutes
#' @return \code{total_activity_time_greater_10min} Total activity time in the bouts greater than 10 minutes
#' @return \code{total_activity_time_greater_30min} Total activity time in the bouts greater than 30 minutes
#' @return \code{percentile_activity_time_5} 5\% percentile of activity bouth length
#' @return \code{percentile_activity_time_25} 25\% percentile of activity bouth length
#' @return \code{percentile_activity_time_50} 50\% percentile of activity bouth length
#' @return \code{percentile_activity_time_75} 75\% percentile of activity bouth length
#' @return \code{percentile_activity_time_95} 95\% percentile of activity bouth length
#' @return \code{alpha_activity} alpha of activity time, see details_
#' @return \code{stepping_to_standing_ratio} Ratio of stepping to standing, the ratio of total stepping hours to standing hours
#' @return \code{table} A table that combine all outputs
#' @details  \code{alpha_activity} is defined by \code{1+1/M}, where \code{M} is the average of \code{log(activity bout length /minimum activity bout length)}_

#' @examples  data(sampledata);Activity_summary(sampledata)
#' @importFrom stats quantile
#' @export
#'

Activity_summary=function(final_dat){
  #library(reldist)
  dat=final_dat

  if(is.numeric(dat$Time)==F) ####if  is.numeric(dat$Time)==F, we need further modification of time in next step
  {
    ee<-as.character(dat$Time)
    max_length<-max(nchar(ee))
    ee[nchar(ee)!=max_length]<-"#1899-12-30 00:00:00#"
    #### use character type, may not be good
    ee_new<- (as.numeric( as.POSIXlt( substr(ee, 2, max_length-1 )  )    )+2209190400)/24/60/60
    #### use interval type, this is the best
    start_ee<-  min(which(dat[,2]>0))-1
    if (start_ee>1)
    {
      ee_new_int_type <-c( ee_new[1:((start_ee)-1)],ee_new[start_ee]+(dat$DataCount[start_ee:nrow(dat)]/10/24/60/60)  )
    }else
    {    ee_new_int_type <-ee_new[start_ee]+(dat$DataCount[start_ee:nrow(dat)]/10/24/60/60)
    } #### if interval type has large difference with character type, use character type
    int_dif_char<-which(abs(ee_new-ee_new_int_type)>0.1 )
    ee_new_int_type[int_dif_char]<-ee_new[int_dif_char]
    ####
    dat<-cbind(ee_new_int_type,dat[,2:6])
  }
  final_dat<-dat[,c(1,3,4,6)]
  colnames(final_dat)<-c("date_time","Interval","ActivityCode", "METs")
  final_dat=final_dat[final_dat$date_time!=0.625,] #delete rows with time as #1899-12-30 00:00:00#

temp_mat=final_dat


  temp_mat_for_activity<- temp_mat
  temp_mat_for_activity$Activity[temp_mat_for_activity$Activity==1]<-2
  end_pos<-cumsum(rle(temp_mat_for_activity$Activity)$lengths)
  start_pos<-c(0,end_pos[1:(length(end_pos)-1)])+1
  ############### for each runs, handle the data
  handle_runs<- sapply(1:length(end_pos),function(x,data_mat=temp_mat_for_activity)
  {
    select_data<-data_mat[start_pos[x]:end_pos[x],]
    combine_data<- c(min(select_data$date_time),sum(select_data$Interval),max(select_data$Activity),sum(select_data$METs) )
    return(combine_data)
  }, simplify=F
  )
  ############### combine each run
  combined_temp_mat_for_activity<-data.frame(do.call(rbind,handle_runs))
  colnames(combined_temp_mat_for_activity)<-c("date_time", "Interval", "ActivityCode","METs")
  ###############
  ###############Calculation
  ###############
  ###
  temp_activity<-subset(combined_temp_mat_for_activity,combined_temp_mat_for_activity$ActivityCode==2)$Interval
  ###

  length_temp_activity<-length(temp_activity)
  total_number_of_activity_bouts<- length_temp_activity
  mean_activity_bout_length<- mean(temp_activity) /60/60

  prop_of_activity_time_greater_5min<- 100*length(temp_activity[temp_activity>5*60])/length_temp_activity
  prop_of_activity_time_greater_10min<- 100*length(temp_activity[temp_activity>10*60])/length_temp_activity
  prop_of_activity_time_greater_30min<- 100*length(temp_activity[temp_activity>30*60])/length_temp_activity

  total_activity_time_greater_5min<- sum(temp_activity[temp_activity>5*60])/60/60
  total_activity_time_greater_10min<- sum(temp_activity[temp_activity>10*60])/60/60
  total_activity_time_greater_30min<- sum(temp_activity[temp_activity>30*60])/60/60

  quantile_activity_temp<-quantile(temp_activity, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile_activity_time_5<- quantile_activity_temp[1]
  percentile_activity_time_25<- quantile_activity_temp[2]
  percentile_activity_time_50<- quantile_activity_temp[3]
  percentile_activity_time_75<- quantile_activity_temp[4]
  percentile_activity_time_95<- quantile_activity_temp[5]

  alpha_activity<- 1+ 1/mean(log(temp_activity/ min(temp_activity)))
  #gini_index_activity<- gini(temp_activity)
  step_hour<- sum(subset(temp_mat,temp_mat$ActivityCode==2)$Interval) /60/60
  stand_hour<- sum(subset(temp_mat,temp_mat$ActivityCode==1)$Interval) /60/60

  stepping_to_standing_ratio<- step_hour/stand_hour

  table<- cbind(total_number_of_activity_bouts,mean_activity_bout_length,prop_of_activity_time_greater_5min,prop_of_activity_time_greater_10min,prop_of_activity_time_greater_30min,total_activity_time_greater_5min,total_activity_time_greater_10min,total_activity_time_greater_30min,percentile_activity_time_5,percentile_activity_time_25,percentile_activity_time_50,percentile_activity_time_75,percentile_activity_time_95,alpha_activity,stepping_to_standing_ratio)
  colnames(table)<- c("total_number_of_activity_bouts","mean_activity_bout_length","prop_of_activity_time_greater_5min","prop_of_activity_time_greater_10min","prop_of_activity_time_greater_30min","total_activity_time_greater_5min","total_activity_time_greater_10min","total_activity_time_greater_30min","percentile_activity_time_5","percentile_activity_time_25","percentile_activity_time_50","percentile_activity_time_75","percentile_activity_time_95","alpha_activity","stepping_to_standing_ratio")
  out=list( total_number_of_activity_bouts=total_number_of_activity_bouts,mean_activity_bout_length=mean_activity_bout_length,prop_of_activity_time_greater_5min=prop_of_activity_time_greater_5min,prop_of_activity_time_greater_10min=prop_of_activity_time_greater_10min,prop_of_activity_time_greater_30min=prop_of_activity_time_greater_30min,total_activity_time_greater_5min=total_activity_time_greater_5min,total_activity_time_greater_10min=total_activity_time_greater_10min,total_activity_time_greater_30min=total_activity_time_greater_30min,percentile_activity_time_5=percentile_activity_time_5,percentile_activity_time_25=percentile_activity_time_25,percentile_activity_time_50=percentile_activity_time_50,percentile_activity_time_75=percentile_activity_time_75,percentile_activity_time_95=percentile_activity_time_95,alpha_activity=alpha_activity,stepping_to_standing_ratio=stepping_to_standing_ratio,table=table)
  return(out)
}
