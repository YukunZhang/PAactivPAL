#' Physical Activity Summary--Sedentary Proportions and Percentiles
#'
#' Summarize activity measures using proportions and percentiles
#' @param final_dat cleaned final data that is between record getup time and sleep time

#' @return \code{total_sed_time} Total sedantary hour, same as PA_summary function output sed_hour
#' @return \code{total_number_of_sed_bouts} Total number of sedentary bouts_ It counts the number of sedentary recordings_
#' @return \code{mean_sed_bout_length} Mean sedentary bout length_ It is the average of the duration time for all sedentary activities
#' @return \code{prop_of_sed_time_greater_20min} Proportions of sedentary bout greater than 20 minutes
#' @return \code{prop_of_sed_time_greater_60min} Proportions of sedentary bout greater than 60 minutes
#' @return \code{prop_of_sed_time_greater_120min} Proportions of sedentary bout greater than 120 minutes
#' @return \code{total_sed_time_greater_20min} Total sedentary time greater than 20 minutes
#' @return \code{total_sed_time_greater_60min} Total sedentary time greater than 60 minutes
#' @return \code{total_sed_time_greater_120min} Total sedentary time greater than 120 minutes
#' @return \code{percentile_sed_time_5}  5\% Percentile of sedentary time
#' @return \code{percentile_sed_time_25} 25\% Percentile of sedentary time
#' @return \code{percentile_sed_time_50} 50\% Percentile of sedentary time
#' @return \code{percentile_sed_time_75} 75\% Percentile of sedentary time
#' @return \code{percentile_sed_time_95} 95\% Percentile of sedentary time
#' @return \code{alpha_sed} alpha of sendataty time, see details_
#' @return \code{prop_sed_time_6_12} Proportions of sedentary time between 6:00-12:00
#' @return \code{prop_sed_time_12_18} Proportions of sedentary time between 12:00-18:00
#' @return \code{prop_sed_time_18_22} Proportions of sedentary time between 18:00-22:00
#' @details Proportions of sedentary bout greater than 20/60/120 minutes is the ratio of the number of sedentary bouts greater than 20/60/120 minutes to the total number of sedentary recordings_
#' @details Total sedentary time greater than 20/60/120 minutes is the summation of the sedentary durations which are greater than 20/60/120 minutes_
#' @details To calculate 5\%/25\%/___/95\% percentile of sedentary time, all of the recorded sedentary durations are listed and R function \emph{quantile} is used to find the percentiles_
#' @details  alpha_sed is defined by \code{1+1/M}, where \code{M} is the average of \code{log(sedentary bout length /minimum sedentary bout length)}_
#' @details Proportions of sedentary time between 6:00-12:00/12:00-18:00/18:00-22:00 is the ratio of the sedentary durations to the total activity durations between 6:00-12:00/12:00-18:00/18:00-22:00_


#' @examples  data(sampledata);sed_summary(sampledata)
#' @importFrom stats quantile
#' @export

sed_summary=function(final_dat)

{#library(reldist)

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
  hour_char<-as.numeric(format(as.POSIXlt(temp_mat$date_time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0)),"%H"))
  temp_sed<-subset(temp_mat,temp_mat$ActivityCode==0)$Interval
  sed_hour<- sum(temp_sed) /60/60
  length_temp_sed<-length(temp_sed)
  num_changes_from_sed_to_non_sed<- length_temp_sed
  total_sed_time<-sed_hour
  total_number_of_sed_bouts<-num_changes_from_sed_to_non_sed
  mean_sed_bout_length<- mean(temp_sed) /60/60
  prop_of_sed_time_greater_20min<- 100*length(temp_sed[temp_sed>20*60])/length_temp_sed
  prop_of_sed_time_greater_60min<- 100*length(temp_sed[temp_sed>60*60])/length_temp_sed
  prop_of_sed_time_greater_120min<- 100*length(temp_sed[temp_sed>120*60])/length_temp_sed

  total_sed_time_greater_20min<- sum(temp_sed[temp_sed>20*60])/60/60
  total_sed_time_greater_60min<- sum(temp_sed[temp_sed>60*60])/60/60
  total_sed_time_greater_120min<- sum(temp_sed[temp_sed>120*60])/60/60

  quantile_temp<-quantile(temp_sed, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile_sed_time_5<- quantile_temp[1]
  percentile_sed_time_25<- quantile_temp[2]
  percentile_sed_time_50<- quantile_temp[3]
  percentile_sed_time_75<- quantile_temp[4]
  percentile_sed_time_95<- quantile_temp[5]

  alpha_sed<- 1+ 1/mean(log(temp_sed/ min(temp_sed)))
  #gini_index_sed<- gini(temp_sed)

  prop_sed_time_6_12<- 100*sum(subset(temp_mat,temp_mat$ActivityCode==0 & hour_char>=6 & hour_char<12)$Interval) /(sum(subset(temp_mat,hour_char>=6 & hour_char<12)$Interval)+0.0001)   ###prevent this value is zero
  prop_sed_time_12_18<- 100*sum(subset(temp_mat,temp_mat$ActivityCode==0 & hour_char>=12 & hour_char<18)$Interval) /(sum(subset(temp_mat,hour_char>=12 & hour_char<18)$Interval)+0.0001)
  prop_sed_time_18_22<- 100*sum(subset(temp_mat,temp_mat$ActivityCode==0 & hour_char>=18 & hour_char<22)$Interval) /(sum(subset(temp_mat,hour_char>=18 & hour_char<22)$Interval)+0.0001)
  table<-cbind(total_sed_time,total_number_of_sed_bouts,mean_sed_bout_length,prop_of_sed_time_greater_20min,prop_of_sed_time_greater_60min,prop_of_sed_time_greater_120min,total_sed_time_greater_20min,total_sed_time_greater_60min,total_sed_time_greater_120min,percentile_sed_time_5,percentile_sed_time_25,percentile_sed_time_50,percentile_sed_time_75,percentile_sed_time_95,alpha_sed,prop_sed_time_6_12,prop_sed_time_12_18,prop_sed_time_18_22)
 colnames(table)<-c("total_sed_time","total_number_of_sed_bouts","mean_sed_bout_length" ,"prop_of_sed_time_greater_20min","prop_of_sed_time_greater_60min","prop_of_sed_time_greater_120min","total_sed_time_greater_20min","total_sed_time_greater_60min","total_sed_time_greater_120min","percentile_sed_time_5","percentile_sed_time_25","percentile_sed_time_50","percentile_sed_time_75","percentile_sed_time_95","alpha_sed","prop_sed_time_6_12","prop_sed_time_12_18","prop_sed_time_18_22")

  out=list( total_sed_time=total_sed_time,total_number_of_sed_bouts=total_number_of_sed_bouts,mean_sed_bout_length=mean_sed_bout_length,prop_of_sed_time_greater_20min=prop_of_sed_time_greater_20min,prop_of_sed_time_greater_60min=prop_of_sed_time_greater_60min, prop_of_sed_time_greater_120min= prop_of_sed_time_greater_120min,total_sed_time_greater_20min=total_sed_time_greater_20min,total_sed_time_greater_60min=total_sed_time_greater_60min,total_sed_time_greater_120min=total_sed_time_greater_120min,percentile_sed_time_5=percentile_sed_time_5,percentile_sed_time_25=percentile_sed_time_25,percentile_sed_time_50=percentile_sed_time_50,percentile_sed_time_75=percentile_sed_time_75,percentile_sed_time_95=percentile_sed_time_95,alpha_sed=alpha_sed,prop_sed_time_6_12=prop_sed_time_6_12,prop_sed_time_12_18=prop_sed_time_12_18,prop_sed_time_18_22=prop_sed_time_18_22,table=table)
  return(out)
}
