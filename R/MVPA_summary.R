#' Activity Summary--MVPA
#'
#' Summarize activity measures using MVPA
#' @param final_dat cleaned final data that is between record getup time and sleep time

#' @return \code{Total_light_time} Total light time: the total activity minutes minus the total MVPA hours
#' @return \code{Total_MVPA_time}  Total MVPA hours: the summation of the total Long Bout MVPA and total Sporadic MVPA durations
#' @return \code{Total_MVPA_Long_Bout_time} Total MVPA long bout time: the summation of the total Long Bout MVPA durations
#' @return \code{Total_mvpa_sporadic_time} Total MVPA sporadic time: the summation of the total Sporadic MVPA durations
#' @return \code{Total_Number_of_MVPA_Long_Bouts_and_Sporadic} Total Number of MVPA (Long Bouts or Sporadic): count the number of MVPA from 15 second data
#' @return \code{Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2} Proportion of MVPA (Long Bouts or Sporadic) greater than 2 minutes
#' @return \code{Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5} Proportion of MVPA (Long Bouts or Sporadic) greater than 5 minutes
#' @return \code{Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10} Proportion of MVPA (Long Bouts or Sporadic) greater than 10 minutes
#' @return \code{Total_Number_of_MVPA_Long_Bouts} Total number of long bout MVPA: count the number of Long Bouts MVPA from 10 minute data
#' @return \code{Mean_MVPA_Long_Bout_Length} Mean of long bout MVPA length
#' @return \code{Proportion_of_MVPA_Long_Bouts_greater_10} Proportion of Long Bout MVPA greater than 10 minutes
#' @return \code{Proportion_of_MVPA_Long_Bouts_greater_20} Proportion of Long Bout MVPA greater than 20 minutes
#' @return \code{Highest_MET_value_15s} Highest METs values in 15 second
#' @return \code{Highest_MET_value_10min} Highest METs values in 10 minutes
#' @return \code{Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa} Total METs hours from Long Bout MVPA and sporadic MVPA: the summation of METs hours from all MVPA records
#' @return \code{Total_MET_hrs_Long_Bouts} Total METs hours from Long Bout MVPA: the summation of METs hours from Long Bout MVPA records
#' @details MVPA is defined into two types: Long Bout MVPA and Sporadic MVPA_ Long Bout MVPA is defined as 10 consecutive minutes with METs>=3 (allowing 2 min below that threshold). Sporadic MVPA is defined as activities at any time with METS>=3 and they are not in Long Bout MVPA.
#' @details Highest METs values in 15 second/10 minutes are calculated by picking up the maximum METs values from the combined data with 15 second intervals and the data with 10 minutes intervals, respectively.
#' @examples data(sampledata);MVPA_summary(sampledata)
#' @export

MVPA_summary=function(final_dat)
{
  mvpa_sporadic=NULL
  mvpa=NULL
  is_interval_valid=NULL


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
mvpa_sporadic_interval<- 1/4          ### 1 means 1 minute; 0_5 means 30 seconds; 1/6 means 10 seconds


mvpa_1min_mat<-temp_mat
start_time<-mvpa_1min_mat$date_time[1]
end_time<-  mvpa_1min_mat$date_time[nrow(mvpa_1min_mat)]
num_1min_interval<- trunc( (end_time-start_time)*24*60*60/(60*mvpa_sporadic_interval) )   ###point 1
#(end_time-start_time) != sum(temp_mat$Interval)/60/60/24 if there is take off
interval_length<-mvpa_sporadic_interval*60/60/60/24
interval_1min_start<-start_time+interval_length*(1:(num_1min_interval))
##################
mvpa_record_start_time<-c(start_time, interval_1min_start)
mvpa_record_end_time<-c(interval_1min_start,end_time)
################## if there is take off, they won't be in combine_original_pseudo_mat
combine_original_pseudo_mat<-do.call(rbind,sapply(1: length(mvpa_record_start_time),function(ll){
  temp_mat<-subset(mvpa_1min_mat,mvpa_1min_mat[,1]+mvpa_1min_mat[,2]/24/60/60>mvpa_record_start_time[ll] & mvpa_1min_mat[,1]<mvpa_record_end_time[ll] )
  if(nrow(temp_mat)==0) return (NULL)
  if(temp_mat[nrow(temp_mat),1]+(temp_mat[nrow(temp_mat),2]/24/60/60)> mvpa_record_end_time[ll])  ###if this activity is the last one and it surpass the take off log time
  {
    temp_mat[nrow(temp_mat),4]<-temp_mat[nrow(temp_mat),4]*(mvpa_record_end_time[ll]-temp_mat[nrow(temp_mat),1])/(temp_mat[nrow(temp_mat),2]/24/60/60)
    temp_mat[nrow(temp_mat),2]<- (mvpa_record_end_time[ll]-temp_mat[nrow(temp_mat),1])*24*60*60
  }
  if(temp_mat[1,1]<mvpa_record_start_time[ll])   ###if this activity is the first one and itis earlier than the take on log time
  {
    temp_mat[1,4]<-temp_mat[1,4]* (temp_mat[1,2]-(mvpa_record_start_time[ll]- temp_mat[1,1])*24*60*60)/temp_mat[1,2]
    temp_mat[1,2]<-temp_mat[1,2]-(mvpa_record_start_time[ll]- temp_mat[1,1])*24*60*60
    temp_mat[1,1]<-mvpa_record_start_time[ll]
  }

  return(cbind(temp_mat,ll))
}, simplify = F)  )

colnames(combine_original_pseudo_mat)<-c("date_time","Interval","ActivityCode", "METs","one_minute_interval")
############################################################
############################################################ step2 summary 1 min intervals
############################################################
one_minute_collection<-by(combine_original_pseudo_mat,combine_original_pseudo_mat$one_minute_interval,function(s)c(min(s$date_time),sum(s$METs)*(60/mvpa_sporadic_interval),unique(s$one_minute_interval),sum(s$Interval)  )) ###point 3
one_minute_mat<-do.call(rbind,one_minute_collection)
one_minute_mat<-subset(one_minute_mat,one_minute_mat[,3]!=0 & one_minute_mat[,4]>(60*mvpa_sporadic_interval*0.9) & one_minute_mat[,4]<(60*mvpa_sporadic_interval*1.1)     )  ### one_minute_mat[,4] is the true length, it may not be exactly 30 second, can have a few seconds bias
#########################
if(trunc(nrow(one_minute_mat)/(10/mvpa_sporadic_interval))==0)  {
  ten_minute_vec<-rep(1,nrow(one_minute_mat))
  if(nrow(one_minute_mat)==1) ten_minute_mat<- data.frame(t(c(one_minute_mat[1:length(ten_minute_vec),],ten_minute_vec)))
  if(nrow(one_minute_mat)>1) ten_minute_mat<- data.frame(cbind(one_minute_mat[1:length(ten_minute_vec),],ten_minute_vec))
} else
{
  ten_minute_vec<-rep(1:trunc(nrow(one_minute_mat)/(10/mvpa_sporadic_interval)),each=  (10/mvpa_sporadic_interval)    )    ##### 30s to 10 min ###point 4
  ten_minute_mat<- data.frame(cbind(one_minute_mat[1:length(ten_minute_vec),],ten_minute_vec))
}

colnames(ten_minute_mat)<-c("date_time","mets","one_minute_interval","interval_length","ten_minute_interval")
############################################################
############################################################ step3 summary 10 min intervals
############################################################
#### if in 10 minutes, 8 minutes have METs>3, it is MVPA bout; if less than 8minutes, they are counted as mvpa sporadic_
is_mvpa<-function(s) if(s>=(8/mvpa_sporadic_interval)    ) return(1) else return(0)   ###point 5  ### this is for MVPA long bout
ten_minute_collection<- data.frame(do.call(rbind,by(ten_minute_mat,ten_minute_mat$ten_minute_interval,function(s)c(min(s$date_time), is_mvpa(length(which(s$mets>=3))), mean(s$mets),length(which(s$mets>=3)), abs(max(s$date_time)-min(s$date_time)-sum(s$interval_length[1:(length(s$interval_length)-1)])/24/60/60 )   ))))
colnames(ten_minute_collection)<-c("date_time","mvpa","mets","mvpa_sporadic","is_interval_valid")  #### is_interval_valid is to avoid the wear off during the day problem
ten_minute_collection<-subset(ten_minute_collection,is_interval_valid<0.003) ###if the interval has 5 minutes take off, we do not take it

############################################################
############################################################ step4 MVPA information
############################################################
#### total time
Total_MVPA_Long_Bout_time<-nrow(subset(ten_minute_collection,mvpa==1))/6  ###by hours
Total_mvpa_sporadic_time<-sum(subset(ten_minute_collection,mvpa_sporadic>0 & mvpa!=1)$mvpa_sporadic )/(60/mvpa_sporadic_interval) ###by hours ###point 6
Total_MVPA_time<- Total_MVPA_Long_Bout_time+Total_mvpa_sporadic_time
Total_light_time<- sum(temp_activity) /60/60-Total_MVPA_time
#### Long Bouts+Sporadic_time runs
Long_Bouts_and_Sporadic_run<- rle(ifelse( one_minute_mat[,2]>=3,1,0))
Total_Number_of_MVPA_Long_Bouts_and_Sporadic<-  length(which(Long_Bouts_and_Sporadic_run$values==1))

run_for_Long_Bouts_and_Sporadic_mvpa<- Long_Bouts_and_Sporadic_run$lengths[which(Long_Bouts_and_Sporadic_run$values==1)]/ (60/mvpa_sporadic_interval) ###by hours ###point 7

if(Total_MVPA_time==0)  Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2<-0 else  Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2<-100*length(run_for_Long_Bouts_and_Sporadic_mvpa[run_for_Long_Bouts_and_Sporadic_mvpa>1/30])/Total_Number_of_MVPA_Long_Bouts_and_Sporadic
if(Total_MVPA_time==0)  Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5<-0 else  Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5<-100*length(run_for_Long_Bouts_and_Sporadic_mvpa[run_for_Long_Bouts_and_Sporadic_mvpa>1/12])/Total_Number_of_MVPA_Long_Bouts_and_Sporadic
if(Total_MVPA_time==0)  Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10<-0 else Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10<-100*length(run_for_Long_Bouts_and_Sporadic_mvpa[run_for_Long_Bouts_and_Sporadic_mvpa>1/6])/Total_Number_of_MVPA_Long_Bouts_and_Sporadic


#### Long Bouts Only runs
run_for_mvpa<-rle(ten_minute_collection$mvpa)

Total_Number_of_MVPA_Long_Bouts<-  length(which(run_for_mvpa$values==1))
temp_mvpa_long_bout<-run_for_mvpa$lengths[which(run_for_mvpa$values==1)]/6 ###by hours
if(Total_Number_of_MVPA_Long_Bouts==0) Mean_MVPA_Long_Bout_Length<-0 else Mean_MVPA_Long_Bout_Length<-mean(temp_mvpa_long_bout)
####
if(Total_Number_of_MVPA_Long_Bouts==0)  Proportion_of_MVPA_Long_Bouts_greater_10<-0 else  Proportion_of_MVPA_Long_Bouts_greater_10<-100*length(temp_mvpa_long_bout[temp_mvpa_long_bout>1/6])/Total_Number_of_MVPA_Long_Bouts
if(Total_Number_of_MVPA_Long_Bouts==0)  Proportion_of_MVPA_Long_Bouts_greater_20<-0 else  Proportion_of_MVPA_Long_Bouts_greater_20<-100*length(temp_mvpa_long_bout[temp_mvpa_long_bout>2/6])/Total_Number_of_MVPA_Long_Bouts
#### percentile is not meaningful, always too short
####if(Total_Number_of_MVPA_Long_Bouts==0)
#### {
####percentile_MVPA_Long_Bouts_time_5<- 0
####percentile_MVPA_Long_Bouts_time_25<- 0
####percentile_MVPA_Long_Bouts_time_50<- 0
####percentile_MVPA_Long_Bouts_time_75<- 0
####percentile_MVPA_Long_Bouts_time_95<- 0
#### } else {
####  Percentiles_of_MVPA_Long_Bout_Length_quantile<- quantile(temp_mvpa_long_bout, probs = c(0_05,0_25,0_5,0_75,0_95))
####percentile_MVPA_Long_Bouts_time_5<- Percentiles_of_MVPA_Long_Bout_Length_quantile[1]
####percentile_MVPA_Long_Bouts_time_25<- Percentiles_of_MVPA_Long_Bout_Length_quantile[2]
####percentile_MVPA_Long_Bouts_time_50<- Percentiles_of_MVPA_Long_Bout_Length_quantile[3]
####percentile_MVPA_Long_Bouts_time_75<- Percentiles_of_MVPA_Long_Bout_Length_quantile[4]
####percentile_MVPA_Long_Bouts_time_95<- Percentiles_of_MVPA_Long_Bout_Length_quantile[5]
####        }


####################################################
#################################################### MET_value
####################################################
Highest_MET_value_15s<- max(one_minute_mat[,2])
Highest_MET_value_10min<- max(ten_minute_collection[,3])
################################################ MET_value from MVPA
Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa<- sum((one_minute_mat[,2]/60/60*one_minute_mat[,4])[one_minute_mat[,2]>=3])
Total_MET_hrs_Long_Bouts<- sum((subset(ten_minute_collection,mvpa==1))$mets/60*10)


table<- cbind(Total_light_time,Total_MVPA_time, Total_MVPA_Long_Bout_time,Total_mvpa_sporadic_time,Total_Number_of_MVPA_Long_Bouts_and_Sporadic,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10, Total_Number_of_MVPA_Long_Bouts,Mean_MVPA_Long_Bout_Length, Proportion_of_MVPA_Long_Bouts_greater_10,Proportion_of_MVPA_Long_Bouts_greater_20, Highest_MET_value_15s,Highest_MET_value_10min, Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa,Total_MET_hrs_Long_Bouts)
colnames(table)<- c("Total_light_time","Total_MVPA_time","Total_MVPA_Long_Bout_time","Total_mvpa_sporadic_time","Total_Number_of_MVPA_Long_Bouts_and_Sporadic","Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2","Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5","Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10","Total_Number_of_MVPA_Long_Bouts","Mean_MVPA_Long_Bout_Length","Proportion_of_MVPA_Long_Bouts_greater_10","Proportion_of_MVPA_Long_Bouts_greater_20","Highest_MET_value_15s","Highest_MET_value_10min","Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa","Total_MET_hrs_Long_Bouts")
out=list( Total_light_time=Total_light_time,Total_MVPA_time=Total_MVPA_time, Total_MVPA_Long_Bout_time=Total_MVPA_Long_Bout_time,Total_mvpa_sporadic_time=Total_mvpa_sporadic_time,Total_Number_of_MVPA_Long_Bouts_and_Sporadic=Total_Number_of_MVPA_Long_Bouts_and_Sporadic,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2=Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_2,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5=Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_5,Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10=Proportion_of_MVPA_Long_Bouts_and_Sporadic_greater_10, Total_Number_of_MVPA_Long_Bouts=Total_Number_of_MVPA_Long_Bouts,Mean_MVPA_Long_Bout_Length=Mean_MVPA_Long_Bout_Length, Proportion_of_MVPA_Long_Bouts_greater_10=Proportion_of_MVPA_Long_Bouts_greater_10,Proportion_of_MVPA_Long_Bouts_greater_20=Proportion_of_MVPA_Long_Bouts_greater_20, Highest_MET_value_15s= Highest_MET_value_15s,Highest_MET_value_10min=Highest_MET_value_10min, Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa= Total_MET_hrs_Long_Bouts_and_Sporadic_mvpa,Total_MET_hrs_Long_Bouts=Total_MET_hrs_Long_Bouts,table=table)
return(out)

}
