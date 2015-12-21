#' Physical Activity Summary--Activity Proportions
#'
#' Summarize sedantary, standing, and stepping activity measures
#' @param final_dat cleaned final data that is between record getup time and sleep time
#' @return \code{perc_sedentary} Proportion of sedentary
#' @return \code{perc_stand} Proportion of standing
#' @return \code{perc_step} Proportion of stepping
#' @return \code{step_per_day}  Number of steps during the given period of time
#' @return \code{break_per_day} Number of interrupting sedentary behavior during the given period of time
#' @return \code{break_rate} Rate of interrupting sedentary behavior
#' @return \code{MET_hour} METs hours
#' @examples  data(sampledata);ActProp_summary(sampledata)
#' @details  Proportion of sedentary/standing/stepping is the ratio of the total durations of sedentary/standing/step to the total worn time_
#' @details  For number of steps, the number of stepping records multiplied by 2_ This is because every two steps lead to one row's record in the event files_
#' @details Number of interrupting sedentary behavior is approximated by the number of  sedentary bouts_
#' @details Rate of interrupting sedentary behavior is the ratio of the daily number of interrupting sedentary behavior to total sedentary hours_
#' @details	 METs hours is the summation of METs hours from all activity records_

#'@export
#'

ActProp_summary=function (final_dat) {
 out=PA_summary(final_dat)
  sed_hour=out$sed_hour
  stand_hour=out$stand_hour
  step_hour=out$step_hour
  hours_worn_total=out$hours_worn_total
  step_count_total=out$step_count_total
  num_changes_from_sed_to_non_sed=out$num_changes_from_sed_to_non_sed
  MET_hours=out$MET_hours
perc_sedentary<- 100*sed_hour/hours_worn_total
perc_stand<- 100*stand_hour/hours_worn_total
perc_step<- 100*step_hour/hours_worn_total
step_per_day<-step_count_total
break_per_day<-num_changes_from_sed_to_non_sed
break_rate<-break_per_day/sed_hour
MET_hour<- MET_hours

table<- cbind(perc_sedentary,perc_stand,perc_step,step_per_day,break_per_day,break_rate,MET_hour)
colnames(table)<- c("perc_sedentary","perc_stand","perc_step","step_per_day","break_per_day","break_rate","MET_hour")
out=list(  perc_sedentary=perc_sedentary,perc_stand=perc_stand,perc_step=perc_step,step_per_day=step_per_day,break_per_day=break_per_day,break_rate=break_rate,MET_hour=MET_hour,table=table)
return(out)
}
