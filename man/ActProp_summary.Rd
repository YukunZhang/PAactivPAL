% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ActProp_summary.R
\name{ActProp_summary}
\alias{ActProp_summary}
\title{Physical Activity Summary--Activity Proportions}
\usage{
ActProp_summary(final_dat)
}
\arguments{
\item{final_dat}{cleaned final data that is between record getup time and sleep time}
}
\value{
\code{perc_sedentary} Proportion of sedentary

\code{perc_stand} Proportion of standing

\code{perc_step} Proportion of stepping

\code{step_per_day}  Number of steps during the given period of time

\code{break_per_day} Number of interrupting sedentary behavior during the given period of time

\code{break_rate} Rate of interrupting sedentary behavior

\code{MET_hour} METs hours
}
\description{
Summarize sedantary, standing, and stepping activity measures
}
\details{
Proportion of sedentary/standing/stepping is the ratio of the total durations of sedentary/standing/step to the total worn time_

For number of steps, the number of stepping records multiplied by 2_ This is because every two steps lead to one row's record in the event files_

Number of interrupting sedentary behavior is approximated by the number of  sedentary bouts_

Rate of interrupting sedentary behavior is the ratio of the daily number of interrupting sedentary behavior to total sedentary hours_

METs hours is the summation of METs hours from all activity records_
}
\examples{
 data(sampledata);ActProp_summary(sampledata)
}

