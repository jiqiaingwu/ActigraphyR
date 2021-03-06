% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractdata.R
\name{extractdata}
\alias{extractdata}
\title{Process and analyze the Actigraphy Data to csv file}
\usage{
extractdata(
  table_raw,
  initalize,
  identifier = FALSE,
  rangetype = c("DAILY", "REST", "SLEEP", "ACTIVE"),
  actiware_property = c("Actiwatch Type:", "Logging Mode:", "Time Zone:",
    "Epoch Length:", "Number of Days:", "Actiwatch Serial Number:",
    "Actiwatch Firmware Version:", "Activity Calibration Factor:",
    "White Calibration Factor:", "Red Calibration Factor:", "Green Calibration Factor:",
    "Blue Calibration Factor:"),
  calcols_byday_chosen = c("Start Time", "End Time", "Duration", "Sleep Time", "WASO",
    "Fragmentation", "Efficiency", "Onset Latency", "Avg White", "Avg Green", "Avg Red",
    "Avg Blue"),
  calcols_overall_chosen = c("Start Time", "End Time", "Duration", "Sleep Time",
    "WASO", "Fragmentation", "Efficiency", "Onset Latency", "Avg White", "Avg Green",
    "Avg Red", "Avg Blue"),
  cols_chosen = c("Interval Type", "Interval#", "Start Time", "End Time", "Duration",
    "Off-Wrist", "Total AC", "Onset Latency", "Efficiency", "WASO", "Sleep Time",
    "Fragmentation")
)
}
\arguments{
\item{table_raw}{Import Actigraphy Raw data}

\item{initalize}{Import Initialize Raw data}

\item{identifier}{If False(default), output identifier variables such as Name, Age etc.}

\item{rangetype}{Decide which type of interval will be output. The default values are all four intervals: Active, DAILY, REST, SLEEP.}

\item{actiware_property}{Output useful variables in Actiware Data Property section}

\item{calcols_byday_chosen}{Calculate the mean value of columns by weekday and weekend}

\item{calcols_overall_chosen}{Calculate the overall mean value of columns}

\item{cols_chosen}{Select variables to output for each day. Please note variables Interval Type & Interval# are required in the list}
}
\value{
The final result
}
\description{
Process and analyze the Actigraphy Data to csv file
}
