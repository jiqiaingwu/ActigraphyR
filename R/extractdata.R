#' Extract Actigraphy Data to csv file
#'
#' @param table_raw import Actigraphy Raw data
#' @param initalize import Initalize Raw data
#' @return The final result.
#' @export


## Step 1. Create cunstomized functions
## Step 2. Import Raw Data and split into multiple pieces.
## Step 3. Start to do the data cleaning, such as format date variables and time variables.
## Step 4. The Raw data can be divided by two parts. One part could be directly extract. Another part is byday summary, which need calculations.
## Step 5. Start to work/import on initalize sheet
## Step 6. Data cleaning for initalize sheet. Finally, I produce a single row data, which will combine with the directly extraction part in Raw data.
## Step 7. Data cleaning for byday summary
## Step 8. Combine all data together

extractdata = function(table_raw,initalize){


  ##The excel sheet contains multiple tables, then I need to find a way to split the tables individually.
  split_table <- table_raw %>%
    split_df(complexity = 2)

  ##Finally, we got 17 seperate tables. We only need 3,5,7,9,10.(Maybe 16&17)
  dt1<-t(data.frame(split_table[3])[,-3])
  colnames(dt1) <- dt1[1,]
  dt1 <- dt1[-1, ]

  dt2<-t(data.frame(split_table[5])[,-3])
  colnames(dt2) <- dt2[1,]
  dt2 <- dt2[-1, ]

  dt3<-t(data.frame(split_table[7])[,-3])
  colnames(dt3) <- dt3[1,]
  dt3 <- dt3[-1, ]

  dt4<-data.frame(split_table[10])
  colnames(dt4) <- data.frame(split_table[9])[1,]

  dt5<-t(data.frame(split_table[1])[,-3])
  colnames(dt5) <- dt5[1,]
  dt5 <- dt5[-1, ]
  dt6 <- str_split(dt5, "_", simplify = TRUE)
  dt6 <- dt6[,-3 ]
  names(dt6) <- c("Participant_ID", "Event Time")

  ##Divded day data and summary data for part 4
  dt4b <- dt4[!(dt4$`Interval Type` %in%  c("Rest Summary","Active Summary","Sleep Summary","Daily Summary","EXCLUDED")) ,]

  #Note: The origin date point are different between R & Excel
  #Convert date variables
  dt4b$`Start Date` <- as.Date(as.numeric(dt4b$`Start Date`), origin = "1899-12-30")
  dt4b$`End Date` <- as.Date(as.numeric(dt4b$`End Date`), origin = "1899-12-30")


  # Work on time variables
  time <- dt4b[,c("End Time","Interval Type")]
  time$time <- round(as.numeric(time$`End Time`)*24,2)
  time0 <- time %>% separate(time, c("hour", "min"))
  #time <- as.data.frame(do.call(rbind, strsplit(as.character(round(as.numeric(dt4b$`End Time`)*24,2)),"\\.")))
  time0$min2 <- round(as.numeric(time0$min)*0.6,0)
  time0$min2[is.na(time0$min2)] <- "00"
  time0$time <- paste0(time0$hour,":",time0$min2)
  time0$time[time0$time == "NaN:00"] <- NA
  dt4b$`End Time` <- time0$time
  dt4b$`End Date`[time$time <=6 & !is.na(time$time)] <- dt4b$`End Date`-1
  dt4b$`End Day` <- weekdays(dt4b$`End Date`)


  time <- dt4b[,c("Start Time","Interval Type")]
  time$time <- round(as.numeric(time$`Start Time`)*24,2)
  time0 <- time %>% separate(time, c("hour", "min"))
  #time <- as.data.frame(do.call(rbind, strsplit(as.character(round(as.numeric(dt4b$`End Time`)*24,2)),"\\.")))
  time0$min2 <- round(as.numeric(time0$min)*0.6,0)
  time0$min2[is.na(time0$min2)] <- "00"
  time0$time <- paste0(time0$hour,":",time0$min2)
  time0$time[time0$time == "NaN:00"] <- NA
  dt4b$`Start Time` <- time0$time
  dt4b$`Start Date`[time$time <=6 & !is.na(time$time)] <- dt4b$`Start Date`[time$time <=6 & !is.na(time$time)]-1
  dt4b$`Start Day` <- weekdays(dt4b$`Start Date`)


  ##Some variables could be directly extract, I plan to save them in final dataset
  dt2 <-as.data.frame(dt2[c(1,6,7,9,12,13,15,16,17,18,19,20,21)])
  rownames(dt2) <- gsub(":", "", rownames(dt2), fixed = T)

  dt3 <- as.data.frame(dt3)
  rownames(dt3) <- gsub(":", "", rownames(dt3), fixed = T)

  #COmbine part 2 and part 3 togther
  finaldata <- data.frame(cbind.fill(t(dt2),t(dt3)))
  rownames(finaldata) <- c()


  split_initalize <- initalize %>%
    split_df(complexity = 2)

  #optimize the first part
  initalize1<-t(data.frame(split_initalize[1])[,-3])
  colnames(initalize1) <- initalize1[1,]
  initalize1 <- as.data.frame(initalize1[-1, ])

  initalize2<-t(data.frame(split_initalize[16])[,-3])
  colnames(initalize2) <- initalize2[1,]
  initalize2 <- as.data.frame(initalize2[-1,-1])

  #Use loop to read day1-day7 part, and then combine them to a list
  listofdfs <- list()
  for(i in 2:15){
    if(ncol(data.frame(split_initalize[i]))>1){
      df<-data.frame(t(data.frame(split_initalize[i]))[,-3])
      #names(df) <- df[1,]
      df <- df[2, c(2,4)]
      listofdfs[[i]] <- df
    }
  }


  #Combine all useful info in initalize sheet
  big_data = do.call(rbind, listofdfs)


  big_data$day_cat <- apply(big_data, 1, function(r) any(r %in% c("Monday","Tuesday","Wednesday","Thursday","Sunday")))
  X4 <- big_data$X4
  big_data$X4 <- as.numeric(X4)
  big_data$number_days<-max(as.numeric(big_data$X4),na.rm=T)
  big_data$number_days
  big_data$weekday <- nrow(big_data[big_data$day_cat == TRUE,])
  big_data$weekday
  big_data$weekendday <- nrow(big_data[big_data$day_cat == FALSE,])
  big_data$weekendday

  big_data1<-big_data[1,4:6]
  big_data1

  big_data2 <- as.data.frame(cbind.fill(t(initalize1),big_data1))
  rownames(big_data2) <- c()
  names(big_data2)

  Date_Scored <- big_data2$`Date Scored`
  big_data2$`Date Scored` <- as.numeric(Date_Scored)
  Date_Study<- big_data2$`Date of Study`
  big_data2$`Date of Study` <- as.numeric(Date_Study)

  big_data2$`Date Scored` <- as.Date(as.numeric(big_data2$`Date Scored`),origin = "1899-12-30")
  big_data2$`Date of Study` <- as.Date(as.numeric(big_data2$`Date of Study`),origin = "1899-12-30")
  big_data2


  newdata <- as.data.frame(cbind.fill(t(dt6),big_data2,finaldata))
  rownames(newdata) <- c()
  newdata<-newdata[,-c(3,12,24,25,27,29)]


  Date_Study<- as.numeric(levels(Date_Study))[Date_Study]
  Date_Study <- as.Date(as.numeric(Date_Study),origin = "1899-12-30")

  initalize2<-as.numeric(unlist(initalize2))
  dt4b$`Interval#` <- as.numeric(dt4b$`Interval#`)
  newdt <- dt4b[dt4b$`Interval#` %in% unique(initalize2), ]


  newdt$day_cat <- "weekday"
  newdt$day_cat[newdt$`Start Day` %in% c("Saturday", "Friday")] <- "weekend"
  #newdt$day_cat[newdt$`Start Day` %in% c("Saturday", "Sunday")] <- "weekend"

  newdt_active <- newdt[newdt$`Interval Type` %in% c("ACTIVE"),]
  newdt_rest <- newdt[newdt$`Interval Type` %in% c("REST"),]
  newdt_sleep <- newdt[newdt$`Interval Type` %in% c("SLEEP"),]
  newdt_daily <- newdt[newdt$`Interval Type` %in% c("DAILY"),]




  newdt$Duration <- as.numeric(newdt$Duration)
  newdt$`Sleep Time` <- as.numeric(newdt$`Sleep Time`)
  newdt$WASO <- as.numeric(newdt$`%Wake`)
  newdt$Fragmentation <- as.numeric(newdt$Fragmentation)
  newdt$Efficiency <- as.numeric(newdt$Efficiency)
  newdt$`Onset Latency` <- as.numeric(newdt$`Onset Latency`)
  newdt$`Off-Wrist` <- as.numeric(newdt$`Off-Wrist`)
  newdt$`Total AC` <- as.numeric(newdt$`Total AC`)
  newdt$`Exposure White` <- as.numeric(newdt$`Exposure White`)
  newdt$`Avg White` <- as.numeric(newdt$`Avg White`)


  newdt$newstarttime <- difftime(strptime(newdt$`Start Time`,"%H:%M"),strptime("00:00","%H:%M"), units="mins")
  newdt$newendtime <- difftime(strptime(newdt$`End Time`,"%H:%M"),strptime("00:00","%H:%M"), units="mins")

  df.means <- aggregate(list(newdt$newstarttime, newdt$newendtime, newdt$Duration, newdt$`Sleep Time`, newdt$WASO, newdt$Fragmentation,
                             newdt$Efficiency, newdt$`Onset Latency`) ,by=list(newdt$day_cat,newdt$`Interval Type`),mean)
  names(df.means) <- c("day_cat","Interval Type","newstarttime","newendtime","Avg_Duration_byday","Avg_Sleep_Time_byday","Avg_WASO_byday",
                       "Avg_Fragmentation_byday","Avg_Efficiency_byday", "Avg_Onset_Latency_byday")
  df.means$avg_starttime_byday <- round((df.means$newstarttime) / 60,2)
  df.means$avg_endtime_byday <- round((df.means$newendtime) / 60,2)
  df.means<-df.means[,-c(3:4),drop=FALSE]

  na.omit=T
  df.means1 <- aggregate(list(newdt$newstarttime, newdt$newendtime, newdt$Duration, newdt$`Sleep Time`, newdt$WASO, newdt$Fragmentation,
                              newdt$Efficiency, newdt$`Onset Latency`,newdt$`Off-Wrist`,newdt$`Total AC`,newdt$`Exposure White`,newdt$`Avg White`)
                         ,by=list(newdt$`Interval Type`),mean)
  names(df.means1) <- c("Interval Type","newstarttime","newendtime","Avg_Duration_overall","Avg_Sleep_Time_overall","Avg_WASO_overall",
                        "Avg_Fragmentation_overall","Avg_Efficiency_overall","Avg_Onset_Latency_overall","Avg_Off_Wrist_overall",
                        "Avg_Total_AC_overall","Avg_Exp_White_overall","Avg_Avg_White_overall")
  df.means1$avg_starttime_overall <- round((df.means1$newstarttime) / 60,2)
  df.means1$avg_endtime_overall <- round((df.means1$newendtime) / 60,2)
  df.means1<-df.means1[,-c(2:3),drop=FALSE]



  #####################Reshape to one row#############
  df.means <- df.means[df.means$`Interval Type` %in% c('DAILY','REST','SLEEP'),]
  df.means1 <- df.means1[df.means1$`Interval Type` %in% c('DAILY','REST','SLEEP'),]

  #Melt weekday and weekend into variables. Number of row will change from 8 to 4 to match what we have for overall.
  df.means3 <- reshape(data=df.means,idvar = "Interval Type", timevar =c("day_cat"), v.names = c("Avg_Duration_byday","Avg_Sleep_Time_byday",
                                                                                                 "Avg_WASO_byday","Avg_Fragmentation_byday",
                                                                                                 "Avg_Efficiency_byday","Avg_Onset_Latency_byday",
                                                                                                 "avg_starttime_byday","avg_endtime_byday"), direction="wide")

  df.means3<-data.frame(t(gsub("mins", "", t(df.means3), fixed = TRUE)))
  names(df.means3) <- gsub("byday.", "", names(df.means3),fixed = TRUE)


  #Create ID variable, then Combine byday and overall data together.
  df.means1$ID <- 1
  names(df.means3)[1] <- "Interval Type"
  df.means4 <- merge(df.means1,df.means3,by="Interval Type")
  # df.means3 <- reshape(data=df.means1,v.names = "Interval Type",timevar = c("Avg_Duration_overall","Avg_Sleep_Time_overall",
  #                                                                                             "Avg_WASO_overall","Avg_Fragmentation_overall",
  #                                                                                             "Avg_Efficiency_overall","Avg_Onset_Latency_overall",
  #                                                                                             "Avg_Off_Wrist_overall","Avg_Total_AC_overall","Avg_Exp_White_overall",
  #                                                                                             "Avg_Avg_White_overall","avg_starttime_overall","avg_endtime_overall"), direction="wide")

  df.means5 <- reshape(data=df.means4,idvar = "ID", timevar =c("Interval Type"), v.names = c("Avg_Duration_overall","Avg_Sleep_Time_overall","Avg_WASO_overall",
                                                                                           "Avg_Fragmentation_overall","Avg_Efficiency_overall","Avg_Onset_Latency_overall","Avg_Off_Wrist_overall",
                                                                                           "Avg_Total_AC_overall","Avg_Exp_White_overall","Avg_Avg_White_overall","avg_starttime_overall",
                                                                                           "avg_endtime_overall","Avg_Duration_weekday","Avg_Sleep_Time_weekday",
                                                                                           "Avg_WASO_weekday","Avg_Fragmentation_weekday","Avg_Efficiency_weekday","Avg_Onset_Latency_weekday",
                                                                                           "avg_starttime_weekday","avg_endtime_weekday","Avg_Duration_weekend","Avg_Sleep_Time_weekend",
                                                                                           "Avg_WASO_weekend","Avg_Fragmentation_weekend","Avg_Efficiency_weekend","Avg_Onset_Latency_weekend",
                                                                                           "avg_starttime_weekend","avg_endtime_weekend"), direction="wide")

  df.means5$Avg_Duration_overall.DAILY<-NA
  df.means5$Avg_Sleep_Time_overall.DAILY<-NA
  df.means5$Avg_WASO_overall.DAILY<-NA
  df.means5$Avg_WASO_overall.REST<-NA
  df.means5$Avg_Fragmentation_overall.DAILY<-NA
  df.means5$Avg_Fragmentation_overall.REST<-NA
  df.means5$Avg_Off_Wrist_overall.SLEEP<-NA
  df.means5$Avg_Off_Wrist_overall.REST<-NA
  df.means5$Avg_Total_AC_overall.SLEEP<-NA
  df.means5$Avg_Total_AC_overall.REST<-NA
  df.means5$Avg_Exp_White_overall.SLEEP<-NA
  df.means5$Avg_Exp_White_overall.REST<-NA
  df.means5$Avg_Avg_White_overall.SLEEP<-NA
  df.means5$Avg_Avg_White_overall.REST<-NA
  df.means5$avg_starttime_overall.DAILY<-NA
  df.means5$avg_endtime_overall.DAILY<-NA


  df.means5$Avg_Duration_weekday.DAILY<-NA
  df.means5$Avg_Duration_weekend.DAILY<-NA
  df.means5$Avg_Sleep_Time_weekday.DAILY<-NA
  df.means5$Avg_Sleep_Time_weekend.DAILY<-NA
  df.means5$Avg_WASO_weekday.DAILY<-NA
  df.means5$Avg_WASO_weekend.DAILY<-NA
  df.means5$Avg_WASO_weekday.REST<-NA
  df.means5$Avg_WASO_weekend.REST<-NA
  df.means5$Avg_Fragmentation_weekday.DAILY<-NA
  df.means5$Avg_Fragmentation_weekend.DAILY<-NA
  df.means5$Avg_Fragmentation_weekday.REST<-NA
  df.means5$Avg_Fragmentation_weekend.REST<-NA
  df.means5$avg_starttime_weekday.DAILY<-NA
  df.means5$avg_starttime_weekend.DAILY<-NA
  df.means5$avg_endtime_weekday.DAILY<-NA
  df.means5$avg_endtime_weekend.DAILY<-NA




  newdt1 <- newdt[newdt$`Interval Type` %in% c('DAILY','REST','SLEEP'), c("Interval Type","Interval#","Start Time","End Time","Duration","Off-Wrist",
                                                                        "Total AC","Onset Latency","Efficiency","WASO",
                                                                        "Sleep Time","Fragmentation","Exposure White","Avg White")]


  newdt2 <- reshape(data=newdt1,idvar = "Interval#", timevar =c("Interval Type"), v.names = c("Start Time","End Time","Duration","Off-Wrist",
                                                                                              "Total AC","Onset Latency","Efficiency","WASO",
                                                                                              "Sleep Time","Fragmentation","Exposure White","Avg White"), direction="wide")
  newdt2$`Interval#`<- paste("D",newdt2$`Interval#`)
  newdt2$`Interval#`<-gsub(" ", "", newdt2$`Interval#`, fixed = TRUE)

  newdt2$ID <- 1

  newdt2t <- t(newdt2)
  newdt3t <-  na.omit(newdt2t)
  newdt3 <- data.frame(t(newdt3t))

  newdt3$Off.Wrist.REST<-NA
  newdt3$Total.AC.REST<-NA
  newdt3$WASO.REST<-NA
  newdt3$Fragmentation.REST<-NA
  newdt3$Exposure.White.REST<-NA
  newdt3$Avg.White.REST<-NA

  newdt3$Off.Wrist.SLEEP<-NA
  newdt3$Total.AC.SLEEP<-NA
  newdt3$Exposure.White.SLEEP<-NA
  newdt3$Avg.White.SLEEP<-NA

  newdt3$Start.Time.DAILY<-NA
  newdt3$End.Time.DAILY<-NA
  newdt3$Duration.DAILY<-NA
  newdt3$WASO.DAILY<-NA
  newdt3$Sleep.Time.DAILY<-NA
  newdt3$Fragmentation.DAILY<-NA

  newdt4 <- reshape(data=newdt3,idvar = "ID", timevar ="Interval.", v.names = c("Duration.REST","Off.Wrist.REST","Total.AC.REST",
                                                                                "WASO.REST","Sleep.Time.REST","Fragmentation.REST","Exposure.White.REST","Avg.White.REST","Start.Time.SLEEP",
                                                                                "End.Time.SLEEP","Duration.SLEEP","Off.Wrist.SLEEP","Total.AC.SLEEP","Onset.Latency.SLEEP","Efficiency.SLEEP",
                                                                                "WASO.SLEEP","Sleep.Time.SLEEP","Fragmentation.SLEEP","Exposure.White.SLEEP","Avg.White.SLEEP","Start.Time.DAILY",
                                                                                "End.Time.DAILY","Duration.DAILY","Off.Wrist.DAILY","Total.AC.DAILY","WASO.DAILY","Sleep.Time.DAILY",
                                                                                "Fragmentation.DAILY","Exposure.White.DAILY","Avg.White.DAILY"), direction="wide")
  df.means6 <- cbind(newdata,df.means5,newdt4)
  df.means6t <- t(df.means6)
  df.means7t <-  na.omit(df.means6t)
  #df.means7 <- t(df.means7t)

  df.means7<-data.frame(t(gsub("mins", "", df.means7t, fixed = TRUE)))
  names(df.means7) <- gsub(".", "_", names(df.means7), fixed = T)

  df.means7 <- subset(df.means7, select = -c(ID, ID_1))
}
