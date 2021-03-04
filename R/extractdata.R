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

extractdata = function(table_raw,initalize,identifier=FALSE,dailydata=TRUE,rangetype=c()){


  ##The excel sheet contains multiple tables, then I need to find a way to split the tables individually.
  split_table <- table_raw %>%
    split_df(complexity = 2)

  ##The spreadsheet contains 17 separate tables. Table 2,4,6,8 are just section titles. The tables between 11-17 won't be used right now(May revise in future version).
  ##Here is the type of each table:
  ##1.title(Keep)
  ##2.section title(omit)
  ##3.data(identifier, depend on requirement)
  ##4.section title(omit)
  ##5.data(keep)
  ##6.section title(omit)
  ##7.data(keep)
  ##8.section title(omit)
  ##9.variable name of table 10(keep)
  ##10.data(keep)
  ##11-17. (omit)

  #----------------------------------------------------------------------------

  ##In table 1, analysis name is the only useful information.
  table1<-t(as.data.frame(split_table[1])[3,])
  table1 <- table1[-1, ]
  table1a <- str_split(table1, "_", simplify = TRUE)
  #dt6 <- as.data.frame(t(dt6[,-3 ]))
  table1a <- table1a[,-3]
  names(table1a) <- c("Participant_ID", "Event Time")

  ##In table 3, all variables are identifiers. The default setting is omitting.
  if(identifier=TRUE){
    table3<-t(data.frame(split_table[3])[,-3]) #delete the units
  } else {
    table3<-c()
  }

  ##Output Table 5
  table5<-t(data.frame(split_table[5])[,-3]) #delete the units

  ##Output Table 7
  table7<-t(data.frame(split_table[7])[,-3]) #delete the units

  ##Output Table 10, table 9 should be the variable name of table 10. Need to combine them together.
  table10<-data.frame(split_table[10])
  colnames(table10) <- data.frame(split_table[9])[1,]

  #----------------------------------------------------------------------------

  ##Start with initialize sheet
  split_initalize <- initalize %>%
    split_df(complexity = 2)

  ##optimize the first part
  initalize1<-t(data.frame(split_initalize[1])[,-3])
  colnames(initalize1) <- initalize1[1,]
  initalize1 <- as.data.frame(initalize1[-1, ])

  initalize2<-t(data.frame(split_initalize[16])[,-3])
  colnames(initalize2) <- initalize2[1,]
  initalize2 <- as.data.frame(initalize2[-1,-1])

  ##Use loop to read day1-day14 part, and then combine them to a list
  listofdfs <- list()
  for(i in 2:15){
    if(ncol(data.frame(split_initalize[i]))>1){
      df<-data.frame(t(data.frame(split_initalize[i]))[,-3])
      df <- df[2, c(2,4)]
      listofdfs[[i]] <- df
    }
  }

  ##Combine all useful info in initialize sheet
  big_data = do.call(rbind, listofdfs)
  big_data[order(big_data$X4), ]

  ##Setup Friday and Saturday as weekend
  big_data$day_cat <- apply(big_data, 1, function(r) any(r %in% c("Monday","Tuesday","Wednesday","Thursday","Sunday")))
  X4 <- big_data$X4
  big_data$X4 <- as.numeric(X4)
  big_data$number_days<-max(as.numeric(big_data$X4),na.rm=T)
  big_data$weekday <- nrow(big_data[big_data$day_cat == TRUE,])
  big_data$weekendday <- nrow(big_data[big_data$day_cat == FALSE,])

  ##Only keep number of days, number of weekdays and number of weekends
  big_data1<-big_data[1,4:6]

  big_data2 <- as.data.frame(cbind.fill(t(initalize1),big_data1))
  rownames(big_data2) <- c()

  Date_Scored <- big_data2$`Date Scored`
  big_data2$`Date Scored` <- as.numeric(Date_Scored)
  Date_Study<- big_data2$`Date of Study`
  big_data2$`Date of Study` <- as.numeric(Date_Study)

  big_data2$`Date Scored` <- as.Date(as.numeric(big_data2$`Date Scored`),origin = "1899-12-30")
  big_data2$`Date of Study` <- as.Date(as.numeric(big_data2$`Date of Study`),origin = "1899-12-30")

  #----------------------------------------------------------------------------

  table5 <-as.data.frame(table5[,c(1,6,7,9,12,13,15,16,17,18,19,20,21)])
  # table5$`Data Collection Start Date:` <- as.Date(as.numeric(table5$`Data Collection Start Date:`), origin = "1899-12-30")
  # table5$`Data Collection End Date:` <- as.Date(as.numeric(table5$`Data Collection End Date:`), origin = "1899-12-30")

  #Combine part 2 and part 3 together; correct variable name
  finaldata <- data.frame(cbind.fill(table5,table7))
  rownames(finaldata) <- c()
  colnames(finaldata) <- finaldata[1,]
  finaldata <- finaldata[-1, ]
  colnames(finaldata) <- gsub(":", "", colnames(finaldata), fixed = T)

  #----------------------------------------------------------------------------
  ##Combine data together
  newdata <- as.data.frame(cbind.fill(t(table1a),big_data2,finaldata))
  rownames(newdata) <- c()
  newdata<-newdata[,-c(3,12,24,25,27,29)]

  #----------------------------------------------------------------------------

  ##Divided day data and summary data for table 10
  if(dailydata=TRUE){
    table10a <- table10[!(dt4$`Interval Type` %in%  c("Rest Summary","Active Summary","Sleep Summary","Daily Summary","EXCLUDED")) ,]

    ##Convert date variables
    table10a$`Start Date` <- as.Date(as.numeric(table10a$`Start Date`), origin = "1899-12-30")
    table10a$`End Date` <- as.Date(as.numeric(table10a$`End Date`), origin = "1899-12-30")

    ##Work on time variables
    time <- table10a[,c("End Time","Interval Type")]
    time$time <- round(as.numeric(time$`End Time`)*24,2)
    time0 <- time %>% separate(time, c("hour", "min"))
    time0$min2 <- round(as.numeric(time0$min)*0.6,0)
    time0$min2[is.na(time0$min2)] <- "00"
    time0$time <- paste0(time0$hour,":",time0$min2)
    time0$time[time0$time == "NaN:00"] <- NA
    table10a$`End Time` <- time0$time
    table10a$`End Date`[time$time <=6 & !is.na(time$time)] <- table10a$`End Date`-1
    table10a$`End Day` <- weekdays(table10a$`End Date`)


    time <- table10a[,c("Start Time","Interval Type")]
    time$time <- round(as.numeric(time$`Start Time`)*24,2)
    time0 <- time %>% separate(time, c("hour", "min"))
    time0$min2 <- round(as.numeric(time0$min)*0.6,0)
    time0$min2[is.na(time0$min2)] <- "00"
    time0$time <- paste0(time0$hour,":",time0$min2)
    time0$time[time0$time == "NaN:00"] <- NA
    table10a$`Start Time` <- time0$time
    table10a$`Start Date`[time$time <=6 & !is.na(time$time)] <- table10a$`Start Date`[time$time <=6 & !is.na(time$time)]-1
    table10a$`Start Day` <- weekdays(table10a$`Start Date`)

  } else {
    table10a <- table10[(dt4$`Interval Type` %in%  c("Rest Summary","Active Summary","Sleep Summary","Daily Summary")) ,]
  }

  #----------------------------------------------------------------------------


  #initalize2<-as.numeric(unlist(initalize2))
  table10a$`Interval#` <- as.numeric(table10a$`Interval#`)
  #newdt <- table10a[table10a$`Interval#` %in% unique(initalize2), ]
  ##Select date base on date instead of interval
  newdt <- table10a[!is.na(table10a$`Start Date`) & (table10a$`Start Date` >= big_data2$`Date of Study`) & (table10a$`Start Date` <= (big_data2$`Date of Study`+big_data1$number_days-1)),]

  ##Define weekday and weekend in Table 10
  newdt$day_cat <- "weekday"
  newdt$day_cat[newdt$`Start Day` %in% c("Saturday", "Friday")] <- "weekend"


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

  ##Calculate mean value for following variables base on weekdays and weekends.
  df.means <- aggregate(list(newdt$newstarttime, newdt$newendtime, newdt$Duration, newdt$`Sleep Time`, newdt$WASO, newdt$Fragmentation,
                             newdt$Efficiency, newdt$`Onset Latency`) ,by=list(newdt$day_cat,newdt$`Interval Type`),mean)
  ##Give name for above variables
  names(df.means) <- c("day_cat","Interval Type","newstarttime","newendtime","Avg_Duration_byday","Avg_Sleep_Time_byday","Avg_WASO_byday",
                       "Avg_Fragmentation_byday","Avg_Efficiency_byday", "Avg_Onset_Latency_byday")
  ##Convert time variable
  df.means$avg_starttime_byday <- round((df.means$newstarttime) / 60,2)
  df.means$avg_endtime_byday <- round((df.means$newendtime) / 60,2)
  ##Delete previous time variable
  df.means<-df.means[,-c(3:4),drop=FALSE]

  na.omit=T
  ##Calculate mean value for following variables base on interval type.
  df.means1 <- aggregate(list(newdt$newstarttime, newdt$newendtime, newdt$Duration, newdt$`Sleep Time`, newdt$WASO, newdt$Fragmentation,
                              newdt$Efficiency, newdt$`Onset Latency`,newdt$`Off-Wrist`,newdt$`Total AC`,newdt$`Exposure White`,newdt$`Avg White`)
                         ,by=list(newdt$`Interval Type`),mean)
  ##Give name for above variables
  names(df.means1) <- c("Interval Type","newstarttime","newendtime","Avg_Duration_overall","Avg_Sleep_Time_overall","Avg_WASO_overall",
                        "Avg_Fragmentation_overall","Avg_Efficiency_overall","Avg_Onset_Latency_overall","Avg_Off_Wrist_overall",
                        "Avg_Total_AC_overall","Avg_Exp_White_overall","Avg_Avg_White_overall")
  ##Convert time variable
  df.means1$avg_starttime_overall <- round((df.means1$newstarttime) / 60,2)
  df.means1$avg_endtime_overall <- round((df.means1$newendtime) / 60,2)
  ##Delete previous time variable
  df.means1<-df.means1[,-c(2:3),drop=FALSE]

  #----------------------------------------------------------------------------


  #####################Reshape to one row####################
  ##Use rangetype to define which interval type we need to use. We have 4 different interval types: Active, Daily, Sleep and Rest.
  df.means <- df.means[df.means$`Interval Type` %in% rangetype,]
  df.means1 <- df.means1[df.means1$`Interval Type` %in% rangetype,]


  #Melt weekday and weekend into variables. Number of row will change from 8 to 4 to match what we have for overall.
  df.means3 <- reshape(data=df.means,idvar = "Interval Type", timevar =c("day_cat"), v.names = c("Avg_Duration_byday","Avg_Sleep_Time_byday",
                                                                                                 "Avg_WASO_byday","Avg_Fragmentation_byday",
                                                                                                 "Avg_Efficiency_byday","Avg_Onset_Latency_byday",
                                                                                                 "avg_starttime_byday","avg_endtime_byday"), direction="wide")
  ##Some variables' value showed the units such as mins.Need to fix it.
  df.means3<-data.frame(gsub("mins", "", t(df.means3), fixed = TRUE))

  ##Some variable name included dots. Need to fix it.
  rownames(df.means3) <- gsub("byday.", "", rownames(df.means3),fixed = TRUE)


  ##Create ID variable, then Combine byday and overall data together.
  df.means1$ID <- 1
  df.means4 <- merge(df.means1,t(df.means3),by="Interval Type")

  ##Combine different interval types into one row data
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

  #----------------------------------------------------------------------------


  ##Select interval type & variables we need
  newdt1 <- newdt[newdt$`Interval Type` %in% rangetype, c("Interval Type","Interval#","Start Time","End Time","Duration","Off-Wrist",
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

  df.means7<-data.frame(t(gsub("mins", "", df.means7t, fixed = TRUE)))
  names(df.means7) <- gsub(".", "_", names(df.means7), fixed = T)

  df.means7 <- subset(df.means7, select = -c(ID, ID_1))
}
