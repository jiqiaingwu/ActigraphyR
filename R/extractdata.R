#' Extract Actigraphy Data to csv file
#'
#' @param table_raw Import Actigraphy Raw data
#' @param initalize Import Initialize Raw data
#' @param identifier Decide if output identifier variables such as Name, Age etc. The default value is False
#' @param rangetype Decide which type of interval will be output. The default value is all four intervals: Active, DAILY, REST, SLEEP
#' @param actiware_property Select useful variables in Actiware Data Property section
#' @param calcols_byday_chosen Calculate the mean value of columns base on weekday and weekend
#' @param calcols_overall_chosen Calculate the overall mean value of columns
#' @param cols_chosen Select variables to output for each day. Please note variables Interval Type & Interval# are required in the list
#' @return The final result
#' @export


## Step 1. Create cunstomized functions
## Step 2. Import Raw Data and split into multiple pieces.
## Step 3. Start to do the data cleaning, such as format date variables and time variables.
## Step 4. The Raw data can be divided by two parts. One part could be directly extract. Another part is byday summary, which need calculations.
## Step 5. Start to work/import on initalize sheet
## Step 6. Data cleaning for initalize sheet. Finally, I produce a single row data, which will combine with the directly extraction part in Raw data.
## Step 7. Data cleaning for byday summary
## Step 8. Combine all data together

extractdata = function(table_raw,initalize,  identifier=FALSE,
                       rangetype=c("DAILY","REST","SLEEP","ACTIVE"),
                       actiware_property=c("Actiwatch Type:","Logging Mode:","Time Zone:","Epoch Length:","Number of Days:","Actiwatch Serial Number:",
                                            "Actiwatch Firmware Version:","Activity Calibration Factor:","White Calibration Factor:",
                                            "Red Calibration Factor:","Green Calibration Factor:","Blue Calibration Factor:"),
                       calcols_byday_chosen=c("Start Time", "End Time", "Duration", "Sleep Time", "WASO", "Fragmentation","Efficiency","Onset Latency",
                                              "Avg White","Avg Green","Avg Red","Avg Blue"),
                       calcols_overall_chosen=c("Start Time", "End Time", "Duration", "Sleep Time", "WASO", "Fragmentation","Efficiency","Onset Latency",
                                                "Avg White","Avg Green","Avg Red","Avg Blue"),
                       cols_chosen=c("Interval Type","Interval#","Start Time","End Time","Duration","Off-Wrist","Total AC","Onset Latency","Efficiency",
                                     "WASO","Sleep Time","Fragmentation")){


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

  ##In table 1, analysis name is the only useful information.Then split it into participant ID and Event time.
  table1<-t(as.data.frame(split_table[1])[3,])
  table1 <- table1[-1, ]
  table1a <- str_split(table1, "_", simplify = TRUE)
  table1a <- table1a[,-3]
  names(table1a) <- c("Participant_ID", "Event Time")

  ##In table 3, all variables are identifiers. The default setting is omitting.(Subject Property)
  if(identifier==TRUE){
    table3<-t(data.frame(split_table[3])[,-3]) #delete the units
  } else if (identifier==FALSE) {
    table3<-NULL
  }

  ##Output Table 5 (Actiwatch Data Property)
  table5<-t(data.frame(split_table[5])[,-3]) #delete the units

  ##Output Table 7 (Analysis Output)
  table7<-t(data.frame(split_table[7])[,-3]) #delete the units

  ##Output Table 10, table 9 should be the variable name of table 10. Need to combine them together. (Statistics)
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

  ##Combine initialize sheet first 4 key variables with above dataset
  big_data2 <- as.data.frame(cbind.fill(t(initalize1),big_data1))
  rownames(big_data2) <- c() #Delete rowname

  ##Format date variable to numeric
  Date_Scored <- big_data2$`Date Scored`
  big_data2$`Date Scored` <- as.numeric(Date_Scored)
  Date_Study<- big_data2$`Date of Study`
  big_data2$`Date of Study` <- as.numeric(Date_Study)

  ##Re-assign to date format
  big_data2$`Date Scored` <- as.Date(as.numeric(big_data2$`Date Scored`),origin = "1899-12-30")
  big_data2$`Date of Study` <- as.Date(as.numeric(big_data2$`Date of Study`),origin = "1899-12-30")

  #----------------------------------------------------------------------------
  colnames(table5) <- table5[1,]
  table5 <- as.data.frame(table5[-1,actiware_property])

  colnames(table7) <- table7[1,]
  table7 <- as.data.frame(table7[-1,])

  finaldata <- merge(t(table5),t(table7))


  # table5 <-as.data.frame(table5[,c(1,6,7,9,12,13,15,16,17,18,19,20,21)])
  # table5$`Data Collection Start Date:` <- as.Date(as.numeric(table5$`Data Collection Start Date:`), origin = "1899-12-30")
  # table5$`Data Collection End Date:` <- as.Date(as.numeric(table5$`Data Collection End Date:`), origin = "1899-12-30")

  #Combine part 2 and part 3 together; correct variable name
  # finaldata <- data.frame(cbind.fill(table5,table7))
  # rownames(finaldata) <- c()
  # colnames(finaldata) <- finaldata[1,]
  # finaldata <- finaldata[-1, ]
  colnames(finaldata) <- gsub(":", "", colnames(finaldata), fixed = T)

  #----------------------------------------------------------------------------
  ##Combine data together
  newdata <- as.data.frame(cbind.fill(t(table1a),big_data2,finaldata))
  rownames(newdata) <- c()
  newdata<-newdata[,-match(c("Study ID","Time Zone","Wake Threshold Value","Sleep Interval Detection Algorithm","Sleep Onset Setting", "Sleep End Setting"),names(newdata))]
  # newdata<-newdata[,-c(3,12,24,25,27,29)]

  #----------------------------------------------------------------------------

  ##Divided day data and summary data for table 10
  table10a <- table10[!(table10$`Interval Type` %in%  c("Rest Summary","Active Summary","Sleep Summary","Daily Summary","EXCLUDED")) ,]

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

  #----------------------------------------------------------------------------

  ##Convert number of intervals to numeric
  table10a$`Interval#` <- as.numeric(table10a$`Interval#`)
  ##Select date base on study date instead of interval number
  newdt <- table10a[!is.na(table10a$`Start Date`) & (table10a$`Start Date` >= big_data2$`Date of Study`) & (table10a$`Start Date` <= (big_data2$`Date of Study`+big_data1$number_days-1)),]

  ##Define weekday and weekend in Table 10
  newdt$day_cat <- "weekday"
  newdt$day_cat[newdt$`Start Day` %in% c("Saturday", "Friday")] <- "weekend"

  ##Convert multiple variables into numeric
  newdt$Duration <- as.numeric(newdt$Duration)
  newdt$`Sleep Time` <- as.numeric(newdt$`Sleep Time`)
  newdt$WASO <- as.numeric(newdt$WASO)
  newdt$Fragmentation <- as.numeric(newdt$Fragmentation)
  newdt$Efficiency <- as.numeric(newdt$Efficiency)
  newdt$`Onset Latency` <- as.numeric(newdt$`Onset Latency`)
  newdt$`Off-Wrist` <- as.numeric(newdt$`Off-Wrist`)
  newdt$`Total AC` <- as.numeric(newdt$`Total AC`)
  newdt$`Exposure White` <- as.numeric(newdt$`Exposure White`)
  newdt$`Avg White` <- as.numeric(newdt$`Avg White`)


  newdt$`Start Time` <- difftime(strptime(newdt$`Start Time`,"%H:%M"),strptime("00:00","%H:%M"), units="mins")
  newdt$`End Time` <- difftime(strptime(newdt$`End Time`,"%H:%M"),strptime("00:00","%H:%M"), units="mins")

  ##Calculate mean value for following variables base on weekdays and weekends.
  # df.means <- newdt[order(day_cat, `Interval Type`), lapply(.SD, mean, na.rm =T), by = .(day_cat, `Interval Type`), .SDcols = calcols_byday_chosen]
  df.means <- aggregate(list(newdt[calcols_byday_chosen]) ,by=list(newdt$day_cat,newdt$`Interval Type`),mean)

  ##Give name for above variables
  names(df.means) <- paste0("Avg_", names(df.means),"_byday")
  names(df.means)[names(df.means) == "Avg_Group.1_byday"] <- "day_cat"
  names(df.means)[names(df.means) == "Avg_Group.2_byday"] <- "Interval Type"

  ##Convert time variable
  if ("Avg_Start Time_byday" %in% colnames(df.means) ==TRUE){
    df.means$Avg_starttime_byday <- round((df.means$`Avg_Start Time_byday` ) / 60,2)
    df.means$`Avg_Start Time_byday`<-NULL
  }

  if ("Avg_End Time_byday" %in% colnames(df.means) ==TRUE){
    df.means$Avg_endtime_byday <- round((df.means$`Avg_End Time_byday` ) / 60,2)
    df.means$`Avg_End Time_byday`<-NULL
  }


  ##Calculate mean value for following variables base on interval type.
  # newdt <- as.data.table(newdt)
  # df.means1 <- newdt[order(day_cat, `Interval Type`), lapply(.SD, mean), by = .(day_cat, `Interval Type`), .SDcols = calcols_overall_chosen]
  df.means1 <- aggregate(list(newdt[calcols_overall_chosen]) ,by=list(newdt$`Interval Type`),mean)

  ##Give name for above variables
  names(df.means1) <- paste0("Avg_", names(df.means1),"_overall")
  # names(df.means1)[names(df.means1) == "Avg_Group.1_overall"] <- "day_cat"
  names(df.means1)[names(df.means1) == "Avg_Group.1_overall"] <- "Interval Type"

  ##Convert time variable & Delete previous time variable
  if ("Avg_Start Time_overall" %in% colnames(df.means1) ==TRUE){
    df.means1$Avg_starttime_overall <- round((df.means1$`Avg_Start Time_overall` ) / 60,2)
    df.means1$`Avg_Start Time_overall`<-NULL
  }

  if ("Avg_End Time_overall" %in% colnames(df.means1) ==TRUE){
    df.means1$Avg_endtime_overall <- round((df.means1$`Avg_End Time_overall` ) / 60,2)
    df.means1$`Avg_End Time_overall`<-NULL
  }


  #----------------------------------------------------------------------------


  #####################Reshape to one row####################
  ##Use rangetype to define which interval type we need to use. We have 4 different interval types: Active, Daily, Sleep and Rest.
  df.means <- df.means[df.means$`Interval Type` %in% rangetype,]
  df.means1 <- df.means1[df.means1$`Interval Type` %in% rangetype,]


  #Melt weekday and weekend into variables. Number of row will change from 8 to 4 to match what we have for overall.
  # df.means_name<-df.means[,grep("Avg_", names(df.means), value=TRUE)]
  # df.means<-as.data.frame(df.means)
  df.means_name<-grep("Avg_", names(df.means), value=TRUE)
  df.means3 <- reshape(data=df.means,idvar = "Interval Type", timevar =c("day_cat"), v.names = df.means_name, direction="wide")


  ##Some variables' value showed the units such as mins.Need to fix it.
  df.means3<-data.frame(gsub("mins", "", t(df.means3), fixed = TRUE))

  ##Some variable name included dots. Need to fix it.
  rownames(df.means3) <- gsub("byday.", "", rownames(df.means3),fixed = TRUE)


  ##Create ID variable, then Combine byday and overall data together.
  df.means1$ID <- 1
  df.means4 <- merge(df.means1,t(df.means3),by="Interval Type")

  ##Combine different interval types into one row data
  # df.means4_name<-df.means[,grep("Avg_", names(df.means4), value=TRUE)]
  df.means4_name<-grep("Avg_", names(df.means4), value=TRUE)

  df.means5 <- reshape(data=df.means4,idvar = "ID", timevar =c("Interval Type"), v.names = df.means4_name, direction="wide")

  #----------------------------------------------------------------------------

  ##Select interval type & variables we need
  newdt1 <- newdt[newdt$`Interval Type` %in% rangetype, cols_chosen]

  cols_chosen<-cols_chosen[-which(cols_chosen %in% c("Interval Type","Interval#"))]

  ##Melt interval type into data, the data will be byday.
  newdt2 <- reshape(data=newdt1,idvar = "Interval#", timevar =c("Interval Type"), v.names = cols_chosen, direction="wide")

  ##Rename day variable, clean variable name
  newdt2$`Interval#`<- paste("D",newdt2$`Interval#`)
  newdt2$`Interval#`<-gsub(" ", "", newdt2$`Interval#`, fixed = TRUE)

  newdt2$ID <- 1

  newdt2t <- t(newdt2)
  newdt3t <- newdt2t[rowSums(is.na(newdt2t)) != ncol(newdt2t), ] # Apply is.na function. Only delete if entire row is NA
  newdt3 <- data.frame(t(newdt3t))

  var_select<-names(newdt3)
  var_select<-var_select[-which(var_select %in% c("ID","Interval."))]


  newdt4 <- reshape(data=newdt3,idvar = "ID", timevar ="Interval.", v.names = var_select, direction="wide")
  #----------------------------------------------------------------------------

  df.means6 <- cbind(newdata,df.means5,newdt4)
  df.means6t <- t(df.means6)
  df.means7t <- df.means6t[rowSums(is.na(df.means6t)) != ncol(df.means6t), ] # Apply is.na function, Only delete if entire row is NA

  df.means7<-data.frame(t(gsub("mins", "", df.means7t, fixed = TRUE)))
  names(df.means7) <- gsub(".", "_", names(df.means7), fixed = T)

  df.means7 <- subset(df.means7, select = -c(ID, ID_1))
  #----------------------------------------------------------------------------

}
