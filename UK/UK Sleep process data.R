library(foreign)
library(lubridate)
library(dplyr)
library(tidyr)


###load the raw data
setwd("C:\\Users\\caspar\\Dropbox\\Baby Laughter\\Pampers\\UK Sleep study")
setwd("C:\\Users\\pss02ca\\Dropbox\\Baby Laughter\\Pampers\\UK Sleep study")

preuse = read.spss("Final Data_Screener&Pre-Use.sav", to.data.frame=TRUE )
diary =  read.spss("Final data_Diary Week 1+2+3.clean.sav", to.data.frame=TRUE)

#Infant behaviour questionnaire scores
#first convert these to numeric
#check that these cols are in teh right place
colnames(preuse)[135] #ought to be preuse.Q31_1
colnames(preuse)[171] #ought to be preuse.Q31_37
#need to do these individually
for (i in 135:171){
  preuse[,i]<-as.numeric(preuse[,i])
}

preuse$preuse.Q31_11r <- (8-as.numeric(preuse$preuse.Q31_11)) 
#which columns count for which list?
surglist = c('preuse.Q31_1','preuse.Q31_2','preuse.Q31_7','preuse.Q31_8','preuse.Q31_13','preuse.Q31_14','preuse.Q31_15','preuse.Q31_20','preuse.Q31_21','preuse.Q31_26','preuse.Q31_27','preuse.Q31_36','preuse.Q31_37')
neglist = c('preuse.Q31_3','preuse.Q31_4','preuse.Q31_9','preuse.Q31_10','preuse.Q31_16','preuse.Q31_17','preuse.Q31_22','preuse.Q31_23','preuse.Q31_28','preuse.Q31_29','preuse.Q31_32','preuse.Q31_33')
efflist = c('preuse.Q31_5','preuse.Q31_6','preuse.Q31_11r','preuse.Q31_12','preuse.Q31_18','preuse.Q31_19','preuse.Q31_24','preuse.Q31_25','preuse.Q31_30','preuse.Q31_31','preuse.Q31_34','preuse.Q31_35')
preuse$SURGENCY <- rowMeans(preuse[,surglist],na.rm=TRUE)
preuse$NEG_AFFECT <- rowMeans(preuse[,neglist],na.rm=TRUE)
preuse$EFFORTFUL <- rowMeans(preuse[,efflist],na.rm=TRUE)


###relabel and turn text based times into numerical values
#start date 
week1 = as.Date("20apr2016","%d%B%Y")
preuse$DOB <- as.Date(preuse$preuse.Q4,"%d/%m/%Y" )
preuse$daysOld <- as.numeric(difftime(week1,preuse$DOB))

#filter the babies 
babies<-filter(preuse, daysOld > 200 & daysOld < 400 )
diaries<-subset(diary, Resp_id %in% babies$Resp_id)

diaries$sleeparrrangements <-    babies[match(diaries$Resp_id, babies$Resp_id),"preuse.Q12"]
diaries$daysOld <-    babies[match(diaries$Resp_id, babies$Resp_id),"daysOld"]
diaries$SURGENCY <-   babies[match(diaries$Resp_id, babies$Resp_id),"SURGENCY"]
diaries$NEG_AFFECT <- babies[match(diaries$Resp_id, babies$Resp_id),"NEG_AFFECT"]
diaries$EFFORTFUL <-  babies[match(diaries$Resp_id, babies$Resp_id),"EFFORTFUL"]

#relabel DVs
diaries$feed <- as.factor(diaries$diary.Q5)
diaries$numwakes <- diaries$diary.Q3
diaries$changed <- diaries$diary.Q6
diaries$diaperstate <- diaries$diary.Q9
diaries$happiness <- diaries$diary.Q12 
diaries$energy <- diaries$diary.Q11

diaries$parenthappiness <- diaries$diary.Q24
diaries$parentenergy <- diaries$diary.Q23
diaries$parentsleepquality <- diaries$diary.Q22

diaries$parentwokenbybaby <- diaries$diary.Q18
diaries$parentotherwakes <- diaries$diary.Q19
diaries$parenttotalwakes <- diaries$parentwokenbybaby + diaries$parentotherwakes

latebedtime<-function(bedtime, noon){
  #function to adjust the date of bedtime if it passes midnight
  if (is.na(bedtime)){
    #skip this one
  }else if (as.numeric(difftime(bedtime,noon)) < 0) {
    #if bedtime is before noon then we need to increase the date
    bedtime <- bedtime + period(1,"days")
  }
  return (bedtime)  
} 


#### calculate baby sleep hours
week1 = as.character(diaries$start_date)
noon <- paste(week1, "12:00")
noon <-parse_date_time(noon, "dmy HM")

bedtime = as.character(diaries$diary.Q1)
bedtime = paste(week1, bedtime)
bedtime <-parse_date_time(bedtime, "dmy HM")
bedtime <- mapply(latebedtime,bedtime,noon)
#for some stupid reason that spits out the numeric value rather than date 
#so have to convert it back
bedtime <- as.POSIXct(bedtime,origin="1970-01-01", tz = "UTC")

waketime = as.character(diaries$diary.Q2)
waketime = paste(week1, waketime)
waketime <-parse_date_time(waketime, "dmy HM")
#morning is next day
waketime <- waketime + period(1,"days")

diaries$bedtime <- bedtime
diaries$waketime <- waketime
diaries$hourssleep <- as.numeric(difftime(waketime,bedtime,units = "hours"))
diaries$relativebedtime <- as.numeric(difftime(bedtime,noon,units = "hours"))


### parent sleep hours
bedtime = as.character(diaries$diary.Q16)
bedtime = paste(week1, bedtime)
bedtime <-parse_date_time(bedtime, "dmy HM")
bedtime <- mapply(latebedtime,bedtime,noon)
bedtime <- as.POSIXct(bedtime,origin="1970-01-01", tz = "UTC")

night2 = as.character(diaries$diary.Q17)
sleeptime = paste(week1, night2)
sleeptime <-parse_date_time(sleeptime, "dmy HM")
sleeptime <- mapply(latebedtime,sleeptime,noon)
#for some stupid reason that spits out the numeric value rather than date 
#so have to convert it back
sleeptime <- as.POSIXct(sleeptime,origin="1970-01-01", tz = "UTC")


morning1 = as.character(diaries$diary.Q20)
morning2 = as.character(diaries$diary.Q21)
waketime = paste(week1, morning1)
waketime <-parse_date_time(waketime, "dmy HM")
waketime <- waketime + period(1,"days") #morning is next day
getuptime = paste(week1, morning2)
getuptime <-parse_date_time(getuptime, "dmy HM")
getuptime <- getuptime + period(1,"days") #morning is next day

diaries$parenthourssleep <- as.numeric(difftime(waketime,sleeptime,units = "hours"))
diaries$parenthoursinbed <- as.numeric(difftime(getuptime,bedtime,units = "hours"))
diaries$parentssleepefficiency <- diaries$parenthourssleep / diaries$parenthoursinbed
diaries$parentrelativebedtime <- as.numeric(difftime(bedtime,noon,units = "hours"))
diaries$parentrelativesleeptime <- as.numeric(difftime(sleeptime,noon,units = "hours"))

#diaries$Resp_id[diaries$parenthoursinbed<diaries$parenthourssleep]


### calculate daily bedtime variations.
#count from day 1 to 21
diaries$daycount <- (diaries$week-1)*7 + diaries$day

#make sure data in order
diaries <- diaries[order(diaries$Resp_id, diaries$daycount),]
#differences between yesterday and today bedtimes. 
bedvariations <- diff(diaries$relativebedtime)
#offset to start from day 2
bedvariations <- append(bedvariations, NA,0) 
#knock out the first day value which is meaningless
bedvariations[diaries$daycount == 1] <- NA
diaries$bedvariations <- bedvariations
bedvariations[diaries$day == 1] <- NA
diaries$bedvariationsbyweek <- bedvariations

#now do same for parent bedtimes
bedvariations <- diff(diaries$parentrelativebedtime)
bedvariations <- append(bedvariations, NA,0) 
bedvariations[diaries$daycount == 1] <- NA
diaries$parentbedvariations <- bedvariations
bedvariations[diaries$day == 1] <- NA
diaries$parentbedvariationsbyweek <- bedvariations

sleepvariations <- diff(diaries$hourssleep)
#offset to start from day 2
sleepvariations <- append(sleepvariations, NA,0) 
#knock out the first day value which is meaningless
sleepvariations[diaries$daycount == 1] <- NA
diaries$sleepvariations <- sleepvariations
sleepvariations[diaries$day == 1] <- NA
diaries$sleepvariationsbyweek <- sleepvariations

#now do same for parent bedtimes
sleepvariations <- diff(diaries$parenthourssleep)
sleepvariations <- append(sleepvariations, NA,0) 
sleepvariations[diaries$daycount == 1] <- NA
diaries$parentsleepvariations <- sleepvariations
sleepvariations[diaries$day == 1] <- NA
diaries$parentsleepvariationsbyweek <- sleepvariations


#get rid of annoying as.factor datatypes
babies$Resp_id <- as.numeric(as.character(babies$Resp_id))
diaries$Resp_id <- as.numeric(as.character(diaries$Resp_id))
diaries$feed <- as.numeric(as.character(diaries$feed))
diaries$numwakes <- as.numeric(as.character(diaries$numwakes))

#simple formating doesn't seem to reload very reliably
#write.table(babies,file="babies.dat",sep="\t", qmethod = "escape")
#write.table(diaries,file="diaries.dat",sep="\t", qmethod = "escape",quote = TRUE)

#R format
saveRDS(babies, "babies.rds", ascii=TRUE) 
saveRDS(diaries, "diaries.rds", ascii=TRUE) 

#SPSS format
write_sav(babies, "babies.sav")
write_sav(diaries, "diaries.sav")