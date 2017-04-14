rm(list=ls())
#Team 7
#Brittany Hayes, Verghese Polakunnil, Lisha Shangguan, Matthew Esporrin
#read in the data
train<-read.csv("train.csv",sep=",",header=T)
store<-read.csv("store.csv",sep=",",header=T)
levels(store$Assortment)[levels(store$Assortment)=="a"] <- "basic"
levels(store$Assortment)[levels(store$Assortment)=="b"] <- "extra"
levels(store$Assortment)[levels(store$Assortment)=="c"] <- "extended"

#train$Open[train$Open == 1] <- "open"
#train$Open[train$Open == 0] <- "closed"

train$Promo[train$Promo == 1] <- "yes"
train$Promo[train$Promo == 0] <- "no"

levels(train$StateHoliday)[levels(train$StateHoliday)=="0"] <- "None"
levels(train$StateHoliday)[levels(train$StateHoliday)=="a"] <- "Public holiday"
levels(train$StateHoliday)[levels(train$StateHoliday)=="c"] <- "Easter holiday"
levels(train$StateHoliday)[levels(train$StateHoliday)=="d"] <- "Christmas"

train$SchoolHoliday[train$SchoolHoliday == 1] <- "yes"
train$SchoolHoliday[train$SchoolHoliday == 0] <- "no"
store$CompDate <- paste(store$CompetitionOpenSinceYear,store$CompetitionOpenSinceMonth,1, sep="-")
store$CompDate <- as.Date(store$CompDate)
store$CompDays <- as.numeric(as.Date("2015-07-31")-store$CompDate)
#join the tables
joinedData<-merge(x = train, y = store, by = "Store", all.x = TRUE)
write.csv(joinedData, file = "JoinedData.csv")
data<-read.csv("JoinedData.csv",sep=",",header=T)

#convert categoric data to factors
data$Date<-as.Date(data$Data, '%m/%d/%Y')
data$DayOfWeek<-as.factor(data$DayOfWeek)
data$Open <-as.factor(data$Open)
data$Promo<-as.factor(data$Promo)
data$Promo2<-as.factor(data$Promo2)
data$SchoolHoliday<-as.factor(data$SchoolHoliday)
data$CompetitionOpenSinceMonth<-as.factor(data$CompetitionOpenSinceMonth)
data$CompetitionOpenSinceYear<-as.factor(data$CompetitionOpenSinceYear)
data$Promo2SinceWeek<-as.factor(data$Promo2SinceWeek)
data$Promo2SinceYear<-as.factor(data$Promo2SinceYear)
summary(data)

#if we want to take out the days when the store is closed since there will not be sales
datawhenOpen<-subset(data, Open!=0)

#train with 2013 and 2014, test with 2015

#GAM

#Regression Trees

#Boosting

#SVMs





