rm(list=ls())
#Team 7
#Brittany Hayes, Verghese Polakunnil, Lisha Shangguan, Matthew Esporrin
#read in the data
#traindata<-read.csv("train.csv",sep=",",header=T)
#storedata<-read.csv("store.csv",sep=",",header=T)
#join the tables
#joinedData<-merge(x = traindata, y = storedata, by = "Store", all.x = TRUE)
#write.csv(joinedData, file = "JoinedData.csv")
data<-read.csv("JoinedData.csv",sep=",",header=T)

#convert categoric data to factors
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

#GAM

#Regression Trees

#Boosting

#SVMs

