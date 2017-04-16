rm(list = ls())

library(lubridate)
library(ggplot2)

data <- read.csv('train.csv',head=TRUE,stringsAsFactors = TRUE)
store <- read.csv('store.csv',head=TRUE)

###################### Data Cleaning & Preprocessing ##############################

data$Open<-factor(data$Open,levels=c(1,0),labels=c("Open","Closed"))

data$Promo<-factor(data$Promo,levels=c(1,0),labels=c("Yes","No"))

levels(data$StateHoliday)[levels(data$StateHoliday)=="0"] <- "None"
levels(data$StateHoliday)[levels(data$StateHoliday)=="a"] <- "Public holiday"
levels(data$StateHoliday)[levels(data$StateHoliday)=="c"] <- "Easter holiday"
levels(data$StateHoliday)[levels(data$StateHoliday)=="d"] <- "Christmas"

data$SchoolHoliday<-factor(data$SchoolHoliday,levels=c(1,0),labels=c("Yes","No"))

data$Date <- as.Date(data$Date)
data$Year <- as.numeric(format(data$Date,"%y"))
data$Month <- as.numeric(format(data$Date,"%m"))

store$CompDate <- paste(store$CompetitionOpenSinceYear,store$CompetitionOpenSinceMonth,1, sep="-")
store$CompDate <- as.Date(store$CompDate)
store$CompDays <- as.numeric(as.Date("2015-07-31")-store$CompDate)
store <- store[,c(-7,-8,-9,-10,-11)]

levels(store$Assortment)[levels(store$Assortment)=="a"] <- "basic"
levels(store$Assortment)[levels(store$Assortment)=="b"] <- "extra"
levels(store$Assortment)[levels(store$Assortment)=="c"] <- "extended"

newdata <- merge(x = data, y = store, by = "Store", all.x = TRUE)
newdata <- newdata[,-5]

summary(newdata)
str(newdata)

# Remove sales = 0
newdata<-newdata[newdata$Sales>0,]

nrow(newdata[which(newdata$CompDays == -1),])
newdata$CompetitionOpenSinceMonth[which(newdata$CompDays == -1)] <- NA
newdata$CompetitionOpenSinceYear[which(newdata$CompDays == -1)] <- NA
newdata$CompDays[which(newdata$CompDays == -1)] <- NA

# Rmove outliers
nrow(newdata[which(newdata$CompetitionOpenSinceYear == 1900),])
nrow(newdata[which(newdata$CompetitionOpenSinceYear == 1961),])
list <- which(newdata$CompetitionOpenSinceYear == 1900)
newdata <- newdata[-list,]
list <- which(newdata$CompetitionOpenSinceYear == 1961)
newdata <- newdata[-list,]
# Remove sales = 46
newdata<-newdata[newdata$Sales>46,]

################## CV #######################
# Data Split: test data are records of 2015
test <- newdata[which(newdata$Year==15),]
train <- newdata[which(newdata$Year<15),]


