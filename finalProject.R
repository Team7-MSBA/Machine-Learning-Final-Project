rm(list = ls())

library(lubridate)
library(ggplot2)
library(dplyr)

data <- read.csv('train.csv',head=TRUE,stringsAsFactors = TRUE)
store <- read.csv('store.csv',head=TRUE)

###################### Data Cleaning & Preprocessing ##############################

data$Open<-factor(data$Open,levels=c(1,0),labels=c("Open","Closed"))

data$Promo<-factor(data$Promo,levels=c(1,0),labels=c("Yes","No"))

levels(data$StateHoliday)[levels(data$StateHoliday)=="0"] <- "None"
levels(data$StateHoliday)[levels(data$StateHoliday)=="a"] <- "Public holiday"
levels(data$StateHoliday)[levels(data$StateHoliday)=="b"] <- "Easter holiday"
levels(data$StateHoliday)[levels(data$StateHoliday)=="c"] <- "Christmas"

data$SchoolHoliday<-factor(data$SchoolHoliday,levels=c(1,0),labels=c("Yes","No"))

data$Date <- as.Date(data$Date)
data$Year <- as.numeric(format(data$Date,"%y"))
data$Month <- as.numeric(format(data$Date,"%m"))

# by_Date <- data %>% group_by(Date) %>% summarise(NumStores=n())
# ggplot(by_Date, aes(Date,NumStores)) + geom_line()

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

# Remove open =0
newdata<-newdata[newdata$Open == 'Open',]
newdata[,-5]

nrow(newdata[which(newdata$CompDays == -1),])
newdata$CompetitionOpenSinceMonth[which(newdata$CompDays == -1)] <- NA
newdata$CompetitionOpenSinceYear[which(newdata$CompDays == -1)] <- NA
newdata$CompDays[which(newdata$CompDays == -1)] <- NA

############# Rmove outliers ###############
# Remove year remote years 1900 1961
nrow(newdata[which(newdata$CompetitionOpenSinceYear == 1900),])
nrow(newdata[which(newdata$CompetitionOpenSinceYear == 1961),])
list <- which(newdata$CompetitionOpenSinceYear == 1900)
newdata <- newdata[-list,]
list <- which(newdata$CompetitionOpenSinceYear == 1961)
newdata <- newdata[-list,]
# Remove sales = 46
newdata <- newdata[newdata$Sales>46,]

############# Convert variable type ##############
# newdata$Month <- factor (newdata$Month)
# newdata$CompetitionOpenSinceYear<-factor(newdata$CompetitionOpenSinceYear)
# newdata$CompetitionOpenSinceMonth <- factor (newdata$CompetitionOpenSinceMonth)

###################################  Transform  #########################################
newdata <- na.omit(newdata)
# distance <- (newdata$CompetitionDistance-min(newdata$CompetitionDistance))/(max(newdata$CompetitionDistance)-min(newdata$CompetitionDistance))
# newdata$CompetitionDistance <- distance
# day <- (newdata$CompDays-min(newdata$CompDays))/(max(newdata$CompDays)-min(newdata$CompDays))
# newdata$CompDays <- day

###################################      CV       ########################################
# Data Split: test data are records of 2015
test <- newdata[which(newdata$Year==15),]
train <- newdata[which(newdata$Year<15),]


############################ Modeling #######################################
rmspe<-function(actuals,predictions){return(mean(((actuals-predictions)/actuals)^2)^0.5)}
######################### Boosting #################################
library(gbm)
library(caret)

control <- trainControl(method='cv', number=5)
# boost.fit<-train(Sales~.,train[,c(1,2,4,6:16)],method="gbm",trControl=control,metric="RMSE",preProc = c("center","scale"))
boost.fit<-train(Sales~.,train[,c(1,2,4,6:16)],method="gbm",trControl=control,metric="RMSE")
summary(boost.fit)
pred<-predict(object=boost.fit,test[,c(1,2,4,6:16)])
mse <- mean((test[4]-pred)^2) #5866356
rmspe(test$Sales,pred) # 0.427

pred<-predict(object=boost.fit,train[,c(1,2,4,6:16)])
mse <- mean((train[4]-pred)^2) #5899619
rmspe(train$Sales,pred) # 0.474


#################### Random Forest #############################
library(randomForest)
library(h2o)
# tell computer to use all the CPUs on the machine
localH2O <- h2o.init(nthreads = -1)
h2o.init()
# run cluster on a server with high memory for optimal performance
h2o.init(nthreads=-1,max_mem_size='6G')
train.h2o <- as.h2o(train[,c(1,2,4,6:16)])
test.h2o <- as.h2o(test[,c(1,2,4,6:16)])
features<-colnames(train[,c(1,2,6:16)])
rf.fit <- h2o.randomForest(x=features,y="Sales",training_frame=train.h2o,ntrees=100, mtries=4, max_depth = 4,seed =5072)
h2o.performance(rf.fit)
# check variable importance
h2o.varimp(rf.fit)
pred <- h2o.predict(rf.fit,test.h2o)
mse <- mean((test$Sales-as.data.frame(pred))^2) # 7280485
rmspe(test$Sales,as.data.frame(pred)) # 0.487

pred <- h2o.predict(rf.fit,train.h2o)
mse <- mean((train$Sales-as.data.frame(pred))^2) # 7280485
rmspe(train$Sales,as.data.frame(pred)) # 0.487



####Subset Selection####
library(leaps)
regfit.full=regsubsets(Sales~.,train, really.big = T)
summary(regfit.full)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")


####GAM####
require(gam)
#with subset selected
gam1=gam(Sales~s(CompDays)+CompetitionOpenSinceYear+s(CompetitionDistance)+Assortment+StoreType+Month+Promo+DayOfWeek, data=train)
trainpreds=predict(gam1,train)
SE=(trainpreds-train$Sales)^2
gam.trainMSE<-summary(SE)[4]
gam.trainMSE
gam.preds<-predict(gam1,newdata = test)
SE=(gam.preds-test$Sales)^2
gam.testMSE<-summary(SE)[4]
gam.testMSE  
rmspe<-function(actuals,predictions){return(mean(((actuals-predictions)/actuals)^2)^0.5)}
SE=(((test$Sales-gam.preds)/test$Sales)^2)^0.5
gam.RMSPE=summary(SE)[4]  

#no CompetitionOpenSinceYear
gam2=gam(Sales~s(CompDays)+s(CompetitionDistance)+Assortment+StoreType+Month+Promo+DayOfWeek, data=train)
#get train MSE
trainpreds=predict(gam2,train)
SE=(trainpreds-train$Sales)^2
gam.trainMSE<-summary(SE)[4]
gam.trainMSE 
SE=(((train$Sales-trainpreds)/train$Sales)^2)^0.5
gam.RMSPE=summary(SE)[4]

#get test MSE
gam.preds<-predict(gam2,newdata = test)
SE=(gam.preds-test$Sales)^2
gam.testMSE<-summary(SE)[4]
gam.testMSE 
SE=(((test$Sales-gam.preds)/test$Sales)^2)^0.5
gam.RMSPE=summary(SE)[4]
gam.RMSPE  

#try with more degrees of freedom
gam3=gam(Sales~s(CompDays,3)+s(CompetitionDistance,3)+Assortment+StoreType+Month+Promo+DayOfWeek, data=train)
gam.preds<-predict(gam3,newdata = test)
SE=(gam.preds-test$Sales)^2
gam.testMSE<-summary(SE)[4]
gam.testMSE 
SE=(((test$Sales-gam.preds)/test$Sales)^2)^0.5
gam.RMSPE=summary(SE)[4]
gam.RMSPE 

#more degrees of freedom
gam4=gam(Sales~s(CompDays,4)+s(CompetitionDistance,4)+Assortment+StoreType+Month+Promo+DayOfWeek, data=train)
gam.preds<-predict(gam4,newdata = test)
SE=(gam.preds-test$Sales)^2
gam.testMSE<-summary(SE)[4]
gam.testMSE 
SE=(((test$Sales-gam.preds)/test$Sales)^2)^0.5
gam.RMSPE=summary(SE)[4]
gam.RMSPE  

#gam1 has the best balance of RMSPE and MSE for the test set
#plot(gam1)

#PCA
library(pls)
library(reshape2)
crs$dataset <- train
# Display a simple summary (structure) of the dataset.
str(crs$dataset)
set.seed(5072) 
crs$nobs <- nrow(crs$dataset) # 647256 observations 
crs$sample <- crs$train
#Pull Data
crs$numeric <- c("Store", "DayOfWeek", "Year", "Month",
                 "CompetitionDistance", "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "CompDays")
#Run PCA
pc <- prcomp(na.omit(crs$dataset[, crs$numeric]), scale=TRUE, center=TRUE, tol=0)
# Show the output of the analysis.
pc
# Summarise the importance of the components found.
summary(pc)
# Display a plot showing the two most principal components.

#biplot(pc)
