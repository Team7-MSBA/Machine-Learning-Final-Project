cat("\f")
rm(list=ls())
setwd("C:\\MSBA\\Spring2017\\MachineLearning2\\JoinedData")
dir()
data <- read.csv("JoinedData.csv")


###### KAGGLE TRAINING DATA STRUCTURE: 13 VARIABLES INCLUDING
###### KAGGLE TEST DATA HAS ONLY 8 COLUMNS NO SALES OR CUSTOMER COUNT DATA, DOES NOT INCLUDE STORETYPE OR ASSORTMENT OR COMPETITOR DISTANCE
##### THESE FEATURES CAN BE OBTAINED FOR EACH STORE FROM TRAINING DATA AS SHOWN BELOW.
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data <- data[order(as.Date(data$Date, format="%d/%m/%Y")),]
names(data) <- tolower(names(data))
data <- data[-15][-15][-15]
#str(data)
data <- data[-3]
data
nrow(data)
#data <- na.omit(data)
nrow(data)
data <- data[-12][-12][-12]
data[which(is.na(data$competitiondistance)),]$competitiondistance <- median(data[na.omit(data$competitiondistance),]$competitiondistance)
#str(data)
data2 <- read.csv('test.csv')
names(data2) <- tolower(names(data2))
data2 <- data2[-4]
data2$storetype <- NA
data2$assortment <- NA
data2$competitiondistance <- NA
st_index <- unique(data2$store)
for(i in 1:length(st_index)){
      data2[which(data2$store == st_index[i]),]$storetype <-  rep(as.character(data[which(data$store == st_index[i]),]$storetype[1]),length(data2[which(data2$store == st_index[i]),]$storetype))
      data2[which(data2$store == st_index[i]),]$assortment <-  rep(as.character(data[which(data$store == st_index[i]),]$assortment[1]),length(data2[which(data2$store == st_index[i]),]$assortment))
      data2[which(data2$store == st_index[i]),]$competitiondistance <-  rep((data[which(data$store == st_index[i]),]$competitiondistance[1]),length(data2[which(data2$store == st_index[i]),]$competitiondistance))
}

data2 <- data2[-c(480,1336,2192,3048,4760,5616,6472,7328,8184,9040,10752),]
data2$storetype <- as.factor(data2$storetype)
data2$assortment <- as.factor(data2$assortment)
str(data)
str(data2)

#### TESTING MODELS WITH 1 STORE #####
store_data <- split(data, f = data$store)

set.seed(7)
train <- sample(1:nrow(store_data$`1`), .8*nrow(store_data$`1`))

data_train <- store_data$`1`[train,]
data_train <- data_train[-4]

data_test <- store_data$`1`[-train,]
data_test <- data_test[-4]

library(randomForest)
rf.fit <- randomForest(x = data_train[-3], y = data_train$sales, ntree = 100, keep.forest=TRUE)
pred.rf <- predict(rf.fit, newdata = data_test[-3])
pred.c1 <- cbind(pred.rf,data_test$sales)

error1 <- mean(abs(pred.rf - data_test$sales))


library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the rf model
set.seed(7)
modelrf <- train(sales~., data=data_train, method="rf", trControl=control)

# train the gbm model
set.seed(7)
modelgbm <- train(sales~., data=data_train, method="gbm", trControl=control, verbose = FALSE)

predtest <- predict(modelgbm, data_test[-3])

error2 <- mean(abs(predtest - data_test$sales))

# train the gam model
set.seed(7)
modelgam <- train(sales~., data=data_train, method="gamSpline", trControl=control)
# collect resamples
results <- resamples(list(rf=modelrf, GBM=modelgbm, GAM=modelgam))
# summarize the distributions
summary(results)


## tuning gbm
control <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm.tune <- train(sales~., data=data_train, method="gbm", trControl=control, tuneGrid = gbmGrid, verbose = FALSE)


predgbm <- predict(gbm.tune, newdata = data_test[-3])
error3 <- mean(abs(predgbm - data_test$sales))
### tuning gam

control <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)


gam1.tune <- train(sales~., data=data_train, method="gam", trControl=control)

predgam1 <- predict(gam1.tune, newdata = data_test[-3])
error4 <- mean(abs(predgam1 - data_test$sales))

#### store_model function 
#### input test data store number
#### function will look up coresponding rows in training data and create a gbm model
#### the gbm model will then pick the test data observations pertaining to the store and predict sales for 2015
store_model <- function(x){
      set.seed(7)
      
      data_train <- store_data[[x]]
      data_train <- data_train[-4]
      data_test <- data2[which(data2$store == x),]
      data_test <- data_test[2:10]
      
      control <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
      gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                              n.trees = (1:30)*50, 
                              shrinkage = 0.1,
                              n.minobsinnode = 20)
      gbm.tune <- train(sales~., data=data_train, method="gbm", trControl=control, tuneGrid = gbmGrid, verbose = FALSE)
      # train the gbm model
      
      pred.gbm.store <- predict(gbm.tune, newdata = data_test)
      return(pred.gbm.store)
}

pred_st3 <- store_model(3)

#testing


################### Clustering ##########
#### General Overview:
#### USE DUMBIES PACKAGE TO CREATE DUMMY VARIABLE FOR ALL CATEGORICAL DATA
#### SCALE DATA THEN USE HCLUST() TO HIERARCHICAL CLUSTER DATA
#### CREATE CLASSIFICATION MODEL TO PREDICT TEST DATA CLUSTERS USING CLUSTERED TRAINING DATA
#### CREATE 5 SALES PREDICTION GBM MODELS TO PREDICT BE USED ON TRAINING DATA TO PREDICT SALES FOR EACH CLUSTER

#install.packages("dummies")
library("dummies")
data3 <- read.csv("JoinedData.csv")
names(data3) <- tolower(names(data3))
#str(data3)
nrow(data3)
data3 <- na.omit(data3)
nrow(data3)
d <- dummy(data3$stateholiday, sep = ".")
data3 <- data3[-8]
data3 <- cbind(data3,d)
d2 <- dummy(data3$storetype, sep = ".")
data3 <- data3[-9]
data3 <- cbind(data3,d2)
d3 <- dummy(data3$assortment, sep = ".")
data3 <- data3[-9]
data3 <- cbind(data3,d3)
d4 <- dummy(data3$promointerval, sep = '.')
data3 <- data3[-15]
data3 <- cbind(data3,d4)
data3 <- data3[-3]
data3[2:25] <- scale(data3[2:25])
data3 <- data3[-11]
sales3 <- data3[3]
cust3 <- data3[4]
str(data3)

##

set.seed(11)
smpl <- sample(1:nrow(data3), .002*nrow(data3))
length(smpl)
smpldata <- data3[smpl,]
#head(smpldata)
store_clust <- hclust(dist(smpldata[,5:25]), method="complete")
cuts <- cutree(store_clust,5)

smpldata$clust <- cuts
smpldata$clust <- as.factor(smpldata$clust)
nrow(smpldata[which(smpldata$clust == 1),])
nrow(smpldata[which(smpldata$clust == 2),])
nrow(smpldata[which(smpldata$clust == 3),])
nrow(smpldata[which(smpldata$clust == 4),])
nrow(smpldata[which(smpldata$clust == 5),])
str(smpldata)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
grid <- expand.grid(size=c(5,10,15,20,25,30,35,40,45,50), k=c(3,5))

### Cluster predictor
clust1 <- train(clust~., data=smpldata[-c(1,3,4)], method="lvq", trControl=control,tuneGrid=grid)
pred22 <- predict(clust1, smpldata)

####### create 5 models for each cluster:
train1 <- smpldata[which(smpldata$clust == 1),]
train2 <- smpldata[which(smpldata$clust == 2),]
train3 <- smpldata[which(smpldata$clust == 3),]
train4 <- smpldata[which(smpldata$clust == 4),]
train5 <- smpldata[which(smpldata$clust == 5),]

sal1 <- train(sales~., data=train1[-c(1,4,25)], method="gbm", trControl=control, verbose = FALSE)
sal2 <- train(sales~., data=train2[-c(1,4,25)], method="gbm", trControl=control, verbose = FALSE)
sal3 <- train(sales~., data=train3[-c(1,4,25)], method="gbm", trControl=control, verbose = FALSE)
sal4 <- train(sales~., data=train4[-c(1,4,25)], method="gbm", trControl=control, verbose = FALSE)
sal5 <- train(sales~., data=train5[-c(1,4,25)], method="gbm", trControl=control, verbose = FALSE)

















