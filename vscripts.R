
cat("\f")
rm(list=ls())
setwd("C:\\MSBA\\Spring2017\\MachineLearning2\\JoinedData")
dir()

data <- read.csv("JoinedData.csv")


data$Date <- as.Date(data$Date, format = "%m/%d/%Y")



data <- data[order(as.Date(data$Date, format="%d/%m/%Y")),]
names(data) <- tolower(names(data))
data <- data[-15][-15][-15]
str(data)
data <- data[-3]
str(data)

nrow(data)
data <- na.omit(data)
nrow(data)
data$competitionopensincemonth <- as.factor(data$competitionopensincemonth)
data$competitionopensinceyear <- as.factor(data$competitionopensinceyear)



hc.complete <- hclust(dist(data[-3]), method="complete")
par(mfrow=c(1,1))
plot(hc.complete)












store_data <- split(data, f = data$store)


set.seed(7)
train <- sample(1:nrow(store_data$`1`), .8*nrow(store_data$`1`))

data_train <- store_data$`1`[train,]
data_test <- store_data$`1`[-train,]

library(randomForest)
rf.fit <- randomForest(x = data_train[-4], y = data_train$sales, ntree = 100)
pred.rf <- predict(rf.fit, newdata = data_test[-4])
pred.c1 <- cbind(pred.rf,data_test$sales)

library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the rf model
set.seed(7)
modelrf <- train(sales~., data=data_train, method="rf", trControl=control)
# train the gbm model
set.seed(7)
modelgbm <- train(sales~., data=data_train, method="gbm", trControl=control)
# train the SVM model
set.seed(7)
modelSvm <- train(sales~., data=data_train, method="svmLinear2", trControl=control)
# collect resamples
results <- resamples(list(rf=modelrf, GBM=modelgbm, SVM=modelSvm))
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

summary(rf.tune)
# train the gbm model

pred.gbm <- predict(rf.tune, newdata = data_test[-4])
pred.c2 <- cbind(pred.gbm, pred.c1)
pred.c2 <- as.data.frame(pred.c2)
pred.c2

### tuning rf

control <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)

rfGrid <-  expand.grid(mtry =  c(1:ncol(data_train[-4]))    ) 

rf2.tune <- train(sales~., data=data_train, method="rf", trControl=control, tuneGrid = rfGrid)
pred.rf2 <- predict(rf2.tune, newdata = data_test[-4])


compTable <- as.data.frame(cbind(pred.rf, pred.rf2, pred.gbm, data_test$sales))
names(compTable) <- c('rf1','rf2','gbm','actual')

compTable$rf1.diff <- abs(compTable$rf1 - compTable$actual)
compTable$rf2.diff <- abs(compTable$rf2 - compTable$actual)
compTable$gbm.diff <- abs(compTable$gbm - compTable$actual)

mean(compTable$rf1.diff)
mean(compTable$rf2.diff)
mean(compTable$gbm.diff)




