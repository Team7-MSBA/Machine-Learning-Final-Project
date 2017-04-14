rm(list = ls())

library(lubridate)
library(ggplot2)

test <- read.csv('test.csv',head=TRUE)
train <- read.csv('train.csv',head=TRUE)
store <- read.csv('store.csv',head=TRUE)

levels(store$Assortment)[levels(store$Assortment)=="a"] <- "basic"
levels(store$Assortment)[levels(store$Assortment)=="b"] <- "extra"
levels(store$Assortment)[levels(store$Assortment)=="c"] <- "extended"

train$Open[train$Open == 1] <- "open"
train$Open[train$Open == 0] <- "closed"

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





