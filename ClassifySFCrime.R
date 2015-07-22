setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")

library(randomForest)

test <- read.csv("./test.csv")
train <- read.csv("./train.csv")
# sample <- read.csv("./sampleSubmission.csv")

#I found the dates to be a bit messy in the file.
#Comes with the job though.

#Adjust the formatting of the text for Dates.
test$Dates <- as.character(test$Dates)
train$Dates <- as.character(train$Dates)

#Make Hour
test$Hour<- as.matrix(as.numeric(substring(test$Dates, 12, 13)))
train$Hour<- as.matrix(as.numeric(substring(train$Dates, 12, 13)))

#Make Year 
test$Year<- as.matrix(as.numeric(substring(test$Dates, 1, 4)))
train$Year<- as.matrix(as.numeric(substring(train$Dates, 1, 4)))

#Make Month
test$Month<- as.matrix(as.numeric(substring(test$Dates, 6, 7)))
train$Month<- as.matrix(as.numeric(substring(train$Dates, 6, 7)))

#Make Day
test$Day<- as.matrix(as.numeric(substring(test$Dates, 9, 10)))
train$Day<- as.matrix(as.numeric(substring(train$Dates, 9, 10)))


#### Samples to test on ####
trainSample <- droplevels(train[sample(nrow(train), 8000), ])
testSample <- droplevels(test[sample(nrow(test), 8000), ])


# Quick code to turn the predictions into submittable csv
# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)

first_attempt <- randomForest(Category ~ DayOfWeek + PdDistrict + Hour + Month, data=trainSample, ntree=5000, importance=TRUE)
