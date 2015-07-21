setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")

test <- read.csv("./test.csv")
train <- read.csv("./train.csv")
sample <- read.csv("./sampleSubmission.csv")

trainSample <- train[sample(nrow(train), 8000), ]

library(randomForest)

# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)

first_attempt <- randomForest(Category ~ DayOfWeek + PdDistrict, data=trainSample, ntree=5000, mtry=10, importance=TRUE)
