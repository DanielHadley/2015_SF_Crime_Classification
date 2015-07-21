setwd("~/Github/2015_SF_Crime_Classification/")

test <- read.csv("./test.csv")
train <- read.csv("./train.csv")
sample <- read.csv("./sampleSubmission.csv")

# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)

first_attempt <- randomForest(Category ~ DayOfWeek + PdDistrict, data=train, ntree=5000, mtry=15, importance=TRUE)
