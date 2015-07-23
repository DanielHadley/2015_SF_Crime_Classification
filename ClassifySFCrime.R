setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")

library(randomForest)
library(dplyr)
set.seed(543)

train <- read.csv("./train.csv")
# sample <- read.csv("./sampleSubmission.csv")
test <- read.csv("./test.csv")
# Only load test when needed

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



#### Ok, let's try to engineer some geo-spatial features ####
# After this we will put it in the cloud to train on all of the available data instead of the sample

# K means
# This is how we group crimes on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- test %>% select(X, Y)
clust2 <- train %>% select(X, Y)

clust <- merge(clust, clust2)

# Add cluster variable back to the data frame with the last n clusters
# We use the last 'n' clusters because we will use those to train the model
# And ultimately we will predict future clusters based on the last n clusters
# n is specified in the for loop
# I went with 30 because I suspect that will be enough for prediction
c <- augment(clust, d) %>% select(.cluster)

for(i in 1:30){
  c[[paste('lag', i, sep="_")]] <- lag(c[[i]])
}

c$order <- d$order

d <- merge(d, c, by='order')



# #### First model ####
#
### Samples to test on ##
#trainSample <- droplevels(train[sample(nrow(train), 20000), ])
# 
# # Couple conversions
# trainSample$Hour <- as.factor(trainSample$Hour)
# test$Hour <- as.factor(test$Hour)
# 
# 
# first_attempt <- randomForest(Category ~ PdDistrict + Hour, data=trainSample, ntree=5000, importance=TRUE)
# 
# cm <- data.frame(first_attempt["confusion"])
# varImpPlot(first_attempt)
# 
# 
# ### Now predict ###
# # Load test in first and change the dates
# 
# predicted <- predict(object = first_attempt, newdata = test, type = "prob")
# final <- data.frame(Id = test$Id , predicted)
# 
# 
# ## Prepare for submission
# # Add the factors that were dropped back in
# final$TREA <- 0
# final$"PORNOGRAPHY/OBSCENE MAT" <- 0
# final$"SEX OFFENSES NON FORCIBLE" <- 0
# 
# # Now reorder alphabetically
# final <- final[,order(names(final))]
# # And move ID back
# final <- final %>% select(Id, everything())
# 
# # Add column names
# colnames(final)  <- c("Id",levels(train$Category))
# write.csv(final,file = "dh_submission.csv",row.names = FALSE,quote = F)
# 
# # Result: 14, 229 / 294
# 
# 
# ## Now I want to check if it helps to take minimum values and add them to maximum for certain rows
# fo <- final # final_optimized
# fo[fo < 0.2] <- 0
# fo$Id <- final$Id
# write.csv(fo,file = "dh_submission_2.csv",row.names = FALSE,quote = F)
# 
# # Result: 25, a serious decline
# 







#### Might need later ####

# Quick code to turn the predictions into submittable csv
# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)
