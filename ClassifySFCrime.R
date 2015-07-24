setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2

library(randomForest)
library(dplyr)
library(tidyr)
set.seed(543)

train <- read.csv("./train.csv")
# sample <- read.csv("./sampleSubmission.csv")
test <- read.csv("./test.csv")
# Only load test when needed


#### Processing data ####

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

clust <- dplyr::bind_rows(clust, clust2)
rm(clust2)

k <- kmeans(clust, 30)

# Add cluster variable back to the data frame with the last n clusters
# I went with 30 because I suspect that will be enough for prediction
test$Cluster <- as.factor(k$cluster[1:884262]) 
train$Cluster <- as.factor(k$cluster[884263:1762311])

rm(k, clust)



#### Model 4.0 ####
# Set up for the amazon EC2 environment
# Reducing the trees seemed to cut into performance more than the gains from training on all of the data
model <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=train, ntree=300, importance=TRUE)

cm <- data.frame(model["confusion"])
varImpPlot(model)


### Now predict ###
# Load test in first and change the dates

predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_04.csv",row.names = FALSE,quote = F)

# Result: 9.53563, no improvement




#### Model 3.0 ####
# This time we will move away from RF to see how well we do with frequencies from the given cluster
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 1))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

finalFinal <- final[c(2,13:51)]
finalFinal <- finalFinal[order(finalFinal$Id) , ]

finalFinal$Id <- test$Id
write.csv(finalFinal, file = "dh_submission_03.csv",row.names = FALSE,quote = F)

# Result = 3.55121, moved 10 positions, 205 / 294




#### Model 2.0 ####

model2_0 <- randomForest(Category ~ PdDistrict + Hour, data=trainSample, ntree=5000, importance=TRUE)

## Samples to test on ##
trainSample <- droplevels(train[sample(nrow(train), 20000), ])

model <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=trainSample, ntree=5000, importance=TRUE)

cm <- data.frame(model["confusion"])
varImpPlot(model)


### Now predict ###
# Load test in first and change the dates

predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)


## Prepare for submission
# Add the factors that were dropped back in
final$TREA <- 0

# Now reorder alphabetically
final <- final[,order(names(final))]
# And move ID back
final <- final %>% select(Id, everything())

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_02.csv",row.names = FALSE,quote = F)

# Result: 9.1, up 14 positions, 215 / 294




# #### Model 1.0 ####
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
# write.csv(final,file = "dh_submission_01.csv",row.names = FALSE,quote = F)
# 
# # Result: 14, 229 / 294
# 
# 
# ## Now I want to check if it helps to take minimum values and add them to maximum for certain rows
# fo <- final # final_optimized
# fo[fo < 0.2] <- 0
# fo$Id <- final$Id
# write.csv(fo,file = "dh_submission_01_2.csv",row.names = FALSE,quote = F)
# 
# # Result: 25, a serious decline
# 







#### Might need later ####

# Quick code to turn the predictions into submittable csv
# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)
