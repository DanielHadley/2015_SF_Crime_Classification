#### A classification contest: using x vars, predict the type of crime ####
setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2

library(randomForest)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(xgboost) # XGboost approach
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

# Rounding off the latitude and longitudes values to 2 decimals 
test$X <- round(test$X,2)
test$Y <- round(test$Y,2)

train$X <- round(train$X,2)
train$Y <- round(train$Y,2)




#### Ok, let's try to engineer some geo-spatial and temporal features ####
# After this we will put it in the cloud to train on all of the available data instead of the sample

# K means 30
# This is how we group crimes on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- test %>% select(X, Y)
clust2 <- train %>% select(X, Y)

clust <- rbind(clust, clust2)
rm(clust2)

k <- kmeans(clust, 30)

# Add cluster variable back to the data frame with the last n clusters
# I went with 30 because I suspect that will be enough for prediction
test$Cluster <- as.factor(k$cluster[1:884262]) 
train$Cluster <- as.factor(k$cluster[884263:1762311])

rm(k, clust)




#### Model 18.0 ####
# First attempt at xgboost

## Data engineering to prep for xgboost
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         Cluster = as.numeric(Cluster)-1) %>% 
  select(-Dates, -Address)
  
test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()


# train_final <- train %>% 
#   mutate(Category = as.numeric(Category)-1,
#          DayOfWeek = as.numeric(DayOfWeek)-1,
#          PdDistrict = as.numeric(PdDistrict)-1,
#          X = as.numeric(X)-1,
#          Y = as.numeric(Y)-1,
#          Hour = as.numeric(Hour)-1,
#          Year = as.numeric(Year)-1,
#          Month = as.numeric(Month)-1,
#          Day = as.numeric(Day)-1,
#          Cluster = as.numeric(Cluster)-1) %>% 
#   select(-Dates, -Descript, -Address, -Resolution) %>% 
#   as.matrix()
# 
# 
# test_final <- test %>% 
#   mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
#          PdDistrict = as.numeric(PdDistrict)-1,
#          X = as.numeric(X)-1,
#          Y = as.numeric(Y)-1,
#          Hour = as.numeric(Hour)-1,
#          Year = as.numeric(Year)-1,
#          Month = as.numeric(Month)-1,
#          Day = as.numeric(Day)-1,
#          Cluster = as.numeric(Cluster)-1) %>% 
#   select(-Dates, -Address) %>% 
#   as.matrix()




## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 15
xgboost_model <- xgboost(param =param, data = train_final[, -c(10)], label = train_final[, c(10)], nrounds=nround)


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)


# # Predict
# pred <- predict(xgboost_model, test_final[, -c(1)])
# pred = matrix(pred,39,length(pred)/39)
# pred = t(pred)
# 
# # Output submission
# pred = format(pred, digits=2,scientific=F) # shrink the size of submission
# pred = data.frame(1:nrow(pred),pred)
# names(pred) = c('id', levels(train$Category))
# 
# write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
# 
# 
# 
# pred <- predict(xgboost_model, test_final)
# test4 <- matrix(pred_xgboost_test, nrow(test), 39)
# prediction_final <- as.data.frame(test4)
# colnames(prediction_final)  <- c(levels(train$Category))



# Predict
pred <- predict(xgboost_model, test_final)
pred <- matrix(pred, nrow(test), 39)
pred <- as.data.frame(pred)
colnames(pred)  <- c(levels(train$Category))
pred = format(pred, digits=2,scientific=F)


#### Model 17.0 ####
# Use the super priors generated in 16 & Update as in 15

train$"LARCENY.THEFT" <- as.factor(ifelse(train$Category == "LARCENY/THEFT", 1, 0))

trainSample <- droplevels(train[sample(nrow(train), 200000), ])

model <- randomForest(LARCENY.THEFT ~ Cluster + X + Y + DayOfWeek + Hour + Year, data=trainSample, ntree=1500, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
LARCENY.THEFT <- data.frame(Id = test$Id , predicted)


# Now create priors and update them with the predicted values
# Priors
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

final <- final[c(2,13:51)]
final <- final[order(final$Id) , ]

# Put in the RF predictions
LARCENY.THEFT$Prior <- final$`LARCENY/THEFT`
LARCENY.THEFT$Avg <- rowMeans(LARCENY.THEFT[,3:4])
final$`LARCENY/THEFT` <- LARCENY.THEFT$Avg

final$Id <- test$Id


write.csv(final, file = "dh_submission_14.csv",row.names = FALSE,quote = F)

# Result: 2.56297




#### Model 16.0  ####
# Try to make super-priors

# Add more K-means and average them
# K means 40
clust <- test %>% select(X, Y)
clust2 <- train %>% select(X, Y)

clust <- rbind(clust, clust2)
rm(clust2)

k <- kmeans(clust, 40)

# Add cluster variable back to the data frame with the last n clusters
# I went with 30 because I suspect that will be enough for prediction
test$Cluster.40 <- as.factor(k$cluster[1:884262]) 
train$Cluster.40 <- as.factor(k$cluster[884263:1762311])

rm(k, clust)


# K means 20
clust <- test %>% select(X, Y)
clust2 <- train %>% select(X, Y)

clust <- rbind(clust, clust2)
rm(clust2)

k <- kmeans(clust, 20)

# Add cluster variable back to the data frame with the last n clusters
# I went with 30 because I suspect that will be enough for prediction
test$Cluster.20 <- as.factor(k$cluster[1:884262]) 
train$Cluster.20 <- as.factor(k$cluster[884263:1762311])

rm(k, clust)


## Now make tables

mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")


rm(mytable, d, Crime_Frequencies)

final.30 <-  final[,15:53]
final.30$Id <- final$Id
final.30 <- arrange(final.30, Id)
rm(final)

  

mytable <- table(train$Category, train$Cluster.40)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster.40 = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster.40")

rm(mytable, d, Crime_Frequencies)

final.40 <-  final[,15:53]
final.40$Id <- final$Id
final.40 <- arrange(final.40, Id)
rm(final)



mytable <- table(train$Category, train$Cluster.20)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster.20 = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster.20")

rm(mytable, d, Crime_Frequencies)

final.20 <-  final[,15:53]
final.20$Id <- final$Id
final.20 <- arrange(final.20, Id)
rm(final)



# Merge data frames
temp <- cbind(final.20, final.30, final.40)

priors <- sapply(unique(colnames(temp)), 
                 function(x) rowMeans(temp[, colnames(temp) == x, drop = FALSE]))

priors <- as.data.frame(priors)

priors$Id <- test$Id

write.csv(priors, file = "dh_submission_16.csv", row.names = FALSE,quote = F)
write.csv(priors, file = "priors.csv", row.names = FALSE, quote = F)

# Result: 2.57792




#### Model 14.0 - 15.0 ####
# Same as 12, but we average instead of replacing the values
# Cluster frequencies as priors: update with rf on individual categories
train$"LARCENY.THEFT" <- as.factor(ifelse(train$Category == "LARCENY/THEFT", 1, 0))

trainSample <- droplevels(train[sample(nrow(train), 200000), ])

model <- randomForest(LARCENY.THEFT ~ Cluster + X + Y + DayOfWeek + Hour + Year, data=trainSample, ntree=1500, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
LARCENY.THEFT <- data.frame(Id = test$Id , predicted)


# Now create priors and update them with the predicted values
# Priors
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

final <- final[c(2,13:51)]
final <- final[order(final$Id) , ]

# Put in the RF predictions
LARCENY.THEFT$Prior <- final$`LARCENY/THEFT`
LARCENY.THEFT$Avg <- rowMeans(LARCENY.THEFT[,3:4])
final$`LARCENY/THEFT` <- LARCENY.THEFT$Avg

final$Id <- test$Id


write.csv(final, file = "dh_submission_14.csv",row.names = FALSE,quote = F)

# Result: 2.56297

final$`LARCENY/THEFT` <- LARCENY.THEFT$X1

write.csv(final, file = "dh_submission_15.csv",row.names = FALSE,quote = F)

# Result: 2.57367 




#### Model 13.0 ####
# Same as 12, but we average instead of replacing the values
# Cluster frequencies as priors: update with rf on individual categories
train$"LARCENY.THEFT" <- as.factor(ifelse(train$Category == "LARCENY/THEFT", 1, 0))

trainSample <- droplevels(train[sample(nrow(train), 100000), ])

model <- randomForest(LARCENY.THEFT ~ Cluster + X + Y + DayOfWeek + Hour + Year, data=trainSample, ntree=1000, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
LARCENY.THEFT <- data.frame(Id = test$Id , predicted)


# Now create priors and update them with the predicted values
# Priors
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

final <- final[c(2,13:51)]
final <- final[order(final$Id) , ]

# Put in the RF predictions
LARCENY.THEFT$Prior <- final$`LARCENY/THEFT`
LARCENY.THEFT$Avg <- rowMeans(LARCENY.THEFT[,3:4])
final$`LARCENY/THEFT` <- LARCENY.THEFT$Avg

final$Id <- test$Id


write.csv(final, file = "dh_submission_13.csv",row.names = FALSE,quote = F)


# Result: 2.56310 You improved on your best score by 0.01288. You just moved up 13 positions on the leaderboard.




#### Model 12.0 ####
# Cluster frequencies as priors: update with rf on individual categories
train$"LARCENY.THEFT" <- as.factor(ifelse(train$Category == "LARCENY/THEFT", 1, 0))

trainSample <- droplevels(train[sample(nrow(train), 100000), ])

model <- randomForest(LARCENY.THEFT ~ Cluster + X + Y + DayOfWeek + Hour + Year, data=trainSample, ntree=1000, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
LARCENY.THEFT <- data.frame(Id = test$Id , predicted)


# Now create priors and update them with the predicted values
# Priors
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

final <- final[c(2,13:51)]
final <- final[order(final$Id) , ]

# Put in the RF predictions
final$`LARCENY/THEFT` <- LARCENY.THEFT$X1

final$Id <- test$Id

# Now reorder alphabetically
final <- final[,order(names(final))]
# And move ID back
final <- final %>% select(Id, everything())


write.csv(final, file = "dh_submission_12.csv",row.names = FALSE,quote = F)


# Result: 2.57598 You improved on your best score by 0.01012. You just moved up 22 positions on the leaderboard




#### Model 11.0 ####
# Single tree
tree <- rpart(Category ~ X + Y + Cluster,
              data = train,
              method = "class",
              control = rpart.control(minsplit = 200,cp=0)
)

predicted <- predict(object = tree,newdata = test)
final <- data.frame(Id = test$Id , predicted)
colnames(final)  <- c("Id",levels(train$Category))

write.csv(final, file = "dh_submission_11.csv",row.names = FALSE,quote = F)

# Result = 3.02348




#### Model 10.0 ####
# Corrected cluster frequencies
# This time we will move away from RF to see how well we do with frequencies from the district
mytable <- table(train$Category, train$Cluster)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Cluster = Var2)

final <- merge(test, Crime_Frequencies, by = "Cluster")

final <- final[c(2,13:51)]
final <- final[order(final$Id) , ]

final$Id <- test$Id
write.csv(final, file = "dh_submission_10.csv",row.names = FALSE,quote = F)

# Result = 2.58610.  107 / 315




#### Model 9.0 ####
# Weird: it's different from the one on the scripts forum
# This time we will move away from RF to see how well we do with frequencies from the district
mytable <- table(train$Category, train$PdDistrict)
d <- data.frame(prop.table(mytable, 2))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(PdDistrict = Var2)

final <- merge(test, Crime_Frequencies, by = "PdDistrict")

finalFinal <- final[c(2,12:50)]
finalFinal <- finalFinal[order(finalFinal$Id) , ]

finalFinal$Id <- test$Id
write.csv(finalFinal, file = "dh_submission_09.csv",row.names = FALSE,quote = F)

# Result = 3.59304 before I found an error in the table??
# Probably more like score = 2.616




##### Model 8.0 #####
# Hits memory allocation wall. Should try on server.
set.seed(1)
trainSample <- droplevels(train[sample(nrow(train), 20000), ])

model <- randomForest(Category ~ PdDistrict + X + Y, data=trainSample, ntree=30000, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)


## Prepare for submission
# Add the factors that were dropped back in
final$TREA <- 0
final$BRIBERY <- 0


# Now reorder alphabetically
final <- final[,order(names(final))]
# And move ID back
final <- final %>% select(Id, everything())

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_07.csv",row.names = FALSE,quote = F)

# Result: 5.63118, huge improvement over model 6.0! More trees seem to be the answer




##### Model 7.0 #####

set.seed(1)
trainSample <- droplevels(train[sample(nrow(train), 8000), ])

model <- randomForest(Category ~ PdDistrict + X + Y, data=trainSample, ntree=20000, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)


## Prepare for submission
# Add the factors that were dropped back in
final$TREA <- 0
final$BRIBERY <- 0


# Now reorder alphabetically
final <- final[,order(names(final))]
# And move ID back
final <- final %>% select(Id, everything())

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_07.csv",row.names = FALSE,quote = F)

# Result: 5.63118, huge improvement over model 6.0! More trees seem to be the answer




##### Model 6.0 #####
set.seed(1)
trainSample <- droplevels(train[sample(nrow(train), 80000), ])

model <- randomForest(Category ~ PdDistrict + X + Y, data=trainSample, ntree=500, importance=TRUE, do.trace=100)


### Now predict ###
predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)


# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_06.csv",row.names = FALSE,quote = F)

# Result: 11.94028,, no improvement




#### Model 5.0 ####
# Back to my desktop


## Samples to test on ##
set.seed(1)
trainSample1 <- droplevels(train[sample(nrow(train), 20000), ])

set.seed(2)
trainSample2 <- droplevels(train[sample(nrow(train), 20000), ])

set.seed(3)
trainSample3 <- droplevels(train[sample(nrow(train), 20000), ])

set.seed(4)
trainSample4 <- droplevels(train[sample(nrow(train), 20000), ])


model1 <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=trainSample1, ntree=5000, importance=TRUE)

model2 <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=trainSample2, ntree=5000, importance=TRUE)

model3 <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=trainSample3, ntree=5000, importance=TRUE)

model4 <- randomForest(Category ~ PdDistrict + Hour + Month + Cluster, data=trainSample4, ntree=5000, importance=TRUE)

# model <- randomForest::combine(model1, model2, model3, model4)

my_combine <- function (...) 
{
  pad0 <- function(x, len) c(x, rep(0, len - length(x)))
  padm0 <- function(x, len) rbind(x, matrix(0, nrow = len - 
                                              nrow(x), ncol = ncol(x)))
  rflist <- list(...)
  areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
  if (any(!areForest)) 
    stop("Argument must be a list of randomForest objects")
  rf <- rflist[[1]]
  classRF <- rf$type == "classification"
  trees <- sapply(rflist, function(x) x$ntree)
  ntree <- sum(trees)
  rf$ntree <- ntree
  nforest <- length(rflist)
  haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
  vlist <- lapply(rflist, function(x) rownames(importance(x)))
  numvars <- sapply(vlist, length)
  if (!all(numvars[1] == numvars[-1])) 
    stop("Unequal number of predictor variables in the randomForest objects.")
  for (i in seq_along(vlist)) {
    if (!all(vlist[[i]] == vlist[[1]])) 
      stop("Predictor variables are different in the randomForest objects.")
  }
  haveForest <- sapply(rflist, function(x) !is.null(x$forest))
  if (all(haveForest)) {
    nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
    rf$forest$nrnodes <- nrnodes
    rf$forest$ndbigtree <- unlist(sapply(rflist, function(x) x$forest$ndbigtree))
    rf$forest$nodestatus <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$nodestatus, nrnodes)))
    rf$forest$bestvar <- do.call("cbind", lapply(rflist, 
                                                 function(x) padm0(x$forest$bestvar, nrnodes)))
    rf$forest$xbestsplit <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$xbestsplit, nrnodes)))
    rf$forest$nodepred <- do.call("cbind", lapply(rflist, 
                                                  function(x) padm0(x$forest$nodepred, nrnodes)))
    tree.dim <- dim(rf$forest$treemap)
    if (classRF) {
      rf$forest$treemap <- array(unlist(lapply(rflist, 
                                               function(x) apply(x$forest$treemap, 2:3, pad0, 
                                                                 nrnodes))), c(nrnodes, 2, ntree))
    }
    else {
      rf$forest$leftDaughter <- do.call("cbind", lapply(rflist, 
                                                        function(x) padm0(x$forest$leftDaughter, nrnodes)))
      rf$forest$rightDaughter <- do.call("cbind", lapply(rflist, 
                                                         function(x) padm0(x$forest$rightDaughter, nrnodes)))
    }
    rf$forest$ntree <- ntree
    if (classRF) 
      rf$forest$cutoff <- rflist[[1]]$forest$cutoff
  }
  else {
    rf$forest <- NULL
  }
  #
  #Tons of stuff removed here...
  #
  if (classRF) {
    rf$confusion <- NULL
    rf$err.rate <- NULL
    if (haveTest) {
      rf$test$confusion <- NULL
      rf$err.rate <- NULL
    }
  }
  else {
    rf$mse <- rf$rsq <- NULL
    if (haveTest) 
      rf$test$mse <- rf$test$rsq <- NULL
  }
  rf
}

model <- my_combine(model1, model2, model3, model4)

save(model,file = "model_05.RData")

### Now predict ###
# Load test in first and change the dates

load("model_05.RData")

predicted <- predict(object = model, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
final$Id <- test$Id
write.csv(final,file = "dh_submission_04.csv",row.names = FALSE,quote = F)

# Result: ???




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




#### Model 1.0 ####

## Samples to test on ##
trainSample <- droplevels(train[sample(nrow(train), 20000), ])

# Couple conversions
trainSample$Hour <- as.factor(trainSample$Hour)
test$Hour <- as.factor(test$Hour)


first_attempt <- randomForest(Category ~ PdDistrict + Hour, data=trainSample, ntree=5000, importance=TRUE)

cm <- data.frame(first_attempt["confusion"])
varImpPlot(first_attempt)


### Now predict ###
# Load test in first and change the dates

predicted <- predict(object = first_attempt, newdata = test, type = "prob")
final <- data.frame(Id = test$Id , predicted)


## Prepare for submission
# Add the factors that were dropped back in
final$TREA <- 0
final$"PORNOGRAPHY/OBSCENE MAT" <- 0
final$"SEX OFFENSES NON FORCIBLE" <- 0

# Now reorder alphabetically
final <- final[,order(names(final))]
# And move ID back
final <- final %>% select(Id, everything())

# Add column names
colnames(final)  <- c("Id",levels(train$Category))
write.csv(final,file = "dh_submission_01.csv",row.names = FALSE,quote = F)

# Result: 14, 229 / 294


## Now I want to check if it helps to take minimum values and add them to maximum for certain rows
fo <- final # final_optimized
fo[fo < 0.2] <- 0
fo$Id <- final$Id
write.csv(fo,file = "dh_submission_01_2.csv",row.names = FALSE,quote = F)

# Result: 25, a serious decline








#### Might need later ####

# Quick code to turn the predictions into submittable csv
# train$Row <- seq(1:nrow(train))
# long <- data.frame(table(train$Row, train$Category)
# wide <- spread(long, Var2, Freq)
