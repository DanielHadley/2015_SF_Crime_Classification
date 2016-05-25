#### A classification contest: using x vars, predict the type of crime ####
setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2

library(randomForest)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(stringr)
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




#### Model 28.0 ####
# Try to add the address features back in 
# And add a couple new features, like is_dup

# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)


## Make an address_scale as above with street
by_address <- test_and_train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address")
rm(by_address)

# And one for test
by_address <- test %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address", all.x = TRUE)
rm(by_address)

# And one for train
by_address <- train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address", all.x = TRUE)
rm(by_address)

# Get rid of NAs
test_and_train <- test_and_train %>% 
  mutate(address_scale = ifelse(is.na(address_scale), 0, address_scale),
         address_scale.x = ifelse(is.na(address_scale.x), 0, address_scale.x),
         address_scale.y = ifelse(is.na(address_scale.y), 0, address_scale.y))


# Find duplicates
test_and_train_dup <- test_and_train %>% 
  arrange(test_and_train_ID) %>% 
  select(-Dates, -test_and_train_ID) %>% 
  duplicated(fromLast = TRUE)

test_and_train <- test_and_train %>% 
  arrange(test_and_train_ID) %>%
  mutate(is_dup = as.numeric(test_and_train_dup))

rm(test_and_train_dup)



## More data engineering 
test_and_train <- test_and_train %>% 
  arrange(test_and_train_ID) %>% 
  
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>%
  
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2))) %>% 
  
  # Rotate X and Ys
  # https://www.kaggle.com/c/sf-crime/forums/t/18853/feature-engineering-of-lat-long-x-y-helps
  mutate(rot45_X = .707 * Y + .707 * X,
         rot45_Y = .707 * Y - .707 * X,
         rot30_X = (1.732/2) * X + (1/2) * Y,
         rot30_Y = (1.732/2) * Y + (1/2) * X,
         rot60_X = (1/2) * X + (1.732/2) * Y,
         rot60_Y = (1/2) * Y - (1.732/2) * X,
         radial_r = sqrt(`^`(Y,2) + `^`(X, 2)))


# one more for |minutes|
date_and_time <- strptime(test_and_train$Dates, '%Y-%m-%d %H:%M:%S')
test_and_train$MinuteAbs30 <- abs(as.numeric(format(date_and_time, '%M')) - 30)

rm(date_and_time)



## Add in the logodds of addresses 
# We will use this table as a feature in the model
C_counts = train %>% group_by(Category) %>% summarise(n=n())
A_C_counts = train %>% group_by(Address, Category) %>% summarise(n=n())
A_counts = train %>% group_by(Address) %>% summarise(n=n())
default_logodds = log(C_counts$n / nrow(train)) - log(1 - (C_counts$n / nrow(train)))

table <- spread(A_C_counts, Category, n)
table[is.na(table)] <- 0
prob_m <- apply(table[2:40], 1, function(x) x / sum(x))
prob_table <- (data.frame(t(prob_m)))

log_m <- apply(prob_table, 1, function(x) log(x))
log_m[is.infinite(log_m)] <- 0
log_table <- (data.frame(t(log_m)))


# These are ones where there was only one categroy at that address
# Or where there were none and so the odds are 0
# We replace both kinds with the default logodds
for (r in 1:nrow(log_table)) {
  for (c in 1:ncol(log_table)) {
    log_table[r,c] <- ifelse(log_table[r,c] == 0 | log_table[r,c] == 1, default_logodds[c], log_table[r,c])
  }
  
}


log_table$Address <- A_counts$Address

rm(A_counts, A_C_counts, C_counts, log_m, prob_m, prob_table, table)


# Put the log odds into the df
# This first line puts them all in, but there are about 3k that don't have log odds 
# Because they are in test but not train
test_and_train <- merge(test_and_train, log_table, by = "Address", all.x = T)

# Create a df of default logodds to merge with a subset of test_and_train that is missing 
default_logodds_df <- data.frame(t(default_logodds))
colnames(default_logodds_df) <- colnames(test_and_train)[28:66]
default_logodds_df$var_to_combine <- 4321

# These are the problem ones with addresses that are in test but not train
need_replacement <- test_and_train %>% 
  filter(is.na(ASSAULT)) %>% 
  select(-ARSON : - WEAPON.LAWS) %>% 
  mutate(var_to_combine = rep(4321))

# Add in the defaults
need_replacement <- merge(need_replacement, default_logodds_df, by = "var_to_combine") %>% 
  select(-var_to_combine)

# Now take out the problem ones
test_and_train <- test_and_train %>% filter(!is.na(ASSAULT))

# And add them back in with the defaults
test_and_train <- rbind(test_and_train, need_replacement) %>% 
  arrange(test_and_train_ID)

# Save for use later
log_table_address_frequencies <- test_and_train %>% 
  select(test_and_train_ID, ARSON : WEAPON.LAWS)

write.csv(log_table_address_frequencies, "./log_table_address_frequencies.csv")


rm(need_replacement, log_table, default_logodds_df, default_logodds, log_table_address_frequencies)




# Get it ready for the model
test_and_train_final <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>%
  arrange(test_and_train_ID) %>% 
  select(-Dates, -Address, -test_and_train_ID)


test_final <- test_and_train_final[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train_final[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)


# # Cross validization 
# cv.nround <- 50
# cv.nfold <- 5
# 
# xgboost_cv = xgb.cv(param=param, data = train_final[, -c(64)], label = train_final[, c(64)], nfold = cv.nfold, nrounds = cv.nround)
# 
# 
# # Need to inspect this closely
# plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)
# 
# # Too many outliers
# xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
# plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)



# xgboost model
nround  = 15
xgboost_model <- xgboost(param = param, data = train_final[, -c(64)], label = train_final[, c(64)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_28')

# [0]	train-mlogloss:3.084693
# [1]	train-mlogloss:2.899164
# ...
# [31]	train-mlogloss:2.315538
# [32]	train-mlogloss:2.312935
# [33]	train-mlogloss:2.310417
# [34]	train-mlogloss:2.308016


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./importance_matrix_28.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_28.csv",row.names = FALSE,quote = F)

# 2.35488, You improved on your best score by 0.00949.  
# Very small incremental improvement over 22




#### Model 27.0 ####
# drop some features that were not adding much in 26, 
# engineer more on the x,y

# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)


## Make an address_scale as above with street
by_address <- test_and_train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address")
rm(by_address)




## More data engineering 
test_and_train <- test_and_train %>% 
  
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>%
  
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2))) %>% 
  
  # Rotate X and Ys
  # https://www.kaggle.com/c/sf-crime/forums/t/18853/feature-engineering-of-lat-long-x-y-helps
  mutate(rot45_X = .707 * Y + .707 * X,
         rot45_Y = .707 * Y - .707 * X,
         rot30_X = (1.732/2) * X + (1/2) * Y,
         rot30_Y = (1.732/2) * Y + (1/2) * X,
         rot60_X = (1/2) * X + (1.732/2) * Y,
         rot60_Y = (1/2) * Y - (1.732/2) * X,
         radial_r = sqrt(`^`(Y,2) + `^`(X, 2)))



# Get it ready for the model
test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>%
  arrange(test_and_train_ID) %>% 
  select(-Dates, -Address, -test_and_train_ID)

test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)


# Cross validization 
cv.nround <- 200
cv.nfold <- 7

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(21)], label = train_final[, c(21)], nfold = cv.nfold, nrounds = cv.nround)


# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)
# looks like 2.5 is about where the relationship starts flattening out



# xgboost model
nround  = 55
xgboost_model <- xgboost(param =param, data = train_final[, -c(21)], label = train_final[, c(21)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_27')

# [0]	train-mlogloss:3.084693
# [1]	train-mlogloss:2.899164
# ...
# [31]	train-mlogloss:2.315538
# [32]	train-mlogloss:2.312935
# [33]	train-mlogloss:2.310417
# [34]	train-mlogloss:2.308016


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./importance_matrix_27.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_27.csv",row.names = FALSE,quote = F)

# 2.35488, You improved on your best score by 0.00949.  
# Very small incremental improvement over 22




#### Model 26.0 ####
# I'm going to pause on address featurization and instead engineer more basic features
# Like maybe an address_score and a log of both street and address scores
# and try more with x and y features

# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)


## Make an address_scale as above with street
by_address <- test_and_train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address")
rm(by_address)




## More data engineering 
test_and_train <- test_and_train %>% 
  
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>%
  
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2))) %>% 
  
  # Create logs and Z of several variables
  mutate(street_scale_log = log(street_scale),
         address_scale_log = log(address_scale),
         X_z = scale(X),
         Y_z = scale(Y)) %>% 
  
  # Rotate X and Ys
  mutate(rot45_X = .707 * Y + .707 * X,
         rot45_Y = .707 * Y - .707 * X,
         radial_r = sqrt(`^`(Y,2) + `^`(X, 2)))



# Get it ready for the model
test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>%
  arrange(test_and_train_ID) %>% 
  select(-Dates, -Address, -test_and_train_ID)

test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)


# Cross validization 
cv.nround <- 200
cv.nfold <- 7

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(21)], label = train_final[, c(21)], nfold = cv.nfold, nrounds = cv.nround)

# [0]	train-mlogloss:3.081633+0.000901	test-mlogloss:3.084943+0.002171
# ...
# [39]	train-mlogloss:2.215668+0.001409	test-mlogloss:2.336232+0.003861

# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)
# looks like 2.5 is about where the relationship starts flattening out



# xgboost model
nround  = 35
xgboost_model <- xgboost(param =param, data = train_final[, -c(21)], label = train_final[, c(21)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_26')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./imimportance_matrix_26.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_26.csv",row.names = FALSE,quote = F)

# 2.36619, You just moved up 34 positions on the leaderboard
# Very small incremental improvement over 22





#### Model 25.0 ####
# Featurize addresses as per
# kaggle.com/c/sf-crime/forums/t/15836/predicting-crime-categories-with-address-featurization-and-neural-nets
# but also begin with cross validization to better tune peramaters of the model

### First create a default of logodds for each address
# And then update with a proper table. 
# The default is like the priors, in a bayesian sense, and the table provides updates
# We will use this table as a feature in the model
C_counts = train %>% group_by(Category) %>% summarise(n=n())
A_C_counts = train %>% group_by(Address, Category) %>% summarise(n=n())
A_counts = train %>% group_by(Address) %>% summarise(n=n())
default_logodds = log(C_counts$n / nrow(train)) - log(1 - (C_counts$n / nrow(train)))

table <- spread(A_C_counts, Category, n)
table[is.na(table)] <- 0
prob_m <- apply(table[2:40], 1, function(x) x / sum(x))
prob_table <- (data.frame(t(prob_m)))

log_m <- apply(prob_table, 1, function(x) log(x))
log_m[is.infinite(log_m)] <- 0
log_table <- (data.frame(t(log_m)))


# These are ones where there was only one categroy at that address
# Or where there were none and so the odds are 0
# We replace both kinds with the default logodds
for (r in 1:nrow(log_table)) {
  for (c in 1:ncol(log_table)) {
    log_table[r,c] <- ifelse(log_table[r,c] == 0 | log_table[r,c] == 1, default_logodds[c], log_table[r,c])
  }
  
}



log_table$Address <- A_counts$Address

write.csv(log_table, "./log_table_address_frequencies.csv", row.names = F)

rm(A_counts, A_C_counts, C_counts, log_m, prob_m, prob_table, table)



# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)



# Put the log odds into the df
# This first line puts them all in, but there are about 3k that don't have log odds 
# Because they are in test but not train
test_and_train <- merge(test_and_train, log_table, by = "Address", all.x = T)

# Create a df of default logodds to merge with a subset of test_and_train that is missing 
default_logodds_df <- data.frame(t(default_logodds))
colnames(default_logodds_df) <- colnames(test_and_train)[14:52]
default_logodds_df$var_to_combine <- 4321

# These are the problem ones with addresses that are in test but not train
need_replacement <- test_and_train %>% 
  filter(is.na(ASSAULT)) %>% 
  select(-ARSON : - WEAPON.LAWS) %>% 
  mutate(var_to_combine = rep(4321))

# Add in the defaults
need_replacement <- merge(need_replacement, default_logodds_df) %>% 
  select(-var_to_combine)

# Now take out the problem ones
test_and_train <- test_and_train %>% filter(!is.na(ASSAULT))

# And add them back in with the defaults
test_and_train <- rbind(test_and_train, need_replacement) %>% 
  arrange(test_and_train_ID)

rm(need_replacement, log_table, default_logodds_df, default_logodds)



## More data engineering 
test_and_train <- test_and_train %>% 
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>% 
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2)))



# Get it ready for the model
test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>% 
  select(-Dates, -Address, -test_and_train_ID)

test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)


# Cross validization 
cv.nround <- 40
cv.nfold <- 7

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(52)], label = train_final[, c(52)], nfold = cv.nfold, nrounds = cv.nround)

# [0]	train-mlogloss:2.838191+0.001372	test-mlogloss:2.846667+0.002753
# [1]	train-mlogloss:2.648722+0.000944	test-mlogloss:2.659731+0.002886
# [2]	train-mlogloss:2.523824+0.000662	test-mlogloss:2.537291+0.002822
# ...
# [37]	train-mlogloss:2.019290+0.000757	test-mlogloss:2.098581+0.002885
# [38]	train-mlogloss:2.017434+0.000700	test-mlogloss:2.098405+0.002942
# [39]	train-mlogloss:2.015517+0.000762	test-mlogloss:2.098297+0.002921

# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)
# looks like 2.5 is about where the relationship starts flattening out




# xgboost model
nround  = 15
xgboost_model <- xgboost(param =param, data = train_final[, -c(52)], label = train_final[, c(52)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_06')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_25.csv",row.names = FALSE,quote = F)

# [0]	train-mlogloss:2.837793
# [1]	train-mlogloss:2.649196
# [2]	train-mlogloss:2.524510
# ...

# 15 nrounds:
# Your submission scored 2.50489, which is not an improvement of your best score. Keep trying!
# 5 nrounds:
# Your submission scored 2.61226, which is not an improvement of your best score. Keep trying!
# I guess it's now overfitting?





#### Model 24.0 ####
# Featurize addresses as per
# kaggle.com/c/sf-crime/forums/t/15836/predicting-crime-categories-with-address-featurization-and-neural-nets

### First create a default of logodds for each address
# And then update with a proper table. 
# The default is like the priors, in a bayesian sense, and the table provides updates
# We will use this table as a feature in the model
C_counts = train %>% group_by(Category) %>% summarise(n=n())
A_C_counts = train %>% group_by(Address, Category) %>% summarise(n=n())
A_counts = train %>% group_by(Address) %>% summarise(n=n())
default_logodds = log(C_counts$n / nrow(train)) - log(1 - (C_counts$n / nrow(train)))

table <- spread(A_C_counts, Category, n)
table[is.na(table)] <- 0
prob_m <- apply(table[2:40], 1, function(x) x / sum(x))
prob_table <- (data.frame(t(prob_m)))

log_m <- apply(prob_table, 1, function(x) log(x))
log_m[is.infinite(log_m)] <- 0
log_table <- (data.frame(t(log_m)))


# These are ones where there was only one categroy at that address
# Or where there were none and so the odds are 0
# We replace both kinds with the default logodds
for (r in 1:nrow(log_table)) {
  for (c in 1:ncol(log_table)) {
    log_table[r,c] <- ifelse(log_table[r,c] == 0 | log_table[r,c] == 1, default_logodds[c], log_table[r,c])
  }
  
}



log_table$Address <- A_counts$Address

write.csv(log_table, "./log_table_address_frequencies.csv", row.names = F)

rm(A_counts, A_C_counts, C_counts, log_m, prob_m, prob_table, table)



# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)



# Put the log odds into the df
# This first line puts them all in, but there are about 3k that don't have log odds 
# Because they are in test but not train
test_and_train <- merge(test_and_train, log_table, by = "Address", all.x = T)

# Create a df of default logodds to merge with a subset of test_and_train that is missing 
default_logodds_df <- data.frame(t(default_logodds))
colnames(default_logodds_df) <- colnames(test_and_train)[14:52]
default_logodds_df$var_to_combine <- 4321

# These are the problem ones with addresses that are in test but not train
need_replacement <- test_and_train %>% 
  filter(is.na(ASSAULT)) %>% 
  select(-ARSON : - WEAPON.LAWS) %>% 
  mutate(var_to_combine = rep(4321))
  
# Add in the defaults
need_replacement <- merge(need_replacement, default_logodds_df) %>% 
  select(-var_to_combine)

# Now take out the problem ones
test_and_train <- test_and_train %>% filter(!is.na(ASSAULT))

# And add them back in with the defaults
test_and_train <- rbind(test_and_train, need_replacement) %>% 
  arrange(test_and_train_ID)

rm(need_replacement, log_table, default_logodds_df, default_logodds)



## More data engineering 
test_and_train <- test_and_train %>% 
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>% 
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2)))



# Get it ready for the model
test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>% 
  select(-Dates, -Address, -test_and_train_ID)

test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 85
xgboost_model <- xgboost(param =param, data = train_final[, -c(52)], label = train_final[, c(52)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_05')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_24.csv",row.names = FALSE,quote = F)

# [0]	train-mlogloss:2.837793
# [1]	train-mlogloss:2.649196
# [2]	train-mlogloss:2.524510
# ...
# [83]	train-mlogloss:1.960859
# [84]	train-mlogloss:1.959610

# Your submission scored 2.67462, which is not an improvement of your best score. Keep trying!
# I guess it's now overfitting?




#### Model 23.0 ####
# This time we will move away from xg to see how well we do with frequencies from the given address
# I just read about address featurization and it seems similar to what I was doing earlier with clusters
mytable <- table(train$Category, train$Address)
d <- data.frame(prop.table(mytable, 1))

Crime_Frequencies <- d %>% 
  spread(key = Var1, value = Freq) %>% 
  rename(Address = Var2)

final <- merge(test, Crime_Frequencies, by = "Address", all.x = TRUE)

# find out which ones are missing
missing <- final %>% 
  filter(is.na(ARSON)) %>% 
  select(Id)

# impute missing from last succesful model
model_22 <- read.csv("./dh_submission_22.csv")

imputed <- merge(missing, model_22)

finalFinal <- final %>% 
  select(Id, ARSON:`WEAPON LAWS`) %>% 
  filter(!is.na(ARSON))

colnames(imputed) <- colnames(finalFinal)

finalFinalFinal <- rbind(finalFinal, imputed)

finalFinalFinal = format(finalFinalFinal, digits=2,scientific=F)

write.csv(finalFinalFinal, file = "dh_submission_23.csv",row.names = FALSE,quote = F)

# Ugh: Your submission scored 6.01974, which is not an improvement of your best score. Keep trying!




#### Model 22.0 ####
# Start to engineer more features

# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)
  
street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)


## More data engineering 
test_and_train <- test_and_train %>% 
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>% 
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                                        ifelse((Hour >=14) & (Hour <20), 1, 2)))



# Get it ready for the model
test_and_train <- test_and_train %>% 
  mutate(DayOfWeek = as.numeric(DayOfWeek)-1,
         PdDistrict = as.numeric(PdDistrict)-1,
         X = as.numeric(X)-1,
         Y = as.numeric(Y)-1,
         Hour = as.numeric(Hour)-1,
         Year = as.numeric(Year)-1,
         Month = as.numeric(Month)-1,
         Day = as.numeric(Day)-1,
         street_scale = as.numeric(street_scale)-1,
         Cluster = as.numeric(Cluster)-1) %>% 
  select(-Dates, -Address, -test_and_train_ID)

test_final <- test_and_train[1:884262,] %>% 
  as.matrix()

train_final <- test_and_train[884263:1762311,] %>% 
  mutate(Category = as.numeric(train$Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 65
xgboost_model <- xgboost(param =param, data = train_final[, -c(13)], label = train_final[, c(13)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_04')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_22.csv",row.names = FALSE,quote = F)

# Sweet!! 2.38580: You just moved up 187 positions on the leaderboard





#### Model 21.0 ####
# Combine model 19 with super priors

priors <- read.csv("./priors.csv")
model_19 <- read.csv("./dh_submission_19.csv")

# For names
train <- read.csv("./train.csv")



# Merge data frames
temp <- cbind(priors, model_19)

model <- sapply(unique(colnames(temp)), 
                 function(x) rowMeans(temp[, colnames(temp) == x, drop = FALSE]))

model <- as.data.frame(model)
model <- format(model, digits=2,scientific=F)

colnames(model)  <- c(levels(train$Category), "Id")


write.csv(model, file = "dh_submission_21.csv", row.names = FALSE,quote = F)

#2.51222 Averaging with the 'super priors' actually took this score down relative to 19




#### Model 20.0 ####
# Third attempt at xgboost
# I seem to have overfit this one

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


## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 155
xgboost_model <- xgboost(param =param, data = train_final[, -c(10)], label = train_final[, c(10)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_03')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_20.csv",row.names = FALSE,quote = F)

# Hmmm 2.49614: which is not an improvement of your best score. Keep trying! 
# Seems like I am overfitting now - too many nrounds




#### Model 19.0 ####
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


## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 55
xgboost_model <- xgboost(param =param, data = train_final[, -c(10)], label = train_final[, c(10)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_02')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_19.csv",row.names = FALSE,quote = F)

# Sweet!! 2.48644: You just moved up 84 positions on the leaderboard




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


## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)

# xgboost model
nround  = 15
xgboost_model <- xgboost(param =param, data = train_final[, -c(10)], label = train_final[, c(10)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_01')


# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(train$Category))
prob = format(prob, digits=2,scientific=F)

prob$Id <- test$Id
write.csv(prob,file = "dh_submission_18.csv",row.names = FALSE,quote = F)

# Sweet!! 2.52588: You just moved up 135 positions on the leaderboard




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
