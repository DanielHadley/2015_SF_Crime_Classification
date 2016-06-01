#### Created By Daniel Hadley Wed May 25 13:50:52 EDT 2016 #####
## A classification contest: using x vars, predict the type of crime ##
# This script will load data created in the engineer_features and build the final model
setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2

library(dplyr)
library(tidyr)
library(stringr)
library(xgboost) # XGboost approach
set.seed(543)




#### Model 33 ####
# Add the address features back in with calculations based on a random sample
# hopefully this will avoid overfitting
# and added early stop round

test_and_train <- read.csv("./test_and_train_32.csv")


# Get it ready for the model
test_and_train_final <- test_and_train %>%
  
  # Drop the address features
  select(-ARSON : -WEAPON.LAWS) %>% 
  
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
  # There were a bunch of NAs in dates_numeric, so I'm taking those out with the others
  select(-Dates, -Address, -test_and_train_ID, -dates_numeric)


test_final <- test_and_train_final[1:884262,] %>% 
  select(-Category) %>% 
  as.matrix()

train_final <- test_and_train_final[884263:1762311,] %>% 
  mutate(Category = as.numeric(Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)



# Cross validization 
cv.nround <- 75
cv.nfold <- 5

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(26)], label = train_final[, c(26)], 
                    nfold = cv.nfold, nrounds = cv.nround, early.stop.round = 10)


# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)



# xgboost model
nround  = 75
xgboost_model <- xgboost(param = param, data = train_final[, -c(26)], label = train_final[, c(26)], 
                         nrounds=nround, early.stop.round = 5)

# [0]	train-mlogloss:2.889199
# [1]	train-mlogloss:2.702235
# [2]	train-mlogloss:2.580526
# ...
# [72]	train-mlogloss:1.992173
# [73]	train-mlogloss:1.990597
# [74]	train-mlogloss:1.988961

xgb.save(xgboost_model, 'xgboost_model_33')



# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./importance_matrix_33.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(test_and_train$Category))
prob$Id <- as.numeric(seq(1 : 884262) -1)
prob = format(prob, digits=2,scientific=F)

write.csv(prob,file = "dh_submission_33.csv",row.names = FALSE,quote = F)

# 2.11217 Your Best Entry ↑
# Top Ten!
#   You made the top ten by improving your score by 0.07827. 




#### Model 32 ####
# Add the address features back in with calculations based on a random sample
# hopefully this will avoid overfitting
# and added early stop round

test_and_train <- read.csv("./test_and_train_32.csv")

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
  # There were a bunch of NAs in dates_numeric, so I'm taking those out with the others
  select(-Dates, -Address, -test_and_train_ID, -dates_numeric)


test_final <- test_and_train_final[1:884262,] %>% 
  select(-Category) %>% 
  as.matrix()

train_final <- test_and_train_final[884263:1762311,] %>% 
  mutate(Category = as.numeric(Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)



# Cross validization 
cv.nround <- 75
cv.nfold <- 5

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(65)], label = train_final[, c(65)], 
                    nfold = cv.nfold, nrounds = cv.nround, early.stop.round = 10)


# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)



# xgboost model
nround  = 75
xgboost_model <- xgboost(param = param, data = train_final[, -c(65)], label = train_final[, c(65)], 
                         nrounds=nround, early.stop.round = 10)

# [0]	train-mlogloss:2.731622
# [1]	train-mlogloss:2.540978
# ...
# [72]	train-mlogloss:1.848212
# [73]	train-mlogloss:1.846950
# [74]	train-mlogloss:1.845524

xgb.save(xgboost_model, 'xgboost_model_32')



# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./importance_matrix_32.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(test_and_train$Category))
prob$Id <- as.numeric(seq(1 : 884262) -1)
prob = format(prob, digits=2,scientific=F)

write.csv(prob,file = "dh_submission_32.csv",row.names = FALSE,quote = F)

# 2.19044 You improved on your best score by 0.00485.
# but the multilabel is very powerful, so I'm going to take the damn address features out again




#### Model 30 ####

test_and_train <- read.csv("./test_and_train.csv")

## My parameters
param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss")

nround  = 70


crime_categories <- c(levels(test_and_train$Category))

final_predictions <- data_frame(as.numeric(seq(1 : 884262) -1))
colnames(final_predictions)[1] <- "Id"

for (crime in 1:length(crime_categories)) {
  
  category_to_model <- crime_categories[crime]
  
  # Get it ready for the model
  test_and_train_final <- test_and_train %>%
    
    # Drop the address features
    select(-ARSON : -WEAPON.LAWS) %>%
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
    
    mutate(category_binary = ifelse(Category == category_to_model, 1, 0)) %>% 
    
    arrange(test_and_train_ID) %>% 
    select(-Dates, -Address, -test_and_train_ID, -Category)
  
  
  test_final <- test_and_train_final[1:884262,] %>% 
    select(-category_binary) %>% 
    as.matrix()
  
  train_final <- test_and_train_final[884263:1762311,] %>% 
    as.matrix()
  
  xgboost_model <- xgboost(param = param, data = train_final[, -c(25)], label = train_final[, c(25)], nrounds=nround)
  
  # Predict
  pred <- predict(xgboost_model, test_final)
  
  prob <- matrix(pred, ncol = 1, byrow = T)
  prob <- as.data.frame(prob)
  colnames(prob)  <- category_to_model
  prob$Id <- as.numeric(seq(1 : 884262) -1)

  final_predictions[,crime + 1] <- prob[,1]
  colnames(final_predictions)[crime  + 1] <- category_to_model
  
  write.csv(final_predictions, file = paste("./submission_30/final_predictions_", crime, ".csv", sep=""),row.names = FALSE,quote = F)
  
}

final_predictions = format(final_predictions, digits=2,scientific=F)

write.csv(final_predictions,file = "dh_submission_30.csv",row.names = FALSE,quote = F)

# Tiny improvement. Not sure it was worth it:
# 2.19529 : You improved on your best score by 0.00198. 




#### Model 29 ####
# Drop the address features

test_and_train <- read.csv("./test_and_train.csv")

# Get it ready for the model
test_and_train_final <- test_and_train %>%
  
  # Drop the address features
  select(-ARSON : -WEAPON.LAWS) %>% 

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
  select(-Category) %>% 
  as.matrix()

train_final <- test_and_train_final[884263:1762311,] %>% 
  mutate(Category = as.numeric(Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)



# Cross validization 
cv.nround <- 75
cv.nfold <- 5

xgboost_cv = xgb.cv(param=param, data = train_final[, -c(25)], label = train_final[, c(25)], nfold = cv.nfold, nrounds = cv.nround)


# Need to inspect this closely
plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)

# Too many outliers
xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)

# [0]	train-mlogloss:2.962582+0.001128	test-mlogloss:2.966109+0.002168
# [1]	train-mlogloss:2.775106+0.001063	test-mlogloss:2.781634+0.002613
# [2]	train-mlogloss:2.652626+0.000857	test-mlogloss:2.661122+0.002596
# ...
# [71]	train-mlogloss:2.058685+0.001238	test-mlogloss:2.189235+0.002995
# [72]	train-mlogloss:2.056966+0.001292	test-mlogloss:2.188937+0.002973
# [73]	train-mlogloss:2.055132+0.001361	test-mlogloss:2.188597+0.003000
# [74]	train-mlogloss:2.053324+0.001287	test-mlogloss:2.188201+0.003045


# xgboost model
nround  = 70
xgboost_model <- xgboost(param = param, data = train_final[, -c(25)], label = train_final[, c(25)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_29')



# Compute feature importance matrix
names <- dimnames(train_final)[[2]]
importance_matrix <- xgb.importance(names, model = xgboost_model)

write.csv(importance_matrix, "./importance_matrix_29.csv")

# Plotting
xgb.plot.importance(importance_matrix)



# Predict
pred <- predict(xgboost_model, test_final)

prob <- matrix(pred, ncol = 39, byrow = T)
prob <- as.data.frame(prob)
colnames(prob)  <- c(levels(test_and_train$Category))
prob$Id <- as.numeric(seq(1 : 884262) -1)
prob = format(prob, digits=2,scientific=F)

write.csv(prob,file = "dh_submission_29.csv",row.names = FALSE,quote = F)

# Wow, even better than expected: 2.19727
# 19 / 2155




#### Model 28 ####
# Address featurization through simple counts

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
  select(-Category) %>% 
  as.matrix()

train_final <- test_and_train_final[884263:1762311,] %>% 
  mutate(Category = as.numeric(Category)-1) %>% 
  as.matrix()



## My parameters
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 39)



# # Cross validization 
# cv.nround <- 75
# cv.nfold <- 5
# 
# xgboost_cv = xgb.cv(param=param, data = train_final[, -c(64)], label = train_final[, c(64)], nfold = cv.nfold, nrounds = cv.nround)
# 
# # [0]	train-mlogloss:2.849689+0.000994	test-mlogloss:2.855580+0.002806
# # [1]	train-mlogloss:2.633342+0.001820	test-mlogloss:2.642091+0.002372
# # ...
# # [26]	train-mlogloss:1.942508+0.001225	test-mlogloss:1.986560+0.003424
# # [27]	train-mlogloss:1.939151+0.001196	test-mlogloss:1.984520+0.003489
# # [28]	train-mlogloss:1.935856+0.001192	test-mlogloss:1.982650+0.003462
# # [29]	train-mlogloss:1.932666+0.001097	test-mlogloss:1.980919+0.003584
# # [30]	train-mlogloss:1.929659+0.001142	test-mlogloss:1.979338+0.003611
# #...
# # [47]	train-mlogloss:1.891336+0.001107	test-mlogloss:1.966390+0.003465
# # [48]	train-mlogloss:1.889449+0.001176	test-mlogloss:1.966070+0.003485
# 
# 
# # Need to inspect this closely
# plot(xgboost_cv$train.mlogloss.mean, xgboost_cv$test.mlogloss.mean)
# 
# # Too many outliers
# xgboost_cv_n_outliers <- xgboost_cv %>% filter(train.mlogloss.mean < 2.2)
# plot(xgboost_cv_n_outliers$train.mlogloss.mean, xgboost_cv_n_outliers$test.mlogloss.mean)



# xgboost model
nround  = 30
xgboost_model <- xgboost(param = param, data = train_final[, -c(64)], label = train_final[, c(64)], nrounds=nround)

xgb.save(xgboost_model, 'xgboost_model_28')



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
colnames(prob)  <- c(levels(test_and_train$Category))
prob$Id <- as.numeric(seq(1 : 884262) -1)
prob = format(prob, digits=2,scientific=F)

write.csv(prob,file = "dh_submission_28.csv",row.names = FALSE,quote = F)

# There is no way to avoid overfitting. Maybe neural nets will help!?
# Your submission scored 2.60565, which is not an improvement of your best score. Keep trying!
